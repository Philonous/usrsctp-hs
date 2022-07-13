-- | SCTP packet parsing, mostly for debugging

{-# LANGUAGE StrictData#-}

module SctpPacket where

import           Control.Applicative (some, many)
import           Control.Monad       (when, replicateM, guard)
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import qualified Data.List           as List
import           Data.Serialize
import           Data.Word
import           Numeric             (showHex)

paddingFor :: Word16 -> Get ()
paddingFor len = do
    let padLength = (4 - (fromIntegral len `mod` 4)) `mod` 4
    _padding <- getByteString padLength
    return ()


data SctpPacketHeader =
  SctpPacketHeader
  { sourcePort :: Word16
  , destPort :: Word16
  , verificationTag :: Word32
  , checksum :: Word32
  } deriving Show

data SctpPacket =
  SctpPacket
  { header :: SctpPacketHeader
  , chunks :: [Chunk]
  } deriving Show

data Chunk =
  Chunk
  { chunkType :: Word8
  , chunkFlags :: Word8
  , chunkLength :: Word16
  , chunkValue :: ByteString
  } deriving Show

getChunkHeader = do
    chunkType <- getWord8
    chunkFlags <- getWord8
    chunkLength <- getWord16be
    let chunkValue = BS.empty
    return Chunk{..}


getChunk = do
    chunkType <- getWord8
    chunkFlags <- getWord8
    chunkLength <- getWord16be
    -- Chunk length includes the 4 header bytes
    chunkValue <- getByteString (fromIntegral chunkLength - 4)
    paddingFor chunkLength
    return Chunk{..}

getSctpPacketHeader = do
    sourcePort <- getWord16be
    destPort <- getWord16be
    verificationTag <- getWord32be
    checksum <- getWord32be
    return SctpPacketHeader{..}

getSctpPacket = do
  header <- getSctpPacketHeader
  chunks <- some getChunk
  isEmpty >>= \case
    True -> return ()
    False -> fail "Not all input was consumed"
  return SctpPacket{..}

--------------------------------------------------------------------------------
-- Data Chunk ------------------------------------------------------------------
--------------------------------------------------------------------------------
data UserData = UserData
  { tsn :: Word32
  , sid :: Word16
  , ssn :: Word16
  , ppid :: Word32
  , userData :: ByteString
  } deriving Show


getUserData = do
  tsn <- getWord32be
  sid <- getWord16be
  ssn <- getWord16be
  ppid <- getWord32be
  userData <- remaining >>= getByteString
  return UserData{..}

--------------------------------------------------------------------------------
-- SACK Chunk ------------------------------------------------------------------
--------------------------------------------------------------------------------

data SACK = SACK
  { cumTSNAck :: Word32
  , aRwnd :: Word32
  , gapAckBlocks :: [(Word16, Word16)]
  , duplicateTSNs :: [Word32]
  } deriving Show

getSack = do
  cumTSNAck <- getWord32be
  aRwnd <- getWord32be
  nrGapAckBlocks <- getWord16be
  nrDuplicateTSNs <- getWord16be
  gapAckBlocks <- replicateM (fromIntegral nrGapAckBlocks) $ do
    start <- getWord16be
    end <- getWord16be
    return (start, end)
  duplicateTSNs <- replicateM (fromIntegral nrDuplicateTSNs)
                              getWord32be
  return SACK{..}

--------------------------------------------------------------------------------
-- INIT Chunk ------------------------------------------------------------------
--------------------------------------------------------------------------------
newtype Hex a = Hex a

instance (Integral a, Show a) => Show (Hex a) where
  show (Hex a) = "0x" ++ showHex a ""

newtype HexBS = HexBS ByteString

instance Show HexBS where
  show (HexBS bs) = "0x" ++ BS.foldr hex "" bs
    where
      hex w | w < 16 = ('0':) . showHex w
            | otherwise = showHex w

data INIT = INIT
  { initiateTag :: Word32
  , initARwnd :: Word32
  , initOStreams :: Word16
  , initIStreams :: Word16
  , initTsn :: Word32
  , initParams :: [InitParam]
  , restBs :: HexBS
  } deriving Show

getInit = do
  initiateTag <- getWord32be
  initARwnd <- getWord32be
  initOStreams <- getWord16be
  initIStreams <- getWord16be
  initTsn <- getWord32be
  let initParams = []
  initParams <- many getInitParam

  restBs <- HexBS <$> (remaining >>= getByteString)
  -- guard $ rest == 0
  return INIT{..}

data InitParam = InitParam
  { tp :: Hex Word16
  , contents :: HexBS
  } deriving Show

getInitParam = do
  tp <- Hex <$> getWord16be
  initParamLen <- getWord16be
  contents <- HexBS <$> (getByteString $ fromIntegral initParamLen - 4)
  r <- remaining
  when (r > 0) $ paddingFor initParamLen
  return InitParam{..}

--------------------------------------------------------------------------------
-- Pretty Printing -------------------------------------------------------------
--------------------------------------------------------------------------------

ppChunkContents 0 = show <$> getUserData
ppChunkContents 3 = show <$> getSack
ppChunkContents 1 = show <$> getInit
ppChunkContents _ = return ""



ppChunks :: SctpPacket -> [Char]
ppChunks SctpPacket{..} =
  "[ " ++ List.intercalate "; "
    [ppChunkType chunkType
    ++ case runGet (ppChunkContents chunkType) chunkValue of
        Left e -> " Can't parse chunk data " ++ show chunkValue
        Right v -> " " ++ v
    | Chunk{..} <- chunks]
  ++ " ]"

ppChunkType :: Word8 -> String
ppChunkType 0  = "DATA"
ppChunkType 1  = "INIT"
ppChunkType 2  = "INIT ACK"
ppChunkType 3  = "SACK"
ppChunkType 4  = "HEARTBEAT"
ppChunkType 5  = "HEARTBEAT ACK"
ppChunkType 6  = "ABORT"
ppChunkType 7  = "SHUTDOWN"
ppChunkType 8  = "SHUTDOWN ACK"
ppChunkType 9  = "ERROR"
ppChunkType 10 = "COOKIE ECHO"
ppChunkType 11 = "COOKIE ACK"
ppChunkType 12 = "ECNE"
ppChunkType 13 = "CWR"
ppChunkType 14 = "SHUTDOWN COMPLETE"
ppChunkType _ = "unknown"


parse :: ByteString -> Either String SctpPacket
parse = runGet getSctpPacket

debugParsePacket bs =
  case runGetState getSctpPacketHeader bs 0 of
    Left e -> (Nothing, [], Just e, BS.length bs, bs)
    Right (header, rest) ->
      let (chunks, e, _, leftover) = debugParseChunks rest
      in (Just header, chunks, e, BS.length leftover, leftover)
  where
    debugParseChunks bs
      | BS.null bs = ([], Nothing, 0,  BS.empty)
      | otherwise = case runGetState getChunk bs 0 of
          Left e -> case runGetState getChunkHeader bs 0 of
            Left e -> ([], Just e, BS.length bs, bs)
            Right (hdr, leftover) -> ([hdr], Just e, BS.length leftover, leftover)
          Right (chunk, rest) ->
            let (chunks, e, len, leftover) = debugParseChunks rest
            in (chunk : chunks, e, len, leftover)
