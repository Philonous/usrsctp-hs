{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Sctp where

import           Data.Bits
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.ByteString.Internal as BS
import           Data.ByteString.Unsafe   (unsafeUseAsCStringLen, unsafePackCStringLen)
import qualified Data.Foldable            as Foldable
import           Data.List                (foldl')
import           Foreign.C
import           Foreign.Ptr
import           System.IO

import           Data.Coerce              (coerce)
import           Data.IORef
import           Data.Word
import qualified Foreign.C.Types          as C
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc    (allocaBytes, alloca, mallocBytes)
import           Foreign.Marshal.Utils    (with)
import           Foreign.Ptr
import           Foreign.Storable         (poke, peek, sizeOf, Storable)

import qualified Foreign.C.Error          as CError

import           Network.Socket           hiding (setSocketOption, getSocketOption)
import           Network.Socket.Address   (SocketAddress(..))

import qualified FFI
import qualified Settings
import qualified SocketOptions

run :: IO ()
run = return ()

testFlag :: Bits a => a -> a -> Bool
testFlag x y = x .&. y /= zeroBits

data SctpSocket = SctpSocket
  { sockRaw :: FFI.Socket
  , sockPendingRecvCount :: IORef Int
  , sockPendingFlushCount :: IORef Int
  , sockRecvBuffer :: Ptr Word8
  , sockRecvBufferSize :: Int
  , sockDownstreamSend :: Ptr Word8 -> Int -> IO Int
  , sockDownstreamRecv :: Ptr Word8 -> Int -> IO Int
  }

data RecvFlags = RecvFlags
  { recvFlagEor :: Bool
  , recvFlagNotification :: Bool
  }

toRecvFlags flg =
  RecvFlags
  { recvFlagEor          = testFlag FFI.recvvFlagMsgEor flg
  , recvFlagNotification = testFlag FFI.recvvFlagMsgNotification flg
  }

sockReadBuf :: SctpSocket -> Int -> Ptr Word8 -> IO (Int, (FFI.RcvInfo, RecvFlags))
sockReadBuf SctpSocket{..} buffSize buffer = go
  where
    go = do
      haveData <- atomicModifyIORef' sockPendingRecvCount $
        \c -> if c > 0
               then (c - 1, True)
               else (0, False)

      if haveData
      then (do
        (read, info, flags) <- recvv sockRaw buffSize buffer
        return (read, (info, toRecvFlags flags))
       )
      else (do
        received <- sockDownstreamRecv sockRecvBuffer sockRecvBufferSize
        FFI.conninput (intPtrToPtr 0) sockRecvBuffer (fromIntegral received)
        go
        )

-- -- sockRead :: SctpSocket -> Int -> IO ByteString
-- sockRead sock buffSize = do
--   (bs, flags) <- BS.createUptoN' buffSize $ sockReadBuf sock buffSize
--   case

sctpSocket dSend dRecv = do
  sock <- FFI.socket FFI.aF_CONN Stream
  defaultSocketOpts sock (1280 - 8 - 40) -- UDP, IPv6
  pendingRecvCount <- newIORef 0
  pendingFlushCount <- newIORef 0
  let upcallHandler s _flags = do
        events <- getEvents s
        Foldable.for_ events $ \case
          FFI.SctpEventRead -> modifyIORef pendingRecvCount (+1)
          FFI.SctpEventWrite -> modifyIORef pendingFlushCount (+1)
          _ -> return ()
  setUpcall sock upcallHandler
  let bufferSize = 65536
  buffer <- mallocBytes bufferSize
  return
    SctpSocket{ sockRaw = sock
              , sockPendingRecvCount = pendingRecvCount
              , sockPendingFlushCount = pendingFlushCount
              , sockRecvBuffer = buffer
              , sockRecvBufferSize = bufferSize
              , sockDownstreamSend = dSend
              , sockDownstreamRecv = dRecv
              }

udpToSctp address s = do
  sctpSocket (\buf size -> sendBufTo s buf size address )
    (\buf bufSize -> do
        (cnt, _) <- recvBufFrom s buf bufSize
        return cnt
    )

-- rcv pull SctpSocket{..} = do
--   pendingRcvs <- readIORef sockPendingRecvCount
--   if pendingRcvs > 0
--     then recv

--   return ()

-- recvTest target = do
--   let target = SockAddrInet 6000 (tupleToHostAddress (127,0,0,1))
--   s <- socket AF_INET Datagram defaultProtocol
--   bind s target

--   FFI.init (\_ bytes len _ _ ->
--                fromIntegral <$> sendBufTo s bytes (fromIntegral len) target
--            )
--   defaultSettings

--   FFI.registerAddress (intPtrToPtr 0)
--   putStrLn "Setup done"
--   socket <- udpToSctp target s
--   go socket
--   where
--     go socket = do
--         bs <- sockRead socket 4096
--         BS.putStr bs
--     buffsize = 2 ^ (16 :: Int) :: Int

data Reliability = Reliable
                 | ReXMit Int -- Try X retransmissions
                 | Timed Int -- Try transmitting for X miliseconds

data Message = Message
  { messageData :: Ptr Word8
  , messageLength :: C.CSize
  , messageSid :: Word16
  , messagePPID:: Word16
  , messageOrdered :: Bool
  , messageReliability :: Reliability
  }

sendv :: FFI.Socket -> ByteString -> FFI.SendvSpa -> C.CInt -> IO C.CLong
sendv socket bytes info flags =
  unsafeUseAsCStringLen bytes $ \(ptr, len) ->
  with info $ \infoPtr ->
  FFI.sendv socket (castPtr ptr) (fromIntegral len)
                   nullPtr 0
                   (castPtr infoPtr)
                   (fromIntegral $ sizeOf (undefined :: FFI.SendvSpa))
                   (fromIntegral FFI.infoTypeSpa) flags


recvv :: FFI.Socket -> Int -> Ptr Word8 -> IO (Int, FFI.RcvInfo, FFI.RcvFlags)
recvv socket recvBufferLen recvBuffer =
  with (fromIntegral $ sizeOf (undefined :: FFI.SockAddrStorage)) $ \fromLenPtr ->
  with 0 $ \infoTypePtr ->
  with 0 $ \msgFlagsPtr ->
  with (fromIntegral $ sizeOf (undefined :: FFI.Rcvinfo)) $ \recvInfoLenPtr ->
  alloca $ \(recvInfoBuffer :: Ptr FFI.RcvInfo) -> do
    -- TODO: this could throw EAGAIN / EWOULDBLOCK. How does it interact with upcalls?
    received <- throwErrnoIfMinus1 "usrsctp_recvv" $
      FFI.recvv socket (castPtr recvBuffer) (fromIntegral recvBufferLen)
        nullPtr -- received sockaddr
        fromLenPtr -- in/out: length of receivedSockedLen
        (castPtr recvInfoBuffer)
        recvInfoLenPtr
        infoTypePtr msgFlagsPtr
    rcvinfo <- peek recvInfoBuffer
    flags <- peek msgFlagsPtr
    return (fromIntegral received, rcvinfo, FFI.RcvFlags flags)

setSocketOption socket (SockOpt level name) value = do
  with value $ \ptr ->
    FFI.setsockopt socket level name (castPtr ptr) (fromIntegral $ sizeOf value)

getSocketOption :: forall a. Storable a => FFI.Socket -> SocketOption -> IO a
getSocketOption socket (SockOpt level name) = alloca $ \ptr -> do
  let bufSize = fromIntegral $ sizeOf (undefined :: a)
  FFI.getsockopt socket level name (castPtr ptr) bufSize
  peek ptr

setUpcall socket cb = do
  cbPtr <- FFI.mkUpcallCallback (\socket' _ptr flags -> cb socket' flags)
  FFI.setUpcall socket cbPtr nullPtr

getEvents socket = do
  evMask <- FFI.getEvents socket
  return $ (if evMask .|. fromEvent FFI.SctpEventRead /= 0
           then (FFI.SctpEventRead:) else Prelude.id)
          $  (if evMask .|. fromEvent FFI.SctpEventWrite /= 0
            then (FFI.SctpEventWrite:) else Prelude.id)
    []
    where
      fromEvent :: FFI.SctpEvent -> CInt
      fromEvent x = fromIntegral $ fromEnum x

--------------------------------------------------------------------------------
-- Default settings ------------------------------------------------------------
--------------------------------------------------------------------------------

defaultSettings = do
    Settings.sysctlSetRecvspace $ 1024 * 1024
    Settings.sysctlSetSendspace $ 1024 * 1024

    -- Increase maximum chunks number on queue to 10K by default
    Settings.sysctlSetMaxChunksOnQueue $ 10 * 1024;

    -- Increase initial congestion window size to 10 MTUs (RFC 6928) by default
    Settings.sysctlSetInitialCwnd 10

    -- Set max burst to 10 MTUs by default (max burst is initially 0,
    -- meaning disabled)
    Settings.sysctlSetMaxBurstDefault 10

    -- Use standard SCTP congestion control (RFC 4960) by default
    -- See https:--github.com/paullouisageneau/libdatachannel/issues/354
    Settings.sysctlSetDefaultCcModule 0

    -- Reduce SACK delay to 20ms by default (the recommended default value
    -- from RFC 4960 is 200ms)
    Settings.sysctlSetDelayedSackTimeDefault 20 -- ms

    -- RTO settings
    --
    -- RFC 2988 recommends a 1s min RTO, which is very high, but TCP on
    -- Linux has a 200ms min RTO
    Settings.sysctlSetRtoMinDefault 200 -- ms
    -- Set only 10s as max RTO instead of 60s for shorter connection timeout
    Settings.sysctlSetRtoMaxDefault 10000 -- ms
    Settings.sysctlSetInitRtoMaxDefault 10000 -- ms
    -- Still set 1s as initial RTO
    Settings.sysctlSetRtoInitialDefault 1000 -- ms

    -- RTX settings
    --
    -- 5 retransmissions instead of 8 to shorten the backoff for shorter
    -- connection timeout
    let maxRtx = 5
    Settings.sysctlSetInitRtxMaxDefault maxRtx
    Settings.sysctlSetAssocRtxMaxDefault maxRtx
    Settings.sysctlSetPathRtxMaxDefault maxRtx -- single path

    -- Heartbeat interval
    Settings.sysctlSetHeartbeatIntervalDefault 10000 -- ms

subscribeEvent socket (FFI.EventType event) =
  setSocketOption socket SocketOptions.sctpEvent
    FFI.Event { eventSeAssocId = FFI.allAssoc
                  , eventSeOn = 1
                  , eventSeType = event
                  }

debug = hPutStrLn stderr

defaultSocketOpts :: FFI.Socket -> Word32 -> IO ()
defaultSocketOpts socket mtu = do
  FFI.setNonBlocking socket True
  -- SCTP must stop sending after the lower layer is shut down, so disable
  -- linger

  debug "linger"
  setSocketOption socket Linger StructLinger{sl_onoff = 1, sl_linger = 0 }
  debug "sctpEnableStreamReset"
  setSocketOption socket SocketOptions.sctpEnableStreamReset
    FFI.AssocValue { assocValueAssocId = FFI.allAssoc
                       , assocValueAssocValue = 1
                       }
  debug "sctpRecvrcvinfo"
  setSocketOption socket SocketOptions.sctpRecvrcvinfo (1 :: CInt)
  debug "assocChange"
  subscribeEvent socket FFI.eventAssocChange
  debug "senderDryEvent"
  subscribeEvent socket FFI.eventSenderDryEvent
  debug "streamResetEvent"
  subscribeEvent socket FFI.eventStreamResetEvent

  debug "nodelay"
  setSocketOption socket SocketOptions.sctpNodelay (1 :: CInt)

  let paddrparams =
        FFI.Paddrparams
        { paddrparamsSppAddress = SockAddrInet 0 0
        , paddrparamsSppAssocId = 0
        , paddrparamsSppHbinterval = 0
        , paddrparamsSppPathmtu = mtu - 12 -- SCTP header
        , paddrparamsSppFlags = fromFlags [FFI.SPP_PMTUD_DISABLE]
        , paddrparamsSppIpv6Flowlabel = 0
        , paddrparamsSppPathmaxrxt = 0
        , paddrparamsSppDscp = 0
        }
  debug "paddrparams"
  setSocketOption socket SocketOptions.sctpPeerAddrParams paddrparams

  let initmsg = FFI.Initmsg
                  { initmsgSinitNumOstreams = 65535
                  , initmsgSinitMaxInstreams = 65535
                  , initmsgSinitMaxAttempts = 0
                  , initmsgSinitMaxInitTimeo = 0
                  }
  debug "initmsg"
  setSocketOption socket SocketOptions.sctpInitmsg initmsg

  debug "fragmentInterleave"
  setSocketOption socket SocketOptions.sctpFragmentInterleave (0 :: CInt)

  return ()

fromFlags :: (Enum a, Integral b) => [a] -> b
fromFlags = fromIntegral . foldl' (.|.) 0 . map fromEnum
