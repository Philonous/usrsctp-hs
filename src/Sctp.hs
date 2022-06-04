{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sctp where

import           Data.Bits
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.Foldable          as Foldable
import           Data.List              (foldl')
import           Foreign.C
import           Foreign.Ptr
import           System.IO

import           Data.Coerce            (coerce)
import           Data.IORef
import           Data.Word
import qualified Foreign.C.Types        as C
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc  (allocaBytes, alloca)
import           Foreign.Marshal.Utils  (with)
import           Foreign.Ptr
import           Foreign.Storable       (poke, peek, sizeOf, Storable)

import qualified Foreign.C.Error        as CError

import           Network.Socket         hiding (setSocketOption, getSocketOption)
import           Network.Socket.Address (SocketAddress(..))

import qualified FFI
import qualified Settings
import qualified SocketOptions



-- recvTest = do
--   let target = SockAddrInet 6000 (tupleToHostAddress (127,0,0,1))
--   s <- socket AF_INET Datagram defaultProtocol
--   bind s target

--   FFI.init (\_ bytes len _ _ ->
--                fromIntegral <$> sendBufTo s bytes (fromIntegral len) target
--            )
--   defaultSettings
--   sock <- FFI.socket FFI.aF_CONN Stream
--   defaultSocketOpts sock
--   putStrLn "Setup done"
--   allocaBytes 4096 $ \buf ->
--     forever $ do
--       (read, addr) <- recvFrom s ptr 4096

data SctpSocket = SctpSocket
  { sockRaw :: FFI.Socket
  , sockPendingRecvCount :: IORef Int
  , sockPendingFlushCount :: IORef Int
  }

sctpSocket = do
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
  return
    SctpSocket{ sockRaw = sock
              , sockPendingRecvCount = pendingRecvCount
              , sockPendingFlushCount = pendingFlushCount
              }

-- rcv pull SctpSocket{..} = do
--   pendingRcvs <- readIORef sockPendingRecvCount
--   if pendingRcvs > 0
--     then recv

--   return ()



recv_test target = allocaBytes buffsize $ \buffer ->  do
  let target = SockAddrInet 6000 (tupleToHostAddress (127,0,0,1))
  s <- socket AF_INET Datagram defaultProtocol
  bind s target

  FFI.init (\_ bytes len _ _ ->
               fromIntegral <$> sendBufTo s bytes (fromIntegral len) target
           )
  defaultSettings

  FFI.registerAddress (intPtrToPtr 0)
  putStrLn "Setup done"
  go buffer s
  where
    go buffer socket = do
        (received, from) <- recvBufFrom socket buffer buffsize
        FFI.conninput (intPtrToPtr 0) buffer (fromIntegral received)
    buffsize = 2 ^ (16 :: Int) :: Int

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

recv socket recvBufferLen =
  with (fromIntegral $ sizeOf (undefined :: FFI.SockAddrStorage)) $ \fromLenPtr ->
  with 0 $ \infoTypePtr ->
  with 0 $ \msgFlagsPtr ->
  with (fromIntegral $ sizeOf (undefined :: FFI.RcvInfo)) $ \recvInfoLenPtr ->
  alloca $ \(recvInfoBuffer :: Ptr FFI.RcvInfo)->
  allocaBytes recvBufferLen $ \recvBuffer -> do
    FFI.recvv socket recvBuffer (fromIntegral recvBufferLen)
      nullPtr -- received sockaddr
      fromLenPtr -- in/out: length of receivedSockedLen
      (castPtr recvInfoBuffer)
      recvInfoLenPtr
      infoTypePtr msgFlagsPtr -- TODO

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
  return (if evMask .|. fromEnum FFI.SctpEventRead /= 0
    then (FFI.SctpEventRead:) else Prelude.id)
    $  (if evMask .|. fromEnum FFI.SctpEventWrite /= 0
          then (FFI.SctpEventWrite:) else Prelude.id)
    []

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

subscribeEvent socket event =
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
  subscribeEvent socket FFI.assocChange
  debug "senderDryEvent"
  subscribeEvent socket FFI.senderDryEvent
  debug "streamResetEvent"
  subscribeEvent socket FFI.streamResetEvent

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
