{-# LANGUAGE TemplateHaskell #-}

module Sctp where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Foreign.Ptr

import           Data.Coerce            (coerce)
import           Data.IORef
import           Data.Word
import qualified Foreign.C.Types        as C
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc  (allocaBytes, alloca)
import           Foreign.Marshal.Utils  (with)
import           Foreign.Ptr
import           Foreign.Storable       (poke, sizeOf)

import qualified Foreign.C.Error        as CError

import           Network.Socket
import           Network.Socket (SocketOption(..))
import           Network.Socket.Address (SocketAddress(..))

import qualified FFI
import qualified Settings

recv_test target = allocaBytes buffsize $ \buffer ->  do
  s <- socket AF_INET Datagram defaultProtocol
  FFI.init (\_ bytes len _ _ ->
               fromIntegral <$> sendBufTo s bytes (fromIntegral len) target
           )
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
  with 0 $ \fromLenPtr ->
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
    FFI.setsockopt socket level name ptr (fromIntegral $ sizeOf value)

-- defaultSocketOptions socket

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
