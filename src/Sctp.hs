{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Sctp where

import           Control.Concurrent       (threadDelay, forkIO)
import           Control.Concurrent.Async
import           Control.Monad            (forever, unless)
import qualified Control.Monad.Catch      as Ex
import           Data.Bits
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.ByteString.Internal as BS
import           Data.ByteString.Unsafe   (unsafeUseAsCStringLen, unsafePackCStringLen)
import qualified Data.Foldable            as Foldable
import           Data.List                (foldl')
import qualified Data.List                as List
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Foreign.C
import           Foreign.C.Error
import           Foreign.Ptr
import           System.Exit
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

import qualified Network.Socket           as Socket
import           Network.Socket.Address   (SocketAddress(..))

import qualified FFI
import qualified Settings
import qualified SocketOptions

import qualified SctpPacket

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
  , sockNotificationBuffer :: IORef [ByteString]
  }

data RecvFlags = RecvFlags
  { recvFlagEor :: Bool
  , recvFlagNotification :: Bool
  } deriving Show

toRecvFlags flg =
  RecvFlags
  { recvFlagEor          = testFlag FFI.recvvFlagMsgEor flg
  , recvFlagNotification = testFlag FFI.recvvFlagMsgNotification flg
  }

debugPacket :: String -> Ptr a -> Int -> IO ()
debugPacket prefix ptr len = do
  bs <- BS.packCStringLen (castPtr ptr, len)
  unless (BS.length bs == len) $ do
    hPutStrLn stderr ("packCStringLen exploded, "
      ++ (show $ BS.length bs) ++ " /= " ++ (show len))
    exitFailure
  debug $ prefix ++ case SctpPacket.parse bs of
    Left e -> "could not parse packet: " ++ show e ++ " ; " ++ show bs
             ++ "\n" ++ show (SctpPacket.debugParsePacket bs)
    Right packet -> SctpPacket.ppChunks packet
  where
    elided bs =
      if BS.length bs < 100
      then (show bs)
      else (show (BS.take 90 bs) ++ " (" ++ show (BS.length bs - 90) ++ " bytes elided)")


sockReadBuf :: SctpSocket -> Int -> Ptr Word8 -> IO (Int, (FFI.RcvInfo, RecvFlags))
sockReadBuf SctpSocket{..} buffSize buffer = go
  where
    go =
      recvv sockRaw buffSize buffer >>= \case
        Nothing -> do
           threadDelay 300000 -- @TODO ???
           go
        Just (read, info, flags) -> return (read, (info, toRecvFlags flags))

  -- where
  --   go = do
  --     haveData <- atomicModifyIORef' sockPendingRecvCount $ \c -> (0, c > 0)
  --     if haveData
  --     then (do
  --       (read, info, flags) <- recvv sockRaw buffSize buffer
  --       return (read, (info, toRecvFlags flags))
  --      )
  --     else (do
  --       threadDelay 10000 -- @TODO ???
  --       go
  --       )

sockRead :: SctpSocket -> Int -> IO (ByteString, (FFI.RcvInfo, RecvFlags))
sockRead sock buffSize = BS.createUptoN' buffSize $ sockReadBuf sock buffSize


handleNotification :: FFI.Notification -> IO ()
handleNotification (FFI.NotificationAssocChange
                     FFI.AssocChange{FFI.assocChangeSacState = state}) =
  debug $ "Notification: AssocChange (" ++ (show $ (toEnum $ fromIntegral state :: FFI.SACState)) ++ ")"
handleNotification FFI.NotificationSenderDryEvent{} = debug "Notification: SenderDryEvent"
handleNotification FFI.NotificationStreamResetEvent{} = debug "Notification: StreamReset"
handleNotification (FFI.Other other) = debug $ "Notification: Unknown (" ++ show other ++ ")"

recv sock buffsize = do
  res@(bytes, (info, flags)) <- sockRead sock buffsize
  case recvFlagNotification flags of
    False -> return res
    True -> do
      modifyIORef (sockNotificationBuffer sock) $ (bytes :)
      case recvFlagEor flags of
        False -> return ()
        True -> do
          parts <- readIORef (sockNotificationBuffer sock)
          writeIORef (sockNotificationBuffer sock) []
          notification <-
            unsafeUseAsCStringLen (BS.concat $ List.reverse parts)
              $ \(ptr, _len) -> peek (castPtr ptr)
          handleNotification notification
      recv sock buffsize


sctpSocket dSend dRecv = do
  sock <- FFI.socket FFI.aF_CONN Socket.Stream
  defaultSocketOpts sock (1280 - 8 - 40) -- UDP, IPv6
  pendingRecvCount <- newIORef 0
  pendingFlushCount <- newIORef 0
  let upcallHandler s _flags = do
        debug "Upcall"
        events <- getEvents s
        Foldable.for_ events $ \case
          FFI.SctpEventRead -> do
            debug "Events: read"
            modifyIORef pendingRecvCount (+1)
          FFI.SctpEventWrite -> do
            debug "Events: write"
            modifyIORef pendingFlushCount (+1)
          _ -> return ()
  setUpcall sock upcallHandler
  let bufferSize = 65536
  buffer <- mallocBytes bufferSize
  notificationBuffer <- newIORef []

  -- TODO, threads need to be handled properly
  forkIO $ forever $
    Ex.handle (\(e :: Ex.SomeException) -> do
                  debug $ "Receive thread got exception: " ++ show e
                  Ex.throwM e
              )( do
                   received <- dRecv buffer bufferSize
                   debug $ "raw received: " ++ show received
                   debugPacket "packet= " buffer received
                   FFI.conninput (intPtrToPtr 1) buffer (fromIntegral received)
               )


  return
    SctpSocket{ sockRaw = sock
              , sockPendingRecvCount = pendingRecvCount
              , sockPendingFlushCount = pendingFlushCount
              , sockRecvBuffer = buffer
              , sockRecvBufferSize = bufferSize
              , sockDownstreamSend = dSend
              , sockDownstreamRecv = dRecv
              , sockNotificationBuffer = notificationBuffer
              }

udpToSctp address s = do
  sctpSocket (\buf size -> Socket.sendBufTo s buf size address )
    (\buf bufSize -> do
        (cnt, _) <- Socket.recvBufFrom s buf bufSize
        return cnt
    )

-- rcv pull SctpSocket{..} = do
--   pendingRcvs <- readIORef sockPendingRecvCount
--   if pendingRcvs > 0
--     then recv

--   return ()

bind :: SctpSocket -> IntPtr -> Word16 -> IO ()
bind SctpSocket{..} addr port =
  FFI.bind sockRaw
    FFI.SockaddrConn
      { sockaddrConnSconnPort = port
      , sockaddrConnSconnAddr = intPtrToPtr addr
      }

connect :: SctpSocket -> IntPtr -> Word16 -> IO ()
connect SctpSocket{..} addr port =
  FFI.connect sockRaw
    FFI.SockaddrConn
      { sockaddrConnSconnPort = port
      , sockaddrConnSconnAddr = intPtrToPtr addr
      }

debug str = do
  hPutStrLn stderr str
  hFlush stderr

recvTest = do
  let local = Socket.SockAddrInet 6000 (Socket.tupleToHostAddress (127,0,0,1))
  let remote = Socket.SockAddrInet 6001 (Socket.tupleToHostAddress (127,0,0,1))
  debug "Socket"
  s <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
  debug "bind"
  Socket.bind s local

  debug "init"
  FFI.init (\_ bytes len _ _ -> do
               debug $ "raw send: " ++ show len
               debugPacket "packet= " bytes (fromIntegral len)
               fromIntegral <$> Socket.sendBufTo s bytes (fromIntegral len) remote
           )
  defaultSettings

  debug "registerAddress"
  FFI.registerAddress (intPtrToPtr 1)
  socket <- udpToSctp local s
  debug "bind"
  bind socket 1 5000
  debug "connect"
  connect socket 1 5000
  debug "Reading"
  go socket
  where
    go socket = do
      (bs, (info, flags)) <- recv socket 4096
      debug $ "Info: " ++ show info
      debug $ "Flags: " ++ show flags
      BS.putStrLn bs
      go socket
    buffsize = 2 ^ (16 :: Int) :: Int

sendTest = do
  let local = Socket.SockAddrInet 6001 (Socket.tupleToHostAddress (127,0,0,1))
  let remote = Socket.SockAddrInet 6000 (Socket.tupleToHostAddress (127,0,0,1))
  debug "Socket"
  s <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
  debug "bind"
  Socket.bind s local

  debug "init"
  FFI.init (\_ bytes len _ _ -> do
               debug $ "raw send: " ++ show len
               debugPacket "packet= " bytes (fromIntegral len)
               sent <- Socket.sendBufTo s bytes (fromIntegral len) remote
               return $ fromIntegral sent
           )
  defaultSettings

  debug "registerAddress"
  FFI.registerAddress (intPtrToPtr 1)
  socket <- udpToSctp local s
  debug "bind"
  bind socket 1 5000
  debug "connect"
  connect socket 1 5000
  debug "loop"
  forkIO $ allocaBytes 4096 $ \buffer -> forever $ do
    (bs, _) <- recv socket 4096
    debug $ "received: "++ show bs
    threadDelay 100000
  threadDelay 1000000
  go (0 :: Integer) (sockRaw socket)
  where
    go n socket = do
      let bs = "Hullo: " <> (Text.encodeUtf8 . Text.pack $ show n)
      sent <- sendv socket bs (FFI.SendvSpa Nothing Nothing Nothing) 0
      putStrLn $ "Sent " ++ show sent ++ " bytes"
      threadDelay 3000000
      go (n+1) socket




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
    throwErrnoIfMinus1 "usrsctp_sendv" $
      FFI.sendv socket (castPtr ptr) (fromIntegral len)
                       nullPtr 0
                       (castPtr infoPtr)
                       (fromIntegral $ sizeOf (undefined :: FFI.SendvSpa))
                       (fromIntegral FFI.infoTypeSpa) flags


recvv :: FFI.Socket -> Int -> Ptr Word8 -> IO (Maybe (Int, FFI.RcvInfo, FFI.RcvFlags))
recvv socket recvBufferLen recvBuffer =
  with (fromIntegral $ sizeOf (undefined :: FFI.SockAddrStorage)) $ \fromLenPtr ->
  with 0 $ \infoTypePtr ->
  with 0 $ \msgFlagsPtr ->
  with (fromIntegral $ sizeOf (undefined :: FFI.Rcvinfo)) $ \recvInfoLenPtr ->
  alloca $ \(recvInfoBuffer :: Ptr FFI.RcvInfo) -> do
    received <- FFI.recvv socket (castPtr recvBuffer) (fromIntegral recvBufferLen)
        nullPtr -- received sockaddr
        fromLenPtr -- in/out: length of receivedSockedLen
        (castPtr recvInfoBuffer)
        recvInfoLenPtr
        infoTypePtr msgFlagsPtr
    case received of
      (-1) -> do
        en <- getErrno
        if en == eAGAIN || en == eWOULDBLOCK
          then return Nothing
          else throwErrno "usrsctp_recvv"
      _ -> do
        rcvinfo <- peek recvInfoBuffer
        flags <- peek msgFlagsPtr
        return $ Just (fromIntegral received, rcvinfo, FFI.RcvFlags flags)

setSocketOption socket (Socket.SockOpt level name) value = do
  with value $ \ptr ->
    FFI.setsockopt socket level name (castPtr ptr) (fromIntegral $ sizeOf value)

getSocketOption :: forall a. Storable a => FFI.Socket -> Socket.SocketOption -> IO a
getSocketOption socket (Socket.SockOpt level name) = alloca $ \ptr -> do
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

defaultSocketOpts :: FFI.Socket -> Word32 -> IO ()
defaultSocketOpts socket mtu = do
  FFI.setNonBlocking socket True
  -- SCTP must stop sending after the lower layer is shut down, so disable
  -- linger

  debug "setsocketopt: linger"
  setSocketOption socket Socket.Linger Socket.StructLinger{sl_onoff = 1, sl_linger = 0 }
  debug "setsocketopt: sctpEnableStreamReset"
  setSocketOption socket SocketOptions.sctpEnableStreamReset
    FFI.AssocValue { assocValueAssocId = FFI.allAssoc
                       , assocValueAssocValue = 1
                       }
  debug "setsocketopt: sctpRecvrcvinfo"
  setSocketOption socket SocketOptions.sctpRecvrcvinfo (1 :: CInt)
  debug "setsocketopt: assocChange"
  subscribeEvent socket FFI.eventAssocChange
  debug "setsocketopt: senderDryEvent"
  subscribeEvent socket FFI.eventSenderDryEvent
  debug "setsocketopt: streamResetEvent"
  subscribeEvent socket FFI.eventStreamResetEvent

  debug "setsocketopt: nodelay"
  setSocketOption socket SocketOptions.sctpNodelay (1 :: CInt)

  let paddrparams =
        FFI.Paddrparams
        { paddrparamsSppAddress = Socket.SockAddrInet 0 0
        , paddrparamsSppAssocId = 0
        , paddrparamsSppHbinterval = 0
        , paddrparamsSppPathmtu = mtu - 12 -- SCTP header
        , paddrparamsSppFlags = fromFlags [FFI.SPP_PMTUD_DISABLE]
        , paddrparamsSppIpv6Flowlabel = 0
        , paddrparamsSppPathmaxrxt = 0
        , paddrparamsSppDscp = 0
        }
  debug "setsocketopt: paddrparams"
  setSocketOption socket SocketOptions.sctpPeerAddrParams paddrparams

  let initmsg = FFI.Initmsg
                  { initmsgSinitNumOstreams = 65535
                  , initmsgSinitMaxInstreams = 65535
                  , initmsgSinitMaxAttempts = 0
                  , initmsgSinitMaxInitTimeo = 0
                  }
  debug "setsocketopt: initmsg"
  setSocketOption socket SocketOptions.sctpInitmsg initmsg

  debug "setsocketopt: fragmentInterleave"
  setSocketOption socket SocketOptions.sctpFragmentInterleave (0 :: CInt)

  return ()

fromFlags :: (Enum a, Integral b) => [a] -> b
fromFlags = fromIntegral . foldl' (.|.) 0 . map fromEnum
