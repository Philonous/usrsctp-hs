{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Sctp
  ( module Sctp
  , module FFI
  ) where

import           Control.Concurrent       ( threadDelay, forkIO
                                          , MVar, takeMVar, tryPutMVar, newEmptyMVar)
import           Control.Concurrent.Async
import qualified Control.Concurrent.Async as Async
import           Control.Monad            (forever, unless)
import qualified Control.Monad.Catch      as Ex
import           Data.Bits
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.ByteString.Internal as BS
import           Data.ByteString.Unsafe   (unsafeUseAsCStringLen, unsafePackCStringLen)
import qualified Data.Foldable            as Foldable
import qualified Data.IntMap.Strict       as IMap
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
import           System.IO.Unsafe         (unsafePerformIO)

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
import           FFI                      (init, initDebug, registerAddress
                                          , SendvSpa(..))

import           GHC.Exts                 (touch#)
import           GHC.IO                   (IO (..))
import           GHC.IORef                (IORef (..))
import           GHC.STRef                (STRef (..))


import qualified Settings
import qualified SocketOptions

import qualified SctpPacket

testFlag :: Bits a => a -> a -> Bool
testFlag x y = x .&. y /= zeroBits

data GlobalState =
  GlobalState
  { globalStateConMap :: IORef (IMap.IntMap (Ptr Word8 -> Int -> Word8 -> Word8 -> IO Bool))
  , globalStateNextAddr :: IORef Int
  }


-- Initialize usersctp once and keep global state
-- Since we can only set a single downstream send function we keep a global
-- map of connections to send functions
{-# NOINLINE globalState #-}
globalState :: GlobalState
globalState = unsafePerformIO $ do
  map <- newIORef IMap.empty
  let conSend addr buffer len tos setDf = do
        let addr' = fromIntegral $ ptrToIntPtr addr
        cons <- readIORef map
        case IMap.lookup addr' cons of
          Nothing -> do -- connection doesn't exist
            debug $ "Could not find connection " ++ show addr'
            return 1 -- failure
          Just send -> do
            sentSuccess <- send buffer (fromIntegral len) tos setDf
            return $ if sentSuccess then 0 else 1
  FFI.init conSend
  defaultSettings
  nextAddr <- newIORef 1
  return $
    GlobalState
    { globalStateConMap = map
    , globalStateNextAddr = nextAddr
    }

data Socket = Socket
  { -- IORef, because we want to invalidate the socket on close
    -- Also, we can attach a finalizer to this IORef
    sockRaw :: IORef FFI.Socket
  -- Buffer for assembling partially received notifications
  , sockNotificationBuffer :: IORef [ByteString]
  -- recv and send should block when usrsctp isn't ready, sempahores are filled
  -- when usrsctp signals availability via upcall
  , sockReceiveFlag :: MVar ()
  -- , sockSendFlag :: MVar ()
  -- Hold on to connections so they don't get harvested
  , sockConnections :: IORef [Connection]
  }

data RecvFlags = RecvFlags
  { -- Last fragment of this chunk
    recvFlagEor :: Bool
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
      then show bs
      else (show (BS.take 90 bs) ++ " (" ++ show (BS.length bs - 90) ++ " bytes elided)")


handleNotification :: FFI.Notification -> IO ()
handleNotification (FFI.NotificationAssocChange
                     asc@FFI.AssocChange{FFI.assocChangeSacState = state}) =
  debug $ "Notification: AssocChange (" ++ (show (toEnum $ fromIntegral state :: FFI.SACState)) ++ "; " ++ show asc ++ ")"
handleNotification FFI.NotificationSenderDryEvent{} = debug "Notification: SenderDryEvent"
handleNotification FFI.NotificationStreamResetEvent{} = debug "Notification: StreamReset"
handleNotification (FFI.Other other) = debug $ "Notification: Unknown (" ++ show other ++ ")"

sockRead :: Socket -> Int -> IO (ByteString, (Maybe FFI.RcvInfo, FFI.RcvFlags))
sockRead sock buffsize = BS.createAndTrim' buffsize readBuffer
  where
    readBuffer buffer = do
      trySockReadBuf sock buffsize buffer >>= \case
        Nothing -> do
          -- wait for more input
          takeMVar $ sockReceiveFlag sock
          readBuffer buffer
        Just (read, info) -> return (0, read, info)


recv :: Socket -> Int -> IO (ByteString, Maybe FFI.RcvInfo, RecvFlags)
recv sock buffsize = go
  where
    go = do
      res@(bytes, (info, rawFlags)) <- sockRead sock buffsize
      let flags = toRecvFlags rawFlags
      case recvFlagNotification flags of
        False -> return (bytes, info, flags)
        True -> do
          modifyIORef (sockNotificationBuffer sock) (bytes :)
          case recvFlagEor flags of
            False -> return ()
            True -> do
              parts <- readIORef (sockNotificationBuffer sock)
              writeIORef (sockNotificationBuffer sock) []
              notification <-
                unsafeUseAsCStringLen (BS.concat $ List.reverse parts)
                  $ \(ptr, _len) -> peek (castPtr ptr)
              handleNotification notification
          go


socket = do
  receiveFlag <- newEmptyMVar
  sendFlag <- newEmptyMVar
  connections <- newIORef []
  let upcallHandler s _flags = do
        -- debug "Upcall"
        events <- getEvents s
        Foldable.for_ events $ \case
          FFI.SctpEventRead -> do
            -- debug "Events: read"
            _ <- tryPutMVar receiveFlag ()
            return ()
          FFI.SctpEventWrite -> do
            -- debug "Events: write"
            _ <- tryPutMVar sendFlag ()
            return ()
          _ -> return ()

  sock <- FFI.socket FFI.aF_CONN Socket.Stream
  setUpcall sock upcallHandler
  notificationBuffer <- newIORef []
  raw <- newIORef sock
  let sock =
        Socket{ sockRaw = raw
              , sockNotificationBuffer = notificationBuffer
              , sockReceiveFlag = receiveFlag
              -- , sockSendFlag = sendFlag
              , sockConnections = connections
              }

  -- Close uscrsctp socket when this Socket value goes out of scope
  mkWeakIORef raw (close sock)
  return sock

close :: Socket -> IO ()
close sock = do
  invalidateSocket sock (return ()) FFI.close
  -- Not strictly speaking necessary
  writeIORef (sockConnections sock) []
  where
    invalidateSocket sock error success = do
      cur <- atomicModifyIORef (sockRaw sock) $ \cur -> (FFI.Socket nullPtr, cur)
      if invalid cur then error else success cur
      where
        invalid (FFI.Socket ptr) = ptr == nullPtr

-- |
touchSocket :: Socket -> IO ()
touchSocket sock = do
  touch (sockRaw sock)
  touch (sockConnections sock)
  where
    -- This is stolen from the network package.
    touch (IORef (STRef mutVar)) =
      IO (\s -> (# touch# mutVar s, () #)) :: IO ()


withRawSocket sock f = do
  raw <- readIORef (sockRaw sock)
  res <- f raw
  touchSocket sock
  return res


-- | While usrsctp can send data via sockets, this isn't implemented in this
-- library. Instead, we rely on usrsctp's ability to use send and receive
-- callbacks. This also means that we don't care about the IP address of our
-- peers (they are handled by the underlying transport). Instead connections are
-- tracked via abstract handlers.
data Connection =
  Connection
  { connAddr :: IORef Int
  , connStop :: IO ()
  }

--   withRawSocket sock $ \raw ->
    -- defaultSocketOpts raw (1280 - 8 - 40) -- UDP, IPv6


connection send recv recvBufferSize = do
  addr <- atomicModifyIORef (globalStateNextAddr globalState) (\x -> (x+1, x))
  debug $ "Addr: " ++ show addr
  FFI.registerAddress $ intPtrToPtr (fromIntegral addr)
  -- TODO, threads need to be handled properly
  buffer <- mallocBytes recvBufferSize

  atomicModifyIORef (globalStateConMap globalState)
    $ \mp -> (IMap.insert addr send mp, ())

  -- Perhaps a more "elegant" solution would be to modify recv so it checks if
  -- data is available, and if not it calls the lower layer, which might even
  -- properly block on an fd which the runtime can handle better. But usrsctp
  -- can generate notifications independently of incoming data and they are
  -- handed to the user via recv.
  thread <- async $ forever $
    Ex.handle (\(e :: Ex.SomeException) -> do
                  debug $ "Receive thread got exception: " ++ show e
                  Ex.throwM e
              )( do
                   received <- recv buffer recvBufferSize
                   -- debug $ "raw received: " ++ show received
                   -- debugPacket "packet= " buffer received
                   FFI.conninput (intPtrToPtr $ fromIntegral addr)
                     buffer (fromIntegral received)
               )
  let stop = do
        uninterruptibleCancel thread
        atomicModifyIORef (globalStateConMap globalState)
          $ \mp ->(IMap.delete addr mp, ())

  addrRef <- newIORef addr
  -- Stop the thread when socket goes out of scope
  _ <- mkWeakIORef addrRef stop
  return
    Connection
    { connAddr = addrRef
    , connStop = stop
    }

-- | Create an estalished UDP connection. This is just a helper for simple
-- onto-to-one style connections. Real scenarios are probably more complex.
simpleUdpConnection localAddr remoteAddr = do
  udpSocket <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
  Socket.bind udpSocket localAddr
  Socket.connect udpSocket remoteAddr
  connection (\buf bufsize _ _ -> do
                 sent <- Socket.sendBuf udpSocket buf bufsize
                 return $ sent == bufsize
             )
    (\buf bufSize -> do
        Socket.recvBuf udpSocket buf bufSize
    ) 65507 {- Maximum UDP packet size -}

-- rcv pull Socket{..} = do
--   pendingRcvs <- readIORef sockPendingRecvCount
--   if pendingRcvs > 0
--     then recv

--   return ()

bind :: Socket -> Connection -> Word16 -> IO ()
bind sock conn port = withRawSocket sock $ \raw -> do
  addr <- fromIntegral <$> readIORef (connAddr conn)
  FFI.bind raw
    FFI.SockaddrConn
      { sockaddrConnSconnPort = port
      , sockaddrConnSconnAddr = intPtrToPtr addr
      }
  -- Keep the connection alive as long as the socket exists
  atomicModifyIORef (sockConnections sock) (\conns -> (conn : conns, ()))

connect :: Socket -> Connection -> Word16 -> IO ()
connect sock conn port = withRawSocket sock $ \raw -> do
  addr <- readIORef $ connAddr conn
  FFI.connect raw
    FFI.SockaddrConn
      { sockaddrConnSconnPort = port
      , sockaddrConnSconnAddr = intPtrToPtr $ fromIntegral addr
      }

debugCont = hPutStr stderr

debug str = do
  hPutStrLn stderr str
  hFlush stderr

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

sendv :: Socket -> ByteString -> FFI.SendvSpa -> C.CInt -> IO C.CLong
sendv sock bytes info flags = withRawSocket sock $ \raw ->
  unsafeUseAsCStringLen bytes $ \(ptr, len) ->
  with info $ \infoPtr -> do
    -- debug "test"
    -- info' <- peek infoPtr :: IO FFI.SendvSpa
    -- debug $ "re-peeked info: " ++ show info'
    throwErrnoIfMinus1 "usrsctp_sendv" $
      FFI.sendv raw (castPtr ptr) (fromIntegral len)
                    nullPtr 0
                    (castPtr infoPtr)
                    (fromIntegral $ sizeOf (undefined :: FFI.SendvSpa))
                    (fromIntegral FFI.infoTypeSpa) flags


trySockReadBuf :: Socket -> Int -> Ptr Word8 -> IO (Maybe (Int, (Maybe FFI.RcvInfo, FFI.RcvFlags)))
trySockReadBuf sock recvBufferLen recvBuffer =
  withRawSocket sock $ \raw ->
  with (fromIntegral $ sizeOf (undefined :: FFI.SockAddrStorage)) $ \fromLenPtr ->
  with 0 $ \infoTypePtr ->
  with 0 $ \msgFlagsPtr ->
  with (fromIntegral $ sizeOf (undefined :: FFI.Rcvinfo)) $ \recvInfoLenPtr ->
  alloca $ \(recvInfoBuffer :: Ptr FFI.RcvInfo) -> do
    received <- FFI.recvv raw (castPtr recvBuffer) (fromIntegral recvBufferLen)
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
        infoType <- peek infoTypePtr

        rcvinfo <- case toEnum (fromIntegral infoType) of
                     FFI.RecvvNoinfo -> return Nothing
                     FFI.RecvvRcvinfo -> Just <$> peek recvInfoBuffer
                     n -> do
                       debug "Recevied unexpected infor type"
                       return Nothing

        flags <- peek msgFlagsPtr
        return $ Just (fromIntegral received
                      , ( rcvinfo
                        , FFI.RcvFlags flags))

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
    Settings.sysctlSetDebugOn $ (fromIntegral $ fromEnum FFI.DebugAll)
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

  setSocketOption socket Socket.Linger Socket.StructLinger{sl_onoff = 1, sl_linger = 0 }
  setSocketOption socket SocketOptions.sctpEnableStreamReset
    FFI.AssocValue { assocValueAssocId = FFI.allAssoc
                       , assocValueAssocValue = 1
                       }
  setSocketOption socket SocketOptions.sctpRecvrcvinfo (1 :: CInt)
  subscribeEvent socket FFI.eventAssocChange
  subscribeEvent socket FFI.eventSenderDryEvent
  subscribeEvent socket FFI.eventStreamResetEvent

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
  setSocketOption socket SocketOptions.sctpPeerAddrParams paddrparams

  let initmsg = FFI.Initmsg
                  { initmsgSinitNumOstreams = 65535
                  , initmsgSinitMaxInstreams = 65535
                  , initmsgSinitMaxAttempts = 0
                  , initmsgSinitMaxInitTimeo = 0
                  }
  setSocketOption socket SocketOptions.sctpInitmsg initmsg
  setSocketOption socket SocketOptions.sctpFragmentInterleave (0 :: CInt)

  return ()

fromFlags :: (Enum a, Integral b) => [a] -> b
fromFlags = fromIntegral . foldl' (.|.) 0 . map fromEnum
