{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE LambdaCase      #-}

module Run
    ( runMain
    ) where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.Char             as Char
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           Data.Tuple            (swap)
import           Foreign.Ptr           (intPtrToPtr)
import           Foreign.Ptr           (castPtr)
import           Foreign.Marshal.Alloc (allocaBytes)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import qualified Network.Socket        as Socket

import qualified Sctp


data Options = Options
  { optionCompanion :: Bool
  , optionSend :: Bool
  , optionSlow :: Bool
  , optionDebug :: Bool
  , optionCount :: Maybe Int
  }

parseOpt descr str =
  case reads str of
    [] -> do
      hPutStrLn stderr $  "Could not read " ++ show str ++ "as " ++ descr
      exitFailure
    ((r, _): _) -> return r

options = do
  args <- getArgs
  let (ops, _noOps, errs) = getOpt Permute opts args
  case errs of
    [] -> (\ops -> foldl (.) Prelude.id ops defaultOptions) <$> sequence ops
    _ -> do
      hPutStrLn stderr $ unlines ("Error" : errs ++ [usageInfo "test" opts])
      exitFailure

  where
    opts =
      [ Option [] ["send"] (NoArg (return $ \o -> o {optionSend = True})) "send data"
      , Option [] ["slow"] (NoArg (return $ \o -> o {optionSlow = True})) "send/receive slowly"
      , Option [] ["companion"] (NoArg (return $ \o -> o {optionCompanion = True})) "use companion ports"
      , Option [] ["debug"] (NoArg (return $ \o -> o {optionDebug = True})) "debug logging"
      , Option [] ["count"] (ReqArg (\cntStr -> do
                                        cnt <- parseOpt "int" cntStr
                                        return $ \o -> o {optionCount = Just cnt})
                              "number of packets"
                            ) "stop after n"
      ]
    defaultOptions =
      Options
      { optionCompanion = False
      , optionSend = False
      , optionSlow = False
      , optionDebug = False
      , optionCount = Nothing
      }


runMain :: IO ()
runMain = do
  Sctp.setLogFun (BS.hPutStr stderr)
  Sctp.defaultSettings
  recvTest =<< options

-- debugCont = Sctp.debugCont

debug = Sctp.debug

recvTest opts = do
  let (local, remote) =
        (if optionCompanion opts then swap else Prelude.id)
          ( Socket.SockAddrInet 6000 (Socket.tupleToHostAddress (127,0,0,1))
          , Socket.SockAddrInet 6001 (Socket.tupleToHostAddress (127,0,0,1))
          )
  udpSocket <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
  Socket.bind udpSocket local
  conn <- Sctp.connection (\buf bufsize _ _ -> do
                              -- debug ">"
                              bs <- BS.packCStringLen (castPtr buf, bufsize)
                              debug $ "Sending " <> show bs
                              size <- Socket.sendBufTo udpSocket buf bufsize remote
                              return $ size == bufsize
                          )

  let bufSize = 65507 {- Maximum UDP packet size -}
  forkIO $ allocaBytes bufSize $ \buf ->
    forever $ do
        (len, _) <- Socket.recvBufFrom udpSocket buf bufSize
        debug "<"
        Sctp.conInput conn buf (fromIntegral len)

  debug "connect"
  socket <- Sctp.socket
  Sctp.defaultSocketOpts socket 1500
  Sctp.bind socket conn 5000
  Sctp.connect socket conn 5000
  when (optionSend opts) $ void . forkIO $ do
    threadDelay 1000000
    doSend opts socket
  debug "Reading"
  go socket
  forever $ do
    debug "Waiting."
    threadDelay 1000000
  where
    go socket = do
      (bs, mbInfo, flags) <- Sctp.recv socket 4096
      case BS.null bs of
        True -> do
          debug "Peer closed connection"
          Sctp.close socket
        False -> do
          case mbInfo of
            Nothing -> return ()
            Just info -> debug $ "Info: " ++ show info
          -- debug $ "Flags: " ++ show flags
          debug $ "Content: " ++ show (BS.takeWhile (/= (fromIntegral $ Char.ord '#')) bs)
          when (optionSlow opts) $ threadDelay 300000
          go socket
    buffsize = 2 ^ (16 :: Int) :: Int

doSend options socket = do
  go (0 :: Int)
  Sctp.close socket
  where
    go n = do
      let numBs = Text.encodeUtf8 (Text.pack $ show n)
          bs = numBs <> BS.replicate (1024 - 2 * BS.length numBs)
                         (fromIntegral $ Char.ord '#')
                     <> numBs
      sent <- Sctp.sendv socket bs (Sctp.SendvSpa Nothing Nothing Nothing) 0
      debug $ "Sent " ++ show n
      when (optionSlow options) $ threadDelay 100000
      case (optionCount options) of
        Just m | m <= n -> return ()
        _ -> go (n+1)


-- sendTest = do
--   let local = Socket.SockAddrInet 6001 (Socket.tupleToHostAddress (127,0,0,1))
--   let remote = Socket.SockAddrInet 6000 (Socket.tupleToHostAddress (127,0,0,1))
--   debug "Socket"
--   s <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
--   debug "bind"
--   Socket.bind s local

--   debug "init"
--   FFI.initDebug
--       (\_ bytes len _ _ -> do
--           debugCont $ "raw send: " ++ show len ++ " -> "
--           sent <- Socket.sendBufTo s bytes (fromIntegral len) remote
--           debug $ show sent
--           debugPacket "packet= " bytes (fromIntegral len)
--           return $ if fromIntegral sent < len then (-1) else 0
--       )
--   defaultSettings

--   debug "registerAddress"
--   FFI.registerAddress (intPtrToPtr 1)
--   socket <- udpToSctp local s
--   debug "bind"
--   bind socket 1 5000
--   debug "connect"
--   connect socket 1 5000
--   debug "loop"
--   forkIO $ allocaBytes 4096 $ \buffer -> forever $ do
--     (bs, _, _) <- recv socket 4096
--     debug $ "received: "++ show bs
--     threadDelay 100000
--   threadDelay 1000000
--   go (0 :: Integer) (sockRaw socket)
--   where
--     go n socket = do
--       let bs = "Hullo: " <> (Text.encodeUtf8 . Text.pack $ show n)
--       let sndInfo =
--             FFI.SndInfo
--             { FFI.sndInfoSid = 17
--             , FFI.sndInfoFlags = 0
--             , FFI.sndInfoPpid = 1
--             , FFI.sndInfoContext = 0
--             , FFI.sndInfoAssocId = 0
--             }
--       sent <- sendv socket bs (FFI.SendvSpa (Just sndInfo) Nothing Nothing) 0
--       putStrLn $ "Sent " ++ show sent ++ " bytes"
--       threadDelay 3000000
--       go (n+1) socket
