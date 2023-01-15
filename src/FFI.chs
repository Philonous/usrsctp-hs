{-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}

module FFI
  ( module Structs
  , module FFI
  )
where

import           Data.Bits              ((.|.))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Coerce
import qualified Data.List              as List
import           Data.Word
import           Foreign.C.Error        as CError
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils  (with)
import           Foreign.Ptr
import           Foreign.Storable
import           Network.Socket         hiding (Socket)
import           Network.Socket.Address (SocketAddress(..))

import           Structs

#include <usrsctp.h>

{# context lib="usrsctp" prefix="usrsctp" #}

{# typedef size_t CSize #}


type WriteCallback = Ptr () -> Ptr Word8 -> CSize -> Word8 -> Word8 -> IO CSize
type LogFun = Ptr CChar -> IO ()

foreign import ccall safe "init_debug"
  usrsctp_init :: CUShort -> FunPtr WriteCallback -> FunPtr LogFun -> IO ()

foreign import ccall "wrapper"
  mkWriteCallback :: WriteCallback -> IO (FunPtr WriteCallback)


foreign import ccall "wrapper"
  mkLogFun :: LogFun -> IO (FunPtr LogFun)



-- NB: The conn-send callback MUST return 0 on successful send, NOT the number
-- ob bytes sent! Any other value is interpreted as an error
init :: FFI.WriteCallback -> Maybe (ByteString -> IO ()) -> IO ()
init callback mbLogFun = do
  callbackPtr <- mkWriteCallback callback
  logFunPtr <-
    case mbLogFun of
      Nothing -> return nullFunPtr
      Just logFun -> do
        let logFun' ptr = BS.packCString ptr >>= logFun
        mkLogFun logFun'
  usrsctp_init
    0 -- Set UDP port to 0 to disable UDP encapsulation
    callbackPtr
    logFunPtr

-- initDebug :: FFI.WriteCallback -> IO ()
-- initDebug callback = do
--   callbackPtr <- mkWriteCallback callback
--   usrsctp_init_debug
--     0 -- Set UDP port to 0 to disable UDP encapsulation
--     callbackPtr


conninput :: Ptr () -- ^ This pointer is only used to identify the connection and
                   -- never dereferenced, it's OK to pass an arbitrary value
                   -- like a connection number
         -> Ptr Word8 -> CSize -> IO ()
conninput connection bytes len =
  {# call usrsctp_conninput #} connection (castPtr bytes) (coerce len) 0
{-# INLINE conninput #-}

{#pointer *socket as Socket newtype #}

aF_CONN :: CInt
aF_CONN = {#const AF_CONN #}

socket domain (GeneralSocketType tp) =
  {#call usrsctp_socket #} domain tp ipprotoSctp nullFunPtr nullFunPtr 0 nullPtr
  where
    ipprotoSctp = 132 -- {#const IPPROTO_SCTP #}

bind :: Socket -> SockaddrConn -> IO ()
bind sock addr = with addr $ \addrPtr -> do
  _ <- throwErrnoIfMinus1 "usrsctp_bind"
         $ {#call usrsctp_bind #} sock (castPtr addrPtr)
                                  (fromIntegral $ sizeOf addr)
  return ()

-- {#fun usrsctp_bind as bind {`Socket', with* `SockaddrConn'&}
--   -> `Int' 'throwErrnoOnMinus1 "usrsctp_bind"'*- #}

{#fun usrsctp_close as close {`Socket'} -> `()' #}

connect :: Socket -> SockaddrConn -> IO ()
connect sock addr = with addr $ \addrPtr -> do
  _ <- throwErrnoIfMinus1Except "usrsctp_connect"
         [(eINPROGRESS, return ())]
         $ {#call usrsctp_connect#} sock (castPtr addrPtr)
                                   (fromIntegral $ sizeOf addr)
  return ()

{#enum define Policy { SCTP_PR_SCTP_NONE as None
                     , SCTP_PR_SCTP_TTL as TTL
                     , SCTP_PR_SCTP_BUF as Buffer
                     , SCTP_PR_SCTP_RTX as Retransmit
                     } #}

withSpa spa f = with spa $ \ptr -> do
  f (castPtr ptr, {#sizeof sctp_sendv_spa#})

infoTypeSpa = {#const SCTP_SENDV_SPA #}

sendv = {# call sendv #}

recvv = {# call usrsctp_recvv #}

{#enum define RecvvInfotype
  { SCTP_RECVV_NOINFO  as RecvvNoinfo
  , SCTP_RECVV_RCVINFO as RecvvRcvinfo
  , SCTP_RECVV_NXTINFO as RecvvNxtinfo
  , SCTP_RECVV_RN      as RecvvRN
  } #}

registerAddress = {# call usrsctp_register_address #}
deregisterAddress = {# call usrsctp_deregister_address #}


{# fun usrsctp_setsockopt as setsockopt {`Socket', `CInt', `CInt', `Ptr ()', `CUInt'}
  -> `Int' 'throwErrnoOnMinus1 "usrsctp_setsockopt"'*- #}

{# fun usrsctp_getsockopt as getsockopt {`Socket', `CInt', `CInt', `Ptr ()', `CUInt'}
  -> `Int' 'throwErrnoOnMinus1 "usrsctp_setsockopt"'*- #}

{# fun usrsctp_set_non_blocking as setNonBlocking {`Socket', `Bool'}
  -> `Int' 'throwErrnoOnMinus1 "usrsctp_set_non_blocking"'*- #}

{# fun usrsctp_shutdown as shutdown {`Socket', `CInt'}
  -> `Int' 'throwErrnoOnMinus1 "usrsctp_shutdown"'*- #}

{# enum define SppFlag
  { SPP_HB_ENABLE       as SPP_HB_ENABLE
  , SPP_HB_DISABLE      as SPP_HB_DISABLE
  , SPP_HB_DEMAND       as SPP_HB_DEMAND
  , SPP_PMTUD_ENABLE    as SPP_PMTUD_ENABLE
  , SPP_PMTUD_DISABLE   as SPP_PMTUD_DISABLE
  , SPP_HB_TIME_IS_ZERO as SPP_HB_TIME_IS_ZERO
  , SPP_IPV6_FLOWLABEL  as SPP_IPV6_FLOWLABEL
  , SPP_DSCP            as SPP_DSCP
  } #}

{# fun usrsctp_finish as finish {} -> `Int' #}

type UpcallCallback = Socket -> Ptr () -> CInt -> IO ()

foreign import ccall "wrapper"
  mkUpcallCallback :: UpcallCallback -> IO (FunPtr (UpcallCallback))

setUpcall = {#call usrsctp_set_upcall #}

eventRead = {#const SCTP_EVENT_READ #}
eventWrite = {#const SCTP_EVENT_WRITE #}

{#enum define SctpEvent
  { SCTP_EVENT_READ as  SctpEventRead
  , SCTP_EVENT_WRITE as SctpEventWrite
  , SCTP_EVENT_ERROR as SctpEventError
  } #}

getEvents = {#call usrsctp_get_events #}

{#enum define SACState
  { SCTP_COMM_UP        as SACStateCommUp
  , SCTP_COMM_LOST      as SACStateCommLost
  , SCTP_RESTART        as SACStateRestart
  , SCTP_SHUTDOWN_COMP  as SACStateShutdownComp
  , SCTP_CANT_STR_ASSOC as SACStateCantStrAssoc
  } deriving (Show, Eq) #}

{# enum define SctpDebug
  { SCTP_DEBUG_NONE as DebugNone
  , SCTP_DEBUG_ALL as DebugAll
  }
#}

--------------------------------------------------------------------------------
--  Helpers --------------------------------------------------------------------
--------------------------------------------------------------------------------

throwErrnoOnMinus1 descr (-1) = CError.throwErrno descr
throwErrnoOnMinus1 _ _ = return ()

throwErrnoIfMinus1Except :: (Num a, Eq a) =>
                            String
                         -> [(Errno, IO b)]
                         -> IO a
                         -> IO (Either b a)
throwErrnoIfMinus1Except descr excepts m = do
  res <- m
  case res of
    (-1) -> do
      en <- getErrno
      case List.lookup en excepts of
        Nothing -> throwErrno descr
        Just k -> Left <$> k
    _ -> return $ Right res


withSockAddrLen :: SockAddr -> ((Ptr (), CUInt) -> IO a) -> IO a
withSockAddrLen addr f = allocaBytes addrLen $ \addrPtr -> do
  pokeSocketAddress addrPtr addr
  f (addrPtr, fromIntegral addrLen)
  where
    addrLen = sizeOfSocketAddress addr

withSockAddr :: SockAddr -> (Ptr () -> IO a) -> IO a
withSockAddr addr f = withSockAddrLen addr $ \(ptr, len) -> f ptr
