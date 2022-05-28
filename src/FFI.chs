{-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}

module FFI
  ( module Structs
  , module FFI
  )
where

import Data.Bits              ((.|.))
import Data.Coerce
import Data.Word
import Foreign.C.Error        as CError
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils  (with)
import Foreign.Ptr
import Foreign.Storable
import Network.Socket         hiding (Socket)
import Network.Socket.Address (SocketAddress(..))

import Structs

#include <usrsctp.h>

{# context lib="usrsctp" prefix="usrsctp" #}

{# typedef size_t CSize #}



type WriteCallback = Ptr () -> Ptr Word8 -> CSize -> Word8 -> Word8 -> IO CSize

foreign import ccall safe "Bindings.chs.h usrsctp_init"
  usrsctp_init :: CUShort -> FunPtr WriteCallback -> FunPtr () -> IO ()

foreign import ccall "wrapper"
  mkWriteCallback :: WriteCallback -> IO (FunPtr WriteCallback)

init :: FFI.WriteCallback -> IO ()
init callback = do
  callbackPtr <- mkWriteCallback callback
  usrsctp_init
    0 -- Set UDP port to 0 to disable UDP encapsulation
    callbackPtr
    nullFunPtr


conninput :: Ptr () -- ^ This pointer is only used to identify the connection and
                   -- never dereferenced, it's OK to pass an arbitrary value
                   -- like a connection number
         -> Ptr Word8 -> CSize -> IO ()
conninput connection bytes len =
  {# call usrsctp_conninput #} connection (castPtr bytes) (coerce len) 0
{-# INLINE conninput #-}

{#pointer *socket as Socket newtype #}

socket domain tp protocol =
  {#call usrsctp_socket #} domain tp protocol nullFunPtr nullFunPtr 0 nullPtr

{#fun usrsctp_bind as bind {`Socket', withSockAddrLen* `SockAddr'&}
  -> `Int' 'throwErrnoOnMinus1 "usrsctp_bind"'*- #}

{#fun usrsctp_close as close {`Socket'} -> `()' #}
{#fun usrsctp_connect as connect {`Socket', withSockAddrLen* `SockAddr'&}
  -> `Int' 'throwErrnoOnMinus1 "usrsctp_connect"'*- #}


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

registerAddress = {# call usrsctp_register_address #}
deregisterAddress = {# call usrsctp_deregister_address #}


{# fun usrsctp_setsockopt as setsockopt {`Socket', `CInt', `CInt', `Ptr ()', `CUInt'}
  -> `Int' 'throwErrnoOnMinus1 "usrsctp_setsockopt"'*- #}

--------------------------------------------------------------------------------
--  Helpers --------------------------------------------------------------------
--------------------------------------------------------------------------------

throwErrnoOnMinus1 descr (-1) = CError.throwErrno descr
throwErrnoOnMinus1 _ _ = return ()

withSockAddrLen :: SockAddr -> ((Ptr (), CUInt) -> IO a) -> IO a
withSockAddrLen addr f = allocaBytes addrLen $ \addrPtr -> do
  pokeSocketAddress addrPtr addr
  f (addrPtr, fromIntegral addrLen)
  where
    addrLen = sizeOfSocketAddress addr

withSockAddr :: SockAddr -> (Ptr () -> IO a) -> IO a
withSockAddr addr f = withSockAddrLen addr $ \(ptr, len) -> f ptr
