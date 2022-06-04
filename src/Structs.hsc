{-# LANGUAGE StrictData #-}

module Structs where

import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Data.Bits
import Foreign.Ptr (castPtr)
import Network.Socket   (SockAddr(..))
import Network.Socket.Address (pokeSocketAddress, peekSocketAddress)


#include "usrsctp.h"

--------------------------------------------------------------------------------
-- Dealing With SockAddr -------------------------------------------------------
--------------------------------------------------------------------------------

newtype SockAddrStorage = SockAddrStorage SockAddr

instance Storable SockAddrStorage where
  sizeOf _ = #size struct sockaddr_storage
  alignment _ = #alignment struct sockaddr_storage
  poke _ (SockAddrStorage SockAddrUnix{}) = error "poke SockAddrUnix not supported"
  poke ptr (SockAddrStorage addr) = pokeSocketAddress ptr addr
  peek ptr = SockAddrStorage <$> peekSocketAddress (castPtr ptr)

data SndInfo = SndInfo
   { sndInfoSid :: Word16
   , sndInfoFlags :: Word16
   , sndInfoPpid :: Word32
   , sndInfoContext :: Word32
   , sndInfoAssocId :: Word32
   }

instance Storable SndInfo where
  sizeOf _ = #size struct sctp_sndinfo
  alignment _ = #alignment struct sctp_sndinfo
  poke ptr SndInfo{..} = do
    #{poke struct sctp_sndinfo, snd_sid} ptr sndInfoSid
    #{poke struct sctp_sndinfo, snd_flags} ptr sndInfoFlags
    #{poke struct sctp_sndinfo, snd_ppid} ptr sndInfoPpid
    #{poke struct sctp_sndinfo, snd_context} ptr sndInfoContext
    #{poke struct sctp_sndinfo, snd_assoc_id} ptr sndInfoAssocId
  peek = error "SndInfo.peek: not implemented"


data PrInfo = PrInfo
  { prInfoPolicy :: Word16
  , prInfoValue :: Word32
  }

instance Storable PrInfo where
  sizeOf _ = #size struct sctp_prinfo
  alignment _ = #alignment struct sctp_prinfo
  poke ptr PrInfo{..} = do
    #{poke struct sctp_prinfo, pr_policy} ptr prInfoPolicy
    #{poke struct sctp_prinfo, pr_value} ptr prInfoValue
  peek = error "PrInfo.peek: not implemented"

data AuthInfo = AuthInfo {authInfoKeynumber :: Word16}

instance Storable AuthInfo where
  sizeOf _ = #size struct sctp_authinfo
  alignment _ = #alignment struct sctp_authinfo
  poke ptr AuthInfo{..} = do
    #{poke struct sctp_authinfo, auth_keynumber} ptr authInfoKeynumber
  peek = error "AuthInfo.peek: not implemented"


data SendvSpa = SendvSpa
  { sendvSpaSndInfo :: Maybe SndInfo
  , sendvSpaPrInfo :: Maybe PrInfo
  , sendvSpaAuthInfo :: Maybe AuthInfo
  }

instance Storable SendvSpa where
  sizeOf _ = #size struct sctp_sendv_spa
  alignment _ = #alignment struct sctp_sendv_spa
  poke ptr SendvSpa{..} = do
    sndInfoValid <- case sendvSpaSndInfo of
      Just si -> do
        #{poke struct sctp_sendv_spa, sendv_sndinfo} ptr si
        return #{const SCTP_SEND_SNDINFO_VALID}
      Nothing -> return 0
    prinfoValid <- case sendvSpaPrInfo of
      Just pri -> do
        #{poke struct sctp_sendv_spa, sendv_prinfo} ptr pri
        return #{const SCTP_SEND_PRINFO_VALID}
      Nothing -> return 0
    authInfoValid <- case sendvSpaAuthInfo of
      Just ai -> do
        #{poke struct sctp_sendv_spa, sendv_authinfo} ptr ai
        return #{const SCTP_SEND_AUTHINFO_VALID}
      Nothing -> return 0

    let  flags :: Word32
         flags = sndInfoValid .|. prinfoValid .|. authInfoValid
    #{poke struct sctp_sendv_spa, sendv_flags} ptr flags


--------------------------------------------------------------------------------
-- RecvInfo --------------------------------------------------------------------
--------------------------------------------------------------------------------

data RcvInfo = RcvInfo
  { rcvInfoSid     :: Word16
  , rcvInfoSsn     :: Word16
  , rcvInfoFlags   :: Word16
  , rcvInfoPpid    :: Word32
  , rcvInfoTsn     :: Word32
  , rcvInfoCumtsn  :: Word32
  , rcvInfoContext :: Word32
  , rcvInfoAssocId :: Word32
  }

instance Storable RcvInfo where
  sizeOf _ = #size struct sctp_rcvinfo
  alignment _ = #alignment struct sctp_rcvinfo
  peek ptr = do
    rcvInfoSid     <- #{peek struct sctp_rcvinfo, rcv_sid} ptr
    rcvInfoSsn     <- #{peek struct sctp_rcvinfo, rcv_ssn} ptr
    rcvInfoFlags   <- #{peek struct sctp_rcvinfo, rcv_flags} ptr
    rcvInfoPpid    <- #{peek struct sctp_rcvinfo, rcv_ppid} ptr
    rcvInfoTsn     <- #{peek struct sctp_rcvinfo, rcv_tsn} ptr
    rcvInfoCumtsn  <- #{peek struct sctp_rcvinfo, rcv_cumtsn} ptr
    rcvInfoContext <- #{peek struct sctp_rcvinfo, rcv_context} ptr
    rcvInfoAssocId <- #{peek struct sctp_rcvinfo, rcv_assoc_id} ptr
    return RcvInfo{..}

--------------------------------------------------------------------------------
-- Socket Options --------------------------------------------------------------
--------------------------------------------------------------------------------

futureAssoc :: #{type sctp_assoc_t}
futureAssoc = #{const SCTP_FUTURE_ASSOC}
currentAssoc :: #{type sctp_assoc_t}
currentAssoc = #{const SCTP_CURRENT_ASSOC}
allAssoc :: #{type sctp_assoc_t}
allAssoc = #{const SCTP_ALL_ASSOC}

-- Notification Types

assocChange :: Word16
assocChange                = #{const SCTP_ASSOC_CHANGE                }
peerAddrChange :: Word16
peerAddrChange             = #{const SCTP_PEER_ADDR_CHANGE            }
remoteError :: Word16
remoteError                = #{const SCTP_REMOTE_ERROR                }
sendFailed :: Word16
sendFailed                 = #{const SCTP_SEND_FAILED                 }
shutdownEvent :: Word16
shutdownEvent              = #{const SCTP_SHUTDOWN_EVENT              }
adaptationIndication :: Word16
adaptationIndication       = #{const SCTP_ADAPTATION_INDICATION       }
partialDeliveryEvent :: Word16
partialDeliveryEvent       = #{const SCTP_PARTIAL_DELIVERY_EVENT      }
authenticationEvent :: Word16
authenticationEvent        = #{const SCTP_AUTHENTICATION_EVENT        }
streamResetEvent :: Word16
streamResetEvent           = #{const SCTP_STREAM_RESET_EVENT          }
senderDryEvent :: Word16
senderDryEvent             = #{const SCTP_SENDER_DRY_EVENT            }
notificationsStoppedEvent :: Word16
notificationsStoppedEvent  = #{const SCTP_NOTIFICATIONS_STOPPED_EVENT }
assocResetEvent :: Word16
assocResetEvent            = #{const SCTP_ASSOC_RESET_EVENT           }
streamChangeEvent :: Word16
streamChangeEvent          = #{const SCTP_STREAM_CHANGE_EVENT         }
sendFailedEvent :: Word16
sendFailedEvent            = #{const SCTP_SEND_FAILED_EVENT           }


-- struct sctp_assoc_value
data AssocValue = AssocValue
  { assocValueAssocId :: #{type sctp_assoc_t}
  , assocValueAssocValue :: Word32
  }

instance Storable AssocValue where
  sizeOf _ = #size struct sctp_assoc_value
  alignment _ = #alignment struct sctp_assoc_value

  peek ptr = do
    assocValueAssocId <- #{peek struct sctp_assoc_value, assoc_id} ptr
    assocValueAssocValue <- #{peek struct sctp_assoc_value, assoc_value} ptr
    return AssocValue{..}

  poke ptr AssocValue{..} = do
    #{poke struct sctp_assoc_value, assoc_id} ptr assocValueAssocId
    #{poke struct sctp_assoc_value, assoc_value} ptr assocValueAssocValue

-- struct sctp_event
data Event = Event
  { eventSeAssocId :: #{type sctp_assoc_t}
  , eventSeType :: Word16
  , eventSeOn :: Word8
  }

instance Storable Event where
  sizeOf _ = #size struct sctp_event
  alignment _ = #alignment struct sctp_event

  peek ptr = do
    eventSeAssocId <- #{peek struct sctp_event, se_assoc_id} ptr
    eventSeType <- #{peek struct sctp_event, se_type} ptr
    eventSeOn <- #{peek struct sctp_event, se_on} ptr
    return Event{..}

  poke ptr Event{..} = do
    #{poke struct sctp_event, se_assoc_id} ptr eventSeAssocId
    #{poke struct sctp_event, se_type} ptr eventSeType
    #{poke struct sctp_event, se_on} ptr eventSeOn

-- struct sctp_paddrparams
data Paddrparams = Paddrparams
  { paddrparamsSppAddress :: SockAddr
  , paddrparamsSppAssocId :: #{type sctp_assoc_t}
  , paddrparamsSppHbinterval :: Word32
  , paddrparamsSppPathmtu :: Word32
  , paddrparamsSppFlags :: Word32
  , paddrparamsSppIpv6Flowlabel :: Word32
  , paddrparamsSppPathmaxrxt :: Word16
  , paddrparamsSppDscp :: Word8
  }

instance Storable Paddrparams where
  sizeOf _ = #size struct sctp_paddrparams
  alignment _ = #alignment struct sctp_paddrparams

  peek ptr = do
    SockAddrStorage paddrparamsSppAddress
      <- #{peek struct sctp_paddrparams, spp_address} ptr
    paddrparamsSppAssocId <- #{peek struct sctp_paddrparams, spp_assoc_id} ptr
    paddrparamsSppHbinterval <- #{peek struct sctp_paddrparams, spp_hbinterval} ptr
    paddrparamsSppPathmtu <- #{peek struct sctp_paddrparams, spp_pathmtu} ptr
    paddrparamsSppFlags <- #{peek struct sctp_paddrparams, spp_flags} ptr
    paddrparamsSppIpv6Flowlabel <- #{peek struct sctp_paddrparams, spp_ipv6_flowlabel} ptr
    paddrparamsSppPathmaxrxt <- #{peek struct sctp_paddrparams, spp_pathmaxrxt} ptr
    paddrparamsSppDscp <- #{peek struct sctp_paddrparams, spp_dscp} ptr
    return Paddrparams{..}

  poke ptr Paddrparams{..} = do
    #{poke struct sctp_paddrparams, spp_address} ptr
      (SockAddrStorage paddrparamsSppAddress)
    #{poke struct sctp_paddrparams, spp_assoc_id} ptr paddrparamsSppAssocId
    #{poke struct sctp_paddrparams, spp_hbinterval} ptr paddrparamsSppHbinterval
    #{poke struct sctp_paddrparams, spp_pathmtu} ptr paddrparamsSppPathmtu
    #{poke struct sctp_paddrparams, spp_flags} ptr paddrparamsSppFlags
    #{poke struct sctp_paddrparams, spp_ipv6_flowlabel} ptr paddrparamsSppIpv6Flowlabel
    #{poke struct sctp_paddrparams, spp_pathmaxrxt} ptr paddrparamsSppPathmaxrxt
    #{poke struct sctp_paddrparams, spp_dscp} ptr paddrparamsSppDscp


-- struct sctp_initmsg
data Initmsg = Initmsg
  { initmsgSinitNumOstreams :: Word16
  , initmsgSinitMaxInstreams :: Word16
  , initmsgSinitMaxAttempts :: Word16
  , initmsgSinitMaxInitTimeo :: Word16
  }

instance Storable Initmsg where
  sizeOf _ = #size struct sctp_initmsg
  alignment _ = #alignment struct sctp_initmsg

  peek ptr = do
    initmsgSinitNumOstreams <- #{peek struct sctp_initmsg, sinit_num_ostreams} ptr
    initmsgSinitMaxInstreams <- #{peek struct sctp_initmsg, sinit_max_instreams} ptr
    initmsgSinitMaxAttempts <- #{peek struct sctp_initmsg, sinit_max_attempts} ptr
    initmsgSinitMaxInitTimeo <- #{peek struct sctp_initmsg, sinit_max_init_timeo} ptr
    return Initmsg{..}

  poke ptr Initmsg{..} = do
    #{poke struct sctp_initmsg, sinit_num_ostreams} ptr initmsgSinitNumOstreams
    #{poke struct sctp_initmsg, sinit_max_instreams} ptr initmsgSinitMaxInstreams
    #{poke struct sctp_initmsg, sinit_max_attempts} ptr initmsgSinitMaxAttempts
    #{poke struct sctp_initmsg, sinit_max_init_timeo} ptr initmsgSinitMaxInitTimeo
