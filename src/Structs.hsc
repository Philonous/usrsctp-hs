{-# LANGUAGE StrictData #-}

module Structs where

import Data.Bits
import Data.Word
import Foreign.C.Types
import Foreign.Ptr            (castPtr, Ptr)
import Foreign.Storable
import Network.Socket         (SockAddr(..))
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

-- struct sockaddr_conn
data SockaddrConn = SockaddrConn
  { -- sockaddrConnSconnFamily :: #{type uint16_t}
    -- Family is always AF_CONN
    sockaddrConnSconnPort :: #{type uint16_t}
  , sockaddrConnSconnAddr :: Ptr () -- ^ Opaque value, but can't be null!bSct
  }

foreign import ccall unsafe "arpa/inet.h htons" htons :: Word16 -> Word16
foreign import ccall unsafe "arpa/inet.h ntohs" ntohs :: Word16 -> Word16

instance Storable SockaddrConn where
  sizeOf _ = #size struct sockaddr_conn
  alignment _ =  #alignment struct sockaddr_conn
  peek ptr = do
    -- Family is always AF_CONN
    -- sockaddrConnSconnFamily <- #{peek struct sockaddr_conn, sconn_family} ptr
    sockaddrConnSconnPort <- ntohs <$> #{peek struct sockaddr_conn, sconn_port} ptr
    sockaddrConnSconnAddr <- #{peek struct sockaddr_conn, sconn_addr} ptr
    return SockaddrConn{..}
  poke ptr SockaddrConn{..} = do
    #{poke struct sockaddr_conn, sconn_family} ptr
      (#{const AF_CONN} :: #{type uint16_t})
    #{poke struct sockaddr_conn, sconn_port} ptr (htons sockaddrConnSconnPort)
    #{poke struct sockaddr_conn, sconn_addr} ptr sockaddrConnSconnAddr

--------------------------------------------------------------------------------
-- SndInfo ---------------------------------------------------------------------
--------------------------------------------------------------------------------

data SndInfo = SndInfo
   { sndInfoSid :: Word16
   , sndInfoFlags :: Word16
   , sndInfoPpid :: Word32
   , sndInfoContext :: Word32
   , sndInfoAssocId :: Word32
   } deriving Show

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
  } deriving Show

instance Storable PrInfo where
  sizeOf _ = #size struct sctp_prinfo
  alignment _ = #alignment struct sctp_prinfo
  poke ptr PrInfo{..} = do
    #{poke struct sctp_prinfo, pr_policy} ptr prInfoPolicy
    #{poke struct sctp_prinfo, pr_value} ptr prInfoValue
  peek = error "PrInfo.peek: not implemented"

data AuthInfo = AuthInfo {authInfoKeynumber :: Word16}
  deriving Show

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
  } deriving Show

foreign import ccall unsafe "memset"
  memset :: Ptr a -> Word8 -> CSize -> IO ()

instance Storable SendvSpa where
  sizeOf _ = #size struct sctp_sendv_spa
  alignment _ = #alignment struct sctp_sendv_spa
  peek _ = error "SendvSpa Storable.peek: not implemented"
  poke ptr SendvSpa{..} = do
    memset ptr 0 (fromIntegral $ sizeOf (undefined :: SendvSpa))
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
  } deriving Show

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

--------------------------------------------------------------------------------
-- recvv -----------------------------------------------------------------------
--------------------------------------------------------------------------------

-- struct sctp_rcvinfo
data Rcvinfo = Rcvinfo
  { rcvinfoRcvSid :: #{type uint16_t}
  , rcvinfoRcvSsn :: #{type uint16_t}
  , rcvinfoRcvFlags :: #{type uint16_t}
  , rcvinfoRcvPpid :: #{type uint32_t}
  , rcvinfoRcvTsn :: #{type uint32_t}
  , rcvinfoRcvCumtsn :: #{type uint32_t}
  , rcvinfoRcvContext :: #{type uint32_t}
  , rcvinfoRcvAssocId :: #{type sctp_assoc_t}
  }

instance Storable Rcvinfo where
  sizeOf _ = #size struct sctp_rcvinfo
  alignment _ =  #alignment struct sctp_rcvinfo
  peek ptr = do
    rcvinfoRcvSid <- #{peek struct sctp_rcvinfo, rcv_sid} ptr
    rcvinfoRcvSsn <- #{peek struct sctp_rcvinfo, rcv_ssn} ptr
    rcvinfoRcvFlags <- #{peek struct sctp_rcvinfo, rcv_flags} ptr
    rcvinfoRcvPpid <- #{peek struct sctp_rcvinfo, rcv_ppid} ptr
    rcvinfoRcvTsn <- #{peek struct sctp_rcvinfo, rcv_tsn} ptr
    rcvinfoRcvCumtsn <- #{peek struct sctp_rcvinfo, rcv_cumtsn} ptr
    rcvinfoRcvContext <- #{peek struct sctp_rcvinfo, rcv_context} ptr
    rcvinfoRcvAssocId <- #{peek struct sctp_rcvinfo, rcv_assoc_id} ptr
    return Rcvinfo{..}
  poke ptr Rcvinfo{..} = do
    #{poke struct sctp_rcvinfo, rcv_sid} ptr rcvinfoRcvSid
    #{poke struct sctp_rcvinfo, rcv_ssn} ptr rcvinfoRcvSsn
    #{poke struct sctp_rcvinfo, rcv_flags} ptr rcvinfoRcvFlags
    #{poke struct sctp_rcvinfo, rcv_ppid} ptr rcvinfoRcvPpid
    #{poke struct sctp_rcvinfo, rcv_tsn} ptr rcvinfoRcvTsn
    #{poke struct sctp_rcvinfo, rcv_cumtsn} ptr rcvinfoRcvCumtsn
    #{poke struct sctp_rcvinfo, rcv_context} ptr rcvinfoRcvContext
    #{poke struct sctp_rcvinfo, rcv_assoc_id} ptr rcvinfoRcvAssocId


-- struct sctp_nxtinfo
data Nxtinfo = Nxtinfo
  { nxtinfoNxtSid :: #{type uint16_t}
  , nxtinfoNxtFlags :: #{type uint16_t}
  , nxtinfoNxtPpid :: #{type uint32_t}
  , nxtinfoNxtLength :: #{type uint32_t}
  , nxtinfoNxtAssocId :: #{type sctp_assoc_t}
  }

instance Storable Nxtinfo where
  sizeOf _ = #size struct sctp_nxtinfo
  alignment _ =  #alignment struct sctp_nxtinfo
  peek ptr = do
    nxtinfoNxtSid <- #{peek struct sctp_nxtinfo, nxt_sid} ptr
    nxtinfoNxtFlags <- #{peek struct sctp_nxtinfo, nxt_flags} ptr
    nxtinfoNxtPpid <- #{peek struct sctp_nxtinfo, nxt_ppid} ptr
    nxtinfoNxtLength <- #{peek struct sctp_nxtinfo, nxt_length} ptr
    nxtinfoNxtAssocId <- #{peek struct sctp_nxtinfo, nxt_assoc_id} ptr
    return Nxtinfo{..}
  poke ptr Nxtinfo{..} = do
    #{poke struct sctp_nxtinfo, nxt_sid} ptr nxtinfoNxtSid
    #{poke struct sctp_nxtinfo, nxt_flags} ptr nxtinfoNxtFlags
    #{poke struct sctp_nxtinfo, nxt_ppid} ptr nxtinfoNxtPpid
    #{poke struct sctp_nxtinfo, nxt_length} ptr nxtinfoNxtLength
    #{poke struct sctp_nxtinfo, nxt_assoc_id} ptr nxtinfoNxtAssocId

-- struct sctp_recvv_rn
data RecvvRn = RecvvRn
  { recvvRnRecvvRcvinfo :: Rcvinfo
  , recvvRnRecvvNxtinfo :: Nxtinfo
  }

instance Storable RecvvRn where
  sizeOf _ = #size struct sctp_recvv_rn
  alignment _ =  #alignment struct sctp_recvv_rn
  peek ptr = do
    recvvRnRecvvRcvinfo <- #{peek struct sctp_recvv_rn, recvv_rcvinfo} ptr
    recvvRnRecvvNxtinfo <- #{peek struct sctp_recvv_rn, recvv_nxtinfo} ptr
    return RecvvRn{..}
  poke ptr RecvvRn{..} = do
    #{poke struct sctp_recvv_rn, recvv_rcvinfo} ptr recvvRnRecvvRcvinfo
    #{poke struct sctp_recvv_rn, recvv_nxtinfo} ptr recvvRnRecvvNxtinfo

--------------------------------------------------------------------------------
-- Notificaitons ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- notification types

newtype EventType = EventType Word16
  deriving (Show, Eq, Bits)

eventAssocChange :: EventType
eventAssocChange = EventType #{const SCTP_ASSOC_CHANGE}
eventPeerAddrChange :: EventType
eventPeerAddrChange = EventType #{const SCTP_PEER_ADDR_CHANGE}
eventRemoteError :: EventType
eventRemoteError = EventType #{const SCTP_REMOTE_ERROR}
eventSendFailed :: EventType
eventSendFailed = EventType #{const SCTP_SEND_FAILED}
eventShutdownEvent :: EventType
eventShutdownEvent = EventType #{const SCTP_SHUTDOWN_EVENT}
eventAdaptationIndication :: EventType
eventAdaptationIndication = EventType #{const SCTP_ADAPTATION_INDICATION}
eventPartialDeliveryEvent :: EventType
eventPartialDeliveryEvent = EventType #{const SCTP_PARTIAL_DELIVERY_EVENT}
eventAuthenticationEvent :: EventType
eventAuthenticationEvent = EventType #{const SCTP_AUTHENTICATION_EVENT}
eventStreamResetEvent :: EventType
eventStreamResetEvent = EventType #{const SCTP_STREAM_RESET_EVENT}
eventSenderDryEvent :: EventType
eventSenderDryEvent = EventType #{const SCTP_SENDER_DRY_EVENT}
eventNotificationsStoppedEvent :: EventType
eventNotificationsStoppedEvent = EventType #{const SCTP_NOTIFICATIONS_STOPPED_EVENT}
eventAssocResetEvent :: EventType
eventAssocResetEvent = EventType #{const SCTP_ASSOC_RESET_EVENT}
eventStreamChangeEvent :: EventType
eventStreamChangeEvent = EventType #{const SCTP_STREAM_CHANGE_EVENT}
eventSendFailedEvent :: EventType
eventSendFailedEvent = EventType #{const SCTP_SEND_FAILED_EVENT}

newtype RcvFlags = RcvFlags CInt deriving (Eq,Bits)

recvvFlagMsgNotification = RcvFlags #{const MSG_NOTIFICATION}
recvvFlagMsgEor = RcvFlags #{const MSG_EOR}


-- union sctp_notification
data Notification
  = NotificationAssocChange AssocChange
  | NotificationSenderDryEvent SenderDryEvent
  | NotificationStreamResetEvent StreamResetEvent
  | Other Word16 -- ^ not implemented


instance Storable Notification where
  sizeOf _ = #size union sctp_notification
  alignment _ = #alignment union sctp_notification
  peek ptr = do
    tp <- #{ peek struct sctp_tlv, sn_type } ptr :: IO Word16
    case tp  of
      #{const SCTP_ASSOC_CHANGE} -> NotificationAssocChange <$> peek (castPtr ptr)
      #{const SCTP_SENDER_DRY_EVENT} -> NotificationSenderDryEvent
                                          <$> peek (castPtr ptr)
      #{const SCTP_STREAM_RESET_EVENT} -> NotificationStreamResetEvent
                                           <$> peek (castPtr ptr)
      _ -> return $ Other tp
  poke _ _ = error "Storable.poke for union sctp_notification: not implemented"

-- struct sctp_assoc_change
---------------------------
data AssocChange = AssocChange
  { assocChangeSacType :: #{type uint16_t}
  , assocChangeSacFlags :: #{type uint16_t}
  , assocChangeSacLength :: #{type uint32_t}
  , assocChangeSacState :: #{type uint16_t}
  , assocChangeSacError :: #{type uint16_t}
  , assocChangeSacOutboundStreams :: #{type uint16_t}
  , assocChangeSacInboundStreams :: #{type uint16_t}
  , assocChangeSacAssocId :: #{type sctp_assoc_t}
  } deriving (Show)

instance Storable AssocChange where
  sizeOf _ = #size struct sctp_assoc_change
  alignment _ =  #alignment struct sctp_assoc_change
  peek ptr = do
    assocChangeSacType <- #{peek struct sctp_assoc_change, sac_type} ptr
    assocChangeSacFlags <- #{peek struct sctp_assoc_change, sac_flags} ptr
    assocChangeSacLength <- #{peek struct sctp_assoc_change, sac_length} ptr
    assocChangeSacState <- #{peek struct sctp_assoc_change, sac_state} ptr
    assocChangeSacError <- #{peek struct sctp_assoc_change, sac_error} ptr
    assocChangeSacOutboundStreams <- #{peek struct sctp_assoc_change, sac_outbound_streams} ptr
    assocChangeSacInboundStreams <- #{peek struct sctp_assoc_change, sac_inbound_streams} ptr
    assocChangeSacAssocId <- #{peek struct sctp_assoc_change, sac_assoc_id} ptr
    return AssocChange{..}
  poke ptr AssocChange{..} = do
    #{poke struct sctp_assoc_change, sac_type} ptr assocChangeSacType
    #{poke struct sctp_assoc_change, sac_flags} ptr assocChangeSacFlags
    #{poke struct sctp_assoc_change, sac_length} ptr assocChangeSacLength
    #{poke struct sctp_assoc_change, sac_state} ptr assocChangeSacState
    #{poke struct sctp_assoc_change, sac_error} ptr assocChangeSacError
    #{poke struct sctp_assoc_change, sac_outbound_streams} ptr assocChangeSacOutboundStreams
    #{poke struct sctp_assoc_change, sac_inbound_streams} ptr assocChangeSacInboundStreams
    #{poke struct sctp_assoc_change, sac_assoc_id} ptr assocChangeSacAssocId

-- struct sctp_sender_dry_event
-------------------------------
data SenderDryEvent = SenderDryEvent
  { senderDryEventSenderDryType :: #{type uint16_t}
  , senderDryEventSenderDryFlags :: #{type uint16_t}
  , senderDryEventSenderDryLength :: #{type uint32_t}
  , senderDryEventSenderDryAssocId :: #{type sctp_assoc_t}
  }

instance Storable SenderDryEvent where
  sizeOf _ = #size struct sctp_sender_dry_event
  alignment _ =  #alignment struct sctp_sender_dry_event
  peek ptr = do
    senderDryEventSenderDryType <- #{peek struct sctp_sender_dry_event, sender_dry_type} ptr
    senderDryEventSenderDryFlags <- #{peek struct sctp_sender_dry_event, sender_dry_flags} ptr
    senderDryEventSenderDryLength <- #{peek struct sctp_sender_dry_event, sender_dry_length} ptr
    senderDryEventSenderDryAssocId <- #{peek struct sctp_sender_dry_event, sender_dry_assoc_id} ptr
    return SenderDryEvent{..}
  poke ptr SenderDryEvent{..} = do
    #{poke struct sctp_sender_dry_event, sender_dry_type} ptr senderDryEventSenderDryType
    #{poke struct sctp_sender_dry_event, sender_dry_flags} ptr senderDryEventSenderDryFlags
    #{poke struct sctp_sender_dry_event, sender_dry_length} ptr senderDryEventSenderDryLength
    #{poke struct sctp_sender_dry_event, sender_dry_assoc_id} ptr senderDryEventSenderDryAssocId


-- struct sctp_stream_reset_event
---------------------------------
data StreamResetEvent = StreamResetEvent
  { streamResetEventStrresetType :: #{type uint16_t}
  , streamResetEventStrresetFlags :: #{type uint16_t}
  , streamResetEventStrresetLength :: #{type uint32_t}
  , streamResetEventStrresetAssocId :: #{type sctp_assoc_t}
  , streamResetEventStrresetStreamList :: #{type uint16_t}
  }

instance Storable StreamResetEvent where
  sizeOf _ = #size struct sctp_stream_reset_event
  alignment _ =  #alignment struct sctp_stream_reset_event
  peek ptr = do
    streamResetEventStrresetType <- #{peek struct sctp_stream_reset_event, strreset_type} ptr
    streamResetEventStrresetFlags <- #{peek struct sctp_stream_reset_event, strreset_flags} ptr
    streamResetEventStrresetLength <- #{peek struct sctp_stream_reset_event, strreset_length} ptr
    streamResetEventStrresetAssocId <- #{peek struct sctp_stream_reset_event, strreset_assoc_id} ptr
    streamResetEventStrresetStreamList <- #{peek struct sctp_stream_reset_event, strreset_stream_list} ptr
    return StreamResetEvent{..}
  poke ptr StreamResetEvent{..} = do
    #{poke struct sctp_stream_reset_event, strreset_type} ptr streamResetEventStrresetType
    #{poke struct sctp_stream_reset_event, strreset_flags} ptr streamResetEventStrresetFlags
    #{poke struct sctp_stream_reset_event, strreset_length} ptr streamResetEventStrresetLength
    #{poke struct sctp_stream_reset_event, strreset_assoc_id} ptr streamResetEventStrresetAssocId
    #{poke struct sctp_stream_reset_event, strreset_stream_list} ptr streamResetEventStrresetStreamList
