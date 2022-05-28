module Structs where

import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Data.Bits

#include "usrsctp.h"

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
