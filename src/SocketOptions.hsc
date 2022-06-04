module SocketOptions where

import Network.Socket (SocketOption(..))
import Foreign.C (CInt)

#include "usrsctp.h"

sctpSockOpt :: CInt -> SocketOption
sctpSockOpt = SockOpt #{const IPPROTO_SCTP }

sctpRtoinfo              :: SocketOption
sctpRtoinfo              = sctpSockOpt #{const  SCTP_RTOINFO                }
sctpAssocinfo            :: SocketOption
sctpAssocinfo            = sctpSockOpt #{const  SCTP_ASSOCINFO              }
sctpInitmsg              :: SocketOption
sctpInitmsg              = sctpSockOpt #{const  SCTP_INITMSG                }
sctpNodelay              :: SocketOption
sctpNodelay              = sctpSockOpt #{const  SCTP_NODELAY                }
sctpAutoclose            :: SocketOption
sctpAutoclose            = sctpSockOpt #{const  SCTP_AUTOCLOSE              }
sctpPrimaryAddr          :: SocketOption
sctpPrimaryAddr          = sctpSockOpt #{const  SCTP_PRIMARY_ADDR           }
sctpAdaptationLayer      :: SocketOption
sctpAdaptationLayer      = sctpSockOpt #{const  SCTP_ADAPTATION_LAYER       }
sctpDisableFragments     :: SocketOption
sctpDisableFragments     = sctpSockOpt #{const  SCTP_DISABLE_FRAGMENTS      }
sctpPeerAddrParams       :: SocketOption
sctpPeerAddrParams       = sctpSockOpt #{const  SCTP_PEER_ADDR_PARAMS       }
sctpIWantMappedV4Addr    :: SocketOption
sctpIWantMappedV4Addr    = sctpSockOpt #{const  SCTP_I_WANT_MAPPED_V4_ADDR  }
sctpMaxseg               :: SocketOption
sctpMaxseg               = sctpSockOpt #{const  SCTP_MAXSEG                 }
sctpDelayedSack          :: SocketOption
sctpDelayedSack          = sctpSockOpt #{const  SCTP_DELAYED_SACK           }
sctpFragmentInterleave   :: SocketOption
sctpFragmentInterleave   = sctpSockOpt #{const  SCTP_FRAGMENT_INTERLEAVE    }
sctpPartialDeliveryPoint :: SocketOption
sctpPartialDeliveryPoint = sctpSockOpt #{const  SCTP_PARTIAL_DELIVERY_POINT }
sctpHmacIdent            :: SocketOption
sctpHmacIdent            = sctpSockOpt #{const  SCTP_HMAC_IDENT             }
sctpAuthActiveKey        :: SocketOption
sctpAuthActiveKey        = sctpSockOpt #{const  SCTP_AUTH_ACTIVE_KEY        }
sctpAutoAsconf           :: SocketOption
sctpAutoAsconf           = sctpSockOpt #{const  SCTP_AUTO_ASCONF            }
sctpMaxBurst             :: SocketOption
sctpMaxBurst             = sctpSockOpt #{const  SCTP_MAX_BURST              }
sctpContext              :: SocketOption
sctpContext              = sctpSockOpt #{const  SCTP_CONTEXT                }
sctpExplicitEor          :: SocketOption
sctpExplicitEor          = sctpSockOpt #{const  SCTP_EXPLICIT_EOR           }
sctpReusePort            :: SocketOption
sctpReusePort            = sctpSockOpt #{const  SCTP_REUSE_PORT             }
sctpEvent                :: SocketOption
sctpEvent                = sctpSockOpt #{const  SCTP_EVENT                  }
sctpRecvrcvinfo          :: SocketOption
sctpRecvrcvinfo          = sctpSockOpt #{const  SCTP_RECVRCVINFO            }
sctpRecvnxtinfo          :: SocketOption
sctpRecvnxtinfo          = sctpSockOpt #{const  SCTP_RECVNXTINFO            }
sctpDefaultSndinfo       :: SocketOption
sctpDefaultSndinfo       = sctpSockOpt #{const  SCTP_DEFAULT_SNDINFO        }
sctpDefaultPrinfo        :: SocketOption
sctpDefaultPrinfo        = sctpSockOpt #{const  SCTP_DEFAULT_PRINFO         }
sctpRemoteUdpEncapsPort  :: SocketOption
sctpRemoteUdpEncapsPort  = sctpSockOpt #{const  SCTP_REMOTE_UDP_ENCAPS_PORT }
sctpEcnSupported         :: SocketOption
sctpEcnSupported         = sctpSockOpt #{const  SCTP_ECN_SUPPORTED          }
sctpPrSupported          :: SocketOption
sctpPrSupported          = sctpSockOpt #{const  SCTP_PR_SUPPORTED           }
sctpAuthSupported        :: SocketOption
sctpAuthSupported        = sctpSockOpt #{const  SCTP_AUTH_SUPPORTED         }
sctpAsconfSupported      :: SocketOption
sctpAsconfSupported      = sctpSockOpt #{const  SCTP_ASCONF_SUPPORTED       }
sctpReconfigSupported    :: SocketOption
sctpReconfigSupported    = sctpSockOpt #{const  SCTP_RECONFIG_SUPPORTED     }
sctpNrsackSupported      :: SocketOption
sctpNrsackSupported      = sctpSockOpt #{const  SCTP_NRSACK_SUPPORTED       }
sctpPktdropSupported     :: SocketOption
sctpPktdropSupported     = sctpSockOpt #{const  SCTP_PKTDROP_SUPPORTED      }
sctpMaxCwnd              :: SocketOption
sctpMaxCwnd              = sctpSockOpt #{const  SCTP_MAX_CWND               }
sctpEnableStreamReset    :: SocketOption
sctpEnableStreamReset    = sctpSockOpt #{const  SCTP_ENABLE_STREAM_RESET    }
sctpPluggableSs          :: SocketOption
sctpPluggableSs          = sctpSockOpt #{const  SCTP_PLUGGABLE_SS           }
sctpSsValue              :: SocketOption
sctpSsValue              = sctpSockOpt #{const  SCTP_SS_VALUE               }
sctpStatus               :: SocketOption
sctpStatus               = sctpSockOpt #{const  SCTP_STATUS                 }
sctpGetPeerAddrInfo      :: SocketOption
sctpGetPeerAddrInfo      = sctpSockOpt #{const  SCTP_GET_PEER_ADDR_INFO     }
sctpPeerAuthChunks       :: SocketOption
sctpPeerAuthChunks       = sctpSockOpt #{const  SCTP_PEER_AUTH_CHUNKS       }
sctpLocalAuthChunks      :: SocketOption
sctpLocalAuthChunks      = sctpSockOpt #{const  SCTP_LOCAL_AUTH_CHUNKS      }
sctpGetAssocNumber       :: SocketOption
sctpGetAssocNumber       = sctpSockOpt #{const  SCTP_GET_ASSOC_NUMBER       }
sctpGetAssocIdList       :: SocketOption
sctpGetAssocIdList       = sctpSockOpt #{const  SCTP_GET_ASSOC_ID_LIST      }
sctpTimeouts             :: SocketOption
sctpTimeouts             = sctpSockOpt #{const  SCTP_TIMEOUTS               }
sctpPrStreamStatus       :: SocketOption
sctpPrStreamStatus       = sctpSockOpt #{const  SCTP_PR_STREAM_STATUS       }
sctpPrAssocStatus        :: SocketOption
sctpPrAssocStatus        = sctpSockOpt #{const  SCTP_PR_ASSOC_STATUS        }
sctpSetPeerPrimaryAddr   :: SocketOption
sctpSetPeerPrimaryAddr   = sctpSockOpt #{const  SCTP_SET_PEER_PRIMARY_ADDR  }
sctpAuthChunk            :: SocketOption
sctpAuthChunk            = sctpSockOpt #{const  SCTP_AUTH_CHUNK             }
sctpAuthKey              :: SocketOption
sctpAuthKey              = sctpSockOpt #{const  SCTP_AUTH_KEY               }
sctpAuthDeactivateKey    :: SocketOption
sctpAuthDeactivateKey    = sctpSockOpt #{const  SCTP_AUTH_DEACTIVATE_KEY    }
sctpAuthDeleteKey        :: SocketOption
sctpAuthDeleteKey        = sctpSockOpt #{const  SCTP_AUTH_DELETE_KEY        }
sctpResetStreams         :: SocketOption
sctpResetStreams         = sctpSockOpt #{const  SCTP_RESET_STREAMS          }
sctpResetAssoc           :: SocketOption
sctpResetAssoc           = sctpSockOpt #{const  SCTP_RESET_ASSOC            }
sctpAddStreams           :: SocketOption
sctpAddStreams           = sctpSockOpt #{const  SCTP_ADD_STREAMS            }
