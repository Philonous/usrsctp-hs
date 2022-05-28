{-# LANGUAGE TemplateHaskell #-}

module Settings where

import           Data.Char
import qualified Data.List           as List
import           Data.Word
import           Foreign.C.Types

import           Language.Haskell.TH


-- usrsctp.h contains a long list of "sysctl" variables that can be with
-- functions of the form usrsctp_sysctl_set_$variable. The variables are all
-- uint32_t, so we can use template haskell to import all of them automatically

$( let underScoreToCamel [] = []
       underScoreToCamel ('_':c:cs) = (toUpper c : underScoreToCamel cs)
       underScoreToCamel (c:cs) = c : underScoreToCamel cs
       upcase [] = []
       upcase (c:cs) = toUpper c : cs
       usrsctp_sysctl_decls name =
         let cGet = "usrsctp_sysctl_get_" ++ name
             cSet = "usrsctp_sysctl_set_" ++ name
             haskellSuffix =
               case List.stripPrefix "sctp_" name of
                 Nothing -> error $ "usrsctpSysctlDecl: " ++ name
                                     ++ " does not have prefix sctp_"
                 Just name' -> upcase $ underScoreToCamel name'
             haskellGet = mkName $ "sysctlGet" ++ haskellSuffix
             haskellSet = mkName $ "sysctlSet" ++ haskellSuffix
         in
           [ forImpD CCall Safe cSet haskellSet [t|Word32 -> IO CInt|]
           , forImpD CCall Safe cGet haskellGet [t|IO Word32|]
           ]
  in sequence $ usrsctp_sysctl_decls =<<
        ["sctp_sendspace"
        ,"sctp_recvspace"
        ,"sctp_auto_asconf"
        ,"sctp_multiple_asconfs"
        ,"sctp_ecn_enable"
        ,"sctp_pr_enable"
        ,"sctp_auth_enable"
        ,"sctp_asconf_enable"
        ,"sctp_reconfig_enable"
        ,"sctp_nrsack_enable"
        ,"sctp_pktdrop_enable"
        ,"sctp_no_csum_on_loopback"
        ,"sctp_peer_chunk_oh"
        ,"sctp_max_burst_default"
        ,"sctp_max_chunks_on_queue"
        ,"sctp_min_split_point"
        ,"sctp_delayed_sack_time_default"
        ,"sctp_sack_freq_default"
        ,"sctp_system_free_resc_limit"
        ,"sctp_asoc_free_resc_limit"
        ,"sctp_heartbeat_interval_default"
        ,"sctp_pmtu_raise_time_default"
        ,"sctp_shutdown_guard_time_default"
        ,"sctp_secret_lifetime_default"
        ,"sctp_rto_max_default"
        ,"sctp_rto_min_default"
        ,"sctp_rto_initial_default"
        ,"sctp_init_rto_max_default"
        ,"sctp_valid_cookie_life_default"
        ,"sctp_init_rtx_max_default"
        ,"sctp_assoc_rtx_max_default"
        ,"sctp_path_rtx_max_default"
        ,"sctp_add_more_threshold"
        ,"sctp_nr_incoming_streams_default"
        ,"sctp_nr_outgoing_streams_default"
        ,"sctp_cmt_on_off"
        ,"sctp_cmt_use_dac"
        ,"sctp_use_cwnd_based_maxburst"
        ,"sctp_nat_friendly"
        ,"sctp_L2_abc_variable"
        ,"sctp_mbuf_threshold_count"
        ,"sctp_do_drain"
        ,"sctp_hb_maxburst"
        ,"sctp_abort_if_one_2_one_hits_limit"
        ,"sctp_min_residual"
        ,"sctp_max_retran_chunk"
        ,"sctp_logging_level"
        ,"sctp_default_cc_module"
        ,"sctp_default_frag_interleave"
        ,"sctp_mobility_base"
        ,"sctp_mobility_fasthandoff"
        ,"sctp_inits_include_nat_friendly"
        ,"sctp_udp_tunneling_port"
        ,"sctp_enable_sack_immediately"
        ,"sctp_vtag_time_wait"
        ,"sctp_blackhole"
        ,"sctp_sendall_limit"
        ,"sctp_diag_info_code"
        ,"sctp_fr_max_burst_default"
        ,"sctp_path_pf_threshold"
        ,"sctp_default_ss_module"
        ,"sctp_rttvar_bw"
        ,"sctp_rttvar_rtt"
        ,"sctp_rttvar_eqret"
        ,"sctp_steady_step"
        ,"sctp_use_dccc_ecn"
        ,"sctp_buffer_splitting"
        ,"sctp_initial_cwnd"
        ])
