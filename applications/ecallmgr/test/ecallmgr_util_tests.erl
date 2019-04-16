-module(ecallmgr_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/ecallmgr.hrl").

maybe_sanitize_fs_value_test_() ->
    UTF8Bin = <<"Bör1-Goes2$%^ To4 Škofja Loka"/utf8>>,
    Expected = <<"Bör1-Goes2 To4 Škofja Loka"/utf8>>,

    Keys = [<<"Outbound-Caller-ID-Name">>
           ,<<"Outbound-Callee-ID-Name">>
           ,<<"Caller-ID-Name">>
           ,<<"Callee-ID-Name">>
           ],
    TestMsg = fun(KeyStr) ->
                      lists:flatten(["When sanitizing "
                                    ,kz_term:to_list(KeyStr)
                                    ,"'s value it should allow utf8 characters"
                                    ])
              end,
    [{TestMsg(Key)
     ,?_assertEqual(Expected, ecallmgr_util:maybe_sanitize_fs_value(Key, UTF8Bin))
     } || Key <- Keys].

build_simple_channels_test_() ->
    SIPHeaders = kz_json:from_list([{<<"X-Account-ID">>, <<"{account_id}">>}
                                   ,{<<"X-Billing-Number">>, <<"1234">>}
                                   ,{<<"X-Caller-Macro">>, <<"{caller_id_number}">>}
                                   ]),

    Endpoint = kz_json:from_list([{<<"Invite-Format">>, <<"username">>}
                                 ,{<<"To-User">>, <<"to_user">>}
                                 ,{<<"To-Realm">>, <<"to_realm">>}
                                 ,{<<"To-DID">>, <<"to_did">>}
                                 ]),
    [{"simple channels"
     ,?_assertEqual([<<"sofia/", ?SIP_INTERFACE, "/to_user@to_realm">>], ecallmgr_util:build_simple_channels([Endpoint]))
     }
    ,{"with custom sip headers"
     ,?_assertEqual([<<"[^^"
                       ?BRIDGE_CHANNEL_VAR_SEPARATOR
                       "sip_h_X-Caller-Macro=${caller_id_number}"
                       ?BRIDGE_CHANNEL_VAR_SEPARATOR
                       "sip_h_X-Billing-Number=1234"
                       ?BRIDGE_CHANNEL_VAR_SEPARATOR
                       "sip_h_X-Account-ID=${ecallmgr_Account-ID}]"
                       "sofia/", ?SIP_INTERFACE, "/to_user@to_realm"
                     >>
                    ]
                   ,ecallmgr_util:build_simple_channels(
                      [kz_json:set_value(<<"Custom-SIP-Headers">>, SIPHeaders, Endpoint)]
                     )
                   )
     }
    ].

build_bridge_string_test_() ->
    [{"check endpoints are in order when 'failover_when_all_unreg' is 'false'"
     ,bridge_string_is_in_order('false')
     }
    ,{"check endpoints are in order when 'failover_when_all_unreg' is 'true'"
     ,bridge_string_is_in_order('true')
     }
    ].

bridge_string_is_in_order(ShouldFailoverWhenAllUnreg) ->
    {ResourceEps, ResourceStrs} = resource_endpoints(),
    {[D1, D2], [DStr1, DStr2]} = device_endpoints(),
    {FwdEps = [Fwd1, Fwd2, Fwd3], FwdStrs = [FwdStr1, FwdStr2, FwdStr3]} = call_forward_endpoints(),
    {MixedEps, MixedStrs} =
        case ShouldFailoverWhenAllUnreg of
            'false' ->
                {[Fwd1, D2, Fwd3, D1, Fwd2], [FwdStr1, DStr2, FwdStr3, DStr1, FwdStr2]};
            'true' ->
                {[Fwd1, D2, Fwd3, D1, Fwd2], [DStr2, DStr1]}
        end,
    {FwdOnlyStr, FwdOnlyStrReverse} =
        case ShouldFailoverWhenAllUnreg of
            'false' -> {FwdStrs, lists:reverse(FwdStrs)};
            'true' -> {[FwdStr1, FwdStr3], [FwdStr3, FwdStr2]}
        end,

    [{"resource endpoints list as-is"
     ,?_assertEqual(kz_binary:join(ResourceStrs, ?SEPARATOR_SINGLE)
                   ,ecallmgr_util:build_bridge_string(ResourceEps
                                                     ,?SEPARATOR_SINGLE
                                                     ,ShouldFailoverWhenAllUnreg
                                                     )
                   )
     }
    ,{"resource endpoints list in reverse"
     ,?_assertEqual(kz_binary:join(lists:reverse(ResourceStrs), ?SEPARATOR_SINGLE)
                   ,ecallmgr_util:build_bridge_string(lists:reverse(ResourceEps)
                                                     ,?SEPARATOR_SINGLE
                                                     ,ShouldFailoverWhenAllUnreg
                                                     )
                   )
     }
    ,{"device only endpoints list as-is"
     ,?_assertEqual(kz_binary:join([DStr1, DStr2], ?SEPARATOR_SINGLE)
                   ,ecallmgr_util:build_bridge_string([D1, D2]
                                                     ,?SEPARATOR_SINGLE
                                                     ,ShouldFailoverWhenAllUnreg
                                                     )
                   )
     }
    ,{"device only endpoints list in reverse"
     ,?_assertEqual(kz_binary:join([DStr2, DStr1], ?SEPARATOR_SINGLE)
                   ,ecallmgr_util:build_bridge_string([D2, D1]
                                                     ,?SEPARATOR_SINGLE
                                                     ,ShouldFailoverWhenAllUnreg
                                                     )
                   )
     }
    ,{"mixed device and froward endpoints list as-is"
     ,?_assertEqual(kz_binary:join(MixedStrs, ?SEPARATOR_SINGLE)
                   ,ecallmgr_util:build_bridge_string(MixedEps
                                                     ,?SEPARATOR_SINGLE
                                                     ,ShouldFailoverWhenAllUnreg
                                                     )
                   )
     }
    ,{"mixed device and froward endpoints list in reverse"
     ,?_assertEqual(kz_binary:join(lists:reverse(MixedStrs), ?SEPARATOR_SINGLE)
                   ,ecallmgr_util:build_bridge_string(lists:reverse(MixedEps)
                                                     ,?SEPARATOR_SINGLE
                                                     ,ShouldFailoverWhenAllUnreg
                                                     )
                   )
     }
    ,{"froward only endpoints list as-is"
      %% Fwd1 and Fwd2 have same loopback destination number and should be de-deuped
     ,?_assertEqual(kz_binary:join(FwdOnlyStr, ?SEPARATOR_SINGLE)
                   ,ecallmgr_util:build_bridge_string(FwdEps
                                                     ,?SEPARATOR_SINGLE
                                                     ,ShouldFailoverWhenAllUnreg
                                                     )
                   )
     }
    ,{"froward only endpoints list in reverse"
      %% Fwd1 and Fwd2 have same loopback destination number and should be de-deuped
     ,?_assertEqual(kz_binary:join(FwdOnlyStrReverse, ?SEPARATOR_SINGLE)
                   ,ecallmgr_util:build_bridge_string(lists:reverse(FwdEps)
                                                     ,?SEPARATOR_SINGLE
                                                     ,ShouldFailoverWhenAllUnreg
                                                     )
                   )
     }
    ].

resource_endpoints() ->
    Resource1 = kz_json:from_list(
                  [{<<"To-DID">>, <<"+15555567890">>}
                  ,{<<"Route">>, <<"sip:+15555567890@192.168.0.1">>}
                  ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Resource-ID">>, <<"resource_id_1">>}])}
                  ,{<<"Invite-Format">>, <<"route">>}
                  ]),
    Resource2 = kz_json:from_list(
                  [{<<"To-DID">>, <<"+15555567890">>}
                  ,{<<"Route">>, <<"sip:+15555567890@192.168.0.2">>}
                  ,{<<"Custom-Channel-Vars">>, kz_json:from_list([{<<"Resource-ID">>, <<"resource_id_2">>}])}
                  ,{<<"Invite-Format">>, <<"route">>}
                  ]),
    ResourceString1 = <<"[^^"
                        ?BRIDGE_CHANNEL_VAR_SEPARATOR
                        "ecallmgr_Resource-ID='resource_id_1']"
                        "sofia/", ?SIP_INTERFACE, "/+15555567890@192.168.0.1"
                      >>,
    ResourceString2 = <<"[^^"
                        ?BRIDGE_CHANNEL_VAR_SEPARATOR
                        "ecallmgr_Resource-ID='resource_id_2']"
                        "sofia/", ?SIP_INTERFACE, "/+15555567890@192.168.0.2"
                      >>,
    {[Resource1, Resource2], [ResourceString1, ResourceString2]}.

device_endpoints() ->
    Endpoint1 = kz_json:from_list([{<<"Invite-Format">>, <<"username">>}
                                  ,{<<"To-User">>, <<"to_user1">>}
                                  ,{<<"To-Realm">>, <<"to_realm1">>}
                                  ,{<<"To-DID">>, <<"to_did1">>}
                                  ]),
    Endpoint2 = kz_json:from_list([{<<"Invite-Format">>, <<"username">>}
                                  ,{<<"To-User">>, <<"to_user2">>}
                                  ,{<<"To-Realm">>, <<"to_realm2">>}
                                  ,{<<"To-DID">>, <<"to_did2">>}
                                  ]),
    String1 = <<"[^^"
                ?BRIDGE_CHANNEL_VAR_SEPARATOR
                "ecallmgr_Username='to_user1'"
                ?BRIDGE_CHANNEL_VAR_SEPARATOR
                "ecallmgr_Realm='to_realm1']"
                "sofia/", ?SIP_INTERFACE, "/to_user1@to_realm1"
              >>,
    String2 = <<"[^^"
                ?BRIDGE_CHANNEL_VAR_SEPARATOR
                "ecallmgr_Username='to_user2'"
                ?BRIDGE_CHANNEL_VAR_SEPARATOR
                "ecallmgr_Realm='to_realm2']"
                "sofia/", ?SIP_INTERFACE, "/to_user2@to_realm2"
              >>,
    {[Endpoint1, Endpoint2], [String1, String2]}.

call_forward_endpoints() ->
    Endpoint1 = kz_json:from_list([{<<"Invite-Format">>, <<"loopback">>}
                                  ,{<<"Route">>, <<"forward_1">>}
                                  ,{<<"To-DID">>, <<"to_did1">>}
                                  ,{<<"Custom-Channel-Vars">>
                                   ,kz_json:from_list(
                                      [{<<"Call-Forward">>, true}
                                      ,{<<"Authorizing-ID">>, <<"forward_1">>}
                                      ])
                                   }
                                  ]),
    Endpoint2 = kz_json:from_list([{<<"Invite-Format">>, <<"loopback">>}
                                  ,{<<"Route">>, <<"forward_1">>}
                                  ,{<<"To-DID">>, <<"to_did2">>}
                                  ,{<<"Custom-Channel-Vars">>
                                   ,kz_json:from_list(
                                      [{<<"Call-Forward">>, true}
                                      ,{<<"Authorizing-ID">>, <<"forward_2">>}
                                      ])
                                   }
                                  ]),
    Endpoint3 = kz_json:from_list([{<<"Invite-Format">>, <<"loopback">>}
                                  ,{<<"Route">>, <<"forward_3">>}
                                  ,{<<"To-DID">>, <<"to_did2">>}
                                  ,{<<"Custom-Channel-Vars">>
                                   ,kz_json:from_list(
                                      [{<<"Call-Forward">>, true}
                                      ,{<<"Authorizing-ID">>, <<"forward_3">>}
                                      ])
                                   }
                                  ]),
    String1 = <<"[^^"
                ?BRIDGE_CHANNEL_VAR_SEPARATOR
                "ecallmgr_Authorizing-ID='forward_1'"
                ?BRIDGE_CHANNEL_VAR_SEPARATOR
                "ecallmgr_Call-Forward='true']"
                "loopback/forward_1/context_2"
              >>,
    String2 = <<"[^^"
                ?BRIDGE_CHANNEL_VAR_SEPARATOR
                "ecallmgr_Authorizing-ID='forward_2'"
                ?BRIDGE_CHANNEL_VAR_SEPARATOR
                "ecallmgr_Call-Forward='true']"
                "loopback/forward_1/context_2"
              >>,
    String3 = <<"[^^"
                ?BRIDGE_CHANNEL_VAR_SEPARATOR
                "ecallmgr_Authorizing-ID='forward_3'"
                ?BRIDGE_CHANNEL_VAR_SEPARATOR
                "ecallmgr_Call-Forward='true']"
                "loopback/forward_3/context_2"
              >>,
    {[Endpoint1, Endpoint2, Endpoint3], [String1, String2, String3]}.


%% dbg_test() ->
%%     dbg:start(),

%%     dbg:tracer(),

%%     dbg:tpl(ecallmgr_util, [{'_', [], [$_]}]),
%%     dbg:tpl(ecallmgr_fs_xml, [{'_', [], [$_]}]),
%%     dbg:p(all, c),

%%     Endpoint = kz_json:from_list([{<<"Invite-Format">>, <<"username">>}
%%                                  ,{<<"To-User">>, <<"to_user">>}
%%                                  ,{<<"To-Realm">>, <<"to_realm">>}
%%                                  ,{<<"To-DID">>, <<"to_did">>}
%%                                  ]),
%%     Endpoints = [Endpoint],
%%     [BS] = ecallmgr_util:build_simple_channels(Endpoints),

%%     dbg:stop_clear(),
%%     dbg:stop(),

%%     ?assertEqual(<<"sofia/", ?SIP_INTERFACE, "/to_user@to_realm">>, BS).
