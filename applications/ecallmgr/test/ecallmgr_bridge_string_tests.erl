-module(ecallmgr_bridge_string_tests).

-include("src/ecallmgr.hrl").
-include_lib("eunit/include/eunit.hrl").

simple_bridge_test() ->
    Endpoint = kz_json:from_list([{<<"Invite-Format">>, <<"username">>}
                                 ,{<<"To-User">>, <<"to_user">>}
                                 ,{<<"To-Realm">>, <<"to_realm">>}
                                 ,{<<"To-DID">>, <<"to_did">>}
                                 ]),
    Endpoints = [Endpoint],
    [BS] = ecallmgr_util:build_simple_channels(Endpoints),
    ?assertEqual(<<"sofia/", ?SIP_INTERFACE, "/to_user@to_realm">>, BS).

sip_headers_test() ->
    SIPHeaders = kz_json:from_list([{<<"X-Account-ID">>, <<"{account_id}">>}
                                   ,{<<"X-Billing-Number">>, <<"1234">>}
                                   ,{<<"X-Caller-Macro">>, <<"{caller_id_number}">>}
                                   ]),

    Endpoint = kz_json:from_list([{<<"Custom-SIP-Headers">>, SIPHeaders}
                                 ,{<<"Invite-Format">>, <<"username">>}
                                 ,{<<"To-User">>, <<"to_user">>}
                                 ,{<<"To-Realm">>, <<"to_realm">>}
                                 ,{<<"To-DID">>, <<"to_did">>}
                                 ]),
    Endpoints = [Endpoint],
    [BS] = ecallmgr_util:build_simple_channels(Endpoints),
    ?assertEqual(<<"[^^"
                   ?BRIDGE_CHANNEL_VAR_SEPARATOR
                   "sip_h_X-Caller-Macro=${caller_id_number}"
                   ?BRIDGE_CHANNEL_VAR_SEPARATOR
                   "sip_h_X-Billing-Number=1234"
                   ?BRIDGE_CHANNEL_VAR_SEPARATOR
                   "sip_h_X-Account-ID=${ecallmgr_Account-ID}]"
                   "sofia/", ?SIP_INTERFACE, "/to_user@to_realm"
                 >>
                ,BS
                ).

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
