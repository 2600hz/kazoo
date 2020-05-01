%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
     }
     || Key <- Keys
    ].

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
