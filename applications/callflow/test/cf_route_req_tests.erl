%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_route_req_tests).

-include_lib("eunit/include/eunit.hrl").

avoid_privacy_if_emergency_call_test_() ->
    EmergencyFlow = kz_json:set_value(<<"capture_group">>, <<"911">>, star_67_callflow()),
    NormalFlow = star_67_callflow([<<"+14151234567">>]),

    ModeNoneFlow = kz_json:set_value([<<"flow">>, <<"data">>, <<"mode">>], <<"none">>, EmergencyFlow),

    [{"When dialing emergency number with *67 feature code, ignore privacy settings"
     ,check_expected(ModeNoneFlow, EmergencyFlow)
     }
    ,{"When dialing normal number with *67 feature code, allow privacy settings"
     ,check_expected(NormalFlow, NormalFlow)
     }
    ].

check_expected(Expected, Callflow) ->
    Resp = cf_route_req:avoid_privacy_if_emergency_call(
             kz_json:get_ne_value(<<"capture_group">>, Callflow)
            ,Callflow
            ),
    ?_assert(kz_json:are_equal(Expected, Resp)).

star_67_callflow() ->
    star_67_callflow([]).

star_67_callflow(Numbers) ->
    Flow = kz_json:from_list_recursive([{<<"children">>, []}
                                       ,{<<"data">>
                                        ,[{<<"action">>, <<"full">>}
                                         ,{<<"endpoint_strategy">>, <<"overwrite">>}
                                         ,{<<"mode">>, <<"full">>}
                                         ]
                                        }
                                       ,{<<"module">>, <<"privacy">>}
                                       ]),
    Routines = [{fun kzd_callflows:set_featurecode_name/2, <<"privacy[mode=full]">>}
               ,{fun kzd_callflows:set_featurecode_number/2, <<"67">>}
               ,{fun kzd_callflows:set_flow/2, Flow}
               ,{fun kzd_callflows:set_patterns/2, [<<"^\\*67([0-9]*)", $$/integer>>]}
               ,{fun kzd_callflows:set_numbers/2, Numbers}
               ],
    kz_json:exec_first(Routines, kzd_callflows:new()).
