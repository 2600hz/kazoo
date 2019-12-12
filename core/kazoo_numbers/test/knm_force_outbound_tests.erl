%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_force_outbound_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-export([db_dependant/0
        ,setup/1
        ,cleanup/1
        ]).

-define(KEY, ?FEATURE_FORCE_OUTBOUND).
-define(J(Value), kz_json:from_list([{?KEY, Value}])).

knm_number_test_() ->
    knm_test_util:start_db(fun db_dependant/0, fun setup/1, fun cleanup/1).

db_dependant() ->
    [is_force_outbound()
    ,force_outbound()
    ,available_features()
    ].

setup(TestState) ->
    {'ok', Sup} = kazoo_numbers_sup:start_link(),
    TestState#{'number_cache_pid' => Sup}.

cleanup(#{'number_cache_pid' := Pid}) ->
    erlang:exit(Pid, 'normal').

is_force_outbound() ->
    {'ok', ?RESELLER_ACCOUNT_ID, Props1} = knm_numbers:lookup_account(?TEST_PORT_IN_NUM),
    {'error', {'not_in_service', ?RESELLER_ACCOUNT_ID}} = knm_numbers:lookup_account(?TEST_TELNYX_NUM),
    {'ok', ?RESELLER_ACCOUNT_ID, Props2} = knm_numbers:lookup_account(?TEST_VITELITY_NUM),
    {'ok', ?RESELLER_ACCOUNT_ID, Props3} = knm_numbers:lookup_account(?TEST_IN_SERVICE_NUM),
    {'ok', ?RESELLER_ACCOUNT_ID, Props4} = knm_numbers:lookup_account(?TEST_IN_SERVICE_MDN),
    {'ok', ?RESELLER_ACCOUNT_ID, Props5} = knm_numbers:lookup_account(?TEST_IN_SERVICE_BAD_CARRIER_NUM),
    {'ok', ?RESELLER_ACCOUNT_ID, Props6} = knm_numbers:lookup_account(?TEST_NEW_PORT_NUM),
    {'ok', ?RESELLER_ACCOUNT_ID, Props7} = knm_numbers:lookup_account(?TEST_PORT_IN3_NUM),
    [{"knm_local + port_in --> true"
     ,?_assert(knm_options:should_force_outbound(Props1))
     }
    ,{"knm_vitelity + in_service --> false"
     ,?_assert(not knm_options:should_force_outbound(Props2))
     }
    ,{"knm_local + in_service --> true"
     ,?_assert(knm_options:should_force_outbound(Props3))
     }
    ,{"knm_mdn + in_service --> true"
     ,?_assert(knm_options:should_force_outbound(Props4))
     }
    ,{"knm_pacwest + in_service --> false"
     ,?_assert(not knm_options:should_force_outbound(Props5))
     }
    ,{"pending port request --> true"
     ,?_assert(knm_options:should_force_outbound(Props6))
     }
    ,{"knm_bandwidth2 + port_in --> true"
     ,?_assert(knm_options:should_force_outbound(Props7))
     }
    ].

force_outbound() ->
    Options = [{'auth_by', ?RESELLER_ACCOUNT_ID}
              ,{'assign_to', ?RESELLER_ACCOUNT_ID}
              ],
    [PN1] = knm_pipe:succeeded(knm_ops:create([?TEST_TELNYX_NUM], Options)),

    [PN2] = knm_pipe:succeeded(knm_ops:update([PN1], [{fun knm_phone_number:reset_doc/2, ?J('true')}])),
    [PN3] = knm_pipe:succeeded(knm_ops:update([PN1], [{fun knm_phone_number:update_doc/2, ?J(<<"blabla">>)}])),
    [PN4] = knm_pipe:succeeded(knm_ops:update([PN2], [{fun knm_phone_number:reset_doc/2, ?J('undefined')}])),
    [PN5] = knm_pipe:succeeded(knm_ops:update([PN4], [{fun knm_phone_number:update_doc/2, ?J(<<"false">>)}])),
    [PN6] = knm_pipe:succeeded(knm_ops:update([PN3], [{fun knm_phone_number:reset_doc/2, ?J('false')}])),
    [PN7] = knm_pipe:succeeded(knm_ops:update([undirty(PN6)], [{fun knm_phone_number:reset_doc/2, ?J('false')}])),
    [?_assert(is_dirty(PN1))
    ,{"Verify private feature", ?_assertEqual('undefined', pvt_feature(PN1))}
    ,{"Verify public feature", ?_assertEqual('undefined', pub_feature(PN1))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(PN1))}
    ,{"Verify feature setting", ?_assert(not is_feature_set(PN1))}
    ,?_assert(is_dirty(PN2))
    ,{"Verify private feature", ?_assert(pvt_feature(PN2))}
    ,{"Verify public feature", ?_assert(pub_feature(PN2))}
    ,{"Verify reading feature", ?_assert(is_force_outbound(PN2))}
    ,{"Verify feature setting", ?_assert(is_feature_set(PN2))}
    ,?_assert(is_dirty(PN3))
    ,{"Verify private feature", ?_assertEqual('undefined', pvt_feature(PN3))}
    ,{"Verify public feature", ?_assertEqual(<<"blabla">>, pub_feature(PN3))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(PN3))}
    ,{"Verify feature setting", ?_assert(not is_feature_set(PN3))}
    ,?_assert(is_dirty(PN4))
    ,{"Verify private feature", ?_assertEqual('undefined', pvt_feature(PN4))}
    ,{"Verify public feature", ?_assertEqual('undefined', pub_feature(PN4))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(PN4))}
    ,{"Verify feature setting", ?_assert(not is_feature_set(PN4))}
    ,?_assert(is_dirty(PN5))
    ,{"Verify private feature", ?_assert(not pvt_feature(PN5))}
    ,{"Verify public feature", ?_assertEqual(<<"false">>, pub_feature(PN5))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(PN5))}
    ,{"Verify feature setting", ?_assert(is_feature_set(PN5))}
    ,?_assert(is_dirty(PN6))
    ,{"Verify private feature", ?_assert(not pvt_feature(PN6))}
    ,{"Verify public feature", ?_assert(not pub_feature(PN6))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(PN6))}
    ,{"Verify feature setting", ?_assert(is_feature_set(PN6))}
    ,?_assert(not is_dirty(PN7))
    ,{"Verify private feature", ?_assert(not pvt_feature(PN7))}
    ,{"Verify public feature", ?_assert(not pub_feature(PN7))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(PN7))}
    ,{"Verify feature setting", ?_assert(is_feature_set(PN7))}
    ].

available_features() ->
    [?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, []))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, [{'auth_by', ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, [{'auth_by', ?MASTER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, [{'auth_by', ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, []))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, [{'auth_by', ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, [{'auth_by', ?MASTER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, [{'auth_by', ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, []))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, [{'auth_by', ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, [{'auth_by', ?MASTER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, [{'auth_by', ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, []))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, [{'auth_by', ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, [{'auth_by', ?MASTER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, [{'auth_by', ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_MDN, []))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_MDN, [{'auth_by', ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_MDN, [{'auth_by', ?MASTER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_MDN, [{'auth_by', ?RESELLER_ACCOUNT_ID}]))
    ].


%% Internals

undirty(PN) ->
    knm_phone_number:set_dirty(PN, 'false').

is_dirty(PN) ->
    knm_phone_number:is_dirty(PN).

is_feature_available(Num, Options) ->
    [PN] = knm_pipe:succeeded(knm_ops:get([Num], Options)),
    lists:member(?KEY, knm_providers:available_features(PN)).

is_feature_set(PN) ->
    lists:member(?KEY, knm_phone_number:features_list(PN)).

pvt_feature(PN) ->
    knm_phone_number:feature(PN, ?KEY).

pub_feature(PN) ->
    Doc = knm_phone_number:doc(PN),
    kz_json:get_value(?KEY, Doc).

is_force_outbound(PN) ->
    knm_lib:is_force_outbound(PN).
