%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_force_outbound_tests).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

-define(KEY, ?FEATURE_FORCE_OUTBOUND).
-define(J(Value), kz_json:from_list([{?KEY, Value}])).


is_force_outbound_test_() ->
    {ok, ?RESELLER_ACCOUNT_ID, Props1} = knm_number:lookup_account(?TEST_PORT_IN_NUM),
    {error, {not_in_service, ?RESELLER_ACCOUNT_ID}} = knm_number:lookup_account(?TEST_TELNYX_NUM),
    {ok, ?RESELLER_ACCOUNT_ID, Props2} = knm_number:lookup_account(?TEST_VITELITY_NUM),
    {ok, ?RESELLER_ACCOUNT_ID, Props3} = knm_number:lookup_account(?TEST_IN_SERVICE_NUM),
    {ok, ?RESELLER_ACCOUNT_ID, Props4} = knm_number:lookup_account(?TEST_IN_SERVICE_MDN),
    {ok, ?RESELLER_ACCOUNT_ID, Props5} = knm_number:lookup_account(?TEST_IN_SERVICE_BAD_CARRIER_NUM),
    {ok, ?RESELLER_ACCOUNT_ID, Props6} = knm_number:lookup_account(?TEST_NEW_PORT_NUM),
    {ok, ?RESELLER_ACCOUNT_ID, Props7} = knm_number:lookup_account(?TEST_PORT_IN3_NUM),
    [{"knm_local + port_in --> true"
     ,?_assert(knm_number_options:should_force_outbound(Props1))
     }
    ,{"knm_vitelity + in_service --> false"
     ,?_assert(not knm_number_options:should_force_outbound(Props2))
     }
    ,{"knm_local + in_service --> true"
     ,?_assert(knm_number_options:should_force_outbound(Props3))
     }
    ,{"knm_mdn + in_service --> true"
     ,?_assert(knm_number_options:should_force_outbound(Props4))
     }
    ,{"knm_pacwest + in_service --> false"
     ,?_assert(not knm_number_options:should_force_outbound(Props5))
     }
    ,{"pending port request --> true"
     ,?_assert(knm_number_options:should_force_outbound(Props6))
     }
    ,{"knm_bandwidth2 + port_in --> true"
     ,?_assert(knm_number_options:should_force_outbound(Props7))
     }
    ].

force_outbound_test_() ->
    Options = [{auth_by, ?RESELLER_ACCOUNT_ID}
              ,{assign_to, ?RESELLER_ACCOUNT_ID}
              ,{<<"auth_by_account">>, kzd_accounts:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, true)}
              ],
    {ok, N1} = knm_number:create(?TEST_TELNYX_NUM, Options),
    #{ok := [N2]} = knm_numbers:update([N1], [{fun knm_phone_number:reset_doc/2, ?J(true)}]),
    #{ok := [N3]} = knm_numbers:update([N1], [{fun knm_phone_number:update_doc/2, ?J(<<"blabla">>)}]),
    #{ok := [N4]} = knm_numbers:update([N2], [{fun knm_phone_number:reset_doc/2, ?J(undefined)}]),
    #{ok := [N5]} = knm_numbers:update([N4], [{fun knm_phone_number:update_doc/2, ?J(<<"false">>)}]),
    #{ok := [N6]} = knm_numbers:update([N3], [{fun knm_phone_number:reset_doc/2, ?J(false)}]),
    #{ok := [N7]} = knm_numbers:update([undirty(N6)], [{fun knm_phone_number:reset_doc/2, ?J(false)}]),
    [?_assert(is_dirty(N1))
    ,{"Verify private feature", ?_assertEqual(undefined, pvt_feature(N1))}
    ,{"Verify public feature", ?_assertEqual(undefined, pub_feature(N1))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(N1))}
    ,{"Verify feature setting", ?_assert(not is_feature_set(N1))}
    ,?_assert(is_dirty(N2))
    ,{"Verify private feature", ?_assert(pvt_feature(N2))}
    ,{"Verify public feature", ?_assert(pub_feature(N2))}
    ,{"Verify reading feature", ?_assert(is_force_outbound(N2))}
    ,{"Verify feature setting", ?_assert(is_feature_set(N2))}
    ,?_assert(is_dirty(N3))
    ,{"Verify private feature", ?_assertEqual(undefined, pvt_feature(N3))}
    ,{"Verify public feature", ?_assertEqual(<<"blabla">>, pub_feature(N3))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(N3))}
    ,{"Verify feature setting", ?_assert(not is_feature_set(N3))}
    ,?_assert(is_dirty(N4))
    ,{"Verify private feature", ?_assertEqual(undefined, pvt_feature(N4))}
    ,{"Verify public feature", ?_assertEqual(undefined, pub_feature(N4))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(N4))}
    ,{"Verify feature setting", ?_assert(not is_feature_set(N4))}
    ,?_assert(is_dirty(N5))
    ,{"Verify private feature", ?_assert(not pvt_feature(N5))}
    ,{"Verify public feature", ?_assertEqual(<<"false">>, pub_feature(N5))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(N5))}
    ,{"Verify feature setting", ?_assert(is_feature_set(N5))}
    ,?_assert(is_dirty(N6))
    ,{"Verify private feature", ?_assert(not pvt_feature(N6))}
    ,{"Verify public feature", ?_assert(not pub_feature(N6))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(N6))}
    ,{"Verify feature setting", ?_assert(is_feature_set(N6))}
    ,?_assert(not is_dirty(N7))
    ,{"Verify private feature", ?_assert(not pvt_feature(N7))}
    ,{"Verify public feature", ?_assert(not pub_feature(N7))}
    ,{"Verify reading feature", ?_assert(not is_force_outbound(N7))}
    ,{"Verify feature setting", ?_assert(is_feature_set(N7))}
    ].

available_features_test_() ->
    [?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, []))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, [{auth_by, ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, [{auth_by, ?MASTER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, [{auth_by, ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, []))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, [{auth_by, ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, [{auth_by, ?MASTER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, [{auth_by, ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, []))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, [{auth_by, ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, [{auth_by, ?MASTER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, [{auth_by, ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, []))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, [{auth_by, ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, [{auth_by, ?MASTER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, [{auth_by, ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_MDN, []))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_MDN, [{auth_by, ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_MDN, [{auth_by, ?MASTER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_MDN, [{auth_by, ?RESELLER_ACCOUNT_ID}]))
    ].


%% Internals

undirty(N) ->
    PN = knm_number:phone_number(N),
    NewPN = knm_phone_number:set_is_dirty(PN, false),
    knm_number:set_phone_number(N, NewPN).

is_dirty(N) ->
    PN = knm_number:phone_number(N),
    knm_phone_number:is_dirty(PN).

is_feature_available(Num, Options) ->
    {ok, N} = knm_number:get(Num, Options),
    PN = knm_number:phone_number(N),
    lists:member(?KEY, knm_providers:available_features(PN)).

is_feature_set(N) ->
    PN = knm_number:phone_number(N),
    lists:member(?KEY, knm_phone_number:features_list(PN)).

pvt_feature(N) ->
    PN = knm_number:phone_number(N),
    knm_phone_number:feature(PN, ?KEY).

pub_feature(N) ->
    PN = knm_number:phone_number(N),
    Doc = knm_phone_number:doc(PN),
    kz_json:get_value(?KEY, Doc).

is_force_outbound(N) ->
    PN = knm_number:phone_number(N),
    knm_number:is_force_outbound(PN).
