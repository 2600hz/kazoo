%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2021, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_rename_carrier_tests).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

create_new_number_rename_carrier_test_() ->
    Options = [{'auth_by', ?MASTER_ACCOUNT_ID}
              ,{'assign_to', ?RESELLER_ACCOUNT_ID}
              ,{'dry_run', 'false'}
              ,{<<"auth_by_account">>, kzd_accounts:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'true')}
              ,{'public_fields', kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"telnyx">>}])}
              ],
    {'ok', N1} = knm_number:create(?TEST_CREATE_NUM, Options),
    PN1 = knm_number:phone_number(N1),

    WrongCarrier = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"wrong_carrier">>}]),
    {'error', Error2} = knm_number:create(?TEST_CREATE_NUM, props:set_value('public_fields', WrongCarrier, Options)),

    {'error', Error3} = knm_number:create(?TEST_CREATE_NUM, props:set_value('auth_by', ?RESELLER_ACCOUNT_ID, Options)),

    [?_assert(knm_phone_number:is_dirty(PN1))
    ,{"Verify only admin can create and set carrier"
     ,?_assertEqual(<<"knm_telnyx">>, knm_phone_number:module_name(PN1))
     }
    ,{"Verify bad carrier module is bad", ?_assertEqual(<<"invalid">>, knm_errors:error(Error2))}
    ,{"Verify bad carrier module message",  ?_assertEqual(<<"invalid">>, knm_errors:message(Error2))}
    ,{"Verify creating number and setting carrier as non-admin is forbidden"
     ,?_assertEqual(<<"forbidden">>, knm_errors:error(Error3))
     }
    ].

rename_carrier_test_() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{assign_to, ?RESELLER_ACCOUNT_ID}
              ,{<<"auth_by_account">>, kz_json:new()}
              ],
    {ok, N1} = knm_number:create(?TEST_TELNYX_NUM, Options),
    PN1 = knm_number:phone_number(N1),
    JObj1 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"local">>}]),
    #{ok := [N2]} = knm_numbers:update([N1], [{fun knm_phone_number:reset_doc/2, JObj1}]),
    PN2 = knm_number:phone_number(N2),
    JObj2 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"blabla">>}]),
    E3 = knm_numbers:update([N1], [{fun knm_phone_number:update_doc/2, JObj2}]),
    #{ok := [N4]} = knm_numbers:update([N2], [{fun knm_phone_number:reset_doc/2, kz_json:new()}]),
    PN4 = knm_number:phone_number(N4),
    JObj3 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"telnyx">>}]),
    #{ko := #{?TEST_TELNYX_NUM := Error5}} = knm_numbers:update([N4]
                                                               ,[{fun knm_phone_number:reset_doc/2, JObj3}]
                                                               ,[{auth_by, ?RESELLER_ACCOUNT_ID}]
                                                               ),
    JObj4 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"gen_carrier">>}]),
    #{ok := [N6]} = knm_numbers:update([N4], [{fun knm_phone_number:reset_doc/2, JObj4}]),
    PN6 = knm_number:phone_number(N6),
    [?_assert(knm_phone_number:is_dirty(PN1))
    ,{"Verify carrier name is right"
     ,?_assertEqual(<<"knm_telnyx">>, knm_phone_number:module_name(PN1))
     }
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN1)))
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ,{"Verify carrier name is changed"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN2))
     }
    ,{"Verify local feature is now set"
     ,?_assertEqual(true, lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN2)))
     }
    ,{"Verify feature is now removed"
     ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN2)))
     }

    ,{"Verify renamed failed"
     ,?_assertEqual(<<"'blabla' is not known by the system">>, kz_json:get_value(<<"cause">>, kz_maps:get([ko, ?TEST_TELNYX_NUM], E3, kz_json:new())))
     }

    ,?_assertEqual(false, knm_phone_number:is_dirty(PN4))
    ,{"Verify local feature is still set"
     ,?_assertEqual(true, lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN4)))
     }
    ,{"Verify carrier name is still changed"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN4))
     }
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN4)))
    ,{"Verify setting carrier as non-admin is forbidden"
     ,?_assertEqual(<<"forbidden">>, knm_errors:error(Error5))
     }
    ,?_assert(knm_phone_number:is_dirty(PN6))
    ,{"Verify carrier name is changed"
     ,?_assertEqual(<<"knm_gen_carrier">>, knm_phone_number:module_name(PN6))
     }
    ,{"Verify local feature is still not set"
     ,?_assert(not lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN6)))
     }
    ,{"Verify feature is now removed"
     ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN6)))
     }
    ].

rename_from_local_test_() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ],
    {ok, N1} = knm_number:get(?TEST_IN_SERVICE_NUM, Options),
    PN1 = knm_number:phone_number(N1),
    JObj1 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"telnyx">>}]),
    #{ok := [N2]} = knm_numbers:update([N1], [{fun knm_phone_number:reset_doc/2, JObj1}], Options),
    PN2 = knm_number:phone_number(N2),
    [?_assertEqual(false, knm_phone_number:is_dirty(PN1))
    ,{"Verify carrier name is right"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN1))
     }
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN1)))
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ,{"Verify carrier name is changed"
     ,?_assertEqual(<<"knm_telnyx">>, knm_phone_number:module_name(PN2))
     }
    ,{"Verify feature is now removed"
     ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN2)))
     }
    ,{"Verify local feature is now unset"
     ,?_assertEqual(false, lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN2)))
     }
    ].

rename_to_local_with_external_features_test_() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ],
    {ok, N1} = knm_number:get(?TEST_VITELITY_NUM, Options),
    PN1 = knm_number:phone_number(N1),
    JObj1 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"local">>}]),
    #{ok := [N2]} = knm_numbers:update([N1], [{fun knm_phone_number:update_doc/2, JObj1}], Options),
    PN2 = knm_number:phone_number(N2),
    [?_assert(not knm_phone_number:is_dirty(PN1))
    ,{"Verify carrier name is right"
     ,?_assertEqual(<<"knm_vitelity">>, knm_phone_number:module_name(PN1))
     }
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN1)))
    ,{"Verify local feature is not set"
     ,?_assert(not lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN1)))
     }
    ,?_assertNotEqual(undefined, kz_json:get_value(?FEATURE_CNAM, knm_phone_number:doc(PN1)))
    ,?_assert(lists:member(?FEATURE_CNAM_INBOUND, knm_phone_number:features_list(PN1)))
    ,?_assert(lists:member(?FEATURE_CNAM_OUTBOUND, knm_phone_number:features_list(PN1)))
    ,?_assert(lists:member(?FEATURE_PREPEND, knm_phone_number:features_list(PN1)))
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ,{"Verify carrier name is changed"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN2))
     }
    ,{"Verify local feature is now set"
     ,?_assert(lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN2)))
     }
    ,{"Verify feature is now removed"
     ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN2)))
     }
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_CNAM, knm_phone_number:doc(PN2)))
    ,?_assert(not lists:member(?FEATURE_CNAM, knm_phone_number:features_list(PN2)))
    ,?_assert(not lists:member(?FEATURE_CNAM_INBOUND, knm_phone_number:features_list(PN2)))
    ,?_assert(not lists:member(?FEATURE_CNAM_OUTBOUND, knm_phone_number:features_list(PN2)))
    ,?_assert(lists:member(?FEATURE_PREPEND, knm_phone_number:features_list(PN2)))
    ].

rename_to_nonlocal_with_external_features_test_() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ],
    {ok, N1} = knm_number:get(?TEST_VITELITY_NUM, Options),
    PN1 = knm_number:phone_number(N1),
    JObj1 = kz_json:from_list([{?FEATURE_RENAME_CARRIER, <<"bandwidth">>}]),
    #{ok := [N2]} = knm_numbers:update([N1], [{fun knm_phone_number:update_doc/2, JObj1}], Options),
    PN2 = knm_number:phone_number(N2),
    [?_assert(not knm_phone_number:is_dirty(PN1))
    ,{"Verify carrier name is right"
     ,?_assertEqual(<<"knm_vitelity">>, knm_phone_number:module_name(PN1))
     }
    ,{"Verify local feature is not set"
     ,?_assert(not lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN1)))
     }
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN1)))
    ,?_assertNotEqual(undefined, kz_json:get_value(?FEATURE_CNAM, knm_phone_number:doc(PN1)))
    ,?_assert(lists:member(?FEATURE_CNAM_INBOUND, knm_phone_number:features_list(PN1)))
    ,?_assert(lists:member(?FEATURE_CNAM_OUTBOUND, knm_phone_number:features_list(PN1)))
    ,?_assert(lists:member(?FEATURE_PREPEND, knm_phone_number:features_list(PN1)))
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ,{"Verify carrier name is changed"
     ,?_assertEqual(<<"knm_bandwidth2">>, knm_phone_number:module_name(PN2))
     }
    ,{"Verify local feature is not set"
     ,?_assert(not lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN2)))
     }
    ,{"Verify feature is now removed"
     ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_RENAME_CARRIER, knm_phone_number:doc(PN2)))
     }
    ,?_assertNotEqual(undefined, kz_json:get_value(?FEATURE_CNAM, knm_phone_number:doc(PN2)))
    ,?_assert(lists:member(?FEATURE_CNAM_INBOUND, knm_phone_number:features_list(PN2)))
    ,?_assert(lists:member(?FEATURE_CNAM_OUTBOUND, knm_phone_number:features_list(PN2)))
    ,?_assert(lists:member(?FEATURE_PREPEND, knm_phone_number:features_list(PN2)))
    ].

available_features_test_() ->
    [?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, []))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, [{auth_by, ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_IN_SERVICE_NUM, [{auth_by, ?MASTER_ACCOUNT_ID}]))
    ,?_assert(not is_feature_available(?TEST_IN_SERVICE_NUM, [{auth_by, ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, []))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, [{auth_by, ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?BW_EXISTING_DID, [{auth_by, ?MASTER_ACCOUNT_ID}]))
    ,?_assert(not is_feature_available(?BW_EXISTING_DID, [{auth_by, ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, []))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, [{auth_by, ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_OLD5_1_NUM, [{auth_by, ?MASTER_ACCOUNT_ID}]))
    ,?_assert(not is_feature_available(?TEST_OLD5_1_NUM, [{auth_by, ?RESELLER_ACCOUNT_ID}]))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, []))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, [{auth_by, ?KNM_DEFAULT_AUTH_BY}]))
    ,?_assert(is_feature_available(?TEST_VITELITY_NUM, [{auth_by, ?MASTER_ACCOUNT_ID}]))
    ,?_assert(not is_feature_available(?TEST_VITELITY_NUM, [{auth_by, ?RESELLER_ACCOUNT_ID}]))
    ].

is_feature_available(Num, Options) ->
    {ok, N} = knm_number:get(Num, Options),
    PN = knm_number:phone_number(N),
    lists:member(?FEATURE_RENAME_CARRIER, knm_providers:available_features(PN)).
