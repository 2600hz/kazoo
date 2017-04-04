%%%-------------------------------------------------------------------
%%% @copyright (C) 2016-2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

available_as_owner_test_() ->
    available_as(?RESELLER_ACCOUNT_ID).

available_as_parent_test_() ->
    available_as(?MASTER_ACCOUNT_ID).

available_as_rando_test_() ->
    available_as(kz_binary:rand_hex(16)).

available_as(AuthAccountId) ->
    case knm_number:get(?TEST_AVAILABLE_NUM, [{'auth_by', AuthAccountId}]) of
        {'ok', Number} -> available_tests(Number);
        {'error', Error} -> unavailable_tests(Error)
    end.

unavailable_tests(ErrorJObj) ->
    [{"verify unavailable number error code"
     ,?_assertEqual(403, knm_errors:code(ErrorJObj))
     }
    ,{"verify unavailable number error"
     ,?_assertEqual(<<"forbidden">>, knm_errors:error(ErrorJObj))
     }
    ].

available_tests(N) ->
    PN = knm_number:phone_number(N),
    [?_assertEqual(false, knm_phone_number:is_dirty(PN))
    ,{"Verify available phone number"
     ,?_assertEqual(?TEST_AVAILABLE_NUM, knm_phone_number:number(PN))
     }
    ,{"Verify available number module"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN))
     }
    ,{"Verify available number state"
     ,?_assertEqual(?NUMBER_STATE_AVAILABLE, knm_phone_number:state(PN))
     }
    ].

get_unreconcilable_number_test_() ->
    [{"Verify non-reconcilable numbers result in errors"
     ,?_assertMatch({'error', 'not_reconcilable'}, knm_number:get(<<"1000">>))
     }
    ].

get_not_found_test_() ->
    [?_assertEqual({error, not_found}, knm_number:get(<<"4156301234">>))
    ].

mdn_transitions_test_() ->
    Num = ?TEST_IN_SERVICE_MDN,
    DefaultOptions = [{assign_to, ?MASTER_ACCOUNT_ID} | knm_number_options:mdn_options()],
    {ok, N1} = knm_number:move(Num, ?MASTER_ACCOUNT_ID, DefaultOptions),
    {ok, N2} = knm_number:release(Num, DefaultOptions),
    {ok, N3} = knm_number:reconcile(Num, DefaultOptions),
    {ok, N4} = knm_number:create(?TEST_CREATE_NUM, [{module_name,?CARRIER_MDN}|DefaultOptions]),
    PN1 = knm_number:phone_number(N1),
    PN2 = knm_number:phone_number(N2),
    PN3 = knm_number:phone_number(N3),
    PN4 = knm_number:phone_number(N4),
    [?_assert(knm_phone_number:is_dirty(PN1))
    ,{"Verify MDN can move from in_service to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN1))
     }
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ,{"Verify releasing MDN results in deletion"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(PN2))
     }
    ,?_assert(knm_phone_number:is_dirty(PN3))
    ,{"Verify MDN can reconcile from in_service to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN3))
     }
    ,{"Verify MDN cannot be reserved"
     ,?_assertMatch({error,_}, knm_number:reserve(Num, knm_number_options:default()))
     }
    ,?_assert(knm_phone_number:is_dirty(PN4))
    ,{"Verify MDN creation forces state to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN4))
     }
    ,{"Verify MDN creation creates local feature"
     ,?_assertEqual([?FEATURE_LOCAL], knm_phone_number:features_list(PN4))
     }
    ].

is_mdn_for_mdn_run_test_() ->
    Run = {mdn_run, true},
    Base = [{auth_by,?MASTER_ACCOUNT_ID}],
    Sudo = knm_number_options:default(),
    Fs = [{fun knm_phone_number:update_doc/2, kz_json:from_list([{<<"*">>,42}])}],
    [{"Verify an mdn_run && knm_mdn number can be updated"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_MDN, Fs, [Run|Base]))
     }
    ,{"Verify an mdn_run && !knm_mdn number cannot be updated"
     ,?_assertMatch({error,_}, knm_number:update(?TEST_IN_SERVICE_NUM, Fs, [Run|Base]))
     }
    ,{"Verify a !mdn_run && knm_mdn number cannot be updated"
     ,?_assertMatch({error,_}, knm_number:update(?TEST_IN_SERVICE_MDN, Fs, Base))
     }
    ,{"Verify a !mdn_run && !knm_mdn number can be updated"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_NUM, Fs, Base))
     }
    ,{"Verify sudo can update mdn_run && knm_mdn number"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_MDN, Fs, [Run|Sudo]))
     }
    ,{"Verify sudo can update mdn_run && !knm_mdn number"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_NUM, Fs, [Run|Sudo]))
     }
    ,{"Verify sudo can update !mdn_run && knm_mdn number"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_MDN, Fs, Sudo))
     }
    ,{"Verify sudo can update !mdn_run && !knm_mdn number"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_NUM, Fs, Sudo))
     }
    ].


attempt_setting_e911_on_disallowed_local_number_test_() ->
    JObj = kz_json:from_list(
             [{?FEATURE_E911
              ,kz_json:from_list(
                 [{?E911_STREET1, <<"140 Geary St.">>}
                 ,{?E911_STREET2, <<"3rd floor">>}
                 ,{?E911_CITY, <<"San Francisco">>}
                 ,{?E911_STATE, <<"CA">>}
                 ,{?E911_ZIP, <<"94108">>}
                 ])
              }
             ]),
    Options = [{auth_by, ?RESELLER_ACCOUNT_ID}
              ,{public_fields, JObj}
              ],
    Updates = [{fun knm_phone_number:reset_doc/2, JObj}],
    Num = ?TEST_IN_SERVICE_NUM,
    {ok, N} = knm_number:get(Num),
    PN = knm_number:phone_number(N),
    #{ko := #{Num := Error}} = knm_numbers:update([N], Updates, Options),
    [{"Verify feature is not set"
     ,?_assertEqual(undefined, knm_phone_number:feature(PN, ?FEATURE_E911))
     }
    ,{"Verify feature cannot be set"
     ,?_assertEqual(<<"forbidden">>, knm_errors:error(Error))
     }
    ].

attempt_setting_e911_on_explicitly_disallowed_number_test_() ->
    JObj = kz_json:from_list(
             [{?FEATURE_E911
              ,kz_json:from_list(
                 [{?E911_STREET1, <<"140 Geary St.">>}
                 ,{?E911_STREET2, <<"3rd floor">>}
                 ,{?E911_CITY, <<"San Francisco">>}
                 ,{?E911_STATE, <<"CA">>}
                 ,{?E911_ZIP, <<"94108">>}
                 ])
              }
             ]),
    Options = [{auth_by, ?RESELLER_ACCOUNT_ID}
              ,{public_fields, JObj}
              ],
    Updates = [{fun knm_phone_number:reset_doc/2, JObj}],
    Num = ?BW_EXISTING_DID,
    {ok, N} = knm_number:get(Num),
    PN = knm_number:phone_number(N),
    #{ko := #{Num := ErrorJObj}} = knm_numbers:update([N], Updates, Options),
    [?_assertEqual(false, knm_phone_number:is_dirty(PN))
    ,{"Verify feature is not set"
     ,?_assertEqual(undefined, knm_phone_number:feature(PN, ?FEATURE_E911))
     }
    ,{"Verify feature cannot be set"
     ,?_assertEqual(<<"forbidden">>, knm_errors:error(ErrorJObj))
     }
    ].


assign_to_app_test_() ->
    Num = ?TEST_IN_SERVICE_NUM,
    MyApp = <<"my_app">>,
    {ok, N0} = knm_number:get(Num),
    PN0 = knm_number:phone_number(N0),
    {ok, N1} = knm_number:assign_to_app(Num, MyApp),
    PN1 = knm_number:phone_number(N1),
    [{"Verify number is not already assigned to MyApp"
     ,?_assertNotEqual(MyApp, knm_phone_number:used_by(PN0))
     }
    ,?_assertEqual(false, knm_phone_number:is_dirty(PN0))
    ,{"Verify number is now used by MyApp"
     ,?_assertEqual(MyApp, knm_phone_number:used_by(PN1))
     }
    ,{"Verify updated number will get saved"
     ,?_assertEqual(true, knm_phone_number:is_dirty(PN1))
     }
    ].

update_used_by_from_defined_test_() ->
    Num = ?TEST_IN_SERVICE_NUM,
    MyApp = <<"my_app">>,
    {ok, N0} = knm_number:get(Num),
    PN0 = knm_number:phone_number(N0),
    #{ok := [N1a]} = knm_numbers:update([N0], [{fun knm_phone_number:set_used_by/2, undefined}]),
    PN1a = knm_number:phone_number(N1a),
    #{ok := [N1b]} = knm_numbers:update([N0], [{fun knm_phone_number:set_used_by/2, MyApp}]),
    PN1b = knm_number:phone_number(N1b),
    #{ok := [N2]} = knm_numbers:update([N1b], [{fun knm_phone_number:set_used_by/2, undefined}]),
    PN2 = knm_number:phone_number(N2),
    [?_assertEqual(<<"callflow">>, knm_phone_number:used_by(PN0))
    ,?_assert(not knm_phone_number:is_dirty(PN0))
    ,?_assertEqual(undefined, knm_phone_number:used_by(PN1a))
    ,?_assert(knm_phone_number:is_dirty(PN1a))
    ,?_assertEqual(MyApp, knm_phone_number:used_by(PN1b))
    ,?_assert(knm_phone_number:is_dirty(PN1b))
    ,?_assertEqual(undefined, knm_phone_number:used_by(PN2))
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ].

update_used_by_from_undefined_test_() ->
    Num = ?TEST_IN_SERVICE_MDN,
    MyApp = <<"my_app">>,
    {ok, N0} = knm_number:get(Num),
    PN0 = knm_number:phone_number(N0),
    #{ok := [N1a]} = knm_numbers:update([N0], [{fun knm_phone_number:set_used_by/2, undefined}]),
    PN1a = knm_number:phone_number(N1a),
    #{ok := [N1b]} = knm_numbers:update([N0], [{fun knm_phone_number:set_used_by/2, MyApp}]),
    PN1b = knm_number:phone_number(N1b),
    #{ok := [N2]} = knm_numbers:update([N1b], [{fun knm_phone_number:set_used_by/2, undefined}]),
    PN2 = knm_number:phone_number(N2),
    [?_assertEqual(undefined, knm_phone_number:used_by(PN0))
    ,?_assert(not knm_phone_number:is_dirty(PN0))
    ,?_assertEqual(undefined, knm_phone_number:used_by(PN1a))
    ,?_assert(not knm_phone_number:is_dirty(PN1a))
    ,?_assertEqual(MyApp, knm_phone_number:used_by(PN1b))
    ,?_assert(knm_phone_number:is_dirty(PN1b))
    ,?_assertEqual(undefined, knm_phone_number:used_by(PN2))
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ].

fix_number_test_() ->
    {ok, N1} = knm_number:get(?TEST_OLD7_NUM),
    PN1 = knm_number:phone_number(N1),
    #{ok := [N2]} = fix_number(N1),
    PN2 = knm_number:phone_number(N2),
    [?_assert(not knm_phone_number:is_dirty(PN1))
    ,?_assertEqual(?TEST_OLD7_NUM, knm_phone_number:number(PN1))
    ,?_assertEqual(?CHILD_ACCOUNT_ID, knm_phone_number:assigned_to(PN1))
    ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN1))
    ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN1))
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_CNAM, knm_phone_number:doc(PN1)))
    ,?_assert(lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN1)))
    ,?_assert(lists:member(?FEATURE_CNAM_INBOUND, knm_phone_number:features_list(PN1)))
    ,?_assert(lists:member(?FEATURE_CNAM_OUTBOUND, knm_phone_number:features_list(PN1)))
    ,?_assertEqual(<<"callflow">>, knm_phone_number:used_by(PN1))
    ,?_assert(knm_phone_number:is_dirty(PN1))
    ,?_assertEqual(?TEST_OLD7_NUM, knm_phone_number:number(PN2))
    ,?_assertEqual(?CHILD_ACCOUNT_ID, knm_phone_number:assigned_to(PN2))
    ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN2))
    ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN2))
    ,?_assertEqual(undefined, kz_json:get_value(?FEATURE_CNAM, knm_phone_number:doc(PN2)))
    ,?_assert(lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN2)))
    ,?_assert(not lists:member(?FEATURE_CNAM_INBOUND, knm_phone_number:features_list(PN2)))
    ,?_assert(not lists:member(?FEATURE_CNAM_OUTBOUND, knm_phone_number:features_list(PN2)))
    ,?_assertEqual(<<"trunkstore">>, knm_phone_number:used_by(PN2))
    ].

fix_number(N) ->
    PN = knm_number:phone_number(N),
    Num = knm_phone_number:number(PN),
    AuthBy = knm_phone_number:assigned_to(PN),
    AccountDb = kz_util:format_account_db(AuthBy),
    %% -- below is verbatim from maintenance module --
    UsedBy = kazoo_number_manager_maintenance:app_using(knm_converters:normalize(Num), AccountDb),
    Routines = [{fun knm_phone_number:set_used_by/2, UsedBy}
               ,fun knm_phone_number:remove_denied_features/1
               ],
    Options = [{auth_by, AuthBy}
              ,{dry_run, false}
              ,{batch_run, false}
              ],
    %% -- above is verbatim from maintenance module --
    knm_numbers:update([N], Routines, Options).
