%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
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
    available_as(kz_util:rand_hex_binary(16)).

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

available_tests(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    [{"Verify available phone number"
     ,?_assertEqual(?TEST_AVAILABLE_NUM, knm_phone_number:number(PhoneNumber))
     }
    ,{"Verify available number module"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PhoneNumber))
     }
    ,{"Verify available number state"
     ,?_assertEqual(?NUMBER_STATE_AVAILABLE, knm_phone_number:state(PhoneNumber))
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
    [{"Verify MDN can move from in_service to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(knm_number:phone_number(N1)))
     }
    ,?_assert(knm_phone_number:is_dirty(knm_number:phone_number(N1)))
    ,{"Verify releasing MDN results in deletion"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(knm_number:phone_number(N2)))
     }
    ,?_assert(knm_phone_number:is_dirty(knm_number:phone_number(N2)))
    ,{"Verify MDN can reconcile from in_service to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(knm_number:phone_number(N3)))
     }
    ,?_assert(knm_phone_number:is_dirty(knm_number:phone_number(N3)))
    ,{"Verify MDN cannot be reserved"
     ,?_assertMatch({error,_}, knm_number:reserve(Num, knm_number_options:default()))
     }
    ,{"Verify MDN creation forces state to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(knm_number:phone_number(N4)))
     }
    ,?_assert(knm_phone_number:is_dirty(knm_number:phone_number(N4)))
    ,{"Verify MDN creation creates local feature"
     ,?_assertEqual([?FEATURE_LOCAL], knm_phone_number:features_list(knm_number:phone_number(N4)))
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
    [{"Verify feature is not set"
     ,?_assertEqual(undefined, knm_phone_number:feature(PN, ?FEATURE_E911))
     }
    ,{"Verify feature cannot be set"
     ,?_assertThrow({error, unauthorized}, knm_number:update_phone_number(N, Updates, Options))
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
    [{"Verify feature is not set"
     ,?_assertEqual(undefined, knm_phone_number:feature(PN, ?FEATURE_E911))
     }
    ,{"Verify feature cannot be set"
     ,?_assertThrow({error, unauthorized}, knm_number:update_phone_number(N, Updates, Options))
     }
    ].

reserve_test_() ->
    AssignToChild = [{assign_to, ?CHILD_ACCOUNT_ID} | knm_number_options:default()],
    {ok, N1} = knm_number:reserve(?TEST_AVAILABLE_NUM, knm_number_options:default()),
    {ok, N2} = knm_number:reserve(?TEST_IN_SERVICE_NUM, knm_number_options:default()),
    {ok, N3} = knm_number:reserve(?TEST_IN_SERVICE_NUM, AssignToChild),
    [?_assert(knm_phone_number:is_dirty(N1))
    ,{"verify number was indeed reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(N1))
     }
    ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(N1))
    ,?_assert(knm_phone_number:is_dirty(N2))
    ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(N2))
    ,{"verify number is now reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(N2))
     }
    ,?_assert(knm_phone_number:is_dirty(N3))
    ,{"verify number was indeed reserved"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(N3))
     }
    ,?_assertEqual([?CHILD_ACCOUNT_ID, ?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(N3))
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
    {ok, N1a} = knm_number:update_phone_number(N0, [{fun knm_phone_number:set_used_by/2, undefined}]),
    PN1a = knm_number:phone_number(N1a),
    {ok, N1b} = knm_number:update_phone_number(N0, [{fun knm_phone_number:set_used_by/2, MyApp}]),
    PN1b = knm_number:phone_number(N1b),
    {ok, N2} = knm_number:update_phone_number(N1b, [{fun knm_phone_number:set_used_by/2, undefined}]),
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
    {ok, N1a} = knm_number:update_phone_number(N0, [{fun knm_phone_number:set_used_by/2, undefined}]),
    PN1a = knm_number:phone_number(N1a),
    {ok, N1b} = knm_number:update_phone_number(N0, [{fun knm_phone_number:set_used_by/2, MyApp}]),
    PN1b = knm_number:phone_number(N1b),
    {ok, N2} = knm_number:update_phone_number(N1b, [{fun knm_phone_number:set_used_by/2, undefined}]),
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
