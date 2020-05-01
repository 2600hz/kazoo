%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_number_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-export([db_dependant/0]).

knm_number_test_() ->
    knm_test_util:start_db(fun db_dependant/0).

db_dependant() ->
    [available_as_owner()
    ,available_as_parent()
    ,available_as_random()
    ,get_unreconcilable_number()
    ,get_not_found()
    ,mdn_transitions()
    ,is_mdn_for_mdn_run()
    ,attempt_setting_e911_on_disallowed_local_number()
    ,attempt_setting_e911_on_explicitly_disallowed_number()
    ,assign_to_app()
    ,update_used_by_from_defined()
    ,update_used_by_from_undefined()
    ,fix_number()
    ,fix_number_wrong_used_by_and_dangling_pvt_features()
    ].

available_as_owner() ->
    available_as(?RESELLER_ACCOUNT_ID).

available_as_parent() ->
    available_as(?MASTER_ACCOUNT_ID).

available_as_random() ->
    available_as(kz_binary:rand_hex(16)).

available_as(AuthAccountId) ->
    %% FIXME: update these tests to work on knm_numbers successes jobjs
    Collection = knm_ops:get([?TEST_AVAILABLE_NUM], [{'auth_by', AuthAccountId}]),
    case knm_pipe:succeeded(Collection) of
        [Number] -> available_tests(Number);
        [] ->
            [{_, Error}] = knm_pipe:failed_to_proplist(Collection),
            unavailable_tests(Error)
    end.

unavailable_tests(ErrorJObj) ->
    [{"verify unavailable number error code"
     ,?_assertEqual(403, knm_errors:code(ErrorJObj))
     }
    ,{"verify unavailable number error"
     ,?_assertEqual(<<"forbidden">>, knm_errors:error(ErrorJObj))
     }
    ].

available_tests(PN) ->
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

get_unreconcilable_number() ->
    [{"Verify non-reconcilable numbers result in errors"
     ,?_assertMatch({'ok', [], [{_, 'not_reconcilable'}]}, knm_numbers:get(<<"1000">>))
     }
    ].

get_not_found() ->
    [?_assertEqual({'ok', [], [{<<"+14156301234">>, 'not_found'}]}, knm_numbers:get(<<"4156301234">>))
    ].

mdn_transitions() ->
    Num = ?TEST_IN_SERVICE_MDN,
    DefaultOptions = [{'assign_to', ?MASTER_ACCOUNT_ID} | knm_options:mdn_options()],

    %% FIXME: update these tests to work on knm_numbers successes jobjs
    [PN1] = knm_pipe:succeeded(knm_ops:move([Num], ?MASTER_ACCOUNT_ID, DefaultOptions)),
    [PN2] = knm_pipe:succeeded(knm_ops:release([Num], DefaultOptions)),
    [PN4] = knm_pipe:succeeded(knm_ops:create([?TEST_CREATE_NUM], [{'module_name', ?CARRIER_MDN} | DefaultOptions])),

    [?_assert(knm_phone_number:is_dirty(PN1))
    ,{"Verify MDN can move from in_service to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN1))
     }
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ,{"Verify releasing MDN results in deletion"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(PN2))
     }
    ,{"Verify MDN cannot be reserved"
     ,?_assertMatch({'ok', [], [{Num, _Error}]}, knm_numbers:reserve(Num, knm_options:default()))
     }
    ,?_assert(knm_phone_number:is_dirty(PN4))
    ,{"Verify MDN creation forces state to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN4))
     }
    ,{"Verify MDN creation creates local feature"
     ,?_assertEqual([?FEATURE_LOCAL], knm_phone_number:features_list(PN4))
     }
    ].

is_mdn_for_mdn_run() ->
    Run = {'mdn_run', 'true'},
    Base = [{'auth_by',?MASTER_ACCOUNT_ID}],
    Sudo = knm_options:default(),
    Fs = [{fun knm_phone_number:update_doc/2, kz_json:from_list([{<<"*">>,42}])}],

    [{"Verify an mdn_run && knm_mdn number can be updated"
     ,?_assertMatch({'ok', [_JObj]}, knm_numbers:update(?TEST_IN_SERVICE_MDN, Fs, [Run|Base]))
     }
    ,{"Verify an mdn_run && !knm_mdn number cannot be updated"
     ,?_assertMatch({'ok', [], [{?TEST_IN_SERVICE_NUM, _Error}]}
                   ,knm_numbers:update(?TEST_IN_SERVICE_NUM, Fs, [Run|Base])
                   )
     }
    ,{"Verify a !mdn_run && knm_mdn number cannot be updated"
     ,?_assertMatch({'ok', [], [{?TEST_IN_SERVICE_MDN, _Error}]}
                   ,knm_numbers:update(?TEST_IN_SERVICE_MDN, Fs, Base)
                   )
     }
    ,{"Verify a !mdn_run && !knm_mdn number can be updated"
     ,?_assertMatch({'ok', [_]}, knm_numbers:update(?TEST_IN_SERVICE_NUM, Fs, Base))
     }
    ,{"Verify sudo can update mdn_run && knm_mdn number"
     ,?_assertMatch({'ok', [_]}, knm_numbers:update(?TEST_IN_SERVICE_MDN, Fs, [Run|Sudo]))
     }
    ,{"Verify sudo can update mdn_run && !knm_mdn number"
     ,?_assertMatch({'ok', [_]}, knm_numbers:update(?TEST_IN_SERVICE_NUM, Fs, [Run|Sudo]))
     }
    ,{"Verify sudo can update !mdn_run && knm_mdn number"
     ,?_assertMatch({'ok', [_]}, knm_numbers:update(?TEST_IN_SERVICE_MDN, Fs, Sudo))
     }
    ,{"Verify sudo can update !mdn_run && !knm_mdn number"
     ,?_assertMatch({'ok', [_]}, knm_numbers:update(?TEST_IN_SERVICE_NUM, Fs, Sudo))
     }
    ].


attempt_setting_e911_on_disallowed_local_number() ->
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
    Options = [{'auth_by', ?RESELLER_ACCOUNT_ID}
              ,{'public_fields', JObj}
              ],
    Updates = [{fun knm_phone_number:reset_doc/2, JObj}],
    Num = ?TEST_IN_SERVICE_NUM,

    %% FIXME: update these tests to work on knm_numbers successes jobjs
    [PN] = knm_pipe:succeeded(knm_ops:get([Num])),

    [{Num, Error}] = knm_pipe:failed_to_proplist(knm_ops:update([PN], Updates, Options)),

    [{"Verify feature is not set"
     ,?_assertEqual('undefined', knm_phone_number:feature(PN, ?FEATURE_E911))
     }
    ,{"Verify feature cannot be set"
     ,?_assertEqual(<<"forbidden">>, knm_errors:error(Error))
     }
    ].

attempt_setting_e911_on_explicitly_disallowed_number() ->
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
    Options = [{'auth_by', ?RESELLER_ACCOUNT_ID}
              ,{'public_fields', JObj}
              ],
    Updates = [{fun knm_phone_number:reset_doc/2, JObj}],
    Num = ?BW_EXISTING_DID,

    %% FIXME: update these tests to work on knm_numbers successes jobjs
    [PN] = knm_pipe:succeeded(knm_ops:get([Num])),
    [{Num, ErrorJObj}] = knm_pipe:failed_to_proplist(knm_ops:update([PN], Updates, Options)),

    [?_assertEqual('false', knm_phone_number:is_dirty(PN))
    ,{"Verify feature is not set"
     ,?_assertEqual('undefined', knm_phone_number:feature(PN, ?FEATURE_E911))
     }
    ,{"Verify feature cannot be set"
     ,?_assertEqual(<<"forbidden">>, knm_errors:error(ErrorJObj))
     }
    ].

assign_to_app() ->
    Num = ?TEST_IN_SERVICE_NUM,
    MyApp = <<"my_app">>,
    Collection0 = knm_ops:get([Num]),
    [PN0] = knm_pipe:succeeded(Collection0),

    %% FIXME: update these tests to work on knm_numbers successes jobjs
    [PN1] = knm_pipe:succeeded(knm_ops:assign_to_app([Num], MyApp)),

    [{"Verify number is not already assigned to MyApp"
     ,?_assertNotEqual(MyApp, knm_phone_number:used_by(PN0))
     }
    ,?_assertEqual('false', knm_phone_number:is_dirty(PN0))
    ,{"Verify number is now used by MyApp"
     ,?_assertEqual(MyApp, knm_phone_number:used_by(PN1))
     }
    ,{"Verify updated number will get saved"
     ,?_assertEqual('true', knm_phone_number:is_dirty(PN1))
     }
    ].

update_used_by_from_defined() ->
    Num = ?TEST_IN_SERVICE_NUM,
    MyApp = <<"my_app">>,

    %% FIXME: update these tests to work on knm_numbers successes jobjs
    [PN0] = knm_pipe:succeeded(knm_ops:get([Num])),
    [PN1a] = knm_pipe:succeeded(knm_ops:update([PN0], [{fun knm_phone_number:set_used_by/2, undefined}])),
    [PN1b] = knm_pipe:succeeded(knm_ops:update([PN0], [{fun knm_phone_number:set_used_by/2, MyApp}])),
    [PN2] = knm_pipe:succeeded(knm_ops:update([PN1b], [{fun knm_phone_number:set_used_by/2, undefined}])),

    [?_assertEqual(<<"callflow">>, knm_phone_number:used_by(PN0))
    ,?_assert(not knm_phone_number:is_dirty(PN0))
    ,?_assertEqual(undefined, knm_phone_number:used_by(PN1a))
    ,?_assert(knm_phone_number:is_dirty(PN1a))
    ,?_assertEqual(MyApp, knm_phone_number:used_by(PN1b))
    ,?_assert(knm_phone_number:is_dirty(PN1b))
    ,?_assertEqual(undefined, knm_phone_number:used_by(PN2))
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ].

update_used_by_from_undefined() ->
    Num = ?TEST_IN_SERVICE_MDN,
    MyApp = <<"my_app">>,

    %% FIXME: update these tests to work on knm_numbers successes jobjs
    [PN0] = knm_pipe:succeeded(knm_ops:get([Num])),
    [PN1a] = knm_pipe:succeeded(knm_ops:update([PN0], [{fun knm_phone_number:set_used_by/2, undefined}])),
    [PN1b] = knm_pipe:succeeded(knm_ops:update([PN0], [{fun knm_phone_number:set_used_by/2, MyApp}])),
    [PN2] = knm_pipe:succeeded(knm_ops:update([PN1b], [{fun knm_phone_number:set_used_by/2, undefined}])),

    [?_assertEqual(undefined, knm_phone_number:used_by(PN0))
    ,?_assert(not knm_phone_number:is_dirty(PN0))
    ,?_assertEqual(undefined, knm_phone_number:used_by(PN1a))
    ,?_assert(not knm_phone_number:is_dirty(PN1a))
    ,?_assertEqual(MyApp, knm_phone_number:used_by(PN1b))
    ,?_assert(knm_phone_number:is_dirty(PN1b))
    ,?_assertEqual(undefined, knm_phone_number:used_by(PN2))
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ].

fix_number() ->
    Num = ?TEST_OLD5_1_NUM,

    %% FIXME: update these tests to work on knm_numbers successes jobjs
    [PN1] = knm_pipe:succeeded(knm_ops:get([Num])),
    Doc1 = knm_phone_number:doc(PN1),
    FeaturesList1 = knm_phone_number:features_list(PN1),
    [PN2] = knm_pipe:succeeded(fix_number(PN1)),
    Doc2 = knm_phone_number:doc(PN2),
    FeaturesList2 = knm_phone_number:features_list(PN2),

    [?_assert(not knm_phone_number:is_dirty(PN1))
    ,?_assertEqual(Num, knm_phone_number:number(PN1))
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN1))
    ,?_assertEqual(<<"knm_bandwidth2">>, knm_phone_number:module_name(PN1))
    ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN1))
    ,?_assertNotEqual(undefined, kz_json:get_value(?FEATURE_CNAM, Doc1))
    ,?_assertNotEqual(undefined, kz_json:get_value(?FEATURE_E911, Doc1))
    ,?_assertNotEqual(undefined, kz_json:get_value(?LEGACY_DASH_E911, Doc1))
    ,?_assert(not lists:member(?FEATURE_LOCAL, FeaturesList1))
    ,?_assert(lists:member(?FEATURE_CNAM_INBOUND, FeaturesList1))
    ,?_assert(lists:member(?FEATURE_CNAM_OUTBOUND, FeaturesList1))
    ,?_assert(lists:member(?FEATURE_E911, FeaturesList1))
    ,?_assert(not lists:member(?LEGACY_DASH_E911, FeaturesList1))
    ,?_assertEqual(undefined, knm_phone_number:feature(PN1, ?LEGACY_DASH_E911))
    ,?_assert(kz_json:are_equal(knm_phone_number:feature(PN1, ?FEATURE_E911), kz_json:get_value(?FEATURE_E911, Doc1)))
    ,?_assert(kz_json:are_equal(knm_phone_number:feature(PN1, ?FEATURE_E911), kz_json:get_value(?LEGACY_DASH_E911, Doc1)))
    ,?_assertEqual(undefined, knm_phone_number:used_by(PN1))
    ,?_assert(not knm_phone_number:is_dirty(PN2))
    ,?_assertEqual(Num, knm_phone_number:number(PN2))
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN2))
    ,?_assertEqual(<<"knm_bandwidth2">>, knm_phone_number:module_name(PN2))
    ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN2))
    ,?_assertNotEqual(undefined, kz_json:get_value(?FEATURE_CNAM, Doc2))
    ,?_assertNotEqual(undefined, kz_json:get_value(?FEATURE_E911, Doc2))
    ,?_assertNotEqual(undefined, kz_json:get_value(?LEGACY_DASH_E911, Doc2))
    ,?_assert(not lists:member(?FEATURE_LOCAL, FeaturesList2))
    ,?_assert(lists:member(?FEATURE_CNAM_INBOUND, FeaturesList2))
    ,?_assert(lists:member(?FEATURE_CNAM_OUTBOUND, FeaturesList2))
    ,?_assert(lists:member(?FEATURE_E911, FeaturesList2))
    ,?_assert(not lists:member(?LEGACY_DASH_E911, FeaturesList2))
    ,?_assertEqual(undefined, knm_phone_number:feature(PN2, ?LEGACY_DASH_E911))
    ,?_assert(kz_json:are_equal(knm_phone_number:feature(PN2, ?FEATURE_E911), kz_json:get_value(?FEATURE_E911, Doc2)))
    ,?_assert(kz_json:are_equal(knm_phone_number:feature(PN2, ?FEATURE_E911), kz_json:get_value(?LEGACY_DASH_E911, Doc2)))
    ,?_assertEqual(undefined, knm_phone_number:used_by(PN2))
    ].

fix_number_wrong_used_by_and_dangling_pvt_features() ->
    %% FIXME: update these tests to work on knm_numbers successes jobjs
    [PN1] = knm_pipe:succeeded(knm_ops:get([?TEST_OLD7_NUM])),
    [PN2] = knm_pipe:succeeded(fix_number(PN1)),

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
    ,?_assert(knm_phone_number:is_dirty(PN2))
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


%%
%% TODO: make this works with fixturedb (remove denied features)
fix_number(PN) ->
    Num = knm_phone_number:number(PN),
    AuthBy = knm_phone_number:assigned_to(PN),
    AccountDb = kzs_util:format_account_db(AuthBy),
    UsedBy = kazoo_numbers_maintenance:app_using(knm_converters:normalize(Num), AccountDb),
    Routines = [{fun knm_phone_number:set_used_by/2, UsedBy}
               ,fun knm_phone_number:remove_denied_features/1
               ],
    Options = [{'auth_by', AuthBy}
              ,{'dry_run', 'false'}
              ,{'batch_run', 'false'}
              ],
    %% -- above is verbatim from maintenance module --
    knm_ops:update([PN], Routines, Options).
