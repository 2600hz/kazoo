%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Account document
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_account_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").

-define(AN_ACCOUNT_ID, <<"4fe69c5b61015084f1fe5684abc6e502">>).

-define(ID, <<"_id">>).
-define(TREE, <<"pvt_tree">>).

kz_account_test_() ->
    {setup
    ,fun kzd_test_fixtures:setup/0
    ,fun kzd_test_fixtures:cleanup/1
    ,fun(_) ->
             [test_account_doc_against_fixture()
             ,test_undefined_account_id()
             ,test_account_id()
             ,test_account_name()
             ,test_account_realm()
             ,test_language()
             ,test_timezone()
             ,test_parent_account_id()
             ,test_account_tree()
             ,test_notification_preference()
             ,test_enabled()
             ,test_expired()
             ,test_api_key()
             ,test_superduper_admin()
             ,test_allow_number_additions()
             ,test_reseller()
             ,test_account_hierarchy()
             ]
     end
    }.

normalize_account_name_test_() ->
    [?_assertEqual(undefined, kzd_accounts:normalize_name(undefined))
    ,?_assertEqual(<<"blip2blop">>, kzd_accounts:normalize_name(<<"Blip#2!Blop">>))
    ].

is_in_account_hierarchy_test_() ->
    [?_assertEqual(false, kzd_accounts:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID))
    ,?_assertEqual(false, kzd_accounts:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID))
    ,?_assertEqual(false, kzd_accounts:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID, true))
    ,?_assertEqual(false, kzd_accounts:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID, false))
    ,?_assertEqual(false, kzd_accounts:is_in_account_hierarchy(?AN_ACCOUNT_ID, undefined, false))
    ,?_assertEqual(false, kzd_accounts:is_in_account_hierarchy(?AN_ACCOUNT_ID, undefined, true))
    ,?_assertEqual(true, kzd_accounts:is_in_account_hierarchy(?AN_ACCOUNT_ID, ?AN_ACCOUNT_ID, true))
    ].

is_system_admin_test_() ->
    [?_assertEqual(false, kzd_accounts:is_superduper_admin(undefined))
    ].

is_account_enabled_test_() ->
    [?_assertEqual(false, kzd_accounts:is_enabled(undefined))
    ].

is_account_expired_test_() ->
    [?_assertEqual(false, kzd_accounts:is_expired(undefined))
    ].

test_account_doc_against_fixture() ->
    {'ok', Schema} = kz_json_schema:fload(<<"accounts">>),
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kzd_accounts:fetch(?FIXTURE_RESELLER_ACCOUNT_ID),
    {'ok', SubSubAccount} = kzd_accounts:fetch(?FIXTURE_PARENT_ACCOUNT_ID),
    [{"validate master account fixture", ?_assertMatch({'ok', _}, validate(Schema, MasterAccount))}
    ,{"validate sub account fixture", ?_assertMatch({'ok', _}, validate(Schema, SubAccount))}
    ,{"validate sub-sub account fixture", ?_assertMatch({'ok', _}, validate(Schema, SubSubAccount))}
    ].

test_undefined_account_id() ->
    [?_assertEqual({error,invalid_db_name}, kzd_accounts:fetch(undefined))
    ,?_assertEqual(undefined, kzd_accounts:fetch_realm(undefined))
    ,?_assertEqual(undefined, kzd_accounts:fetch_name(undefined))
    ].

new_test_() ->
    Account = kzd_accounts:new(),
    [{"validate new returns a JSON object", ?_assert(kz_json:is_json_object(Account))}
    ,{"validate new sets the correct doc type", ?_assertEqual(<<"account">>, kz_json:get_value(<<"pvt_type">>, Account))}
    ].

type_test_() ->
    [{"validate type returns the expected value", ?_assertEqual(<<"account">>, kzd_accounts:type())}].

test_account_id() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    [{"validate id returns the expected value", ?_assertEqual(?FIXTURE_MASTER_ACCOUNT_ID, kz_doc:id(MasterAccount))}].

test_account_name() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"name">>, MasterAccount),
    Updated = kzd_accounts:set_name(MasterAccount, <<"updated">>),
    [{"validate fetch_name returns the expected value", ?_assertEqual(<<"Master Account">>, kzd_accounts:fetch_name(?FIXTURE_MASTER_ACCOUNT_ID))}
    ,{"validate name returns the expected value", ?_assertEqual(<<"Master Account">>, kzd_accounts:name(MasterAccount))}
    ,{"validate name returns 'undefined' if not found", ?_assertEqual('undefined', kzd_accounts:name(Missing))}
    ,{"validate name can return a default value if not found", ?_assertEqual(<<"default">>, kzd_accounts:name(Missing, <<"default">>))}
    ,{"validate set_name changes the name", ?_assertEqual(<<"updated">>, kzd_accounts:name(Updated))}
    ].

test_account_realm() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"realm">>, MasterAccount),
    Updated = kzd_accounts:set_realm(MasterAccount, <<"updated">>),
    [{"validate fetch_realm returns the expected value", ?_assertEqual(<<"4a6863.sip.2600hz.local">>, kzd_accounts:fetch_realm(?FIXTURE_MASTER_ACCOUNT_ID))}
    ,{"validate realm returns the expected value", ?_assertEqual(<<"4a6863.sip.2600hz.local">>, kzd_accounts:realm(MasterAccount))}
    ,{"validate realm returns 'undefined' if not found", ?_assertEqual('undefined', kzd_accounts:realm(Missing))}
    ,{"validate realm can return a default value if not found", ?_assertEqual(<<"default">>, kzd_accounts:realm(Missing, <<"default">>))}
    ,{"validate set_realm changes the realm", ?_assertEqual(<<"updated">>, kzd_accounts:realm(Updated))}
    ].

test_language() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"language">>, MasterAccount),
    Updated = kzd_accounts:set_language(MasterAccount, <<"updated">>),
    [{"validate language returns the expected value", ?_assertEqual(<<"en-US">>, kzd_accounts:language(MasterAccount))}
    ,{"validate language returns 'undefined' if not found", ?_assertEqual('undefined', kzd_accounts:language(Missing))}
    ,{"validate language can return a default value if not found", ?_assertEqual(<<"default">>, kzd_accounts:language(Missing, <<"default">>))}
    ,{"validate set_language changes the language", ?_assertEqual(<<"updated">>, kzd_accounts:language(Updated))}
    ].

test_timezone() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"timezone">>, MasterAccount),
    Invalid = kz_json:set_value(<<"timezone">>, <<"inherit">>, MasterAccount),
    Updated = kzd_accounts:set_timezone(MasterAccount, <<"updated">>),
    Default = kzd_accounts:default_timezone(),
    [{"validate timezone returns the expected value", ?_assertEqual(<<"America/Los_Angeles">>, kzd_accounts:timezone(MasterAccount))}
    ,{"validate timezone returns the default if not found", ?_assertEqual(Default, kzd_accounts:timezone(Missing))}
    ,{"validate timezone returns the default if set to 'inherit'", ?_assertEqual(Default, kzd_accounts:timezone(Invalid))}
    ,{"validate timezone can return a default value if not found", ?_assertEqual(<<"default">>, kzd_accounts:timezone(Missing, <<"default">>))}
    ,{"validate set_timezone changes the timezone", ?_assertEqual(<<"updated">>, kzd_accounts:timezone(Updated))}
    ].

test_parent_account_id() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kzd_accounts:fetch(?FIXTURE_RESELLER_ACCOUNT_ID),
    {'ok', SubSubAccount} = kzd_accounts:fetch(?FIXTURE_PARENT_ACCOUNT_ID),
    [{"verify that fetching the parent id of the master account returns 'undefined'"
     ,?_assertEqual('undefined', kzd_accounts:parent_account_id(MasterAccount))
     }
    ,{"verify that fetching the parent id of sub account is the master account"
     ,?_assertEqual(?FIXTURE_MASTER_ACCOUNT_ID, kzd_accounts:parent_account_id(SubAccount))
     }
    ,{"verify fetching the parent id of a sub-sub account is the direct ancestor"
     ,?_assertEqual(?FIXTURE_RESELLER_ACCOUNT_ID, kzd_accounts:parent_account_id(SubSubAccount))
     }
    ].

test_account_tree() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kzd_accounts:fetch(?FIXTURE_RESELLER_ACCOUNT_ID),
    {'ok', SubSubAccount} = kzd_accounts:fetch(?FIXTURE_PARENT_ACCOUNT_ID),
    [?_assertEqual([], kzd_accounts:tree(MasterAccount))
    ,?_assertEqual([?FIXTURE_MASTER_ACCOUNT_ID], kzd_accounts:tree(SubAccount))
    ,?_assertEqual([?FIXTURE_MASTER_ACCOUNT_ID, ?FIXTURE_RESELLER_ACCOUNT_ID], kzd_accounts:tree(SubSubAccount))
    ].

test_notification_preference() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_notification_preference">>, MasterAccount),
    Updated = kzd_accounts:set_notification_preference(MasterAccount, <<"notify">>),
    [{"validate notification_preference returns the expected value", ?_assertEqual(<<"teletype">>, kzd_accounts:notification_preference(MasterAccount))}
    ,{"validate notification_preference returns 'undefined' if not found", ?_assertEqual('undefined', kzd_accounts:notification_preference(Missing))}
    ,{"validate set_notification_preference changes the notification_preference", ?_assertEqual(<<"notify">>, kzd_accounts:notification_preference(Updated))}
    ].

test_enabled() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    Disabled = kzd_accounts:disable(MasterAccount),
    ReEnabled = kzd_accounts:enable(MasterAccount),
    [{"validate is_enabled returns the expected value", ?_assert(kzd_accounts:is_enabled(MasterAccount))}
    ,{"validate disable returns the expected value", ?_assertNot(kzd_accounts:is_enabled(Disabled))}
    ,{"validate enable returns the expected value", ?_assert(kzd_accounts:is_enabled(ReEnabled))}
    ,{"validate enabled is false when account is undefined", ?_assertNot(kzd_accounts:is_enabled('undefined'))}
    ].

test_expired() ->
    [{"validate is_expired is false when account is undefined", ?_assertNot(kzd_accounts:is_expired('undefined'))}].


test_account_hierarchy() ->
    [?_assertNot(kzd_accounts:is_in_account_hierarchy('undefined', ?FIXTURE_MASTER_ACCOUNT_ID))
    ,?_assertNot(kzd_accounts:is_in_account_hierarchy('undefined', ?FIXTURE_MASTER_ACCOUNT_ID))
    ,?_assertNot(kzd_accounts:is_in_account_hierarchy('undefined', ?FIXTURE_MASTER_ACCOUNT_ID, 'true'))
    ,?_assertNot(kzd_accounts:is_in_account_hierarchy('undefined', ?FIXTURE_MASTER_ACCOUNT_ID, 'false'))
    ,?_assertNot(kzd_accounts:is_in_account_hierarchy(?FIXTURE_MASTER_ACCOUNT_ID, 'undefined', 'false'))
    ,?_assertNot(kzd_accounts:is_in_account_hierarchy(?FIXTURE_MASTER_ACCOUNT_ID, 'undefined', 'true'))
    ,?_assert(kzd_accounts:is_in_account_hierarchy(?FIXTURE_MASTER_ACCOUNT_ID, ?FIXTURE_MASTER_ACCOUNT_ID, 'true'))
    ].

test_api_key() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_api_key">>, MasterAccount),
    Updated = kzd_accounts:set_api_key(MasterAccount, <<"updated">>),
    [{"validate api_key returns the expected value", ?_assertEqual(<<"apikey0000000000000000000000000000000000000000000000000000000001">>, kzd_accounts:api_key(MasterAccount))}
    ,{"validate api_key returns 'undefined' if not found", ?_assertEqual('undefined', kzd_accounts:api_key(Missing))}
    ,{"validate set_api_key changes the api_key", ?_assertEqual(<<"updated">>, kzd_accounts:api_key(Updated))}
    ].

test_superduper_admin() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_superduper_admin">>, MasterAccount),
    Updated = kzd_accounts:set_superduper_admin(MasterAccount, 'false'),
    [{"validate superduper_admin returns the expected value", ?_assert(kzd_accounts:is_superduper_admin(MasterAccount))}
    ,{"validate superduper_admin returns 'false' if not found", ?_assertNot(kzd_accounts:is_superduper_admin(Missing))}
    ,{"validate set_superduper_admin changes the superduper_admin", ?_assertNot(kzd_accounts:is_superduper_admin(Updated))}
    ,{"validate is_superduper_admin is false when account is undefined", ?_assertNot(kzd_accounts:is_superduper_admin('undefined'))}
    ].

test_allow_number_additions() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_wnm_allow_additions">>, MasterAccount),
    Updated = kzd_accounts:set_allow_number_additions(MasterAccount, 'false'),
    [{"validate allow_number_additions returns the expected value", ?_assert(kzd_accounts:allow_number_additions(MasterAccount))}
    ,{"validate allow_number_additions returns 'undefined' if not found", ?_assertNot(kzd_accounts:allow_number_additions(Missing))}
    ,{"validate set_allow_number_additions changes the allow_number_additions", ?_assertNot(kzd_accounts:allow_number_additions(Updated))}
    ].

trial_time_test_() ->
    Now = kz_time:now_s(),
    Passed = kzd_accounts:set_trial_expiration(kzd_accounts:new(), Now - 10000),
    Active = kzd_accounts:set_trial_expiration(kzd_accounts:new(), Now + 10000),

    [{"testing expired trial accounts are computed as such"
     ,?_assertEqual('true', kzd_accounts:trial_has_expired(Passed, Now))
     }
    ,{"testing current trial accounts are computed as such"
     ,?_assertEqual('false', kzd_accounts:trial_has_expired(Active, Now))
     }
    ,{"testing that current trial accounts have proper time left computed"
     ,?_assertEqual(10000, kzd_accounts:trial_time_left(Active, Now))
     }
    ,{"testing that expired trial accounts have proper time since expiration computed"
     ,?_assertEqual(-10000, kzd_accounts:trial_time_left(Passed, Now))
     }
    ].

test_reseller() ->
    {'ok', MasterAccount} = kzd_accounts:fetch(?FIXTURE_MASTER_ACCOUNT_ID),
    DemotedMasterAccount = kzd_accounts:demote(MasterAccount),
    Missing = kz_json:delete_key(<<"pvt_reseller">>, MasterAccount),
    {'ok', SubAccount} = kzd_accounts:fetch(?FIXTURE_RESELLER_ACCOUNT_ID),
    Demoted = kzd_accounts:demote(SubAccount),
    RePromoted = kzd_accounts:promote(SubAccount),
    [{"validate master account is a reseller if improperly configured", ?_assert(kzd_accounts:is_reseller(DemotedMasterAccount))}
    ,{"validate master account is a reseller if the value is missing", ?_assert(kzd_accounts:is_reseller(Missing))}
    ,{"validate is_reseller returns the expected value", ?_assert(kzd_accounts:is_reseller(SubAccount))}
    ,{"validate promote returns the expected value", ?_assert(kzd_accounts:is_reseller(RePromoted))}
    ,{"validate demote returns the expected value", ?_assertNot(kzd_accounts:is_reseller(Demoted))}
    ].

validate(Schema, AccountDoc) ->
    case kz_json_schema:validate(Schema
                                ,AccountDoc
                                ,[{'schema_loader_fun', fun kz_json_schema:fload/1}]
                                )
    of
        {'ok', _}=OK -> OK;
        {'error', Errors}=ERR ->
            ?debugFmt("~nvalidation failed for ~p:~n~p~n~n"
                     ,[AccountDoc
                      ,[kz_json:encode(Error) || {_Code, _Msg, Error} <- kz_json_schema:errors_to_jobj(Errors)]
                      ]
                     ),
            ERR
    end.
