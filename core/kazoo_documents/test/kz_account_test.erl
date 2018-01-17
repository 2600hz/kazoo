%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_account_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(ID, <<"_id">>).
-define(TREE, <<"pvt_tree">>).

-define(MASTER_ACCOUNT_ID, <<"account0000000000000000000000001">>).
-define(SUB_ACCOUNT_ID, <<"account0000000000000000000000002">>).
-define(SUB_SUB_ACCOUNT_ID, <<"account0000000000000000000000003">>).

kz_account_test_() ->
    {setup
    ,fun setup/0
    ,fun cleanup/1
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
             ,test_api_key()
             ,test_superduper_admin()
             ,test_allow_number_additions()
             ,test_reseller()
             ]
     end
    }.

setup() ->
    ?LOG_DEBUG(":: Setting up Kazoo FixtureDB"),

    {ok, _} = application:ensure_all_started(kazoo_config),
    {ok, LinkPid} = kazoo_data_link_sup:start_link(),

    LinkPid.

cleanup(LinkPid) ->
    _DataLink = erlang:exit(LinkPid, normal),
    Ref = monitor(process, LinkPid),
    receive
        {'DOWN', Ref, process, LinkPid, _Reason} ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: ~p kazoo_config: ~p", [_DataLink, _KConfig])
    after 1000 ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: timeout kazoo_config: ~p", [_KConfig])
    end.

test_account_doc_against_fixture() ->
    {'ok', Schema} = kz_json_schema:fload(<<"accounts">>),
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kz_account:fetch(?SUB_ACCOUNT_ID),
    {'ok', SubSubAccount} = kz_account:fetch(?SUB_SUB_ACCOUNT_ID),
    [{"validate master account fixture", ?_assertMatch({'ok', _}, validate(Schema, MasterAccount))}
    ,{"validate sub account fixture", ?_assertMatch({'ok', _}, validate(Schema, SubAccount))}
    ,{"validate sub-sub account fixture", ?_assertMatch({'ok', _}, validate(Schema, SubSubAccount))}
    ].

test_undefined_account_id() ->
    [?_assertEqual({error,invalid_db_name}, kz_account:fetch(undefined))
    ,?_assertEqual(undefined, kz_account:fetch_realm(undefined))
    ,?_assertEqual(undefined, kz_account:fetch_name(undefined))
    ].

new_test_() ->
    Account = kz_account:new(),
    [{"validate new returns a JSON object", ?_assert(kz_json:is_json_object(Account))}
    ,{"validate new sets the correct doc type", ?_assertEqual(<<"account">>, kz_json:get_value(<<"pvt_type">>, Account))}
    ].

type_test_() ->
    [{"validate type returns the expected value", ?_assertEqual(<<"account">>, kz_account:type())}].

test_account_id() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    [{"validate id returns the expected value", ?_assertEqual(?MASTER_ACCOUNT_ID, kz_account:id(MasterAccount))}].

test_account_name() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"name">>, MasterAccount),
    Updated = kz_account:set_name(MasterAccount, <<"updated">>),
    [{"validate fetch_name returns the expected value", ?_assertEqual(<<"Master Account">>, kz_account:fetch_name(?MASTER_ACCOUNT_ID))}
    ,{"validate name returns the expected value", ?_assertEqual(<<"Master Account">>, kz_account:name(MasterAccount))}
    ,{"validate name returns 'undefined' if not found", ?_assertEqual('undefined', kz_account:name(Missing))}
    ,{"validate name can return a default value if not found", ?_assertEqual(<<"default">>, kz_account:name(Missing, <<"default">>))}
    ,{"validate set_name changes the name", ?_assertEqual(<<"updated">>, kz_account:name(Updated))}
    ].

test_account_realm() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"realm">>, MasterAccount),
    Updated = kz_account:set_realm(MasterAccount, <<"updated">>),
    [{"validate fetch_realm returns the expected value", ?_assertEqual(<<"4a6863.sip.2600hz.local">>, kz_account:fetch_realm(?MASTER_ACCOUNT_ID))}
    ,{"validate realm returns the expected value", ?_assertEqual(<<"4a6863.sip.2600hz.local">>, kz_account:realm(MasterAccount))}
    ,{"validate realm returns 'undefined' if not found", ?_assertEqual('undefined', kz_account:realm(Missing))}
    ,{"validate realm can return a default value if not found", ?_assertEqual(<<"default">>, kz_account:realm(Missing, <<"default">>))}
    ,{"validate set_realm changes the realm", ?_assertEqual(<<"updated">>, kz_account:realm(Updated))}
    ].

test_language() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"language">>, MasterAccount),
    Updated = kz_account:set_language(MasterAccount, <<"updated">>),
    [{"validate language returns the expected value", ?_assertEqual(<<"en-US">>, kz_account:language(MasterAccount))}
    ,{"validate language returns 'undefined' if not found", ?_assertEqual('undefined', kz_account:language(Missing))}
    ,{"validate language can return a default value if not found", ?_assertEqual(<<"default">>, kz_account:language(Missing, <<"default">>))}
    ,{"validate set_language changes the language", ?_assertEqual(<<"updated">>, kz_account:language(Updated))}
    ].

test_timezone() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"timezone">>, MasterAccount),
    Invalid = kz_json:set_value(<<"timezone">>, <<"inherit">>, MasterAccount),
    Updated = kz_account:set_timezone(MasterAccount, <<"updated">>),
    Default = kz_account:default_timezone(),
    [{"validate timezone returns the expected value", ?_assertEqual(<<"America/Los_Angeles">>, kz_account:timezone(MasterAccount))}
    ,{"validate timezone returns the default if not found", ?_assertEqual(Default, kz_account:timezone(Missing))}
    ,{"validate timezone returns the default if set to 'inherit'", ?_assertEqual(Default, kz_account:timezone(Invalid))}
    ,{"validate timezone can return a default value if not found", ?_assertEqual(<<"default">>, kz_account:timezone(Missing, <<"default">>))}
    ,{"validate set_timezone changes the timezone", ?_assertEqual(<<"updated">>, kz_account:timezone(Updated))}
    ].

test_parent_account_id() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kz_account:fetch(?SUB_ACCOUNT_ID),
    {'ok', SubSubAccount} = kz_account:fetch(?SUB_SUB_ACCOUNT_ID),
    [{"verify that fetching the parent id of the master account returns 'undefined'"
     ,?_assertEqual('undefined', kz_account:parent_account_id(MasterAccount))
     }
    ,{"verify that fetching the parent id of sub account is the master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, kz_account:parent_account_id(SubAccount))
     }
    ,{"verify fetching the parent id of a sub-sub account is the direct ancestor"
     ,?_assertEqual(?SUB_ACCOUNT_ID, kz_account:parent_account_id(SubSubAccount))
     }
    ].

test_account_tree() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kz_account:fetch(?SUB_ACCOUNT_ID),
    {'ok', SubSubAccount} = kz_account:fetch(?SUB_SUB_ACCOUNT_ID),
    [?_assertEqual([], kz_account:tree(MasterAccount))
    ,?_assertEqual([?MASTER_ACCOUNT_ID], kz_account:tree(SubAccount))
    ,?_assertEqual([?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID], kz_account:tree(SubSubAccount))
    ].

test_notification_preference() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_notification_preference">>, MasterAccount),
    Updated = kz_account:set_notification_preference(MasterAccount, <<"notify">>),
    [{"validate notification_preference returns the expected value", ?_assertEqual(<<"teletype">>, kz_account:notification_preference(MasterAccount))}
    ,{"validate notification_preference returns 'undefined' if not found", ?_assertEqual('undefined', kz_account:notification_preference(Missing))}
    ,{"validate set_notification_preference changes the notification_preference", ?_assertEqual(<<"notify">>, kz_account:notification_preference(Updated))}
    ].

test_enabled() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    Disabled = kz_account:disable(MasterAccount),
    ReEnabled = kz_account:enable(MasterAccount),
    [{"validate is_enabled returns the expected value", ?_assert(kz_account:is_enabled(MasterAccount))}
    ,{"validate disable returns the expected value", ?_assertNot(kz_account:is_enabled(Disabled))}
    ,{"validate enable returns the expected value", ?_assert(kz_account:is_enabled(ReEnabled))}
    ].

test_api_key() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_api_key">>, MasterAccount),
    Updated = kz_account:set_api_key(MasterAccount, <<"updated">>),
    [{"validate api_key returns the expected value", ?_assertEqual(<<"apikey0000000000000000000000000000000000000000000000000000000001">>, kz_account:api_key(MasterAccount))}
    ,{"validate api_key returns 'undefined' if not found", ?_assertEqual('undefined', kz_account:api_key(Missing))}
    ,{"validate set_api_key changes the api_key", ?_assertEqual(<<"updated">>, kz_account:api_key(Updated))}
    ].

test_superduper_admin() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_superduper_admin">>, MasterAccount),
    Updated = kz_account:set_superduper_admin(MasterAccount, 'false'),
    [{"validate superduper_admin returns the expected value", ?_assert(kz_account:is_superduper_admin(MasterAccount))}
    ,{"validate superduper_admin returns 'false' if not found", ?_assertNot(kz_account:is_superduper_admin(Missing))}
    ,{"validate set_superduper_admin changes the superduper_admin", ?_assertNot(kz_account:is_superduper_admin(Updated))}
    ].

test_allow_number_additions() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_wnm_allow_additions">>, MasterAccount),
    Updated = kz_account:set_allow_number_additions(MasterAccount, 'false'),
    [{"validate allow_number_additions returns the expected value", ?_assert(kz_account:allow_number_additions(MasterAccount))}
    ,{"validate allow_number_additions returns 'undefined' if not found", ?_assertNot(kz_account:allow_number_additions(Missing))}
    ,{"validate set_allow_number_additions changes the allow_number_additions", ?_assertNot(kz_account:allow_number_additions(Updated))}
    ].

trial_time_test_() ->
    Now = kz_time:now_s(),
    Passed = kz_account:set_trial_expiration(kz_account:new(), Now - 10000),
    Active = kz_account:set_trial_expiration(kz_account:new(), Now + 10000),

    [{"testing expired trial accounts are computed as such"
     ,?_assertEqual('true', kz_account:trial_has_expired(Passed, Now))
     }
    ,{"testing current trial accounts are computed as such"
     ,?_assertEqual('false', kz_account:trial_has_expired(Active, Now))
     }
    ,{"testing that current trial accounts have proper time left computed"
     ,?_assertEqual(10000, kz_account:trial_time_left(Active, Now))
     }
    ,{"testing that expired trial accounts have proper time since expiration computed"
     ,?_assertEqual(-10000, kz_account:trial_time_left(Passed, Now))
     }
    ].

test_reseller() ->
    {'ok', MasterAccount} = kz_account:fetch(?MASTER_ACCOUNT_ID),
    DemotedMasterAccount = kz_account:demote(MasterAccount),
    Missing = kz_json:delete_key(<<"pvt_reseller">>, MasterAccount),
    {'ok', SubAccount} = kz_account:fetch(?SUB_ACCOUNT_ID),
    Demoted = kz_account:demote(SubAccount),
    RePromoted = kz_account:promote(SubAccount),
    [{"validate master account is a reseller if improperly configured", ?_assert(kz_account:is_reseller(DemotedMasterAccount))}
    ,{"validate master account is a reseller if the value is missing", ?_assert(kz_account:is_reseller(Missing))}
    ,{"validate is_reseller returns the expected value", ?_assert(kz_account:is_reseller(SubAccount))}
    ,{"validate promote returns the expected value", ?_assert(kz_account:is_reseller(RePromoted))}
    ,{"validate demote returns the expected value", ?_assertNot(kz_account:is_reseller(Demoted))}
    ].

validate(Schema, Device) ->
    kz_json_schema:validate(Schema
                           ,Device
                           ,[{'schema_loader_fun', fun kz_json_schema:fload/1}]
                           ).
