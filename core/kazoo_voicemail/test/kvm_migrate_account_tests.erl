%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kvm_migrate_account_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").
-include("kz_voicemail.hrl").

-define(LEGACY_VIEW, <<"vmboxes/legacy_msg_by_timestamp">>).

-spec kvm_migrate_account_test_() -> any().
kvm_migrate_account_test_() ->
    {'setup'
    ,fun setup_fixtures/0
    ,fun cleanup/1
    ,fun(_) -> [{"Testing manual account voicemail migration", test_manual_voicemail_account()}
               ,{"Testing manual mailbox migration", test_manual_mailbox()}
               ,{"Validating mocked functions", validate_mock()}
               ]
     end
    }.

setup_fixtures() ->
    ?LOG_DEBUG(":: Setting up Voice mail Manual Migration test"),

    Pid = kz_fixturedb_util:start_me(),

    meck:new('kz_datamgr', ['unstick', 'passthrough']),
    meck:expect('kz_datamgr', 'get_results', check_kz_datamgr_history()),

    meck:new('kz_fixturedb_db', ['unstick', 'passthrough']),
    meck:expect('kz_fixturedb_db', 'db_exists', this_month_db_exists()),

    Pid.

cleanup(Pid) ->
    kz_fixturedb_util:stop_me(Pid),
    meck:unload().

test_manual_mailbox() ->
    Result = [{<<"total_processed">>, 10}
             ,{<<"total_succeeded">>, 6}
              %% couldn't find a better way, "vmbox01-msg06-now_s" for manual mailbox migration
              %% is getting it's own timestamp from vmbox (since the vmbox is opened by directly)
              %% so its modb is 201709 and it would failed because this db is not exists.
              %% But for account migration test, I explicitly removed box_timestamp to force testing
              %% setting missing timestamp to now_s (see is_db_under_test/1)
             ,{<<"total_failed">>, 2}
             ,{<<"total_no_ids">>, 2}
             ],
    [{"Trying to migrate test mailbox"
     ,?_assertEqual(Result, kvm_migrate_account:manual_vmbox_migrate(?FIXTURE_MASTER_ACCOUNT_ID, <<"vmbox01">>))
     }
    ].

test_manual_voicemail_account() ->
    %% But for account migration test, I explicitly removed box_timestamp to force testing
    %% setting missing timestamp to now_s (see above)
    Result = [{<<"total_processed">>, 10}
             ,{<<"total_succeeded">>, 7}
             ,{<<"total_failed">>, 1}
             ,{<<"total_no_ids">>, 2}
             ],
    [{"Trying to migrate test account"
     ,?_assertEqual(Result, kvm_migrate_account:manual_account_migrate(?FIXTURE_MASTER_ACCOUNT_ID))
     }
    ].

validate_mock() ->
    [{"Validating mocked kz_datamgr"
     ,?_assertEqual('true', meck:validate('kz_datamgr'))
     },
     {"Validating mocked kz_fixturedb_db"
     ,?_assertEqual('true', meck:validate('kz_fixturedb_db'))
     }
    ].

%% this is required for voice mail messages which their timestamp is missing from metadata and
%% their private media is missing or both private media and mailbox doesn't have create/modified
this_month_db_exists() ->
    fun(Server, Db) ->
            %% why meck can't call the original mfa properly?
            %% meck_code_gen:get_current_call/1 returns undefined and it cause bad_match
            %% in meck:passthrough/1
            kz_datamgr:db_classification(Db) =:= 'modb'
                andalso is_db_under_test(Db, kazoo_modb:get_modb(kzs_util:format_account_id(Db)))
                orelse erlang:apply('kz_fixturedb_db_meck_original', 'db_exists', [Server, Db])
    end.

is_db_under_test(ThisMonth, ThisMonth) -> 'true';
is_db_under_test(ThisMonth, _) ->
    Expected = <<(?FIXTURE_MASTER_ACCOUNT_DB)/binary, "-201710">>,
    case ThisMonth of
        Expected -> 'true';
        _Else -> 'false'
    end.

-define(GET_LEGACY_CALL, {'kz_datamgr', 'get_results', [?FIXTURE_MASTER_ACCOUNT_DB, ?LEGACY_VIEW, [{'limit', 2000}, 'descending']]}).

%% Checking history calls to kz_datamgr:get_results to see if any calls happens to
%% get legacy messages, if yes return empty result to stop the process.
check_kz_datamgr_history() ->
    fun(Db, ?LEGACY_VIEW=View, Options) ->
            History = meck:history('kz_datamgr'),

            %% meck:history returns:
            %% * {CallerPid, MFA, Result}
            %% * {CallerPid, MFA, ExceptionClass, ExceptionReason, StackTrace}
            case lists:keyfind(?GET_LEGACY_CALL, 2, History) of
                {_Pid, _MFA, {'ok', ViewResult}} when length(ViewResult) == 10 ->
                    %% ViewResult length is 10. This should be same as what is inside vmboxes+legacy_msg_by_timestamp.json
                    %% in ?FIXTURE_MASTER_ACCOUNT_DB FixtureDb directory.
                    {'ok', []};
                _ ->
                    meck:passthrough([Db, View, Options])
            end;
       (Db, View, Options) ->
            meck:passthrough([Db, View, Options])
    end.
