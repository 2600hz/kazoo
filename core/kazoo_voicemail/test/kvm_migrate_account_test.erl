-module(kvm_migrate_account_test).

-include_lib("eunit/include/eunit.hrl").
-include("kz_voicemail.hrl").

-define(ACCOUNT_ID_UNDER_TEST, <<"account0000000000000000000000001">>).
-define(ACCOUNT_DB_UNDER_TEST, <<"account%2Fac%2Fco%2Funt0000000000000000000000001">>).

-define(LEGACY_VIEW, <<"vmboxes/legacy_msg_by_timestamp">>).

-spec kvm_migrate_account_test_() -> any().
kvm_migrate_account_test_() ->
    {setup
    ,fun setup_fixtures/0
    ,fun cleanup/1
    ,fun(_) -> [{"Testing manual account voicemail migration", test_manual_voicemail_account()}
               ,{"Validating mocked functions", validate_mock()}
               ]
     end
    }.

setup_fixtures() ->
    ?LOG_DEBUG(":: Setting up Voice mail Manual Migration test"),


    {ok, _} = application:ensure_all_started(kazoo_config),
    {ok, LinkPid} = kazoo_data_link_sup:start_link(),

    meck:new(kz_datamgr, [unstick, passthrough]),
    meck:expect(kz_datamgr, get_results, check_kz_datamgr_history()),

    meck:new(kz_fixturedb_db, [unstick, passthrough]),
    meck:expect(kz_fixturedb_db, db_exists, this_month_db_exists()),

    % lager:set_loglevel(lager_console_backend, none),
    % lager:set_loglevel(lager_file_backend, none),
    % lager:set_loglevel(lager_syslog_backend, none),

    LinkPid.

cleanup(LinkPid) ->
    _ = erlang:exit(LinkPid, normal),
    _ = application:stop(kazoo_config),
    ?LOG_DEBUG(":: Stopped Kazoo FixtureDB"),
    meck:unload().

test_manual_voicemail_account() ->
    Result = [{<<"total_processed">>, 10}
             ,{<<"total_succeeded">>, 8}
             ,{<<"total_failed">>, 0}
             ,{<<"total_no_modb">>, 0}
             ,{<<"total_no_timestamp">>, 4}
             ,{<<"total_no_ids">>, 2}
             ],
    [{"Trying to migrate test account"
     ,?_assertEqual(Result, kvm_migrate_account:manual_account_migrate(<<"account0000000000000000000000001">>))
     }
    ].

validate_mock() ->
    [{"Validating mocked kz_datamgr"
     ,?_assertEqual(true, meck:validate(kz_datamgr))
     },
     {"Validating mocked kz_fixturedb_db"
     ,?_assertEqual(true, meck:validate(kz_fixturedb_db))
     }
    ].

%% this is required for voice mail messages which their timestamp is missing from metadata and
%% their private media is missing or both private media and mailbox doesn't have create/modified
this_month_db_exists() ->
    fun(Server, Db) ->
            %% why meck can't call the original mfa properly?
            kz_datamgr:db_classification(Db) =:= 'modb'
                andalso is_db_under_test(Db, kazoo_modb:get_modb(kz_util:format_account_id(Db)))
                orelse erlang:apply('kz_fixturedb_db_meck_original', db_exists, [Server, Db])
    end.

is_db_under_test(ThisMonth, ThisMonth) ->                                             true;
is_db_under_test(<<"account%2Fac%2Fco%2Funt0000000000000000000000001-201710">>, _) -> true;
is_db_under_test(_, _) ->                                                             false.

-define(GET_LAGACY_CALL, {kz_datamgr, get_results, [?ACCOUNT_DB_UNDER_TEST, ?LEGACY_VIEW, [{limit, 2000}, descending]]}).

%% Checking history calls  to kz_datamgr:get_results to see if any calls happens to
%% get legacy messages, if yes return empty result to stop the process.
check_kz_datamgr_history() ->
    fun(Db, ?LEGACY_VIEW=View, Options) ->
            History = meck:history(kz_datamgr),

            %% meck:history returns:
            %% * {CallerPid, MFA, Result}
            %% * {CallerPid, MFA, ExceptionClass, ExceptionReason, StackTrace}
            case lists:keyfind(?GET_LAGACY_CALL, 2, History) of
                {_Pid, _MFA, {ok, ViewResult}} when length(ViewResult) == 10 ->
                    %% ViewResult length is 10. This should be same as what is inside vmboxes+legacy_msg_by_timestamp.json
                    %% in ?ACCOUNT_DB_UNDER_TEST FixtureDb directory.
                    {ok, []};
                _ ->
                    meck:passthrough([Db, View, Options])
            end;
       (Db, View, Options) ->
            meck:passthrough([Db, View, Options])
    end.
