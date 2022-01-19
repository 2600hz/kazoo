%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2022, 2600Hz
%%% @doc Test creating MODBs ahead of time
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_kt_modb_creation).

-export([seq/0
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).

-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar']
                             ,['cb_accounts']
                             ),
    AccountId = create_account(API),

    {Year, Month, Day} = erlang:date(),
    CurrentMODB = kz_util:format_account_id(AccountId, Year, Month),

    'true' = kz_datamgr:db_exists(CurrentMODB),

    %% ensure we trigger the task
    _ = kapps_config:set_default(<<"tasks.modb_creation">>, <<"creation_day">>, Day),
    %% create all MODBs in the first time unit
    _ = kapps_config:set_default(<<"tasks.modb_creation">>, <<"create_in_parallel">>, 1000),

    {NextYear, NextMonth, _} = kz_date:normalize({Year, Month+1, 1}),
    NextMODB = kz_util:format_account_id(AccountId, NextYear, NextMonth),

    'false' = check_for_creation(NextMODB, 0),
    lager:info("modb ~s not created yet", [NextMODB]),

    %% trigger the task
    kt_modb_creation:handle_req(),

    lager:info("checking for '~s'", [NextMODB]),
    'true' = check_for_creation(NextMODB, 5 * ?MILLISECONDS_IN_SECOND),

    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    cleanup(API).

check_for_creation(NextMODB, WaitForMs) ->
    check_for_creation(NextMODB, WaitForMs, kz_time:start_time()).

check_for_creation(NextMODB, WaitForMs, StartTime) ->
    case kz_datamgr:db_exists(NextMODB) of
        'true' ->
            lager:info("found ~s", [NextMODB]),
            'true';
        'false' ->
            ElapsedMs = kz_time:elapsed_ms(StartTime),
            case ElapsedMs > WaitForMs of
                'true' ->
                    lager:info("failed to find ~s in ~p(~p)", [NextMODB, WaitForMs, ElapsedMs]),
                    'false';
                'false' ->
                    timer:sleep(50),
                    check_for_creation(NextMODB, WaitForMs, StartTime)
            end
    end.

create_account(API) ->
    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),
    ?INFO("created account: ~s", [AccountResp]),

    kz_json:get_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)).

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),
    cleanup_system().

cleanup(API) ->
    ?INFO("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() -> 'ok'.
