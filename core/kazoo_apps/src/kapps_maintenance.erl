%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_maintenance).

-export([rebuild_db/1
        ,rebuild_db/2
        ]).
-export([migrate_to_4_0/0]).
-export([migrate/0
        ,migrate/1
        ]).
-export([parallel_migrate/1
        ,parallel_migrate/2
        ]).
-export([migrate_modbs/0
        ,migrate_modbs/1
        ]).
-export([parallel_migrate_modbs/1
        ,parallel_migrate_modbs/2
        ]).
-export([migrate_modbs_ranged/1
        ,migrate_modbs_ranged/2
        ,migrate_modbs_ranged/3
        ]).
-export([parallel_migrate_modbs_ranged/2
        ,parallel_migrate_modbs_ranged/3
        ,parallel_migrate_modbs_ranged/4
        ]).
-export([find_invalid_acccount_dbs/0]).
-export([refresh/0, refresh/1
        ,refresh_account/1
        ,refresh_account_db/1
        ,maybe_delete_db/1
        ]).
-export([import_account/2]).
-export([remove_deprecated_databases/0]).
-export([ensure_reseller_id_accounts/0
        ,ensure_reseller_id_accounts/1
        ]).
-export([ensure_reseller_id_account/1
        ,ensure_reseller_id_account/2
        ,ensure_reseller_id_services/2
        ]).
-export([ensure_aggregates/0
        ,ensure_aggregate/1
        ]).
-export([ensure_aggregate_accounts/0
        ,ensure_aggregate_account/1
        ]).
-export([ensure_aggregate_devices/0
        ,ensure_aggregate_device/1
        ]).
-export([ensure_aggregate_faxboxes/0
        ,ensure_aggregate_faxbox/1
        ]).
-export([ensure_tree_accounts/0
        ,ensure_tree_accounts_dry_run/0
        ,ensure_tree_accounts/1
        ,ensure_tree_accounts_dry_run/1
        ]).
-export([ensure_tree_account/1
        ,ensure_tree_account_dry_run/1
        ]).
-export([cleanup_aggregated_devices/0
        ,cleanup_aggregated_device/1
        ]).
-export([cleanup_aggregated_accounts/0
        ,cleanup_aggregated_account/1
        ,remove_aggregated_account/1
        ]).
-export([migrate_limits/0, migrate_limits/1]).
-export([migrate_media/0, migrate_media/1]).
-export([purge_doc_type/2, purge_doc_type/3]).
-export([call_id_status/1, call_id_status/2]).

-export([cleanup_voicemail_media/1]).
-export([cleanup_orphan_modbs/0]).

-export([migrate_system/0]).

-export([validate_system_config/1, validate_system_configs/0]).
-export([cleanup_system_config/1, cleanup_system_configs/0]).
-export([check_system_config/1, check_system_configs/0]).

-export([bind/3, unbind/3
        ,binding/1
        ]).

-export([register_views/0
        ,register_views/1
        ]).

-export([bind_and_register_views/3]).

-export([flush_getby_cache/0
        ,flush_account_views/0
        ,get_all_account_views/0
        ,read_all_account_views/0
        ]).

-export([init_system/0
        ,register_account_views/0
        ,register_system_dbs_views/0

        ,refresh_system_views/0
        ]).

-export([check_release/0]).

-include("kazoo_apps.hrl").

-type bind() :: 'migrate' | 'refresh' | 'refresh_account' | 'register_views'.
-type text_or_integer() :: kz_term:text() | integer().

-define(DEVICES_CB_LIST, <<"devices/crossbar_listing">>).
-define(RESELLER_VIEW_FILE, <<"views/reseller.json">>).

-define(ACCOUNTS_AGG_NOTIFY_VIEW_FILE, <<"views/notify.json">>).

-define(VMBOX_VIEW, <<"vmboxes/crossbar_listing">>).
-define(PMEDIA_VIEW, <<"media/listing_private_media">>).
-define(FAXBOX_VIEW, <<"faxbox/crossbar_listing">>).

-define(DEFAULT_PAUSE, 1 * ?MILLISECONDS_IN_SECOND).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec binding(bind() | {bind(), kz_term:ne_binary()}) -> kz_term:ne_binary().
binding('migrate') -> <<"maintenance.migrate">>;
binding('refresh') -> <<"maintenance.refresh">>;
binding('refresh_account') -> <<"maintenance.refresh.account">>;
binding('register_views') -> <<"maintenance.register_views">>;
binding({Common, Specific}) when is_atom(Common), is_binary(Specific) ->
    CommonPath = binding(Common),
    <<CommonPath/binary, ".", Specific/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bind(atom() | {atom(), binary()}, module(), atom()) -> kazoo_bindings:bind_result().
bind(Event, M, F) -> kazoo_bindings:bind(binding(Event), M, F).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec unbind(atom() | {atom(), binary()}, module(), atom()) -> kazoo_bindings:unbind_result().
unbind(Event, M, F) -> kazoo_bindings:unbind(binding(Event), M, F).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rebuild_db(kz_term:text()) -> 'ok'.
rebuild_db(Database) ->
    rebuild_db(Database, 5 * ?MILLISECONDS_IN_SECOND).

-spec rebuild_db(kz_term:text(), text_or_integer()) -> 'ok'.
rebuild_db(Database, Pause) ->
    _ = kz_datamgr:db_delete(Database),
    timer:sleep(kz_term:to_integer(Pause)),
    _ = refresh(Database),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_to_4_0() -> 'no_return'.
migrate_to_4_0() ->
    _ = kazoo_bindings:map(binding({'migrate', <<"4.0">>}), []),
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate() -> 'no_return'.
migrate() ->
    migrate(?DEFAULT_PAUSE).

-spec migrate(text_or_integer()) -> 'no_return'.
migrate(Pause) when not is_integer(Pause) ->
    migrate(kz_term:to_integer(Pause));
migrate(Pause) ->
    _ = migrate_system(),
    _ = kapps_config:migrate(),

    Databases = get_databases(),
    _ = migrate(Pause, Databases),

    _ = kazoo_bindings:map(binding('migrate'), []),

    'no_return'.

-spec migrate(integer(), kz_term:ne_binaries()) -> 'no_return'.
migrate(Pause, Databases) ->
    Accounts = [kzs_util:format_account_db(Db)
                || Db <- Databases,
                   kapps_util:is_account_db(Db)
               ],
    io:format("~p updating dbs...~n", [self()]),
    _ = refresh(Databases, Pause),

    io:format("~p removing deprecated databases...~n", [self()]),
    _  = remove_deprecated_databases(Databases),

    migrate_kapps_account_config(Accounts),

    _ = kazoo_bindings:map(binding('migrate'), [Accounts]),

    'no_return'.

migrate_kapps_account_config([]) ->
    'ok';
migrate_kapps_account_config([AccountDb | AccountDbs]) ->
    kapps_account_config:migrate(AccountDb),
    _ = timer:sleep(?DEFAULT_PAUSE),
    migrate_kapps_account_config(AccountDbs).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec parallel_migrate(text_or_integer()) -> 'no_return'.
parallel_migrate(Workers) ->
    parallel_migrate(Workers, ?DEFAULT_PAUSE).

-spec parallel_migrate(text_or_integer(), text_or_integer()) -> 'no_return'.
parallel_migrate(Workers, Pause) when not is_integer(Workers) ->
    parallel_migrate(kz_term:to_integer(Workers), Pause);
parallel_migrate(Workers, Pause) when not is_integer(Pause) ->
    parallel_migrate(Workers, kz_term:to_integer(Pause));
parallel_migrate(Workers, Pause) when is_integer(Workers) ->
    _ = migrate_system(),
    _ = kapps_config:migrate(),
    Databases = split_parallel_migrate(Workers),
    parallel_migrate(Pause, Databases, []).

-spec parallel_migrate(integer(), kz_term:ne_binaries(), kz_term:references()) -> 'no_return'.
parallel_migrate(_, [], Refs) -> wait_for_parallel_migrate(Refs);
parallel_migrate(Pause, [Databases|Jobs], Refs) ->
    Self = self(),
    Ref = make_ref(),
    _Pid = kz_process:spawn_link(fun parallel_migrate_worker/4, [Ref, Pause, Databases, Self]),
    parallel_migrate(Pause, Jobs, [Ref|Refs]).

-spec parallel_migrate_worker(reference(), integer(), kz_term:ne_binaries(), pid()) -> reference().
parallel_migrate_worker(Ref, Pause, Databases, Parent) ->
    io:format("~p parallel migrate worker ~p with ~p databases~n"
             ,[self(), Ref, length(Databases)]
             ),
    _ = (catch migrate(Pause, Databases)),
    Parent ! Ref.

-spec wait_for_parallel_migrate(kz_term:references()) -> 'no_return'.
wait_for_parallel_migrate([]) ->
    _ = kazoo_bindings:map(binding('migrate'), []),
    'no_return';
wait_for_parallel_migrate([Ref|Refs]) ->
    receive
        Ref -> wait_for_parallel_migrate(Refs)
    end.

-spec split_parallel_migrate(integer()) -> kz_term:ne_binaries().
split_parallel_migrate(Workers) ->
    Databases = get_databases(),
    split_parallel_migrate(Workers, Databases).

-spec split_parallel_migrate(integer(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
split_parallel_migrate(Workers, Databases) ->
    io:format("splitting ~p databases amoung ~p workers~n", [length(Databases), Workers]),
    Jobs = lists:foldl(fun(Index, J) ->
                               dict:store(Index, [], J)
                       end
                      ,dict:new()
                      ,lists:seq(0, Workers - 1)
                      ),
    split_parallel_migrate(Workers, Databases, Jobs, 0).

-spec split_parallel_migrate(integer(), kz_term:ne_binaries(), dict:dict(), integer()) -> kz_term:ne_binaries().
split_parallel_migrate(_Workers, [], Jobs, _Index) ->
    [Databases || {_, Databases} <- dict:to_list(Jobs)];
split_parallel_migrate(Workers, Databases, Jobs, Workers) ->
    split_parallel_migrate(Workers, Databases, Jobs, 0);
split_parallel_migrate(Workers, [Database | Databases], Jobs, Index) ->
    UpdatedJobs = dict:append(Index, Database, Jobs),
    split_parallel_migrate(Workers, Databases, UpdatedJobs, Index + 1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_modbs() -> 'no_return'.
migrate_modbs() ->
    migrate_modbs(?DEFAULT_PAUSE).

-spec migrate_modbs(text_or_integer()) -> 'no_return'.
migrate_modbs(Pause) when not is_integer(Pause) ->
    migrate_modbs(kz_term:to_integer(Pause));
migrate_modbs(Pause) ->
    Databases = [Database
                 || Database <- get_databases()
                        ,kapps_util:is_account_mod(Database)
                ],
    _ = refresh(Databases, Pause),
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec parallel_migrate_modbs(text_or_integer()) -> 'no_return'.
parallel_migrate_modbs(Workers) ->
    parallel_migrate_modbs(Workers, ?DEFAULT_PAUSE).

-spec parallel_migrate_modbs(text_or_integer(), text_or_integer()) -> 'no_return'.
parallel_migrate_modbs(Workers, Pause) when not is_integer(Workers) ->
    parallel_migrate_modbs(kz_term:to_integer(Workers), Pause);
parallel_migrate_modbs(Workers, Pause) when not is_integer(Pause) ->
    parallel_migrate_modbs(Workers, kz_term:to_integer(Pause));
parallel_migrate_modbs(Workers,Pause) ->
    Databases = [Database
                 || Database <- get_databases()
                        ,kapps_util:is_account_mod(Database)
                ],
    SplitDatabases = split_parallel_migrate(Workers, Databases),
    _ = parallel_migrate(Pause, SplitDatabases, []),
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_modbs_ranged(text_or_integer()) -> 'no_return'.
migrate_modbs_ranged(Start) ->
    migrate_modbs_ranged(?DEFAULT_PAUSE, Start).

-spec migrate_modbs_ranged(text_or_integer(), text_or_integer()) -> 'no_return'.
migrate_modbs_ranged(Pause, Start) when not is_integer(Pause) ->
    migrate_modbs_ranged(kz_term:to_integer(Pause), Start);
migrate_modbs_ranged(Pause, Start) when not is_integer(Start) ->
    migrate_modbs_ranged(Pause, kz_term:to_integer(Start));
migrate_modbs_ranged(Pause, Start) ->
    Databases = [Database
                 || Database <- get_databases()
                        ,kapps_util:is_account_mod(Database)
                        ,modb_in_range(Database, Start)
                ],
    _ = refresh(Databases, Pause),
    'no_return'.

-spec migrate_modbs_ranged(text_or_integer(), text_or_integer(), text_or_integer()) -> 'no_return'.
migrate_modbs_ranged(Pause, Start, End) when not is_integer(Pause) ->
    migrate_modbs_ranged(kz_term:to_integer(Pause), Start, End);
migrate_modbs_ranged(Pause, Start, End) when not is_integer(Start) ->
    migrate_modbs_ranged(Pause, kz_term:to_integer(Start), End);
migrate_modbs_ranged(Pause, Start, End) when not is_integer(End) ->
    migrate_modbs_ranged(Pause, Start, kz_term:to_integer(End));
migrate_modbs_ranged(Pause, Start, End) ->
    Databases = [Database
                 || Database <- get_databases()
                        ,kapps_util:is_account_mod(Database)
                        ,modb_in_range(Database, Start, End)
                ],
    _ = refresh(Databases, Pause),
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec parallel_migrate_modbs_ranged(text_or_integer(), text_or_integer()) -> 'no_return'.
parallel_migrate_modbs_ranged(Workers, Start) ->
    parallel_migrate_modbs_ranged(Workers, ?DEFAULT_PAUSE, Start).

-spec parallel_migrate_modbs_ranged(text_or_integer(), text_or_integer(), text_or_integer()) -> 'no_return'.
parallel_migrate_modbs_ranged(Workers, Pause, Start) when not is_integer(Workers) ->
    parallel_migrate_modbs_ranged(kz_term:to_integer(Workers), Pause, Start);
parallel_migrate_modbs_ranged(Workers, Pause, Start) when not is_integer(Pause) ->
    parallel_migrate_modbs_ranged(Workers, kz_term:to_integer(Pause), Start);
parallel_migrate_modbs_ranged(Workers, Pause, Start) when not is_integer(Start) ->
    parallel_migrate_modbs_ranged(Workers, Pause, kz_term:to_integer(Start));
parallel_migrate_modbs_ranged(Workers, Pause, Start) ->
    Databases = [Database
                 || Database <- get_databases()
                        ,kapps_util:is_account_mod(Database)
                        ,modb_in_range(Database, Start)
                ],
    SplitDatabases = split_parallel_migrate(Workers, Databases),
    _ = parallel_migrate(Pause, SplitDatabases, []),
    'no_return'.

-spec parallel_migrate_modbs_ranged(text_or_integer(), text_or_integer(), text_or_integer(), text_or_integer()) -> 'no_return'.
parallel_migrate_modbs_ranged(Workers, Pause, Start, End) when not is_integer(Workers) ->
    parallel_migrate_modbs_ranged(kz_term:to_integer(Workers), Pause, Start, End);
parallel_migrate_modbs_ranged(Workers, Pause, Start, End) when not is_integer(Pause) ->
    parallel_migrate_modbs_ranged(Workers, kz_term:to_integer(Pause), Start, End);
parallel_migrate_modbs_ranged(Workers, Pause, Start, End) when not is_integer(Start) ->
    parallel_migrate_modbs_ranged(Workers, Pause, kz_term:to_integer(Start), End);
parallel_migrate_modbs_ranged(Workers, Pause, Start, End) when not is_integer(End) ->
    parallel_migrate_modbs_ranged(Workers, Pause, Start, kz_term:to_integer(End));
parallel_migrate_modbs_ranged(Workers, Pause, Start, End) ->
    Databases = [Database
                 || Database <- get_databases()
                        ,kapps_util:is_account_mod(Database)
                        ,modb_in_range(Database, Start, End)
                ],
    SplitDatabases = split_parallel_migrate(Workers, Databases),
    _ = parallel_migrate(Pause, SplitDatabases, []),
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec modb_in_range(kz_term:ne_binary(), integer()) -> boolean().
modb_in_range(Database, StartDate) ->
    {_, MODbYear, MODbMonth} = kazoo_modb_util:split_account_mod(Database),
    modb_before_start(StartDate, MODbYear, MODbMonth).

-spec modb_in_range(kz_term:ne_binary(), integer(), integer()) -> boolean().
modb_in_range(Database, StartDate, EndDate) ->
    {_, MODbYear, MODbMonth} = kazoo_modb_util:split_account_mod(Database),
    modb_before_start(StartDate, MODbYear, MODbMonth)
        andalso modb_after_end(EndDate, MODbYear, MODbMonth).

-spec modb_before_start(integer(), integer(), integer()) -> boolean().
modb_before_start(StartDate, MODbYear, MODbMonth) ->
    {StartYear, StartMonth} =
        case kz_term:floor(math:log10(StartDate)) + 1 of
            4 -> {StartDate, 1};
            6 -> {kz_term:to_integer(StartDate/100)
                 ,StartDate rem 100
                 }
        end,
    MODbYear >= StartYear
        andalso MODbMonth >= StartMonth.

-spec modb_after_end(integer(), integer(), integer()) -> boolean().
modb_after_end(EndDate, MODbYear, MODbMonth) ->
    {EndYear, EndMonth} =
        case kz_term:floor(math:log10(EndDate)) + 1 of
            4 -> {EndDate, 12};
            6 -> {kz_term:to_integer(EndDate/100)
                 ,EndDate rem 100
                 }
        end,
    MODbYear =< EndYear
        andalso MODbMonth =< EndMonth.

%%------------------------------------------------------------------------------
%% @doc Register views from all applications that have binding to
%% `register_views'.
%% @end
%%------------------------------------------------------------------------------
-spec register_views() -> 'ok'.
register_views() ->
    _ = kazoo_bindings:map(binding('register_views'), []),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Register views from specific `App' application.
%%
%% `App' needs to bind to `register_views'.
%% @end
%%------------------------------------------------------------------------------
-spec register_views(kz_term:ne_binary() | atom()) -> 'ok'.
register_views(?NE_BINARY=App) ->
    _ = kazoo_bindings:map(binding({'register_views', App}), []),
    'ok';
register_views(App) when is_atom(App) ->
    register_views(kz_term:to_binary(App)).

%%------------------------------------------------------------------------------
%% @doc Bind to register_views and call the registration function at same time.
%% @end
%%------------------------------------------------------------------------------
-spec bind_and_register_views(atom() | kz_term:ne_binary(), atom(), atom()) -> 'ok'.
bind_and_register_views(_AppName, Module, Function) ->
    _ = bind('register_views', Module, Function),
    %% bind({'register_views', kz_term:to_binary(AppName)}, Module, Function),
    _ = Module:Function(),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec refresh() -> 'no_return'.
refresh() ->
    Databases = get_databases(),
    refresh(Databases, ?DEFAULT_PAUSE).

-spec refresh(kz_term:ne_binaries(), kz_term:text() | non_neg_integer()) -> 'no_return'.
refresh(Databases, Pause) ->
    Total = length(Databases),
    refresh(Databases, kz_term:to_integer(Pause), Total).

-spec refresh(kz_term:ne_binaries(), non_neg_integer(), non_neg_integer()) -> 'no_return'.
refresh([], _, _) -> 'no_return';
refresh([Database|Databases], Pause, Total) ->
    io:format("~p (~p/~p) refreshing database '~s'~n"
             ,[self(), length(Databases) + 1, Total, Database]
             ),
    _ = case refresh(Database) of
            {'error', _Reason} ->
                io:format("    - refresh failed: ~p~n", [_Reason]);
            Bol -> Bol
        end,
    _ = case Pause < 1 of
            'false' -> timer:sleep(Pause);
            'true' -> 'ok'
        end,
    refresh(Databases, Pause, Total).

-spec get_databases() -> kz_term:ne_binaries().
get_databases() ->
    {'ok', Databases} = kz_datamgr:db_info(),
    kzs_util:sort_by_priority(Databases ++ ?KZ_SYSTEM_DBS).

-spec refresh(kz_term:ne_binary()) -> 'ok' |
          boolean() |
          {'error', 'invalid_db_name' | 'db_not_found'}.
refresh(Database) ->
    case kz_datamgr:refresh_views(Database) of
        {'error', _} = Error ->
            Error;
        _Bol ->
            _ = kazoo_bindings:map(binding({'refresh', Database}), [Database]),
            refresh_by_classification(Database, kz_datamgr:db_classification(Database))
    end.

refresh_by_classification(Database, 'account') ->
    AccountDb = kzs_util:format_account_db(Database),
    AccountId = kzs_util:format_account_id(Database),
    _ = kazoo_bindings:map(binding({'refresh_account', AccountDb}), AccountId),
    _ = ensure_aggregate(AccountId),
    'ok';
refresh_by_classification(_, _) ->
    'ok'.

-spec refresh_account(kz_term:ne_binary()) -> 'ok'.
refresh_account(AccountId) ->
    refresh_account_db(AccountId).

-spec refresh_account_db(kz_term:ne_binary()) -> 'ok'.
refresh_account_db(Database) ->
    refresh(kzs_util:format_account_db(Database)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_deprecated_databases() -> 'ok'.
remove_deprecated_databases() ->
    Databases = get_databases(),
    remove_deprecated_databases(Databases).

-spec remove_deprecated_databases(kz_term:ne_binaries()) -> 'ok'.
remove_deprecated_databases([]) -> 'ok';
remove_deprecated_databases([Database|Databases]) ->
    _ = case kz_datamgr:db_classification(Database) of
            'deprecated' ->
                io:format("    archive and remove depreciated database ~s~n", [Database]),
                _ = kz_datamgr:db_archive(Database),
                maybe_delete_db(Database);
            _Else -> 'ok'
        end,
    remove_deprecated_databases(Databases).


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_aggregates() -> 'ok'.
ensure_aggregates() ->
    Databases = kapps_util:get_all_accounts(),
    Total = length(Databases),
    ensure_aggregates(Databases, Total).

-spec ensure_aggregates(kz_term:ne_binaries(), non_neg_integer()) -> 'ok'.
ensure_aggregates([], _Total) -> 'ok';
ensure_aggregates([Account|Accounts], Total) ->
    io:format("~p (~p/~p) aggregating database '~s'~n"
             ,[self(), length(Accounts) + 1, Total, Account]
             ),
    _ = ensure_aggregate(Account),
    ensure_aggregates(Accounts, Total).


-spec ensure_aggregate(kz_term:ne_binary()) -> 'ok'.
ensure_aggregate(Account) ->
    io:format("ensuring necessary documents from account '~s' are in aggregate dbs~n"
             ,[Account]
             ),
    _ = ensure_aggregate_faxbox(Account),
    _ = ensure_aggregate_device(Account),
    _ = ensure_aggregate_account(Account),
    _ = kazoo_services_maintenance:reconcile(Account),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_aggregate_faxboxes() -> 'ok'.
ensure_aggregate_faxboxes() ->
    Databases = kapps_util:get_all_accounts(),
    Total = length(Databases),
    ensure_aggregate_faxboxes(Databases, Total).

-spec ensure_aggregate_faxboxes(kz_term:ne_binaries(), non_neg_integer()) -> 'ok'.
ensure_aggregate_faxboxes([], _Total) -> 'ok';
ensure_aggregate_faxboxes([Account|Accounts], Total) ->
    io:format("~p (~p/~p) aggregating faxboxes account '~s'~n"
             ,[self(), length(Accounts) + 1, Total, Account]
             ),
    _ = ensure_aggregate_faxbox(Account),
    ensure_aggregate_faxboxes(Accounts, Total).

-spec ensure_aggregate_faxbox(kz_term:ne_binary()) -> 'ok'.
ensure_aggregate_faxbox(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    case kz_datamgr:get_results(AccountDb, ?FAXBOX_VIEW, ['include_docs']) of
        {'ok', Faxboxes} ->
            update_or_add_to_faxes_db([kz_json:get_value(<<"doc">>, Faxbox) || Faxbox <- Faxboxes]);
        {'error', _} -> 'ok'
    end.

-spec update_or_add_to_faxes_db(kz_json:objects()) -> 'ok'.
update_or_add_to_faxes_db(Faxboxes) ->
    case kz_datamgr:all_docs(?KZ_FAXES_DB, [{'keys', [kz_doc:id(Doc) || Doc <- Faxboxes]}]) of
        {'ok', Docs} ->
            Revs = maps:from_list([{kz_json:get_value(<<"key">>, D)
                                   ,kz_json:get_value([<<"value">>, <<"rev">>], D)
                                   }
                                   || D <- Docs
                                  ]),
            _ = kz_datamgr:save_docs(?KZ_FAXES_DB, prepare_faxboxes(Faxboxes, Revs, [])),
            'ok';
        {'error', _Message} ->
            'ok'
    end.

-spec prepare_faxboxes(kz_json:objects(), #{kz_term:ne_binary() => kz_term:ne_binary()}, kz_json:objects()) -> kz_json:objects().
prepare_faxboxes([], _Revs, Acc) -> Acc;
prepare_faxboxes([Doc|Faxboxes], Revs, Acc) ->
    case maps:get(kz_doc:id(Doc), Revs, 'undefined') of
        'undefined' ->
            prepare_faxboxes(Faxboxes, Revs, [kz_doc:delete_revision(Doc)|Acc]);
        Rev ->
            prepare_faxboxes(Faxboxes, Revs, [kz_doc:set_revision(Doc, Rev)|Acc])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_aggregate_accounts() -> 'ok'.
ensure_aggregate_accounts() ->
    Databases = kapps_util:get_all_accounts(),
    Total = length(Databases),
    ensure_aggregate_accounts(Databases, Total).

-spec ensure_aggregate_accounts(kz_term:ne_binaries(), non_neg_integer()) -> 'ok'.
ensure_aggregate_accounts([], _Total) -> 'ok';
ensure_aggregate_accounts([Account|Accounts], Total) ->
    io:format("~p (~p/~p) aggregating accounts '~s'~n"
             ,[self(), length(Accounts) + 1, Total, Account]
             ),
    _ = ensure_aggregate_account(Account),
    ensure_aggregate_accounts(Accounts, Total).

-spec ensure_aggregate_account(kz_term:ne_binary()) -> 'ok'.
ensure_aggregate_account(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    AccountId = kzs_util:format_account_id(Account),
    case kz_datamgr:open_doc(AccountDb, AccountId) of
        {'error', _} -> 'ok';
        {'ok', JObj} ->
            update_or_add_to_accounts_db(AccountId, JObj)
    end.

-spec update_or_add_to_accounts_db(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
update_or_add_to_accounts_db(AccountId, JObj) ->
    <<"account">> = kz_doc:type(JObj),
    case kz_datamgr:lookup_doc_rev(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', Rev} ->
            _ = kz_datamgr:save_doc(?KZ_ACCOUNTS_DB, kz_doc:set_revision(JObj, Rev)),
            'ok';
        {'error', 'not_found'} ->
            _ = kz_datamgr:save_doc(?KZ_ACCOUNTS_DB, kz_doc:delete_revision(JObj)),
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_aggregate_devices() -> 'ok'.
ensure_aggregate_devices() ->
    Databases = kapps_util:get_all_accounts(),
    Total = length(Databases),
    ensure_aggregate_devices(Databases, Total).

-spec ensure_aggregate_devices(kz_term:ne_binaries(), non_neg_integer()) -> 'ok'.
ensure_aggregate_devices([], _Total) -> 'ok';
ensure_aggregate_devices([Account|Accounts], Total) ->
    io:format("~p (~p/~p) aggregating devices account '~s'~n"
             ,[self(), length(Accounts) + 1, Total, Account]
             ),
    _ = ensure_aggregate_device(Account),
    ensure_aggregate_devices(Accounts, Total).

-spec ensure_aggregate_device(kz_term:ne_binary()) -> 'ok'.
ensure_aggregate_device(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    case kz_datamgr:get_results(AccountDb, ?DEVICES_CB_LIST, ['include_docs']) of
        {'ok', Devices} ->
            AccountRealm = kzd_accounts:fetch_realm(Account),
            _ = remove_aggregate_devices(AccountDb, AccountRealm, Devices),
            refresh_account_devices(AccountDb, AccountRealm, Devices);
        {'error', _} -> 'ok'
    end.

-spec refresh_account_devices(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
refresh_account_devices(AccountDb, AccountRealm, Devices) ->
    _ = [kapps_util:add_aggregate_device(AccountDb, kz_json:get_value(<<"doc">>, Device))
         || Device <- Devices,
            should_aggregate_device(AccountRealm, kz_json:get_value(<<"doc">>, Device))
        ],
    'ok'.

-spec should_aggregate_device(kz_term:ne_binary(), kz_json:object()) -> boolean().
should_aggregate_device(AccountRealm, Device) ->
    kzd_devices:sip_realm(Device, AccountRealm) =/= AccountRealm
        orelse kzd_devices:sip_ip(Device) =/= 'undefined'.

-spec remove_aggregate_devices(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
remove_aggregate_devices(AccountDb, AccountRealm, Devices) ->
    _ = [kapps_util:rm_aggregate_device(AccountDb, kz_json:get_value(<<"doc">>, Device))
         || Device <- Devices,
            should_remove_aggregate(AccountRealm, kz_json:get_value(<<"doc">>, Device))
        ],
    'ok'.

-spec should_remove_aggregate(kz_term:ne_binary(), kz_json:object()) -> boolean().
should_remove_aggregate(AccountRealm, Device) ->
    kzd_devices:sip_realm(Device, AccountRealm) =:= AccountRealm
        andalso kzd_devices:sip_ip(Device) =:= 'undefined'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_aggregated_accounts() -> 'ok'.
cleanup_aggregated_accounts() ->
    case kz_datamgr:all_docs(?KZ_ACCOUNTS_DB, []) of
        {'ok', JObjs} -> cleanup_aggregated_accounts(JObjs);
        _ -> 'ok'
    end.

-spec cleanup_aggregated_accounts(kz_json:objects()) -> 'ok'.
cleanup_aggregated_accounts([]) -> 'ok';
cleanup_aggregated_accounts([JObj|JObjs]) ->
    _ = case kz_doc:id(JObj) of
            <<"_design", _/binary>> -> 'ok';
            AccountId ->
                io:format("    verifying ~s doc ~s~n", [?KZ_ACCOUNTS_DB, AccountId]),
                cleanup_aggregated_account(AccountId)
        end,
    cleanup_aggregated_accounts(JObjs).

-spec cleanup_aggregated_account(kz_term:ne_binary()) -> 'ok'.
cleanup_aggregated_account(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    AccountId = kzs_util:format_account_id(Account),
    case kz_datamgr:open_doc(AccountDb, AccountId) of
        {'error', 'not_found'} -> remove_aggregated_account(AccountDb);
        _Else -> 'ok'
    end.

-spec remove_aggregated_account(kz_term:ne_binary()) -> 'ok'.
remove_aggregated_account(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    {'ok', JObj} = kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId),
    io:format("    removing invalid ~s doc ~s~n", [?KZ_ACCOUNTS_DB, AccountId]),
    _ = kz_datamgr:del_doc(?KZ_ACCOUNTS_DB, JObj),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_aggregated_devices() -> 'ok'.
cleanup_aggregated_devices() ->
    case kz_datamgr:all_docs(?KZ_SIP_DB, []) of
        {'ok', JObjs} -> cleanup_aggregated_devices(JObjs);
        _ -> 'ok'
    end.

-spec cleanup_aggregated_devices(kz_json:objects()) -> 'ok'.
cleanup_aggregated_devices([]) -> 'ok';
cleanup_aggregated_devices([JObj|JObjs]) ->
    _ = case kz_doc:id(JObj) of
            <<"_design", _/binary>> -> 'ok';
            DocId ->
                io:format("    verifying ~s doc ~s~n", [?KZ_SIP_DB, DocId]),
                cleanup_aggregated_device(DocId)
        end,
    cleanup_aggregated_devices(JObjs).

-spec cleanup_aggregated_device(kz_term:ne_binary()) -> 'ok'.
cleanup_aggregated_device(DocId) ->
    {'ok', JObj} = kz_datamgr:open_doc(?KZ_SIP_DB, DocId),
    case kz_json:get_first_defined([<<"pvt_account_db">>
                                   ,<<"pvt_account_id">>
                                   ], JObj)
    of
        'undefined' -> 'ok';
        Account ->
            AccountDb = kzs_util:format_account_db(Account),
            AccountId = kzs_util:format_account_id(Account),
            verify_aggregated_device(AccountDb, AccountId, JObj)
    end.

-spec verify_aggregated_device(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
verify_aggregated_device(AccountDb, AccountId, JObj) ->
    case kz_datamgr:open_doc(AccountDb, AccountId) of
        {'error', 'not_found'} ->
            io:format("    removing ~s doc ~s referencing missing db ~s~n"
                     ,[?KZ_SIP_DB, AccountId, AccountDb]),
            _ = kz_datamgr:del_doc(?KZ_SIP_DB, JObj),
            'ok';
        _Else -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_invalid_acccount_dbs() -> kz_term:ne_binaries().
find_invalid_acccount_dbs() ->
    lists:foldr(fun find_invalid_acccount_dbs_fold/2, [], kapps_util:get_all_accounts()).

find_invalid_acccount_dbs_fold(AccountDb, Acc) ->
    AccountId = kzs_util:format_account_id(AccountDb),
    case kz_datamgr:open_doc(AccountDb, AccountId) of
        {'error', 'not_found'} -> [AccountDb|Acc];
        {'ok', _} -> Acc
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_log_doc_update({'ok', kz_json:object() | kz_json:objects()} | kazoo_data:data_error()
                          ,kz_term:api_ne_binary()
                          ,kz_term:ne_binary()
                          ) -> 'ok'.
maybe_log_doc_update({'ok', _}, 'undefined', _) -> 'ok';
maybe_log_doc_update({'ok', _}, Success, _) ->
    io:format("~s~n", [Success]);
maybe_log_doc_update({'error', _Reason}, _, Failed) ->
    io:format("~s: ~p~n", [Failed, _Reason]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-type ensure_state() :: #{master := kz_term:ne_binary()
                         ,real_ids := gb_sets:set()
                         ,fixed_trees := map()
                         }.

-spec get_accounts_tree() -> kz_datamgr:get_results_return().
get_accounts_tree() ->
    ViewOptions = [{'reduce', 'false'}],
    ViewName = <<"accounts_tree/list_by_tree_length">>,
    kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ViewName, ViewOptions).

-spec get_accounts_tree(kz_term:ne_binaries()) -> kz_datamgr:get_results_return().
get_accounts_tree(Accounts) ->
    ViewOptions = [{'keys', [kzs_util:format_account_id(A) || A <- Accounts]}
                  ,'include_docs'
                  ],
    ViewName = <<"accounts/listing_by_simple_id">>,
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ViewName, ViewOptions) of
        {'ok', Results} ->
            {'ok', [format_result(JObj) || JObj <- Results]};
        {'error', _}=Error -> Error
    end.

-spec format_result(kz_json:object()) -> kz_json:object().
format_result(JObj) ->
    Tree = kzd_accounts:tree(kz_json:get_json_value(<<"doc">>, JObj)),
    kz_json:from_list([{<<"id">>, kz_doc:id(JObj)}
                      ,{<<"key">>, length(Tree)}
                      ,{<<"value">>, Tree}
                      ]
                     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_tree_accounts() -> 'ok'.
ensure_tree_accounts() ->
    actually_ensure_tree_accounts(get_accounts_tree(), 'false').

%% all accounts (dry run)
-spec ensure_tree_accounts_dry_run() -> 'ok'.
ensure_tree_accounts_dry_run() ->
    actually_ensure_tree_accounts(get_accounts_tree(), 'true').

%% acounts
-spec ensure_tree_accounts(kz_term:ne_binaries()) -> 'ok'.
ensure_tree_accounts(Accounts) ->
    actually_ensure_tree_accounts(get_accounts_tree(Accounts), 'false').

%% acounts (dry run)
-spec ensure_tree_accounts_dry_run(kz_term:ne_binaries()) -> 'ok'.
ensure_tree_accounts_dry_run(Accounts) ->
    actually_ensure_tree_accounts(get_accounts_tree(Accounts), 'true').

%% single acount
-spec ensure_tree_account(kz_term:ne_binary()) -> 'ok'.
ensure_tree_account(Account) ->
    ensure_tree_account(Account, 'false').

%% single acount (dry run)
-spec ensure_tree_account_dry_run(kz_term:ne_binary()) -> 'ok'.
ensure_tree_account_dry_run(Account) ->
    ensure_tree_account(Account, 'true').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_tree_accounts(kz_term:ne_binaries(), boolean() | kz_term:ne_binary()) -> 'ok'.
ensure_tree_accounts(Accounts, DryRun) ->
    actually_ensure_tree_accounts(get_accounts_tree(Accounts), DryRun).

-spec ensure_tree_account(kz_term:ne_binary(), boolean() | kz_term:ne_binary()) -> 'ok'.
ensure_tree_account(Account, DryRun) ->
    ensure_tree_accounts([Account], DryRun).

%% actually ensuring
-spec actually_ensure_tree_accounts(kazoo_data:get_results_return(), boolean() | kz_term:ne_binary()) -> 'ok'.
actually_ensure_tree_accounts({'ok', JObjs}, DryRun) ->
    io:format("::: ensuring pvt_tree is clean for ~b account(s)~n~n", [length(JObjs)]),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    State = #{master => MasterAccountId
             ,real_ids => gb_sets:new()
             ,fixed_trees => #{}
             },
    {NewState, Families} = create_families(JObjs, State, #{}),
    CalculateState =
        maps:fold(fun(Depth, Family, StateAcc) ->
                          ensure_tree_is_tree(StateAcc, Family, Depth)
                  end
                 ,NewState
                 ,Families
                 ),
    maybe_fix_accounts_tree(CalculateState, kz_term:is_true(DryRun));
actually_ensure_tree_accounts({'error', _Reason}, _) ->
    io:format("failed to get accounts tree to compare: ~p~n", [_Reason]).

-spec maybe_fix_accounts_tree(ensure_state(), boolean()) -> 'ok'.
maybe_fix_accounts_tree(#{fixed_trees := Fixed}, _) when map_size(Fixed) =:= 0 ->
    io:format("~n==> nothing to fix, all hail kazoo!~n");
maybe_fix_accounts_tree(#{fixed_trees := Fixed}, 'true') ->
    io:put_chars(kz_term:to_binary(
                   ["\n==> fixed tree:\n"
                   ,kz_json:encode(kz_json:from_map(Fixed), ['pretty'])
                   ,"\n\n"
                   ]
                  )
                );
maybe_fix_accounts_tree(#{fixed_trees := Fixed}=State, 'false') ->
    io:format("~n==> going to fix pvt_tree~n~n"),
    TotalDepth = maps:size(State),
    _ = maps:fold(fun(Depth, AccountsMap, Curr) ->
                          io:format("--> (~b/~b) fixing pvt_tree for depth ~s~n"
                                   ,[Curr, TotalDepth, Depth]
                                   ),
                          _ = fix_accounts_tree(AccountsMap),
                          Curr - 1
                  end
                 ,TotalDepth
                 ,Fixed
                 ),
    'ok'.

-spec fix_accounts_tree(map()) -> integer().
fix_accounts_tree(AccountsMap) ->
    Total = maps:size(AccountsMap),
    maps:fold(fun(AccountId, Tree, Curr) ->
                      io:format("         - (~b/~b) fixing pvt_tree for '~s'~n"
                               ,[Curr, Total, AccountId]
                               ),
                      fix_account_tree(AccountId, Tree),
                      _ = timer:sleep(500),
                      Curr - 1
              end
             ,Total
             ,AccountsMap
             ).

-spec fix_account_tree(kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
fix_account_tree(AccountId, Tree) ->
    Updates =[{kzd_accounts:path_tree(), Tree}],
    maybe_log_doc_update(kzd_accounts:update(AccountId, Updates)
                        ,'undefined'
                        ,<<"    !!! failed to save account definitions">>
                        ),
    fix_services_tree(AccountId, Tree),
    fix_port_requests_tree(AccountId, Tree).

-spec fix_services_tree(kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
fix_services_tree(AccountId, Tree) ->
    Services = kz_services:fetch(AccountId),
    fix_services_tree(Services, Tree, kz_services:services_jobj(Services)).

-spec fix_services_tree(kz_services:services(), kz_term:ne_binaries(), kzd_services:doc()) -> 'ok'.
fix_services_tree(Services, Tree, ServicesJObj) ->
    case kzd_services:tree(ServicesJObj) =:= Tree of
        'true' -> 'ok';
        'false' ->
            io:format("           --> fixing pvt_tree in service doc~n"),
            NewJObj = kzd_services:set_tree(ServicesJObj, Tree),
            _ = kz_services:save_services_jobj(kz_services:set_services_jobj(Services, NewJObj)),
            'ok'
    end.

-spec fix_port_requests_tree(kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
fix_port_requests_tree(AccountId, Tree) ->
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ,'include_docs'
                  ],
    ViewName = <<"port_requests/crossbar_listing">>,
    case kz_datamgr:get_results(?KZ_PORT_REQUESTS_DB, ViewName, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} ->
            UpdatedJObjs = [kzd_port_requests:set_pvt_tree(JObj, Tree)
                            || J <- JObjs,
                               JObj <- [kz_json:get_value(<<"doc">>, J)],
                               Tree =/= kzd_port_requests:pvt_tree(JObj)
                           ],
            maybe_log_doc_update(kz_datamgr:save_docs(?KZ_PORT_REQUESTS_DB, UpdatedJObjs)
                                ,<<"           --> fixed pvt_tree for"
                                  ,(kz_term:to_binary(length(UpdatedJObjs)))/binary, "port request docs"
                                 >>
                                ,<<"    !!! failed to update pvt_tree for port_requests">>
                                );
        {'error', _Reason} ->
            io:format("    !!! failed to get port requests: ~p~n", [_Reason])
    end.

-spec ensure_tree_is_tree(ensure_state(), map(), non_neg_integer()) ->
          ensure_state().
ensure_tree_is_tree(State, Family, Depth) ->
    io:format("--> calculating tree for depth ~b~n", [Depth]),
    maps:fold(fun(AccountId, Tree, StateAcc) ->
                      ensure_tree_is_tree_fold(StateAcc, AccountId, Tree)
              end
             ,State
             ,Family
             ).

-spec ensure_tree_is_tree_fold(ensure_state(), kz_term:ne_binary(), kz_term:ne_binaries()) ->
          ensure_state().
ensure_tree_is_tree_fold(#{fixed_trees := FixedTrees
                          }=State
                        ,AccountId, Tree) ->
    UpdatedTree = ensure_tree_is_tree_fold(State, AccountId, Tree, []),
    UniqTree = kz_term:uniq_list(UpdatedTree),
    case UniqTree =:= Tree of
        'true' ->
            %% tree is okay
            State;
        'false' ->
            %% tree is fixed
            NewDepth = kz_term:to_binary(length(UniqTree)),
            State#{fixed_trees => maps:update_with(NewDepth
                                                  ,fun(DepthMap) -> DepthMap#{AccountId => UniqTree} end
                                                  ,#{AccountId => UniqTree}
                                                  ,FixedTrees
                                                  )
                  }
    end.

-spec ensure_tree_is_tree_fold(ensure_state(), kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binaries()) ->
          kz_term:ne_binaries().
ensure_tree_is_tree_fold(#{master := MasterAccountId}, MasterAccountId, _, _) ->
    [];

ensure_tree_is_tree_fold(#{master := MasterAccountId}, _, [], []) ->
    [MasterAccountId];

ensure_tree_is_tree_fold(_, _, [], UpdatedTree) ->
    lists:reverse(UpdatedTree);

ensure_tree_is_tree_fold(#{master := MasterAccountId}=State, AccountId, [MasterAccountId|Tree], []) ->
    ensure_tree_is_tree_fold(State, AccountId, Tree, [MasterAccountId]);

ensure_tree_is_tree_fold(#{master := MasterAccountId}=State, AccountId, [_BadMasterId|Tree], []) ->
    ensure_tree_is_tree_fold(State, AccountId, Tree, [MasterAccountId]);

ensure_tree_is_tree_fold(#{master := MasterAccountId
                          ,real_ids := RealIds
                          }=State, AccountId, [Id|Tree], UpdatedTree) ->
    case Id =/= MasterAccountId
        andalso gb_sets:is_member(Id, RealIds)
    of
        'true' ->
            ensure_tree_is_tree_fold(State, AccountId, Tree, [Id|UpdatedTree]);
        'false' ->
            ensure_tree_is_tree_fold(State, AccountId, Tree, UpdatedTree)
    end.

%%------------------------------------------------------------------------------
%% @doc A function F is a relation, a subset of X × Y, such that the domain of F
%% is equal to X and such that for every x in X there is a unique element y in Y
%% with (x, y) in F.
%%
%% Sometimes, when the range of a function is more important than the function
%% itself, the function is called a family.
%%
%%    -- from Sets of Sets: http://erlang.org/doc/man/sofs.html
%%
%% Family of an account is its own ID and the domain is the length of the
%% pvt_tree.
%% @end
%%------------------------------------------------------------------------------
-spec create_families(kz_json:objects(), ensure_state(), map()) -> {ensure_state(), map()}.
create_families([], State, Families) ->
    {State, Families};
create_families([JObj|JObjs], #{real_ids := RealIds}=State, Families) ->
    AccountId = kz_doc:id(JObj),
    AccountDepth = kz_json:get_value(<<"key">>, JObj),
    AccountTree = kz_json:get_value(<<"value">>, JObj),

    NuclearFamilies = add_to_family_domain(AccountDepth, AccountId, AccountTree, Families),
    create_families(JObjs
                   ,State#{real_ids => gb_sets:insert(AccountId, RealIds)}
                   ,NuclearFamilies
                   ).

-spec add_to_family_domain(non_neg_integer(), kz_term:ne_binary(), kz_term:ne_binaries(), map()) -> map().
add_to_family_domain(Depth, AccountId, AccountTree, Families) ->
    UpdateWithFun = fun(DomainMap) ->
                            DomainMap#{AccountId => AccountTree}
                    end,
    maps:update_with(Depth, UpdateWithFun, #{AccountId => AccountTree}, Families).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec import_account(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
import_account(Account, Parent) ->
    AccountId = kzs_util:format_account_id(Account),
    AccountDb = kzs_util:format_account_db(Account),
    ParentId = kzs_util:format_account_id(Parent),
    ParentDb = kzs_util:format_account_db(Parent),
    import_account(AccountId
                  ,ParentId
                  ,kz_datamgr:open_doc(AccountDb, AccountId)
                  ,kz_datamgr:open_doc(ParentDb, ParentId)
                  ).

-spec import_account(kz_term:ne_binary()
                    ,kz_term:ne_binary()
                    ,{'ok', kz_json:object()} | kazoo_data:data_error()
                    ,{'ok', kz_json:object()} | kazoo_data:data_error()
                    ) -> 'ok'.
import_account(_AccountId, _ParentId, {'error', _Reason1}, {'error', _Reason2}) ->
    io:format("can not open account '~s' (~p) and parent '~s' (~p)~n"
             ,[_AccountId, _Reason1, _ParentId, _Reason2]
             );
import_account(_AccountId, _, {'error', _Reason}, _) ->
    io:format("can not open account '~s' definition: ~p~n", [_AccountId, _Reason]);
import_account(_, _ParentId, _, {'error', _Reason}) ->
    io:format("can not open parent '~s' definition: ~p~n", [_ParentId, _Reason]);
import_account(AccountId, ParentId, {'ok', AccountJObj}, {'ok', ParentJObj}) ->
    io:format("importing account '~s' under parent '~s'~n", [AccountId, ParentId]),
    ParentTree = kzd_accounts:tree(ParentJObj),
    AccountTree = ParentTree ++ [ParentId],
    ResellerId = kz_services_reseller:find_id(lists:reverse(AccountTree)),

    Setters = [{fun kzd_accounts:set_tree/2, AccountTree}
              ,{fun kzd_accounts:set_reseller_id/2, ResellerId}
              ],
    NewAccountJObj = kz_doc:setters(AccountJObj, Setters),
    case kz_datamgr:save_doc(kzs_util:format_account_db(AccountId), NewAccountJObj) of
        {'ok', SavedJObj} ->
            io:format("account saved, updating services and import account's numbers to number dbs~n"),
            update_or_add_to_accounts_db(AccountId, SavedJObj),
            _ = remove_and_reconcile_services(AccountId),
            _ = kazoo_numbers_maintenance:copy_single_account_to_number_dbs(AccountId),
            'ok';
        {'error', _Reason} ->
            io:format("failed to update account '~s' definition in accountdb: ~p~n", [AccountId, _Reason])
    end.

-spec remove_and_reconcile_services(kz_term:ne_binary()) -> kz_services:services().
remove_and_reconcile_services(AccountId) ->
    io:format("ensuring services doc for '~s'~n", [AccountId]),
    _ = kz_datamgr:del_doc(?KZ_SERVICES_DB, AccountId),
    kz_services:reconcile(AccountId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_reseller_id_accounts() -> 'ok'.
ensure_reseller_id_accounts() ->
    Accounts = kapps_util:get_all_accounts(),
    ensure_reseller_id_accounts(Accounts, length(Accounts)).

-spec ensure_reseller_id_accounts(kz_term:ne_binaries()) -> 'ok'.
ensure_reseller_id_accounts(Accounts) ->
    ensure_reseller_id_accounts(Accounts, length(Accounts)).

-spec ensure_reseller_id_accounts(kz_term:ne_binaries(), non_neg_integer()) -> 'ok'.
ensure_reseller_id_accounts([], _) -> 'ok';
ensure_reseller_id_accounts([Account|Accounts], Total) ->
    io:format("(~p/~p) ensuring reseller id for account '~s'~n"
             ,[length(Accounts) + 1, Total, kzs_util:format_account_db(Account)]
             ),
    ensure_reseller_id_account(Account),
    ensure_reseller_id_accounts(Accounts, Total).

-spec ensure_reseller_id_account(kz_term:ne_binary()) -> 'ok'.
ensure_reseller_id_account(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    case kzd_accounts:fetch(AccountId, 'accounts') of
        {'ok', JObj} ->
            ResellerId = kz_services_reseller:find_id(lists:reverse(kzd_accounts:tree(JObj))),
            ensure_reseller_id_account(AccountId, ResellerId);
        {'error', _Reason} ->
            io:format("failed to open account from accounts db: ~p~n", [_Reason])
    end.

-spec ensure_reseller_id_account(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
ensure_reseller_id_account(Account, ResellerId) ->
    AccountId = kzs_util:format_account_id(Account),
    Updates =[{kzd_accounts:path_reseller_id(), kzs_util:format_account_id(ResellerId)}],
    maybe_log_doc_update(kzd_accounts:update(AccountId, Updates)
                        ,<<"updated account definitions">>
                        ,<<"failed to update account definitions">>
                        ),
    ensure_reseller_id_services(AccountId, ResellerId).

-spec ensure_reseller_id_services(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
ensure_reseller_id_services(AccountId, ResellerId) ->
    Services = kz_services:fetch(AccountId),
    ServicesJObj = kz_services:services_jobj(Services),
    case kzd_services:reseller_id(ServicesJObj) of
        ResellerId -> 'ok';
        _Invalid ->
            NewJObj = kzd_services:set_reseller_id(ServicesJObj, ResellerId),
            _ = kz_services:save_services_jobj(kz_services:set_services_jobj(Services, NewJObj)),
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_voicemail_media(kz_term:ne_binary()) ->
          {'ok', kz_json:objects()} |
          {'error', any()}.
cleanup_voicemail_media(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    Medias = get_medias(Account),
    Messages = get_messages(Account),
    ExtraMedia = Medias -- Messages,
    case kz_datamgr:del_docs(AccountDb, ExtraMedia) of
        {'ok', _}=Res -> Res;
        {'error', _E}=Err ->
            lager:error("could not delete docs ~p: ~p", [ExtraMedia, _E]),
            Err
    end.

-spec cleanup_orphan_modbs() -> 'ok'.
cleanup_orphan_modbs() ->
    AccountMODbs = kapps_util:get_all_account_mods('encoded'),
    AccountIds = kapps_util:get_all_accounts('raw'),
    DeleteOrphaned = fun (AccountMODb) ->
                             kazoo_modb:maybe_delete(AccountMODb, AccountIds)
                     end,
    lists:foreach(DeleteOrphaned, AccountMODbs).

-spec get_messages(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_messages(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    ViewOptions = ['include_docs'],
    case kz_datamgr:get_results(AccountDb, ?VMBOX_VIEW, ViewOptions) of
        {'ok', ViewRes} ->
            lists:foldl(fun extract_messages/2, [], ViewRes);
        {'error', _E} ->
            lager:error("could not load view ~p: ~p", [?VMBOX_VIEW, _E]),
            []
    end.

-spec extract_messages(kz_json:objects() | kz_json:object(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
extract_messages([], CurMessages) -> CurMessages;
extract_messages([Mess|Messages], CurMessages) ->
    extract_messages(Messages, [kz_json:get_value(<<"media_id">>, Mess)|CurMessages]);
extract_messages(JObj, CurMessages) ->
    Messages = kz_json:get_value([<<"doc">>, <<"messages">>], JObj, []),
    extract_messages(Messages, CurMessages).

-spec get_medias(kz_term:ne_binary()) -> kz_term:ne_binaries().
get_medias(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    ViewOptions = [],
    case kz_datamgr:get_results(AccountDb, ?PMEDIA_VIEW, ViewOptions) of
        {'ok', ViewRes} -> [kz_doc:id(JObj) || JObj<- ViewRes];
        {'error', _E} ->
            lager:error("could not load view ~p: ~p", [?PMEDIA_VIEW, _E]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec migrate_limits() -> 'ok'.
migrate_limits() ->
    migrate_all_limits(kapps_util:get_all_accounts()).

-spec migrate_all_limits(kz_term:ne_binaries()) -> 'ok'.
migrate_all_limits(Accounts) ->
    Total = length(Accounts),
    lists:foldr(fun(A, C) -> migrate_limits_fold(A, C, Total) end, 1, Accounts),
    'ok'.

migrate_limits_fold(AccountDb, Current, Total) ->
    io:format("checking limits doc in database (~p/~p) '~s'~n", [Current, Total, AccountDb]),
    _ = case kz_datamgr:open_doc(AccountDb, <<"limits">>) of
            {'error', 'not_found'} -> migrate_limits(AccountDb);
            _Else -> 'ok'
        end,
    Current + 1.

-spec migrate_limits(atom() | string() | binary()) -> 'ok'.
migrate_limits(Account) when not is_binary(Account) ->
    migrate_limits(kz_term:to_binary(Account));
migrate_limits(Account) ->
    TStamp = kz_time:now_s(),

    TwowayTrunks = kapps_config:get_integer(<<"jonny5">>, <<"default_twoway_trunks">>),
    InboundTrunks = kapps_config:get_integer(<<"jonny5">>, <<"default_inbound_trunks">>),

    AccountDb = case kz_datamgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> kzs_util:format_account_db(Account)
                end,
    {TT, IT} = clean_trunkstore_docs(AccountDb, TwowayTrunks, InboundTrunks),
    JObj = kz_json:from_list(
             [{<<"_id">>, <<"limits">>}
             ,{<<"twoway_trunks">>, TT}
             ,{<<"inbound_trunks">>, IT}
             ,{<<"pvt_account_db">>, AccountDb}
             ,{<<"pvt_account_id">>, kzs_util:format_account_id(Account)}
             ,{<<"pvt_type">>, <<"limits">>}
             ,{<<"pvt_created">>, TStamp}
             ,{<<"pvt_modified">>, TStamp}
             ,{<<"pvt_vsn">>, 1}
             ]),
    _ = kz_datamgr:save_doc(AccountDb, JObj),
    'ok'.

-spec clean_trunkstore_docs(kz_term:ne_binary(), integer(), integer()) ->
          {integer(), integer()}.
clean_trunkstore_docs(AccountDb, TwowayTrunks, InboundTrunks) ->
    ViewOptions = ['include_docs'
                  ,{'reduce', 'false'}
                  ],
    case kz_datamgr:get_results(AccountDb, <<"trunkstore/crossbar_listing">>, ViewOptions) of
        {'ok', JObjs} -> clean_trunkstore_docs(AccountDb, JObjs, TwowayTrunks, InboundTrunks);
        {'error', _}=E -> E
    end.

-spec clean_trunkstore_docs(kz_term:ne_binary(), kz_json:objects(), integer(), integer()) ->
          {integer(), integer()}.

clean_trunkstore_docs(_, [], Trunks, InboundTrunks) ->
    {Trunks, InboundTrunks};
clean_trunkstore_docs(AccountDb, [JObj|JObjs], Trunks, InboundTrunks) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    %% if there are no servers and it was created by jonny5 soft-delete the doc
    _ = case kz_json:get_ne_value(<<"servers">>, Doc) =:= 'undefined'
            andalso kz_json:get_ne_value(<<"pvt_created_by">>, Doc) =:= <<"jonny5">>
        of
            'true' -> kz_datamgr:save_doc(AccountDb, kz_doc:set_soft_deleted(Doc, 'true'));
            'false' -> 'ok'
        end,
    NewTrunks = case kz_json:get_integer_value([<<"account">>, <<"trunks">>], Doc, 0) of
                    OldTrunks when OldTrunks > Trunks -> OldTrunks;
                    _ -> Trunks
                end,

    NewInboundTrunks =
        case kz_json:get_integer_value([<<"account">>, <<"inbound_trunks">>], Doc, 0) of
            OldInboundTrunks when OldInboundTrunks > InboundTrunks -> OldInboundTrunks;
            _ -> Trunks
        end,
    clean_trunkstore_docs(AccountDb, JObjs, NewTrunks, NewInboundTrunks).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec migrate_media() -> 'ok'.
migrate_media() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(A, C) -> migrate_media_fold(A, C, Total) end, 1, Accounts),
    'ok'.

migrate_media_fold(AccountDb, Current, Total) ->
    io:format("migrating media in database (~p/~p) '~s'", [Current, Total, AccountDb]),
    _ = migrate_media(AccountDb),
    Current + 1.

-spec migrate_media(atom() | string() | binary()) -> 'ok'.
migrate_media(Account) when not is_binary(Account) ->
    migrate_media(kz_term:to_binary(Account));
migrate_media(Account) ->
    AccountDb = case kz_datamgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> kzs_util:format_account_db(Account)
                end,
    case kz_datamgr:get_results(AccountDb, <<"media/listing_by_name">>, []) of
        {'ok', []} -> io:format("no public media files in db ~s~n", [AccountDb]);
        {'ok', [_|_]=JObjs1}->
            _ = [migrate_attachment(AccountDb, JObj) || JObj <- JObjs1],
            'ok';
        {'error', _}=E1 ->
            io:format("unable to fetch media files in db ~s: ~p~n", [AccountDb, E1])
    end,
    case kz_datamgr:get_results(AccountDb, <<"media/listing_private_media">>, []) of
        {'ok', []} -> io:format("no private media files in db ~s~n", [AccountDb]);
        {'ok', JObjs2}->
            _ = [migrate_attachment(AccountDb, JObj) || JObj <- JObjs2],
            'ok';
        {'error', _}=E2 ->
            io:format("unable to fetch private media files in db ~s: ~p~n", [AccountDb, E2])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_migrate_attachments(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> any().
maybe_migrate_attachments(AccountDb, Id, JObj) ->
    case kz_doc:attachments(JObj) of
        'undefined' ->
            io:format("media doc ~s/~s has no attachments, removing~n", [AccountDb, Id]),
            kz_datamgr:save_doc(AccountDb, kz_doc:set_soft_deleted(JObj, 'true'));
        Attachments ->
            _ = [catch migrate_attachment(AccountDb, JObj, K, V)
                 || {K,V} <- kz_json:to_proplist(Attachments)
                ]
    end.

-spec migrate_attachment(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
migrate_attachment(AccountDb, ViewJObj) ->
    Id = kz_doc:id(ViewJObj),
    _ = case kz_datamgr:open_doc(AccountDb, Id) of
            {'error', _}=E1 ->
                io:format("unable to open media for attachment migration ~s/~s: ~p~n", [AccountDb, Id, E1]);
            {'ok', JObj1} ->
                maybe_migrate_attachments(AccountDb, Id, JObj1)
        end,

    %% we must reopen the doc since the _attachments has changed or we will effectively remove all attachments!
    case kz_datamgr:open_doc(AccountDb, Id) of
        {'error', _}=E2 ->
            io:format("unable to open media for depreciated field removal ~s/~s: ~p~n", [AccountDb, Id, E2]);
        {'ok', JObj2} ->
            remove_deprecated_attachment_properties(AccountDb, Id, JObj2)
    end.

remove_deprecated_attachment_properties(AccountDb, Id, JObj) ->
    J = kz_json:delete_keys([<<"status">>, <<"content_size">>, <<"size">>
                            ,<<"content_type">>, <<"content_length">>
                            ,<<"format">>, <<"sample">>, <<"media_type">>
                            ], JObj),
    Result = case (J =/= JObj)
                 andalso kz_json:get_value(<<"source_id">>, J)
             of
                 'false' -> 'ignore';
                 'undefined' ->
                     kz_datamgr:save_doc(AccountDb, kz_json:set_value(<<"media_source">>, <<"upload">>, J));
                 _Else ->
                     kz_datamgr:save_doc(AccountDb, kz_json:set_value(<<"media_source">>, <<"recording">>, J))
             end,
    case Result of
        'ignore' -> 'ok';
        {'ok', _} ->
            io:format("removed depreciated properties from ~s/~s~n", [AccountDb, Id]);
        {'error', _}=E3 ->
            io:format("removal of depreciated properties from ~s/~s failed: ~p~n", [AccountDb, Id, E3])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_attachment(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
migrate_attachment(AccountDb, JObj, Attachment, MetaData) ->
    DocCT = kz_json:get_value(<<"content_type">>, JObj),
    MetaCT = kz_json:get_value(<<"content_type">>, MetaData),
    Migrations = [fun({A, MCT}) -> maybe_update_attachment_content_type(A, MCT, DocCT) end
                 ,fun maybe_add_extension/1
                 ],
    Migrate = lists:foldl(fun(F, Acc) -> F(Acc) end
                         ,{Attachment, MetaCT}
                         ,Migrations
                         ),
    maybe_update_attachment(AccountDb, kz_doc:id(JObj), {Attachment, MetaCT}, Migrate).

maybe_update_attachment_content_type(A, MCT, DocCT) ->
    case {is_audio_content(DocCT), is_audio_content(MCT)} of
        {_, 'true'} -> {A, MCT};
        {'true', 'false'} -> {A, DocCT};
        {'false', 'false'} -> {A, find_attachment_content_type(A)}
    end.

-spec find_attachment_content_type(kz_term:ne_binary()) -> kz_term:ne_binary().
find_attachment_content_type(A) ->
    try cow_mimetypes:all(A) of
        {Type, SubType, _Options} -> kz_binary:join([Type, SubType], <<"/">>)
    catch
        'error':'function_clause' -> <<"audio/mpeg">>
    end.

-spec maybe_add_extension({kz_term:ne_binary(), kz_term:ne_binary()}) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
maybe_add_extension({A, CT}=T) ->
    case kz_term:is_empty(filename:extension(A)) of
        'false' -> T;
        'true' -> {add_extension(A, CT), CT}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type attachment_and_content() :: {kz_term:ne_binary(), kz_term:ne_binary()}.
-spec maybe_update_attachment(kz_term:ne_binary(), kz_term:ne_binary(), attachment_and_content(), attachment_and_content()) -> 'ok'.
maybe_update_attachment(_, _, {Attachment, CT}, {Attachment, CT}) -> 'ok';
maybe_update_attachment(AccountDb, Id, {OrigAttach, _CT1}, {NewAttach, CT}) ->
    %% this preforms the following:
    %% 1. Get the current attachment
    %% 2. Fix the name and content type then put the new attachment on the doc
    %% 3. Save the old attachment content (I am paranoid) to disk
    %% 4. Remove the original (erroneous) attachment
    %% However, if it fails at any of those stages it will leave the media doc with multiple
    %%    attachments and require manual intervention
    Updaters = [fun(_) -> try_load_attachment(AccountDb, Id, OrigAttach) end
               ,fun(Content1) ->
                        maybe_resave_attachment(Content1, AccountDb, Id, OrigAttach, NewAttach, CT)
                end
               ,fun(_) ->
                        maybe_cleanup_old_attachment(AccountDb, Id, OrigAttach, NewAttach)
                end
               ],
    lists:foldl(fun(F, Acc) -> F(Acc) end, [], Updaters).

-spec try_load_attachment(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          binary().
try_load_attachment(AccountDb, Id, OrigAttach) ->
    case kz_datamgr:fetch_attachment(AccountDb, Id, OrigAttach) of
        {'ok', Content} -> Content;
        {'error', _R}=E ->
            io:format("unable to fetch attachment ~s/~s/~s: ~p~n", [AccountDb, Id, OrigAttach, _R]),
            throw(E)
    end.

-spec maybe_resave_attachment(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'ok'.
maybe_resave_attachment(Content1, AccountDb, Id, OrigAttach, NewAttach, CT) ->
    {'ok', Rev} = kz_datamgr:lookup_doc_rev(AccountDb, Id),
    Options = [{'content_type', CT}
              ,{'rev', Rev}
              ],
    %% BigCouch is awesome in that it sometimes returns 409 (conflict) but does the work anyway..
    %%   so rather than check the put return fetch the new attachment and compare it to the old
    Result = kz_datamgr:put_attachment(AccountDb, Id, NewAttach, Content1, Options),
    {'ok', JObj} = kz_datamgr:open_doc(AccountDb, Id),

    case kz_doc:attachment_length(JObj, OrigAttach)
        =:= kz_doc:attachment_length(JObj, NewAttach)
    of
        'false' ->
            io:format("unable to put new attachment ~s/~s/~s: ~p~n", [AccountDb, Id, NewAttach, Result]),
            throw({'error', 'length_mismatch'});
        'true' ->
            Filename = kz_term:to_list(<<"/tmp/media_", Id/binary, "_", OrigAttach/binary>>),
            case file:write_file(Filename, Content1) of
                'ok' -> 'ok';
                {'error', _R}=E2 ->
                    io:format("unable to backup attachment ~s/~s/~s: ~p~n", [AccountDb, Id, NewAttach, _R]),
                    throw(E2)
            end
    end.

-spec maybe_cleanup_old_attachment(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_cleanup_old_attachment(_AccountDb, _Id, NewAttach, NewAttach) ->
    io:format("updated content type for ~s/~s/~s~n", [_AccountDb, _Id, NewAttach]);
maybe_cleanup_old_attachment(AccountDb, Id, OrigAttach, NewAttach) ->
    case kz_datamgr:delete_attachment(AccountDb, Id, OrigAttach) of
        {'ok', _} ->
            io:format("updated attachment name ~s/~s/~s~n", [AccountDb, Id, NewAttach]),
            'ok';
        {'error', _R}=E ->
            io:format("unable to remove original attachment ~s/~s/~s: ~p~n", [AccountDb, Id, OrigAttach, _R]),
            throw(E)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_extension(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
add_extension(A, <<"audio/x-wav">>) ->
    <<A/binary, ".wav">>;
add_extension(A, <<"audio/wav">>) ->
    <<A/binary, ".wav">>;
add_extension(A, <<"audio/mpeg3">>) ->
    <<A/binary, ".mp3">>;
add_extension(A, <<"audio/x-mpeg-3">>) ->
    <<A/binary, ".mp3">>;
add_extension(A, <<"audio/mpeg">>) ->
    <<A/binary, ".mp3">>;
add_extension(A, <<"audio/x-mpeg">>) ->
    <<A/binary, ".mp3">>;
add_extension(A, <<"audio/mp3">>) ->
    <<A/binary, ".mp3">>;
add_extension(A, _) -> A.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_audio_content(kz_term:ne_binary()) -> boolean().
is_audio_content(<<"audio/", _/binary>>) -> 'true';
is_audio_content(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_delete_db(kz_term:ne_binary()) -> 'ok'.
maybe_delete_db(Database) ->
    case kapps_config:get_is_true(?SYSCONFIG_COUCH, <<"allow_maintenance_db_delete">>, 'false') of
        'true' ->
            lager:warning("deleting database ~s", [Database]),
            kz_datamgr:db_delete(Database);
        'false' ->
            lager:warning("database deletion requested but disabled for ~s", [Database])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec purge_doc_type(kz_term:ne_binaries() | kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:objects()} |
          {'error', _} |
          'ok'.
purge_doc_type(Type, Account)
  when is_binary(Type);
       is_binary(Account) ->
    purge_doc_type(Type
                  ,Account
                  ,kapps_config:get_integer(?SYSCONFIG_COUCH, <<"default_chunk_size">>, ?MILLISECONDS_IN_SECOND)
                  );
purge_doc_type([], _Account) -> 'ok';
purge_doc_type([Type|Types], Account) ->
    _ = purge_doc_type(Type, Account),
    purge_doc_type(Types
                  ,Account
                  ,kapps_config:get_integer(?SYSCONFIG_COUCH, <<"default_chunk_size">>, ?MILLISECONDS_IN_SECOND)
                  );
purge_doc_type(Type, Account) when not is_binary(Type) ->
    purge_doc_type(kz_term:to_binary(Type)
                  ,Account
                  ,kapps_config:get_integer(?SYSCONFIG_COUCH, <<"default_chunk_size">>, ?MILLISECONDS_IN_SECOND)
                  );
purge_doc_type(Type, Account) when not is_binary(Account) ->
    purge_doc_type(Type
                  ,kz_term:to_binary(Account)
                  ,kapps_config:get_integer(?SYSCONFIG_COUCH, <<"default_chunk_size">>, ?MILLISECONDS_IN_SECOND)
                  ).

-spec purge_doc_type(kz_term:ne_binaries() | kz_term:ne_binary(), kz_term:ne_binary(), integer()) ->
          {'ok', kz_json:objects()} |
          {'error', _} |
          'ok'.
purge_doc_type(Type, Account, ChunkSize) ->
    Db = kzs_util:format_account_db(Account),
    Opts = [{'key', Type}
           ,{'limit', ChunkSize}
           ,'include_docs'
           ],
    case kz_datamgr:get_results(Db, <<"maintenance/listing_by_type">>, Opts) of
        {'error', _}=E -> E;
        {'ok', []} -> 'ok';
        {'ok', Ds} ->
            lager:debug("deleting up to ~p documents of type ~p", [ChunkSize, Type]),
            _ = kz_datamgr:del_docs(Db, [kz_json:get_value(<<"doc">>, D) || D <- Ds]),
            purge_doc_type(Type, Account, ChunkSize)
    end.

-spec call_id_status(kz_term:ne_binary()) -> 'ok'.
call_id_status(CallId) ->
    call_id_status(CallId, 'false').

-spec call_id_status(kz_term:ne_binary(), boolean() | kz_term:ne_binary()) -> 'ok'.
call_id_status(CallId, Verbose) ->
    Req = [{<<"Call-ID">>, kz_term:to_binary(CallId)}
           | kz_api:default_headers(<<"shell">>, <<"0">>)
          ],
    case kz_amqp_worker:call(Req
                            ,fun kapi_call:publish_channel_status_req/1
                            ,fun kapi_call:channel_status_resp_v/1
                            )
    of
        {'ok', Resp} ->
            show_status(CallId, kz_term:is_true(Verbose), Resp);
        {'error', _E} ->
            lager:info("failed to get status of '~s': '~p'", [CallId, _E])
    end.

-spec show_status(kz_term:ne_binary(), boolean(), kz_term:api_terms()) -> 'ok'.
show_status(CallId, 'false', Resp) ->
    lager:info("channel '~s' has status '~s'", [CallId, kapi_call:get_status(Resp)]);
show_status(CallId, 'true', Resp) ->
    lager:info("channel ~s", [CallId]),
    lager:info("status: ~s", [kz_json:get_value(<<"Status">>, Resp)]),
    lager:info("media Server: ~s", [kz_json:get_value(<<"Switch-Hostname">>, Resp)]),
    lager:info("responding App: ~s", [kz_json:get_value(<<"App-Name">>, Resp)]),
    lager:info("responding Node: ~s", [kz_json:get_value(<<"Node">>, Resp)]).

-spec last_migrate_version() -> kz_term:ne_binary().
last_migrate_version() ->
    kapps_config:get_ne_binary(<<?MODULE_STRING>>, <<"migrate_current_version">>, <<"3.22">>).

-spec set_last_migrate_version(kz_term:ne_binary()) -> {'ok', kz_json:object()}.
set_last_migrate_version(Version) ->
    kapps_config:set(<<?MODULE_STRING>>, <<"migrate_current_version">>, Version).

-spec migrate_system() -> 'ok'.
migrate_system() ->
    migrate_system(last_migrate_version(), kz_util:kazoo_version()).

-spec migrate_system(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
migrate_system(ThisVersion, ThisVersion) ->
    lager:notice("system config version is ~s", [ThisVersion]);
migrate_system(PreviousVersion, ThisVersion) ->
    Routines = migrate_system_version_routines(PreviousVersion, ThisVersion),
    Result = lists:foldl(
               fun(Fun, Acc) when is_function(Fun, 2) ->
                       [Fun(PreviousVersion, ThisVersion) | Acc];
                  (Fun, Acc) when is_function(Fun, 1) ->
                       [Fun(ThisVersion) | Acc];
                  (Fun, Acc) when is_function(Fun) ->
                       [Fun() | Acc]
               end, [], Routines),
    case lists:all(fun kz_term:is_true/1, Result) of
        'true' -> _ = set_last_migrate_version(ThisVersion),
                  'ok';
        'false' -> 'ok'
    end.

-spec migrate_system_version_routines(kz_term:ne_binary(), kz_term:ne_binary()) -> [fun()].
migrate_system_version_routines(<<"3.22">>, _) ->
    [fun migrate_and_cleanup_system_config/0
    ,fun handle_module_rename/0
    ];
migrate_system_version_routines(_, _) -> [].

-spec handle_module_rename() -> boolean().
handle_module_rename() ->
    {'ok', JObjs} = kz_datamgr:all_docs(?KZ_CONFIG_DB, ['include_docs']),
    Results = [handle_module_rename_doc(kz_json:get_value(<<"doc">>, JObj)) || JObj <- JObjs],
    lists:all(fun kz_term:is_true/1, Results).

-spec handle_module_rename_doc(kz_json:object()) -> boolean().
handle_module_rename_doc(JObj) ->
    Bin = kz_json:encode(JObj),
    WHBin = binary:replace(Bin, <<"wh_">>, <<"kz_">>, ['global']),
    WNMBin = binary:replace(WHBin, <<"wnm_">>, <<"knm_">>, ['global']),
    case WNMBin of
        Bin -> 'true';
        Replaced ->
            lager:notice("found wh_ / wnm_ pattern in ~s, replacing.", [kz_doc:id(JObj)]),
            case kz_datamgr:ensure_saved(?KZ_CONFIG_DB, kz_json:decode(Replaced)) of
                {'ok', _NewDoc} -> 'true';
                {'error', _Error} -> 'false'
            end
    end.

-spec migrate_and_cleanup_system_config() -> 'true'.
migrate_and_cleanup_system_config() ->
    cleanup_invalid_notify_docs(),
    delete_system_media_references(),
    accounts_config_deprecate_timezone_for_default_timezone(),
    'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_invalid_notify_docs() -> 'ok'.
cleanup_invalid_notify_docs() ->
    _ = kz_datamgr:db_archive(<<"system_config">>),
    case kz_datamgr:all_docs(?KZ_CONFIG_DB, ['include_docs']) of
        {'ok', JObjs} -> cleanup_invalid_notify_docs(JObjs);
        {'error', _R} ->
            lager:warning("unable to fetch all system config docs: ~p", [_R])
    end.

-spec cleanup_invalid_notify_docs(kz_json:objects()) -> 'ok'.
cleanup_invalid_notify_docs([]) -> 'ok';
cleanup_invalid_notify_docs([JObj|JObjs]) ->
    Id = kz_json:get_value(<<"id">>, JObj),
    Doc = kz_json:get_value(<<"doc">>, JObj),
    _ = maybe_remove_invalid_notify_doc(kz_doc:type(Doc), Id, Doc),
    cleanup_invalid_notify_docs(JObjs).

-spec maybe_remove_invalid_notify_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_remove_invalid_notify_doc(<<"notification">>, <<"notification", _/binary>>, _) -> 'ok';
maybe_remove_invalid_notify_doc(<<"notification">>, _, JObj) ->
    _ = kz_datamgr:del_doc(?KZ_CONFIG_DB, JObj),
    'ok';
maybe_remove_invalid_notify_doc(_Type, _Id, _Doc) -> 'ok'.


-spec delete_system_media_references() -> 'ok'.
delete_system_media_references() ->
    DocId = <<"call_response">>,
    case kz_datamgr:open_doc(?KZ_CONFIG_DB, DocId) of
        {'ok', CallResponsesDoc} ->
            delete_system_media_references(DocId, CallResponsesDoc);
        {'error', 'not_found'} -> 'ok'
    end.

-spec delete_system_media_references(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
delete_system_media_references(DocId, CallResponsesDoc) ->
    TheKey = <<"default">>,
    Default = kz_json:get_value(TheKey, CallResponsesDoc),

    case kz_json:map(fun remove_system_media_refs/2, Default) of
        Default -> 'ok';
        NewDefault ->
            io:format("updating ~s with stripped system_media references~n", [DocId]),
            NewCallResponsesDoc = kz_json:set_value(TheKey, NewDefault, CallResponsesDoc),
            _Resp = kz_datamgr:save_doc(?KZ_CONFIG_DB, NewCallResponsesDoc),
            'ok'
    end.

-spec remove_system_media_refs(kz_json:path(), kz_json:objects()) ->
          {kz_json:path(), kz_json:json_term()}.
remove_system_media_refs(HangupCause, Config) ->
    case kz_json:is_json_object(Config) of
        'false' -> {HangupCause, Config};
        'true' ->
            {HangupCause
            ,kz_json:foldl(fun remove_system_media_ref/3, kz_json:new(), Config)
            }
    end.

-spec remove_system_media_ref(kz_json:path(), kz_json:json_term(), kz_json:object()) ->
          kz_json:object().
remove_system_media_ref(Key, <<"/system_media/", Value/binary>>, Acc) -> kz_json:set_value(Key, Value, Acc);
remove_system_media_ref(Key, Value, Acc) -> kz_json:set_value(Key, Value, Acc).

%%------------------------------------------------------------------------------
%% @doc Remove system_config/accounts timezone key and use only
%% default_timezone
%% @end
%%------------------------------------------------------------------------------

-spec accounts_config_deprecate_timezone_for_default_timezone() -> 'ok'.
accounts_config_deprecate_timezone_for_default_timezone() ->
    case kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, <<"accounts">>) of
        {'ok', AccountsConfig} ->
            accounts_config_deprecate_timezone_for_default_timezone(AccountsConfig);
        {'error', E} ->
            lager:warning("unable to fetch system_config/accounts: ~p", [E])
    end.

-spec accounts_config_deprecate_timezone_for_default_timezone(kz_json:object()) -> 'ok'.
accounts_config_deprecate_timezone_for_default_timezone(AccountsConfig) ->
    PublicFields = kz_doc:public_fields(AccountsConfig),
    case kz_json:get_keys(PublicFields) of
        [] -> 'ok';
        Keys ->
            MigratedConfig = deprecate_timezone_for_default_timezone(Keys, AccountsConfig),
            {'ok', _} = kz_datamgr:save_doc(?KZ_CONFIG_DB, MigratedConfig),
            'ok'
    end.

-spec deprecate_timezone_for_default_timezone(kz_json:keys(), kz_json:object()) ->
          kz_json:object().
deprecate_timezone_for_default_timezone(Nodes, AccountsConfig) ->
    lists:foldl(fun deprecate_timezone_for_node/2, AccountsConfig, Nodes).

-spec deprecate_timezone_for_node(kz_json:key(), kz_json:object()) ->
          kz_json:object().
deprecate_timezone_for_node(Node, AccountsConfig) ->
    Timezone = kz_json:get_value([Node, <<"timezone">>], AccountsConfig),
    DefaultTimezone = kz_json:get_value([Node, <<"default_timezone">>], AccountsConfig),
    deprecate_timezone_for_node(Node, AccountsConfig, Timezone, DefaultTimezone).

-spec deprecate_timezone_for_node(kz_json:key(), kz_json:object(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) ->
          kz_json:object().
deprecate_timezone_for_node(_Node, AccountsConfig, 'undefined', _Default) ->
    AccountsConfig;
deprecate_timezone_for_node(Node, AccountsConfig, Timezone, 'undefined') ->
    io:format("setting default timezone to ~s for node ~s~n", [Timezone, Node]),
    kz_json:set_value([Node, <<"default_timezone">>]
                     ,Timezone
                     ,kz_json:delete_key([Node, <<"timezone">>], AccountsConfig)
                     );
deprecate_timezone_for_node(Node, AccountsConfig, _Timezone, _Default) ->
    kz_json:delete_key([Node, <<"timezone">>], AccountsConfig).

-spec check_system_configs() -> 'ok'.
check_system_configs() ->
    io:format("starting system schemas validation~n"),
    _ = [log_system_config_errors(Config, Errors)
         || {Config, Errors} <- validate_system_configs()
        ],
    io:format("finished system schemas validation~n"),
    'ok'.

-spec check_system_config(kz_term:ne_binary()) -> 'ok'.
check_system_config(SystemConfig) ->
    case validate_system_config(SystemConfig) of
        [] -> io:format("system config schema ~s is sane~n", [SystemConfig]);
        Errors -> log_system_config_errors(SystemConfig, Errors)
    end.

-spec log_system_config_errors(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
log_system_config_errors(Config, ErrorsObj) ->
    io:format("System config ~s validation errors~n", [Config]),
    Fun = fun({Path, ErrorJObj}) ->
                  Keys = [binary:replace(Part, <<"%2E">>, <<".">>, ['global'])
                          || Part <- binary:split(Path, <<".">>, ['global'])
                         ],
                  Fun2 = fun({_Type, TypeObj}) ->
                                 Message = kz_json:get_ne_binary_value(<<"message">>, TypeObj),
                                 Value = kz_json:get_value(<<"value">>, TypeObj),
                                 io:format(">>>>> ~p => '~p' : ~s~n", [Keys, Value, Message])
                         end,
                  kz_json:foreach(Fun2, ErrorJObj)
          end,
    lists:foreach(fun(J) -> kz_json:foreach(Fun, J) end, ErrorsObj),
    'ok'.

-spec validate_system_configs() -> [{kz_term:ne_binary(), kz_json:objects()}].
validate_system_configs() ->
    [{ConfigId, Status}
     || ConfigId <- kapps_config_doc:list_configs(),
        Status <- [validate_system_config(ConfigId)],
        [] =/= Status
    ].

-spec validate_system_config(kz_term:ne_binary()) -> kz_json:objects().
validate_system_config(Id) ->
    Doc = get_config_document(Id),
    Schema = kapps_config_util:system_config_document_schema(Id),
    case kz_json_schema:validate(Schema, Doc) of
        {'ok', _} -> [];
        {'error', Errors} ->
            [JObj || {_, _, JObj} <- kz_json_schema:errors_to_jobj(Errors)]
    end.

-spec get_config_document(kz_term:ne_binary()) -> kz_json:object().
get_config_document(Id) ->
    kz_doc:public_fields(maybe_new(kapps_config:fetch_category(Id, 'false'))).

-spec maybe_new({'ok', kz_json:object()} | {'error', any()}) -> kz_json:object().
maybe_new({'ok', Doc}) -> Doc;
maybe_new(_) -> kz_json:new().

-spec cleanup_system_config(kz_term:ne_binary()) -> {'ok', kz_json:object()}.
cleanup_system_config(Id) ->
    Doc = maybe_new(kapps_config:fetch_category(Id)),
    ErrorKeys = error_keys(validate_system_config(Id)),
    NewDoc = lists:foldl(fun kz_json:delete_key/2, Doc, ErrorKeys),
    kz_datamgr:save_doc(?KZ_CONFIG_DB, NewDoc).

-spec error_keys(kz_json:objects()) -> kz_json:paths().
error_keys(Errors) ->
    [binary:split(ErrorKey, <<".">>)
     || ErrorJObj <- Errors,
        ErrorKey <- kz_json:get_keys(ErrorJObj)
    ].

-spec cleanup_system_configs() -> [{'ok', kz_json:object() | kz_json:objects()}].
cleanup_system_configs() ->
    [cleanup_system_config(Id) || {Id, _Err} <- validate_system_configs()].

-spec flush_getby_cache() -> 'ok'.
flush_getby_cache() ->
    kz_cache:flush_local(?KAPPS_GETBY_CACHE),
    'ok'.

-spec flush_account_views() -> 'ok'.
flush_account_views() ->
    put('account_views', 'undefined').

%%------------------------------------------------------------------------------
%% @doc Returns all views related to account database by first trying fetching
%% from process dictionary otherwise reads from file system.
%%
%% @deprecated Views refresh functionality is now handled from database. View
%% definitions in database are updated during system startup and refreshing views
%% is now reading view definition from database not from file system.
%% @see read_all_account_views/0
%% @see flush_account_views/0
%% @end
%%------------------------------------------------------------------------------
-spec get_all_account_views() -> kz_datamgr:views_listing().
get_all_account_views() ->
    case get('account_views') of
        'undefined' ->
            Views = read_all_account_views(),
            put('account_views', Views),
            Views;
        Views -> Views
    end.

%%------------------------------------------------------------------------------
%% @doc Returns all account database related views from file system (only those
%% files within {@link kazoo_apps} private couch views directory.
%% @see read_all_account_views/0
%% @end
%%------------------------------------------------------------------------------
-spec read_all_account_views() -> kz_datamgr:views_listing().
read_all_account_views() ->
    kapps_util:get_views_json(?APP, "account").

%%------------------------------------------------------------------------------
%% @doc Update account's MODB and account's specific view definitions in database.
%%
%% This function reads all account and account's MODB related views updates (or
%% creates) them in `system_data' database.
%% Refreshing views will read those views from this this database.
%%
%% During system startup this function should always be called (by
%% {@link init_system/0}) to make sure account's view definitions are updated to
%% latest version.
%% @end
%%------------------------------------------------------------------------------
-spec register_account_views() -> 'ok'.
register_account_views() ->
    kazoo_modb_maintenance:register_views(),
    Views = read_all_account_views(),
    kz_datamgr:register_views(?APP, Views).

%%------------------------------------------------------------------------------
%% @doc Update view definitions for core system databases.
%%
%% During system startup this function should always be called (by
%% {@link init_system/0}) to make sure account's view definitions are updated.
%% @end
%%------------------------------------------------------------------------------
-spec register_system_dbs_views() -> 'ok'.
register_system_dbs_views() ->
    Views = [kapps_util:get_view_json(?APP, ?ACCOUNTS_AGG_VIEW_FILE)
            ,kapps_util:get_view_json(?APP, ?MAINTENANCE_VIEW_FILE)
            ,kapps_util:get_view_json(?APP, ?SEARCH_VIEW_FILE)
            ,kapps_util:get_view_json(?APP, <<"views/alerts.json">>)
            ,kapps_util:get_view_json(?APP, <<"views/system_configs.json">>)
            ,kapps_util:get_view_json(?APP, <<"views/pending_notify.json">>)
            ,kapps_util:get_view_json(?APP, <<"views/rates.json">>)
            ,kapps_util:get_view_json(?APP, <<"views/token_auth.json">>)
             | kapps_util:get_views_json('kazoo_ips', "views")
            ],
    kz_datamgr:register_views(?APP, Views).

-spec refresh_system_views() -> 'ok'.
refresh_system_views() ->
    Databases = [?KZ_ACCOUNTS_DB
                ,?KZ_ALERTS_DB
                ,?KZ_CONFIG_DB
                ,?KZ_DEDICATED_IP_DB
                ,?KZ_PENDING_NOTIFY_DB
                ,?KZ_RATES_DB
                ,?KZ_TOKEN_DB
                ],
    refresh_system_views(Databases).

-spec refresh_system_views(kz_term:ne_binaries()) -> 'ok'.
refresh_system_views([]) -> 'ok';
refresh_system_views([Db | Dbs]) ->
    _ = refresh(Db),
    refresh_system_views(Dbs).

%%------------------------------------------------------------------------------
%% @doc Initialize and update core database views and register account's views.
%%
%% This function is always called by {@link kapps_controller} during system
%% startup to update core views and register account views.
%%
%% @see kapps_controller:start_link/0. `kapps_controller' for system startup
%% @end
%%------------------------------------------------------------------------------
-spec init_system() -> 'ok'.
init_system() ->
    register_account_views(),
    register_system_dbs_views(),
    refresh_system_views(),
    lager:notice("system initialized").

-spec check_release() -> 'ok' | 'error'.
check_release() ->
    kz_log:put_callid('check_release'),
    Checks = [fun kapps_started/0
             ,fun check_system_configs/0
             ,fun master_account_created/0
             ,fun migration_4_0_ran/0
             ,fun migration_ran/0
             ,fun kazoo_proper_maintenance:run_seq_modules/0
             ],
    try lists:foreach(fun(F) -> run_check(F) end, Checks) of
        'ok' ->
            lager:info("check_release/0 succeeded"),
            init:stop()
    catch
        'throw':Error ->
            lager:error("check_release/0 failed: ~p", [Error]),
            init:stop(1);
        ?STACKTRACE(_E, _R, ST)
        lager:error("check_release/0 crashed: ~s: ~p", [_E, _R]),
        kz_log:log_stacktrace(ST),
        init:stop(1)
        end.

-spec run_check(fun()) -> 'ok'.
run_check(CheckFun) ->
    {Pid, Ref} = kz_process:spawn_monitor(CheckFun, []),
    wait_for_check(Pid, Ref).

-spec wait_for_check(pid(), reference()) -> 'ok'.
wait_for_check(Pid, Ref) ->
    receive
        {'DOWN', Ref, 'process', Pid, 'normal'} -> 'ok';
        {'DOWN', Ref, 'process', Pid, Reason} ->
            lager:error("check in ~p failed to run: ~p", [Pid, Reason]),
            throw(Reason)
    after 5 * ?MILLISECONDS_IN_MINUTE ->
            lager:error("check in ~p timed out", [Pid]),
            exit(Pid, 'timeout'),
            throw('timeout')
    end.

-spec kapps_started() -> boolean().
kapps_started() ->
    lager:info("checking that kapps have started"),
    kapps_started(180 * ?MILLISECONDS_IN_SECOND).

-spec kapps_started(integer()) -> 'true'.
kapps_started(Timeout) when Timeout > 0 ->
    kapps_started(Timeout, kapps_controller:ready());
kapps_started(_Timeout) ->
    lager:error("timed out waiting for kapps to start"),
    throw({'error', 'timeout'}).

-spec kapps_started(integer(), boolean()) -> 'true'.
kapps_started(_Timeout, 'true') -> 'true';
kapps_started(Timeout, 'false') ->
    timer:sleep(500),
    kapps_started(Timeout - 500).

-spec master_account_created() -> 'true'.
master_account_created() ->
    master_account_created(kapps_util:get_master_account_id()).

master_account_created({'ok', MasterAccountId}) ->
    validate_master_account(MasterAccountId);
master_account_created({'error', _}) ->
    lager:info("trying to create the master account"),
    case rpc:call(node()
                 ,'crossbar_maintenance'
                 ,'create_account'
                 ,[<<"compte_maitre">>
                  ,<<"royaume">>
                  ,<<"superduperuser">>
                  ,<<"pwd!">>
                  ]
                 )
    of
        'ok' ->
            {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
            lager:info("created master account ~s", [MasterAccountId]),
            validate_master_account(MasterAccountId);
        'failed' -> throw({'error', 'create_account'})
    end.

validate_master_account(MasterAccountId) ->
    {'ok', MasterAccountDoc} = kzd_accounts:fetch(MasterAccountId),
    lager:debug("account: ~s", [kz_json:encode(MasterAccountDoc)]),
    'true' = kzd_accounts:is_superduper_admin(MasterAccountDoc).

-spec migration_4_0_ran() -> boolean().
migration_4_0_ran() ->
    lager:info("migrating to 4.x"),
    'no_return' =:= migrate_to_4_0().

-spec migration_ran() -> boolean().
migration_ran() ->
    lager:info("migrating"),
    'no_return' =:= migrate().
