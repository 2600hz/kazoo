%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_maintenance).

-export([rebuild_token_auth/0
        ,rebuild_token_auth/1
        ]).
-export([migrate/0
        ,migrate/1
        ,migrate_to_4_0/0
        ]).
-export([parallel_migrate/1
        ,parallel_migrate/2
        ]).
-export([find_invalid_acccount_dbs/0]).
-export([refresh/0, refresh/1
        ,refresh_account_db/1
        ,maybe_delete_db/1
        ]).
-export([blocking_refresh/0
        ,blocking_refresh/1
        ]).
-export([remove_depreciated_databases/0]).
-export([ensure_aggregate_devices/0
        ,ensure_aggregate_device/1
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
-export([validate_system_config/1, cleanup_system_config/1, validate_system_configs/0, cleanup_system_configs/0]).

-export([bind/3, unbind/3
        ,binding/1
        ]).

-export([flush_getby_cache/0
        ,flush_account_views/0
        ,get_all_account_views/0
        ,read_all_account_views/0
        ]).

-export([init_system/0, init_dbs/0, register_account_views/0]).

-export([check_release/0]).

-include_lib("kazoo_caches/include/kazoo_caches.hrl").
-include("kazoo_apps.hrl").

-type bind() :: 'migrate' | 'refresh' | 'refresh_account'.
-spec binding(bind() | {bind(), kz_term:ne_binary()}) -> kz_term:ne_binary().
binding('migrate') -> <<"maintenance.migrate">>;
binding('refresh') -> <<"maintenance.refresh">>;
binding('refresh_account') -> <<"maintenance.refresh.account">>;
binding({Common, Specific}) when is_atom(Common), is_binary(Specific) ->
    CommonPath = binding(Common),
    <<CommonPath/binary, ".", Specific/binary>>.

-spec bind(atom() | {atom(), binary()}, module(), atom()) -> any().
bind(Event, M, F) -> kazoo_bindings:bind(binding(Event), M, F).

-spec unbind(atom() | {atom(), binary()}, module(), atom()) -> any().
unbind(Event, M, F) -> kazoo_bindings:unbind(binding(Event), M, F).

-define(DEVICES_CB_LIST, <<"devices/crossbar_listing">>).
-define(RESELLER_VIEW_FILE, <<"views/reseller.json">>).

-define(ACCOUNTS_AGG_NOTIFY_VIEW_FILE, <<"views/notify.json">>).

-define(VMBOX_VIEW, <<"vmboxes/crossbar_listing">>).
-define(PMEDIA_VIEW, <<"media/listing_private_media">>).

-spec refresh_account_db(kz_term:ne_binary()) -> 'ok'.
refresh_account_db(Database) ->
    kz_datamgr:refresh_views(Database),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rebuild_token_auth() -> 'ok'.
rebuild_token_auth() ->
    rebuild_token_auth(5 * ?MILLISECONDS_IN_SECOND).

-spec rebuild_token_auth(kz_term:text() | integer()) -> 'ok'.
rebuild_token_auth(Pause) ->
    _ = kz_datamgr:db_delete(?KZ_TOKEN_DB),
    timer:sleep(kz_term:to_integer(Pause)),
    refresh(?KZ_TOKEN_DB),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_to_4_0() -> no_return.
migrate_to_4_0() ->
    %% Number migration
    kazoo_number_manager_maintenance:migrate(),
    %% Voicemail migration
    kazoo_voicemail_maintenance:migrate(),
    no_return.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate() -> 'no_return'.
migrate() ->
    migrate(2 * ?MILLISECONDS_IN_SECOND).

-spec migrate(kz_term:text() | integer()) -> 'no_return'.
migrate(Pause) ->
    _ = migrate_system(),
    _ = kapps_config:migrate(),

    Databases = get_databases(),
    _ = migrate(Pause, Databases),

    %% Migrate settings for kazoo_media
    io:format("running media migrations...~n"),
    _ = kazoo_media_maintenance:migrate(),

    'no_return'.

-spec migrate(kz_term:text() | integer(), kz_term:ne_binaries()) -> 'no_return'.
migrate(Pause, Databases) ->
    Accounts = [kz_util:format_account_id(Db, 'encoded')
                || Db <- Databases,
                   kapps_util:is_account_db(Db)
               ],
    io:format("updating dbs...~n"),
    _ = refresh(Databases, Pause),

    %% Remove depreciated dbs
    io:format("removing depreciated databases...~n"),
    _  = remove_depreciated_databases(Databases),

    kazoo_bindings:map(binding('migrate'), [Accounts]),

    'no_return'.

-spec parallel_migrate(kz_term:text() | integer()) -> 'no_return'.
parallel_migrate(Workers) ->
    parallel_migrate(Workers, 2 * ?MILLISECONDS_IN_SECOND).

-spec parallel_migrate(kz_term:text() | integer(), kz_term:text() | integer()) -> 'no_return'.
parallel_migrate(Workers, Pause) ->
    _ = migrate_system(),
    _ = kapps_config:migrate(),
    {Accounts, Others} = lists:partition(fun kapps_util:is_account_db/1, get_databases()),
    AccountDbs = [kz_util:format_account_db(Db) || Db <- Accounts],
    OtherSplit = kz_term:to_integer(length(Others) / kz_term:to_integer(Workers)),
    AccountSplit = kz_term:to_integer(length(AccountDbs) / kz_term:to_integer(Workers)),
    SplitDbs = split(AccountSplit, AccountDbs, OtherSplit, Others, []),
    parallel_migrate(Pause, SplitDbs, []).

-type split_results() :: [{kz_term:ne_binaries(), kz_term:ne_binaries()}].
-spec split(integer(), kz_term:ne_binaries(), integer(), kz_term:ne_binaries(), split_results()) -> split_results().
split(_, [], _, [], Results) -> Results;
split(AccountSplit, Accounts, OtherSplit, Others, Results) ->
    {OtherDbs, RemainingOthers} = split(OtherSplit, Others),
    {AccountDbs, RemainingAccounts} = split(AccountSplit, Accounts),
    NewResults = [{AccountDbs, OtherDbs}|Results],
    split(AccountSplit, RemainingAccounts, OtherSplit, RemainingOthers, NewResults).

-spec split(integer(), [any()]) -> {[any()],[any()]}.
split(Count, List) ->
    case length(List) >= Count of
        'false' -> {List, []};
        'true' -> lists:split(Count, List)
    end.

-spec parallel_migrate(integer(), split_results(), kz_term:references()) -> 'no_return'.
parallel_migrate(_, [], Refs) -> wait_for_parallel_migrate(Refs);
parallel_migrate(Pause, [{Accounts, Others}|Remaining], Refs) ->
    Self = self(),
    Dbs = lists:sort(fun get_database_sort/2, lists:usort(Accounts ++ Others)),
    Ref = make_ref(),
    _Pid = kz_util:spawn_link(fun parallel_migrate_worker/4, [Ref, Pause, Dbs, Self]),
    parallel_migrate(Pause, Remaining, [Ref|Refs]).

-spec parallel_migrate_worker(reference(), integer(), kz_term:ne_binaries(), pid()) -> reference().
parallel_migrate_worker(Ref, Pause, Databases, Parent) ->
    _ = (catch migrate(Pause, Databases)),
    Parent ! Ref.

-spec wait_for_parallel_migrate(kz_term:references()) -> 'no_return'.
wait_for_parallel_migrate([]) ->
    %% Migrate settings for kazoo_media
    io:format("running media migrations...~n"),
    _ = kazoo_media_maintenance:migrate(),
    'no_return';
wait_for_parallel_migrate([Ref|Refs]) ->
    receive
        Ref -> wait_for_parallel_migrate(Refs)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec blocking_refresh() -> 'no_return'.
blocking_refresh() -> refresh().

-spec blocking_refresh(kz_term:text() | non_neg_integer()) -> 'no_return'.
blocking_refresh(Pause) ->
    Databases = get_databases(),
    refresh(Databases, Pause).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec refresh() -> 'no_return'.
refresh() ->
    Databases = get_databases(),
    refresh(Databases, 2 * ?MILLISECONDS_IN_SECOND).

-spec refresh(kz_term:ne_binaries(), kz_term:text() | non_neg_integer()) -> 'no_return'.
refresh(Databases, Pause) ->
    Total = length(Databases),
    refresh(Databases, kz_term:to_integer(Pause), Total).

-spec refresh(kz_term:ne_binaries(), non_neg_integer(), non_neg_integer()) -> 'no_return'.
refresh([], _, _) -> 'no_return';
refresh([Database|Databases], Pause, Total) ->
    io:format("~p (~p/~p) refreshing database '~s'~n"
             ,[self(), length(Databases) + 1, Total, Database]),
    _ = refresh(Database),
    _ = case Pause < 1 of
            'false' -> timer:sleep(Pause);
            'true' -> 'ok'
        end,
    refresh(Databases, Pause, Total).

-spec get_databases() -> kz_term:ne_binaries().
get_databases() ->
    {'ok', Databases} = kz_datamgr:db_info(),
    lists:sort(fun get_database_sort/2, lists:usort(Databases ++ ?KZ_SYSTEM_DBS)).

-spec get_database_sort(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
get_database_sort(Db1, Db2) ->
    kzs_util:db_priority(Db1) < kzs_util:db_priority(Db2).

-spec refresh(kz_term:ne_binary()) -> 'ok'.
refresh(Database) ->
    kz_datamgr:refresh_views(Database),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_depreciated_databases() -> 'ok'.
remove_depreciated_databases() ->
    Databases = get_databases(),
    remove_depreciated_databases(Databases).

-spec remove_depreciated_databases(kz_term:ne_binaries()) -> 'ok'.
remove_depreciated_databases([]) -> 'ok';
remove_depreciated_databases([Database|Databases]) ->
    _ = case kz_datamgr:db_classification(Database) of
            'deprecated' ->
                io:format("    archive and remove depreciated database ~s~n", [Database]),
                _ = kz_datamgr:db_archive(Database),
                maybe_delete_db(Database);
            _Else -> 'ok'
        end,
    remove_depreciated_databases(Databases).

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
                io:format("    verifing ~s doc ~s~n", [?KZ_ACCOUNTS_DB, AccountId]),
                cleanup_aggregated_account(AccountId)
        end,
    cleanup_aggregated_accounts(JObjs).

-spec cleanup_aggregated_account(kz_term:ne_binary()) -> 'ok'.
cleanup_aggregated_account(Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kz_datamgr:open_doc(AccountDb, AccountId) of
        {'error', 'not_found'} -> remove_aggregated_account(AccountDb);
        _Else -> 'ok'
    end.

-spec remove_aggregated_account(kz_term:ne_binary()) -> 'ok'.
remove_aggregated_account(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
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
                io:format("    verifing ~s doc ~s~n", [?KZ_SIP_DB, DocId]),
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
            AccountDb = kz_util:format_account_id(Account, 'encoded'),
            AccountId = kz_util:format_account_id(Account, 'raw'),
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
-spec cleanup_voicemail_media(kz_term:ne_binary()) ->
                                     {'ok', kz_json:objects()} |
                                     {'error', any()}.
cleanup_voicemail_media(Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
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
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    ViewOptions = ['include_docs'],
    case kz_datamgr:get_results(AccountDb, ?VMBOX_VIEW, ViewOptions) of
        {'ok', ViewRes} ->
            lists:foldl(fun extract_messages/2, [], ViewRes);
        {'error', _E} ->
            lager:error("coumd not load view ~p: ~p", [?VMBOX_VIEW, _E]),
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
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
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
                    'false' -> kz_util:format_account_id(Account, 'encoded')
                end,
    {TT, IT} = clean_trunkstore_docs(AccountDb, TwowayTrunks, InboundTrunks),
    JObj = kz_json:from_list(
             [{<<"_id">>, <<"limits">>}
             ,{<<"twoway_trunks">>, TT}
             ,{<<"inbound_trunks">>, IT}
             ,{<<"pvt_account_db">>, AccountDb}
             ,{<<"pvt_account_id">>, kz_util:format_account_id(Account, 'raw')}
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
    %% if there are no servers and it was created by jonny5 softdelete the doc
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
                    'false' -> kz_util:format_account_id(Account, 'encoded')
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
-spec ensure_aggregate_devices() -> 'ok'.
ensure_aggregate_devices() ->
    ensure_aggregate_devices(kapps_util:get_all_accounts()).

-spec ensure_aggregate_devices(kz_term:ne_binaries()) -> 'ok'.
ensure_aggregate_devices([]) -> 'ok';
ensure_aggregate_devices([Account|Accounts]) ->
    _ = ensure_aggregate_device(Account),
    ensure_aggregate_devices(Accounts).

-spec ensure_aggregate_device(kz_term:ne_binary()) -> 'ok'.
ensure_aggregate_device(Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
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
-spec find_invalid_acccount_dbs() -> kz_term:ne_binaries().
find_invalid_acccount_dbs() ->
    lists:foldr(fun find_invalid_acccount_dbs_fold/2, [], kapps_util:get_all_accounts()).

find_invalid_acccount_dbs_fold(AccountDb, Acc) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    case kz_datamgr:open_doc(AccountDb, AccountId) of
        {'error', 'not_found'} -> [AccountDb|Acc];
        {'ok', _} -> Acc
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
    %% 4. Remove the original (erronous) attachment
    %% However, if it failes at any of those stages it will leave the media doc with multiple
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
    %% bigcouch is awesome in that it sometimes returns 409 (conflict) but does the work anyway..
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
    Db = kz_util:format_account_id(Account, 'encoded'),
    Opts = [{'key', Type}
           ,{'limit', ChunkSize}
           ,'include_docs'
           ],
    case kz_datamgr:get_results(Db, <<"maintenance/listing_by_type">>, Opts) of
        {'error', _}=E -> E;
        {'ok', []} -> 'ok';
        {'ok', Ds} ->
            lager:debug("deleting up to ~p documents of type ~p", [ChunkSize, Type]),
            kz_datamgr:del_docs(Db, [kz_json:get_value(<<"doc">>, D) || D <- Ds]),
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
    lager:info("Channel ~s", [CallId]),
    lager:info("Status: ~s", [kz_json:get_value(<<"Status">>, Resp)]),
    lager:info("Media Server: ~s", [kz_json:get_value(<<"Switch-Hostname">>, Resp)]),
    lager:info("Responding App: ~s", [kz_json:get_value(<<"App-Name">>, Resp)]),
    lager:info("Responding Node: ~s", [kz_json:get_value(<<"Node">>, Resp)]).

-spec last_migrate_version() -> kz_term:ne_binary().
last_migrate_version() ->
    kapps_config:get_ne_binary(?MODULE, <<"migrate_current_version">>, <<"3.22">>).

-spec set_last_migrate_version(kz_term:ne_binary()) -> {'ok', kz_json:object()}.
set_last_migrate_version(Version) ->
    kapps_config:set(?MODULE, <<"migrate_current_version">>, Version).

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
    [fun handle_module_rename/0];
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

-spec validate_system_configs() -> [{kz_term:ne_binary(), kz_json:object()}].
validate_system_configs() ->
    [{Config, merge_errors(Status)}
     || Config <- kapps_config_doc:list_configs(),
        Status <- [validate_system_config(Config)],
        [] =/= Status
    ].

-spec merge_errors(kz_json_schema:validation_errors()) -> kz_json:object().
merge_errors([{_Code, _Message, ErrorJObj} | StatusErrors]) ->
    lists:foldl(fun({_C, _M, ErrJObj}, AccJObj) ->
                        kz_json:merge(ErrJObj, AccJObj)
                end
               ,ErrorJObj
               ,StatusErrors
               ).

-spec validate_system_config(kz_term:ne_binary()) -> kz_json_schema:validation_errors().
validate_system_config(Id) ->
    Doc = get_config_document(Id),
    Schema = kapps_config_util:system_config_document_schema(Id),
    case kz_json_schema:validate(Schema, Doc) of
        {'ok', _} -> [];
        {'error', Errors} -> kz_json_schema:errors_to_jobj(Errors)
    end.

-spec get_config_document(kz_term:ne_binary()) -> kz_json:object().
get_config_document(Id) ->
    kz_doc:public_fields(maybe_new(kapps_config:fetch_category(Id))).

-spec maybe_new({'ok', kz_json:object()} | {'error', any()}) -> kz_json:object().
maybe_new({'ok', Doc}) -> Doc;
maybe_new(_) -> kz_json:new().

-spec cleanup_system_config(kz_term:ne_binary()) -> {'ok', kz_json:object()}.
cleanup_system_config(Id) ->
    Doc = maybe_new(kapps_config:fetch_category(Id)),
    ErrorKeys = error_keys(validate_system_config(Id)),
    NewDoc = lists:foldl(fun kz_json:delete_key/2, Doc, ErrorKeys),
    kz_datamgr:save_doc(?KZ_CONFIG_DB, NewDoc).

-spec error_keys(kz_json_schema:validation_errors()) -> kz_json:paths().
error_keys(Errors) ->
    io:format("errors: ~p~n", [Errors]),
    [binary:split(ErrorKey, <<".">>)
     || {_Code, _Message, ErrorJObj} <- Errors,
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
%% If account's views were read from file system, it put them inside
%% `account_view' process dictionary.
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
%% @doc Returns all account related views from file system.
%%
%% Currently reads account's related views from these apps:
%% <ul>
%% <li>{@link kazoo_apps}</li>
%% <li>{@link conference}</li>
%% <li>{@link webhooks}</li>
%% <li>{@link crossbar}</li>
%% <li>{@link callflow}</li>>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec read_all_account_views() -> kz_datamgr:views_listing().
read_all_account_views() ->
    [kapps_util:get_view_json('kazoo_apps', ?MAINTENANCE_VIEW_FILE)
    ,kapps_util:get_view_json('conference', <<"views/conference.json">>)
    ,kapps_util:get_view_json('webhooks', <<"webhooks.json">>)
     |kapps_util:get_views_json('crossbar', "account")
     ++ kapps_util:get_views_json('callflow', "views")
    ].

%%------------------------------------------------------------------------------
%% @doc Initialize and update core database views from file system.
%%
%% This includes reading views from file system and revise below views docs:
%% <ul>
%% <li>`maintenance' view in `sip_auth' database</li>
%% <li>`accounts', `maintenance' and `search' views for `accounts' database</li>
%% <li>`dedicated_ips' database views</li>
%% </ul>
%%
%% This function is always called by {@link init_system/0} during system startup.
%% @end
%%------------------------------------------------------------------------------
-spec init_dbs() -> 'ok'.
init_dbs() ->
    SipViews = [kapps_util:get_view_json('kazoo_apps', ?MAINTENANCE_VIEW_FILE)],
    _ = kapps_util:update_views(?KZ_SIP_DB, SipViews, 'false'),
    Views = [kapps_util:get_view_json('kazoo_apps', ?MAINTENANCE_VIEW_FILE)
            ,kapps_util:get_view_json('kazoo_apps', ?ACCOUNTS_AGG_VIEW_FILE)
            ,kapps_util:get_view_json('kazoo_apps', ?SEARCH_VIEW_FILE)
            ],
    _ = kapps_util:update_views(?KZ_ACCOUNTS_DB, Views, 'false'),
    _ = kz_datamgr:revise_docs_from_folder(?KZ_DEDICATED_IP_DB, 'kazoo_ips', "views"),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Read all account and account's MODB views from file system and put/update
%% them `system_data' database to be read by refresh views later.
%%
%% This function reads all account and account's MODB related views from various
%% application and classify each view and creates and updates them in
%% `system_data' database. Refreshing views will read account's views from this
%% this database.
%%
%% This function is always called by {@link init_system/0} during system startup.
%% @end
%%------------------------------------------------------------------------------
-spec register_account_views() -> 'ok'.
register_account_views() ->
    kazoo_modb_maintenance:register_views(),
    Views = read_all_account_views(),
    kz_datamgr:register_views('account', Views).

%%------------------------------------------------------------------------------
%% @doc Initialize and update core database views and register account's views.
%%
%% This function is always called by {@link kapps_controller} during system
%% startup to update core views and register account views.
%%
%% @see kapps_controller:start_link/0. `kapps_controller' for system startup
%% @see crossbar_maintenance:db_init/0. `crossbar_maintenance:db_init/0' for
%% updating other system databases views and system schema.
%% @end
%%------------------------------------------------------------------------------
-spec init_system() -> 'ok'.
init_system() ->
    init_dbs(),
    register_account_views(),
    lager:notice("system initialized").

-spec check_release() -> 'ok' | 'error'.
check_release() ->
    Checks = [fun kapps_started/0
             ,fun master_account_created/0
             ,fun migration_4_0_ran/0
             ,fun migration_ran/0
             ,fun kazoo_proper_maintenance:run_seq_modules/0
             ],
    try lists:foreach(fun(F) -> F() end, Checks) of
        'ok' ->
            lager:info("check_release/0 succeeded"),
            init:stop()
    catch
        'throw':Error ->
            lager:error("check_release/0 failed: ~p", [Error]),
            halt(1);
        _E:_R ->
            lager:error("check_release/0 crashed: ~s: ~p", [_E, _R]),
            halt(1)
    end.

-spec kapps_started() -> boolean().
kapps_started() ->
    kapps_started(180 * ?MILLISECONDS_IN_SECOND).

-spec kapps_started(integer()) -> 'true'.
kapps_started(Timeout) when Timeout > 0 ->
    kapps_controller:ready()
        orelse begin
                   timer:sleep(100),
                   kapps_started(Timeout - 100)
               end;
kapps_started(_Timeout) ->
    lager:error("timed out waiting for kapps to start"),
    throw({'error', 'timeout'}).

-spec master_account_created() -> 'true'.
master_account_created() ->
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
            'true' = kzd_accounts:is_superduper_admin(MasterAccountId);
        'failed' -> throw({'error', 'create_account'})
    end.

-spec migration_4_0_ran() -> boolean().
migration_4_0_ran() ->
    'no_return' =:= migrate_to_4_0().

-spec migration_ran() -> boolean().
migration_ran() ->
    'no_return' =:= migrate().
