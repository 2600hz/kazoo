%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc data plan
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_plan).

-export([plan/0, plan/1, plan/2, plan/3]).

-export([get_dataplan/2]).

-export([init/1, reload/0, reload/1]).

-include("kz_data.hrl").

-define(IS_JSON_GUARD(Obj), is_tuple(Obj)
        andalso is_list(element(1, Obj))
       ).

-define(NEW_CONNECTION_TIMEOUT, ?MILLISECONDS_IN_SECOND * 5).

-define(SYSTEM_DATAPLAN, <<"system">>).
-define(DATAPLAN_FILE_LOCATION, "defaults").

-define(CACHED_SYSTEM_DATAPLAN, fetch_cached_dataplan(?SYSTEM_DATAPLAN, fun fetch_simple_dataplan/1)).
-define(CACHED_ACCOUNT_DATAPLAN(A), fetch_cached_dataplan(A, fun fetch_account_dataplan/1)).
-define(CACHED_STORAGE_DATAPLAN(A,B), fetch_cached_dataplan({A, B}, fun fetch_storage_dataplan/1)).

-define(KZS_PLAN_INIT_SLICE, 100).
-define(KZS_PLAN_INIT_VIEW, <<"storage/accounts">>).

-spec plan() -> map().
plan() ->
    system_dataplan().

-spec plan(kz_term:ne_binary()) -> map().
plan(DbName) ->
    get_dataplan(DbName).

-spec plan(kz_term:ne_binary(), atom() | kz_term:ne_binary() | view_options() | kz_json:object()) -> map().
plan(DbName, DocType) when is_binary(DocType) ->
    plan(DbName, DocType, 'undefined');
plan(DbName, Props) when is_list(Props) ->
    Type = props:get_value('doc_type', Props),
    Owner = props:get_first_defined(['storage_id', 'doc_owner'], Props),
    Plan = plan(DbName, Type, Owner),
    maybe_override_plan(Plan, props:get_value('plan_override', Props));
plan(DbName, Doc) when ?IS_JSON_GUARD(Doc) ->
    plan(DbName, kz_doc:type(Doc));
plan(DbName, 'undefined')  ->
    plan(DbName);
plan(DbName, DocType)
  when is_atom(DocType) ->
    plan(DbName, kz_term:to_binary(DocType)).

-spec plan(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary()) -> map().
plan(DbName, 'undefined', 'undefined') ->
    get_dataplan(DbName);
plan(DbName, DocType, 'undefined') ->
    get_dataplan(DbName, DocType);
plan(DbName, DocType, DocOwner) ->
    get_dataplan(DbName, DocType, DocOwner).

maybe_override_plan(Plan, 'undefined') -> Plan;
maybe_override_plan(Plan, #{}=Map) ->
    maps:merge(Plan, Map).

get_dataplan(DBName) ->
    case kzs_util:db_classification(DBName) of
        'modb' -> account_modb_dataplan(DBName);
        'account' -> account_dataplan(DBName);
        'resource_selectors' -> account_dataplan(DBName);
        Else -> system_dataplan(DBName, Else)
    end.

-spec get_dataplan(kz_term:ne_binary(), kz_term:api_ne_binary()) -> map().
get_dataplan(DBName, 'undefined') ->
    get_dataplan(DBName);
get_dataplan(DBName, DocType) ->
    case kzs_util:db_classification(DBName) of
        'modb' -> account_modb_dataplan(DBName, DocType);
        'account' -> account_dataplan(DBName, DocType);
        'resource_selectors' -> account_dataplan(DBName, DocType);
        Else -> system_dataplan(DBName, Else)
    end.

get_dataplan(DBName, DocType, 'undefined') ->
    get_dataplan(DBName, DocType);
get_dataplan(DBName, DocType, DocOwner) ->
    case kzs_util:db_classification(DBName) of
        'modb' -> account_modb_dataplan(DBName, DocType, DocOwner);
        'account' -> account_dataplan(DBName, DocType, DocOwner);
        'resource_selectors' -> account_dataplan(DBName, DocType, DocOwner);
        Else -> system_dataplan(DBName, Else)
    end.

system_dataplan() ->
    system_dataplan(?KZ_CONFIG_DB, 'system').

system_dataplan(DBName, _Classification)
  when DBName == ?KZ_CONFIG_DB;
       DBName == ?KZ_DATA_DB;
       DBName == ?KZ_SERVICES_DB ->
    SysTag = 'local',
    #{tag => SysTag, server => kz_dataconnections:get_server(SysTag)};
system_dataplan(_DBName, 'numbers') ->
    Plan = ?CACHED_SYSTEM_DATAPLAN,
    dataplan_type_match(<<"system">>, <<"numbers">>, Plan);
system_dataplan(DBName, _Classification) ->
    Plan = ?CACHED_SYSTEM_DATAPLAN,
    dataplan_type_match(<<"system">>, DBName, Plan).

account_dataplan(AccountDb) ->
    AccountId = kz_util:format_account_id(AccountDb),
    Plan = ?CACHED_ACCOUNT_DATAPLAN(AccountId),
    dataplan_match(<<"account">>, Plan, AccountId).

account_dataplan(AccountDb, 'undefined') ->
    account_dataplan(AccountDb);
account_dataplan(AccountDb, DocType) ->
    AccountId = kz_util:format_account_id(AccountDb),
    Plan = ?CACHED_ACCOUNT_DATAPLAN(AccountId),
    dataplan_type_match(<<"account">>, DocType, Plan, AccountId).

account_dataplan(AccountDb, DocType, 'undefined') ->
    account_dataplan(AccountDb, DocType);
account_dataplan(AccountDb, DocType, StorageId) ->
    AccountId = kz_util:format_account_id(AccountDb),
    Plan = ?CACHED_STORAGE_DATAPLAN(AccountId, StorageId),
    dataplan_type_match(<<"account">>, DocType, Plan, AccountId).

account_modb_dataplan(AccountMODB) ->
    AccountId = kz_util:format_account_id(AccountMODB),
    Plan = ?CACHED_ACCOUNT_DATAPLAN(AccountId),
    dataplan_match(<<"modb">>, Plan, AccountId).

account_modb_dataplan(AccountMODB, 'undefined') ->
    account_modb_dataplan(AccountMODB);
account_modb_dataplan(AccountMODB, DocType) ->
    AccountId = kz_util:format_account_id(AccountMODB),
    Plan = ?CACHED_ACCOUNT_DATAPLAN(AccountId),
    dataplan_type_match(<<"modb">>, DocType, Plan, AccountId).

account_modb_dataplan(AccountMODB, DocType, 'undefined') ->
    account_modb_dataplan(AccountMODB, DocType);
account_modb_dataplan(AccountMODB, DocType, StorageId) ->
    AccountId = kz_util:format_account_id(AccountMODB),
    Plan = ?CACHED_STORAGE_DATAPLAN(AccountId, StorageId),
    dataplan_type_match(<<"modb">>, DocType, Plan, AccountId).

-spec dataplan_connections(map(),map()) -> [{atom(), server()}].
dataplan_connections(Map, Index) ->
    [maybe_start_connection(C, maps:get(C, Index, #{}))
     || {_, #{<<"connection">> := C}} <- maps:to_list(Map)
    ].

-spec dataplan_match(kz_term:ne_binary(), map(), kz_term:api_binary()) -> map().
dataplan_match(Classification, Plan, AccountId) ->
    #{<<"plan">> := #{Classification := #{<<"connection">> := CCon
                                         ,<<"attachments">> := CAtt
                                         ,<<"types">> := Types
                                         }
                     }
     ,<<"connections">> := GCon
     ,<<"attachments">> := GAtt
     } = Plan,

    {Tag, Server} = maybe_start_connection(CCon, maps:get(CCon, GCon, #{})),
    Others = [T || T <- lists:usort(fun({T1,_}, {T2, _}) -> T1 =< T2 end
                                   ,dataplan_connections(Types, GCon)
                                   ),
                   T =/= Tag
             ],

    case maps:get(<<"handler">>, CAtt, 'undefined') of
        'undefined' ->
            #{tag => Tag
             ,server => Server
             ,others => Others
             ,classification => Classification
             ,account_id => AccountId
             };
        AttConnection ->
            #{AttConnection := #{<<"handler">> := AttHandlerBin
                                ,<<"settings">> := AttSettings
                                }
             } = GAtt,
            AttHandler = kz_term:to_atom(<<"kz_att_", AttHandlerBin/binary>>,'true'),
            Params = maps:merge(AttSettings, maps:get(<<"params">>, CAtt, #{})),

            #{tag => Tag
             ,server => Server
             ,others => Others
             ,att_proxy => 'true'
             ,att_post_handler => att_post_handler(CAtt)
             ,att_handler => {AttHandler, kz_maps:keys_to_atoms(Params)}
             ,att_handler_id => AttConnection
             ,classification => Classification
             ,account_id => AccountId
             }
    end.


-spec dataplan_type_match(kz_term:ne_binary(), kz_term:ne_binary(), map()) -> map().
dataplan_type_match(Classification, DocType, Plan) ->
    dataplan_type_match(Classification, DocType, Plan, 'undefined').

-spec dataplan_type_match(kz_term:ne_binary(), kz_term:ne_binary(), map(), kz_term:api_binary()) -> map().
dataplan_type_match(Classification, DocType, Plan, AccountId) ->
    #{<<"plan">> := #{Classification := #{<<"types">> := Types
                                         ,<<"connection">> := CCon
                                         ,<<"attachments">> := CAtt
                                         }
                     }
     ,<<"connections">> := GCon
     ,<<"attachments">> := GAtt
     } = Plan,

    TypeMap = maps:get(DocType, Types, #{}),

    Connection = maps:get(<<"connection">>, TypeMap, CCon),
    {Tag, Server} = maybe_start_connection(Connection, maps:get(Connection, GCon, #{})),

    TypeAttMap = maps:merge(CAtt, maps:get(<<"attachments">>, TypeMap, #{})),
    case maps:get(<<"handler">>, TypeAttMap, 'undefined') of
        'undefined' ->
            #{tag => Tag, server => Server
             ,classification => Classification
             ,doc_type => DocType
             ,account_id => AccountId
             };
        AttConnection ->
            #{AttConnection := #{<<"handler">> := AttHandlerBin
                                ,<<"settings">> := AttSettings
                                }
             } = GAtt,
            AttHandler = kz_term:to_atom(<<"kz_att_", AttHandlerBin/binary>>,'true'),
            Params = maps:merge(AttSettings, maps:get(<<"params">>, TypeAttMap, #{})),
            #{tag => Tag
             ,server => Server
             ,att_proxy => 'true'
             ,att_post_handler => att_post_handler(TypeAttMap)
             ,att_handler => {AttHandler, kz_maps:keys_to_atoms(Params)}
             ,att_handler_id => AttConnection
             ,classification => Classification
             ,doc_type => DocType
             ,account_id => AccountId
             }
    end.

-spec att_post_handler(map()) -> atom().
att_post_handler(#{<<"stub">> := 'true'}) -> 'stub';
att_post_handler(#{}) -> 'external'.

-type fetch_dataplan_ret() :: {list(), kz_json:object()}.

-spec fetch_cached_dataplan(term(), fun()) -> map().
fetch_cached_dataplan(Key, Fun) ->
    case kz_cache:fetch_local(?KAZOO_DATA_PLAN_CACHE, {'plan', Key}) of
        {'ok', Plan} -> Plan;
        {'error', 'not_found'} when Key =:= ?SYSTEM_DATAPLAN ->
            load_dataplan(Key, Fun);
        {'error', 'not_found'} when is_tuple(Key) ->
            {AccountId, OwnerId} = Key,
            lager:debug("failed to find owner ~s dataplan, trying account ~s", [OwnerId, AccountId]),
            ?CACHED_ACCOUNT_DATAPLAN(AccountId);
        {'error', 'not_found'} ->
            ?CACHED_SYSTEM_DATAPLAN
    end.

-spec load_dataplan(kz_term:ne_binary(), fun()) -> map().
load_dataplan(Key, Fun) ->
    case Fun(Key) of
        'invalid' ->
            lager:error("something is wrong!! dataplan key ~s is invalid", [Key]),
            kz_json:to_map(default_dataplan());
        {Keys, PlanJObj} -> cache_dataplan(Keys, PlanJObj)
    end.

-spec cache_dataplan(kz_term:ne_binaries(), kz_json:object()) -> map().
cache_dataplan([Key|_] = Keys, PlanJObj) ->
    Plan = kz_json:to_map(PlanJObj),
    CacheProps = [{'origin', [{'db', ?KZ_DATA_DB, K } || K <- Keys]}
                 ,{'expires','infinity'}
                 ,{'callback', fun cache_callback/3}
                 ],
    kz_cache:store_local_async(?KAZOO_DATA_PLAN_CACHE, {'plan', Key}, Plan, CacheProps),
    Plan.

-spec cache_callback({'plan', kz_term:ne_binary()}, map(), atom()) -> 'ok'.
cache_callback({'plan', ?SYSTEM_DATAPLAN}, _V, 'erase') ->
    lager:warning("received dataplan cache update for system plan"),
    _ = load_dataplan(?SYSTEM_DATAPLAN, fun fetch_simple_dataplan/1),
    'ok';
cache_callback({'plan', AccountId}, _V, 'erase') ->
    lager:warning("received dataplan cache update for account ~s", [AccountId]),
    _ = load_dataplan(AccountId, fun fetch_account_dataplan/1),
    'ok';
cache_callback(_Key, _V, _Action) ->
    lager:warning_unsafe("unhandled cache callback : ~p , ~p , ~p", [_Key, _V, _Action]).

-spec fetch_simple_dataplan(kz_term:ne_binary()) -> fetch_dataplan_ret().
fetch_simple_dataplan(Id) ->
    {[Id], fetch_dataplan(Id)}.

-spec fetch_account_dataplan(kz_term:ne_binary()) -> fetch_dataplan_ret().
fetch_account_dataplan(AccountId) ->
    case fetch_dataplan(AccountId) of
        'undefined' -> 'invalid';
        JObj -> fetch_account_dataplan(AccountId, JObj)
    end.

-spec fetch_account_dataplan(kz_term:ne_binary(), kz_json:object()) -> fetch_dataplan_ret().
fetch_account_dataplan(AccountId, AccountJObj) ->
    SystemJObj = fetch_dataplan(?SYSTEM_DATAPLAN),
    case kz_json:get_ne_binary_value(<<"pvt_plan_id">>, AccountJObj) of
        'undefined' ->
            Keys = [AccountId, ?SYSTEM_DATAPLAN],
            MergedJObj = kz_json:merge_recursive(SystemJObj, AccountJObj),
            {Keys, MergedJObj};
        PlanId ->
            PlanJObj = kz_json:merge_recursive(SystemJObj, fetch_dataplan(PlanId)),
            MergedJObj = kz_json:merge_recursive(PlanJObj, AccountJObj),
            Keys = [AccountId, PlanId, ?SYSTEM_DATAPLAN],
            {Keys, MergedJObj}
    end.

-spec fetch_storage_dataplan({kz_term:ne_binary(), kz_term:ne_binary()}) -> fetch_dataplan_ret().
fetch_storage_dataplan({AccountId, StorageId}) ->
    {Keys, AccountPlan} = fetch_account_dataplan(AccountId),
    MergedJObj = kz_json:merge_recursive(AccountPlan, fetch_dataplan(StorageId)),
    {[StorageId | Keys], MergedJObj}.

-spec fetch_dataplan(kz_term:ne_binary()) -> kz_json:api_object().
fetch_dataplan(Id) ->
    case kz_datamgr:open_cache_doc(?KZ_DATA_DB, Id) of
        {'ok', JObj} -> JObj;
        {'error', _} when Id =:= ?SYSTEM_DATAPLAN -> default_dataplan();
        {'error', _ERR} -> 'undefined'
    end.

-spec fetch_dataplan_from_file(kz_term:ne_binary()) -> kz_json:object().
fetch_dataplan_from_file(Id) ->
    JObj = kz_json:load_fixture_from_file(?APP, ?DATAPLAN_FILE_LOCATION, [Id, ".json"]),
    'ok' = kzs_cache:add_to_doc_cache(?KZ_DATA_DB, Id, JObj),
    JObj.

-spec default_dataplan() -> kz_json:object().
default_dataplan() ->
    fetch_dataplan_from_file(?SYSTEM_DATAPLAN).

-spec maybe_start_connection(atom() | kz_term:ne_binary(), map()) -> {atom(), server()}.
maybe_start_connection(Connection, Params)
  when is_binary(Connection) ->
    maybe_start_connection(kz_term:to_atom(Connection, 'true'), Params);
maybe_start_connection(Tag, Params) ->
    case kz_dataconnections:get_server(Tag) of
        'undefined' -> start_connection(Tag, Params);
        Server -> {Tag, Server}
    end.

-spec start_connection(atom(), map()) -> {atom(), server()}.
start_connection(Tag, Params) ->
    Connection = kz_dataconfig:connection(kz_maps:keys_to_atoms(Params#{tag => Tag})),
    kz_dataconnections:add(Connection),
    case kz_dataconnections:wait_for_connection(Tag, ?NEW_CONNECTION_TIMEOUT) of
        'no_connection' ->
            lager:critical("connection ~s unavailable, check for connection errors", [Tag]),
            {Tag, {'kazoo_dataconnection_error', #{}}};
        'ok' -> {Tag, kz_dataconnections:get_server(Tag)}
    end.

-spec init(map()) -> 'ok'.
init(Server) ->
    case kzs_view:get_results(Server, ?KZ_DATA_DB, ?KZS_PLAN_INIT_VIEW, []) of
        {'ok', []} -> lager:info("no dataplans to load");
        {'ok', JObjs} -> load_accounts(kz_datamgr:get_result_ids(JObjs));
        Error -> lager:error_unsafe("error initializing dataplans ~p", [Error])
    end,
    bind().

-spec bind() -> 'ok'.
-ifdef(TEST).
bind() -> 'ok'.
-else.
bind() ->
    RK = kz_binary:join([<<"kapi.conf">>
                        ,kz_term:to_binary(?KAZOO_DATA_PLAN_CACHE)
                        ,?KZ_DATA_DB
                        ,<<"storage">>
                        ,<<"doc_created">>
                        ,<<"*">>
                        ], <<".">>),
    lager:debug("binding for new storage: ~s", [RK]),
    kazoo_bindings:bind(RK, fun handle_new/1).

-spec handle_new(kz_json:objects()) -> 'ok'.
handle_new([JObj]) ->
    AccountId = kz_json:get_ne_binary_value(<<"ID">>, JObj),
    lager:warning("received new storage for account ~s", [AccountId]),
    load_account(AccountId).
-endif.

-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?KAZOO_DATA_PLAN_CACHE).

-spec reload() -> 'ok'.
reload() ->
    _ = flush(),
    case kz_datamgr:get_result_ids(?KZ_DATA_DB, ?KZS_PLAN_INIT_VIEW, []) of
        {'ok', []} -> lager:info("no dataplans to load");
        {'ok', Accounts} -> load_accounts(Accounts);
        Error -> lager:error_unsafe("error reloading dataplans ~p", [Error])
    end.

-spec reload(kz_term:ne_binary()) -> 'ok'.
reload(AccountId) ->
    case kz_cache:peek_local(?KAZOO_DATA_PLAN_CACHE, {'plan', AccountId}) of
        {'ok', _Plan} -> kz_cache:erase_local(?KAZOO_DATA_PLAN_CACHE, {'plan', AccountId});
        _Else -> load_account(AccountId)
    end.

-spec load_accounts(kz_term:ne_binaries()) -> 'ok'.
load_accounts(Accounts)
  when length(Accounts) > ?KZS_PLAN_INIT_SLICE ->
    {A, B} = lists:split(?KZS_PLAN_INIT_SLICE, Accounts),
    _ = kz_util:spawn(fun load_accounts/1, [B]),
    load_accounts(A);
load_accounts(Accounts) ->
    lists:foreach(fun load_account/1, Accounts),
    'ok'.

-spec load_account(kz_term:ne_binary()) -> 'ok'.
load_account(AccountId) ->
    _ = load_dataplan(AccountId, fun fetch_account_dataplan/1),
    'ok'.
