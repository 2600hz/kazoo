%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data plan
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_plan).

-include("kz_data.hrl").

-export([plan/0, plan/1, plan/2, plan/3, flush/0]).

-define(IS_JSON_GUARD(Obj), is_tuple(Obj) andalso is_list(element(1, Obj))).

-define(NEW_CONNECTION_TIMEOUT, ?MILLISECONDS_IN_SECOND * 5).

-define(SYSTEM_DATAPLAN, <<"system">>).
-define(DATAPLAN_FILE_LOCATION, "defaults").

flush() ->
    kz_cache:flush_local(?KZ_DP_CACHE).

plan() ->
    system_dataplan().

plan(DbName) ->
    get_dataplan(DbName).

-spec plan(ne_binary(), atom() | ne_binary() | view_options() | wh_json:object()) -> map().
plan(DbName, DocType) when is_binary(DocType) ->
    plan(DbName, DocType, 'undefined');
plan(DbName, Props) when is_list(Props) ->
    plan(DbName, props:get_value('doc_type', Props), props:get_value('doc_owner', Props));
plan(DbName, Doc) when ?IS_JSON_GUARD(Doc) ->
    plan(DbName, wh_doc:type(Doc));
plan(DbName, 'undefined')  ->
    plan(DbName);
plan(DbName, DocType)
  when is_atom(DocType) ->
    plan(DbName, wh_util:to_binary(DocType)).

-spec plan(ne_binary(), api_binary(), api_binary()) -> map().
plan(DbName, 'undefined', 'undefined') ->
    get_dataplan(DbName);
plan(DbName, DocType, 'undefined') ->
    get_dataplan(DbName, DocType);
plan(DbName, DocType, DocOwner) ->
    get_dataplan(DbName, DocType, DocOwner).

get_dataplan(DBName) ->
    case kzs_util:db_classification(DBName) of
        'modb' -> account_modb_dataplan(DBName);
        'account' -> account_dataplan(DBName);
        Else -> system_dataplan(DBName, Else)
    end.

get_dataplan(DBName, 'undefined') ->
    get_dataplan(DBName);
get_dataplan(DBName, DocType) ->
    case kzs_util:db_classification(DBName) of
        'modb' -> account_modb_dataplan(DBName, DocType);
        'account' -> account_dataplan(DBName, DocType);
        Else -> system_dataplan(DBName, Else)
    end.

get_dataplan(DBName, DocType, 'undefined') ->
    get_dataplan(DBName, DocType);
get_dataplan(DBName, DocType, DocOwner) ->
    case kzs_util:db_classification(DBName) of
        'modb' -> account_modb_dataplan(DBName, DocType, DocOwner);
        'account' -> account_dataplan(DBName, DocType, DocOwner);
        Else -> system_dataplan(DBName, Else)
    end.

system_dataplan() ->
    system_dataplan(?WH_CONFIG_DB, 'system').

system_dataplan(DBName, _Classification)
  when DBName == ?WH_CONFIG_DB;
       DBName == ?KZ_DATA_DB;
       DBName == ?WH_SERVICES_DB ->
    SysTag = 'local',
    #{tag => SysTag, server => kz_dataconnections:get_server(SysTag)};
system_dataplan(_DBName, 'numbers') ->
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN]),
    dataplan_match(<<"system">>, <<"numbers">>, Plan);
system_dataplan(DBName, _Classification) ->
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN]),
    dataplan_match(<<"system">>, DBName, Plan).

account_dataplan(AccountDb) ->
    AccountId = wh_util:format_account_id(AccountDb),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ]),
    dataplan_match(<<"account">>, Plan).

account_dataplan(AccountDb, 'undefined') ->
    account_dataplan(AccountDb);
account_dataplan(AccountDb, DocType) ->
    AccountId = wh_util:format_account_id(AccountDb),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ]),
    dataplan_match(<<"account">>, DocType, Plan).

account_dataplan(AccountDb, DocType, 'undefined') ->
    account_dataplan(AccountDb, DocType);
account_dataplan(AccountDb, DocType, DocOwner) ->
    AccountId = wh_util:format_account_id(AccountDb),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ,DocOwner
                          ]),
    dataplan_match(<<"account">>, DocType, Plan).

account_modb_dataplan(AccountMODB) ->
    AccountId = wh_util:format_account_id(AccountMODB),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ]),
    dataplan_match(<<"modb">>, Plan).

account_modb_dataplan(AccountMODB, 'undefined') ->
    account_modb_dataplan(AccountMODB);
account_modb_dataplan(AccountMODB, DocType) ->
    AccountId = wh_util:format_account_id(AccountMODB),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ]),
    dataplan_match(<<"modb">>, DocType, Plan).

account_modb_dataplan(AccountMODB, DocType, 'undefined') ->
    account_modb_dataplan(AccountMODB, DocType);
account_modb_dataplan(AccountMODB, DocType, DocOwner) ->
    AccountId = wh_util:format_account_id(AccountMODB),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ,DocOwner
                          ]),
    dataplan_match(<<"modb">>, DocType, Plan).

reseller_id(AccountId) ->
    case get('$plan_reseller') of
        AccountId -> 'undefined';
        _ -> put('$plan_reseller', AccountId),
             case whapps_util:get_master_account_id() of
                 {'ok', MasterAccountId} ->
                     ResellerId = reseller_id(AccountId, MasterAccountId),
                     put('$plan_reseller', 'undefined'),
                     ResellerId;
                 {'error', _Err} ->
                     lager:critical("no master account id whn getting account ~s", [AccountId]),
                     'undefined'
             end
    end.

reseller_id(AccountId, AccountId) -> 'undefined';
reseller_id(AccountId, _MasterAccountId) ->
    wh_services:find_reseller_id(AccountId).

-spec dataplan_connections(map(),map()) -> [{atom(), server()}].
dataplan_connections(Map, Index) ->
    [maybe_start_connection(C, maps:get(C, Index, #{}))
      || {_, #{<<"connection">> := C}} <- maps:to_list(Map)
    ].

-spec dataplan_match(ne_binary(), map()) -> map().
dataplan_match(Classification, Plan) ->
    #{<<"plan">> := #{Classification := #{<<"connection">> := CCon
                                         ,<<"attachments">> := CAtt
                                         ,<<"types">> := Types
                                         }
                     }
     ,<<"connections">> := GCon
     ,<<"attachments">> := GAtt
     } = Plan,

    {Tag, Server} = maybe_start_connection(CCon, maps:get(CCon, GCon, #{})),
    Others = lists:filter(fun(T) -> T =/= Tag end
                         ,lists:usort(fun({T1,_}, {T2, _}) -> T1 =< T2 end
                                     ,dataplan_connections(Types, GCon)
                                     )
                         ),

    case maps:get(<<"handler">>, CAtt, 'undefined') of
        'undefined' ->
            #{tag => Tag
             ,server => Server
             ,others => Others
             };
        AttConnection ->
            #{AttConnection := #{<<"handler">> := AttHandlerBin
                                ,<<"settings">> := AttSettings
                                }
             } = GAtt,
            AttHandler = wh_util:to_atom(AttHandlerBin,'true'),
            Params = maps:merge(AttSettings, maps:get(<<"params">>, CAtt, #{})),

            #{tag => Tag
             ,server => Server
             ,others => Others
             ,att_proxy => 'true'
             ,att_post_handler => att_post_handler(CAtt)
             ,att_handler => {AttHandler, map_keys_to_atoms(Params)}
             }
    end.

-spec dataplan_match(ne_binary(), ne_binary(), map()) -> map().
dataplan_match(Classification, DocType, Plan) ->
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
            #{tag => Tag, server => Server};
        AttConnection ->
            #{AttConnection := #{<<"handler">> := AttHandlerBin
                                ,<<"settings">> := AttSettings
                                }
             } = GAtt,
             AttHandler = wh_util:to_atom(AttHandlerBin,'true'),
             Params = maps:merge(AttSettings, maps:get(<<"params">>, TypeAttMap, #{})),
            #{tag => Tag
             ,server => Server
             ,att_proxy => 'true'
             ,att_post_handler => att_post_handler(TypeAttMap)
             ,att_handler => {AttHandler, map_keys_to_atoms(Params)}
             }
    end.

att_post_handler(#{<<"stub">> := 'true'}) -> 'stub';
att_post_handler(#{}) -> 'external'.

-spec fetch_dataplans(ne_binaries()) -> map().
fetch_dataplans([Key | Keys]=KPlan) ->
    case kz_cache:fetch_local(?KZ_DP_CACHE, {KPlan}) of
        {'ok', Plan} -> Plan;
        {'error', 'not_found'} ->
            lager:debug("creating new dataplan ~p", [KPlan]),
            Plan = fetch_dataplans(Keys, fetch_dataplan(Key)),
            CacheProps = [{'origin', [{'db', ?KZ_DATA_DB, K } || K <- Keys]}],
            kz_cache:store_local(?KZ_DP_CACHE, {KPlan}, Plan, CacheProps),
            Plan
    end.

fetch_dataplans([], Plan) -> wh_json:to_map(Plan);
fetch_dataplans([Key | Keys], Plan) ->
    NewPlan = fetch_dataplan(Key),
    MergedPlan = wh_json:merge_recursive(Plan, NewPlan),
    fetch_dataplans(Keys, MergedPlan).

fetch_dataplan(Keys)
  when is_list(Keys) ->
    fetch_dataplans([K || K <- Keys, K =/= 'undefined']);
fetch_dataplan(Id) ->
    case kz_datamgr:open_cache_doc(?KZ_DATA_DB, Id, ['cache_failures']) of
        {'ok', JObj} -> JObj;
        {'error', _} when Id =:= ?SYSTEM_DATAPLAN ->
            JObj = default_dataplan(),
            kz_datamgr:add_to_doc_cache(?KZ_DATA_DB, Id, JObj),
            JObj;
        {'error', _} -> wh_json:new()
    end.

fetch_dataplan_from_file(Id) ->
    JObj = wh_json:load_fixture_from_file('kazoo_data'
                                         ,?DATAPLAN_FILE_LOCATION
                                         ,[Id, ".json"]
                                         ),
    kzs_cache:add_to_doc_cache(?KZ_DATA_DB, Id, JObj),
     JObj.

default_dataplan() ->
    fetch_dataplan_from_file(?SYSTEM_DATAPLAN).

-spec maybe_start_connection(atom() | ne_binary(), map()) -> {atom(), server()}.
maybe_start_connection(Connection, Params)
  when is_binary(Connection) ->
    maybe_start_connection(wh_util:to_atom(Connection, 'true'), Params);
maybe_start_connection(Tag, Params) ->
    case kz_dataconnections:get_server(Tag) of
        'undefined' -> start_connection(Tag, Params);
        Server -> {Tag, Server}
    end.

-spec start_connection(atom(), map()) -> {atom(), server()}.
start_connection(Tag, Params) ->
    Connection = kz_dataconfig:connection(map_keys_to_atoms(Params#{tag => Tag})),
    kz_dataconnections:add(Connection),
    case kz_dataconnections:wait_for_connection(Tag, ?NEW_CONNECTION_TIMEOUT) of
        'no_connection' ->
            lager:critical("connection ~s unavailable, check for connection errors", [Tag]),
            {Tag, {'kazoo_dataconnection_error', #{}}};
        'ok' -> {Tag, kz_dataconnections:get_server(Tag)}
    end.

map_keys_to_atoms(Map) ->
    maps:fold(fun map_keys_to_atoms_fold/3, #{}, Map).

map_keys_to_atoms_fold(K, V, Acc) when is_map(V) ->
    Acc#{wh_util:to_atom(K, 'true') => map_keys_to_atoms(V)};
map_keys_to_atoms_fold(K, V, Acc) ->
    Acc#{wh_util:to_atom(K, 'true') => V}.
