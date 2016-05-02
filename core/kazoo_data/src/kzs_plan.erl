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
    kz_cache:flush_local(?KAZOO_DATA_PLAN_CACHE).

plan() ->
    system_dataplan().

plan(DbName) ->
    get_dataplan(DbName).

-spec plan(ne_binary(), atom() | ne_binary() | view_options() | kz_json:object()) -> map().
plan(DbName, DocType) when is_binary(DocType) ->
    plan(DbName, DocType, 'undefined');
plan(DbName, Props) when is_list(Props) ->
    plan(DbName, props:get_value('doc_type', Props), props:get_value('doc_owner', Props));
plan(DbName, Doc) when ?IS_JSON_GUARD(Doc) ->
    plan(DbName, kz_doc:type(Doc));
plan(DbName, 'undefined')  ->
    plan(DbName);
plan(DbName, DocType)
  when is_atom(DocType) ->
    plan(DbName, kz_util:to_binary(DocType)).

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
    system_dataplan(?KZ_CONFIG_DB, 'system').

system_dataplan(DBName, _Classification)
  when DBName == ?KZ_CONFIG_DB;
       DBName == ?KZ_DATA_DB;
       DBName == ?KZ_SERVICES_DB ->
    SysTag = 'local',
    #{tag => SysTag, server => kz_dataconnections:get_server(SysTag)};
system_dataplan(_DBName, 'numbers') ->
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN]),
    dataplan_type_match(<<"system">>, <<"numbers">>, Plan);
system_dataplan(DBName, _Classification) ->
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN]),
    dataplan_type_match(<<"system">>, DBName, Plan).

account_dataplan(AccountDb) ->
    AccountId = kz_util:format_account_id(AccountDb),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ]),
    dataplan_match(<<"account">>, Plan, AccountId).

account_dataplan(AccountDb, 'undefined') ->
    account_dataplan(AccountDb);
account_dataplan(AccountDb, DocType) ->
    AccountId = kz_util:format_account_id(AccountDb),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ]),
    dataplan_type_match(<<"account">>, DocType, Plan, AccountId).

account_dataplan(AccountDb, DocType, 'undefined') ->
    account_dataplan(AccountDb, DocType);
account_dataplan(AccountDb, DocType, DocOwner) ->
    AccountId = kz_util:format_account_id(AccountDb),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ,DocOwner
                          ]),
    dataplan_type_match(<<"account">>, DocType, Plan, AccountId).

account_modb_dataplan(AccountMODB) ->
    AccountId = kz_util:format_account_id(AccountMODB),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ]),
    dataplan_match(<<"modb">>, Plan, AccountId).

account_modb_dataplan(AccountMODB, 'undefined') ->
    account_modb_dataplan(AccountMODB);
account_modb_dataplan(AccountMODB, DocType) ->
    AccountId = kz_util:format_account_id(AccountMODB),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ]),
    dataplan_type_match(<<"modb">>, DocType, Plan, AccountId).

account_modb_dataplan(AccountMODB, DocType, 'undefined') ->
    account_modb_dataplan(AccountMODB, DocType);
account_modb_dataplan(AccountMODB, DocType, DocOwner) ->
    AccountId = kz_util:format_account_id(AccountMODB),
    Plan = fetch_dataplan([?SYSTEM_DATAPLAN
                          ,reseller_id(AccountId)
                          ,AccountId
                          ,DocOwner
                          ]),
    dataplan_type_match(<<"modb">>, DocType, Plan, AccountId).

reseller_id(AccountId) ->
    case get('$plan_reseller') of
        AccountId -> 'undefined';
        _ -> put('$plan_reseller', AccountId),
             case kapps_util:get_master_account_id() of
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
    kz_services:find_reseller_id(AccountId).

-spec dataplan_connections(map(),map()) -> [{atom(), server()}].
dataplan_connections(Map, Index) ->
    [maybe_start_connection(C, maps:get(C, Index, #{}))
      || {_, #{<<"connection">> := C}} <- maps:to_list(Map)
    ].

-spec dataplan_match(ne_binary(), map(), api_binary()) -> map().
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
             ,classification => Classification
             ,account_id => AccountId
             };
        AttConnection ->
            #{AttConnection := #{<<"handler">> := AttHandlerBin
                                ,<<"settings">> := AttSettings
                                }
             } = GAtt,
            AttHandler = kz_util:to_atom(AttHandlerBin,'true'),
            Params = maps:merge(AttSettings, maps:get(<<"params">>, CAtt, #{})),

            #{tag => Tag
             ,server => Server
             ,others => Others
             ,att_proxy => 'true'
             ,att_post_handler => att_post_handler(CAtt)
             ,att_handler => {AttHandler, kzs_util:map_keys_to_atoms(Params)}
             ,classification => Classification
             ,account_id => AccountId
             }
    end.

-spec dataplan_type_match(ne_binary(), ne_binary(), map()) -> map().
-spec dataplan_type_match(ne_binary(), ne_binary(), map(), api_binary()) -> map().

dataplan_type_match(Classification, DocType, Plan) ->
    dataplan_type_match(Classification, DocType, Plan, 'undefined').

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
             AttHandler = kz_util:to_atom(AttHandlerBin,'true'),
             Params = maps:merge(AttSettings, maps:get(<<"params">>, TypeAttMap, #{})),
            #{tag => Tag
             ,server => Server
             ,att_proxy => 'true'
             ,att_post_handler => att_post_handler(TypeAttMap)
             ,att_handler => {AttHandler, kzs_util:map_keys_to_atoms(Params)}
             ,classification => Classification
             ,doc_type => DocType
             ,account_id => AccountId
             }
    end.

att_post_handler(#{<<"stub">> := 'true'}) -> 'stub';
att_post_handler(#{}) -> 'external'.

-spec fetch_dataplans(ne_binaries()) -> map().
fetch_dataplans([Key | Keys]=KPlan) ->
    case kz_cache:fetch_local(?KAZOO_DATA_PLAN_CACHE, {KPlan}) of
        {'ok', Plan} -> Plan;
        {'error', 'not_found'} ->
            lager:debug("creating new dataplan ~p", [KPlan]),
            Plan = fetch_dataplans(Keys, fetch_dataplan(Key)),
            CacheProps = [{'origin', [{'db', ?KZ_DATA_DB, K } || K <- KPlan]}
                          ,{'expires','infinity'}
                         ],
            kz_cache:store_local(?KAZOO_DATA_PLAN_CACHE, {KPlan}, Plan, CacheProps),
            Plan
    end.

fetch_dataplans([], Plan) -> kz_json:to_map(Plan);
fetch_dataplans([Key | Keys], Plan) ->
    NewPlan = fetch_dataplan(Key),
    MergedPlan = kz_json:merge_recursive(Plan, NewPlan),
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
        {'error', _} -> kz_json:new()
    end.

fetch_dataplan_from_file(Id) ->
    JObj = kz_json:load_fixture_from_file('kazoo_data'
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
    maybe_start_connection(kz_util:to_atom(Connection, 'true'), Params);
maybe_start_connection(Tag, Params) ->
    case kz_dataconnections:get_server(Tag) of
        'undefined' -> start_connection(Tag, Params);
        Server -> {Tag, Server}
    end.

-spec start_connection(atom(), map()) -> {atom(), server()}.
start_connection(Tag, Params) ->
    Connection = kz_dataconfig:connection(kzs_util:map_keys_to_atoms(Params#{tag => Tag})),
    kz_dataconnections:add(Connection),
    case kz_dataconnections:wait_for_connection(Tag, ?NEW_CONNECTION_TIMEOUT) of
        'no_connection' ->
            lager:critical("connection ~s unavailable, check for connection errors", [Tag]),
            {Tag, {'kazoo_dataconnection_error', #{}}};
        'ok' -> {Tag, kz_dataconnections:get_server(Tag)}
    end.
