%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kapps_account_config).

-include("kazoo_config.hrl").

-export([get/2, get/3, get/4
        ,get_ne_binary/3, get_ne_binary/4
        ,get_ne_binaries/3, get_ne_binaries/4
        ,get_global/3, get_global/4
        ,get_from_reseller/3, get_from_reseller/4
        ,set/4
        ,set_global/4
        ,flush/2
        ,migrate/1
        ,config_doc_id/1
        ]).

-type api_account() :: api_ne_binary() | kapps_call:call() | kz_json:object().
-type account_or_not() :: ne_binary() | 'no_account_id'.


-spec get_ne_binary(api_account(), ne_binary(), kz_json:path()) -> api_ne_binary().
-spec get_ne_binary(api_account(), ne_binary(), kz_json:path(), Default) -> ne_binary() | Default.
get_ne_binary(Account, Category, Path) ->
    get_ne_binary(Account, Category, Path, undefined).
get_ne_binary(Account, Category, Path, Default) ->
    Value = get(Account, Category, Path, Default),
    case kz_term:is_empty(Value) of
        true -> Default;
        false -> kz_term:to_binary(Value)
    end.

-spec get_ne_binaries(api_account(), ne_binary(), kz_json:path()) -> ne_binaries().
-spec get_ne_binaries(api_account(), ne_binary(), kz_json:path(), Default) -> ne_binaries() | Default.
get_ne_binaries(Account, Category, Path) ->
    get_ne_binaries(Account, Category, Path, undefined).
get_ne_binaries(Account, Category, Path, Default) ->
    Values = get(Account, Category, Path, Default),
    case kz_term:is_ne_binaries(Values) of
        false -> Default;
        true -> Values
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Will search the account db first, then system_config for values.
%% @end
%%--------------------------------------------------------------------
-spec get_global(api_account(), ne_binary(), kz_json:path()) ->
                        kz_json:json_term().
-spec get_global(api_account(), ne_binary(), kz_json:path(), kz_json:api_json_term()) ->
                        kz_json:json_term().
get_global(Account, Category, Key) ->
    get_global(Account, Category, Key, 'undefined').

get_global(Account, Category, Key, Default) ->
    case fetch_from_account(Account, Category) of
        {'ok', JObj} -> get_global_from_doc(Category, Key, Default, JObj);
        {'error', 'no_account_id'} -> kapps_config:get(Category, Key, Default);
        {'error', _} -> get_from_reseller(Account, Category, Key, Default)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get global starting from reseller config.
%% i.e. makes sure to skip reading from Account (i.e. sub-account of reseller).
%% @end
%%--------------------------------------------------------------------
-spec get_from_reseller(api_account(), ne_binary(), kz_json:path()) -> kz_json:api_json_term().
get_from_reseller(Account, Category, Key) ->
    get_from_reseller(Account, Category, Key, 'undefined').

-spec get_from_reseller(api_account(), ne_binary(), kz_json:path(), kz_json:api_json_term()) -> kz_json:api_json_term().
-ifdef(TEST).
get_from_reseller(_, _, _, Default) -> Default.
-else.
get_from_reseller(Account, Category, Key, Default) ->
    maybe_get_global_from_reseller(account_id(Account), Category, Key, Default).
-endif.

-spec maybe_get_global_from_reseller(account_or_not(), ne_binary(), kz_json:path(), kz_json:api_json_term()) ->
                                            kz_json:api_json_term().
maybe_get_global_from_reseller('no_account_id', Category, Key, Default) ->
    kapps_config:get(Category, Key, Default);
maybe_get_global_from_reseller(AccountId, Category, Key, Default) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    maybe_get_global_from_reseller(AccountId, ResellerId, Category, Key, Default).

-spec maybe_get_global_from_reseller(ne_binary(), api_binary(), ne_binary(), kz_json:path(), kz_json:api_json_term()) ->
                                            kz_json:api_json_term().
maybe_get_global_from_reseller(_AccountId, 'undefined', Category, Key, Default) ->
    kapps_config:get(Category, Key, Default);
maybe_get_global_from_reseller(AccountId, AccountId, Category, Key, Default) ->
    kapps_config:get(Category, Key, Default);
maybe_get_global_from_reseller(_AccountId, ResellerId, Category, Key, Default) ->
    case fetch_from_account(ResellerId, Category) of
        {'ok', JObj} -> get_global_from_doc(Category, Key, Default, JObj);
        {'error', _} -> kapps_config:get(Category, Key, Default)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get Key's value from account's db document if defined, otherwise get
%% from system_config
%% @end
%%--------------------------------------------------------------------
-spec get_global_from_doc(ne_binary(), kz_json:path(), kz_json:api_json_term(), kz_json:object()) -> kz_json:object().
get_global_from_doc(Category, Key, Default, JObj) ->
    case kz_json:get_value(Key, JObj) of
        'undefined' -> kapps_config:get(Category, Key, Default);
        V -> V
    end.

-spec get(api_account(), ne_binary()) -> kz_json:object().
-spec get(api_account(), ne_binary(), kz_json:path()) -> kz_json:api_json_term().
-spec get(api_account(), ne_binary(), kz_json:path(), Default) -> kz_json:json_term() | Default.

get(Account, Category, Key) ->
    get(Account, Category, Key, 'undefined').

get(Account, Category, Key, Default) ->
    kz_json:get_value(Key, get(Account, Category), Default).

get(Account, Category) ->
    case fetch_from_account(account_id(Account), Category) of
        {'ok', JObj} -> JObj;
        {'error', _} -> kz_doc:set_id(kz_json:new(), config_doc_id(Category))
    end.

-ifdef(TEST).
-spec fetch_from_account(account_or_not(), ne_binary()) ->  kazoo_data:get_results_return().
-else.
fetch_from_account('no_account_id', _Category) ->
    {'error', 'no_account_id'};
fetch_from_account(AccountId, Category) ->
    DocId = config_doc_id(Category),
    AccountDb = kz_util:format_account_db(AccountId),
    kz_datamgr:open_cache_doc(AccountDb, DocId, [{'cache_failures', ['not_found']}]).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the Value for the Key in account db
%% @end
%%--------------------------------------------------------------------
-spec set(api_account(), ne_binary(), kz_json:path(), kz_json:json_term()) -> kz_json:object().
-ifdef(TEST).
set(_, _, Key, Value) ->
    kz_json:set_value(Key, Value, kz_json:new()).
-else.
set(Account, Category, Key, Value) ->
    maybe_set_account(account_id(Account), Category, Key, Value).
-endif.

-spec maybe_set_account(account_or_not(), ne_binary(), kz_json:path(), kz_json:json_term()) -> kz_json:object().
maybe_set_account('no_account_id', _, Key, Value) ->
    kz_json:set_value(Key, Value, kz_json:new());
maybe_set_account(AccountId, Category, Key, Value) ->
    JObj = kz_json:set_value(Key, Value, get(AccountId, Category)),
    JObj1 = update_config_for_saving(AccountId, JObj),
    AccountDb = kz_util:format_account_db(AccountId),
    {'ok', JObj2} = kz_datamgr:ensure_saved(AccountDb, JObj1),
    JObj2.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the Value for the Key in account db if found, otherwise get
%% system_config value then save in account db.
%% @end
%%--------------------------------------------------------------------
-ifdef(TEST).
set_global(_, _, Key, Value) ->
    kz_json:set_value(Key, Value, kz_json:new()).
-else.
-spec set_global(api_account(), ne_binary(), kz_json:path(), kz_json:json_term()) -> kz_json:object().
set_global(Account, Category, Key, Value) ->
    set_account_or_merge_global(account_id(Account), Category, Key, Value).
-endif.

-spec set_account_or_merge_global(account_or_not(), ne_binary(), kz_json:path(), kz_json:json_term()) ->
                                         kz_json:object().
set_account_or_merge_global('no_account_id', _, Key, Value) ->
    kz_json:set_value(Key, Value, kz_json:new());
set_account_or_merge_global(AccountId, Category, Key, Value) ->
    Doc = case fetch_from_account(AccountId, Category) of
              {'ok', JObj} -> JObj;
              {'error', _} -> kz_json:set_value(Key, kapps_config:get(Category, Key), kz_json:new())
          end,
    Doc1 = kz_json:set_value(Key, Value, update_config_for_saving(AccountId, Doc)),

    AccountDb = kz_util:format_account_db(AccountId),
    {'ok', JObj1} = kz_datamgr:ensure_saved(AccountDb, Doc1),
    JObj1.

-spec update_config_for_saving(ne_binary(), kz_json:object()) -> kz_json:object().
update_config_for_saving(AccountId, JObj) ->
    AccountDb = kz_util:format_account_db(AccountId),
    kz_doc:update_pvt_parameters(JObj
                                ,AccountDb
                                ,[{'type', <<"account_config">>}
                                 ,{'account_id', AccountId}
                                 ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flush account's specific config cache
%% @end
%%--------------------------------------------------------------------
-spec flush(ne_binary(), ne_binary()) -> 'ok'.
flush(Account, Category) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    kz_datamgr:flush_cache_doc(AccountDb, config_doc_id(Category)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Give document Id of config's document in account db
%% @end
%%--------------------------------------------------------------------
-spec config_doc_id(ne_binary()) -> ne_binary().
config_doc_id(Category) -> <<(?KZ_ACCOUNT_CONFIGS)/binary, Category/binary>>.


%% ====================================================================
%% Internal functions
%% ====================================================================


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find AccountId from binary, Call object or JObj.
%% @end
%%--------------------------------------------------------------------
-spec account_id(api_account()) -> account_or_not().
account_id(?NE_BINARY=Account) -> kz_util:format_account_id(Account);
account_id('undefined') -> 'no_account_id';
account_id(Obj) -> account_id_from_call(Obj, kapps_call:is_call(Obj)).

-spec account_id_from_call(kapps_call:call() | kz_json:object(), boolean()) -> account_or_not().
account_id_from_call(Call, 'true') -> maybe_format_account_id(kapps_call:account_id(Call));
account_id_from_call(Obj, 'false') -> account_id_from_jobj(Obj, kz_json:is_json_object(Obj)).

-spec account_id_from_jobj(kz_json:object(), 'true') -> account_or_not().
account_id_from_jobj(JObj, 'true') ->
    Paths = [<<"Account-ID">>
            ,<<"account_id">>
            ,<<"Account-DB">>
            ,<<"account_db">>
            ],
    maybe_format_account_id(kz_json:get_first_defined(Paths, JObj));
account_id_from_jobj(_Obj, 'false') ->
    lager:debug("unable to find account id from ~p", [_Obj]),
    'no_account_id'.


-spec maybe_format_account_id(api_ne_binary()) -> account_or_not().
maybe_format_account_id('undefined') -> 'no_account_id';
maybe_format_account_id(Account) -> kz_util:format_account_id(Account).


%% ====================================================================
%% Migrates config settings
%% ====================================================================


-type migrate_setting() :: {ne_binary(), kz_json:path()}.
-type migrate_value() :: {ne_binary(), ne_binary(), kz_json:path(), _}.
-type migrate_values() :: [migrate_value()].

-define(ACCOUNT_CONFIG_MIGRATIONS
       ,[{{<<"callflow">>, <<"default_can_text_self">>}
         ,{<<"kazoo_endpoint">>, <<"default_can_text_self">>}
         }
        ,{{<<"callflow">>, <<"recorder_module">>}
         ,{<<"kazoo_endpoint">>, <<"recorder_module">>}
         }
        ]).

-spec migrate(ne_binary()) -> 'ok'.
migrate(Account) ->
    AccountDb = kz_util:format_account_db(Account),
    _ = [migrate_config_setting(AccountDb, From, To)
         || {From, To} <- ?ACCOUNT_CONFIG_MIGRATIONS
        ],
    'ok'.

-spec migrate_config_setting(ne_binary(), migrate_setting(), migrate_setting()) ->
                                    'ok' | {'error', any()}.
migrate_config_setting(AccountDb, From, To) ->
    case remove_config_setting(AccountDb, From) of
        {'ok', _, []} -> 'ok';
        {'ok', JObj, Removed} ->
            migrate_config_setting(AccountDb, JObj, Removed, To);
        {'error', 'not_found'} -> 'ok';
        {'error', Reason} -> {'error', {'remove', Reason}}
    end.

-spec migrate_config_setting(ne_binary(), kz_json:object(), migrate_values(), migrate_setting()) ->
                                    'ok' | {'error', any()}.
migrate_config_setting(AccountDb, UpdatedFrom, Removed, To) ->
    case add_config_setting(AccountDb, To, Removed) of
        {'ok', UpdatedTo} ->
            {'ok', _} = kz_datamgr:save_doc(AccountDb, UpdatedTo),
            {'ok', _} = kz_datamgr:save_doc(AccountDb, UpdatedFrom),
            'ok';
        {'error', Reason} -> {'error', {'add', Reason}}
    end.

-spec add_config_setting(ne_binary(), migrate_setting(), migrate_values()) ->
                                'ok' | {'error', any()}.
add_config_setting(AccountDb, {Id, Setting}, Values) ->
    add_config_setting(AccountDb, Id, Setting, Values).

-spec add_config_setting(ne_binary(), ne_binary(), kz_json:path(), migrate_values()) ->
                                'ok' | {'error', any()}.
add_config_setting(AccountDb, Id, Setting, Values) when is_binary(Id) ->
    case kz_datamgr:open_doc(AccountDb, Id) of
        {'ok', JObj} -> add_config_setting(JObj, Setting, Values);
        {'error', 'not_found'} ->
            add_config_setting(AccountDb
                              ,update_config_for_saving(AccountDb, kz_doc:set_id(kz_json:new(), Id))
                              ,Setting
                              ,Values
                              );
        {'error', _}=Error -> Error
    end;
add_config_setting(_AccountDb, JObj, _, []) -> {'ok', JObj};
add_config_setting(AccountDb, JObj, ToSetting, [{FromId, Node, FromSetting, Value} | Values]) ->
    ToId  = kz_doc:id(JObj),
    Key = config_setting_key(Node, ToSetting),
    case kz_json:get_value(Key, JObj) of
        'undefined' ->
            io:format("migrating setting in ~s from ~s ~s.~s to ~s ~s.~s value ~p~n"
                     ,[AccountDb
                      ,FromId, Node, FromSetting
                      ,ToId, Node, ToSetting
                      ,Value
                      ]
                     ),
            add_config_setting(AccountDb
                              ,kz_json:set_value(Key, Value, JObj)
                              ,ToSetting
                              ,Values
                              );
        Value -> add_config_setting(AccountDb, JObj, ToSetting, Values);
        _Else ->
            io:format("the system tried to move the parameter listed below"
                      " but found a different setting already there,"
                      " you need to correct this disparity manually!~n"),
            io:format("  Source~n    db: ~s~n    id: ~s~n    key: ~s ~s~n    value: ~p~n"
                     ,[AccountDb, FromId, Node, FromSetting, Value]),
            io:format("  Destination~n    db: ~s~n    id: ~s~n    key: ~s ~s~n    value: ~p~n"
                     ,[AccountDb, ToId, Node, ToSetting, _Else]),
            {'error', 'disparity'}
    end.

-spec remove_config_setting(ne_binary(), migrate_setting()) ->
                                   {'ok', kz_json:object(), migrate_values()} |
                                   {'error', any()}.
remove_config_setting(AccountDb, {Id, Setting}) ->
    remove_config_setting(AccountDb, Id, Setting).

-spec remove_config_setting(ne_binary(), ne_binary() | kz_json:object(), kz_json:path()) ->
                                   {'ok', kz_json:object(), migrate_values()} |
                                   {'error', any()}.
remove_config_setting(AccountDb, Id, Setting) when is_binary(Id) ->
    case kz_datamgr:open_doc(AccountDb, Id) of
        {'ok', JObj} -> remove_config_setting(AccountDb, JObj, Setting);
        {'error', _}=Error -> Error
    end;
remove_config_setting(AccountDb, JObj, Setting) ->
    Id = kz_doc:id(JObj),
    Keys =
        [{Id, Node, Setting}
         || Node <- kz_doc:get_public_keys(JObj)
        ],
    remove_config_setting(AccountDb, Keys, JObj, []).

-spec remove_config_setting(ne_binary(), [{ne_binary(), ne_binary(), kz_json:path()}], kz_json:object(), migrate_values()) ->
                                   {'ok', kz_json:object(), migrate_values()}.
remove_config_setting(_AccountDb, [], JObj, Removed) ->
    {'ok', JObj, Removed};
remove_config_setting(AccountDb, [{Id, Node, Setting} | Keys], JObj, Removed) ->
    Key = config_setting_key(Node, Setting),
    case kz_json:get_value(Key, JObj) of
        'undefined' -> remove_config_setting(AccountDb, Keys, JObj, Removed);
        Value ->
            remove_config_setting(AccountDb
                                 ,Keys
                                 ,kz_json:delete_key(Key, JObj)
                                 ,[{Id, Node, Setting, Value} | Removed]
                                 )
    end.

-spec config_setting_key(ne_binary(), kz_json:path()) -> ne_binaries().
%% NOTE: to support nested keys, update this merge function
config_setting_key(Node, Setting) ->
    [Node, Setting].
