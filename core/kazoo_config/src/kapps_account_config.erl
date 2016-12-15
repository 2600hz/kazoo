%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
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
        ,get_global/3, get_global/4
        ,get_from_reseller/3, get_from_reseller/4
        ,set/4
        ,set_global/4
        ,flush/1, flush/2
        ,migrate/1
        ]).

-type account() :: ne_binary() | kapps_call:call() | kz_json:object().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Will search the account db first, then system_config for values.
%% @end
%%--------------------------------------------------------------------
-spec get_global(account(), ne_binary(), kz_json:path()) ->
                        kz_json:json_term().
-spec get_global(account(), ne_binary(), kz_json:path(), kz_json:api_json_term()) ->
                        kz_json:json_term().
get_global(Account, Category, Key) ->
    get_global(Account, Category, Key, 'undefined').

get_global(Account, Category, Key, Default) ->
    AccountId = account_id(Account),
    case get_global_from_account(AccountId, Category, Key, Default) of
        {'ok', JObj} -> get_global_from_doc(Category, Key, Default, JObj);
        {'error', _} -> get_from_reseller(AccountId, Category, Key, Default)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get global starting from reseller config.
%% i.e. makes sure to skip reading from Account (i.e. sub-account of reseller).
%% @end
%%--------------------------------------------------------------------
-spec get_from_reseller(account(), ne_binary(), kz_json:path()) ->
                               kz_json:api_json_term().
get_from_reseller(Account, Category, Key) ->
    get_from_reseller(Account, Category, Key, 'undefined').

-spec get_from_reseller(account(), ne_binary(), kz_json:path(), kz_json:api_json_term()) ->
                               kz_json:api_json_term().
-ifdef(TEST).
get_from_reseller(_, _, _, Default) -> Default.
-else.
get_from_reseller(Account, Category, Key, Default) ->
    AccountId = account_id(Account),
    ResellerId = kz_services:find_reseller_id(AccountId),
    maybe_get_global_from_reseller(AccountId, ResellerId, Category, Key, Default).

-spec maybe_get_global_from_reseller(account(), account(), ne_binary(), kz_json:path(), kz_json:api_json_term()) ->
                                            kz_json:api_json_term().
maybe_get_global_from_reseller(AccountId, AccountId, Category, Key, Default) ->
    kapps_config:get(Category, Key, Default);
maybe_get_global_from_reseller(_AccountId, ResellerId, Category, Key, Default) ->
    case get_global_from_account(ResellerId, Category, Key, Default) of
        {'ok', JObj} -> get_global_from_doc(Category, Key, Default, JObj);
        {'error', _} -> kapps_config:get(Category, Key, Default)
    end.
-endif.

-spec get_global_from_account(account(), ne_binary(), kz_json:path(), kz_json:api_json_term()) ->
                                     {'ok', kz_json:object()} |
                                     {'error', any()}.
get_global_from_account(Account, Category, _Key, _Default) ->
    AccountId = account_id(Account),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    kz_datamgr:open_cache_doc(AccountDb, config_doc_id(Category), [{'cache_failures', ['not_found']}]).

-spec get_global_from_doc(ne_binary(), kz_json:path(), kz_json:api_json_term(), kz_json:object()) ->
                                 kz_json:object().
get_global_from_doc(Category, Key, Default, JObj) ->
    case kz_json:get_value(Key, JObj) of
        'undefined' -> kapps_config:get(Category, Key, Default);
        V -> V
    end.

-spec get(account(), ne_binary()) -> kz_json:object().
get(Account, Config) ->
    AccountId = account_id(Account),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    DocId = config_doc_id(Config),
    case kz_datamgr:open_cache_doc(AccountDb, DocId, [{'cache_failures', ['not_found']}]) of
        {'error', _} -> kz_doc:set_id(kz_json:new(), DocId);
        {'ok', JObj} -> JObj
    end.

-spec flush(account()) -> 'ok'.
-spec flush(account(), ne_binary()) -> 'ok'.
flush(Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    kz_datamgr:flush_cache_docs(AccountDb).

flush(Account, Config) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    kz_datamgr:flush_cache_doc(AccountDb, config_doc_id(Config)).

-spec get(account(), ne_binary(), kz_json:path()) -> kz_json:api_json_term().
-spec get(account(), ne_binary(), kz_json:path(), Default) ->
                 kz_json:json_term() | Default.
-ifdef(TEST).
get(_, _, _) -> 'undefined'.
get(_, _, _, Default) -> Default.
-else.
get(Account, Config, Key) ->
    get(Account, Config, Key, 'undefined').
get(Account, Config, Key, Default) ->
    kz_json:get_value(Key, get(Account, Config), Default).
-endif.

-spec set(account(), ne_binary(), kz_json:path(), kz_json:json_term()) ->
                 kz_json:object().
set(Account, Config, Key, Value) ->
    JObj = kz_json:set_value(Key, Value, get(Account, Config)),

    AccountDb = account_db(Account),
    {'ok', JObj1} = kz_datamgr:ensure_saved(AccountDb
                                           ,update_config_for_saving(AccountDb, JObj)
                                           ),
    JObj1.

-spec update_config_for_saving(ne_binary(), kz_json:object()) -> kz_json:object().
update_config_for_saving(AccountDb, JObj) ->
    kz_doc:update_pvt_parameters(JObj
                                ,AccountDb
                                ,[{'type', <<"account_config">>}
                                 ,{'account_id', account_id(AccountDb)}
                                 ]).

-spec set_global(account(), ne_binary(), kz_json:path(), kz_json:json_term()) ->
                        kz_json:object().
set_global(Account, Category, Key, Value) ->
    AccountId = account_id(Account),
    AccountDb = account_db(Account),

    Doc = case kz_datamgr:open_cache_doc(AccountDb, Category, [{'cache_failures', ['not_found']}]) of
              {'ok', JObj} -> JObj;
              {'error', _} -> kz_json:set_value(Key, kapps_config:get(Category, Key), kz_json:new())
          end,

    Doc1 = kz_json:set_value(Key
                            ,Value
                            ,update_config_for_saving(AccountDb, Doc)
                            ),

    {'ok', JObj1} = kz_datamgr:ensure_saved(AccountDb, Doc1),
    kz_cache:erase_local(?KAPPS_CONFIG_CACHE, cache_key(AccountId, Category)),
    JObj1.

-spec config_doc_id(ne_binary()) -> ne_binary().
config_doc_id(Config) -> <<(?KZ_ACCOUNT_CONFIGS)/binary, Config/binary>>.

-spec cache_key(ne_binary(), ne_binary()) -> {?MODULE, ne_binary(), ne_binary()}.
cache_key(AccountId, Config) -> {?MODULE, Config, AccountId}.

-spec account_id(account()) -> ne_binary().
account_id(Account) when is_binary(Account) ->
    kz_util:format_account_id(Account, 'raw');
account_id(Obj) ->
    account_id_from_call(Obj, kapps_call:is_call(Obj)).

-spec account_id_from_call(kapps_call:call() | kz_json:object(), boolean()) -> ne_binary().
account_id_from_call(Call, 'true') ->
    kapps_call:account_id(Call);
account_id_from_call(Obj, 'false') ->
    account_id_from_jobj(Obj, kz_json:is_json_object(Obj)).

-spec account_id_from_jobj(kz_json:object(), 'true') -> ne_binary().
account_id_from_jobj(JObj, 'true') ->
    kz_json:get_first_defined([<<"Account-ID">>, <<"account_id">>], JObj);
account_id_from_jobj(_Obj, 'false') ->
    lager:debug("unable to find account id from ~p", [_Obj]),
    throw({'error', 'unknown_object'}).

-spec account_db(account()) -> ne_binary().
account_db(Account) when is_binary(Account) ->
    kz_util:format_account_id(Account, 'encoded');
account_db(Obj) ->
    account_db_from_call(Obj, kapps_call:is_call(Obj)).

-spec account_db_from_call(kapps_call:call() | kz_json:object(), boolean()) -> ne_binary().
account_db_from_call(Call, 'true') ->
    kapps_call:account_db(Call);
account_db_from_call(Obj, 'false') ->
    account_db_from_jobj(Obj, kz_json:is_json_object(Obj)).

-spec account_db_from_jobj(kz_json:object(), 'true') -> ne_binary().
account_db_from_jobj(JObj, 'true') ->
    kz_json:get_first_defined([<<"Account-DB">>, <<"account_db">>], JObj);
account_db_from_jobj(_Obj, 'false') ->
    lager:debug("unable to find account db from ~p", [_Obj]),
    throw({'error', 'unknown_object'}).

%% Migrates config settings
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
         || Node <- kz_json:get_public_keys(JObj)
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
