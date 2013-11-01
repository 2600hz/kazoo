%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whapps_account_config).

-include("whistle_config.hrl").

-export([get/2, get/3, get/4
         ,get_global/3, get_global/4
         ,set/4
         ,set_global/4
         ,flush/1, flush/2
        ]).

-type account() :: ne_binary() | whapps_call:call() | wh_json:object().

%% get_global/{3,4} will search the account db first, then system_config for values
-spec get_global(account(), ne_binary(), ne_binary()) ->
                        wh_json:json_term().
-spec get_global(account(), ne_binary(), ne_binary(), wh_json:json_term()) ->
                        wh_json:json_term().
get_global(Account, Category, Key) ->
    get_global(Account, Category, Key, 'undefined').
get_global(Account, Category, Key, Default) ->
    AccountId = account_id(Account),
    case wh_cache:peek_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Category)) of
        {'ok', JObj} -> get_global_from_doc(AccountId, Category, Key, Default, JObj);
        {'error', 'not_found'} -> get_global_from_db(AccountId, Category, Key, Default)
    end.

get_global_from_db(AccountId, Category, Key, Default) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, Category) of
        {'ok', JObj} -> get_global_from_doc(AccountId, Category, Key, Default, JObj);
        {'error', _} -> whapps_config:get(Category, Key, Default)
    end.

get_global_from_doc(AccountId, Category, Key, Default, JObj) ->
    case wh_json:get_value(Key, JObj) of
        'undefined' -> whapps_config:get(Category, Key, Default);
        V ->
            wh_cache:store_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Category), JObj),
            V
    end.

-spec get(account(), ne_binary()) -> wh_json:object().
get(Account, Config) ->
    AccountId = account_id(Account),
    case wh_cache:peek_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Config)) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} -> get_from_db(AccountId, Config)
    end.

get_from_db(AccountId, Config) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    DocId = config_doc_id(Config),
    case couch_mgr:open_doc(AccountDb, DocId) of
        {'error', _} -> wh_json:set_value(<<"_id">>, DocId, wh_json:new());
        {'ok', JObj} ->
            wh_cache:store_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Config), JObj),
            JObj
    end.

-spec flush(account()) -> 'ok'.
-spec flush(account(), ne_binary()) -> 'ok'.
flush(Account) ->
    AccountId = account_id(Account),
    _ = wh_cache:filter_local(?WHAPPS_CONFIG_CACHE
                              ,fun({?MODULE, _Config, AcctId}=K, _V) when AcctId =:= AccountId ->
                                       wh_cache:erase_local(?WHAPPS_CONFIG_CACHE, K),
                                       'true';
                                  (_, _) -> 'false'
                               end
                             ),
    'ok'.
flush(Account, Config) ->
    AccountId = account_id(Account),
    wh_cache:erase_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Config)).

-spec get(account(), ne_binary(), wh_json:key()) ->
                 wh_json:json_term() | 'undefined'.
-spec get(account(), ne_binary(), wh_json:key(), Default) ->
                 wh_json:json_term() | Default.
get(Account, Config, Key) ->
    get(Account, Config, Key, 'undefined').
get(Account, Config, Key, Default) ->
    wh_json:get_value(Key, get(Account, Config), Default).

-spec set(account(), ne_binary(), wh_json:key(), wh_json:json_term()) ->
                 wh_json:object().
set(Account, Config, Key, Value) ->
    JObj = wh_json:set_value(Key, Value, ?MODULE:get(Account, Config)),

    AccountId = account_id(Account),
    AccountDb = account_db(Account),

    {'ok', JObj1} = couch_mgr:ensure_saved(AccountDb
                                           ,wh_doc:update_pvt_parameters(JObj, AccountDb, [{'type', <<"account_config">>}])
                                          ),
    wh_cache:erase_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Config)),
    JObj1.

-spec set_global(account(), ne_binary(), wh_json:key(), wh_json:json_term()) ->
                        wh_json:object().
set_global(Account, Category, Key, Value) ->
    AccountId = account_id(Account),
    AccountDb = account_db(Account),

    Doc = case couch_mgr:open_doc(AccountDb, Category) of
              {'ok', JObj} -> JObj;
              {'error', _} -> wh_json:set_value(Key, whapps_config:get(Category, Key), wh_json:new())
          end,

    Doc1 = wh_json:set_value(Key, Value, wh_doc:update_pvt_parameters(Doc, AccountDb, [{'type', <<"account_config">>}])),

    {'ok', JObj1} = couch_mgr:ensure_saved(AccountDb, Category, Doc1),
    wh_cache:erase_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Category)),
    JObj1.

config_doc_id(Config) -> <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>.

-spec cache_key(ne_binary(), ne_binary()) -> {?MODULE, ne_binary(), ne_binary()}.
cache_key(AccountId, Config) -> {?MODULE, Config, AccountId}.

account_id(Account) when is_binary(Account) ->
    wh_util:format_account_id(Account, 'raw');
account_id(Obj) ->
    account_id_from_call(Obj, whapps_call:is_call(Obj)).
account_id_from_call(Call, 'true') ->
    whapps_call:account_id(Call);
account_id_from_call(Obj, 'false') ->
    account_id_from_jobj(Obj, wh_json:is_json_object(Obj)).
account_id_from_jobj(JObj, 'true') ->
    case wh_json:get_value(<<"Account-ID">>, JObj) of
        'undefined' -> wh_json:get_value(<<"account_id">>, JObj);
        AcctId -> AcctId
    end;
account_id_from_jobj(_Obj, 'false') ->
    lager:debug("unable to find account id from ~p", [_Obj]),
    throw({'error', 'unknown_object'}).

account_db(Account) when is_binary(Account) ->
    wh_util:format_account_id(Account, 'encoded');
account_db(Obj) ->
    account_db_from_call(Obj, whapps_call:is_call(Obj)).
account_db_from_call(Call, 'true') ->
    whapps_call:account_db(Call);
account_db_from_call(Obj, 'false') ->
    account_db_from_jobj(Obj, wh_json:is_json_object(Obj)).
account_db_from_jobj(JObj, 'true') ->
    case wh_json:get_value(<<"Account-DB">>, JObj) of
        'undefined' -> wh_json:get_value(<<"account_db">>, JObj);
        AcctDb -> AcctDb
    end;
account_db_from_jobj(_Obj, 'false') ->
    lager:dxebug("unable to find account db from ~p", [_Obj]),
    throw({'error', 'unknown_object'}).
