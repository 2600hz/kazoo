%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
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
-spec get_global(account(), ne_binary(), wh_json:key()) ->
                        wh_json:json_term().
-spec get_global(account(), ne_binary(), wh_json:key(), wh_json:json_term()) ->
                        wh_json:json_term().
get_global(Account, Category, Key) ->
    get_global(Account, Category, Key, 'undefined').
get_global(Account, Category, Key, Default) ->
    AccountId = account_id(Account),
    case get_global_from_account(AccountId, Category, Key, Default) of
        {'ok', JObj} -> get_global_from_doc(Category, Key, Default, JObj);
        {'error', _} -> maybe_get_global_from_reseller(AccountId, Category, Key, Default)
    end.

-spec maybe_get_global_from_reseller(account(), ne_binary(), wh_json:key(), wh_json:json_term()) ->
                        wh_json:json_term().
maybe_get_global_from_reseller(Account, Category, Key, Default) ->
    AccountId = account_id(Account),
    ResellerId = wh_services:find_reseller_id(AccountId),
    maybe_get_global_from_reseller(AccountId, ResellerId, Category, Key, Default).

-spec maybe_get_global_from_reseller(account(), account(), ne_binary(), wh_json:key(), wh_json:json_term()) ->
                        wh_json:json_term().
maybe_get_global_from_reseller(AccountId, AccountId, Category, Key, Default) ->
    whapps_config:get(Category, Key, Default);
maybe_get_global_from_reseller(_AccountId, ResellerId, Category, Key, Default) ->
    case get_global_from_account(ResellerId, Category, Key, Default) of
        {'ok', JObj} -> get_global_from_doc(Category, Key, Default, JObj);
        {'error', _} -> whapps_config:get(Category, Key, Default)
    end.

-spec get_global_from_account(account(), ne_binary(), wh_json:key(), wh_json:json_term()) ->
                                     {'ok', wh_json:object()} |
                                     {'error', _}.
get_global_from_account(Account, Category, _Key, _Default) ->
    AccountId = account_id(Account),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    couch_mgr:open_cache_doc(AccountDb, config_doc_id(Category), [{'cache_failures', ['not_found']}]).

-spec get_global_from_doc(ne_binary(), wh_json:key(), wh_json:json_term(), wh_json:object()) ->
                                 wh_json:object().
get_global_from_doc(Category, Key, Default, JObj) ->
    case wh_json:get_value(Key, JObj) of
        'undefined' -> whapps_config:get(Category, Key, Default);
        V -> V
    end.

-spec get(account(), ne_binary()) -> wh_json:object().
get(Account, Config) ->
    AccountId = account_id(Account),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    DocId = config_doc_id(Config),
    case couch_mgr:open_cache_doc(AccountDb, DocId, [{'cache_failures', ['not_found']}]) of
        {'error', _} -> wh_doc:set_id(wh_json:new(), DocId);
        {'ok', JObj} -> JObj
    end.

-spec flush(account()) -> 'ok'.
-spec flush(account(), ne_binary()) -> 'ok'.
flush(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    couch_mgr:flush_cache_docs(AccountDb).

flush(Account, Config) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    couch_mgr:flush_cache_doc(AccountDb, config_doc_id(Config)).

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

    AccountDb = account_db(Account),
    {'ok', JObj1} = couch_mgr:ensure_saved(AccountDb
                                           ,wh_doc:update_pvt_parameters(JObj
                                                                         ,AccountDb
                                                                         ,[{'type', <<"account_config">>}
                                                                           ,{'account_id', account_id(Account)}
                                                                          ])
                                          ),
    JObj1.

-spec set_global(account(), ne_binary(), wh_json:key(), wh_json:json_term()) ->
                        wh_json:object().
set_global(Account, Category, Key, Value) ->
    AccountId = account_id(Account),
    AccountDb = account_db(Account),

    Doc = case couch_mgr:open_cache_doc(AccountDb, Category, [{'cache_failures', ['not_found']}]) of
              {'ok', JObj} -> JObj;
              {'error', _} -> wh_json:set_value(Key, whapps_config:get(Category, Key), wh_json:new())
          end,

    Doc1 = wh_json:set_value(Key
                             ,Value
                             ,wh_doc:update_pvt_parameters(Doc, AccountDb, [{'type', <<"account_config">>}])
                            ),

    {'ok', JObj1} = couch_mgr:ensure_saved(AccountDb, Doc1),
    wh_cache:erase_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Category)),
    JObj1.

-spec config_doc_id(ne_binary()) -> ne_binary().
config_doc_id(Config) -> <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>.

-spec cache_key(ne_binary(), ne_binary()) -> {?MODULE, ne_binary(), ne_binary()}.
cache_key(AccountId, Config) -> {?MODULE, Config, AccountId}.

-spec account_id(account()) -> ne_binary().
account_id(Account) when is_binary(Account) ->
    wh_util:format_account_id(Account, 'raw');
account_id(Obj) ->
    account_id_from_call(Obj, whapps_call:is_call(Obj)).

-spec account_id_from_call(whapps_call:call() | wh_json:object(), boolean()) -> ne_binary().
account_id_from_call(Call, 'true') ->
    whapps_call:account_id(Call);
account_id_from_call(Obj, 'false') ->
    account_id_from_jobj(Obj, wh_json:is_json_object(Obj)).

-spec account_id_from_jobj(wh_json:object(), 'true') -> ne_binary().
account_id_from_jobj(JObj, 'true') ->
    wh_json:get_first_defined([<<"Account-ID">>, <<"account_id">>], JObj);
account_id_from_jobj(_Obj, 'false') ->
    lager:debug("unable to find account id from ~p", [_Obj]),
    throw({'error', 'unknown_object'}).

-spec account_db(account()) -> ne_binary().
account_db(Account) when is_binary(Account) ->
    wh_util:format_account_id(Account, 'encoded');
account_db(Obj) ->
    account_db_from_call(Obj, whapps_call:is_call(Obj)).

-spec account_db_from_call(whapps_call:call() | wh_json:object(), boolean()) -> ne_binary().
account_db_from_call(Call, 'true') ->
    whapps_call:account_db(Call);
account_db_from_call(Obj, 'false') ->
    account_db_from_jobj(Obj, wh_json:is_json_object(Obj)).

-spec account_db_from_jobj(wh_json:object(), 'true') -> ne_binary().
account_db_from_jobj(JObj, 'true') ->
    wh_json:get_first_defined([<<"Account-DB">>, <<"account_db">>], JObj);
account_db_from_jobj(_Obj, 'false') ->
    lager:debug("unable to find account db from ~p", [_Obj]),
    throw({'error', 'unknown_object'}).
