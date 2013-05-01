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

-include("whistle_apps.hrl").

-export([get/2, get/3, get/4
         ,set/4
         ,flush/1, flush/2
        ]).

-spec get(ne_binary(), ne_binary()) -> wh_json:object().
get(Account, Config) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case wh_cache:peek_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Config)) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} ->
            AccountDb = wh_util:format_account_id(Account, 'encoded'),
            DocId = config_doc_id(Config),
            case couch_mgr:open_doc(AccountDb, DocId) of
                {'error', _} -> wh_json:set_value(<<"_id">>, DocId, wh_json:new());
                {'ok', JObj} ->
                    wh_cache:store_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Config), JObj),
                    JObj
            end
    end.

-spec flush(ne_binary()) -> 'ok'.
-spec flush(ne_binary(), ne_binary()) -> 'ok'.
flush(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    _ = wh_cache:filter_local(?WHAPPS_CONFIG_CACHE
                              ,fun({?MODULE, _Config, AcctId}=K, _V) when AcctId =:= AccountId ->
                                       wh_cache:erase_local(?WHAPPS_CONFIG_CACHE, K),
                                       'true';
                                  (_, _) -> 'false'
                               end
                             ),
    'ok'.
flush(Account, Config) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    wh_cache:erase_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Config)).

-spec get(ne_binary(), ne_binary(), wh_json:key()) ->
                 wh_json:json_term() | 'undefined'.
-spec get(ne_binary(), ne_binary(), wh_json:key(), Default) ->
                 wh_json:json_term() | Default.
get(Account, Config, Key) ->
    get(Account, Config, Key, 'undefined').
get(Account, Config, Key, Default) ->
    wh_json:get_value(Key, get(Account, Config), Default).

-spec set(ne_binary(), ne_binary(), wh_json:key(), wh_json:json_term()) ->
                 wh_json:object().
set(Account, Config, Key, Value) ->
    JObj = wh_json:set_value(Key, Value, ?MODULE:get(Account, Config)),

    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),

    {'ok', JObj1} = couch_mgr:ensure_saved(AccountDb
                                           ,wh_doc:update_pvt_parameters(JObj, AccountDb, [{'type', <<"account_config">>}])
                                          ),
    wh_cache:erase_local(?WHAPPS_CONFIG_CACHE, cache_key(AccountId, Config)),
    JObj1.

config_doc_id(Config) -> <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>.

cache_key(AccountId, Config) -> {?MODULE, Config, AccountId}.
    
