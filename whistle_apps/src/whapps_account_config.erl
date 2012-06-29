%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whapps_account_config).

-include("whistle_apps.hrl").

-export([get/2, get/3, get/4]).

-spec get/2 :: (ne_binary(), ne_binary()) -> wh_json:json_object().
get(Account, Config) ->
    AccountId = wh_util:format_account_id(Account, raw),
    case wh_cache:peek_local(?WHAPPS_CONFIG_CACHE, {?MODULE, Config, AccountId}) of
        {ok, JObj} -> JObj;
        {error, not_found} ->
            AccountDb = wh_util:format_account_id(Account, encoded),
            case couch_mgr:open_cache_doc(AccountDb, <<(?WH_ACCOUNT_CONFIGS)/binary, Config/binary>>) of
                {error, _} -> wh_json:new();
                {ok, JObj} ->
                    wh_cache:store({?MODULE, Config, AccountId}, JObj),
                    JObj
            end
    end.

-spec get/3 :: (ne_binary(), ne_binary(), wh_json:json_string() | wh_json:json_strings()) -> wh_json:json_term() | 'undefined'.
-spec get/4 :: (ne_binary(), ne_binary(), wh_json:json_string() | wh_json:json_strings(), Default) -> wh_json:json_term() | Default.
get(Account, Config, Key) ->
    get(Account, Config, Key, undefined).
get(Account, Config, Key, Default) ->
    wh_json:get_value(Key, get(Account, Config), Default).
