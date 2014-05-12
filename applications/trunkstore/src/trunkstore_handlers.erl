%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(trunkstore_handlers).

-export([handle_config_change/2]).

-include("ts.hrl").

-spec handle_config_change(wh_json:object(), any()) -> any().
handle_config_change(JObj, _Props) ->
    lager:info("Trunkstore doc change detected: ~p", [JObj]),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Flush = wh_cache:filter(fun(Key, _Value) -> is_ts_cache_object(Key, AccountId) end),
    [wh_cache:erase(Key) || {Key, _Value} <- Flush].

-spec is_ts_cache_object(tuple(), ne_binary()) -> boolean().
is_ts_cache_object({'lookup_user_flags', _Realm, _User, AccountId}, AccountId) ->
    'true';
is_ts_cache_object({'lookup_did', _DID, AccountId}, AccountId) ->
    'true';
is_ts_cache_object(_Key, _AccountId) -> 'false'.
