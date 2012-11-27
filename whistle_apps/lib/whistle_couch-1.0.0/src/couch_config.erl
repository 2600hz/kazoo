%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Access and cache config options for Whistle Couch
%%% It is important to realize that configs are stored
%%% with the key as part of the value.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(couch_config).

-export([start_link/0, ready/0]).
-export([load_config/1]).
-export([fetch/1, fetch/2, fetch/3]).
-export([store/2, store/3]).

-include_lib("whistle_couch/include/wh_couch.hrl").

-define(CONFIG_CAT, <<"whistle_couch">>).

-spec start_link/0 :: () -> 'ignore'.
start_link() ->
    put(callid, ?LOG_SYSTEM_ID),
    {ok, _} = load_config(?CONFIG_FILE_PATH),
    lager:debug("loaded couch configs"),
    ignore.

-spec load_config/1 :: (file:name()) -> {'ok', wh_json:json_object()} |
                                        {'error', 'enoent'}.
load_config(Path) ->
    case file:consult(Path) of
        {ok, Startup} ->
            lager:info("loading ~s successful", [Path]),
            _ = store(<<"couch_host">>, props:get_value(couch_host, Startup)),
            store(<<"default_couch_host">>, props:get_value(default_couch_host, Startup));
        {error, enoent}=E ->
            lager:error("loading ~s failed: file is missing", [Path]),
            E;
        {error, _E}=E ->
            lager:error("loading ~s failed: ~p", [Path, _E]),
            E
    end.

ready() ->
    whapps_config:flush(?CONFIG_CAT),
    whapps_config:couch_ready().

fetch(Key) ->
    fetch(Key, undefined).

fetch(Key, Default) ->
    whapps_config:get(?CONFIG_CAT, Key, Default).

fetch(Key, Default, Cache) ->
    case wh_cache:fetch_local(Cache, {?MODULE, Key}) of
        {error, not_found} -> Default;
        {ok, V} -> V
    end.

-spec store/2 :: (term(), term()) -> {'ok', wh_json:json_object()}.
store(Key, Value) ->
    whapps_config:set(?CONFIG_CAT, wh_util:to_binary(Key), Value).

-spec store/3 :: (term(), term(), atom()) -> 'ok'.
store(Key, Value, Cache) ->
    wh_cache:store_local(Cache, {?MODULE, wh_util:to_binary(Key)}, Value, ?MILLISECONDS_IN_DAY).
