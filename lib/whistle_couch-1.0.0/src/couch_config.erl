%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Access and cache config options for Whistle Couch
%%% It is important to realize that configs are stored
%%% with the key as part of the value.
%%%
%%% So {foo, bar, baz} is internally stored as:
%%% { foo, {foo, bar, baz} }
%%% fetch/1,2,3 will use foo as the key, and return the full tuple.
%%% @end
%%% Created : 28 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(couch_config).

-export([start_link/0, ready/0]).
-export([load_config/1]).
-export([fetch/1, fetch/2]).
-export([store/2, store/3]).

-include("wh_couch.hrl").

-define(CONFIG_CAT, <<"whistle_couch">>).

-spec start_link/0 :: () -> 'ignore'.
start_link() ->
    _ = load_config(?CONFIG_FILE_PATH),
    ?LOG("loaded couch configs"),
    ignore.

-spec load_config/1 :: (file:name()) -> 'ok' | {'error', 'enoent'}.
load_config(Path) ->
    ?LOG("loading ~s", [Path]),
    case file:consult(Path) of
        {ok, Startup} ->
            _ = [cache_from_file(T) || T <- Startup],
            ok;
        {error, enoent}=E ->
            E
    end.

%% convert 3..n-tuples to 2 tuples with the value being (3..n)-1 tuples
%% so {key, v1, v2, v3} becomes {key, {v1, v2, v3}}
%% subsequent writes back to the file will store in the new format
cache_from_file({Key, Value}) ->
    store(Key, Value);
cache_from_file(T) when is_tuple(T) ->
    [Key|V] = erlang:tuple_to_list(T),
    Value = erlang:list_to_tuple(V),
    store(Key, Value).

ready() ->
    whapps_config:couch_ready().

fetch(Key) ->
    fetch(Key, undefined).

fetch(Key, Cache) when is_pid(Cache) ->
    fetch(Key, undefined, Cache);
fetch(Key, Default) ->
    whapps_config:get(?CONFIG_CAT, Key, Default).

fetch(Key, Default, Cache) ->
    case wh_cache:fetch_local(Cache, {?MODULE, Key}) of
        {error, not_found} -> Default;
        {ok, V} -> V
    end.

-spec store/2 :: (term(), term()) -> 'ok'.
store(Key, Value) ->
    whapps_config:set(?CONFIG_CAT, wh_util:to_binary(Key), Value).

-spec store/3 :: (term(), term(), pid()) -> 'ok'.
store(Key, Value, Cache) when is_pid(Cache) ->
    wh_cache:store_local(Cache, {?MODULE, Key}, Value, ?MILLISECONDS_IN_DAY).
