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

-export([start_link/0]).
-export([load_config/1, write_config/1]).
-export([fetch/1, fetch/2]).
-export([store/2, store/3]).

-include("wh_couch.hrl").

-define(CONFIG_CAT, <<"whistle_couch">>).

-spec start_link/0 :: () -> 'ignore'.
start_link() ->
    ?LOG("loading couch configs"),
    _ = load_config(?CONFIG_FILE_PATH),
    ?LOG("loaded couch configs"),
    ignore.

-spec load_config/1 :: (file:name()) -> 'ok' | {'error', 'enoent'}.
load_config(Path) ->
    ?LOG("Loading ~s", [Path]),
    case file:consult(Path) of
	{ok, Startup} ->
	    _ = [cache_from_file(T) || T <- Startup],
	    ok;
	{error, enoent} ->
	    ?LOG("no file"),
	    {error, enoent}
    end.

%% convert 3..n-tuples to 2 tuples with the value being (3..n)-1 tuples
%% so {key, v1, v2, v3} becomes {key, {v1, v2, v3}}
%% subsequent writes back to the file will store in the new format
cache_from_file({Key, Value}) ->
    ?LOG("store ~p:~p", [Key, Value]),
    store(Key, Value);
cache_from_file(T) when is_tuple(T) ->
    [Key|V] = erlang:tuple_to_list(T),
    Value = erlang:list_to_tuple(V),
    ?LOG("store ~p:~p", [Key, Value]),
    store(Key, Value).

-spec write_config/1 :: (file:name()) -> 'ok' | {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
write_config(Path) ->
    ?LOG_SYS("writing cached config to ~s", [Path]),
    Contents = lists:foldl(fun(I, Acc) -> [io_lib:format("~p.~n", [I]) | Acc] end
			   , "", whapps_config:get_all_kvs(?CONFIG_CAT)),
    ?LOG("Writing to config: ~s", [Contents]),
    file:write_file(Path, Contents).

fetch(Key) ->
    fetch(Key, undefined).

fetch(Key, Cache) when is_pid(Cache) ->
    fetch(Key, undefined, Cache);
fetch(Key, Default) ->
    ?LOG("w_c: fetching ~s(~s): ~p", [?CONFIG_CAT, Key, Default]),
    whapps_config:get(?CONFIG_CAT, Key, Default).

fetch(Key, Default, Cache) ->
    case wh_cache:fetch_local(Cache, {?MODULE, Key}) of
	{error, not_found} -> Default;
	{ok, V} -> V
    end.

-spec store/2 :: (term(), term()) -> 'ok'.
store(Key, Value) ->
    whapps_config:set(?CONFIG_CAT, Key, Value).

store(Key, Value, Cache) when is_pid(Cache) ->
    wh_cache:store_local(Cache, {?MODULE, Key}, Value, ?MILLISECONDS_IN_DAY).
