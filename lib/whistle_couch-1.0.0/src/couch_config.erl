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

-export([load_config/1, load_config/2, write_config/1, write_config/2]).
-export([fetch/1, fetch/2, fetch/3]).
-export([store/2, store/3, store/4]).

-include("wh_couch.hrl").

-spec load_config/1 :: (file:name()) -> 'ok' | {'error', 'enoent'}.
load_config(Path) ->
    {ok, Cache} = whistle_couch_sup:cache_proc(),
    load_config(Path, Cache).

-spec load_config/2 :: (file:name(), pid()) -> 'ok' | {'error', 'enoent'}.
load_config(Path, Cache) when is_pid(Cache) ->
    ?LOG("Loading ~s", [Path]),
    case file:consult(Path) of
	{ok, Startup} ->
	    _ = [cache_from_file(Cache, T) || T <- Startup],
	    ok;
	{error, enoent} ->
	    ?LOG("No file"),
	    {error, enoent}
    end.

%% convert 3..n-tuples to 2 tuples with the value being (3..n)-1 tuples
%% so {key, v1, v2, v3} becomes {key, {v1, v2, v3}}
%% subsequent writes back to the file will store in the new format
cache_from_file(Cache, {K, V}) ->
    wh_cache:store_local(Cache, cache_key(K), V);
cache_from_file(Cache, T) when is_tuple(T) ->
    [K|V] = erlang:tuple_to_list(T),
    wh_cache:store_local(Cache, cache_key(K), erlang:list_to_tuple(V)).

-spec write_config/2 :: (file:name(), iodata()) -> 'ok' | {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
write_config(Path, Contents) ->
    file:write_file(Path, Contents).

-spec write_config/1 :: (file:name()) -> 'ok' | {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
write_config(Path) ->
    ?LOG_SYS("writing cached config to ~s", [Path]),
    {ok, Cache} = whistle_couch_sup:cache_proc(),
    KVs = wh_cache:filter_local(Cache, fun(K, _) -> is_cache_key(K) end),
    Contents = lists:foldl(fun(I, Acc) -> [io_lib:format("~p.~n", [I]) | Acc] end
			   , "", [{get_key_from_cache_key(CK),V} || {CK, V} <- KVs]),
    file:write_file(Path, Contents).

fetch(Key) ->
    fetch(Key, undefined).

fetch(Key, Cache) when is_pid(Cache) ->
    fetch(Key, undefined, Cache);
fetch(Key, Default) ->
    {ok, Cache} = whistle_couch_sup:cache_proc(),
    fetch(Key, Default, Cache).

fetch(Key, Default, Cache) ->
    case wh_cache:fetch_local(Cache, cache_key(Key)) of
	{ok, Val} -> Val;
	_ -> Default
    end.

-spec store/2 :: (term(), term()) -> 'ok'.
-spec store/3 :: (term(), term(), non_neg_integer() | 'infinity') -> 'ok'.
-spec store/4 :: (term(), term(), non_neg_integer() | 'infinity', pid()) -> 'ok'.
store(Key, Value) ->
    {ok, Cache} = whistle_couch_sup:cache_proc(),
    store(Key, Value, infinity, Cache).

store(Key, Value, Timeout) ->
    {ok, Cache} = whistle_couch_sup:cache_proc(),
    store(Key, Value, Timeout, Cache).

store(Key, Value, Timeout, Cache) when is_pid(Cache) ->
    wh_cache:store_local(Cache, cache_key(Key), Value, Timeout).

-spec cache_key/1 :: (K) -> {?MODULE, K}.
cache_key(K) ->
    {?MODULE, K}.

-spec is_cache_key/1 :: (term()) -> boolean().
is_cache_key({?MODULE, _}) ->
    true;
is_cache_key(_) ->
    false.

-spec get_key_from_cache_key/1 :: ({?MODULE, K}) -> K.
get_key_from_cache_key({?MODULE, K}) ->
    K.
