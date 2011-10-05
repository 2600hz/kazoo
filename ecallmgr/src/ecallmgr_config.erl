-module(ecallmgr_config).

-export([load_config/1, write_config/1, write_config/2, fetch/1, fetch/2]).

-include("ecallmgr.hrl").

-spec load_config/1 :: (file:name()) -> 'ok' | {'error', 'enoent'}.
load_config(Path) ->
    ?LOG("Loading ~s", [Path]),
    case file:consult(Path) of
	{ok, Startup} ->
	    {ok, Cache} = ecallmgr_sup:cache_proc(),
	    _ = [wh_cache:store_local(Cache, cache_key(K), V) || {K,V} <- Startup],
	    ok;
	{error, enoent} ->
	    ?LOG("No file"),
	    {error, enoent}
    end.

-spec write_config/2 :: (file:name(), iodata()) -> 'ok' | {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
write_config(Path, Contents) ->
    file:write_file(Path, Contents).

-spec write_config/1 :: (file:name()) -> 'ok' | {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
write_config(Path) ->
    {ok, Cache} = ecallmgr_sup:cache_proc(),
    KVs = wh_cache:filter_local(Cache, fun({?MODULE, _}, _) -> true; (_,_) -> false end),
    Contents = lists:foldl(fun(I, Acc) -> [io_lib:format("~p.~n", [I]) | Acc] end
			   , "", [{K,V} || {{?MODULE, K}, V} <- KVs]),
    file:write_file(Path, Contents).

fetch(Key) ->
    fetch(Key, undefined).

fetch(Key, Default) ->
    {ok, Cache} = ecallmgr_sup:cache_proc(),
    case wh_cache:fetch_local(Cache, cache_key(Key)) of
	{ok, Val} -> Val;
	_ -> Default
    end.

cache_key(K) ->
    {?MODULE, K}.
