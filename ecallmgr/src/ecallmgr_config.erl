-module(ecallmgr_config).

-export([load_config/1, write_config/2, fetch/1, fetch/2]).

-include("ecallmgr.hrl").

-spec load_config/1 :: (file:name()) -> 'ok' | {'error', 'enoent'}.
load_config(Path) ->
    case file:consult(Path) of
	{ok, Startup} ->
	    Cache = ecallmgr_sup:cache_proc(),
	    _ = [wh_cache:store_local(Cache, K, V) || {K,V} <- Startup],
	    ok;
	{error, enoent} ->
	    {error, enoent}
    end.

-spec write_config/2 :: (file:name(), iodata()) -> 'ok' | {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
write_config(Path, Contents) ->
    file:write_file(Path, Contents).

fetch(Key) ->
    fetch(Key, undefined).

fetch(Key, Default) ->
    Cache = ecallmgr_sup:cache_proc(),
    case wh_cache:fetch_local(Cache, Key) of
	{ok, Val} -> Val;
	_ -> Default
    end.
