%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Global cache server
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kzc_global_cache).

-export([store/2, store/3]).
-export([peek/1]).
-export([fetch/1, fetch_keys/0]).
-export([erase/1]).
-export([flush/0]).
-export([filter/1]).
-export([dump/0, dump/1]).
-export([wait_for_key/1, wait_for_key/2]).

-include("kz_caches.hrl").

-define(SERVER, ?MODULE).

%%% API

-spec store(any(), any()) -> 'ok'.
-spec store(any(), any(), wh_proplist()) -> 'ok'.
store(K, V) -> kzc_cache:store(?SERVER, K, V).
store(K, V, Props) -> kzc_cache:store(?SERVER, K, V, Props).

-spec peek(any()) -> {'ok', any()} | {'error', 'not_found'}.
peek(K) -> kzc_cache:peek(?SERVER, K).

-spec fetch(any()) -> {'ok', any()} | {'error', 'not_found'}.
fetch(K) -> kzc_cache:fetch(?SERVER, K).

-spec erase(any()) -> 'ok'.
erase(K) -> kzc_cache:erase(?SERVER, K).

-spec flush() -> 'ok'.
flush() -> kzc_cache:flush(?SERVER).

-spec fetch_keys() -> [any()].
fetch_keys() -> kzc_cache:fetch_keys(?SERVER).

-spec filter(kzc_cache:predicate2()) -> [{any(), any()}].
filter(Pred) -> kzc_cache:filter(?SERVER, Pred).

-spec dump() -> 'ok'.
-spec dump(text()) -> 'ok'.
dump() -> kzc_cache:dump(?SERVER).
dump(ShowValue) -> kzc_cache:dump(?SERVER, ShowValue).

-spec wait_for_key(any()) -> {'ok', any()} | {'error', 'timeout'}.
-spec wait_for_key(any(), wh_timeout()) -> {'ok', any()} | {'error', 'timeout'}.
wait_for_key(Key) -> kzc_cache:wait_for_key(?SERVER, Key).
wait_for_key(Key, Timeout) -> kzc_cache:wait_for_key(?SERVER, Key, Timeout).

%%% Internals

%%% End of Module
