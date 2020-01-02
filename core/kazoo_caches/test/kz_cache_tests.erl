%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-include("kz_caches.hrl").

-define(BASE_TIMEOUT_MS, 50).

cache_test_() ->
    {'timeout'
    ,10
    ,{'spawn'
     ,{'setup'
      ,fun init/0
      ,fun cleanup/1
      ,fun(_CachePid) ->
               [{"wait for key exists", fun wait_for_key_local_existing/0}
               ,{"wait for key appears", fun wait_for_key_local_mid_stream/0}
               ,{"wait for key timeout", fun wait_for_key_local_timeout/0}
               ,{"wait for stampede key exists", fun wait_for_stampede_key_existing/0}
               ,{"key peek", fun peek_local/0}
               ,{"key erase", fun key_erase/0}
               ,{"cache flush", fun cache_flush/0}
               ,{"fetch keys", fun fetch_keys/0}
               ,{"filter keys", fun filter_keys/0}
               ,{"filter/erase keys", fun filter_erase_keys/0}
               ,{"callbacks", fun callbacks/0}
               ,{"key timeout is flushed", fun key_timeout_is_flushed/0}
               ,{"bump key timeout", fun bump_key_timeout/0}
               ]
       end
      }
     }
    }.

init() ->
    {'ok', CachePid} = kz_cache_sup:start_link(?MODULE, ?BASE_TIMEOUT_MS),
    CachePid.

cleanup(CachePid) ->
    kz_cache:stop_local(CachePid).

wait_for_key_local_existing() ->
    Timeout = rand:uniform(?BASE_TIMEOUT_MS)+?BASE_TIMEOUT_MS,
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(5),

    kz_cache:store_local(?MODULE, Key, Value),

    _WriterPid = spawn_monitor(fun() -> writer_job(Key, Value, Timeout) end),

    ?assertEqual({'ok', Value}, kz_cache:wait_for_key_local(?MODULE, Key, Timeout)).

wait_for_stampede_key_existing() ->
    Timeout = rand:uniform(?BASE_TIMEOUT_MS)+?BASE_TIMEOUT_MS,
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(5),

    kz_cache:store_local(?MODULE, Key, Value),
    ?assertEqual({'ok', Value}, kz_cache:wait_for_stampede_local(?MODULE, Key, Timeout)).

wait_for_key_local_mid_stream() ->
    Timeout = rand:uniform(?BASE_TIMEOUT_MS)+?BASE_TIMEOUT_MS,
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(5),

    _WriterPid = spawn_monitor(fun() -> writer_job(Key, Value, Timeout) end),

    Wait = kz_cache:wait_for_key_local(?MODULE, Key, Timeout),
    ?assertEqual({'ok', Value}, Wait).

wait_for_key_local_timeout() ->
    Timeout = ?BASE_TIMEOUT_MS,
    Key = kz_binary:rand_hex(5),

    ?assertEqual({'error', 'timeout'}, kz_cache:wait_for_key_local(?MODULE, Key, Timeout)).

peek_local() ->
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(5),

    ?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, Key)),

    kz_cache:store_local(?MODULE, Key, Value),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, Key)).

key_timeout_is_flushed() ->
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(5),

    TimeoutS = 1, % 1s
    HalfTimeoutMs = TimeoutS * ?MILLISECONDS_IN_SECOND div 2,

    kz_cache:store_local(?MODULE, Key, Value, [{'expires', TimeoutS}]),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, Key)),

    timer:sleep(HalfTimeoutMs),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, Key)),

    timer:sleep(HalfTimeoutMs + ?BASE_TIMEOUT_MS),
    ?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, Key)).

bump_key_timeout() ->
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(5),

    TimeoutS = 1, % 1s
    HalfTimeoutMs = TimeoutS * ?MILLISECONDS_IN_SECOND div 2,
    kz_cache:store_local(?MODULE, Key, Value, [{'expires', TimeoutS}]),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, Key)),

    timer:sleep(HalfTimeoutMs),
    ?assertEqual({'ok', Value}, kz_cache:fetch_local(?MODULE, Key)), % bump timestamp

    timer:sleep(HalfTimeoutMs),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, Key)), % now half-way to new timeout

    timer:sleep(HalfTimeoutMs - ?BASE_TIMEOUT_MS),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, Key)), % now just before new timeout

    timer:sleep(?BASE_TIMEOUT_MS * 2), % other side of the new timeout
    ?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, Key)).

key_erase() ->
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(5),

    ?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, Key)),
    ?assertEqual('ok', kz_cache:erase_local(?MODULE, Key)),

    kz_cache:store_local(?MODULE, Key, Value),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, Key)),

    kz_cache:erase_local(?MODULE, Key),
    ?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, Key)).

cache_flush() ->
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(5),

    ?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, Key)),

    kz_cache:store_local(?MODULE, Key, Value),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, Key)),

    kz_cache:flush_local(?MODULE),
    ?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, Key)).

fetch_keys() ->
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(5),

    ?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, Key)),
    ?assertEqual([], kz_cache:fetch_keys_local(?MODULE)),

    kz_cache:store_local(?MODULE, Key, Value),
    ?assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, Key)),

    Keys = kz_cache:fetch_keys_local(?MODULE),
    ?assertEqual([Key], Keys).

filter_keys() ->
    kz_cache:flush_local(?MODULE),
    _ = [kz_cache:store_local(?MODULE, X, X) || X <- lists:seq(1,10)],

    Vs = [{X, X} || X <- lists:seq(6,10)],
    Filtered = lists:sort(kz_cache:filter_local(?MODULE, fun(K, _V) -> K > 5 end)),

    ?assertEqual(Vs, Filtered).

filter_erase_keys() ->
    kz_cache:flush_local(?MODULE),
    _ = [kz_cache:store_local(?MODULE, X, X) || X <- lists:seq(1,10)],

    Erased = kz_cache:filter_erase_local(?MODULE, fun(K, _V) -> K > 5 end),

    ?assertEqual(5, Erased),
    [?assertEqual({'ok', X}, kz_cache:peek_local(?MODULE, X)) || X <- lists:seq(1,5)],
    [?assertEqual({'error', 'not_found'}, kz_cache:peek_local(?MODULE, X)) || X <- lists:seq(6,10)].

callbacks() ->
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(6),

    store_callback(Key, Value),
    kz_cache:erase_local(?MODULE, Key),
    ?assertEqual({Key, Value, 'erase'}
                ,receive_callback()
                ),

    store_callback(Key, Value),
    kz_cache:flush_local(?MODULE),
    ?assertEqual({Key, Value, 'flush'}
                ,receive_callback()
                ),

    store_callback(Key, Value),
    ?assertEqual({Key, Value, 'expire'}
                ,receive_callback()
                ).

store_callback(Key, Value) ->
    Self = self(),
    kz_cache:store_local(?MODULE, Key, Value, [{'callback', fun(K, V, Reason) ->
                                                                    Self ! {'callback', K, V, Reason}
                                                            end
                                               }
                                              ,{'expires', 1}
                                              ]).

receive_callback() ->
    receive
        {'callback', Key, Value, Reason} -> {Key, Value, Reason}
    after 2000 ->
            {'error', 'timeout'}
    end.

writer_job(Key, Value, Timeout) ->
    timer:sleep(Timeout div 2),
    kz_cache:store_local(?MODULE, Key, Value).
