%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc API interface to the cache system
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache).

-export([stop_local/1]).
-export([store_local/3, store_local/4
        ,store_local_async/3, store_local_async/4
        ]).
-export([peek_local/2]).
-export([fetch_local/2, fetch_keys_local/1]).
-export([erase_local/2]).
-export([flush_local/1]).
-export([filter_local/2, filter_erase_local/2]).
-export([dump_local/1, dump_local/2]).
-export([count_local/2, count_local/3, count_local/5]).

-export([wait_for_key_local/2
        ,wait_for_key_local/3
        ,wait_for_stampede_local/2, wait_for_stampede_local/3
        ,mitigate_stampede_local/2, mitigate_stampede_local/3
        ,mitigation_key/0
        ]).

-include("kz_caches.hrl").

-type store_options() :: [{'origin', origin_tuple() | origin_tuples()} |
                          {'expires', timeout()} |
                          {'callback', 'undefined' | callback_fun()} |
                          {'monitor', boolean() | [pid()]}
                         ].

-type start_option() :: {'origin_bindings', origin_tuples()} |
                        {'new_node_flush', boolean()} |
                        {'expire_node_flush', boolean()} |
                        {'new_channel_flush', boolean()} |
                        {'channel_reconnect_flush', boolean()}.
-type start_options() :: [start_option()].

-export_type([store_options/0
             ,start_options/0
             ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------

%% Local cache API
-spec stop_local(kz_types:server_ref()) -> 'true'.
stop_local(Name) when is_atom(Name) ->
    kz_cache_sup:stop(Name);
stop_local(Pid) when is_pid(Pid) ->
    exit(Pid, 'shutdown').

-spec store_local(kz_types:server_ref(), any(), any()) -> 'ok' | 'error'.
store_local(Srv, K, V) -> store_local(Srv, K, V, []).

-spec store_local(kz_types:server_ref(), any(), any(), store_options()) -> 'ok' | 'error'.
store_local(Srv, K, V, Props)  ->
    kz_cache_ets:store(Srv, K, V, Props).

-spec store_local_async(kz_typs:server_ref(), any(), any()) -> 'ok'.
store_local_async(Srv, K, V) ->
    store_local_async(Srv, K, V, []).

-spec store_local_async(kz_typs:server_ref(), any(), any(), kz_term:proplist()) -> 'ok'.
store_local_async(Srv, K, V, Props) ->
    kz_cache_ets:store_async(Srv, K, V, Props).

-spec peek_local(atom(), any()) -> {'ok', any()} |
          {?MITIGATION, pid()} |
          {'error', 'not_found'}.
peek_local(Srv, Key) ->
    kz_cache_ets:peek(Srv, Key).

-spec fetch_local(atom(), any()) -> {'ok', any()} |
          {?MITIGATION, pid()} |
          {'error', 'not_found'}.
fetch_local(Srv, K) ->
    kz_cache_ets:fetch(Srv, K).

-spec erase_local(atom(), any()) -> 'ok'.
erase_local(Srv, K) ->
    kz_cache_ets:erase(Srv, K).

-spec flush_local(kz_term:text() | atom()) -> 'ok'.
flush_local(Srv) when not is_atom(Srv) ->
    flush_local(kz_term:to_atom(Srv));
flush_local(Srv) ->
    kz_cache_ets:flush(Srv).

-spec fetch_keys_local(atom()) -> list().
fetch_keys_local(Srv) ->
    MatchSpec = [{#cache_obj{key = '$1'
                            ,_ = '_'
                            }
                 ,[]
                 ,['$1']
                 }],
    ets:select(Srv, MatchSpec).

-spec filter_erase_local(atom(), fun((any(), any()) -> boolean())) ->
          non_neg_integer().
filter_erase_local(Srv, Pred) when is_function(Pred, 2) ->
    ets:foldl(fun(#cache_obj{key=K, value=V}, Count) ->
                      case Pred(K, V) of
                          'true' -> erase_local(Srv, K), Count+1;
                          'false' -> Count
                      end;
                 (_, Count) -> Count
              end
             ,0
             ,Srv
             ).

-spec filter_local(atom(), fun((any(), any()) -> boolean())) -> [{any(), any()}].
filter_local(Srv, Pred) when is_function(Pred, 2) ->
    ets:foldl(fun(#cache_obj{key=K, value=V}, Acc) ->
                      case Pred(K, V) of
                          'true' -> [{K, V}|Acc];
                          'false' -> Acc
                      end;
                 (_, Acc) -> Acc
              end
             ,[]
             ,Srv
             ).

-spec dump_local(kz_term:text()) -> 'ok'.
dump_local(Srv) -> dump_local(Srv, 'false').

-spec dump_local(kz_term:text(), kz_term:text() | boolean()) -> 'ok'.
dump_local(Srv, ShowValue) when not is_atom(Srv) ->
    dump_local(kz_term:to_atom(Srv), ShowValue);
dump_local(Srv, ShowValue) when not is_boolean(ShowValue) ->
    dump_local(Srv, kz_term:to_boolean(ShowValue));
dump_local(Name, ShowValue) ->
    _ = [dump_table(Tab, ShowValue)
         || Tab <- [Name, kz_cache_ets:pointer_tab(Name), kz_cache_ets:monitor_tab(Name)]
        ],
    'ok'.

-spec dump_table(atom(), boolean()) -> 'ok'.
dump_table(Tab, ShowValue) ->
    Now = kz_time:now_s(),
    io:format('user', "Table ~p~n", [ets:info(Tab, 'name')]),
    _ = [display_cache_obj(CacheObj, ShowValue, Now)
         || CacheObj <- ets:match_object(Tab, #cache_obj{_ = '_'})
        ],
    'ok'.

-spec count_local(atom(), any()) -> non_neg_integer().
count_local(Srv, KeyMatchHead) ->
    count_local(Srv, KeyMatchHead, '_').

-spec count_local(atom(), any(), any()) -> non_neg_integer().
count_local(Srv, KeyMatchHead, ValMatchHead) ->
    kz_cache_ets:count(Srv, KeyMatchHead, 'undefined', ValMatchHead, 'undefined').

-spec count_local(atom(), any(), any(), any(), any()) -> non_neg_integer().
count_local(Srv
           ,KeyMatchHead, KeyMatchCondition
           ,ValMatchHead, ValMatchCondition
           ) ->
    kz_cache_ets:count(Srv, KeyMatchHead, KeyMatchCondition, ValMatchHead, ValMatchCondition).

-spec display_cache_obj(cache_obj(), boolean(), kz_time:gregorian_seconds()) -> 'ok'.
display_cache_obj(#cache_obj{key=Key
                            ,value=Value
                            ,timestamp_ms=Timestamp
                            ,expires_s=Expires
                            ,origin=Origin
                            ,callback=Callback
                            }
                 ,ShowValue
                 ,Now
                 ) ->
    io:format('user', "Key: ~300p~n", [Key]),
    io:format('user', "Expires(s): ~30p~n", [Expires]),
    case is_number(Expires) of
        'true' ->
            io:format('user', "Remaining: ~30p~n"
                     ,[(Timestamp div ?MILLISECONDS_IN_SECOND + Expires) - Now]
                     );
        'false' -> 'ok'
    end,
    io:format('user', "Origin: ~300p~n", [Origin]),
    io:format('user', "Callback: ~s~n", [Callback =/= 'undefined']),
    case ShowValue of
        'true' -> io:format('user', "Value: ~p~n", [Value]);
        'false' -> 'ok'
    end,
    io:format('user', "~n", []).

-spec wait_for_key_local(kz_types:server_ref(), any()) -> {'ok', any()} |
          {'error', 'timeout'}.
wait_for_key_local(Srv, Key) ->
    wait_for_key_local(Srv, Key, ?DEFAULT_WAIT_TIMEOUT_MS).

-spec wait_for_key_local(kz_types:server_ref(), any(), pos_integer()) ->
          {'ok', any()} |
          {'error', 'timeout'}.
wait_for_key_local(Srv, Key, Timeout) when is_integer(Timeout) ->
    kz_cache_ets:wait_for_key(Srv, Key, Timeout).

-spec wait_for_stampede_local(kz_types:server_ref(), any()) ->
          {'ok', any()} |
          {'error', 'timeout'}.
wait_for_stampede_local(Srv, Key) ->
    wait_for_stampede_local(Srv, Key, ?DEFAULT_WAIT_TIMEOUT_MS).

-spec wait_for_stampede_local(kz_types:server_ref(), any(), pos_integer()) ->
          {'ok', any()} |
          {'error', 'timeout'}.
wait_for_stampede_local(Srv, Key, Timeout) when is_integer(Timeout) ->
    wait_for_key_local(Srv, Key, Timeout).

-spec mitigate_stampede_local(atom(), any()) -> 'ok' | 'error'.
mitigate_stampede_local(Srv, Key) ->
    mitigate_stampede_local(Srv, Key, []).

-spec mitigate_stampede_local(atom(), any(), store_options()) -> 'ok' | 'error'.
mitigate_stampede_local(Srv, Key, Options) ->
    kz_cache_ets:mitigate_stampede(Srv, Key, Options).

-spec mitigation_key() -> ?MITIGATION.
mitigation_key() -> ?MITIGATION.
