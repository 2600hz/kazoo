%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_callbacks).

-export([timed_out/2
        ,expire_objects/2
        ,erased/2, erased_mitigation/2
        ,flushed/3
        ,stored/3
        ]).

-export([maybe_log_unlock/3]).

-include("kz_caches.hrl").

-spec flushed(atom(), atom(), atom()) -> 'ok'.
flushed(Name, PointerName, MonitorName) ->
    _ = [flush_table(T) || T <- [Name, PointerName, MonitorName]],
    'ok'.

-spec flush_table(atom()) -> 'ok'.
flush_table(T) ->
    flush_table(T, is_running(T)).

-spec flush_table(atom(), boolean()) -> 'ok'.
flush_table(_T, 'false') ->
    lager:info("cache ~s is not running", [_T]);
flush_table(T, 'true') ->
    exec_flush_callbacks(T),
    ets:delete_all_objects(T).

-spec exec_flush_callbacks(atom()) -> 'ok'.
exec_flush_callbacks(Name) ->
    MatchSpec =
        [{#cache_obj{key = '$1'
                    ,value = '$2'
                    ,callback = '$3'
                    , _ = '_'
                    }
         ,[{'=/=', '$3', 'undefined'}]
         ,[{{'$3', '$1', '$2'}}]
         }],

    exec_callbacks(Name, MatchSpec, 'flush').

-spec erased(atom(), any()) -> 'ok'.
erased(Name, Key) ->
    exec_erase_callbacks(Name, Key),
    _ = [maybe_remove_object(Table, Key)
         || Table <- [Name, kz_cache_ets:pointer_tab(Name), kz_cache_ets:monitor_tab(Name)]
        ],
    'ok'.

-spec erased_mitigation(atom(), any()) -> 'ok'.
erased_mitigation(Name, Key) ->
    maybe_log_unlock(Key, "erase", kz_cache_ets:peek(Name, Key)),
    stored(Name, Key, {'error', 'not_found'}),
    erased(Name, Key).

-spec maybe_log_unlock(any(), string(), any()) -> 'ok'.
maybe_log_unlock(Key, Type, {?MITIGATION, _Pid}) ->
    lager:debug("~s is unlocking key ~p (locked by ~p)", [Type, Key, _Pid]);
maybe_log_unlock(_Key, _Type, _Peek) -> 'ok'.

-spec exec_erase_callbacks(atom(), any()) -> 'ok'.
exec_erase_callbacks(Name, Key) ->
    try ets:lookup_element(Name, Key, #cache_obj.callback) of
        Fun when is_function(Fun, 3) ->
            exec_erase_callbacks(Name, Key, Fun),
            'ok';
        _Else -> 'ok'
    catch
        'error':'badarg' -> 'ok'
    end.

-spec exec_erase_callbacks(atom(), any(), callback_fun()) -> pid().
exec_erase_callbacks(Name, Key, Fun) ->
    Value = ets:lookup_element(Name, Key, #cache_obj.value),
    exec_callback(Fun, [Key, Value, 'erase']).

-spec timed_out(atom(), reference()) -> 'ok'.
timed_out(MonitorName, MonitorRef) ->
    exec_timeout_callbacks(MonitorName, MonitorRef, has_monitors(MonitorName)).

-spec exec_timeout_callbacks(atom(), reference(), boolean()) -> 'ok'.
exec_timeout_callbacks(_MonitorName, _MonitorRef, 'false') -> 'ok';
exec_timeout_callbacks(MonitorName, MonitorRef, 'true') ->
    MatchSpec = [{#cache_obj{key = '$1'
                            ,callback = '$2'
                            ,value = '$3'
                            ,_ = '_'
                            }
                 ,[{'=:=', {'const', MonitorRef}, '$3'}]
                 ,[['$1', '$3', '$2']]
                 }],
    exec_callbacks(MonitorName, MatchSpec, 'timeout'),
    exec_timeout_callbacks(MonitorName, MatchSpec).

-spec exec_timeout_callbacks(atom(), ets:match_spec()) -> 'ok'.
exec_timeout_callbacks(Name, MatchSpec) ->
    _ = [exec_timeout_callback(Name, list_to_tuple(Callback))
         || Callback <- ets:select(Name, MatchSpec)
        ],
    'ok'.

-spec exec_timeout_callback(atom(), {any(), reference(), fun()}) -> 'ok'.
exec_timeout_callback(Name, {Key, Value, Callback}) when is_function(Callback, 3),
                                                         is_reference(Value) ->
    catch Callback(Key, Value, 'timeout'),
    maybe_remove_object(Name, Key),
    'ok'.

-spec expire_objects(atom(), kz_term:atoms()) -> non_neg_integer().
expire_objects(Name, AuxNameles) ->
    Now = kz_time:now_ms(),
    FindSpec = [{#cache_obj{key = '$1'
                           ,value = '$2'
                           ,expires_s = '$3'
                           ,timestamp_ms = '$4'
                           ,callback = '$5'
                           ,_ = '_'
                           }
                ,[{'=/=', '$3', 'infinity'}
                 ,{'>', {'const', Now}, {'+', '$4', {'*', '$3', {'const', ?MILLISECONDS_IN_SECOND}}}}
                 ]
                ,[{{'$5', '$1', '$2'}}]
                }],
    expire_objects(Name, AuxNameles, ets:select(Name, FindSpec)).

-spec expire_objects(atom(), kz_term:atoms(), list()) -> non_neg_integer().
expire_objects(_Name, _Tables, []) ->
    0;
expire_objects(Name, Tables, Objects) ->

    _ = exec_expired_callbacks(Objects),
    Keys = [K || {_, K, _} <- Objects],
    lager:debug("expiring keys from ~s:", [Name]),

    lager:debug("  ~p", [Keys]),
    lists:foreach(fun(Table) -> maybe_remove_objects(Table, Keys) end
                 ,[Name | Tables]
                 ),
    length(Objects).

-spec exec_expired_callbacks([{callback_fun(), any(), any()}]) -> 'ok'.
exec_expired_callbacks(Objects) ->
    [exec_expired_callback(CallbackFun, K, V)
     || {CallbackFun, K, V} <- Objects,
        is_function(CallbackFun, 3)
    ],
    'ok'.

-spec exec_expired_callback(callback_fun(), any(), any()) -> 'ok'.
exec_expired_callback(Fun, K, V) ->
    _ = exec_callback(Fun, [K, V, 'expire']),
    'ok'.

-spec maybe_remove_objects(atom(), list()) -> 'ok'.
maybe_remove_objects(Name, Objects) ->
    _ = [maybe_remove_object(Name, Object) || Object <- Objects],
    'ok'.

-spec maybe_remove_object(atom(), cache_obj() | any()) -> 'true'.
maybe_remove_object(Name, #cache_obj{key = Key}) ->
    maybe_remove_object(Name, Key);
maybe_remove_object(Name, Key) ->
    ets:delete(Name, Key).

-spec stored(atom(), any(), any()) -> 'ok'.
stored(Name, Key, Value) ->
    MonitorName = kz_cache_ets:monitor_tab(Name),
    exec_store_callbacks(MonitorName, Key, Value, has_monitors(MonitorName)).

-spec has_monitors(atom()) -> boolean().
has_monitors(MonitorName) ->
    ets:info(MonitorName, 'size') > 0.

-spec exec_store_callbacks(atom(), any(), any(), boolean()) -> 'ok'.
exec_store_callbacks(_MonitorName, _Key, _Value, 'false') -> 'ok';
exec_store_callbacks(MonitorName, Key, Value, 'true') ->
    MatchSpec = [{#cache_obj{key = '$1'
                            ,callback = '$5'
                            ,_ = '_'
                            }
                 ,[{'=:=', '$1', {'const', Key}}]
                 ,[{{'$5', '$1', {'const', Value}}}]
                 }
                ],
    exec_callbacks(MonitorName, MatchSpec, 'store'),
    maybe_remove_object(MonitorName, Key),
    'ok'.

-spec exec_callbacks(atom(), ets:match_spec(), callback_msg()) -> 'ok'.
exec_callbacks(Name, MatchSpec, Msg) ->
    _Pids = [exec_callback(Callback, [K, V, Msg])
             || {Callback, K, V} <- ets:select(Name, MatchSpec),
                is_function(Callback, 3)
            ],
    'ok'.

-spec is_running(atom()) -> boolean().
is_running(Name) ->
    'undefined' =/= ets:info(Name, 'type').

-spec exec_callback(callback_fun(), list()) -> pid().
exec_callback(Callback, Payload) ->
    kz_process:spawn(Callback, Payload).
