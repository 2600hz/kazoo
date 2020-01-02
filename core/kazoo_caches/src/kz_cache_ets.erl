%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Simple cache server.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_ets).
-behaviour(gen_server).

%%------------------------------------------------------------------------------
%% @doc API functions
%% @end
%%------------------------------------------------------------------------------
-export([start_link/1
        ,pointer_tab/1
        ,monitor_tab/1

        ,store/4
        ,store_async/4

        ,peek/2
        ,fetch/2

        ,erase/2
        ,flush/1

        ,wait_for_key/3
        ,wait_for_stampede/3
        ,mitigate_stampede/3

        ,mitigating_keys/1
        ]).

-export([expire_objects/1]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kz_caches.hrl").

-define(MONITOR_EXPIRE_MSG, 'monitor_cleanup').

-type state() :: atom().

-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Name) ->
    gen_server:start_link({'local', Name}, ?MODULE, [Name], []).

-spec pointer_tab(atom()) -> atom().
pointer_tab(Name) ->
    to_tab(Name, "_pointers").

-spec monitor_tab(atom()) -> atom().
monitor_tab(Name) ->
    to_tab(Name, "_monitors").

-spec to_tab(atom(), string()) -> atom().
to_tab(Name, Suffix) ->
    kz_term:to_atom(kz_term:to_list(Name) ++ Suffix, 'true').

-spec store(kz_types:server_ref(), any(), any(), kz_cache:store_options()) -> 'ok' | 'error'.
store(Srv, K, V, Props)  ->
    gen_server:call(Srv, {'store', cache_obj(K, V, Props)}).

-spec store_async(kz_typs:server_ref(), any(), any(), kz_term:proplist()) -> 'ok'.
store_async(Srv, K, V, Props) ->
    gen_server:cast(Srv, {'store', cache_obj(K, V, Props)}).

-spec peek(atom(), any()) -> {'ok', any()} |
          {?MITIGATION, pid()} |
          {'error', 'not_found'}.
peek(Srv, Key) ->
    try ets:lookup_element(Srv, Key, #cache_obj.value) of
        {?MITIGATION, _Pid}=Mitigation -> Mitigation;
        Value -> {'ok', Value}
    catch
        'error':'badarg' ->
            {'error', 'not_found'}
    end.

-spec fetch(atom(), any()) -> {'ok', any()} |
          {'error', 'not_found'} |
          {?MITIGATION, pid()}.
fetch(Srv, Key) ->
    case peek(Srv, Key) of
        {'error', 'not_found'}=E -> E;
        {?MITIGATION, _Pid}=Stampede -> Stampede;
        {'ok', _Value}=Ok ->
            ets:update_element(Srv, Key, {#cache_obj.timestamp_ms, kz_time:now_ms()}),
            Ok
    end.

-spec erase(atom(), any()) -> 'ok'.
erase(Srv, Key) ->
    case peek(Srv, Key) of
        {'error', 'not_found'} -> 'ok';
        {?MITIGATION, _Pid} -> kz_cache_callbacks:erased_mitigation(Srv, Key);
        {'ok', _} -> kz_cache_callbacks:erased(Srv, Key)
    end.

-spec flush(atom()) -> 'ok'.
flush(Name) ->
    kz_cache_callbacks:flushed(Name, pointer_tab(Name), monitor_tab(Name)).

-spec cache_obj(any(), any(), kz_cache:store_options()) -> cache_obj().
cache_obj(K, V, Props) ->
    #cache_obj{key=K
              ,value=V
              ,expires_s=get_props_expires(Props)
              ,callback=get_props_callback(Props)
              ,origin=get_props_origin(Props)
              }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_props_expires(kz_term:proplist()) -> timeout().
get_props_expires(Props) ->
    case props:get_value('expires', Props) of
        'undefined' -> ?EXPIRES;
        'infinity' -> 'infinity';
        Expires when is_integer(Expires)
                     andalso Expires > 0 ->
            Expires
    end.

-spec get_props_callback(kz_term:proplist()) -> 'undefined' | callback_fun().
get_props_callback(Props) ->
    case props:get_value('callback', Props) of
        'undefined' -> 'undefined';
        Fun when is_function(Fun, 3) -> Fun
    end.

-spec get_props_origin(kz_term:proplist()) -> 'undefined' | origin_tuple() | origin_tuples().
get_props_origin(Props) -> props:get_value('origin', Props).

-spec wait_for_key(kz_types:server_ref(), any(), pos_integer()) ->
          {'ok', any()} |
          {'error', 'timeout'}.
wait_for_key(Srv, Key, Timeout) when is_integer(Timeout) ->
    WaitFor = Timeout + 100,

    case handle_wait_for_key(Srv, monitor_tab(Srv), Key, Timeout) of
        {'exists', Value} -> {'ok', Value};
        {'ok', Ref} -> wait_for_response(Ref, WaitFor)
    end.

-spec wait_for_stampede(kz_types:server_ref(), any(), pos_integer()) ->
          {'ok', any()} |
          {'error', 'timeout'}.
wait_for_stampede(Srv, Key, Timeout) when is_integer(Timeout) ->
    wait_for_key(Srv, Key, Timeout).

-spec wait_for_response(reference(), timeout()) -> {'ok', any()} |
          {'error', 'timeout'}.
wait_for_response(Ref, WaitFor) ->
    receive
        {'exists', Ref, {'error', _Reason}=Error} ->
            Error;
        {'exists', Ref, Value} ->
            {'ok', Value};
        {'store', Ref, {'error', _Reason}=Error} ->
            Error;
        {'store', Ref, Value} ->
            {'ok', Value};
        {_, Ref, _}=_Other ->
            {'error', 'timeout'}
    after WaitFor ->
            {'error', 'timeout'}
    end.

-spec mitigate_stampede(atom(), any(), kz_cache:store_options()) -> 'ok' | 'error'.
mitigate_stampede(Srv, Key, Options) ->
    CacheObj = cache_obj(Key, {?MITIGATION, self()}, Options),
    case ets:insert_new(Srv, CacheObj) of
        'true' ->
            %% link caller to the cache gen_server in case caller dies
            link(whereis(Srv)),
            'ok';
        'false' -> 'error'
    end.

-spec expire_objects(atom()) -> non_neg_integer().
expire_objects(Name) ->
    kz_cache_callbacks:expire_objects(Name, [pointer_tab(Name), monitor_tab(Name)]).

-spec init(kz_terms:atoms()) -> {'ok', state()}.
init([Name]) ->
    process_flag('trap_exit', 'true'),
    _Name = ets:new(Name
                   ,['set', 'public', 'named_table', {'keypos', #cache_obj.key}]
                   ),
    _PointerName = ets:new(pointer_tab(Name)
                          ,['bag', 'public', 'named_table', {'keypos', #cache_obj.key}]
                          ),
    _MonitorName = ets:new(monitor_tab(Name)
                          ,['bag', 'public', 'named_table', {'keypos', #cache_obj.key}]
                          ),
    {'ok', Name}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'store', CacheObj}, _From, Name) ->
    {'reply', handle_store(CacheObj, Name), Name};
handle_call(_Request, _From, Name) ->
    {'noreply', Name}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'store', CacheObj}, Name) ->
    _ = handle_store(CacheObj, Name),
    {'noreply', Name};

handle_cast(_Msg, Name) ->
    {'noreply', Name}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', _Ref, {?MONITOR_EXPIRE_MSG, MonitorRef}}
           ,Name
           ) ->
    _ = kz_process:spawn(fun kz_cache_callbacks:timed_out/2
                        ,[monitor_tab(Name), MonitorRef]
                        ),
    {'noreply', Name};
handle_info({'EXIT', Pid, _Reason}, Name) ->
    kz_process:spawn(fun() -> handle_dead_pid(Name, Pid) end),
    {'noreply', Name};
handle_info(_Info, Name) ->
    ?LOG_INFO("unhandled msg: ~p", [_Info]),
    {'noreply', Name}.

-spec terminate(any(), state()) -> any().
terminate(_Reason, Name) ->
    ets:delete(Name),
    ets:delete(pointer_tab(Name)),
    ets:delete(monitor_tab(Name)),
    lager:debug("terminating ~p", [Name]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec handle_store(cache_obj(), state()) -> 'ok' | 'error'.
handle_store(#cache_obj{key=Key
                       ,value={?MITIGATION, _Pid}
                       }=CacheObj
            ,Name
            ) ->
    case peek(Name, Key) of
        {'error', 'not_found'} ->
            store_cache_obj(CacheObj, Name);
        _Else ->
            'error'
    end;
handle_store(CacheObj, Name) ->
    store_cache_obj(CacheObj, Name).

store_cache_obj(#cache_obj{key=Key
                          ,value=Value
                          ,origin=Origins
                          ,expires_s=Expires
                          }=CacheObj
               ,Name
               ) ->
    kz_cache_callbacks:maybe_log_unlock(Key, "store", peek(Name, Key)),
    'true' = ets:insert(Name, CacheObj#cache_obj{origin='undefined'}),
    kz_cache_listener:add_origin_pointers(Name, CacheObj, Origins),
    kz_cache_callbacks:stored(Name, Key, Value),

    kz_cache_lru:update_expire_period(Name, Expires),
    'ok'.

-spec handle_wait_for_key(atom(), atom(), any(), pos_integer()) ->
          {'ok', reference()} |
          {'exists', any()}.
handle_wait_for_key(Name, MonitorName, Key, Timeout) ->
    case peek(Name, Key) of
        {'ok', Value} ->
            {'exists', Value};
        _Else ->
            Ref = add_monitor(Name, MonitorName, Key, Timeout, self()),
            {'ok', Ref}
    end.

add_monitor(Name, MonitorName, Key, Timeout, FromPid) ->
    Ref = make_ref(),
    CacheObj = #cache_obj{key=Key
                         ,value=Ref
                         ,expires_s=Timeout
                         ,callback=monitor_response_fun(FromPid, Ref)
                         },
    'true' = ets:insert(MonitorName, CacheObj),

    _TRef = start_monitor_expire_timer(Name, Timeout, Ref),
    Ref.

-spec monitor_response_fun(pid(), reference()) -> fun().
monitor_response_fun(Pid, Ref) ->
    fun(_, Value, Reason) -> Pid ! {Reason, Ref, Value} end.

-spec start_monitor_expire_timer(kz_term:server_ref(), pos_integer(), reference()) -> reference().
start_monitor_expire_timer(Cache, Timeout, Ref) ->
    erlang:start_timer(Timeout, whereis(Cache), {?MONITOR_EXPIRE_MSG, Ref}).

-spec handle_dead_pid(atom(), pid()) -> non_neg_integer().
handle_dead_pid(Name, Pid) ->
    MatchSpec =
        [{#cache_obj{key='$1', value='$2', _='_'}
         ,[{'=:=', '$2', {const, {?MITIGATION, Pid}}}]
         ,[{{'$1', '$2'}}]
         }],
    Erased = [erase(Name, Key)
              || {Key, {?MITIGATION, P}} <- ets:select(Name, MatchSpec),
                 not is_process_alive(P)
             ],
    length(Erased).

-spec mitigating_keys(atom()) -> [{any(), pid()}].
mitigating_keys(Name) ->
    MatchSpec =
        [{#cache_obj{key='$1', value='$2', _='_'}
         ,[{'is_record', '$2', ?MITIGATION, 2}]
         ,[{{'$1', '$2'}}]
         }],
    [{Key, Pid} || {Key, {?MITIGATION, Pid}} <- ets:select(Name, MatchSpec)].
