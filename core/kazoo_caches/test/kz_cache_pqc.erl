%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc PropEr testing of the cache
%%% @author James Aimonetti
%%% @todo refactor how wait_for_* functions work in the model. need to track the
%%%       monitors in the model and adjust how now_ms is forwarded
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_pqc).

-ifdef(PROPER).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("kazoo_caches/src/kz_caches.hrl").

-define(CACHE_TTL_MS, 10).
-define(MIN_TIMEOUT_MS, ?CACHE_TTL_MS).
-define(MAX_TIMEOUT_MS, ?MIN_TIMEOUT_MS * 3).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0
        ,run_counterexample/0, run_counterexample/1
        ]).

-define(SERVER, ?MODULE).

-record(state, {cache = [] :: cache_objs()
               ,now_ms = 0 :: pos_integer()
               }).

%% proper_test_() ->
%%     {"Runs kz_cache PropEr tests"
%%     ,[
%%       {'timeout'
%%       ,20 * ?MILLISECONDS_IN_SECOND
%%       ,{"sequential testing"
%%        ,?_assert(proper:quickcheck(?MODULE:correct(), [{'to_file', 'user'}, 10]))
%%        }
%%       }
%% ,{"parallel testing"
%% ,?assert(proper:quickcheck(?MODULE:correct_parallel(), [{'to_file', 'user'}, 50]))
%% }
%%  ]
%% }.

run_counterexample() ->
    run_counterexample(proper:counterexample()).

run_counterexample('undefined') -> 'undefined';
run_counterexample([SeqSteps]) ->
    run_counterexample(SeqSteps, initial_state()).

run_counterexample(SeqSteps, State) ->
    process_flag('trap_exit', 'true'),
    kz_log:put_callid(?MODULE),
    is_pid(whereis(?SERVER))
        andalso kz_cache:stop_local(?SERVER),
    kz_cache_sup:start_link(?SERVER, ?CACHE_TTL_MS),

    try lists:foldl(fun transition_if/2
                   ,{1, State, #{}}
                   ,SeqSteps
                   )
    catch
        'throw':T -> {'throw', T}
    after
        stop(?SERVER)
    end.

transition_if({'set', Var, Call}, {Step, State, Env}) ->
    {'call', M, F, As} = Call,
    Resp = erlang:apply(M, F, fix_args(As, Env)),
    io:format("~w: ~w -> ~w~n", [Step, Call, Resp]),
    print_state(State),

    case postcondition(State, Call, Resp) of
        'true' ->
            {Step+1, next_state(State, Resp, Call), Env#{Var => Resp}};
        'false' ->
            io:format('user', "failed on step ~p~n", [Step]),
            throw({'failed_postcondition', State, Call, Resp})
    end.

fix_args(As, Env) ->
    [fix_arg(A, Env) || A <- As].

fix_arg(A, Env) ->
    case maps:get(A, Env, 'undefined') of
        'undefined' -> A;
        V -> V
    end.

print_state(#state{cache=[], now_ms=NowMs}) ->
    io:format("  at ~p: cache empty~n", [NowMs]);
print_state(#state{cache=Cache, now_ms=NowMs}) ->
    io:format("  at ~p: cache entries:~n", [NowMs]),
    _ = [print_cache_obj(Obj) || Obj <- Cache],
    'ok'.

print_cache_obj(#cache_obj{key=K, value=V, timestamp_ms=T, expires_s=E}) ->
    io:format("    ~p -> ~p: stored: ~p expires: ~p~n", [K, V, T, T + (E*?MILLISECONDS_IN_SECOND)]).

correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,begin
                kz_log:put_callid(?MODULE),
                stop(?SERVER),
                kz_cache_sup:start_link(?SERVER, ?CACHE_TTL_MS),
                {History, State, Result} = run_commands(?MODULE, Cmds),
                stop(?SERVER),
                ?WHENFAIL(io:format("Final State: ~p\nFailing Cmds: ~p\n"
                                   ,[State, zip(Cmds, History)]
                                   )
                         ,aggregate(command_names(Cmds), Result =:= 'ok')
                         )
            end
           ).

stop(Server) ->
    is_pid(whereis(Server))
        andalso kz_cache:stop_local(Server).

correct_parallel() ->
    ?FORALL(Cmds
           ,parallel_commands(?MODULE)
           ,begin
                stop(?SERVER),
                kz_cache_sup:start_link(?SERVER, ?CACHE_TTL_MS),

                {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                stop(?SERVER),

                ?WHENFAIL(io:format("Failing Cmds: ~p\nS: ~p\nP: ~p\n"
                                   ,[Cmds, Sequential, Parallel]
                                   )
                         ,aggregate(command_names(Cmds), Result =:= 'ok')
                         )
            end
           ).

initial_state() ->
    #state{cache=[]
          ,now_ms=0
          }.

command(#state{}=_Model) ->
    frequency([{10, {'call', 'kz_cache', 'store_local', [?SERVER, key(), value(), [{'expires', range(1,2)}]]}}
              ,{10, {'call', 'kz_cache', 'peek_local', [?SERVER, key()]}}
              ,{10, {'call', 'kz_cache', 'fetch_local', [?SERVER, key()]}}
              ,{10, {'call', 'kz_cache', 'erase_local', [?SERVER, key()]}}
              ,{10, {'call', 'kz_cache', 'wait_for_key_local', [?SERVER, key(), range(?MIN_TIMEOUT_MS, ?MAX_TIMEOUT_MS)]}}
              ,{10, {'call', 'kz_cache', 'mitigate_stampede_local', [?SERVER, key(), [{'expires', range(1,2)}]]}}
              ,{10, {'call', 'kz_cache', 'wait_for_stampede_local', [?SERVER, key(), range(?MIN_TIMEOUT_MS, ?MAX_TIMEOUT_MS)]}}
              ,{5, {'call', 'timer', 'sleep', [range(?MIN_TIMEOUT_MS, ?MAX_TIMEOUT_MS)]}}
              ]).

next_state(#state{cache=Cache, now_ms=NowMs}=Model, _V
          ,{'call', 'kz_cache', 'store_local', [?SERVER, Key, Value, Props]}
          ) ->
    Obj = #cache_obj{key=Key
                    ,value=Value
                    ,timestamp_ms=NowMs
                    ,expires_s=props:get_value('expires', Props)
                    },
    NewCache = case lists:keytake(Key, #cache_obj.key, Cache) of
                   'false' -> Cache;
                   {'value', _OldObj, TmpCache} -> TmpCache
               end,
    Model#state{cache=[Obj|NewCache]};
next_state(#state{cache=Cache}=Model
          ,V
          ,{'call', 'kz_cache', 'mitigate_stampede_local', [?SERVER, Key, Options]}
          ) ->
    case lists:keyfind(Key, #cache_obj.key, Cache) of
        #cache_obj{} -> Model;
        'false' ->
            next_state(Model, V
                      ,{'call', 'kz_cache', 'store_local', [?SERVER, Key, {kz_cache:mitigation_key(), self()}, Options]}
                      )

    end;
next_state(#state{}=Model, _V
          ,{'call', 'kz_cache', 'peek_local', [?SERVER, _Key]}
          ) ->
    %% nothing about our state changes when peeking
    Model;
next_state(#state{cache=Cache
                 ,now_ms=NowMs
                 }=Model, _V
          ,{'call', 'kz_cache', 'fetch_local', [?SERVER, Key]}
          ) ->
    %% bump the timestamp if the key exists
    case lists:keyfind(Key, #cache_obj.key, Cache) of
        'false' ->
            Model;
        #cache_obj{}=Obj ->
            Model#state{cache=lists:keyreplace(Key, #cache_obj.key, Cache, Obj#cache_obj{timestamp_ms=NowMs})}
    end;
next_state(#state{cache=Cache}=Model, _V
          ,{'call', 'kz_cache', 'erase_local', [?SERVER, Key]}
          ) ->
    case lists:keytake(Key, #cache_obj.key, Cache) of
        'false' ->
            Model;
        {'value', _Old, Cache1} ->
            Model#state{cache=Cache1}
    end;
next_state(#state{cache=Cache}=Model
          ,V
          ,{'call', 'kz_cache', 'wait_for_key_local', [?SERVER, Key, Timeout]}
          ) ->
    MKey = kz_cache:mitigation_key(),
    case lists:keyfind(Key, #cache_obj.key, Cache) of
        'false' ->
            next_state(Model, V, {'call', 'timer', 'sleep', [Timeout]});
        #cache_obj{value={MKey, _}} ->
            next_state(Model, V, {'call', 'timer', 'sleep', [Timeout]});
        #cache_obj{}=_Old ->
            Model
    end;
next_state(Model, V, {'call', 'kz_cache', 'wait_for_stampede_local', Args}) ->
    next_state(Model, V, {'call', 'kz_cache', 'wait_for_key_local', Args});
next_state(#state{cache=Cache
                 ,now_ms=ThenMs
                 }=Model, _V
          ,{'call', 'timer', 'sleep', [SleptMs]}
          ) ->
    NowMs = ThenMs + SleptMs,
    Model#state{cache=lists:foldl(fun(Obj, Acc) -> maybe_expire(Obj, Acc, NowMs) end, [], Cache)
               ,now_ms=NowMs
               }.

precondition(_Method, _Call) -> 'true'.

postcondition(#state{}
             ,{'call', 'kz_cache', 'store_local', [?SERVER, _Key, _Value, _Props]}
             ,'ok'
             ) ->
    'true';
postcondition(#state{cache=Cache}
             ,{'call', 'kz_cache', 'mitigate_stampede_local', [?SERVER, Key, _Props]}
             ,Result
             ) ->
    case lists:keyfind(Key, #cache_obj.key, Cache) of
        'false' ->      'ok' =:= Result;
        #cache_obj{} -> 'error' =:= Result
    end;
postcondition(#state{cache=Cache}
             ,{'call', 'kz_cache', 'fetch_local', [?SERVER, Key]}
             ,Return
             ) ->
    MKey = kz_cache:mitigation_key(),
    case lists:keyfind(Key, #cache_obj.key, Cache) of
        'false' -> {'error', 'not_found'} =:= Return;
        #cache_obj{value={MKey, _}=Return} -> 'true';
        #cache_obj{value=V} -> {'ok', V} =:= Return
    end;
postcondition(#state{cache=Cache}
             ,{'call', 'kz_cache', 'peek_local', [?SERVER, Key]}
             ,Result
             ) ->
    MKey = kz_cache:mitigation_key(),
    case lists:keyfind(Key, #cache_obj.key, Cache) of
        'false' -> {'error', 'not_found'} =:= Result;
        #cache_obj{value={MKey, _}=V} -> V =:= Result;
        #cache_obj{value=V} -> {'ok', V} =:= Result
    end;
postcondition(#state{cache=Cache}
             ,{'call', 'kz_cache', 'wait_for_key_local', [?SERVER, Key, _Timeout]}
             ,Result
             ) ->
    MKey = kz_cache:mitigation_key(),
    case lists:keyfind(Key, #cache_obj.key, Cache) of
        'false' ->
            {'error', 'timeout'} =:= Result;
        #cache_obj{value={MKey, _Pid}} ->
            {'error', 'timeout'} =:= Result;
        #cache_obj{value=Value} ->
            {'ok', Value} =:= Result
    end;
postcondition(State, {'call', 'kz_cache', 'wait_for_stampede_local', Args}, Result) ->
    postcondition(State, {'call', 'kz_cache', 'wait_for_key_local', Args}, Result);
postcondition(#state{}
             ,{'call', 'kz_cache', 'erase_local', [?SERVER, _Key]}
             ,'ok'
             ) ->
    'true';
postcondition(#state{}
             ,{'call', 'timer', 'sleep', [_Wait]}
             ,'ok'
             ) ->
    'true'.

maybe_expire(#cache_obj{expires_s=Expiry
                       ,timestamp_ms=TStamp
                       }=Obj
            ,NewCache
            ,NowMs
            ) ->
    case (Expiry*?MILLISECONDS_IN_SECOND) + TStamp of
        Expired when Expired < NowMs ->
            NewCache;
        _Expires ->
            [Obj | NewCache]
    end.

key() ->
    range($a, $z).

value() ->
    integer().

-endif.
