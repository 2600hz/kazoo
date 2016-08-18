%% This test requires a running Kazoo node
%% invoke with `proper:quickcheck(kz_cache_pqc:correct())` or `correct_parallel`
-module(kz_cache_pqc).

-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").

-include_lib("kazoo_caches/include/kazoo_caches.hrl").

-behaviour(proper_statem).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0
        ]).

-define(SERVER, ?MODULE).

-record(state, {cache = [] :: cache_objs()
               ,now_ms = 0 :: pos_integer()
               }).

correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   kz_util:put_callid(?MODULE),
                   kz_cache:stop_local(?SERVER),
                   {'ok', P} = kz_cache:start_link(?SERVER, [{'origin_bindings', [[]]}]),
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   kz_cache:stop_local(P),
                   ?WHENFAIL(io:format("Final State of ~p: ~p\nFailing Cmds: ~p\n"
                                      ,[P, State, zip(Cmds, History)]
                                      )
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

correct_parallel() ->
    ?FORALL(Cmds
           ,parallel_commands(?MODULE),
            begin
                kz_cache:stop_local(?SERVER),
                {'ok', P} = kz_cache:start_link(?SERVER, [{'origin_bindings', [[]]}]),
                {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                kz_cache:stop_local(P),
                ?WHENFAIL(io:format("Failing Cmds: ~p\nS: ~p\nP: ~p\n"
                                   ,[Cmds, Sequential, Parallel]
                                   )
                         ,aggregate(command_names(Cmds), Result =:= 'ok')
                         )
            end).

initial_state() ->
    #state{cache=[]
          ,now_ms=0
          }.

command(#state{}=_Model) ->
    frequency([{10, {'call', 'kz_cache', 'store_local', [?SERVER, key(), value(), [{'expires', range(4,10)}]]}}
              ,{1, {'call', 'kz_cache', 'peek_local', [?SERVER, key()]}}
              ,{1, {'call', 'kz_cache', 'fetch_local', [?SERVER, key()]}}
              ,{5, {'call', 'kz_cache', 'erase_local', [?SERVER, key()]}}
              ,{2, {'call', 'timer', 'sleep', [range(1000,15000)]}}
              ]).

next_state(#state{cache=Cache, now_ms=NowMs}=Model, _V
          ,{'call', 'kz_cache', 'store_local', [?SERVER, Key, Value, Props]}
          ) ->
    Obj = #cache_obj{key=Key
                    ,value=Value
                    ,timestamp=NowMs
                    ,expires=proplists:get_value('expires', Props)
                    },
    NewCache = case lists:keytake(Key, #cache_obj.key, Cache) of
                   'false' -> Cache;
                   {'value', _OldObj, TmpCache} -> TmpCache
               end,
    Model#state{cache=[Obj|NewCache]};
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
    case lists:keysearch(Key, #cache_obj.key, Cache) of
        'false' ->
            Model;
        {'value', #cache_obj{}=Obj} ->
            Model#state{cache=lists:keyreplace(Key, #cache_obj.key, Cache, Obj#cache_obj{timestamp=NowMs})}
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
             ,{'call', 'kz_cache', 'fetch_local', [?SERVER, Key]}
             ,{'ok', Value}
             ) ->
    case lists:keysearch(Key, #cache_obj.key, Cache) of
        'false' -> 'false';
        {'value', Obj} ->
            Obj#cache_obj.value =:= Value
    end;
postcondition(#state{cache=Cache}
             ,{'call', 'kz_cache', 'fetch_local', [?SERVER, Key]}
             ,{'error', 'not_found'}
             ) ->
    case lists:keysearch(Key, #cache_obj.key, Cache) of
        'false' -> 'true';
        {'value', _} -> 'false'
    end;
postcondition(#state{cache=Cache}
             ,{'call', 'kz_cache', 'peek_local', [?SERVER, Key]}
             ,{'ok', Value}
             ) ->
    case lists:keysearch(Key, #cache_obj.key, Cache) of
        'false' -> 'false';
        {'value', Obj} ->
            Obj#cache_obj.value =:= Value
    end;
postcondition(#state{cache=Cache}
             ,{'call', 'kz_cache', 'peek_local', [?SERVER, Key]}
             ,{'error', 'not_found'}
             ) ->
    case lists:keysearch(Key, #cache_obj.key, Cache) of
        'false' -> 'true';
        {'value', _} -> 'false'
    end;
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

maybe_expire(#cache_obj{expires=Expiry
                       ,timestamp=TStamp
                       }=Obj
            ,NewCache
            ,NowMs
            ) ->
    case (Expiry*1000) + TStamp of
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
