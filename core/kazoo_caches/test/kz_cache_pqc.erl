-module(kz_cache_pqc).

-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("whistle/include/wh_log.hrl").
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

%% sequential_test_() ->
%%     {"Running sequential PropEr tests"
%%     ,{'timeout'
%%      ,50000
%%      ,?_assertEqual('true'
%%                    ,proper:quickcheck(?MODULE:correct()
%%                                      ,[{'to_file', 'user'}]
%%                                      )
%%                    )
%%      }
%%     }.

%% parallel_test_() ->
%%     {"Running parallel PropEr tests"
%%      ,{'timeout'
%%        ,50000
%%        ,?_assertEqual('true'
%%                       ,proper:quickcheck(?MODULE:correct_parallel()
%%                                         ,[{'to_file', 'user'}]
%%                                         )
%%                      )
%%       }
%%     }.

correct() ->
    ?FORALL(Cmds
            ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   wh_util:put_callid(?MODULE),
                   {'ok', P} = kz_cache:start_link(?SERVER, [{'origin_bindings', [[]]}]),
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   kz_cache:stop_local(P),
                   ?WHENFAIL(io:format('user'
                                      ,"Final State of ~p: ~p\nFailing Cmds: ~p\n"
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
                {'ok', P} = kz_cache:start_link(?SERVER, [{'origin_bindings', [[]]}]),
                {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                kz_cache:stop_local(P),
                ?WHENFAIL(io:format('user'
                                   ,"Failing Cmds: ~p\nS: ~p\nP: ~p\n"
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
    frequency([{5, {'call', 'kz_cache', 'store_local', [?SERVER, key(), value(), [{'expires', range(4,10)}]]}}
              ,{1, {'call', 'kz_cache', 'peek_local', [?SERVER, key()]}}
              ,{1, {'call', 'kz_cache', 'fetch_local', [?SERVER, key()]}}
              ,{1, {'call', 'kz_cache', 'erase_local', [?SERVER, key()]}}
              ,{2, {'call', 'timer', 'sleep', [range(1000,5000)]}}
              ]).

next_state(#state{cache=Cache, now_ms=NowMs}=Model, _V
          ,{'call', 'kz_cache', 'store_local', [?SERVER, Key, Value, Props]}
          ) ->
    lager:debug("ns: store_local ~p ~p", [Key, Value]),
    lager:debug("ns: old cache: ~p", [Cache]),
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
    lager:debug("ns: peek_local ~p", [_Key]),
    Model;
next_state(#state{cache=Cache
                 ,now_ms=NowMs
                 }=Model, _V
          ,{'call', 'kz_cache', 'fetch_local', [?SERVER, Key]}
          ) ->
    %% bump the timestamp if the key exists
    case lists:keysearch(Key, #cache_obj.key, Cache) of
        'false' ->
            lager:debug("ns: key ~p not fetched, no change", [Key]),
            Model;
        {'value', #cache_obj{}=Obj} ->
            lager:debug("ns: key ~p fetched, bumping timestamp to ~p", [Key, NowMs]),
            Model#state{cache=lists:keyreplace(Key, #cache_obj.key, Cache, Obj#cache_obj{timestamp=NowMs})}
    end;
next_state(#state{cache=Cache}=Model, _V
          ,{'call', 'kz_cache', 'erase_local', [?SERVER, Key]}
          ) ->
    case lists:keytake(Key, #cache_obj.key, Cache) of
        'false' ->
            lager:debug("ns: key ~p not found to erase", [Key]),
            Model;
        {'value', _Old, Cache1} ->
            lager:debug("removed ~p from cache, now ~p", [_Old, Cache1]),
            Model#state{cache=Cache1}
    end;
next_state(#state{cache=Cache
                 ,now_ms=ThenMs
                 }=Model, _V
          ,{'call', 'timer', 'sleep', [SleptMs]}
          ) ->
    NowMs = ThenMs + SleptMs,
    lager:debug("ns: sleeping ~pms (moving now to ~p", [SleptMs, NowMs]),
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
    lager:debug("pc: fetching ~p got back ~p, ~p stored", [Key, Value, lists:keysearch(Key, #cache_obj.key, Cache)]),
    {'value', Obj} = lists:keysearch(Key, #cache_obj.key, Cache),
    lager:debug("pc: fetched ~p ~p", [Obj, Obj#cache_obj.value =:= Value]),
    Obj#cache_obj.value =:= Value;
postcondition(#state{cache=Cache}
             ,{'call', 'kz_cache', 'fetch_local', [?SERVER, Key]}
             ,{'error', 'not_found'}
             ) ->
    'false' =:= lists:keysearch(Key, #cache_obj.key, Cache);
postcondition(#state{cache=Cache}
             ,{'call', 'kz_cache', 'peek_local', [?SERVER, Key]}
             ,{'ok', Value}
             ) ->
    lager:debug("pc: peeking ~p got back ~p, ~p stored", [Key, Value, lists:keysearch(Key, #cache_obj.key, Cache)]),
    {'value', Obj} = lists:keysearch(Key, #cache_obj.key, Cache),
    lager:debug("pc: peeked ~p ~p", [Obj, Obj#cache_obj.value =:= Value]),
    Obj#cache_obj.value =:= Value;
postcondition(#state{cache=Cache}
             ,{'call', 'kz_cache', 'peek_local', [?SERVER, Key]}
             ,{'error', 'not_found'}
             ) ->
    'false' =:= lists:keysearch(Key, #cache_obj.key, Cache);
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
                       ,key=_Key
                       }=Obj
            ,NewCache
            ,NowMs
            ) ->
    case (Expiry*1000) + TStamp of
        Expired when Expired < NowMs ->
            lager:debug("ns: ~p: expiring ~p: ~p(~p)~n", [self(), _Key, TStamp, Expiry]),
            NewCache;
        _Expires ->
            lager:debug("ns: not expiring ~p", [Obj]),
            [Obj | NewCache]
    end.

key() ->
    range($a, $z).

value() ->
    integer().

-endif.
