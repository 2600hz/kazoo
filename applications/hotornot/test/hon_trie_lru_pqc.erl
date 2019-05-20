%% invoke with `proper:quickcheck(hon_trie_lru_pqc:correct())` or `correct_parallel`
-module(hon_trie_lru_pqc).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").

-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-behaviour(proper_statem).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0
        ]).

-export([find_prefix/2
        ,recreate_parallel/1
        ]).

proper_seq_test_() ->
    {"Runs "?MODULE_STRING" PropEr sequential tests"
    ,{'timeout'
     ,120
     ,?_assert(proper:quickcheck(?MODULE:correct(), [{'to_file', 'user'}, 1]))
     }
    }.

proper_parallel_test_() ->
    {"Runs "?MODULE_STRING" PropEr parallel tests"
    ,{'timeout'
     ,120
     ,?_assert(proper:quickcheck(?MODULE:correct_parallel(), [{'to_file', 'user'}, 1]))
     }
    }.

-type prefix() :: pos_integer().
-type id() :: kz_term:ne_binary().
-type now_ms() :: non_neg_integer().

-type cache() :: [{prefix(), [{id(), now_ms()}]}].

-define(EXPIRES_S, 1). % expire after 1 second
-record(model, {cache = [] :: cache()
               ,now_ms = 0 :: now_ms()
               }
       ).

correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   hon_trie_lru:stop(?KZ_RATES_DB),
                   {'ok', _Pid} = hon_trie_lru:start_link(?KZ_RATES_DB, ?EXPIRES_S),
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   hon_trie_lru:stop(?KZ_RATES_DB),

                   ?WHENFAIL(begin
                                 {Zipped, Recreate} = print_zip(zip(Cmds, History)),
                                 ?debugFmt("~nLRU: ~p~n Final State: ~p~nZip:~n~s~nRecreate:~n~s.~n"
                                          ,[_Pid, State
                                           ,lists:reverse(Zipped)
                                           ,string:join(lists:reverse(Recreate), ",")
                                           ]
                                          )
                             end
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

args_to_list(Args) ->
    string:join([io_lib:format("~p", [A]) || A <- Args], ", ").

print_zip(Zip) ->
    io:format("~n"),
    StopStep = io_lib:format("hon_trie_lru:stop(~p)~n", [?KZ_RATES_DB]),
    StartStep = io_lib:format("hon_trie_lru:start_link(~p, ~p)~n", [?KZ_RATES_DB, ?EXPIRES_S]),

    {Print, Recreate} = lists:foldl(fun(Item, {Z, R}) -> {[print_zip_item(Item) | Z], [recreate_item(Item) | R]} end
                                   ,{[], [StartStep, StopStep]}
                                   ,Zip
                                   ),
    {Print, [StopStep | Recreate]}.

recreate_item({{set, _Var, {call, M, F, As}}
              ,{#model{cache=_Cache, now_ms=_NowMs}, Result}
              }) ->
    [io_lib:format("~p = ~s:~s(~s)", [Result, M, F, args_to_list(As)])].

print_zip_item({{set, _Var, {call, M, F, As}}
               ,{#model{cache=Cache, now_ms=NowMs}, Result}
               }) ->
    [io_lib:format("~6bms: cache: ~p~n", [NowMs, Cache])
    ,io_lib:format("~12s~ss:~s(~s) results in:~n", ["", M, F, args_to_list(As)])
    ,io_lib:format("~14s~p~n", ["", Result])
    ].

correct_parallel() ->
    ?FORALL(Cmds
           ,parallel_commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   hon_trie_lru:stop(?KZ_RATES_DB),
                   {'ok', _Pid} = hon_trie_lru:start_link(?KZ_RATES_DB, ?EXPIRES_S),
                   {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                   hon_trie_lru:stop(?KZ_RATES_DB),

                   ?WHENFAIL(?debugFmt("S: ~p~nP: ~p~ncmnd: ~p~n"
                                      ,[Sequential, Parallel, command_names(Cmds)]
                                      )
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

initial_state() ->
    #model{}.

command(#model{}) ->
    Ms = ?EXPIRES_S * ?MILLISECONDS_IN_SECOND,
    oneof([{'call', 'hon_trie', 'match_did', [phone_number(), 'undefined', ?KZ_RATES_DB]}
          ,{'call', 'hon_trie_lru', 'cache_rates', [?KZ_RATES_DB, [rate_doc()]]}
          ,{'call', 'timer', 'sleep', [oneof([Ms-200,Ms+200])]}
          ]).

next_state(#model{cache=Cache
                 ,now_ms=NowMs
                 }=Model
          ,_V
          ,{'call', 'hon_trie', 'match_did', [PhoneNumber, _AccountId, _RatedeckId]}
          ) ->
    case find_prefix(Cache, PhoneNumber) of
        'error' -> Model#model{now_ms=NowMs+1};
        {'ok', Prefix, RateIds} ->
            Model#model{cache=bump_matched(expire_rates(Cache, NowMs), NowMs, Prefix, RateIds)
                       ,now_ms=NowMs
                       }
    end;
next_state(#model{cache=Cache
                 ,now_ms=NowMs
                 }=Model
          ,_V
          ,{'call', 'hon_trie_lru', 'cache_rates', [_RatedeckId, RateDocs]}
          ) ->
    {UpdatedCache, NowMs} = cache_rates(Cache, NowMs, RateDocs),
    Model#model{cache=UpdatedCache
               ,now_ms=NowMs
               };
next_state(#model{now_ms=ThenMs
                 ,cache=Cache
                 }=Model
          ,_V
          ,{'call', 'timer', 'sleep', [SleepMs]}
          ) ->
    NowMs = ThenMs + SleepMs,
    Model#model{now_ms=NowMs
               ,cache=expire_rates(Cache, NowMs)
               }.

precondition(_Model, _Call) -> 'true'.

postcondition(#model{cache=Cache}
             ,{'call', 'hon_trie', 'match_did', [PhoneNumber, _AccountId, _RatedeckId]}
             ,{'error', 'not_found'}
             ) ->
    find_prefix(Cache, PhoneNumber) =:= 'error';
postcondition(#model{cache=Cache}
             ,{'call', 'hon_trie', 'match_did', [PhoneNumber, _AccountId, _RatedeckId]}
             ,{'ok', RateDocs}
             ) ->
    case find_prefix(Cache, PhoneNumber) of
        'error' -> 'false';
        {'ok', _Prefix, RateIds} ->
            length(RateDocs) =:= length(RateIds)
                andalso lists:all(fun(RateId) -> props:is_defined(RateId, RateIds) end, RateDocs)
    end;
postcondition(#model{}
             ,{'call', 'hon_trie_lru', 'cache_rates', [_RatedeckId, _Rates]}
             ,'ok'
             ) ->
    'true';
postcondition(#model{}
             ,{'call', 'timer', 'sleep', [_Wait]}
             ,'ok'
             ) ->
    'true'.

%% Generators
phone_number() ->
    oneof(["14158867900"
          ,"14168867900"
          ,"14268867900"
          ,"15158867900"
          ]).

rate_doc() ->
    oneof([?JSON_WRAPPER([{<<"prefix">>, <<"1">>}, {<<"id">>, <<"1">>}])
          ,?JSON_WRAPPER([{<<"prefix">>, <<"14">>}, {<<"id">>, <<"14">>}])
          ,?JSON_WRAPPER([{<<"prefix">>, <<"141">>}, {<<"id">>, <<"141">>}])
          ,?JSON_WRAPPER([{<<"prefix">>, <<"1415">>}, {<<"id">>, <<"1415">>}])
          ]).

%% Helpers
-spec cache_rates(cache(), now_ms(), kz_json:objects()) -> cache().
cache_rates(Cache, NowMs, RateDocs) ->
    lists:foldl(fun cache_rate/2, {Cache, NowMs}, RateDocs).

cache_rate(Rate, {Cache, NowMs}) ->
    Id = kz_doc:id(Rate),
    Prefix = kzd_rates:prefix(Rate),

    Rates = props:get_value(Prefix, Cache, []),
    NewRates = props:set_value(Id, NowMs, Rates),
    {props:set_value(Prefix, NewRates, Cache)
    ,NowMs
    }.

expire_rates(Cache, NowMs) ->
    OldestTimestamp = NowMs - (?EXPIRES_S * ?MILLISECONDS_IN_SECOND),
    {NewCache, _} = lists:foldl(fun expire_rate/2, {[], OldestTimestamp}, Cache),
    NewCache.

expire_rate({Prefix, Rates}, {Cache, OldestTimestamp}) ->
    case lists:filter(fun({_RateId, LastUsed}) -> LastUsed >= OldestTimestamp end
                     ,Rates
                     )
    of
        [] -> {Cache, OldestTimestamp};
        RatesToKeep -> {props:set_value(Prefix, RatesToKeep, Cache), OldestTimestamp}
    end.

bump_matched(Cache, NowMs, Prefix, RateIds) ->
    BumpedRateIds = [{Id, NowMs}
                     || {Id, _LastAccessed} <- RateIds
                    ],
    props:set_value(Prefix, BumpedRateIds, Cache).

-spec find_prefix(cache(), string()) ->
                         'error' |
                         {'ok', prefix(), any()}.
find_prefix(Cache, PhoneNumber) ->
    PNBin = kz_term:to_binary(PhoneNumber),
    case lists:foldl(fun longest_prefix/2, {PNBin, <<>>, 0, []}, Cache) of
        {_, <<>>, 0, []} -> 'error';
        {_, Prefix, _Len, Rates} ->
            {'ok', Prefix, Rates}
    end.

longest_prefix({Prefix, Rates}
              ,{PhoneNumber, _MatchingPrefix, MatchingLength, _MatchingRates}=Acc
              ) ->
    PrefixBin = kz_term:to_binary(Prefix),
    case binary:match(PhoneNumber, PrefixBin) of
        {0, PrefixLength} when PrefixLength > MatchingLength ->
            {PhoneNumber, Prefix, PrefixLength, Rates};
        _Else ->
            Acc
    end.

-endif.

-spec recreate_parallel(any()) -> any().
recreate_parallel({Seq, Parallel}) ->
    hon_trie_lru:stop(?KZ_RATES_DB),
    hon_trie_lru:start_link(?KZ_RATES_DB, ?EXPIRES_S),

    Vars = seq(Seq, []),
    parallel(Parallel, Vars),
    io:format('user', "vars: ~p", [Vars]),
    hon_trie_lru:stop(?KZ_RATES_DB).

seq([], Vars) -> Vars;
seq([{set, Var, {call, M, F, Args}} | Seq], Vars) ->
    Val = apply(M, F, Args),
    seq(Seq, [{Var, Val} | Vars]).

parallel(Parallel, Vars) ->
    [spawn(fun() -> seq(Seq, Vars) end) || Seq <- Parallel].
