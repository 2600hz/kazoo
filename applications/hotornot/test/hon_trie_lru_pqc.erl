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

-define(EXPIRES_S, 2). % expire after 2 seconds
-record(model, {cache = []
               ,now_ms = 0
               ,next_purge = ?EXPIRES_S * 1000
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

                   ?WHENFAIL(?debugFmt("~nLRU: ~p~n Final State: ~p~nRecreate:~n~s~nZip:~n~s~n"
                                      ,[_Pid, State, recreate_steps(Cmds), print_zip(zip(Cmds, History))]
                                      )
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

recreate_steps(Steps) ->
    string:join([recreate_step(Step) || Step <- Steps], ",").

recreate_step({'set', _Var, {'call', M, F, As}}) ->
    io_lib:format("~s:~s(~s)~n", [M, F, args_to_list(As)]).

args_to_list(Args) ->
    string:join([io_lib:format("~p", [A]) || A <- Args], ", ").

print_zip(Zip) ->
    [print_zip_item(Item) || Item <- Zip].

print_zip_item({{set, _Var, {call, M, F, As}}
               ,{#model{cache=Cache, now_ms=NowMs}, Result}
               }) ->
    io_lib:format("~6b: cache: ~p calling ~s:~s(~s) results in ~p~n"
                 ,[NowMs, Cache, M, F, args_to_list(As), Result]
                 ).

correct_parallel() ->
    ?FORALL(Cmds
           ,parallel_commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   hon_trie_lru:stop(?KZ_RATES_DB),
                   {'ok', _Pid} = hon_trie_lru:start_link(?KZ_RATES_DB, ?EXPIRES_S),
                   {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                   hon_trie_lru:stop(?KZ_RATES_DB),

                   ?WHENFAIL(?debugFmt("Failing Cmds: ~p\nS: ~p\nP: ~p\n"
                                      ,[Cmds, Sequential, Parallel]
                                      )
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

-type cache() :: [{kz_term:ne_binary(), {kz_term:ne_binary(), kz_time:gregorian_seconds()}}].
initial_state() ->
    #model{}.

command(#model{}) ->
    Ms = ?EXPIRES_S * ?MILLISECONDS_IN_SECOND,
    oneof([{'call', 'hon_trie', 'match_did', [phone_number(), 'undefined', ?KZ_RATES_DB]}
          ,{'call', 'hon_trie_lru', 'cache_rates', [?KZ_RATES_DB, [rate_doc()]]}
          ,{'call', 'timer', 'sleep', [range(Ms-100,Ms+100)]}
          ]).

next_state(#model{now_ms=NowMs
                 ,next_purge=NextPurge
                 ,cache=Cache
                 }=Model
          ,V
          ,Call
          ) when NextPurge < NowMs ->
    next_state(Model#model{cache=expire_rates(Cache, NowMs)
                          ,next_purge=NextPurge + (?EXPIRES_S * 1000)
                          }
              ,V
              ,Call
              );
next_state(#model{cache=Cache
                 ,now_ms=NowMs
                 }=Model
          ,_V
          ,{'call', 'hon_trie', 'match_did', [PhoneNumber, _AccountId, _RatedeckId]}
          ) ->
    case find_prefix(Cache, PhoneNumber) of
        'error' -> Model;
        {'ok', Prefix, RateIds} ->
            Model#model{cache=bump_matched(Cache, NowMs, Prefix, RateIds)}
    end;
next_state(#model{cache=Cache
                 ,now_ms=NowMs
                 }=Model
          ,_V
          ,{'call', 'hon_trie_lru', 'cache_rates', [_RatedeckId, RateDocs]}
          ) ->
    {UpdatedCache, NowMs} = cache_rates(Cache, NowMs, RateDocs),
    Model#model{cache=UpdatedCache};
next_state(#model{now_ms=ThenMs}=Model
          ,_V
          ,{'call', 'timer', 'sleep', [SleepMs]}
          ) ->
    NowMs = ThenMs + SleepMs,
    Model#model{now_ms=NowMs}.

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
cache_rates(Cache, NowMs, RateDocs) ->
    lists:foldl(fun cache_rate/2, {Cache, NowMs}, RateDocs).

cache_rate(Rate, {Cache, NowMs}) ->
    Id = kz_doc:id(Rate),
    Prefix = kz_term:to_list(kzd_rates:prefix(Rate)),

    Rates = props:get_value(Prefix, Cache, []),
    NewRates = props:insert_value(Id, NowMs, Rates),
    {props:set_value(Prefix, NewRates, Cache)
    ,NowMs
    }.

expire_rates(Cache, NowMs) ->
    OldestTimestamp = NowMs - (?EXPIRES_S * ?MILLISECONDS_IN_SECOND),
    {NewCache, _} = lists:foldl(fun expire_rate/2, {[], OldestTimestamp}, Cache),
    NewCache.

expire_rate({Prefix, Rates}, {Cache, OldestTimestamp}) ->
    case [RateId || {RateId, LastUsed} <- Rates,
                    LastUsed < OldestTimestamp
         ]
    of
        [] -> {props:set_value(Prefix, Rates, Cache), OldestTimestamp};
        [_|_]=_OldRates -> {Cache, OldestTimestamp}
    end.

bump_matched(Cache, NowMs, Prefix, RateIds) ->
    BumpedRateIds = [{Id, NowMs}
                     || {Id, _LastAccessed} <- RateIds
                    ],
    props:set_value(Prefix, BumpedRateIds, Cache).

-spec find_prefix(cache(), string()) ->
                         'error' |
                         {'ok', string(), any()}.
find_prefix(Cache, PhoneNumber) ->
    PNBin = kz_term:to_binary(PhoneNumber),
    case lists:foldl(fun longest_prefix/2, {PNBin, <<>>, 0, []}, Cache) of
        {_, <<>>, 0, []} -> 'error';
        {_, Prefix, _Len, Rates} ->
            {'ok', kz_term:to_list(Prefix), Rates}
    end.

longest_prefix({Prefix, Rates}
              ,{PhoneNumber, _MatchingPrefix, MatchingLength, _MatchingRates}=Acc
              ) ->
    PrefixBin = kz_term:to_binary(Prefix),
    case binary:match(PhoneNumber, PrefixBin) of
        {0, PrefixLength} when PrefixLength > MatchingLength ->
            {PhoneNumber, Prefix, PrefixLength, Rates};
        _ -> Acc
    end.

-endif.
