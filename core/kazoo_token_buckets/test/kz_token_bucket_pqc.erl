%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc The goal is to model how a token bucket works and, using PropEr,
%%% generate test scenarios that prove the properties.
%%%
%%% Our model is a bucket of tokens, starting (and capped) at MAX,
%%% with a refill rate of FR per unit time FRT. We will represent this
%%% as a tuple {TOKENS, MAX, FR, FRT}.
%%%
%%% We want to test that consuming tokens works as long as the requested
%%% amount is < TOKENS, that the bucket refills at FR/FRT, and that the
%%% bucket cannot exceed MAX tokens.
%%%
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_token_bucket_pqc).

-define(DO_NOT_RUN_QC_TOKEN_BUCKETS, true).
-ifndef(DO_NOT_RUN_QC_TOKEN_BUCKETS).

%% -include_lib("kazoo_token_buckets/src/kz_buckets.hrl").

-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(proper_statem).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0
        ]).

-define(SERVER, 'kz_token_bucket').

-record(state, {current :: non_neg_integer()
               ,max :: pos_integer()
               ,fill_rate :: pos_integer()
               ,fill_rate_time = 'second' :: 'second'
               ,handled_ms = 0 :: non_neg_integer()
               }).

sequential_test_() ->
    {"Running sequential PropEr tests"
    ,{'timeout'
     ,50000
     ,?_assertEqual('true'
                   ,proper:quickcheck(correct()
                                     ,[{'max_shrinks', 4}
                                      ,{'numtests', 50}
                                      ,{'to_file', 'user'}
                                      ]
                                     )
                   )
     }
    }.

parallel_test_() ->
    {"Running parallel PropEr tests"
    ,{'timeout'
     ,50000
     ,?_assertEqual('true'
                   ,proper:quickcheck(correct_parallel()
                                     ,[{'max_shrinks', 4}
                                      ,{'numtests', 50}
                                      ,{'to_file', 'user'}
                                      ]
                                     )
                   )
     }
    }.

correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   {History, _State, Result} = run_commands(?MODULE, Cmds),
                   ?SERVER:stop(?SERVER),
                   ?WHENFAIL(io:format('user'
                                      ,"Failing Cmds: ~p\n"
                                      ,[zip(Cmds, History)]
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
                {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                ?SERVER:stop(?SERVER),
                ?WHENFAIL(io:format('user'
                                   ,"Failing Cmds: ~p\nS: ~p\nP: ~p\n"
                                   ,[Cmds, Sequential, Parallel]
                                   )
                         ,aggregate(command_names(Cmds), Result =:= 'ok')
                         )
            end).

initial_state() -> 'ok'.

command('ok') ->
    frequency([{1, {'call'
                   ,?SERVER
                   ,'start_link'
                   ,[?SERVER, pos_integer(), pos_integer(), 'true', 'second']
                   }
               }
              ]);
command(#state{}=_Model) ->
    oneof([{'call', ?SERVER, 'consume', [?SERVER, integer()]}
          ,{'call', ?SERVER, 'consume_until', [?SERVER, integer()]}
          ,{'call', ?SERVER, 'tokens', [?SERVER]}
          ,{'call', ?SERVER, 'credit', [?SERVER, pos_integer()]}
          ,{'call', 'timer', 'sleep', [range(800,5200)]}
          ]).

next_state('ok'
          ,_V
          ,{'call', ?SERVER, 'start_link', [_Server, Max, FR, _, FRT]}
          ) ->
    #state{current=Max
          ,max=Max
          ,fill_rate=FR
          ,fill_rate_time=FRT
          ,handled_ms=0
          };
next_state(#state{}=Model
          ,_V
          ,{'call', _Server, 'consume', [_Server, Ts]}
          ) ->
    tokens_consume(Ts
                  ,Model
                  );
next_state(#state{}=Model
          ,_V
          ,{'call', _Server, 'consume_until', [_Server, Ts]}
          ) ->
    tokens_consume_until(Ts
                        ,Model
                        );
next_state(#state{}=Model
          ,_V
          ,{'call', _Server, 'credit', [_Server, Ts]}
          ) ->
    tokens_credit(Ts, Model);
next_state(#state{}=Model
          ,_V
          ,{'call', _Server, 'tokens', [_Server]}
          ) ->
    Model;
next_state(#state{}=Model
          ,_V
          ,{'call', 'timer', 'sleep', [Wait]}
          ) ->
    adjust_for_time(Model, Wait).

precondition('ok', {'call', ?SERVER, 'start_link', _Args}) ->
    'true';
precondition('ok', _Call) -> 'false';
precondition(_Method, _Call) -> 'true'.

postcondition('ok'
             ,{'call', _Server, 'start_link', _Args}
             ,Result
             ) ->
    {'ok', P} = Result,
    is_pid(P);
postcondition(#state{}
             ,{'call', _Server, Fun, [_Server, Ts]}
             ,Result
             ) when Ts =< 0
                    andalso (Fun == 'consume'
                             orelse Fun == 'consume_until'
                            ) ->
    'true' == Result;
postcondition(#state{current=B}
             ,{'call', _Server, Fun, [_Server, Ts]}
             ,Result
             ) when Ts =< B
                    andalso (Fun == 'consume'
                             orelse Fun == 'consume_until'
                            ) ->
    'true' == Result;
postcondition(#state{}
             ,{'call', _Server, Fun, [_Server, _Ts]}
             ,Result
             ) when Fun == 'consume'
                    orelse Fun == 'consume_until'
                    ->
    'false' == Result;
postcondition(#state{}
             ,{'call', _Server, 'credit', [_Server, _Ts]}
             ,Result
             ) ->
    'ok' == Result;
postcondition(#state{current=B
                    ,max=Max
                    ,fill_rate=FR
                    }
             ,{'call', _Server, 'tokens', [_Server]}
             ,Ts
             ) ->
    B == Ts
        orelse (B+FR) == Ts
        orelse Max == Ts;
postcondition(#state{current=_B
                    ,max=_M
                    ,fill_rate=_FR
                    }
             ,{'call', 'timer', 'sleep', [_Wait]}
             ,'ok'
             ) ->
    'true';
postcondition(#state{}=_Model, _Call, _Res) ->
    _Res.

%% We want to consume N tokens if there are enough
%% otherwise don't consume them
%% if N is <= 0, leave the bucket alone
tokens_consume(N, Model) when N =< 0 ->
    Model;
tokens_consume(N, #state{current=T}=Model) when N > T ->
    Model;
tokens_consume(N, #state{current=T}=Model) ->
    Model#state{current=T-N}.

%% We want to consume N tokens if there are enough
%% otherwise don't consume them
%% if N is <= 0, leave the bucket alone
tokens_consume_until(N, Model) when N =< 0 ->
    Model;
tokens_consume_until(N, #state{current=T}=Model) when N > T ->
    Model#state{current=0};
tokens_consume_until(N, #state{current=T}=Model) ->
    Model#state{current=T-N}.

maybe_add_tokens(#state{current=T
                       ,max=Max
                       }=Model
                ,ToAdd
                ) ->
    case T + ToAdd of
        N when N > Max -> Model#state{current=Max};
        N -> Model#state{current=N}
    end.

adjust_for_time(#state{handled_ms=Handled
                      ,fill_rate=FR
                      }=Model
               ,Wait
               ) ->
    case Handled + Wait of
        N when N < 1000 -> Model#state{handled_ms=N};
        N ->
            adjust_for_time(Model, N, wait_tokens(N, FR))
    end.

adjust_for_time(Model
               ,Elapsed
               ,Tokens
               ) ->
    Model1 = maybe_add_tokens(Model, Tokens),
    Model1#state{handled_ms=(Elapsed rem 1000)}.

wait_tokens(Wait, FR) ->
    ((Wait div 1000) * FR).

tokens_credit(Credit, #state{current=T
                            ,max=M
                            }=Model) ->
    case Credit + T of
        N when N > M -> Model#state{current=M};
        N -> Model#state{current=N}
    end.

-endif.

-endif.
