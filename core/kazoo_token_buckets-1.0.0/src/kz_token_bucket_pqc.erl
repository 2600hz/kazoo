%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% The goal is to model how a token bucket works and, using PropEr,
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
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_token_bucket_pqc).

-ifdef(TEST).

-include("kz_buckets.hrl").

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(proper_statem).

-export([command/1
         ,initial_state/0
         ,next_state/3
         ,postcondition/3
         ,precondition/2

         ,correct/0
        ]).

-define(SERVER, 'kz_token_bucket').

-record(state, {current :: non_neg_integer()
                ,max :: pos_integer()
                ,fill_rate :: pos_integer()
                ,fill_rate_time = 'second' :: 'second'
                ,start = os:timestamp() :: wh_now()
                ,handled_ms = 0 :: non_neg_integer()
               }).

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing"
     ,{'timeout'
       ,15000
       ,[?_assertEqual('true'
                       ,proper:quickcheck(?MODULE:correct()
                                          ,[{'max_shrinks', 4}
                                            ,{'numtests', 10}
                                            ,{'to_file', 'user'}
                                           ]
                                         )
                      )
        ]
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

initial_state() -> 'ok'.

command('ok') ->
    {'call', ?SERVER, 'start_link', [?SERVER, pos_integer(), pos_integer(), 'true', 'second']};
command(#state{}=_Model) ->
    oneof([{'call', ?SERVER, 'consume', [?SERVER, integer()]}
           ,{'call', ?SERVER, 'tokens', [?SERVER]}
           ,{'call', 'timer', 'sleep', [range(1000,5000)]}
          ]).

next_state('ok'
           ,_V
           ,{'call', ?SERVER, 'start_link', [_Server, Max, FR, _, FRT]}
          ) ->
    #state{current=Max
           ,max=Max
           ,fill_rate=FR
           ,fill_rate_time=FRT
           ,start=os:timestamp()
           ,handled_ms=0
          };
next_state(#state{}=Model
           ,_V
           ,{'call', _Server, 'consume', [_Server, Ts]}
          ) ->
    tokens_consume(Ts
                   ,adjust_for_time(Model)
                  );
next_state(#state{}=Model
           ,_V
           ,{'call', _Server, 'tokens', [_Server]}
          ) ->
    adjust_for_time(Model);
next_state(#state{}=Model
           ,_V
           ,{'call', 'timer', 'sleep', [_Wait]}
          ) ->
    adjust_for_time(Model).

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
              ,{'call', _Server, 'consume', [_Server, Ts]}
              ,Result
             ) when Ts =< 0 ->
    'true' == Result;
postcondition(#state{current=B}
              ,{'call', _Server, 'consume', [_Server, Ts]}
              ,Result
             ) when Ts =< B ->
    'true' == Result;
postcondition(#state{}
              ,{'call', _Server, 'consume', [_Server, _Ts]}
              ,Result
             ) ->
    'false' == Result;
postcondition(#state{current=B}
              ,{'call', _Server, 'tokens', [_Server]}
              ,Ts
             ) ->
    B == Ts;
postcondition(#state{current=_B
                     ,max=_M
                     ,fill_rate=_FR
                    }
              ,{'call', 'timer', 'sleep', [_Wait]}
              ,'ok'
             ) ->
    'true';
    %% case wait_tokens(Wait, FR) + B of
    %%     N when N > M -> ?SERVER:tokens(?SERVER) == M;
    %%     N -> ?SERVER:tokens(?SERVER) == N
    %% end;
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

maybe_add_tokens(Model, 0) -> Model;
maybe_add_tokens(#state{current=T
                        ,max=Max
                       }=Model
                 ,ToAdd
                ) ->
    case T + ToAdd of
        N when N > Max -> Model#state{current=Max};
        N -> Model#state{current=N}
    end.

wait_tokens(Wait, FR) ->
    ((Wait div 1000) * FR).

adjust_for_time(#state{start=TS, handled_ms=Hs}=Model) ->
    case elapsed_ms(TS) of
        N when (N-Hs) < 1000 -> Model;
        N -> adjust_for_time(Model, N)
    end.

adjust_for_time(#state{fill_rate=FR
                       ,handled_ms=Handled
                      }=Model
                ,Elapsed
               ) ->
    case wait_tokens(Elapsed-Handled, FR) of
        0 -> Model;
        ToAdd ->
            Model1 = maybe_add_tokens(Model, ToAdd),
            Model1#state{handled_ms=Elapsed+Handled}
    end.

elapsed_ms(Start) ->
    elapsed_ms(Start, os:timestamp()).

elapsed_ms({_,_,_}=Start, {_,_,_}=Now) ->
    timer:now_diff(Now, Start) div 1000.

%% We want to consume up to N tokens, or until T is 0
%% tokens_consume_until(N, Model) when N =< 0 ->
%%     Model;
%% tokens_consume_until(N, {T, M, FR, FRT}) when N > T ->
%%     {0, M, FR, FRT};
%% tokens_consume_until(N, {T, M, FR, FRT}) ->
%%     {T-N, M, FR, FRT}.

-endif.
