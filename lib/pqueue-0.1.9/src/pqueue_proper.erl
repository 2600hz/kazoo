-module(pqueue_proper).

-include_lib("proper/include/proper.hrl").

-behaviour(proper_statem).

-export([qc_pq/0, qc_pq2/0, qc_pq3/0, qc_pq4/0, correct/1]).

-export([command/1, initial_state/0, next_state/3, postcondition/3,
         precondition/2]).

-type value() :: integer().
-record(state, { in_queue :: [{value(), term()}] }).
-define(SERVER, queue_srv).

priority() ->
    integer(-20, 20).

%% Selects priorities we have added
priority(InQ) ->
    elements([P || {P, _} <- InQ]).

value() ->
    integer().

initial_state() ->
    #state { in_queue = [] }.

command(#state { in_queue = InQ }) ->
    oneof([{call, ?SERVER, in, [value()]},
           {call, ?SERVER, in, [value(), priority()]},
           {call, ?SERVER, is_empty, []},
           {call, ?SERVER, is_queue, []},
           {call, ?SERVER, len, []},
           {call, ?SERVER, out, []}] ++
          [{call, ?SERVER, out, [priority(InQ)]} || InQ =/= []] ++
          [{call, ?SERVER, pout, []},
           {call, ?SERVER, to_list, []}]).

next_state(#state { in_queue = InQ } = S, _V, {call, _, out, []}) ->
    S#state { in_queue = listq_rem(InQ) };
next_state(#state { in_queue = InQ } = S, _V, {call, _, out, [Prio]}) ->
    S#state { in_queue = listq_rem(InQ, Prio) };
next_state(#state { in_queue = InQ } = S, _V, {call, _, pout, _}) ->
    S#state { in_queue = listq_rem(InQ) };
next_state(S, _V, {call, _, to_list, _}) -> S;
next_state(S, _V, {call, _, is_queue, _}) -> S;
next_state(S, _V, {call, _, is_empty, _}) -> S;
next_state(S, _V, {call, _, len, _}) -> S;
next_state(#state { in_queue = InQ } = S, _V, {call, _, in, [Value, Prio]}) ->
    S#state { in_queue = listq_insert({Prio, Value}, InQ) };
next_state(#state { in_queue = InQ } = S, _V, {call, _, in, [Value]}) ->
    S#state { in_queue = listq_insert({0, Value}, InQ) }.

precondition(_S, _Call) ->
    true. % No limitation on the things we can call at all.

postcondition(#state { in_queue = InQ }, {call, _, out, [Prio]}, R) ->
    R == listq_prio_peek(InQ, Prio);
postcondition(#state { in_queue = InQ }, {call, _, pout, _}, R) ->
    R == listq_ppeek(InQ);
postcondition(#state { in_queue = InQ }, {call, _, out, _}, R) ->
    R == listq_peek(InQ);
postcondition(S, {call, _, to_list, _}, R) ->
    R == listq_to_list(S#state.in_queue);
postcondition(S, {call, _, len, _}, L) ->
    L == listq_length(S#state.in_queue);
postcondition(_S, {call, _, is_queue, _}, true) -> true;
postcondition(S, {call, _, is_empty, _}, Res) ->
    Res == (S#state.in_queue == []);
postcondition(_S, {call, _, in, _}, _) ->
    true;
postcondition(_, _, _) ->
    false.

correct(M) ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
                begin
                    ?SERVER:start_link(M),
                    {History,State,Result} = run_commands(?MODULE, Cmds),
                    ?SERVER:stop(),
                    ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                        [History,State,Result]),
                              aggregate(command_names(Cmds), Result =:= ok))
                end)).

qc_opts() ->
    [{numtests, 1500}].
     
qc_pq() ->
    proper:quickcheck(pqueue_proper:correct(pqueue), qc_opts()).

qc_pq2() ->
    proper:quickcheck(pqueue_proper:correct(pqueue2), qc_opts()).

qc_pq3() ->
    proper:quickcheck(pqueue_proper:correct(pqueue3), qc_opts()).

qc_pq4() ->
    proper:quickcheck(pqueue_proper:correct(pqueue4), qc_opts()).

%% ----------------------------------------------------------------------

%% A listq is a sorted list of priorities
listq_insert({P, V}, []) ->
    [{P, [V]}];
listq_insert({P, V}, [{P1, _} | _] = LQ) when P < P1 ->
    [{P, [V]} | LQ];
listq_insert({P, V}, [{P1, Vs} | Next]) when P == P1 ->
    [{P, Vs ++ [V]} | Next];
listq_insert({P, V}, [{P1, Vs} | Next]) when P > P1 ->
    [{P1, Vs} | listq_insert({P, V}, Next)].

listq_to_list(L) ->
    lists:concat(
      [ Vals || {_Prio, Vals} <- L]).

listq_length(L) ->
    lists:sum(
      [ length(Vs) || {_Prio, Vs} <- L]).

listq_rem([]) ->
    [];
listq_rem([{_P, [_V]} | Next]) ->
    Next;
listq_rem([{P, [_V1 | Vs]} | Next]) ->
    [{P, Vs} | Next].

listq_rem([], _P) ->
    [];
listq_rem([{P, [_]} | Next], P) ->
    Next;
listq_rem([{P, [_ | Vs]} | Next], P) ->
    [{P, Vs} | Next];
listq_rem([{P1, Vs} | Next], P) ->
    [{P1, Vs} | listq_rem(Next, P)].

listq_peek([]) ->
    empty;
listq_peek([{_P, [V | _]} | _]) ->
    {value, V}.

listq_prio_peek([{P, [V | _]} | _], P) ->
    {value, V};
listq_prio_peek([{_P1, _} | Next], P) ->
    listq_prio_peek(Next, P);
listq_prio_peek([], _P) ->
    empty.

listq_ppeek([]) ->
    empty;
listq_ppeek([{P, [V | _]} | _]) ->
    {value, V, P}.


