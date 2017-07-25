-module(pqc_util).

-export([transition_if/2
        ,run_counterexample/1
        ]).

-include("kazoo_proper.hrl").

-spec transition_if(pqc_kazoo_model:model(), [{fun(), list()}]) -> pqc_kazoo_model:model().
transition_if(CurrentModel, Checks) ->
    case lists:foldl(fun transition_if_fold/2, {'true', CurrentModel}, Checks) of
        {'true', UpdatedModel} -> UpdatedModel;
        {'false', _} -> CurrentModel
    end.

-spec transition_if_fold({fun(), list()}, {boolean(), pqc_kazoo_model:model()}) ->
                                {boolean(), pqc_kazoo_model:model()}.
transition_if_fold({_Fun, _Args}, {'false', _}=False) -> False;
transition_if_fold({Fun, Args}, {'true', Model}) ->
    case apply(Fun, [Model | Args]) of
        'false' -> {'false', Model};
        'true' -> {'true', Model};
        {'true', _NewState}=True -> True;
        NewModel -> {'true', NewModel}
    end.

-spec run_counterexample(module()) -> {integer(), module(), any()}.
run_counterexample(PQC) ->
    run_counterexample(PQC, proper:counterexample(), PQC:initial_state()).
run_counterexample(PQC, [{Seq, Threads}], State) ->
    Steps = lists:usort(fun sort_steps/2, Seq ++ lists:flatten(Threads)),
    lists:foldl(fun run_step/2, {0, PQC, State}, Steps);
run_counterexample(PQC, [Steps], State) ->
    lists:foldl(fun run_step/2, {0, PQC, State}, Steps).

sort_steps({'set', Var1, _Call1}, {'set', Var2, _Call2}) ->
    Var1 < Var2.

run_step({'set', Var, Call}, {Step, PQC, State}) ->
    run_call(Var, Call, {Step, PQC, State}).

run_call(_Var, {'call', M, F, Args}=Call, {Step, PQC, State}) ->
    io:format('user', "(~p) ~p:~p(~p) -> ", [Step, M, F, Args]),
    Resp = erlang:apply(M, F, Args),
    io:format('user', "~p~n~n", [Resp]),
    'true' = PQC:postcondition(State, Call, Resp),
    {Step+1, PQC, PQC:next_state(State, Resp, Call)}.
