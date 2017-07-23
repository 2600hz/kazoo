-module(pqc_util).

-export([transition_if/2
        ,run_counterexample/2
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

-spec run_counterexample(module(), list()) -> {module(), any()}.
run_counterexample(PQC, Steps) ->
    run_counterexample(PQC, Steps, PQC:initial_state()).
run_counterexample(PQC, Steps, State) ->
    lists:foldl(fun run_step/2, {PQC, State}, Steps).

run_step({'set', Var, Call}, {PQC, State}) ->
    run_call(Var, Call, {PQC, State}).

run_call(_Var, {'call', M, F, Args}=Call, {PQC, State}) ->
    io:format('user', "~p:~p(~p) -> ", [M, F, Args]),
    Resp = erlang:apply(M, F, Args),
    io:format('user', "~p~n", [Resp]),
    'true' = PQC:postcondition(State, Call, Resp),
    {PQC, PQC:next_state(State, Resp, Call)}.
