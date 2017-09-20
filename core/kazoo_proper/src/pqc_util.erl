-module(pqc_util).

-export([transition_if/2
        ,simple_counterexample/0
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

-spec simple_counterexample() -> [{module(), function(), list()}].
simple_counterexample() ->
    simple_counterexample(proper:counterexample()).

simple_counterexample('undefined') ->
    {error, no_counterexample};
simple_counterexample([Seq]) ->
    [{M, F, ['{API}'|cleanup_args(Args)]}
     || {set, _Var, {call, M, F, [_|Args]}} <- Seq
    ].

cleanup_args(Args) ->
    [cleanup_arg(Arg) || Arg <- Args].
cleanup_arg({call, M, F, Args}) ->
    {M,F, length(Args)};
cleanup_arg(Arg) -> Arg.


-spec run_counterexample(module()) ->
                                {ne_binary(), 'postcondition_failed'} |
                                {ne_binary(), atom(), any(), [erlang:stack_item()]} |
                                {'ok', ne_binary()}.
run_counterexample(PQC) ->
    PQC:cleanup(),
    io:format("cleaned up, running counterexample~n", []),
    InitialState = PQC:initial_state(),
    try run_counterexample(PQC, proper:counterexample(), InitialState) of
        {'error', _}=E -> E;
        {_Step, _PQC, State, Vars} ->
            ?INFO("final state: ~p~nfinal vars: ~p~n", [State, Vars]),
            #{'request_id' := RequestId} = pqc_kazoo_model:api(State),
            {'ok', RequestId}
    catch
        'throw':{'failed_postcondition'
                ,PreCallState
                ,{M, F, A}
                ,Resp
                } ->
            #{'request_id' := RequestId} = pqc_kazoo_model:api(PreCallState),
            ?INFO("postcondition returned false"),
            ?INFO("state prior to call:"),
            _ = [?INFO("~p: ~p", [Cat, Item]) || {Cat, Item} <- pqc_kazoo_model:pp(PreCallState)],
            ?INFO("call: ~s:~s(~p)", [M, F, A]),
            ?INFO("SUT resp: ~p", [Resp]),
            {RequestId, 'postcondition_failed'};
        E:R ->
            #{'request_id' := RequestId} = pqc_kazoo_model:api(InitialState),
            {RequestId, E, R, erlang:get_stacktrace()}
    after
        PQC:cleanup()
    end.

run_counterexample(_PQC, 'undefined', _State) -> {'error', 'no_counterexample'};
run_counterexample(PQC, [{Seq, Threads}], State) ->
    Steps = lists:usort(fun sort_steps/2, Seq ++ lists:flatten(Threads)),
    lists:foldl(fun run_step/2, {0, PQC, State, #{}}, Steps);
run_counterexample(PQC, [Steps], State) ->
    lists:foldl(fun run_step/2, {0, PQC, State, #{}}, Steps).

sort_steps({'set', Var1, _Call1}, {'set', Var2, _Call2}) ->
    Var1 < Var2.

run_step({'set', Var, Call}, {Step, PQC, State, Vars}) ->
    run_call(Var, Call, {Step, PQC, State, Vars}).

run_call(Var, {'call', M, F, Args}, {Step, PQC, State, Vars}) ->
    Args1 = resolve_args(Args, pqc_kazoo_model:api(State), Vars),
    ?INFO("(~p) ~p:~p(~p) -> ", [Step, M, F, Args1]),
    Resp = erlang:apply(M, F, Args1),
    ?INFO("applied ~p:~p, assoc var ~p with resp ~p~n~n", [M, F, Var, Resp]),
    case PQC:postcondition(State, {'call', M, F, Args1}, Resp) of
        'true' ->
            ?INFO("~n", []),
            {Step+1, PQC, PQC:next_state(State, Resp, {'call', M, F, Args1}), Vars#{Var => Resp}};
        'false' ->
            ?INFO("postcondition failed:~n  model:~n", []),
            _ = [?INFO("    ~p~n", [Model]) || Model <- pqc_kazoo_model:pp(State)],
            ?INFO("  call ~p:~p(~p)~n", [M, F, Args1]),
            throw({'failed_postcondition', State, {M, F, Args1}, Resp})
    end.

resolve_args(Args, API, Vars) ->
    %% ?INFO("args: ~p~nvars: ~p~n~n", [Args, Vars]),
    [resolve_arg(Arg, API, Vars) || Arg <- Args].

resolve_arg({'call', M, F, Args}, API, Vars) ->
    Args1 = resolve_args(Args, API, Vars),
    %% ?INFO("  resolved ~p:~p(~p)~n", [M, F, Args1]),
    resolve_arg(erlang:apply(M, F, Args1), API, Vars);
resolve_arg(#{'auth_token' := _}, API, _Vars) -> API; % replace old api
resolve_arg({'var', _}=Var, _API, Vars) ->
    %% ?INFO("  resolved ~p to ~p~n", [Var, maps:get(Var, Vars)]),
    maps:get(Var, Vars);
resolve_arg(Arg, _API, _Vars) -> Arg.
