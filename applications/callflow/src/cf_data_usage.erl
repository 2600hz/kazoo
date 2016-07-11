-module(cf_data_usage).

%% module for parsing callflow actions for Data usage

-export([process/0, process/1]).

-include("callflow.hrl").
-include_lib("kazoo/include/kz_ast.hrl").

process() ->
    {'ok', Data} = application:get_all_key('callflow'),
    Modules = props:get_value('modules', Data),
    [{Module, Usages} ||
        Module <- Modules,
        (Usages = process(Module)) =/= 'undefined'
    ].

process(Module) ->
    io:format("  ~s: ", [Module]),
    Beam = module_to_beam(Module),
    io:format("beam: ~s ", [Beam]),

    case is_action_module(Beam) of
        'true' ->
            io:format("processing~n", []),
            process_action(Beam);
        'false' ->
            io:format("doesn't implement the gen_cf_action behaviour~n", []),
            'undefined'
    end.

process_action(Beam) ->
    {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(Beam, ['abstract_code']),
    io:format("processing ~p(~p)~n", [Module, Beam]),
    process_action_ast(Module, AST).

process_action_ast(Module, {'raw_abstract_v1', Attributes}) ->
    Fs = [{Module, F, A, Clauses} || {'function', _Line, F, A, Clauses} <- Attributes],
    Handle2Clauses = [Clauses || {_Module, 'handle', 2, Clauses} <- Fs],

    process_handle2(Fs, Handle2Clauses).

process_handle2(Fs, Cs) ->
    process_handle2(Fs, Cs, []).

process_handle2(_Fs, [], Usages) -> Usages;
process_handle2(Fs, [Clause|Clauses], Usages) ->
    process_handle2(Fs
                   ,Clauses
                   ,process_handle2_clause(Fs, Clause, Usages)
                   ).

process_handle2_clause(_Fs, [?CLAUSE([?VAR('_Data'), _Call], _Guards, _Body)], Usages) ->
    Usages;
process_handle2_clause(Fs
                      ,[?CLAUSE([?VAR('Data'), ?VAR('Call')]
                               ,_Guards
                               ,Body
                               )
                       ]
                      ,Usages) ->
    {_, U} =
        lists:foldl(fun(Expression, Acc) -> process_clause_expression(Fs, Expression, Acc) end
                   ,{'Data', Usages}
                   ,Body
                   ),
    U.

process_clause_expression(Fs, ?MATCH(Left, Right), Usages) ->
    process_match(Fs, Left, Right, Usages);
process_clause_expression(Fs, ?MOD_FUN_ARGS(Module, Function, Args), Usages) ->
    process_mfa(Fs, Module, Function, Args, Usages);
process_clause_expression(_Fs, ?VAR(_Name), Usages) ->
    %% Last expression is a variable to return to caller
    Usages;
process_clause_expression(_Fs, ?LAGER, Usages) -> Usages;
process_clause_expression(Fs, ?CASE(Expression, Clauses), Usages) ->
    lists:foldl(fun(Clause, UsagesAcc) ->
                        process_clause_expression(Fs, Clause, UsagesAcc)
                end
                ,process_clause_expression(Fs, Expression, Usages)
                ,Clauses
               );
process_clause_expression(_Fs, _Expression, Usages) ->
    io:format("skipping expression ~p~n", [_Expression]),
    Usages.

process_match(Fs, ?VAR(_Name), ?MOD_FUN_ARGS(Module, Function, Args), Usages) ->
    process_mfa(Fs, Module, Function, Args, Usages);
process_match(_Fs, _Left, _Right, Usages) ->
    io:format("not processing match ~p = ~p~n", [_Left, _Right]),
    Usages.

process_mfa(_Fs, M, F, [?BINARY_MATCH(Key), ?VAR(DataName)], {DataName, Usages}) ->
    {DataName, [{M, F, binary_match_to_binary(Key), 'undefined'} | Usages]};
process_mfa(_Fs, M, F, [?BINARY_MATCH(Key), ?VAR(DataName), ?BINARY_MATCH(Default)], {DataName, Usages}) ->
    {DataName, [{M, F, binary_match_to_binary(Key), binary_match_to_binary(Default)} | Usages]};
process_mfa(Fs, _M, _F, As, Usages) ->
    lists:foldl(fun(Arg, U) ->
                        process_clause_expression(Fs, Arg, U)
                end
                ,Usages
                ,As
               ).

binary_match_to_binary(Match) ->
    iolist_to_binary(
      [binary_part_to_binary(BP) || BP <- Match]
     ).

binary_part_to_binary(?BINARY_STRING(V)) -> V;
binary_part_to_binary(?BINARY_VAR(N)) -> [${, N, $}].

-spec is_action_module(file:filename_all()) -> boolean().
is_action_module(Beam) ->
    {'ok', {_Module, [{'attributes', Attributes}]}} =
        beam_lib:chunks(Beam, ['attributes']),
    Behaviours = props:get_value('behaviour', Attributes, []),
    lists:member('gen_cf_action', Behaviours).

module_to_beam(Module) ->
    filename:join([code:lib_dir('callflow', 'ebin')
                  ,module_to_filename(Module)
                  ]).

-spec module_to_filename(ne_binary() | atom()) -> ne_binary().
module_to_filename(<<_/binary>> = Mod) ->
    case filename:extension(Mod) of
        <<>> -> module_to_filename(<<Mod/binary, ".beam">>);
        <<".beam">> -> kz_util:to_list(Mod)
    end;
module_to_filename(Mod) -> module_to_filename(kz_util:to_binary(Mod)).
