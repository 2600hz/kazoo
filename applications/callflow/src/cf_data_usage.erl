-module(cf_data_usage).

%% module for parsing callflow actions for Data usage

-export([process/0, process/1]).

-include("callflow.hrl").
-include_lib("kazoo/include/kz_ast.hrl").

-record(usage, {usages = []
               ,data_var_name = 'Data'
               ,current_module
               ,functions = []
               }).

process() ->
    {'ok', Data} = application:get_all_key('callflow'),
    Modules = props:get_value('modules', Data),
    [{Module, Usages} ||
        Module <- Modules,
        (Usages = process(Module)) =/= 'undefined'
    ].

process(Module) ->
    Beam = module_to_beam(Module),

    case is_action_module(Beam) of
        'false' -> 'undefined';
        'true' ->
            io:format("~s processing~n", [Module]),
            process_action(Module)
    end.

process_action(Module) ->
    #usage{usages=Us} = process_mfa_call(#usage{current_module=Module}
                                        ,Module, 'handle', [?VAR(0, 'Data'), ?VAR(0, 'Call')]
                                        ),
    Us.

process_expression(Acc, ?TUPLE(Elements)) ->
    process_tuple(Acc, Elements);
process_expression(Acc, ?CLAUSE([Expr], _Guards, Body)) ->
    process_clause_body(process_expression(Acc, Expr), Body);
process_expression(Acc, ?MATCH(Left, Right)) ->
    process_match(Acc, Left, Right);
process_expression(#usage{current_module=Module}=Acc, ?FUN_ARGS(Function, Args)) ->
    process_mfa(Acc, Module, Function, Args);
process_expression(Acc, ?DYN_FUN_ARGS(_Function, Args)) ->
    process_expressions(Acc, Args);
process_expression(Acc, ?MOD_FUN_ARGS(Module, Function, Args)) ->
    process_mfa(Acc, Module, Function, Args);
process_expression(Acc, ?MFA(_M, _F, _Arity)) ->
    Acc;
process_expression(Acc, ?VAR(_Name)) ->
    %% Last expression is a variable to return to caller
    Acc;
process_expression(Acc, ?LAGER) -> Acc;
process_expression(Acc, ?CASE(Expression, Clauses)) ->
    lists:foldl(fun(Clause, UsagesAcc) ->
                        process_expression(UsagesAcc, Clause)
                end
               ,process_expression(Acc, Expression)
               ,Clauses
               );
process_expression(Acc, ?ATOM(_)) ->
    Acc;
process_expression(Acc, ?BINARY_MATCH(_)) ->
    Acc;
process_expression(Acc, ?EMPTY_LIST) ->
    Acc;
process_expression(Acc, ?LIST(Head, Tail)) ->
    process_list(Acc, Head, Tail);
process_expression(Acc, _Expression) ->
    io:format("skipping expression ~p~n", [_Expression]),
    Acc.

process_list(Acc, Head, Tail) ->
    process_expression(process_expression(Acc, Head)
                       ,Tail
                      ).

process_tuple(Acc, Elements) ->
    process_expressions(Acc, Elements).

process_expressions(Acc, Expressions) ->
    lists:foldl(fun(E, UsageAcc) ->
                        process_expression(UsageAcc, E)
                end
                ,Acc
                ,Expressions
               ).

process_clause_body(Acc, Body) ->
    lists:foldl(fun(Expression, UsagesAcc) ->
                        process_expression(UsagesAcc, Expression)
                end
               ,Acc
               ,Body
               ).

process_match(#usage{current_module=Module}=Acc, ?VAR(_Name), ?FUN_ARGS(Function, Args)) ->
    process_mfa(Acc, Module, Function, Args);
process_match(Acc, ?VAR(_Name), ?MOD_FUN_ARGS(Module, Function, Args)) ->
    process_mfa(Acc, Module, Function, Args);
process_match(Acc, ?VAR(_Name), ?CASE(_Expr, _Clauses)=Case) ->
    process_expression(Acc, Case);
process_match(Acc, _Left, _Right) ->
    io:format("not processing match ~p = ~p~n", [_Left, _Right]),
    Acc.

process_mfa(#usage{data_var_name=DataName
                    ,usages=Usages
                   }=Acc
           ,M, F, [?BINARY_MATCH(Key), ?VAR(DataName)]
           ) ->
    Acc#usage{usages=
                  [{M, F, binary_match_to_binary(Key), DataName, 'undefined'} | Usages]
             };
process_mfa(#usage{data_var_name=DataName
                  ,usages=Usages
                  }=Acc
           ,M, F, [?BINARY_MATCH(Key), ?VAR(DataName), ?BINARY_MATCH(Default)]
           ) ->
    Acc#usage{usages=
                  [{M, F, binary_match_to_binary(Key), DataName, binary_match_to_binary(Default)}
                   | Usages
                  ]
             };
process_mfa(#usage{data_var_name=DataName
                   ,usages=Usages
                   }=Acc
            ,M, F, [?BINARY_MATCH(Key), ?VAR(DataName), ?MOD_FUN_ARGS(Mod, Fun, Args)]
           ) ->
    Acc#usage{usages=[{M, F, binary_match_to_binary(Key), DataName, {Mod, Fun, length(Args)}}
                      | Usages
                     ]};
process_mfa(#usage{data_var_name=DataName}=Acc, M, F, As) ->
    case lists:any(fun(?VAR(N)) -> N =:= DataName;
                      (_Arg) -> 'false'
                   end
                  ,As
                  )
    of
        'true' ->
            process_mfa_call(Acc, M, F, As);
        'false' ->
            lists:foldl(fun(Arg, UsageAcc) ->
                                process_expression(UsageAcc, Arg)
                        end
                       ,Acc
                       ,As
                       )
    end.

process_mfa_call(#usage{data_var_name=DataName}=Acc, M, F, As) ->
    case mfa_clauses(Acc, M, F, length(As)) of
        [] -> maybe_add_module_ast(Acc, M, F, As);
        [Clauses] ->
            DataIndex = data_index(DataName, As),
            process_mfa_clauses(Acc, Clauses, DataIndex)
    end.

process_mfa_clauses(Acc, Clauses, DataIndex) ->
    lists:foldl(fun(Clause, UsagesAcc) ->
                        process_mfa_clause(UsagesAcc, Clause, DataIndex)
                end
               ,Acc
               ,Clauses
               ).

process_mfa_clause(#usage{data_var_name=DataName}=Acc
                  ,?CLAUSE(Args, _Guards, Body)
                  ,DataIndex
                  ) ->
    case lists:nth(DataIndex, Args) of
        ?VAR(DataName) -> process_clause_body(Acc, Body);
        ?VAR(NewName) -> process_clause_body(Acc#usage{data_var_name=NewName}, Body);
        _Unexpected ->
            io:format("unexpected arg at ~p in ~p~n", [DataIndex, Args]),
            Acc
    end.

mfa_clauses(#usage{functions=Fs}, Module, Function, Arity) ->
    [Cs || {M, F, A, Cs} <- Fs,
           Module =:= M,
           Function =:= F,
           Arity =:= A
    ].

maybe_add_module_ast(#usage{functions=Fs}=Acc, M, F, As) ->
    case code:which(M) of
        'non_existing' ->
            io:format("failed to find module ~p~n", [M]),
            Acc;
        Beam ->
            {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(Beam, ['abstract_code']),
            process_mfa_call(Acc#usage{functions=add_module_ast(Fs, Module, AST)}
                            ,M, F, As
                            )
    end.

ast_functions(Module, {'raw_abstract_v1', Attributes}) ->
    [{Module, F, Arity, Clauses} || {'function', _Line, F, Arity, Clauses} <- Attributes].

add_module_ast(Fs, Module, AST) ->
    ast_functions(Module, AST) ++ Fs.

data_index(DataName, Args) ->
    data_index(DataName, Args, 1).
data_index(DataName, [?VAR(DataName)|_As], Index) -> Index;
data_index(DataName, [_|As], Index) ->
    data_index(DataName, As, Index+1).

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
