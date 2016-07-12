-module(cf_data_usage).

%% module for parsing callflow actions for Data usage

-export([process/0, process/1]).

-include("callflow.hrl").
-include_lib("kazoo/include/kz_ast.hrl").

%% -define(DEBUG(_Fmt, _Args), 'ok').
-define(DEBUG(Fmt, Args), io:format(Fmt, Args)).

-record(usage, {usages = []
               ,data_var_name = 'Data'
               ,data_var_aliases = []
               ,current_module
               ,functions = []
               }).

process() ->
    {'ok', Data} = application:get_all_key('callflow'),
    Modules = props:get_value('modules', Data),

    io:format("processing "),
    Usages = [{Module, Usages} ||
                 Module <- Modules,
                 (Usages = process(Module)) =/= 'undefined'
             ],
    io:format(" done~n~n"),
    Usages.

process(Module) ->
    Beam = module_to_beam(Module),

    case is_action_module(Beam) of
        'false' -> 'undefined';
        'true' ->
            io:format("."),
            process_action(Module)
    end.

process_action(Module) ->
    #usage{usages=Us} = process_mfa_call(#usage{current_module=Module}
                                        ,Module, 'handle', [?VAR(0, 'Data'), ?VAR(0, 'Call')]
                                        ),
    Us.

process_expression(Acc, ?TUPLE(Elements)) ->
    process_tuple(Acc, Elements);
process_expression(Acc, ?CLAUSE(Exprs, _Guards, Body)) ->
    process_clause_body(process_expressions(Acc, Exprs), Body);
process_expression(Acc, ?MATCH(Left, Right)) ->
    process_match(Acc, Left, Right);
process_expression(#usage{current_module=Module}=Acc, ?FUN_ARGS(Function, Args)) ->
    process_mfa(Acc, Module, Function, Args);
process_expression(Acc, ?DYN_FUN_ARGS(_Function, Args)) ->
    process_expressions(Acc, Args);
process_expression(Acc, ?MOD_FUN_ARGS(Module, Function, Args)) ->
    process_mfa(Acc, Module, Function, Args);
process_expression(Acc, ?ANON(Clauses)) ->
    process_expressions(Acc, Clauses);
process_expression(Acc, ?MFA(_M, _F, _Arity)) ->
    Acc;
process_expression(#usage{current_module=M}=Acc, ?FA(F, Arity)) ->
    process_mf_arity(Acc, M, F, Arity);
process_expression(Acc, ?VAR(_Name)) ->
    %% Last expression is a variable to return to caller
    Acc;
process_expression(Acc, ?CATCH(Expression)) ->
    process_expression(Acc, Expression);
process_expression(Acc, ?LAGER) -> Acc;
process_expression(Acc, ?CASE(Expression, Clauses)) ->
    process_expressions(process_expression(Acc, Expression)
                        ,Clauses
                       );
process_expression(Acc, ?ATOM(_)) ->
    Acc;
process_expression(Acc, ?INTEGER(_)) ->
    Acc;
process_expression(Acc, ?BINARY_MATCH(_)) ->
    Acc;
process_expression(Acc, ?EMPTY_LIST) ->
    Acc;
process_expression(Acc, ?LIST(Head, Tail)) ->
    process_list(Acc, Head, Tail);
process_expression(Acc, ?RECORD(_Name, Fields)) ->
    process_record_fields(Acc, Fields);
process_expression(Acc, ?RECORD_FIELD_ACCESS(_RecordName, _Name, _Value)) ->
    Acc;
process_expression(Acc, ?BINARY_OP(_, First, Second)) ->
    process_expressions(Acc, [First, Second]);
process_expression(Acc, ?UNARY_OP(_, Operand)) ->
    process_expression(Acc, Operand);
process_expression(Acc, ?STRING(_Value)) ->
    Acc;
process_expression(Acc, ?TRY_BODY(Body, CatchClauses)) ->
    process_expressions(process_expressions(Acc, Body)
                       ,CatchClauses
                       );
process_expression(Acc, ?TRY_EXPR(Exprs, Clauses, CatchClauses)) ->
    process_expressions(
      process_expressions(
        process_expressions(Acc, Exprs)
                         ,Clauses
       )
                       ,CatchClauses
     );
process_expression(#usage{current_module=_M}=Acc, _Expression) ->
    io:format("~nskipping expression in ~p: ~p~n", [_M, _Expression]),
    Acc.

process_list(Acc, Head, Tail) ->
    process_expression(process_expression(Acc, Head)
                       ,Tail
                      ).

process_record_fields(Acc, Fields) ->
    Values = [record_field_value(Field) || Field <- Fields],
    process_expressions(Acc, Values).

record_field_value(?RECORD_FIELD_ACCESS(_RecordName, _Name, Value)) -> Value;
record_field_value(?RECORD_FIELD_BIND(_Key, Value)) -> Value.

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
process_match(Acc, ?VAR(Name), ?MOD_FUN_ARGS(Module, Function, Args)) ->
    process_match_mfa(Acc, Name, Module, Function, Args);
process_match(Acc, _Left, Right) ->
    process_expression(Acc, Right).

process_match_mfa(#usage{data_var_name=DataName
                         ,data_var_aliases=Aliases
                        }=Acc
                 ,VarName
                 ,_M, _F, [?BINARY_MATCH(_Key), _Value, ?VAR(DataName)]
                 ) ->
    ?DEBUG("adding alias ~p~n", [VarName]),
    Acc#usage{data_var_aliases=[VarName|Aliases]};
process_match_mfa(Acc, _VarName, M, F, As) ->
    process_mfa(Acc, M, F, As).

process_mfa(#usage{data_var_name=DataName
                  ,usages=Usages
                  }=Acc
           ,M, F, [?BINARY_MATCH(Key), ?VAR(DataName)]
           ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, binary_match_to_binary(Key), DataName, 'undefined'})};
process_mfa(#usage{data_var_name=DataName
                  ,usages=Usages
                  }=Acc
           ,M, F, [?BINARY_MATCH(Key), ?VAR(DataName), ?BINARY_MATCH(Default)]
           ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, binary_match_to_binary(Key), DataName, binary_match_to_binary(Default)})};

process_mfa(#usage{data_var_name=DataName
                  ,usages=Usages
                  }=Acc
           ,M, F, [?BINARY_MATCH(Key), ?VAR(DataName), ?ATOM(Default)]
           ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, binary_match_to_binary(Key), DataName, Default})};

process_mfa(#usage{data_var_name=DataName
                   ,usages=Usages
                   }=Acc
            ,M, F, [?BINARY_MATCH(Key), ?VAR(DataName), ?MOD_FUN_ARGS(Mod, Fun, Args)]
           ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, binary_match_to_binary(Key), DataName, {Mod, Fun, length(Args)}})};
process_mfa(#usage{data_var_name=DataName
                   ,usages=Usages
                   }=Acc
            ,M, F, [?BINARY_MATCH(Key), ?VAR(DataName), ?VAR(Default)]
           ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, binary_match_to_binary(Key), DataName, Default})};

process_mfa(#usage{data_var_name=DataName
                   ,usages=Usages
                   }=Acc
            ,M, F, [?BINARY_MATCH(Key), ?VAR(DataName), ?INTEGER(Default)]
           ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, binary_match_to_binary(Key), DataName, Default})};

process_mfa(#usage{data_var_name=DataName
                   ,usages=Usages
                   }=Acc
            ,M, F, [?LIST(Head, Tail), ?VAR(DataName)]
            ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, list_of_keys_to_binary(Head, Tail)
                                             ,DataName, 'undefined'
                                             }
                                    )
             };

process_mfa(#usage{data_var_name=DataName
                   ,usages=Usages
                   }=Acc
            ,M, F, [?LIST(Head, Tail), ?VAR(DataName), ?VAR(Default)]
            ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, list_of_keys_to_binary(Head, Tail)
                                             ,DataName, Default
                                             }
                                    )
             };
process_mfa(#usage{data_var_name=DataName
                   ,usages=Usages
                   }=Acc
            ,M, F, [?LIST(Head, Tail), ?VAR(DataName), ?ATOM(Default)]
            ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, list_of_keys_to_binary(Head, Tail)
                                             ,DataName, Default
                                             }
                                    )
             };

process_mfa(#usage{data_var_name=DataName}=Acc
           ,_M, _F, [?VAR(_Name), ?VAR(DataName)]=_Args
           ) ->
    ?DEBUG("  skipping usage ~p:~p(~p)~n", [_M, _F, _Args]),
    Acc;
process_mfa(#usage{data_var_name=DataName}=Acc
           ,_M, _F, [?VAR(_Name), ?VAR(DataName), _Default]=_Args
           ) ->
    ?DEBUG("  skipping usage ~p:~p(~p)~n", [_M, _F, _Args]),
    Acc;

process_mfa(#usage{data_var_name=DataName
                   ,data_var_aliases=Aliases
                  }=Acc
           ,M, F, As) ->
    case lists:foldl(fun(?VAR(N), _) when N =:= DataName ->
                            DataName;
                       (?VAR(N), Action) ->
                            case lists:member(N, Aliases) of
                                'true' -> N;
                                'false' -> Action
                            end;
                       (_Arg, Action) -> Action
                    end
                   ,'undefined'
                   ,As
                   )
    of
        DataName ->
            ?DEBUG("  processing call ~p:~p(~p)~n", [M, F, As]),
            process_mfa_call(Acc, M, F, As);
        'undefined' ->
            lists:foldl(fun(Arg, UsageAcc) ->
                                process_expression(UsageAcc, Arg)
                        end
                       ,Acc
                       ,As
                       );
        Alias ->
            ?DEBUG("  processing call with alias ~p: ~p:~p(~p)~n", [Alias, M, F, As]),
            process_mfa_call(Acc#usage{data_var_name=Alias}, M, F, As)
    end.

list_of_keys_to_binary(Head, Tail) ->
    list_of_keys_to_binary(Head, Tail, []).

list_of_keys_to_binary(?BINARY_MATCH(Key), ?EMPTY_LIST, Path) ->
    lists:reverse([binary_match_to_binary(Key) | Path]);
list_of_keys_to_binary(?VAR(Name), ?EMPTY_LIST, Path) ->
    lists:reverse([Name | Path]);
list_of_keys_to_binary(?LIST(SubHead, SubTail), ?EMPTY_LIST, Path) ->
    Key = list_of_keys_to_binary(SubHead, SubTail),
    lists:reverse([Key | Path]);

list_of_keys_to_binary(?BINARY_MATCH(Key), ?LIST(Head, Tail), Path) ->
    list_of_keys_to_binary(Head, Tail, [binary_match_to_binary(Key) | Path]);
list_of_keys_to_binary(?VAR(Name), ?LIST(Head, Tail), Path) ->
    list_of_keys_to_binary(Head, Tail, [Name, Path]);
list_of_keys_to_binary(?LIST(SubHead, SubTail), ?LIST(Head, Tail), Path) ->
    Key = list_of_keys_to_binary(SubHead, SubTail),
    list_of_keys_to_binary(Head, Tail, [Key | Path]).

maybe_add_usage(Usages, Call) ->
    case lists:member(Call, Usages) of
        'true' -> Usages;
        'false' ->
            ?DEBUG("adding usage: ~p~n", [Call]),
            [Call | Usages]
    end.

process_mf_arity(#usage{usages=Usages}=Acc, M, F, Arity) ->
    case mfa_clauses(Acc, M, F, Arity) of
        [] -> Acc;
        [Clauses] ->
            #usage{usages=ModuleUsages
                  ,functions=NewFs
                  ,current_module=_MCM
                  } =
                process_mfa_clauses(Acc#usage{current_module=M
                                             ,usages=[]
                                             ,data_var_aliases=[]
                                             }
                                   ,Clauses
                                   ,0
                                   ),
            Acc#usage{usages=lists:usort(ModuleUsages ++ Usages)
                     ,functions=NewFs
                     }
    end.

process_mfa_call(Acc, M, F, As) ->
    ?DEBUG("~n  calling ~p:~p(~p)~n", [M, F, As]),
    process_mfa_call(Acc, M, F, As, 'true').

process_mfa_call(#usage{data_var_name=DataName
                       ,usages=Usages
                       ,functions=Fs
                       ,current_module=_CM
                       }=Acc
                ,M, F, As, ShouldAddAST) ->
        case mfa_clauses(Acc, M, F, length(As)) of
            [] when ShouldAddAST ->
                case module_ast(M) of
                    'undefined' -> Acc;
                    {M, AST} ->
                        process_mfa_call(Acc#usage{functions=add_module_ast(Fs, M, AST)}
                                        ,M, F, As, 'false'
                                        )
                end;
            [] -> Acc;
            [Clauses] ->
                #usage{usages=ModuleUsages
                      ,functions=NewFs
                      ,current_module=_MCM
                      } =
                    process_mfa_clauses(Acc#usage{current_module=M
                                                 ,usages=[]
                                                 ,data_var_aliases=[]
                                                 }
                                       ,Clauses
                                       ,data_index(DataName, As)
                                       ),
                Acc#usage{usages=lists:usort(ModuleUsages ++ Usages)
                         ,functions=NewFs
                         }
        end.

process_mfa_clauses(Acc, Clauses, DataIndex) ->
    lists:foldl(fun(Clause, UsagesAcc) ->
                        process_mfa_clause(UsagesAcc, Clause, DataIndex)
                end
               ,Acc
               ,Clauses
               ).

process_mfa_clause(#usage{data_var_name=DataName}=Acc
                  ,?CLAUSE(Args, _Guards, _Body)=Clause
                  ,0
                  ) ->
    ?DEBUG("  guessing index for ~p from ~p~n", [DataName, Args]),
    DataIndex = data_index(DataName, Args),
    ?DEBUG("  guessed data index of ~p as ~p~n", [DataName, DataIndex]),
    process_mfa_clause(Acc, Clause, DataIndex);
process_mfa_clause(Acc, _Clause, 'undefined') ->
    Acc;
process_mfa_clause(#usage{data_var_name=DataName
                         ,usages=Us
                         }=Acc
                  ,?CLAUSE(Args, _Guards, Body)
                  ,DataIndex
                  ) ->
    ?DEBUG("  processing mfa clause for ~p(~p)~n", [DataName, DataIndex]),
    case lists:nth(DataIndex, Args) of
        ?VAR('_') -> Acc;
        ?VAR(DataName) -> process_clause_body(Acc, Body);
        ?VAR(NewName) ->
            ?DEBUG("  data name changed from ~p to ~p~n", [DataName, NewName]),
            #usage{usages=ClauseUsages
                  ,functions=ClauseFs
                  } = process_clause_body(Acc#usage{data_var_name=NewName}, Body),
            Acc#usage{usages=lists:usort(ClauseUsages ++ Us)
                     ,functions=ClauseFs
                     };
        ?ATOM('undefined') -> Acc;
        _Unexpected ->
            io:format("unexpected arg(~p) at ~p in ~p, expected ~p~n"
                     ,[_Unexpected, DataIndex, Args, DataName]
                     ),
            Acc
    end.

mfa_clauses(#usage{functions=Fs}, Module, Function, Arity) ->
    [Cs || {M, F, A, Cs} <- Fs,
           Module =:= M,
           Function =:= F,
           Arity =:= A
    ].

module_ast(M) ->
    case code:which(M) of
        'non_existing' -> 'undefined';
        Beam ->
            {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(Beam, ['abstract_code']),
            {Module, AST}
    end.

ast_functions(Module, {'raw_abstract_v1', Attributes}) ->
    [{Module, F, Arity, Clauses} || {'function', _Line, F, Arity, Clauses} <- Attributes].

add_module_ast(Fs, Module, AST) ->
    ast_functions(Module, AST) ++ Fs.

data_index(DataName, Args) ->
    data_index(DataName, Args, 1).

data_index(_DataName, [], _Index) -> 'undefined';
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

-spec module_to_filename(ne_binary() | string() | atom()) -> string().
module_to_filename(<<_/binary>> = Mod) ->
    case filename:extension(Mod) of
        <<>> -> module_to_filename(<<Mod/binary, ".beam">>);
        <<".beam">> -> kz_util:to_list(Mod)
    end;
module_to_filename(Mod) -> module_to_filename(kz_util:to_binary(Mod)).
