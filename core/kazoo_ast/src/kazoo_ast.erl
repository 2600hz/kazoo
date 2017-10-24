-module(kazoo_ast).

-export([walk_project/1
        ,walk_app/2
        ,walk_modules/2
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% define the callback function for the various options

-type accumulator() :: any().

-type fun_return() :: accumulator() |
                      {'skip', accumulator()} |
                      {'stop', accumulator()}.

-type expression_fun() :: fun((any(), accumulator()) -> fun_return()).
-type function_fun() :: fun((function(), accumulator()) -> fun_return()) |
                        fun((function(), arity(), accumulator()) -> fun_return()).
-type clause_fun() :: fun(([erl_parse:abstract_expr()], [erl_parse:abstract_expr()], accumulator()) -> accumulator()).
-type module_fun() :: fun((atom(), accumulator()) -> fun_return()).
-type app_fun() :: fun((atom(), accumulator()) -> fun_return()).
-type record_fun() :: fun((any(), accumulator()) -> fun_return()).

-type option() :: {'expression', expression_fun()} |
                  {'function', function_fun()} |
                  {'clause', clause_fun()} |
                  {'module', module_fun()} |
                  {'after_module', module_fun()} |
                  {'application', app_fun()} |
                  {'after_application', app_fun()} |
                  {'record', record_fun()} |
                  {'accumulator', accumulator()}.
-type options() :: [option()].

-type config() :: #{atom() => fun() | accumulator() | atom()}.

-spec walk_project(options()) -> accumulator() | 'undefined'.
walk_project(Options) ->
    ConfigMap = options_to_config(Options),

    result(process_project(ConfigMap)).

process_project(ConfigMap) ->
    try lists:foldl(fun process_app/2, ConfigMap, kz_ast_util:project_apps())
    catch 'throw':{'stop', Acc} -> Acc
    end.

-spec walk_app(atom(), options()) -> accumulator() | 'undefined'.
walk_app(App, Options) ->
    ConfigMap = options_to_config(Options),
    result(process_app(App, ConfigMap)).

-spec walk_modules(atoms(), options()) -> accumulator() | 'undefined'.
walk_modules(Modules, Options) when is_list(Modules) ->
    ConfigMap = options_to_config(Options),
    result(process_modules(Modules, ConfigMap)).

-spec result(config()) -> accumulator() | 'undefined'.
result(#{'config':='true'
        ,'accumulator' := Acc
        }) -> Acc;
result(#{'config':='true'}) -> 'undefined';
result(Acc) -> Acc.

-spec options_to_config(options()) -> config().
options_to_config(Options) ->
    lists:foldl(fun option_to_config/2, #{'config'=>'true'}, Options).

-spec option_to_config(option(), config()) -> config().
option_to_config({K, V}, Config) ->
    maps:put(K, V, Config).

-spec process_app(atom(), config()) -> config().
-spec process_app_modules(atom(), config()) -> config().
process_app(App, Config0) ->
    Config2 = process_app_modules(App, callback_application(App, Config0)),
    callback_after_application(App, Config2).

process_app_modules(_App, {'skip', Config}) -> Config;
process_app_modules(App, Config) ->
    try process_modules(kz_ast_util:app_modules(App), Config)
    catch 'throw':{'stop', Acc} -> Acc
    end.

process_modules(Modules, Config) ->
    try lists:foldl(fun process_module/2, Config, Modules)
    catch 'throw':{'stop', Acc} -> Acc
    end.

process_module(Module, Config0) ->
    case kz_ast_util:module_ast(Module) of
        'undefined' -> Config0;
        {M, AST} ->
            #module_ast{functions=Fs
                       ,records=Rs
                       } = kz_ast_util:add_module_ast(#module_ast{}, M, AST),
            Routines = [{fun process_functions/2, Fs}
                       ,{fun process_records/2, Rs}
                       ],
            Config1 = fold_over_module(Module, Config0, Routines),
            callback_after_module(M, Config1)
    end.

-spec fold_over_module(atom(), config(), [{fun((ast_functions(), config()) -> config()), ast_functions()} |
                                          {fun((ast_records(), config()) -> config()), ast_records()}
                                         ]) ->
                              config().
fold_over_module(Module, Config0, Routines) ->
    try lists:foldl(fun(_, {'skip', ConfigAcc}) -> ConfigAcc;
                       ({Fun, Arg}, ConfigAcc) ->
                            erlang:apply(Fun, [Arg, ConfigAcc])
                    end
                   ,callback_module(Module, Config0)
                   ,Routines
                   )
    catch
        _E:R ->
            ST = erlang:get_stacktrace(),
            io:format("error processing ~s: '~s': ~p~n", [Module, _E, R]),
            [io:format("~p~n", [S]) || S <- ST],
            throw({'error', Module, R})
    end.

process_records(Records, Config0) ->
    lists:foldl(fun process_record_fields/2, Config0, Records).

process_record_fields({_RecordName, Fields}, Config0) ->
    lists:foldl(fun process_record_field/2, Config0, Fields).

%% look inside a single record field
process_record_field(?RECORD_FIELD(_Key), Config) ->
    Config;
process_record_field(?RECORD_FIELD_BIND(_Key, Value), Config) ->
    process_expression(Value, Config);
process_record_field(?TYPED_RECORD_FIELD(RecordField, _Type), Config) ->
    process_record_field(RecordField, Config).

process_functions(Functions, Config0) ->
    lists:foldl(fun process_function/2
               ,Config0
               ,Functions
               ).

process_function({_Module, Function, Arity, Clauses}, Config) ->
    case callback_function(Function, Arity, Config) of
        {'skip', Config1} -> Config1;
        {'ok', Config1} -> process_clauses(Clauses, Config1)
    end.

process_clauses(Clauses, Config0) ->
    lists:foldl(fun process_clause/2
               ,Config0
               ,Clauses
               ).

process_clause(?CLAUSE(Args, Guards, Expressions), Config0) ->
    process_expressions(Args
                        ++ lists:flatten(Guards)
                        ++ Expressions
                       ,callback_clause(Args, Guards, Config0)
                       ).

process_expressions(Expressions, Config0) ->
    lists:foldl(fun process_expression/2
               ,Config0
               ,Expressions
               ).

process_expression(?MOD_FUN_ARGS(_M, _F, Args)=Expression, Config) ->
    process_expressions(Args, callback_expression(Expression, Config));
process_expression(?FUN_ARGS(_F, Args)=Expression, Config) ->
    process_expressions(Args, callback_expression(Expression, Config));

process_expression(?BINARY_OP(_Name, First, Second)=Expression, Config) ->
    process_expressions([First, Second], callback_expression(Expression, Config));
process_expression(?UNARY_OP(_Name, First)=Expression, Config) ->
    process_expression(First, callback_expression(Expression, Config));
process_expression(?CATCH(CatchExpression)=Expression, Config) ->
    process_expression(CatchExpression, callback_expression(Expression, Config));
process_expression(?TRY_BODY(Body, Clauses)=Expression, Config) ->
    process_clauses(Clauses
                   ,process_expression(Body, callback_expression(Expression, Config))
                   );
process_expression(?TRY_EXPR(Expr, Clauses, CatchClauses)=Expression, Config) ->
    process_clauses(Clauses ++ CatchClauses
                   ,process_expressions(Expr, callback_expression(Expression, Config))
                   );
process_expression(?TRY_BODY_AFTER(Body, Clauses, CatchClauses, AfterBody)=Expression, Config) ->
    process_clauses(Clauses ++ CatchClauses
                   ,process_expressions(Body ++ AfterBody, callback_expression(Expression, Config))
                   );
process_expression(?LC(Expr, Qualifiers)=Expression, Config) ->
    process_expressions([Expr | Qualifiers], callback_expression(Expression, Config));
process_expression(?LC_GENERATOR(Pattern, Expr)=Expression, Config) ->
    process_expressions([Pattern, Expr], callback_expression(Expression, Config));
process_expression(?BC(Expr, Qualifiers)=Expression, Config) ->
    process_expressions([Expr | Qualifiers], callback_expression(Expression, Config));
process_expression(?LC_BIN_GENERATOR(Pattern, Expr)=Expression, Config) ->
    process_expressions([Pattern, Expr], callback_expression(Expression, Config));
process_expression(?ANON(Clauses)=Expression, Config) ->
    process_clauses(Clauses, callback_expression(Expression, Config));
process_expression(?GEN_FUN_ARGS(?ANON(Clauses), Args)=Expression, Config) ->
    process_clauses(Clauses
                   ,process_expressions(Args, callback_expression(Expression, Config))
                   );
process_expression(?GEN_RECORD(_NameExpr, _RecName, Fields)=Expression, Config) ->
    process_expressions(Fields, callback_expression(Expression, Config));
process_expression(?RECORD(_Name, Fields)=Expression, Config) ->
    process_expressions(Fields, callback_expression(Expression, Config));
process_expression(?RECORD_FIELD_BIND(_Key, Value)=Expression, Config) ->
    process_expression(Value, callback_expression(Expression, Config));
process_expression(?GEN_RECORD_FIELD_ACCESS(_RecordName, _Name, Value)=Expression, Config) ->
    process_expression(Value, callback_expression(Expression, Config));
process_expression(?DYN_FUN_ARGS(_F, Args)=Expression, Config) ->
    process_expressions(Args, callback_expression(Expression, Config));
process_expression(?DYN_MOD_FUN_ARGS(_M, _F, Args)=Expression, Config) ->
    process_expressions(Args, callback_expression(Expression, Config));
process_expression(?MOD_DYN_FUN_ARGS(_M, _F, Args)=Expression, Config) ->
    process_expressions(Args, callback_expression(Expression, Config));
process_expression(?GEN_MOD_FUN_ARGS(MExpr, FExpr, Args)=Expression, Config) ->
    process_expressions([MExpr, FExpr | Args], callback_expression(Expression, Config));
process_expression(?TUPLE(Elements)=Expression, Config) ->
    process_expressions(Elements, callback_expression(Expression, Config));
process_expression(?LIST(Head, Tail)=Expression, Config) ->
    process_expressions([Head, Tail], callback_expression(Expression, Config));
process_expression(?RECEIVE(Clauses)=Expression, Config) ->
    process_clauses(Clauses, callback_expression(Expression, Config));
process_expression(?RECEIVE(Clauses, AfterExpr, AfterBody)=Expression, Config) ->
    process_expressions([AfterExpr | AfterBody]
                       ,process_clauses(Clauses, callback_expression(Expression, Config))
                       );
process_expression(?LAGER, Config) ->
    Config;
process_expression(?MATCH(LHS, RHS)=Expression, Config) ->
    process_expressions([LHS, RHS], callback_expression(Expression, Config));
process_expression(?BEGIN_END(Exprs)=Expression, Config) ->
    process_expressions(Exprs, callback_expression(Expression, Config));
process_expression(?CASE(CaseExpression, Clauses)=Expression, Config) ->
    process_clauses(Clauses
                   ,process_expression(CaseExpression, callback_expression(Expression, Config))
                   );
process_expression(?IF(Clauses)=Expression, Config) ->
    process_clauses(Clauses, callback_expression(Expression, Config));
process_expression(?MAP_CREATION(Exprs)=Expression, Config) ->
    process_expressions(Exprs, callback_expression(Expression, Config));
process_expression(?MAP_UPDATE(_Var, Exprs)=Expression, Config) ->
    process_expressions(Exprs, callback_expression(Expression, Config));
process_expression(?MAP_FIELD_ASSOC(K, V)=Expression, Config) ->
    process_expressions([K, V], callback_expression(Expression, Config));
process_expression(?MAP_FIELD_EXACT(K, V)=Expression, Config) ->
    process_expressions([K, V], callback_expression(Expression, Config));
process_expression(Expression, Config) ->
    callback_expression(Expression, Config).

-spec callback_function(function(), arity(), config()) -> {'ok' | 'skip', config()}.
callback_function(Function
                  ,_Arity
                 ,#{'function' := Fun
                   ,'accumulator' := Acc0
                   }=Config0
                 ) when is_function(Fun, 2) ->
    case Fun(Function, Acc0) of
        {'skip', Acc1} -> {'skip', Config0#{accumulator => Acc1}};
        Acc1 -> {'ok', Config0#{'accumulator' => Acc1}}
    end;
callback_function(Function
                  ,Arity
                 ,#{'function' := Fun
                   ,'accumulator' := Acc0
                   }=Config0
                 ) when is_function(Fun, 3) ->
    case Fun(Function, Arity, Acc0) of
        {'skip', Acc1} -> {'skip', Config0#{accumulator => Acc1}};
        Acc1 -> {'ok', Config0#{'accumulator' => Acc1}}
    end;
callback_function(_Function, _Artiy, Config) -> {'ok', Config}.

callback_expression(Expression
                   ,#{'expression':=Fun
                     ,'accumulator':=Acc0
                     }=Config0
                   ) ->
    case Fun(Expression, Acc0) of
        {'stop', Acc} ->
            throw({'stop', Acc});
        {'skip', Acc} ->
            {'skip', Config0#{'accumulator' => Acc}};
        Acc ->
            Config0#{'accumulator' => Acc}
    end;
callback_expression(_Expression, Config) -> Config.

callback_module(Module
               ,#{'module' := Fun
                 ,'accumulator' := Acc0
                 }=Config0
               ) ->
    case Fun(Module, Acc0) of
        {'skip', Acc} ->
            {'skip', Config0#{'accumulator' => Acc}};
        {'stop', Acc} ->
            throw({'stop', Acc});
        Acc ->
            Config0#{'accumulator' => Acc}
    end;
callback_module(_Module, Config) -> Config.

callback_after_module(Module
                     ,#{'after_module' := Fun
                       ,'accumulator' := Acc0
                       }=Config0
                     ) ->
    case Fun(Module, Acc0) of
        {'skip', Acc} ->
            {'skip', Config0#{'accumulator' => Acc}};
        {'stop', Acc} ->
            throw({'stop', Acc});
        Acc ->
            Config0#{'accumulator' => Acc}
    end;
callback_after_module(_Module, Config) -> Config.

callback_application(App, #{'application' := Fun
                           ,'accumulator' := Acc0
                           }=Config0
                    ) ->
    case Fun(App, Acc0) of
        {'skip', Acc} ->
            {'skip', Config0#{'accumulator' => Acc}};
        {'stop', Acc} ->
            throw({'stop', Acc});
        Acc ->
            Config0#{'accumulator' => Acc}
    end;
callback_application(_App, Config) -> Config.

callback_after_application(App, #{'after_application' := Fun
                                 ,'accumulator' := Acc0
                                 }=Config0
                          ) ->
    case Fun(App, Acc0) of
        {'skip', Acc} ->
            {'skip', Config0#{'accumulator' => Acc}};
        {'stop', Acc} ->
            throw({'stop', Acc});
        Acc ->
            Config0#{'accumulator' => Acc}
    end;
callback_after_application(_App, Config) -> Config.

callback_clause(Args, Guards, #{'accumulator' := Acc
                                ,'clause' := ClauseFun
                               }=Config0
               ) when is_function(ClauseFun, 3) ->
    Config0#{'accumulator' => ClauseFun(Args, Guards, Acc)};
callback_clause(_Args, _Guards, Config) -> Config.
