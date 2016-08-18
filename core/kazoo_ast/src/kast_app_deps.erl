-module(kast_app_deps).

-export([process_project/0
        ,process_app/1

        ,fix_project_deps/0
        ,fix_app_deps/1
        ]).

-export([remote_calls/1
        ,remote_apps/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").

fix_project_deps() ->
    Deps = process_project(),
    io:format("processing app files "),
    [fix_app_deps(App, Missing, Unneeded)
     || {App, Missing, Unneeded} <- Deps
    ],
    io:format(" done.~n").

fix_app_deps(App) ->
    [fix_app_deps(App, Missing, Unneeded)
     || {_App, Missing, Unneeded} <- process_app(App)
    ],
    'ok'.

fix_app_deps(App, Missing, Unneeded) ->
    {'application'
    ,App
    ,Properties
    } = read_app_src(App),
    ConfiguredApps = lists:usort(props:get_value('applications', Properties) -- ['kernel']),

    case ordsets:subtract(ordsets:union(ConfiguredApps, Missing)
                         ,Unneeded
                         )
    of
        ConfiguredApps ->
            io:format(".");
        UpdatedApps ->
            io:format("x"),
            write_app_src(App
                         ,{'application'
                          ,App
                          ,props:set_value('applications', UpdatedApps, Properties)
                          }
                         )
    end.

read_app_src(App) ->
    AppBin = kz_util:to_binary(App),
    File = filename:join([code:lib_dir(App, 'src')
                         ,<<AppBin/binary, ".app.src">>
                         ]),
    {'ok', [Config]} = file:consult(kz_util:to_list(File)),
    Config.

write_app_src(App, Config) ->
    AppBin = kz_util:to_binary(App),
    File = filename:join([code:lib_dir(App, 'src')
                         ,<<AppBin/binary, ".app.src">>
                         ]),
    file:write_file(File, io_lib:format("~p.~n", [Config])).

process_project() ->
    io:format("processing application dependencies: "),
    Discrepencies = lists:foldl(fun process_app/2
                               ,[]
                               ,kz_ast_util:project_apps()
                               ),
    io:format(" done~n"),
    lists:keysort(1, Discrepencies).

process_app(App) ->
    process_app(App, []).

process_app('kazoo_ast', Acc) -> Acc;
process_app(App, Acc) ->
    {'ok', ExistingApps} = application:get_key(App, 'applications'),
    KnownApps = ordsets:from_list(ExistingApps -- ['kernel']),

    RemoteModules = remote_calls(App),
    RemoteApps = ordsets:from_list(modules_as_apps(App, RemoteModules)),

    Missing = ordsets:subtract(RemoteApps, KnownApps),
    Unneeded = ordsets:subtract(KnownApps, RemoteApps),

    case {Missing, Unneeded} of
        {[], []} -> Acc;
        _ -> [{App, Missing, Unneeded} | Acc]
    end.

remote_apps(App) ->
    modules_with_apps(App, remote_calls(App)).

remote_calls(App) ->
    lists:usort(
      lists:foldl(fun remote_calls_from_module/2
                 ,[]
                 ,kz_ast_util:app_modules(App)
                 )
     ).

remote_calls_from_module(Module, Acc) ->
    io:format("."),
    {M, AST} = kz_ast_util:module_ast(Module),
    Fs = kz_ast_util:add_module_ast([], M, AST),
    remote_calls_from_functions(Fs, Acc).

remote_calls_from_functions(Fs, Acc) ->
    lists:foldl(fun remote_calls_from_function/2
               ,Acc
               ,Fs
               ).

remote_calls_from_function({_Module, _Function, _Arity, Clauses}, Acc) ->
    remote_calls_from_clauses(Clauses, Acc).

remote_calls_from_clauses(Clauses, Acc) ->
    lists:foldl(fun remote_calls_from_clause/2, Acc, Clauses).

remote_calls_from_clause(?CLAUSE(_Args, _Guards, Expressions), Acc) ->
    remote_calls_from_expressions(Expressions, Acc).

remote_calls_from_expressions(Expressions, Acc) ->
    lists:foldl(fun remote_calls_from_expression/2, Acc, Expressions).

remote_calls_from_expression(?MOD_FUN_ARGS(M, _F, Args), Acc) ->
    remote_calls_from_expressions(Args, add_remote_module(M, Acc));
remote_calls_from_expression(?DYN_MOD_FUN(M, _F), Acc) ->
    add_remote_module(M, Acc);
remote_calls_from_expression(?FUN_ARGS(_F, Args), Acc) ->
    remote_calls_from_expressions(Args, Acc);
remote_calls_from_expression(?GEN_MFA(M, _F, _Arity), Acc) ->
    add_remote_module(M, Acc);
remote_calls_from_expression(?FA(_F, _Arity), Acc) -> Acc;
remote_calls_from_expression(?BINARY_OP(_Name, First, Second), Acc) ->
    remote_calls_from_expressions([First, Second], Acc);
remote_calls_from_expression(?UNARY_OP(_Name, First), Acc) ->
    remote_calls_from_expression(First, Acc);
remote_calls_from_expression(?CATCH(Expression), Acc) ->
    remote_calls_from_expression(Expression, Acc);
remote_calls_from_expression(?TRY_BODY(Body, Clauses), Acc) ->
    remote_calls_from_clauses(Clauses
                             ,remote_calls_from_expression(Body, Acc)
                             );
remote_calls_from_expression(?TRY_EXPR(Expr, Clauses, CatchClauses), Acc) ->
    remote_calls_from_clauses(Clauses ++ CatchClauses
                             ,remote_calls_from_expressions(Expr, Acc)
                             );
remote_calls_from_expression(?TRY_BODY_AFTER(Body, Clauses, CatchClauses, AfterBody), Acc) ->
    remote_calls_from_clauses(Clauses ++ CatchClauses
                             ,remote_calls_from_expressions(Body ++ AfterBody, Acc)
                             );
remote_calls_from_expression(?LC(Expr, Qualifiers), Acc) ->
    remote_calls_from_expressions([Expr | Qualifiers], Acc);
remote_calls_from_expression(?LC_GENERATOR(Pattern, Expr), Acc) ->
    remote_calls_from_expressions([Pattern, Expr], Acc);
remote_calls_from_expression(?BC(Expr, Qualifiers), Acc) ->
    remote_calls_from_expressions([Expr | Qualifiers], Acc);
remote_calls_from_expression(?LC_BIN_GENERATOR(Pattern, Expr), Acc) ->
    remote_calls_from_expressions([Pattern, Expr], Acc);
remote_calls_from_expression(?ANON(Clauses), Acc) ->
    remote_calls_from_clauses(Clauses, Acc);
remote_calls_from_expression(?GEN_FUN_ARGS(?ANON(Clauses), Args), Acc) ->
    remote_calls_from_clauses(Clauses
                             ,remote_calls_from_expressions(Args, Acc)
                             );
remote_calls_from_expression(?VAR(_), Acc) ->
    Acc;
remote_calls_from_expression(?BINARY_MATCH(_), Acc) ->
    Acc;
remote_calls_from_expression(?STRING(_), Acc) ->
    Acc;
remote_calls_from_expression(?GEN_RECORD(_NameExpr, _RecName, Fields), Acc) ->
    remote_calls_from_expressions(Fields, Acc);
remote_calls_from_expression(?RECORD(_Name, Fields), Acc) ->
    remote_calls_from_expressions(Fields, Acc);
remote_calls_from_expression(?RECORD_FIELD_BIND(_Key, Value), Acc) ->
    remote_calls_from_expression(Value, Acc);
remote_calls_from_expression(?GEN_RECORD_FIELD_ACCESS(_RecordName, _Name, Value), Acc) ->
    remote_calls_from_expression(Value, Acc);
remote_calls_from_expression(?RECORD_INDEX(_Name, _Field), Acc) ->
    Acc;
remote_calls_from_expression(?RECORD_FIELD_REST, Acc) ->
    Acc;
remote_calls_from_expression(?DYN_FUN_ARGS(_F, Args), Acc) ->
    remote_calls_from_expressions(Args, Acc);
remote_calls_from_expression(?DYN_MOD_FUN_ARGS(_M, _F, Args), Acc) ->
    remote_calls_from_expressions(Args, Acc);
remote_calls_from_expression(?MOD_DYN_FUN_ARGS(M, _F, Args), Acc) ->
    remote_calls_from_expressions(Args, add_remote_module(M, Acc));
remote_calls_from_expression(?GEN_MOD_FUN_ARGS(MExpr, FExpr, Args), Acc) ->
    remote_calls_from_expressions([MExpr, FExpr | Args], Acc);
remote_calls_from_expression(?ATOM(_), Acc) -> Acc;
remote_calls_from_expression(?INTEGER(_), Acc) -> Acc;
remote_calls_from_expression(?FLOAT(_), Acc) -> Acc;
remote_calls_from_expression(?CHAR(_), Acc) -> Acc;
remote_calls_from_expression(?TUPLE(_Elements), Acc) -> Acc;
remote_calls_from_expression(?EMPTY_LIST, Acc) -> Acc;
remote_calls_from_expression(?LIST(Head, Tail), Acc) ->
    remote_calls_from_expressions([Head, Tail], Acc);
remote_calls_from_expression(?RECEIVE(Clauses), Acc) ->
    remote_calls_from_clauses(Clauses, Acc);
remote_calls_from_expression(?RECEIVE(Clauses, AfterExpr, AfterBody), Acc) ->
    remote_calls_from_expressions([AfterExpr | AfterBody]
                                 ,remote_calls_from_clauses(Clauses, Acc)
                                 );
remote_calls_from_expression(?LAGER, Acc) ->
    add_remote_module('lager', Acc);
remote_calls_from_expression(?MATCH(LHS, RHS), Acc) ->
    remote_calls_from_expressions([LHS, RHS], Acc);
remote_calls_from_expression(?BEGIN_END(Exprs), Acc) ->
    remote_calls_from_expressions(Exprs, Acc);
remote_calls_from_expression(?CASE(Expression, Clauses), Acc) ->
    remote_calls_from_clauses(Clauses
                             ,remote_calls_from_expression(Expression, Acc)
                             );
remote_calls_from_expression(?IF(Clauses), Acc) ->
    remote_calls_from_clauses(Clauses, Acc);
remote_calls_from_expression(?MAP_CREATION(Exprs), Acc) ->
    remote_calls_from_expressions(Exprs, Acc);
remote_calls_from_expression(?MAP_UPDATE(_Var, Exprs), Acc) ->
    remote_calls_from_expressions(Exprs, Acc);
remote_calls_from_expression(?MAP_FIELD_ASSOC(K, V), Acc) ->
    remote_calls_from_expressions([K, V], Acc);
remote_calls_from_expression(?MAP_FIELD_EXACT(K, V), Acc) ->
    remote_calls_from_expressions([K, V], Acc).

add_remote_module(?ATOM(M)=_A, Acc) ->
    add_remote_module(M, Acc);
add_remote_module(?VAR(_Name)=_V, Acc) -> Acc;
add_remote_module(M, Acc) ->
    case lists:member(M, Acc) of
        'true' -> Acc;
        'false' -> [M | Acc]
    end.

modules_with_apps(App, Modules) ->
    lists:usort([{M, AppOf}
                 || M <- Modules,
                    (AppOf = app_of(M)) =/= 'undefined',
                    AppOf =/= App
                ]
               ).

modules_as_apps(App, Modules) ->
    lists:usort([AppOf
                 || M <- Modules,
                    (AppOf = app_of(M)) =/= 'undefined',
                    AppOf =/= App
                ]
               ).

app_of(Module) ->
    case code:which(Module) of
        'non_existing' -> 'undefined';
        'preloaded' -> 'undefined';
        Path ->
            [_File, "ebin", App | _] = lists:reverse(filename:split(Path)),
            cleanup_app(App)
    end.

cleanup_app(App) ->
    case string:tokens(App, "-") of
        ["kernel", _Vsn] -> 'undefined';
        [AppName, _Vsn] -> kz_util:to_atom(AppName, 'true');
        _ -> kz_util:to_atom(App, 'true')
    end.
