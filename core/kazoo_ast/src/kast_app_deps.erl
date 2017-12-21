-module(kast_app_deps).

-export([process_project/0
        ,process_app/1

        ,fix_project_deps/0
        ,fix_app_deps/1
        ,dot_file/0 ,dot_file/1
        ,circles/0, circles/1
        ]).

-export([remote_calls/1
        ,remote_calls_from_module/1
        ,remote_apps/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").

-define(DEBUG(_Fmt, _Args), 'ok').
%% -define(DEBUG(Fmt, Args), io:format([$~, $p, $  | Fmt], [?LINE | Args])).

-spec dot_file() -> 'ok' |
                    {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
-spec dot_file(atom()) ->
                      'ok' |
                      {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
dot_file() ->
    Markup = [create_dot_markup(App, remote_apps(App)) || App <- kz_ast_util:project_apps()],
    create_dot_file("kazoo_project", Markup).

dot_file(App) ->
    AppDeps = remote_apps(App),
    create_dot_file(App, create_dot_markup(App, AppDeps)).

create_dot_file(Name, Markup) ->
    Filename = <<"/tmp/", (kz_term:to_binary(Name))/binary, ".dot">>,
    'ok' = file:write_file(Filename
                          ,["digraph kast_app_deps {\n"
                           ,Markup
                           ,"}\n"
                           ]
                          ),
    io:format("wrote DOT file to ~s~n", [Filename]).

create_dot_markup(App, AppDeps) ->
    RemoteApps = lists:usort([A || {_Module, A} <- AppDeps]),
    app_markup(App, RemoteApps).

app_markup(App, RemoteApps) ->
    M = [ [$", kz_term:to_binary(App), $"
          ," -> "
          ,$", kz_term:to_binary(RemoteApp), $"
          ," [weight=1];\n"
          ]
          || RemoteApp <- RemoteApps
        ],
    ?DEBUG("adding '~s'~n", [M]),
    M.

-spec fix_project_deps() -> 'ok'.
fix_project_deps() ->
    Deps = process_project(),
    io:format("processing app files "),
    _ = [fix_app_deps(App, Missing, Unneeded)
         || {App, Missing, Unneeded} <- Deps
        ],
    io:format(" done.~n").

-spec fix_app_deps(atom()) -> 'ok'.
fix_app_deps(App) ->
    _ = [fix_app_deps(App, Missing, Unneeded)
         || {_App, Missing, Unneeded} <- process_app(App)
        ],
    'ok'.

-spec configured_dep_apps(atom()) -> [atom()].
configured_dep_apps(App) ->
    {'application', App, Properties} = read_app_src(App),
    lists:usort(props:get_value('applications', Properties)).

fix_app_deps(App, Missing, Unneeded) ->
    ConfiguredApps = configured_dep_apps(App),

    ?DEBUG("app ~p~n conf: ~p~n missing ~p~n unneeded ~p~n"
          ,[App, ConfiguredApps, Missing, Unneeded]
          ),
    Union = ordsets:union(ConfiguredApps, Missing),
    ?DEBUG("union: ~p~n", [Union]),

    Subtracted = ordsets:subtract(Union, Unneeded),
    ?DEBUG("sub: ~p~n", [Subtracted]),

    case Subtracted of
        ConfiguredApps ->
            ?DEBUG("no change needed~n", []),
            io:format(".");
        UpdatedApps ->
            ?DEBUG("updated ~s apps to ~p~n", [App, UpdatedApps]),
            {'application', App, Properties} = read_app_src(App),

            write_app_src(App
                         ,{'application'
                          ,App
                          ,props:set_value('applications', UpdatedApps, Properties)
                          }
                         ),
            io:format("x")
    end.

read_app_src(App) ->
    File = app_src_filename(App),
    ?DEBUG("reading app file ~s~n", [File]),
    {'ok', [Config]} = file:consult(kz_term:to_list(File)),
    Config.

write_app_src(App, Config) ->
    File = app_src_filename(App),
    file:write_file(File, io_lib:format("~p.~n", [Config])).

app_src_filename(App) ->
    AppBin = kz_term:to_binary(App),
    filename:join([code:lib_dir(App, 'src')
                  ,<<AppBin/binary, ".app.src">>
                  ]).

-spec circles() -> [{atom(), [atom()]}].
circles() ->
    {'ok', Cache} = kz_cache:start_link(?MODULE),
    io:format("finding circular dependencies "),
    Circles = [circles(App)
               || App <- kz_ast_util:project_apps()
              ],
    kz_cache:stop_local(Cache),
    io:format(" done~n"),
    Circles.

-spec circles(atom()) -> {atom(), [atom()]}.
circles(App) ->
    RemoteApps = remote_app_list(App),
    ?DEBUG("app ~p has remote apps ~p~n", [App, RemoteApps]),
    CircularDeps = circular_deps(App, RemoteApps),
    [] =/= CircularDeps
        andalso ?DEBUG("app ~p has circular deps ~p~n", [App, CircularDeps]),
    {App, lists:usort(CircularDeps)}.

-spec remote_app_list(atom()) -> [atom()].
remote_app_list(App) ->
    lists:usort([A || {_Module, A} <- remote_apps(App)]).

-spec remote_apps(atom()) -> [{atom(), atom()}].
remote_apps(App) ->
    case whereis(?MODULE) of
        'undefined' -> uncached(App);
        _Cache ->
            check_cache_first(App)
    end.

uncached(App) ->
    modules_with_apps(App, remote_calls(App)).

check_cache_first(App) ->
    case kz_cache:fetch_local(?MODULE, App) of
        {'ok', Remote} -> Remote;
        {'error', 'not_found'} ->
            Remote = uncached(App),
            ?DEBUG("caching deps for ~p~n", [App]),
            kz_cache:store_local(?MODULE, App, Remote),
            Remote
    end.

-spec circular_deps(atom(), any()) -> [atom()].
circular_deps(App, RemoteApps) ->
    lists:foldl(fun(Dep, Acc) -> circles_fold(App, Dep, Acc) end
               ,[]
               ,RemoteApps
               ).

circles_fold(App, Dep, Acc) ->
    is_kazoo_app(Dep)
        andalso ?DEBUG("dep ~p (of ~p) has remote apps ~p~n"
                      ,[Dep, App, remote_app_list(Dep)]
                      ),
    case is_kazoo_app(Dep)
        andalso [A || A <- remote_app_list(Dep), A =:= App]
    of
        'false' -> ?DEBUG("dep ~p (of ~p) is not a kazoo app (may be a lib)~n", [Dep, App]), Acc;
        [] -> Acc;
        _ -> ?DEBUG("adding dep ~s to list", [Dep]), [Dep|Acc]
    end.

-spec is_kazoo_app(atom() | file:filename() | {'error', 'bad_name'}) -> boolean().
is_kazoo_app({'error', 'bad_name'}) -> 'false';
is_kazoo_app(App) when is_atom(App) ->
    is_kazoo_app(code:lib_dir(App));
is_kazoo_app(Path) when is_list(Path) ->
    'nomatch' =/= re:run(Path, "(core|applications)/").

-type app_deps() :: {atom(), [atom()], [atom()]}.
-type apps_deps() :: [app_deps()].
-spec process_project() -> apps_deps().
process_project() ->
    {'ok', Cache} = kz_cache:start_link(?MODULE),
    io:format("processing application dependencies: "),
    Discrepencies = lists:foldl(fun process_app/2
                               ,[]
                               ,kz_ast_util:project_apps()
                               ),
    io:format(" done~n"),
    kz_cache:stop_local(Cache),
    lists:keysort(1, Discrepencies).

-spec process_app(atom()) -> apps_deps().
process_app(App) ->
    process_app(App, []).

process_app('kazoo_ast', Acc) -> Acc;
process_app(App, Acc) ->
    case application:get_key(App, 'applications') of
        'undefined' ->
            'ok' = application:load(App),
            process_app(App, Acc);
        {'ok', ExistingApps} ->
            ?DEBUG("app ~p~n existing: ~p~n", [App, ExistingApps]),
            process_app(App, Acc, ExistingApps)
    end.

process_app(App, Acc, ExistingApps) ->
    KnownApps = ordsets:from_list(ExistingApps -- ['kernel']),

    RemoteModules = remote_calls(App),
    RemoteApps = ordsets:from_list(modules_as_apps(App, RemoteModules)),

    ?DEBUG("remote modules: ~p~n", [RemoteModules]),

    ?DEBUG(" known ~p~nremote apps: ~p~n", [KnownApps, RemoteApps]),

    Missing = ordsets:subtract(RemoteApps, KnownApps),
    Unneeded = ordsets:subtract(KnownApps, RemoteApps),

    case {Missing, Unneeded} of
        {[], []} -> Acc;
        _ -> [{App, Missing, Unneeded} | Acc]
    end.

-spec remote_calls(atom()) -> [atom()].
remote_calls(App) ->
    lists:usort(
      lists:foldl(fun remote_calls_from_module/2
                 ,[]
                 ,kz_ast_util:app_modules(App)
                 )
     ).

-spec remote_calls_from_module(atom()) -> [atom()].
remote_calls_from_module(Module) ->
    remote_calls_from_module(Module, []).

remote_calls_from_module(Module, Acc) ->
    io:format("."),
    {M, AST} = kz_ast_util:module_ast(Module),
    #module_ast{functions=Fs} = kz_ast_util:add_module_ast(#module_ast{}, M, AST),
    try remote_calls_from_functions(Fs, Acc) of
        Modules -> lists:delete(M, Modules)
    catch
        _E:R ->
            ST = erlang:get_stacktrace(),
            io:format("process module '~s' failed: ~s: ~p~n", [Module, _E, R]),
            [io:format("st: ~p~n", [S]) || S <- ST],
            throw(R)
    end.

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
remote_calls_from_expression(?NAMED_ANON(_Name, Clauses), Acc) ->
    remote_calls_from_clauses(Clauses, Acc);
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

-spec modules_with_apps(atom(), [atom()]) ->
                               [{atom(), atom()}].
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
        [AppName, _Vsn] -> kz_term:to_atom(AppName, 'true');
        _ -> kz_term:to_atom(App, 'true')
    end.
