-module(raw_json_usage).

-export([process_project/0
        ,process_app/1
        ,process_module/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").

process_project() ->
    io:format("processing raw_json: "),
    Raw = lists:foldl(fun process_app/2
                     ,[]
                     ,kz_ast_util:project_apps()
                     ),
    io:format(" done~n"),
    Raw.

process_app(App) ->
    process_app(App, []).

process_app(App, Acc) ->
    lists:foldl(fun process_module/2
               ,Acc
               ,kz_ast_util:app_modules(App)
               ).

process_module(Module) ->
    process_module(Module, []).

process_module(Module, Acc) ->
    case kz_ast_util:module_ast(Module) of
        'undefined' -> Acc;
        {M, AST} ->
            Fs = kz_ast_util:add_module_ast([], M, AST),
            process_functions(M, Fs, Acc)
    end.

process_functions(M, Fs, Acc) ->
    try raw_json_in_functions(Fs, []) of
        [] -> io:format("."), Acc;
        Lines -> io:format("x"), [{M, lists:usort(Lines)} | Acc]
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            io:format("failed to process ~p: ~s: ~p~n", [M, _E, _R]),
            [io:format("~p~n", [S]) || S <- ST],
            Acc
    end.

raw_json_in_functions(Fs, Acc) ->
    lists:foldl(fun raw_json_in_function/2
               ,Acc
               ,Fs
               ).

raw_json_in_function({_Module, _Function, _Arity, [_|_]=Clauses}, Acc) ->
    raw_json_in_clauses(Clauses, Acc).

raw_json_in_clauses(Clauses, Acc) ->
    lists:foldl(fun raw_json_in_clause/2
               ,Acc
               ,Clauses
               ).

raw_json_in_clause(?CLAUSE(Args, Guards, Expressions), Acc)
  when is_list(Args)
       andalso is_list(Guards)
       andalso is_list(Expressions) ->
    lists:foldl(fun raw_json_in_expression/2
               ,Acc
               ,Args ++ lists:flatten(Guards) ++ Expressions
               ).

raw_json_in_expressions([], Acc) -> Acc;
raw_json_in_expressions([_|_]=Expressions, Acc) ->
    lists:foldl(fun raw_json_in_expression/2
               ,Acc
               ,Expressions
               ).

raw_json_in_expression(?LAGER, Acc) -> Acc;
raw_json_in_expression(?CASE(Expr, Clauses), Acc) ->
    Acc1 = raw_json_in_expression(Expr, Acc),
    raw_json_in_clauses(Clauses, Acc1);
raw_json_in_expression(?IF(Clauses), Acc) ->
    raw_json_in_clauses(Clauses, Acc);
raw_json_in_expression(?TUPLE(?EMPTY_LIST)=Tuple, Acc) ->
    [element(2, Tuple) | Acc];
raw_json_in_expression(?TUPLE([?LIST(H, T)]), Acc) ->
    is_list_json(H, T, Acc);
raw_json_in_expression(?TUPLE(Elements), Acc) ->
    raw_json_in_expressions(Elements, Acc);
raw_json_in_expression(?GEN_MOD_FUN_ARGS(_M, _F, Args), Acc) ->
    raw_json_in_expressions(Args, Acc);
raw_json_in_expression(?GEN_FUN_ARGS(_F, Args), Acc) ->
    raw_json_in_expressions(Args, Acc);
raw_json_in_expression(?ATOM(_), Acc) -> Acc;
raw_json_in_expression(?INTEGER(_), Acc) -> Acc;
raw_json_in_expression(?CHAR(_), Acc) -> Acc;
raw_json_in_expression(?FLOAT(_), Acc) -> Acc;
raw_json_in_expression(?STRING(_), Acc) -> Acc;
raw_json_in_expression(?VAR(_), Acc) -> Acc;
raw_json_in_expression(?BINARY_MATCH(_), Acc) -> Acc;
raw_json_in_expression(?GEN_RECORD(NameExpr, _RecName, Fields), Acc) ->
    raw_json_in_expressions([NameExpr | Fields], Acc);
raw_json_in_expression(?GEN_RECORD_FIELD_ACCESS(NameExpr, _RecordName, Value), Acc) ->
    raw_json_in_expressions([NameExpr, Value], Acc);

raw_json_in_expression(?RECORD(_N, Fields), Acc) ->
    raw_json_in_expressions(Fields, Acc);

raw_json_in_expression(?RECORD_INDEX(_Name, _Field), Acc) -> Acc;
raw_json_in_expression(?RECORD_FIELD_BIND(_Key, Value), Acc) ->
    raw_json_in_expression(Value, Acc);
raw_json_in_expression(?RECORD_FIELD_REST, Acc) -> Acc;
raw_json_in_expression(?FA(_F, _Artiy), Acc) -> Acc;
raw_json_in_expression(?GEN_MFA(_M, _F, _Arity), Acc) -> Acc;
raw_json_in_expression(?ANON(Clauses), Acc) ->
    raw_json_in_clauses(Clauses, Acc);
raw_json_in_expression(?BINARY_OP(_Op, Expr1, Expr2), Acc) ->
    raw_json_in_expressions([Expr1, Expr2], Acc);
raw_json_in_expression(?UNARY_OP(_Op, Expr), Acc) ->
    raw_json_in_expression(Expr, Acc);
raw_json_in_expression(?RECEIVE(Clauses), Acc) ->
    raw_json_in_clauses(Clauses, Acc);
raw_json_in_expression(?RECEIVE(Clauses, AfterExpr, AfterBody), Acc) ->
    raw_json_in_expressions([AfterExpr | AfterBody]
                           ,raw_json_in_clauses(Clauses, Acc)
                           );
raw_json_in_expression(?BC(Expr, Qualifiers), Acc) ->
    raw_json_in_expressions([Expr | Qualifiers], Acc);
raw_json_in_expression(?LC_BIN_GENERATOR(Pattern, Expr), Acc) ->
    raw_json_in_expressions([Pattern, Expr], Acc);
raw_json_in_expression(?LC(Expr, Qualifiers), Acc) ->
    raw_json_in_expressions([Expr | Qualifiers], Acc);
raw_json_in_expression(?LC_GENERATOR(Pattern, Expr), Acc) ->
    raw_json_in_expressions([Pattern, Expr], Acc);
raw_json_in_expression(?CATCH(Expr), Acc) ->
    raw_json_in_expression(Expr, Acc);
raw_json_in_expression(?TRY_EXPR(Body, Clauses, CatchClauses), Acc) ->
    raw_json_in_clauses(Clauses ++ CatchClauses
                       ,raw_json_in_expressions(Body, Acc)
                       );
raw_json_in_expression(?TRY_BODY_AFTER(Body, Clauses, CatchClauses, AfterBody), Acc) ->
    raw_json_in_clauses(Clauses ++ CatchClauses
                       ,raw_json_in_expressions(Body ++ AfterBody, Acc)
                       );
raw_json_in_expression(?BEGIN_END(Exprs), Acc) ->
    raw_json_in_expressions(Exprs, Acc);
raw_json_in_expression(?EMPTY_LIST, Acc) -> Acc;
raw_json_in_expression(?LIST(Head, Tail), Acc) ->
    raw_json_in_expressions([Head, Tail], Acc);
raw_json_in_expression(?MAP_CREATION(Exprs), Acc) ->
    raw_json_in_expressions(Exprs, Acc);
raw_json_in_expression(?MAP_UPDATE(_Var, Exprs), Acc) ->
    raw_json_in_expressions(Exprs, Acc);
raw_json_in_expression(?MAP_FIELD_ASSOC(K, V), Acc) ->
    raw_json_in_expressions([K, V], Acc);
raw_json_in_expression(?MAP_FIELD_EXACT(K, V), Acc) ->
    raw_json_in_expressions([K, V], Acc);
raw_json_in_expression(?MATCH(Left, Right), Acc) ->
    raw_json_in_expressions([Left, Right], Acc).

is_list_json(?TUPLE([_Key, _Value])=T, ?EMPTY_LIST, Acc) ->
    [element(2, T) | Acc];
is_list_json(_El, ?EMPTY_LIST, Acc) -> Acc;
is_list_json(?TUPLE([_Key, _Value]), ?LIST(Head, Tail), Acc) ->
    is_list_json(Head, Tail, Acc);
is_list_json(?TUPLE([_Key, _Value])=T, ?VAR(_Name), Acc) ->
    [element(2, T) | Acc].
