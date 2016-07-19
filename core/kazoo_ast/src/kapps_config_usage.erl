-module(kapps_config_usage).

-export([process_project/0, process_app/1, process_module/1]).

-include_lib("kazoo_ast/include/kz_ast.hrl").

process_project() ->
    'ok'.

-spec process_app(atom()) -> kz_json:object().
process_app(App) ->
    process_app(App, kz_json:new()).

process_app(App, Schemas) ->
    {'ok', Modules} = application:get_key(App, 'modules'),
    lists:foldl(fun module_to_schema/2, Schemas, Modules).

process_module(Module) ->
    module_to_schema(Module, kz_json:new()).

module_to_schema(Module, Schemas) ->
    case kz_ast_util:module_ast(Module) of
        'undefined' -> 'undefined';
        {M, AST} ->
            Fs = kz_ast_util:add_module_ast([], M, AST),
            try functions_to_schema(Fs, Schemas)
            catch
                _E:_R ->
                    ST = erlang:get_stacktrace(),
                    io:format("failed on ~p: ~s: ~p~n", [Module, _E, _R]),
                    [io:format("st: ~p~n", [S]) || S <- ST],
                    Schemas
            end
    end.

functions_to_schema(Fs, Schemas) ->
    lists:foldl(fun function_to_schema/2
                ,Schemas
                ,Fs
               ).

function_to_schema({_Module, _Function, _Arity, Clauses}, Schemas) ->
    clauses_to_schema(Clauses, Schemas).

clauses_to_schema(Clauses, Schemas) ->
    lists:foldl(fun clause_to_schema/2
                ,Schemas
                ,Clauses
               ).

clause_to_schema(?CLAUSE(_Args, _Guards, Expressions), Schemas) ->
    expressions_to_schema(Expressions, Schemas).

expressions_to_schema(Expressions, Schemas) ->
    lists:foldl(fun expression_to_schema/2
               ,Schemas
               ,Expressions
               ).

expression_to_schema(?MOD_FUN_ARGS('kapps_config', F, Args), Schemas) ->
    config_to_schema(F, Args, Schemas);
expression_to_schema(?MOD_FUN_ARGS(_M, _F, Args), Schemas) ->
    expressions_to_schema(Args, Schemas);
expression_to_schema(?DYN_MOD_FUN(_M, _F), Schemas) ->
    Schemas;
expression_to_schema(?FUN_ARGS(_F, Args), Schemas) ->
    expressions_to_schema(Args, Schemas);
expression_to_schema(?GEN_MFA(_M, _F, _Arity), Schemas) ->
    Schemas;
expression_to_schema(?FA(_F, _Arity), Schemas) ->
    Schemas;
expression_to_schema(?BINARY_OP(_Name, First, Second), Schemas) ->
    expressions_to_schema([First, Second], Schemas);
expression_to_schema(?UNARY_OP(_Name, First), Schemas) ->
    expression_to_schema(First, Schemas);
expression_to_schema(?CATCH(Expression), Schemas) ->
    expression_to_schema(Expression, Schemas);
expression_to_schema(?TRY_BODY(Body, Clauses), Schemas) ->
    clauses_to_schema(Clauses
                     ,expression_to_schema(Body, Schemas)
                     );
expression_to_schema(?TRY_EXPR(Expr, Clauses, CatchClauses), Schemas) ->
    clauses_to_schema(Clauses ++ CatchClauses
                     ,expressions_to_schema(Expr, Schemas)
                     );
expression_to_schema(?LC(Expr, Qualifiers), Schemas) ->
    expressions_to_schema([Expr | Qualifiers], Schemas);
expression_to_schema(?LC_GENERATOR(Pattern, Expr), Schemas) ->
    expressions_to_schema([Pattern, Expr], Schemas);
expression_to_schema(?BC(Expr, Qualifiers), Schemas) ->
    expressions_to_schema([Expr | Qualifiers], Schemas);
expression_to_schema(?LC_BIN_GENERATOR(Pattern, Expr), Schemas) ->
    expressions_to_schema([Pattern, Expr], Schemas);
expression_to_schema(?ANON(Clauses), Schemas) ->
    clauses_to_schema(Clauses, Schemas);
expression_to_schema(?VAR(_), Schemas) ->
    Schemas;
expression_to_schema(?BINARY_MATCH(_), Schemas) ->
    Schemas;
expression_to_schema(?STRING(_), Schemas) ->
    Schemas;
expression_to_schema(?RECORD_VAR(_VarName, _RecName, Fields), Schemas) ->
    expressions_to_schema(Fields, Schemas);
expression_to_schema(?RECORD(_Name, Fields), Schemas) ->
    expressions_to_schema(Fields, Schemas);
expression_to_schema(?RECORD_FIELD_BIND(_Key, Value), Schemas) ->
    expression_to_schema(Value, Schemas);
expression_to_schema(?RECORD_FIELD_ACCESS(_RecordName, _Name, Value), Schemas) ->
    expression_to_schema(Value, Schemas);
expression_to_schema(?DYN_FUN_ARGS(_F, Args), Schemas) ->
    expressions_to_schema(Args, Schemas);
expression_to_schema(?DYN_MOD_FUN_ARGS(_M, _F, Args), Schemas) ->
    expressions_to_schema(Args, Schemas);
expression_to_schema(?MOD_DYN_FUN_ARGS(_M, _F, Args), Schemas) ->
    expressions_to_schema(Args, Schemas);
expression_to_schema(?GEN_MOD_FUN_ARGS(MExpr, FExpr, Args), Schemas) ->
    expressions_to_schema([MExpr, FExpr | Args], Schemas);
expression_to_schema(?ATOM(_), Schemas) ->
    Schemas;
expression_to_schema(?INTEGER(_), Schemas) ->
    Schemas;
expression_to_schema(?FLOAT(_), Schemas) ->
    Schemas;
expression_to_schema(?CHAR(_), Schemas) ->
    Schemas;
expression_to_schema(?TUPLE(_Elements), Schemas) ->
    Schemas;
expression_to_schema(?EMPTY_LIST, Schemas) ->
    Schemas;
expression_to_schema(?LIST(Head, Tail), Schemas) ->
    expressions_to_schema([Head, Tail], Schemas);
expression_to_schema(?RECEIVE(Clauses), Schemas) ->
    clauses_to_schema(Clauses, Schemas);
expression_to_schema(?RECEIVE(Clauses, AfterExpr, AfterBody), Schemas) ->
    expressions_to_schema([AfterExpr | AfterBody]
                         ,clauses_to_schema(Clauses, Schemas)
                         );
expression_to_schema(?LAGER, Schemas) ->
    Schemas;
expression_to_schema(?MATCH(LHS, RHS), Schemas) ->
    expressions_to_schema([LHS, RHS], Schemas);
expression_to_schema(?BEGIN_END(Exprs), Schemas) ->
    expressions_to_schema(Exprs, Schemas);
expression_to_schema(?CASE(Expression, Clauses), Schema) ->
    clauses_to_schema(Clauses
                     ,expression_to_schema(Expression, Schema)
                     );
expression_to_schema(_Expr, Schemas) ->
    io:format("unhandled expression: ~p~n", [_Expr]),
    Schemas.

config_to_schema('get_all_kvs', _Args, Schemas) ->
    Schemas;
config_to_schema('flush', _Args, Schemas) ->
    Schemas;
config_to_schema(F, [Cat, K], Schemas) ->
    config_to_schema(F, [Cat, K, 'undefined'], Schemas);
config_to_schema(F, [Cat, K, Default], Schemas) ->
    Document = kz_ast_util:binary_match_to_binary(Cat),

    Key = key_to_key_path(K),

    config_key_to_schema(F, Document, Key, Default, Schemas).

config_key_to_schema(_F, _Document, 'undefined', _Default, Schemas) ->
    Schemas;
config_key_to_schema(F, Document, Key, Default, Schemas) ->
    Properties = guess_properties(Key, guess_type(F), Default),

    Existing = kz_json:get_json_value([Document, <<"properties">> | Key]
                                     ,Schemas
                                     ,kz_json:new()
                                     ),

    Updated = kz_json:merge_jobjs(Existing, Properties),

    kz_json:set_value([Document, <<"properties">> | Key], Updated, Schemas).

key_to_key_path(?ATOM(A)) -> [kz_util:to_binary(A)];
key_to_key_path(?VAR(_)) -> 'undefined';
key_to_key_path(?EMPTY_LIST) -> [];
key_to_key_path(?LIST(?MOD_FUN_ARGS('kapps_config', _F, [Doc, Field | _]), Tail)) ->
    [iolist_to_binary([${
                       ,kz_ast_util:binary_match_to_binary(Doc)
                       ,"."
                       ,kz_ast_util:binary_match_to_binary(Field)
                       ,$}
                      ]
                     )
     ,<<"properties">>
     | key_to_key_path(Tail)
    ];
key_to_key_path(?GEN_MOD_FUN_ARGS(_M, _F, _Args)) ->
    'undefined';
key_to_key_path(?LIST(Head, Tail)) ->
    [kz_ast_util:binary_match_to_binary(Head)
    ,<<"properties">>
    | key_to_key_path(Tail)
    ];
key_to_key_path(?BINARY_MATCH(K)) ->
    [kz_ast_util:binary_match_to_binary(K)].

guess_type(F) -> guess_type(F, 'undefined').

guess_type('get_is_true', _Default) -><<"boolean">>;
guess_type('get', Default) -> guess_type_by_default(Default);
guess_type('get_non_empty', Default) -> guess_type_by_default(Default);
guess_type('get_binary', _Default) -> <<"string">>;
guess_type('get_ne_binary', _Default) -> <<"string">>;
guess_type('get_json', _Default) -> <<"object">>;
guess_type('get_string', _Default) -> <<"string">>;
guess_type('get_integer', _Default) -> <<"integer">>;
guess_type('get_float', _Default) -> <<"number">>;
guess_type('set_default', _Default) -> 'undefined';
guess_type('set', _Default) -> 'undefined';
guess_type(_F, _Default) ->
    io:format("  no guess for ~p ~p~n", [_F, _Default]),
    'undefined'.

guess_type_by_default('undefined') -> 'undefined';
guess_type_by_default(?EMPTY_LIST) -> <<"array">>;
guess_type_by_default(?LIST(_Head, _Tail)) -> <<"array">>.

guess_properties(<<_/binary>> = Key, Type, Default) ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"type">>, Type}
        ,{<<"description">>, <<>>}
        ,{<<"name">>, Key}
        ,{<<"default">>, default_value(Default)}
        ]
       )
     );
guess_properties([<<_/binary>> = Key], Type, Default) ->
    guess_properties(Key, Type, Default);
guess_properties([Key, <<"properties">>], Type, Default) ->
    guess_properties(Key, Type, Default);
guess_properties([_Key, <<"properties">> | Rest], Type, Default) ->
    guess_properties(Rest, Type, Default).

default_value('undefined') -> 'undefined';
default_value(?ATOM('true')) -> 'true';
default_value(?ATOM('false')) -> 'false';
default_value(?ATOM('undefined')) -> 'undefined';
default_value(?ATOM(V)) -> kz_util:to_binary(V);
default_value(?VAR(_)) -> 'undefined';
default_value(?INTEGER(I)) -> I;
default_value(?FLOAT(F)) -> F;
default_value(?BINARY_OP(Op, Arg1, Arg2)) ->
    erlang:Op(default_value(Arg1), default_value(Arg2));
default_value(?BINARY_MATCH(Match)) -> kz_ast_util:binary_match_to_binary(Match);
default_value(?EMPTY_LIST) -> [];
default_value(?TUPLE([Key, Value])) ->
    {default_value(Key), default_value(Value)};
default_value(?LIST(Head, Tail)) ->
    [default_value(Head) | default_value(Tail)];
default_value(?MOD_FUN_ARGS('kz_json', 'from_list', L)) ->
    default_values_from_list(L);
default_value(?MOD_FUN_ARGS(_M, _F, _Args)) -> 'undefined'.

default_values_from_list(KVs) ->
    lists:foldl(fun default_value_from_kv/2
               ,kz_json:new()
               ,KVs
               ).

default_value_from_kv(KV, Acc) ->
    KVs = props:filter_undefined(default_value(KV)),
    kz_json:set_values(KVs, Acc).
