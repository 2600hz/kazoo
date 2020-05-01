-ifndef(KZ_AST_HRL).

-type clauses() :: [erl_parse:abstract_clause()].
-type ast_functions() :: [{module(), atom(), non_neg_integer(), clauses()}].
-type ast_records() :: [{atom(), [atom()]}].
-type ast_exports() :: [{atom(), non_neg_integer()}]. %% [{Function, Arity} | {TypeName, Arity}]
-type ast_specs() :: [{atom(), non_neg_integer(), tuple(), tuple()}]. %% [{Function, Arity, Args, Return}]
-type ast_types() :: [{atom(), tuple()}]. %% [{Name, TypeDef}]

-record(module_ast, {functions = [] :: ast_functions()
                    ,records = [] :: ast_records()
                    ,exports = [] :: ast_exports()
                    ,exported_types = [] :: ast_exports()
                    ,specs = [] :: ast_specs()
                    ,types = [] :: ast_types()
                    ,behaviour :: atom()
                    }).

-type module_ast() :: #module_ast{}.

%% Helper macros for processing Erlang AST forms
-define(EOF, {'eof',_}).

-define(LAGER_RECORDS, {'attribute', _,'lager_records', _}).

-define(AST_FUNCTION(Name, Arity, Clauses)
       ,{'function', _, Name, Arity, Clauses}
       ).

-define(AST_RECORD(Name, Fields)
       ,{'attribute', _, 'record', {Name, Fields}}
       ).

-define(AST_ATTRIBUTE_FILE(Path)
       ,{'attribute',_,'file',{Path,_}}
       ).

-define(AST_ATTRIBUTE_MODULE(M)
       ,{'attribute',_,'module',M}
       ).

-define(BEHAVIOUR(Behaviour)
       ,{'attribute',_,'behaviour',Behaviour}
       ).

-define(AST_EXPORTS(Exports)
       ,{'attribute',_,'export',Exports}
       ).

-define(AST_EXPORTED_TYPES(Types)
       ,{'attribute',_,'export_type',Types}
       ).

-define(TYPE_PRODUCT(Types)
       ,{'type',_,'product', Types}
       ).

-define(TYPE_UNION(Types)
       ,{'type',_,'union', Types}
       ).

-define(REMOTE_TYPE(Module, Fun, Args)
       ,{'remote_type',_, [Module,Fun,Args]}
       ).

-define(USER_TYPE(Name, Args)
       ,{'user_type',_,Name,Args}
       ).

-define(SPEC_TYPE(Type, Args)
       ,{'type',_,Type,Args}
       ).
-define(SPEC_TYPE(Type), ?TYPE(Type, [])).

-define(SPEC_ARGS_RETURN(Args, Return)
       ,[Args, Return]
       ).

-define(SPEC(Fun, Arity, Args, Return)
       ,{'attribute',_,'spec'
        ,{{Fun, Arity}
         ,[{'type',_,'fun', ?SPEC_ARGS_RETURN(Args, Return)}]
         }
        }
       ).

-type acl_fun() :: fun((kz_term:ne_binary()) -> kz_json:object()).
-define(TYPE(Name, TypeDef)
       ,{'attribute',_,'type'
        ,{Name, TypeDef, []}
        }
       ).

-define(CLAUSE(Args, Guards, Body)
       ,{'clause', _, Args, Guards, Body}
       ).

-define(BEGIN_END(Exprs), {'block', _, Exprs}).

-define(VAR(Line, Name), {'var', Line, Name}).
-define(VAR(Name), ?VAR(_, Name)).

-define(MATCH(Left, Right), {'match',_, Left, Right}).

-define(EMPTY_LIST, {'nil',_}).
-define(LIST(Head, Tail)
       ,{'cons', _Line, Head, Tail}
       ).

-define(LC(Expr, Qualifiers)
       ,{'lc', _, Expr, Qualifiers}
       ).
-define(LC_GENERATOR(Pattern, Expr)
       ,{'generate', _, Pattern, Expr}
       ).
-define(LC_BIN_GENERATOR(Pattern, Expr)
       ,{'b_generate', _, Pattern, Expr}
       ).
-define(BC(Expr, Qualifiers)
       ,{'bc', _, Expr, Qualifiers}
       ).

-define(ATOM(Value), {'atom', _, Value}).
-define(INTEGER(I), {'integer', _, I}).
-define(FLOAT(F), {'float', _, F}).
-define(CHAR(C), {'char', _, C}).
-define(STRING(Value), {'string', _, Value}).

-define(GEN_RECORD_FIELD_ACCESS(RecName, FieldName, Value)
       ,{'record_field', _, RecName, FieldName, Value}
       ).
-define(RECORD_FIELD_REST
       ,{'record_field', _, ?VAR('_'), ?ATOM('_')}
       ).
-define(RECORD_FIELD(Key)
       ,{'record_field', _, ?ATOM(Key)}
       ).
-define(RECORD_FIELD_BIND(Key, Value)
       ,{'record_field', _, ?ATOM(Key), Value}
       ).
-define(RECORD_FIELD_ACCESS(RecordName, Name, Value)
       ,?GEN_RECORD_FIELD_ACCESS(?VAR(Name), RecordName, Value)
       ).
-define(RECORD(Name, Fields), {'record', _, Name, Fields}).
-define(GEN_RECORD(NameExpr, RecName, Fields)
       ,{'record', _, NameExpr, RecName, Fields}
       ).
-define(TYPED_RECORD_FIELD(RecordField, Type)
       ,{'typed_record_field', RecordField, Type}
       ).

-define(RECORD_VAR(VarName, RecName, Fields)
       ,?GEN_RECORD(?VAR(VarName), RecName, Fields)
       ).
-define(RECORD_INDEX(Name, Field)
       ,{'record_index',_,Name,?ATOM(Field)}
       ).

-define(CATCH(Expr), {'catch', _, Expr}).
-define(TRY_BODY(Body, CatchClauses)
       ,{'try', _, Body, [], CatchClauses}
       ).
-define(TRY_EXPR(Expr, Clauses, CatchClauses)
       ,{'try', _, Expr, Clauses, CatchClauses, []}
       ).
-define(TRY_BODY_AFTER(Body, Clauses, CatchClauses, AfterBody)
       ,{'try',_,Body,Clauses,CatchClauses,AfterBody}
       ).

-define(GEN_MOD_FUN(MExpr, FExpr)
       ,{'remote', _, MExpr, FExpr}
       ).
-define(MOD_FUN(Module, Function)
       ,?GEN_MOD_FUN(?ATOM(Module),?ATOM(Function))
       ).
-define(DYN_MOD_FUN(Module, Function)
       ,?GEN_MOD_FUN(?VAR(Module),?ATOM(Function))
       ).
-define(MOD_DYN_FUN(Module, Function)
       ,?GEN_MOD_FUN(?ATOM(Module),?VAR(Function))
       ).

-define(GEN_FUN_ARGS(F, Args), {'call', _, F, Args}).
-define(FUN_ARGS(Function, Args), ?GEN_FUN_ARGS(?ATOM(Function), Args)).
-define(DYN_FUN_ARGS(Function, Args), {'call', _, ?VAR(Function), Args}).
-define(DYN_MOD_FUN_ARGS(M, F, As)
       ,{'call', _, ?DYN_MOD_FUN(M, F), As}
       ).
-define(MOD_DYN_FUN_ARGS(M, F, As)
       ,{'call', _, ?MOD_DYN_FUN(M, F), As}
       ).
-define(GEN_MOD_FUN_ARGS(MExpr, FExpr, Args)
       ,?GEN_FUN_ARGS(?GEN_MOD_FUN(MExpr, FExpr), Args)
       ).

-define(GEN_MFA(M, F, A), {'fun', _, {'function', M, F, A}}).
-define(MFA(M, F, A), ?GEN_MFA(?ATOM(M), ?ATOM(F), ?INTEGER(A))).
-define(FA(F, A), {'fun', _, {'function', F, A}}).

-define(ANON(Clauses), {'fun', _, {'clauses', Clauses}}).
-define(NAMED_ANON(Name, Clauses)
       ,{'named_fun', _, Name, Clauses}
       ).

-define(MOD_FUN_ARGS(Module, Function, Args)
       ,{'call',_
        ,?MOD_FUN(Module, Function)
        ,Args
        }
       ).

-define(UNARY_OP(Name, Operand), {'op', _, Name, Operand}).
-define(BINARY_OP(Name, First, Second), {'op', _, Name, First, Second}).

-define(NOT(Operand), ?UNARY_OP('not', Operand)).

-define(APPEND(First, Second), ?BINARY_OP('++', First, Second)).
-define(SUBTRACT(First, Second), ?BINARY_OP('--', First, Second)).
-define(ORELSE(First, Second), ?BINARY_OP('orelse', First, Second)).
-define(ANDALSO(First, Second), ?BINARY_OP('andalso', First, Second)).

-define(BINARY_STRING(Value, L)
       ,{'bin_element', L, {'string', L, Value}, 'default', 'default'}
       ).
-define(BINARY_STRING(Value)
       ,{'bin_element', _, {'string', _, Value}, 'default', 'default'}
       ).
-define(BINARY_VAR(VarName)
       ,{'bin_element',_,?VAR(VarName),'default',['binary']}
       ).
-define(SUB_BINARY(Value)
       ,{'bin_element',_,?BINARY_MATCH([?BINARY_STRING(Value)]),'default',['binary']}
       ).
-define(BINARY_FROM_ATOM(Atom)
       ,{'bin_element',_,?FUN_ARGS(atom_to_binary, [?ATOM(Atom), ?ATOM(utf8)]),'default',['binary']}
       ).

-define(BINARY(Value), {'bin',_, [?BINARY_STRING(Value)]}).
-define(BINARY_MATCH(Matches)
       ,{'bin',_,Matches}
       ).

-define(CASE(Expression, Clauses)
       ,{'case',_, Expression, Clauses}
       ).
-define(IF(Clauses)
       ,{'if',_,Clauses}
       ).

-define(TUPLE(Elements), {'tuple', _, Elements}).

-define(RECEIVE(Clauses), {'receive', _, Clauses}).
-define(RECEIVE(Clauses, AfterExpr, AfterBody)
       ,{'receive', _, Clauses, AfterExpr, AfterBody}
       ).

-define(MAP_CREATION(Exprs), {'map', _, Exprs}).
-define(MAP_UPDATE(Var, Exprs), {'map', _, Var, Exprs}).
-define(MAP_FIELD_ASSOC(K, V), {'map_field_assoc',_,K,V}).
-define(MAP_FIELD_EXACT(K, V), {'map_field_exact',_,K,V}).

-define(LAGER, ?CASE(?TUPLE([?FUN_ARGS('whereis', [?ATOM('lager_event')])
                            ,?FUN_ARGS('whereis', [?ATOM('lager_event')])
                            ,?MOD_FUN_ARGS('lager_config', 'get', _) % args
                            ]
                           )
                    ,_ % clauses
                    )
       ).

-define(LAGER_CALL, {'remote', _, {'atom', _, 'lager'}, _}).

-define(KS_AST_HRL, 'true').
-endif.
