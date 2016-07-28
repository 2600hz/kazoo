-ifndef(KZ_AST_HRL).

%% Helper macros for processing Erlang AST forms

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

-define(RECORD_FIELD_BIND(Key, Value), {'record_field', _
                                       ,?ATOM(Key)
                                       ,Value
                                       }).
-define(RECORD_FIELD_ACCESS(RecordName, Name, Value)
       ,{'record_field', _, ?VAR(Name), RecordName, Value}
       ).
-define(RECORD(Name, Fields), {'record', _, Name, Fields}).
-define(RECORD_VAR(VarName, RecName, Fields)
        ,{'record', _, ?VAR(VarName), RecName, Fields}
       ).

-define(CATCH(Expr), {'catch', _, Expr}).
-define(TRY_BODY(Body, CatchClauses)
       ,{'try', _, Body, [], CatchClauses}
       ).
-define(TRY_EXPR(Expr, Clauses, CatchClauses)
       ,{'try', _, Expr, Clauses, CatchClauses, []}
       ).

-define(GEN_MOD_FUN(MExpr, FExpr), {'remote', _, MExpr, FExpr}).
-define(MOD_FUN(Module, Function)
       ,?GEN_MOD_FUN(?ATOM(Module),?ATOM(Function))
       ).
-define(DYN_MOD_FUN(Module, Function)
       ,?GEN_MOD_FUN(?VAR(Module),?ATOM(Function))
       ).
-define(MOD_DYN_FUN(Module, Function)
       ,?GEN_MOD_FUN(?ATOM(Module),?VAR(Function))
       ).

-define(FUN_ARGS(Function, Args), {'call', _, ?ATOM(Function), Args}).
-define(DYN_FUN_ARGS(Function, Args), {'call', _, ?VAR(Function), Args}).
-define(DYN_MOD_FUN_ARGS(M, F, As)
       ,{'call', _, ?DYN_MOD_FUN(M, F), As}
       ).
-define(MOD_DYN_FUN_ARGS(M, F, As)
       ,{'call', _, ?MOD_DYN_FUN(M, F), As}
       ).
-define(GEN_MOD_FUN_ARGS(MExpr, FExpr, Args)
        ,{'call', _, ?GEN_MOD_FUN(MExpr, FExpr), Args}
       ).

-define(GEN_MFA(M, F, A), {'fun', _, {'function', M, F, A}}).
-define(MFA(M, F, A), ?GEN_MFA(?ATOM(M), ?ATOM(F), ?INTEGER(A))).
-define(FA(F, A), {'fun', _, {'function', F, A}}).

-define(ANON(Clauses), {'fun', _, {'clauses', Clauses}}).

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
-define(ORELSE(First, Second), ?BINARY_OP('orelse', First, Second)).
-define(ANDALSO(First, Second), ?BINARY_OP('andalso', First, Second)).

-define(BINARY_STRING(Value)
       ,{'bin_element', _, {'string', _, Value}, 'default', 'default'}
       ).
-define(BINARY_VAR(VarName)
       ,{'bin_element',_,?VAR(VarName),'default',['binary']}
       ).
-define(SUB_BINARY(Value)
        ,{'bin_element',_,?BINARY_MATCH([?BINARY_STRING(Value)]),'default',['binary']}
       ).
-define(BINARY(Value), {'bin',_, [?BINARY_STRING(Value)]}).
-define(BINARY_MATCH(Matches)
       ,{'bin',_,Matches}
       ).

-define(CASE(Expression, Clauses)
       ,{'case',_, Expression, Clauses}
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

-define(LAGER, ?CASE(
                  ?TUPLE(
                     [?FUN_ARGS('whereis', [?ATOM('lager_event')])
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
