-ifndef(KZ_AST_HRL).

%% Helper macros for processing Erlang AST forms

-define(CLAUSE(Args, Guards, Body)
        ,{'clause', _, Args, Guards, Body}
       ).

-define(VAR(Name), {'var', _, Name}).

-define(MATCH(Left, Right), {'match',_, Left, Right}).

-define(ATOM(Value), {'atom', _, Value}).

-define(MOD_FUN(Module, Function), {'remote',_,?ATOM(Module),?ATOM(Function)}).

-define(FUN_ARGS(Function, Args), {'call', _, ?ATOM(Function), Args}).

-define(MOD_FUN_ARGS(Module, Function, Args)
       ,{'call',_
        ,?MOD_FUN(Module, Function)
        ,Args
        }
       ).

-define(BINARY_STRING(Value)
        ,{'bin_element', _, {'string', _, Value}, 'default', 'default'}
       ).
-define(BINARY_VAR(VarName)
       ,{'bin_element',_,?VAR(VarName),'default',['binary']}
       ).
-define(BINARY(Value), {'bin',_, [?BINARY_STRING(Value)]}).
-define(BINARY_MATCH(Matches)
       ,{'bin',_,Matches}
       ).

-define(CASE(Expression, Clauses)
        ,{'case',_, Expression, Clauses}
       ).

-define(TUPLE(Elements), {'tuple', _, Elements}).

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
