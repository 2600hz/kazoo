-module(cf_data_usage).

%% module for parsing callflow actions for Data usage

-export([process/0, process/1
        ,to_schema_docs/0, to_schema_doc/1
        ]).

-include("callflow.hrl").
-include_lib("kazoo/include/kz_ast.hrl").
-include_lib("kazoo/src/kz_json.hrl").

-define(DEBUG(_Fmt, _Args), 'ok').
%% -define(DEBUG(Fmt, Args), io:format([$~, $p, $  | Fmt], [?LINE | Args])).

-record(usage, {usages = [] %% places the Data is accessed
               ,data_var_name = 'Data' %% Tracks current var name
               ,data_var_aliases = [] %% typically when kz_json:set_value is used
               ,current_module %% what module are we currently in
               ,functions = [] %% AST functions loaded
               ,visited = [] %% MFAs visited (to stop recursion)
               }).

to_schema_docs() ->
    _ = [to_schema_doc(M, Usage) || {M, Usage} <- process()],
    'ok'.

to_schema_doc(M) ->
    to_schema_doc(M, process(M)).

to_schema_doc(M, Usage) ->
    <<"cf_", Base/binary>> = kz_util:to_binary(M),
    Schema = schema_path(Base),
    ensure_file_exists(Schema),
    update_schema(Base, Schema, Usage).

update_schema(Base, Path, Usage) ->
    {'ok', Bin} = file:read_file(Path),
    Schema = kz_json:decode(Bin),
    case augment_schema(ensure_id(Base, Schema), Usage) of
        Schema -> 'ok';
        Augmented ->
            'ok' = file:write_file(Path, kz_json:encode(Augmented))
    end.

ensure_id(Base, Schema) ->
    ID = <<"callflows.", Base/binary>>,
    case kz_doc:id(Schema) of
        ID -> Schema;
        _Id ->
            ?DEBUG("updating _id from ~p to ~p~n", [_Id, ID]),
            kz_json:set_value(<<"_id">>, ID, Schema)
    end.

augment_schema(Schema, Usage) ->
    lists:foldl(fun augment_with_usage/2, Schema, Usage).

augment_with_usage({_M, F, [_|_]=Ks, _Data, Default}, Schema) ->
    maybe_insert_schema(F, Ks, Default, Schema);
augment_with_usage({_M, _F, K, _Data, _Default}, Schema) when is_atom(K) ->
    Schema;
augment_with_usage({M, F, K, Data, Default}, Schema) ->
    augment_with_usage({M, F, [K], Data, Default}, Schema).

maybe_insert_schema('get_first_defined', _Ks, _Default, Schema) ->
    Schema;
maybe_insert_schema('get_first_defined_keys', _Ks, _Default, Schema) ->
    Schema;
maybe_insert_schema(F, [K|Ks], Default, Schema) ->
    Section = kz_json:get_value([<<"properties">>, K], Schema, kz_json:new()),
    Updated = maybe_insert_schema(F, Ks, Default, Section),
    kz_json:insert_value(<<"type">>
                        ,<<"object">>
                        ,kz_json:set_value([<<"properties">>, K], Updated, Schema)
                        );
maybe_insert_schema(F, [], Default, Schema) ->
    Updates = props:filter_undefined(
                [{<<"type">>, guess_type(F, Default)}
                ,{<<"default">>, check_default(Default)}
                ,{<<"description">>, <<>>}
                ]
               ),
    kz_json:insert_values(Updates, Schema).

check_default({_M, _F, _A}) -> 'undefined';
check_default([_|_]=_L) ->
    ?DEBUG("unchanged default list ~p~n", [_L]),
    'undefined';
check_default([]) -> [];
check_default(?EMPTY_JSON_OBJECT=J) -> J;

check_default(<<"true">>) -> 'true';
check_default(<<"false">>) -> 'false';
check_default(B) when is_boolean(B) -> B;
check_default(I) when is_integer(I) -> I;
check_default(<<_/binary>>=B) -> B;
check_default(A) when is_atom(A) -> 'undefined';

check_default(Default) ->
    io:format("unchanged default ~p~n", [Default]),
    Default.

guess_type('get_value', <<_/binary>>) ->
    <<"string">>;
guess_type('get_value', []) ->
    <<"array">>;
guess_type('get_value', ?EMPTY_JSON_OBJECT) ->
    <<"object">>;
guess_type('get_value', I) when is_integer(I) ->
    <<"integer">>;
guess_type('get_value', F) when is_float(F) ->
    <<"float">>;
guess_type('get_value', 'undefined') ->
    'undefined';
guess_type('get_value', A) when is_atom(A) ->
    'undefined';
guess_type('get_value', B) when is_boolean(B) ->
    <<"boolean">>;
guess_type('get_binary_boolean', _) ->
    <<"boolean">>;
guess_type('get_binary_value', _) ->
    <<"string">>;
guess_type('get_ne_binary_value', _) ->
    <<"string">>;
guess_type('get_is_true', _) ->
    <<"boolean">>;
guess_type('is_true', _) ->
    <<"boolean">>;
guess_type('get_is_false', _) ->
    <<"boolean">>;
guess_type('is_false', _) ->
    <<"boolean">>;
guess_type('get_integer_value', _) ->
    <<"integer">>;
guess_type('get_float_value', _) ->
    <<"float">>;
guess_type('get_json_value', _) ->
    <<"object">>;
guess_type('find', _) ->
    'undefined';
guess_type('get_ne_value', <<_/binary>>) ->
    <<"string">>;
guess_type('get_ne_value', 'undefined') ->
    'undefined';
guess_type(_F, _D) ->
    ?DEBUG("couldn't guess ~p(~p)~n", [_F, _D]),
    'undefined'.

schema_path(Base) ->
    filename:join([code:priv_dir('crossbar')
                  ,<<"couchdb">>
                  ,<<"schemas">>
                  ,<<"callflows.", Base/binary, ".json">>
                  ]).

ensure_file_exists(Path) ->
    case filelib:is_regular(Path) of
        'false' -> create_schema(Path);
        'true' -> 'ok'
    end.

create_schema(Path) ->
    Skel = schema_path(<<"skel">>),
    ?DEBUG("copying ~s to ~s~n", [Skel, Path]),
    {'ok', _} = file:copy(Skel, Path),
    ?DEBUG("  copied skel into ~s~n", [Path]).

process() ->
    {'ok', Modules} = application:get_key('callflow', 'modules'),

    io:format("processing "),
    Usages = [{Module, Usages} ||
                 Module <- Modules,
                 (Usages = process(Module)) =/= 'undefined'
             ],
    io:format(" done~n~n~n"),
    Usages.

process(Module) when is_atom(Module) ->
    case is_action_module(Module) of
        'false' -> 'undefined';
        'true' ->
            io:format("."),
            U = process_action(Module),
            ?DEBUG("  usage for ~p: ~p~n", [Module, U]),
            U
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
process_expression(Acc, ?DYN_MOD_FUN_ARGS(_Module, _Function, _Args)) ->
    ?DEBUG("  skipping dyn module call~n", []),
    Acc;
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

process_expression(Acc, ?LC(Expr, Qualifiers)) ->
    process_expressions(process_expression(Acc, Expr)
                       ,Qualifiers
                       );
process_expression(Acc, ?LC_GENERATOR(Pattern, Expr)) ->
    process_expressions(Acc, [Pattern, Expr]);
process_expression(Acc, ?LC_BIN_GENERATOR(Pattern, Expr)) ->
    process_expressions(Acc, [Pattern, Expr]);

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

process_mfa(#usage{data_var_name=DataName}=Acc
           ,'kz_json', 'merge_recursive', [_Arg, ?VAR(DataName)]
           ) ->
    Acc;
process_mfa(#usage{data_var_name=DataName}=Acc
           ,'kz_json', 'merge_recursive', [?VAR(DataName), _Arg]
           ) ->
    Acc;
process_mfa(#usage{data_var_name=DataName}=Acc
           ,'kz_json', 'set_value', [_Key, _Value, ?VAR(DataName)]
           ) ->
    Acc;
process_mfa(#usage{data_var_name=DataName
                  ,usages=Usages
                  }=Acc
           ,'kz_json'=M, F, [Key, ?VAR(DataName)]
           ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, arg_to_key(Key), DataName, 'undefined'})};
process_mfa(#usage{data_var_name=DataName
                  ,usages=Usages
                  }=Acc
           ,'kz_json'=M, F, [Key, ?VAR(DataName), Default]
           ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, arg_to_key(Key), DataName, arg_to_key(Default)})};
process_mfa(#usage{data_var_name=DataName
                  ,data_var_aliases=Aliases
                  ,usages=Usages
                  }=Acc
           ,'kz_json'=M, 'find'=F, [Key, ?LIST(_Head, _Tail)=L, Default]
           ) ->
    case arg_list_has_data_var(DataName, Aliases, L) of
        'undefined' -> Acc;
        {DataName, _T} ->
            Acc#usage{usages=
                          maybe_add_usage(Usages, {M, F, arg_to_key(Key), DataName, arg_to_key(Default)})
                     };
        {Alias, _T} ->
            Acc#usage{usages=
                          maybe_add_usage(Usages, {M, F, arg_to_key(Key), Alias, arg_to_key(Default)})
                     }
    end;

process_mfa(#usage{data_var_name=DataName
                  ,data_var_aliases=Aliases
                  }=Acc
           ,M, F, As) ->
    case arg_list_has_data_var(DataName, Aliases, As) of
        {DataName, T} ->
            ?DEBUG("  found ~p in args of ~p:~p~n", [DataName, M, F]),
            Acc1 = process_mfa_call(Acc, M, F, As),
            process_args(Acc1, T);
        'undefined' ->
            ?DEBUG("  no ~p in arg list ~p, processing args directly~n", [DataName, As]),
            process_args(Acc, As);
        {Alias, T} ->
            ?DEBUG("  processing call with alias ~p: ~p:~p(~p)~n", [Alias, M, F, As]),
            Acc1 = process_mfa_call(Acc#usage{data_var_name=Alias}, M, F, As),
            process_args(Acc1, T)
    end.

process_args(Acc, As) ->
    lists:foldl(fun(Arg, UsageAcc) ->
                        process_expression(UsageAcc, Arg)
                end
               ,Acc
               ,As
               ).

arg_list_has_data_var(DataName, _Aliases, ?LIST(?VAR(DataName), Tail)) ->
    {DataName, Tail};
arg_list_has_data_var(_DataName, _Aliases, ?MOD_FUN_ARGS(_M, _F, _As)) ->
    'undefined';
arg_list_has_data_var(_DataName, _Aliases, ?FUN_ARGS(_F, _As)) ->
    'undefined';
arg_list_has_data_var(_DataName, _Aliases, ?EMPTY_LIST) ->
    'undefined';
arg_list_has_data_var(DataName, Aliases, ?LIST(?VAR(Name), Tail)) ->
    case lists:member(Name, Aliases) of
        'true' -> {Name, Tail};
        'false' -> arg_list_has_data_var(DataName, Aliases, Tail)
    end;
arg_list_has_data_var(DataName, Aliases, ?LIST(_Head, Tail)) ->
    arg_list_has_data_var(DataName, Aliases, Tail);

arg_list_has_data_var(DataName, _Aliases, [?VAR(DataName)|T]) ->
    {DataName, T};
arg_list_has_data_var(DataName, Aliases, [?VAR(Name)|T]) ->
    case lists:member(Name, Aliases) of
        'true' -> {Name, T};
        'false' -> arg_list_has_data_var(DataName, Aliases, T)
    end;
arg_list_has_data_var(_DataName, _Aliases, []) ->
    'undefined';
arg_list_has_data_var(DataName, Aliases, [?MOD_FUN_ARGS('kz_json'
                                                       ,'set_value'
                                                       ,Args
                                                       )
                                          | T
                                         ]) ->
    case arg_list_has_data_var(DataName, Aliases, Args) of
        {DataName, _} -> ?DEBUG("  sublist had ~p~n", [DataName]), {DataName, T};
        'undefined' -> arg_list_has_data_var(DataName, Aliases, T);
        {Alias, _} -> ?DEBUG("  sublist had alias ~p~n", [Alias]), {Alias, T}
    end;
arg_list_has_data_var(DataName, Aliases, [?MOD_FUN_ARGS(_M, _F, Args)|T]=As) ->
    case arg_list_has_data_var(DataName, Aliases, Args) of
        {DataName, _} -> ?DEBUG("  sub-fun had ~p~n", [DataName]), {DataName, As};
        'undefined' -> arg_list_has_data_var(DataName, Aliases, T);
        {Alias, _} -> ?DEBUG("  sub-fun had alias ~p~n", [Alias]), {Alias, As}
    end;
arg_list_has_data_var(DataName, Aliases, [?FUN_ARGS(_F, Args)|T]=As) ->
    case arg_list_has_data_var(DataName, Aliases, Args) of
        {DataName, _} -> ?DEBUG("  sub-fun had ~p~n", [DataName]), {DataName, As};
        'undefined' -> arg_list_has_data_var(DataName, Aliases, T);
        {Alias, _} -> ?DEBUG("  sub-fun had alias ~p~n", [Alias]), {Alias, As}
    end;
arg_list_has_data_var(DataName, Aliases, [?LIST(_H, _T)=H|T]=As) ->
    case arg_list_has_data_var(DataName, Aliases, H) of
        {DataName, _} -> ?DEBUG("  sub-list had ~p~n", [DataName]), {DataName, As};
        'undefined' -> arg_list_has_data_var(DataName, Aliases, T);
        {Alias, _} -> ?DEBUG("  sub-list had alias ~p~n", [Alias]), {Alias, As}
    end;

arg_list_has_data_var(DataName, Aliases, [_H|T]) ->
    ?DEBUG("  ignoring arg ~p~n", [_H]),
    arg_list_has_data_var(DataName, Aliases, T).

arg_to_key(?BINARY_MATCH(Arg)) ->
    binary_match_to_binary(Arg);
arg_to_key(?ATOM(Arg)) ->
    Arg;
arg_to_key(?MOD_FUN_ARGS('kz_json', 'new', [])) ->
    kz_json:new();
arg_to_key(?MOD_FUN_ARGS(M, F, As)) ->
    {M, F, length(As)};
arg_to_key(?VAR(Arg)) ->
    Arg;
arg_to_key(?INTEGER(I)) ->
    I;
arg_to_key(?EMPTY_LIST) ->
    [];
arg_to_key(?LIST(Head, Tail)) ->
    list_of_keys_to_binary(Head, Tail).

list_of_keys_to_binary(Head, Tail) ->
    list_of_keys_to_binary(Head, Tail, []).

list_of_keys_to_binary(Arg, ?EMPTY_LIST, Path) ->
    lists:reverse([arg_to_key(Arg) | Path]);
list_of_keys_to_binary(Arg, ?LIST(Head, Tail), Path) ->
    list_of_keys_to_binary(Head, Tail, [arg_to_key(Arg) | Path]).

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
    case have_visited(Acc, M, F, As) of
        'true' ->
            ?DEBUG("  already visited ~p:~p(~p)~n", [M, F, As]),
            Acc;
        'false' ->
            ?DEBUG("~n  calling ~p:~p(~p)~n", [M, F, As]),
            process_mfa_call(Acc, M, F, As, 'true')
    end.

have_visited(#usage{visited=Vs}, M, F, As) ->
    lists:member({M, F, As}, Vs).

process_mfa_call(#usage{data_var_name=DataName
                       ,usages=Usages
                       ,functions=Fs
                       ,current_module=_CM
                       ,visited=Vs
                       }=Acc
                ,M, F, As, ShouldAddAST) ->
    case mfa_clauses(Acc, M, F, length(As)) of
        [] when ShouldAddAST ->
            case module_ast(M) of
                'undefined' ->
                    ?DEBUG("  failed to find AST for ~p~n", [M]),
                    Acc#usage{visited=lists:usort([{M, F, As} | Vs])};
                {M, AST} ->
                    ?DEBUG("  added AST for ~p~n", [M]),
                    process_mfa_call(Acc#usage{functions=add_module_ast(Fs, M, AST)}
                                    ,M, F, As, 'false'
                                    )
            end;
        [] ->
            ?DEBUG("  no clauses for ~p:~p~n", [M, F]),
            Acc#usage{visited=lists:usort([{M, F, As} | Vs])};
        [Clauses] ->
            #usage{usages=ModuleUsages
                  ,functions=NewFs
                  ,visited=ModuleVisited
                  } =
                process_mfa_clauses(Acc#usage{current_module=M
                                             ,usages=[]
                                             ,data_var_aliases=[]
                                             ,visited=lists:usort([{M, F, As} | Vs])
                                             }
                                   ,Clauses
                                   ,data_index(DataName, As)
                                   ),
            ?DEBUG("  visited ~p:~p(~p)~n", [M, F, As]),
            Acc#usage{usages=lists:usort(ModuleUsages ++ Usages)
                     ,functions=NewFs
                     ,visited=ModuleVisited
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
process_mfa_clause(#usage{data_var_name=DataName}=Acc
                  ,?CLAUSE(Args, _Guards, Body)
                  ,DataIndex
                  ) ->
    ?DEBUG("  processing mfa clause for ~p(~p)~n", [DataName, DataIndex]),
    case lists:nth(DataIndex, Args) of
        ?VAR('_') -> Acc;
        ?EMPTY_LIST -> Acc;
        ?VAR(DataName) -> process_clause_body(Acc, Body);
        ?MOD_FUN_ARGS('kz_json', 'set_value', _Args) -> process_clause_body(Acc, Body);
        ?VAR(NewName) ->
            ?DEBUG("  data name changed from ~p to ~p~n", [DataName, NewName]),
            #usage{usages=ClauseUsages
                  ,functions=ClauseFs
                  ,visited=Vs
                  } = process_clause_body(Acc#usage{data_var_name=NewName}, Body),
            Acc#usage{usages=lists:usort(ClauseUsages)
                     ,functions=ClauseFs
                     ,visited=Vs
                     };
        ?ATOM('undefined') -> Acc;
        ?LIST(?VAR(NewName), _Tail) ->
            ?DEBUG("  data name changed from ~p to ~p~n", [DataName, NewName]),
            #usage{usages=ClauseUsages
                  ,functions=ClauseFs
                  ,visited=Vs
                  } = process_clause_body(Acc#usage{data_var_name=NewName}, Body),
            Acc#usage{usages=lists:usort(ClauseUsages)
                     ,functions=ClauseFs
                     ,visited=Vs
                     };
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
        'preloaded' -> 'undefined';
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
data_index(DataName, [?LIST(?VAR(DataName), _Tail)|_], Index) ->
    Index;
data_index(DataName, [?LIST(_Head, Tail)|As], Index) ->
    data_index(DataName, [Tail|As], Index);
data_index(DataName, [?EMPTY_LIST|As], Index) ->
    data_index(DataName, As, Index+1);
data_index(DataName, [?VAR(DataName)|_As], Index) -> Index;
data_index(DataName
          ,[?MOD_FUN_ARGS('kz_json', 'set_value'
                         ,Args
                         )
            | As
           ]
          ,Index
          ) ->
    case arg_list_has_data_var(DataName, [], Args) of
        {DataName, _} -> Index;
        'undefined' -> data_index(DataName, As, Index+1)
    end;
data_index(DataName, [_|As], Index) ->
    data_index(DataName, As, Index+1).

binary_match_to_binary(Match) ->
    iolist_to_binary(
      [binary_part_to_binary(BP) || BP <- Match]
     ).

binary_part_to_binary(?BINARY_STRING(V)) -> V;
binary_part_to_binary(?BINARY_VAR(N)) -> [${, N, $}].

-spec is_action_module(atom()) -> boolean().
is_action_module(Module) ->
    Attributes = Module:module_info('attributes'),
    Behaviours = props:get_value('behaviour', Attributes, []),
    lists:member('gen_cf_action', Behaviours).
