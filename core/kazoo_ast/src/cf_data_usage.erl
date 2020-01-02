%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc Module for parsing Callflow actions for `Data' usage.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_data_usage).

-export([process/0, process/1
        ,to_schema_docs/0, to_schema_doc/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_ast/src/kz_ast.hrl").

-record(usage, {usages = [] %% places the Data is accessed
               ,data_var_name = 'Data' %% Tracks current var name
               ,data_var_aliases = [] :: [any()] %% typically when kz_json:set_value is used
               ,current_module :: module() %% what module are we currently in
               ,functions = [] :: [tuple()] %% AST functions loaded
               ,visited = [] :: [tuple()] %% MFAs visited (to stop recursion)
               }).

-spec to_schema_docs() -> 'ok'.
to_schema_docs() ->
    _ = [to_schema_doc(M, Usage) || {M, Usage} <- process()],
    'ok'.

-spec to_schema_doc(module()) -> 'ok'.
to_schema_doc(M) ->
    to_schema_doc(M, process(M)).

to_schema_doc(M, Usage) ->
    <<"cf_", Base/binary>> = kz_term:to_binary(M),
    Schema = kz_ast_util:schema_path(<<"callflows.", Base/binary, ".json">>),
    _ = kz_ast_util:ensure_file_exists(Schema),
    update_schema(Base, Schema, Usage),
    update_doc(Base, Schema).

update_doc(Base, Schema) ->
    RefPath = filename:join([code:lib_dir('callflow'), "doc", "ref", <<Base/binary,".md">>]),
    Contents = build_ref_doc(Base, Schema),
    'ok' = file:write_file(RefPath, Contents).

build_ref_doc(Base, Schema) ->
    DocName = kz_ast_util:smash_snake(Base),
    ["## ", DocName, "\n\n"
    ,"### About ", DocName, "\n\n"
    ,kz_ast_util:schema_to_table(Schema)
    ].

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
            ?LOG_DEBUG("updating _id from ~p to ~p~n", [_Id, ID]),
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
maybe_insert_schema(_F, ['undefined' | _Keys], _Default, Schema) ->
    ?LOG_DEBUG("skipping function ~p with key undefined (~p left)", [_F, _Keys]),
    Schema;
maybe_insert_schema(F, [K|Ks], Default, Schema) ->
    Section = kz_json:get_json_value([<<"properties">>, K], Schema, kz_json:new()),
    Updated = maybe_insert_schema(F, Ks, Default, Section),
    kz_json:insert_value(<<"type">>
                        ,<<"object">>
                        ,kz_json:set_value([<<"properties">>, K], Updated, Schema)
                        );
maybe_insert_schema(F, [], Default, Schema) ->
    Updates = props:filter_undefined(
                [{<<"default">>, check_default(Default)}
                ,{<<"description">>, <<>>}
                ]),
    kz_json:insert_values(Updates, insert_type(guess_type(F, Default), Schema)).

insert_type(Types, Schema) ->
    OneOf = kz_json:get_first_defined([<<"oneOf">>, <<"anyOf">>, <<"allOf">>], Schema),
    insert_type(Types, Schema, OneOf).

insert_type({Type, ItemsType}, Schema, 'undefined') ->
    kz_json:insert_values([{<<"items">>, kz_json:from_list([{<<"type">>, ItemsType}])}
                          ,{<<"type">>, Type}
                          ], Schema);
insert_type(Type, Schema, 'undefined' = _OneOf) ->
    kz_json:insert_values([{<<"type">>, Type}], Schema);
insert_type(Type, Schema, [_|_]=Of) ->
    case lists:any(fun(Elem) -> kz_json:get_value(<<"type">>, Elem) =/= 'undefined' end, Of) of
        'true' ->
            case kz_json:get_value(<<"type">>, Schema) of
                'undefined' -> 'ok';
                _ -> io:format("schema can not define type both in (anyOf | allOf | oneOf) and root:~n~p~n", [Schema])
            end,
            kz_json:delete_key(<<"type">>, Schema);
        'false' ->
            insert_type(Type, Schema, 'undefined')
    end.

check_default({_M, _F, _A}) -> 'undefined';
check_default([<<_/binary>>|_]=L) ->
    L;
check_default([_|_]=_L) ->
    ?LOG_DEBUG("default list ~p~n", [_L]),
    'undefined';
check_default([]) -> [];
check_default(?EMPTY_JSON_OBJECT=J) -> J;

check_default(<<"true">>) -> 'true';
check_default(<<"false">>) -> 'false';
check_default(B) when is_boolean(B) -> B;
check_default(I) when is_integer(I) -> I;
check_default(B) when is_binary(B) -> B;
check_default(A) when is_atom(A) -> 'undefined';

check_default(Default) ->
    ?LOG_DEBUG("unchanged default ~p~n", [Default]),
    Default.

guess_type('get_value', <<_/binary>>) ->
    <<"string">>;
guess_type('get_value', []) ->
    {<<"array">>, 'undefined'};
guess_type('get_ne_value', []) ->
    {<<"array">>, 'undefined'};
guess_type('get_value', ?EMPTY_JSON_OBJECT) ->
    <<"object">>;
guess_type('get_value', I) when is_integer(I) ->
    <<"integer">>;
guess_type('get_value', F) when is_float(F) ->
    <<"number">>;
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
guess_type('get_atom_value', _) ->
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
    <<"number">>;
guess_type('get_json_value', _) ->
    <<"object">>;
guess_type('get_list_value', [<<_/binary>>|_]) ->
    {<<"array">>, <<"string">>};
guess_type('get_list_value', _L) ->
    {<<"array">>, 'undefined'};
guess_type('find', _) ->
    'undefined';
guess_type('get_ne_value', <<_/binary>>) ->
    <<"string">>;
guess_type('get_ne_value', 'undefined') ->
    'undefined';
guess_type(_F, _D) ->
    ?LOG_DEBUG("couldn't guess ~p(~p)~n", [_F, _D]),
    'undefined'.

-spec process() -> [{module(), list()}].
process() ->
    io:format("processing callflow data usage: "),
    Usages = [{Module, Usages} ||
                 Module <- kz_ast_util:app_modules('callflow'),
                 (Usages = process(Module)) =/= 'undefined'
             ],
    io:format(" done~n"),
    Usages.

-spec process(module()) -> list().
process(Module) when is_atom(Module) ->
    case is_action_module(Module) of
        'false' -> 'undefined';
        'true' ->
            io:format("."),
            U = process_action(Module),
            ?LOG_DEBUG("  usage for ~p: ~p~n", [Module, U]),
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
    ?LOG_DEBUG("process match ~p = ~p~n", [Left, Right]),
    process_match(Acc, Left, Right);
process_expression(#usage{current_module=Module}=Acc, ?FUN_ARGS(Function, Args)) ->
    process_mfa(Acc, Module, Function, Args);
process_expression(Acc, ?DYN_FUN_ARGS(_Function, Args)) ->
    process_expressions(Acc, Args);
process_expression(Acc, ?MOD_FUN_ARGS(Module, Function, Args)) ->
    process_mfa(Acc, Module, Function, Args);
process_expression(Acc, ?DYN_MOD_FUN_ARGS(_Module, _Function, _Args)) ->
    ?LOG_DEBUG("  skipping dyn module call~n", []),
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
    process_expressions(process_expressions(process_expressions(Acc, Exprs)
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
    ?LOG_DEBUG("~nskipping expression in ~p: ~p~n", [_M, _Expression]),
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
    ?LOG_DEBUG("adding alias ~p~n", [VarName]),
    Acc#usage{data_var_aliases=[VarName|Aliases]};
process_match_mfa(#usage{data_var_name=DataName
                        ,data_var_aliases=Aliases
                        }=Acc
                 ,VarName
                 ,'kz_json', 'set_values', [?VAR(_), ?VAR(DataName)]
                 ) ->
    ?LOG_DEBUG("adding alias ~p~n", [VarName]),
    Acc#usage{data_var_aliases=[VarName|Aliases]};
process_match_mfa(Acc, _VarName, M, F, As) ->
    process_mfa(Acc, M, F, As).

process_mfa(#usage{data_var_name=DataName
                  ,usages=Usages
                  }=Acc
           ,'kz_doc', 'id', [?VAR(DataName)]
           ) ->
    Acc#usage{usages=maybe_add_usage(Usages, {'kz_json', 'get_ne_binary_value', <<"id">>, DataName, 'undefined'})};
process_mfa(#usage{data_var_name=DataName}=Acc
           ,'kz_json', 'merge_recursive', [_Arg, ?VAR(DataName)]
           ) ->
    Acc;
process_mfa(#usage{data_var_name=DataName}=Acc
           ,'kz_json', 'merge_recursive', [?VAR(DataName), _Arg]
           ) ->
    Acc;
process_mfa(#usage{data_var_name=DataName
                  ,usages=Usages
                  }=Acc
           ,'kz_json'=M, 'set_value'=F, [Key, Value, ?VAR(DataName)]
           ) ->
    ?LOG_DEBUG("adding set_value usage ~p, ~p, ~p~n", [Key, Value, DataName]),
    Acc#usage{usages=maybe_add_usage(Usages, {M, F, arg_to_key(Key), DataName, arg_to_key(Value)})};
process_mfa(#usage{data_var_name=DataName}=Acc
           ,'kz_json'=_M, 'set_values'=_F, [?VAR(_KVs), ?VAR(DataName)]
           ) ->
    Acc;
process_mfa(#usage{data_var_name=DataName}=Acc
           ,'kz_json'=M, 'set_values'=_F, [KVs, ?VAR(DataName)=DN]
           ) ->
    ?LOG_DEBUG("adding set_values usage ~p: ~p~n", [KVs, DataName]),
    lists:foldl(fun({Key, Value}, Usage) ->
                        process_mfa(Usage, M, 'set_value', [Key, Value, DN])
                end
               ,Acc
               ,KVs
               );
process_mfa(#usage{}=Acc
           ,'kz_json', _F, [{'call', _, _, _}=_Key|_]
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
    ?LOG_DEBUG("looking in arg list for ~p or ~p~n", [DataName, Aliases]),
    case arg_list_has_data_var(DataName, Aliases, As) of
        {DataName, T} ->
            ?LOG_DEBUG("  found ~p in args of ~p:~p~n", [DataName, M, F]),
            Acc1 = process_mfa_call(Acc, M, F, As),
            process_args(Acc1, T);
        'undefined' ->
            ?LOG_DEBUG("  no ~p in arg list ~p, processing args directly~n", [DataName, As]),
            process_args(Acc, As);
        {Alias, T} ->
            ?LOG_DEBUG("  processing call with alias ~p: ~p:~p(~p)~n", [Alias, M, F, As]),
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
    ?LOG_DEBUG("  ignoring mfa ~p:~p/~p~n", [_M, _F, length(_As)]),
    'undefined';
arg_list_has_data_var(_DataName, _Aliases, ?FUN_ARGS(_F, _As)) ->
    ?LOG_DEBUG("  ignoring fa ~p/~p~n", [_F, length(_As)]),
    'undefined';
arg_list_has_data_var(_DataName, _Aliases, ?EMPTY_LIST) ->
    ?LOG_DEBUG("  reached end of arg list~n", []),
    'undefined';
arg_list_has_data_var(DataName, Aliases, ?LIST(?VAR(Name), Tail)) ->
    case lists:member(Name, Aliases) of
        'true' ->
            ?LOG_DEBUG("  name ~s is an alias of ~s~n", [Name, DataName]),
            {Name, Tail};
        'false' ->
            ?LOG_DEBUG("  failed to find ~s in aliases ~p~n", [Name, Aliases]),
            arg_list_has_data_var(DataName, Aliases, Tail)
    end;
arg_list_has_data_var(DataName, Aliases, ?LIST(_Head, Tail)) ->
    ?LOG_DEBUG("  skipping arg ~p~n", [_Head]),
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
        {DataName, _} -> ?LOG_DEBUG("  sublist had ~p~n", [DataName]), {DataName, T};
        'undefined' -> arg_list_has_data_var(DataName, Aliases, T);
        {Alias, _} -> ?LOG_DEBUG("  sublist had alias ~p~n", [Alias]), {Alias, T}
    end;
arg_list_has_data_var(DataName, Aliases, [?MOD_FUN_ARGS(_M, _F, Args)|T]=As) ->
    case arg_list_has_data_var(DataName, Aliases, Args) of
        {DataName, _} -> ?LOG_DEBUG("  sub-fun had ~p~n", [DataName]), {DataName, As};
        'undefined' -> arg_list_has_data_var(DataName, Aliases, T);
        {Alias, _} -> ?LOG_DEBUG("  sub-fun had alias ~p~n", [Alias]), {Alias, As}
    end;
arg_list_has_data_var(DataName, Aliases, [?FUN_ARGS(_F, Args)|T]=As) ->
    case arg_list_has_data_var(DataName, Aliases, Args) of
        {DataName, _} -> ?LOG_DEBUG("  sub-fun had ~p~n", [DataName]), {DataName, As};
        'undefined' -> arg_list_has_data_var(DataName, Aliases, T);
        {Alias, _} -> ?LOG_DEBUG("  sub-fun had alias ~p~n", [Alias]), {Alias, As}
    end;
arg_list_has_data_var(DataName, Aliases, [?LIST(_H, _T)=H|T]=As) ->
    case arg_list_has_data_var(DataName, Aliases, H) of
        {DataName, _} -> ?LOG_DEBUG("  sub-list had ~p~n", [DataName]), {DataName, As};
        'undefined' -> arg_list_has_data_var(DataName, Aliases, T);
        {Alias, _} -> ?LOG_DEBUG("  sub-list had alias ~p~n", [Alias]), {Alias, As}
    end;

arg_list_has_data_var(DataName, Aliases, [_H|T]) ->
    ?LOG_DEBUG("  arg not data-name ~p: ~p~n", [DataName, _H]),
    arg_list_has_data_var(DataName, Aliases, T).

arg_to_key(?BINARY_MATCH(Arg)) ->
    try kz_ast_util:binary_match_to_binary(Arg)
    catch 'error':'function_clause' -> 'undefined'
    end;
arg_to_key(?ATOM(Arg)) -> Arg;
arg_to_key(?MOD_FUN_ARGS('kz_json', 'new', [])) ->
    kz_json:new();
arg_to_key(?MOD_FUN_ARGS(M, F, As)) ->
    {M, F, length(As)};
arg_to_key(?FUN_ARGS(_F, _As)) -> 'undefined';
arg_to_key(?VAR(_Arg)) -> 'undefined';
arg_to_key(?INTEGER(I)) -> I;
arg_to_key(?FLOAT(F)) -> F;
arg_to_key(?EMPTY_LIST) -> [];
arg_to_key(?LIST(Head, Tail)) ->
    list_of_keys_to_binary(Head, Tail).

list_of_keys_to_binary(Head, Tail) ->
    list_of_keys_to_binary(Head, Tail, []).

list_of_keys_to_binary(Arg, ?EMPTY_LIST, Path) ->
    lists:reverse([arg_to_key(Arg) | Path]);
list_of_keys_to_binary(Arg, ?LIST(Head, Tail), Path) ->
    list_of_keys_to_binary(Head, Tail, [arg_to_key(Arg) | Path]).


maybe_add_usage(Usages, {'kz_json',_Function,<<"source">>,_DataVar, _Default}) ->
    Usages;
maybe_add_usage(Usages, Call) ->
    case lists:member(Call, Usages) of
        'true' -> Usages;
        'false' ->
            ?LOG_DEBUG("adding usage: ~p~n", [Call]),
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
            ?LOG_DEBUG("  already visited ~p:~p(~p)~n", [M, F, As]),
            Acc;
        'false' ->
            ?LOG_DEBUG("~n  calling ~p:~p(~p)~n", [M, F, As]),
            process_mfa_call(Acc, M, F, As, 'true')
    end.

have_visited(#usage{visited=Vs}, M, F, As) ->
    lists:member({M, F, As}, Vs).

process_mfa_call(#usage{functions=Fs
                       ,current_module=_CM
                       ,visited=Vs
                       }=Acc
                ,M, F, As, ShouldAddAST) ->
    case mfa_clauses(Acc, M, F, length(As)) of
        [] when ShouldAddAST ->
            case kz_ast_util:module_ast(M) of
                'undefined' ->
                    ?LOG_DEBUG("  failed to find AST for ~p~n", [M]),
                    Acc#usage{visited=lists:usort([{M, F, As} | Vs])};
                {M, AST} ->
                    ?LOG_DEBUG("  added AST for ~p~n", [M]),
                    #module_ast{functions=NewFs}
                        = kz_ast_util:add_module_ast(#module_ast{functions=Fs}, M, AST),
                    process_mfa_call(Acc#usage{functions=NewFs}
                                    ,M, F, As, 'false'
                                    )
            end;
        [] ->
            ?LOG_DEBUG("  no clauses for ~p:~p~n", [M, F]),
            Acc#usage{visited=lists:usort([{M, F, As} | Vs])};
        [Clauses] when F =:= 'evaluate_rules_for_creation';
                       F =:= 'create_endpoints' ->
            process_mfa_clauses_kz_endpoint(Acc, M, F, As, Clauses);
        [_Clauses] when M =:= 'kzc_recordings_sup',
                        F =:= 'start_recording' ->
            ?LOG_DEBUG("checking kzc_recording:init/1~n", []),
            process_mfa_call(Acc, 'kzc_recording', 'init', [?VAR('_', 'Call'), ?VAR('_', 'Data')]);
        [Clauses] ->
            process_mfa_clauses(Acc, M, F, As, Clauses)
    end.

process_mfa_clauses(#usage{visited=Vs
                          ,data_var_name=DataName
                          ,usages=Usages
                          ,data_var_aliases=Aliases
                          }=Acc
                   ,M, F, As, Clauses
                   ) ->
    #usage{usages=ModuleUsages
          ,functions=NewFs
          ,visited=ModuleVisited
          ,data_var_name=FuntionDataVarName
          ,data_var_aliases=FunctionAliases
          } =
        process_mfa_clauses(Acc#usage{current_module=M
                                     ,usages=[]
                                     ,data_var_aliases=[]
                                     ,visited=lists:usort([{M, F, As} | Vs])
                                     }
                           ,Clauses
                           ,data_index(DataName, As)
                           ),
    ?LOG_DEBUG("  visited ~p:~p(~p)~n", [M, F, As]),
    Acc#usage{usages=lists:usort(ModuleUsages ++ Usages)
             ,functions=NewFs
             ,visited=ModuleVisited
             ,data_var_aliases=lists:usort([FuntionDataVarName | Aliases ++ FunctionAliases])
             }.

process_mfa_clauses_kz_endpoint(#usage{visited=Vs}=Acc
                               ,M, 'evaluate_rules_for_creation'=F, As
                               ,[?CLAUSE(_Args, _Guards, Expressions)]
                               ) ->
    [?MATCH(?VAR('Routines')
           ,?LIST(_, _)=FunExpressions
           )
    ,?MOD_FUN_ARGS('lists'
                  ,'foldl'
                  ,[?FA(_FoldFun, 2), ?TUPLE(FunArgs), ?VAR('Routines')]
                  )
    ] = Expressions,

    ?LOG_DEBUG("  visiting funs in ~p:~p(~p)~n", [M, F, As]),
    process_mfa_clauses_kz_endpoint_folds(Acc#usage{visited=lists:usort([{M, F, As} | Vs])}
                                         ,M, list_of_fun_expressions_to_f(FunExpressions), FunArgs
                                         );
process_mfa_clauses_kz_endpoint(#usage{visited=Vs}=Acc
                               ,M, 'create_endpoints'=F, As
                               ,[?CLAUSE(_Args, _Guards, Expressions)]
                               ) ->
    ?MATCH(?VAR('Routines')
          ,?LIST(_, _)=FunExpressions
          ) = hd(Expressions),

    ?LOG_DEBUG("  visiting funs in ~p:~p(~p)~n", [M, F, As]),
    process_mfa_clauses_kz_endpoint_folds(Acc#usage{visited=lists:usort([{M, F, As} | Vs])}
                                         ,M, list_of_fun_expressions_to_f(FunExpressions), As
                                         ).

process_mfa_clauses_kz_endpoint_folds(#usage{usages=Usages}=Acc, M, Funs, FunArgs) ->
    ForFsAcc = Acc#usage{current_module=M
                        ,usages=[]
                        ,data_var_aliases=[]
                        },

    #usage{usages=ModuleUsages
          ,functions=NewFs
          ,visited=ModuleVisited
          } =
        lists:foldl(fun(LocalFun, MyAcc) ->
                            ?LOG_DEBUG("  checking for usage in ~p:~p(~p)~n", [M, LocalFun, FunArgs]),
                            process_mfa_call(MyAcc, M, LocalFun, FunArgs)
                    end
                   ,ForFsAcc
                   ,Funs
                   ),

    ?LOG_DEBUG("  new usages: ~p~n", [ModuleUsages]),
    Acc#usage{usages=lists:usort(ModuleUsages ++ Usages)
             ,functions=NewFs
             ,visited=ModuleVisited
             }.


list_of_fun_expressions_to_f(ListExpression) ->
    list_of_fun_expressions_to_f(ListExpression, []).
list_of_fun_expressions_to_f(?EMPTY_LIST, Acc) ->
    lists:reverse(Acc);
list_of_fun_expressions_to_f(?LIST(?FA(Function, _Arity), Tail), Acc) ->
    list_of_fun_expressions_to_f(Tail, [Function | Acc]).

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
    ?LOG_DEBUG("  guessing index for ~p from ~p~n", [DataName, Args]),
    DataIndex = data_index(DataName, Args),
    ?LOG_DEBUG("  guessed data index of ~p as ~p~n", [DataName, DataIndex]),
    process_mfa_clause(Acc, Clause, DataIndex);
process_mfa_clause(Acc, _Clause, 'undefined') ->
    Acc;
process_mfa_clause(#usage{data_var_name=DataName
                         ,data_var_aliases=Aliases
                         }=Acc
                  ,?CLAUSE(Args, _Guards, Body)
                  ,DataIndex
                  ) ->
    ?LOG_DEBUG("  processing mfa clause for ~p(~p)~n", [DataName, DataIndex]),
    case lists:nth(DataIndex, Args) of
        ?VAR('_') -> Acc;
        ?EMPTY_LIST -> Acc;
        ?VAR(DataName) -> process_clause_body(Acc, Body);
        ?MOD_FUN_ARGS('kz_json', 'set_value', _Args)=_ClauseArgs ->
            ?LOG_DEBUG("skipping set_value on ~p(~p)~n", [_Args, element(2, _ClauseArgs)]),
            process_clause_body(Acc, Body);
        ?MOD_FUN_ARGS('kz_json', 'set_values', _Args)=_ClauseArgs ->
            ?LOG_DEBUG("skipping set_values on ~p(~p)~n", [_Args, element(2, _ClauseArgs)]),
            process_clause_body(Acc, Body);
        ?VAR(NewName) ->
            ?LOG_DEBUG("  data name changed from ~p to ~p~n", [DataName, NewName]),
            #usage{usages=ClauseUsages
                  ,functions=ClauseFs
                  ,visited=Vs
                  } = process_clause_body(Acc#usage{data_var_name=NewName}, Body),
            Acc#usage{usages=lists:usort(ClauseUsages)
                     ,functions=ClauseFs
                     ,visited=Vs
                     ,data_var_aliases=lists:usort([NewName | Aliases])
                     };
        ?ATOM('undefined') -> Acc;
        ?LIST(?VAR(NewName), _Tail) ->
            ?LOG_DEBUG("  data name changed from ~p to ~p~n", [DataName, NewName]),
            #usage{usages=ClauseUsages
                  ,functions=ClauseFs
                  ,visited=Vs
                  } = process_clause_body(Acc#usage{data_var_name=NewName}, Body),
            Acc#usage{usages=lists:usort(ClauseUsages)
                     ,functions=ClauseFs
                     ,visited=Vs
                     ,data_var_aliases=lists:usort([NewName | Aliases])
                     };
        _Unexpected ->
            ?LOG_DEBUG("unexpected arg(~p) at ~p in ~p, expected ~p~n"
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

-spec is_action_module(atom()) -> boolean().
is_action_module(Module) ->
    Attributes = Module:module_info('attributes'),
    Behaviours = props:get_value('behaviour', Attributes, []),
    lists:member('gen_cf_action', Behaviours).
