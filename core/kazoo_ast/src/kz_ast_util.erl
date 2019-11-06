%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc Provide some utilities to work with AST.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_ast_util).

-export([module_ast/1
        ,add_module_ast/3

        ,ast_to_list_of_binaries/1
        ,ast_list_to_list/1
        ,binary_match_to_binary/1
        ,smash_snake/1, smash_snake/2

        ,default_schema_priv_dir/0
        ,schema_path/1, schema_path/2
        ,api_path/1
        ,ensure_file_exists/1
        ,create_schema/1
        ,schema_to_table/1
        ,load_ref_schema/1

        ,project_apps/0, app_modules/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_amqp/src/api/kapi_dialplan.hrl").
-include_lib("kazoo_amqp/src/api/kapi_call.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-type ast() :: [erl_parse:abstract_form()].
-type abstract_code() :: {'raw_abstract_v1', ast()}.

-export_type([abstract_code/0
             ,ast/0
             ]).

-define(SCHEMA_SECTION, <<"#### Schema\n\n">>).
-define(SUB_SCHEMA_SECTION_HEADER, <<"#####">>).

-spec module_ast(atom()) -> {atom(), abstract_code()} | 'undefined'.
module_ast(M) ->
    case code:which(M) of
        'non_existing' -> 'undefined';
        'preloaded' -> 'undefined';
        Beam ->
            beam_ast(Beam)
    end.

beam_ast(Beam) ->
    case beam_lib:chunks(Beam, ['abstract_code']) of
        {'ok', {Module, [{'abstract_code', AST}]}} ->
            {Module, AST};
        {'error', 'beam_lib', {'unknown_chunk', File, _}} ->
            lager:info("unknown chunk in ~s, no ast", [File]),
            'undefined';
        {'error', 'beam_lib', {'key_missing_or_invalid', File, _Key}} ->
            lager:info("key ~s missing or invalid in ~s", [_Key, File]),
            'undefined';
        {'error', 'beam_lib', {'file_error', File, Posix}} ->
            lager:info("file error ~p on ~s", [Posix, File]),
            'undefined';
        {'error', 'beam_lib', Error} ->
            lager:info("error getting chunks for ~s: ~p", [Error]),
            'undefined'
    end.

-spec add_module_ast(module_ast(), module(), abstract_code()) -> module_ast().
add_module_ast(ModAST, Module, {'raw_abstract_v1', Attributes}) ->
    F = fun(A, Acc) -> add_module_ast_fold(A, Module, Acc) end,
    lists:foldl(F, ModAST, Attributes).

-spec add_module_ast_fold(ast(), module(), module_ast()) -> module_ast().
add_module_ast_fold(?AST_FUNCTION(F, Arity, Clauses), Module, #module_ast{functions=Fs}=Acc) ->
    Acc#module_ast{functions=[{Module, F, Arity, Clauses}|Fs]};
add_module_ast_fold(?AST_RECORD(Name, Fields), _Module, #module_ast{records=Rs}=Acc) ->
    Acc#module_ast{records=[{Name, Fields}|Rs]};
add_module_ast_fold(?AST_EXPORTS(Exports), _Module, #module_ast{exports=Es}=Acc) ->
    Acc#module_ast{exports=Exports++Es};
add_module_ast_fold(?AST_EXPORTED_TYPES(Types), _Module, #module_ast{exported_types=Ts}=Acc) ->
    Acc#module_ast{exported_types=Types++Ts};
add_module_ast_fold(?SPEC(Fun, Arity, Args, Return), _Module, #module_ast{specs=Specs}=Acc) ->
    Acc#module_ast{specs=[{Fun, Arity, Args, Return} | Specs]};
add_module_ast_fold(?TYPE(Name, TypeDef), _Module, #module_ast{types=Ts}=Acc) ->
    Acc#module_ast{types=[{Name, TypeDef} | Ts]};
add_module_ast_fold(?AST_ATTRIBUTE_FILE(_Path), _Module, Acc) -> Acc;
add_module_ast_fold(?EOF, _Module, Acc) -> Acc;
add_module_ast_fold(?LAGER_RECORDS, _Module, Acc) -> Acc;
add_module_ast_fold(?AST_ATTRIBUTE_MODULE(Module), Module, Acc) -> Acc;
add_module_ast_fold(?BEHAVIOUR(Behaviour), _Module, Acc) -> Acc#module_ast{behaviour=Behaviour};
add_module_ast_fold(_Other, _Module, Acc) ->
    Acc.

-spec ast_to_list_of_binaries(erl_parse:abstract_expr()) -> kz_term:ne_binaries().
ast_to_list_of_binaries(ASTList) ->
    ast_to_list_of_binaries(ASTList, []).

ast_to_list_of_binaries(?APPEND(First, Second), Binaries) ->
    ast_to_list_of_binaries(Second, ast_to_list_of_binaries(First, Binaries));
ast_to_list_of_binaries(?SUBTRACT(First, Second), Binaries) ->
    ast_to_list_of_binaries(First, Binaries) -- ast_to_list_of_binaries(Second, []);
ast_to_list_of_binaries(?EMPTY_LIST, Binaries) ->
    lists:reverse(Binaries);
ast_to_list_of_binaries(?MOD_FUN_ARGS('kapi_dialplan', 'optional_bridge_req_headers', []), Binaries) ->
    ?OPTIONAL_BRIDGE_REQ_HEADERS ++ Binaries;
ast_to_list_of_binaries(?MOD_FUN_ARGS('kapi_dialplan', 'optional_bridge_req_endpoint_headers', []), Binaries) ->
    ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS ++ Binaries;
ast_to_list_of_binaries(?MOD_FUN_ARGS('kapi_call', 'optional_call_event_headers', []), Binaries) ->
    ?OPTIONAL_CALL_EVENT_HEADERS ++ Binaries;
ast_to_list_of_binaries(?LIST(?LIST(_, _)=H, T), Binaries) ->
    ast_to_list_of_binaries(T, [ast_to_list_of_binaries(H) | Binaries]);
ast_to_list_of_binaries(?LIST(H, T), Binaries) ->
    ast_to_list_of_binaries(T, [binary_match_to_binary(H) | Binaries]);
ast_to_list_of_binaries(?VAR(_), Binaries) ->
    Binaries.

-spec binary_match_to_binary(erl_parse:abstract_expr()) -> binary().
binary_match_to_binary(?ATOM(A)) -> kz_term:to_binary(A);
binary_match_to_binary(?BINARY_STRING(V)) ->
    kz_term:to_binary(V);
binary_match_to_binary(?BINARY_MATCH(Match)) ->
    binary_match_to_binary(Match);
binary_match_to_binary(?FUN_ARGS('atom_to_binary', [?ATOM(Atom), ?ATOM('utf8')])) ->
    atom_to_binary(Atom, 'utf8');
binary_match_to_binary(Match) when is_list(Match) ->
    iolist_to_binary(
      [binary_part_to_binary(BP) || BP <- Match]
     ).

binary_part_to_binary(?BINARY_STRING(V)) -> V;
binary_part_to_binary(?SUB_BINARY(V)) -> V;
binary_part_to_binary(?BINARY_MATCH(Ms)) -> binary_match_to_binary(Ms);
binary_part_to_binary(?BINARY_FROM_ATOM(Atom)) -> atom_to_binary(Atom, 'utf8').

-spec ast_list_to_list(term()) -> list().
ast_list_to_list(?EMPTY_LIST) -> [];
ast_list_to_list(?LIST(_, _)=ASTList) ->
    ast_list_to_list(ASTList, []).

ast_list_to_list(?EMPTY_LIST, List) -> lists:reverse(List);
ast_list_to_list(?LIST(H, T), List) ->
    ast_list_to_list(T, [ast_list_el_to_el(H) | List]).

ast_list_el_to_el(?TUPLE(Fields)) ->
    list_to_tuple(Fields).

%% user_auth -> User Auth

-spec smash_snake(kz_term:ne_binary()) -> iolist().
smash_snake(BaseName) ->
    smash_snake(BaseName, <<" ">>).

-spec smash_snake(kz_term:ne_binary(), binary()) -> iolist().
smash_snake(BaseName, Glue) ->
    case binary:split(BaseName, <<"_">>, ['global']) of
        [Part] -> format_name_part(Part);
        [H|Parts] ->
            [format_name_part(H)
             | [[Glue, format_name_part(Part)] || Part <- Parts]
            ]
    end.

-spec format_name_part(kz_term:ne_binary()) -> kz_term:ne_binary().
format_name_part(<<"api">>) -> <<"API">>;
format_name_part(<<"ip">>) -> <<"IP">>;
format_name_part(<<"auth">>) -> <<"Authentication">>;
format_name_part(Part) ->
    kz_binary:ucfirst(Part).

-spec default_schema_priv_dir() -> file:filename_all().
default_schema_priv_dir() ->
    kz_term:to_binary(code:priv_dir('crossbar')).

-spec schema_path(binary()) -> file:filename_all().
schema_path(Base) ->
    schema_path(Base, default_schema_priv_dir()).

-spec schema_path(binary(), file:filename_all()) -> file:filename_all().
schema_path(Base, PrivDir) ->
    case filename:join([PrivDir
                       ,<<"couchdb">>
                       ,<<"schemas">>
                       ,Base
                       ]) of
        <<"/", _/binary>> = Path -> Path;
        Path -> <<"./", Path/binary>>
    end.

-spec api_path(binary()) -> file:filename_all().
api_path(Base) ->
    filename:join([code:priv_dir('crossbar')
                  ,<<"api">>
                  ,Base
                  ]).

-spec ensure_file_exists(binary()) -> 'ok' | {'ok', any()}.
ensure_file_exists(Path) ->
    case filelib:is_regular(Path) of
        'false' -> create_schema(Path);
        'true' -> 'ok'
    end.

-spec create_schema(binary()) -> {'ok', any()}.
create_schema(Path) ->
    Skel = schema_path(<<"skel.json">>),
    {'ok', _} = file:copy(Skel, Path).

-spec project_apps() -> [atom()].
project_apps() ->
    Core = siblings_of('kazoo'),
    Apps = siblings_of('sysconf'),
    Core ++ Apps.

siblings_of(App) ->
    [dir_to_app_name(Dir)
     || Dir <- filelib:wildcard(filename:join([code:lib_dir(App), "..", "*"])),
        filelib:is_dir(Dir),
        ".git" =/= filename:basename(Dir),
        ".erlang.mk" =/= filename:basename(Dir)
    ].

dir_to_app_name(Dir) ->
    kz_term:to_atom(filename:basename(Dir), 'true').

-spec app_modules(atom()) -> [atom()].
app_modules(App) ->
    case application:get_key(App, 'modules') of
        {'ok', Modules} -> Modules;
        'undefined' ->
            case application:load(App) of
                'ok' -> app_modules(App);
                _ -> []
            end
    end.

-define(TABLE_ROW(Key, Description, Type, Default, Required, Supported)
       ,[kz_binary:join([Key, Description, Type, Default, Required, Supported]
                       ,<<" | ">>
                       )
        ,$\n
        ]
       ).
-define(TABLE_HEADER
       ,[?TABLE_ROW(<<"Key">>, <<"Description">>, <<"Type">>, <<"Default">>, <<"Required">>, <<"Support Level">>)
        ,?TABLE_ROW(<<"---">>, <<"-----------">>, <<"----">>, <<"-------">>, <<"--------">>, <<"-------------">>)
        ]).

-spec schema_to_table(kz_term:ne_binary() | kz_json:object()) -> iolist().
schema_to_table(<<"#/definitions/", _/binary>>=_S) -> [];
schema_to_table(Schema=?NE_BINARY) ->
    case kz_json_schema:fload(Schema) of
        {'ok', JObj} -> schema_to_table(JObj);
        {'error', 'not_found'} ->
            io:format("failed to find ~s~n", [Schema]),
            throw({'error', 'no_schema'})
    end;
schema_to_table(SchemaJObj) ->
    try schema_to_table(SchemaJObj, []) of
        [Table|RefTables] ->
            [?SCHEMA_SECTION, Table, "\n\n"
            ,cb_api_endpoints:ref_tables_to_doc(RefTables), "\n\n"
            ]
    catch
        ?STACKTRACE('throw', 'no_type', ST)
        io:format("failed to build table from schema ~s~n", [kz_doc:id(SchemaJObj)]),
        io:format("~p~n", [ST]),
        throw('no_type')
        end.

schema_to_table(SchemaJObj, BaseRefs) ->
    Description = kz_json:get_binary_value(<<"description">>, SchemaJObj, <<>>),

    PlusPatternProperties = kz_json:merge(get_pattern_properties(SchemaJObj)
                                         ,get_properties(SchemaJObj)
                                         ),

    F = fun (K, V, Acc) -> property_to_row(SchemaJObj, K, V, Acc) end,
    {Reversed, RefSchemas} = kz_json:foldl(F, {[], BaseRefs}, PlusPatternProperties),

    OneOfs = kz_json:get_list_value(<<"oneOf">>, SchemaJObj, []),
    OneOfRefs = lists:foldl(fun one_of_to_row/2, RefSchemas, OneOfs),

    AnyOfs = kz_json:get_list_value(<<"anyOf">>, SchemaJObj, []),
    AnyOfRefs = lists:foldl(fun any_of_to_row/2, OneOfRefs, AnyOfs),

    WithSubRefs = include_sub_refs(AnyOfRefs),

    [schema_description(Description), [?TABLE_HEADER, Reversed], "\n"]
        ++ [{RefSchemaName, RefTable}
            || RefSchemaName <- WithSubRefs,
               BaseRefs =:= [],
               (RefSchema = load_ref_schema(RefSchemaName)) =/= 'undefined',
               (RefTable = schema_to_table(RefSchema, WithSubRefs)) =/= []
           ].

-spec schema_description(binary()) -> iodata().
schema_description(<<>>) -> <<>>;
schema_description(Description) -> [Description, "\n\n"].

include_sub_refs(Refs) ->
    lists:usort(lists:foldl(fun include_sub_ref/2, [], Refs)).

include_sub_ref(?NE_BINARY = Ref, Acc) ->
    case props:is_defined(Ref, Acc) of
        'true' -> Acc;
        'false' ->
            include_sub_ref(Ref, [Ref | Acc], load_ref_schema(Ref))
    end;
include_sub_ref(RefSchema, Acc) ->
    include_sub_ref(kz_doc:id(RefSchema), Acc).

include_sub_ref(_Ref, Acc, 'undefined') -> Acc;
include_sub_ref(_Ref, Acc, SchemaJObj) ->
    kz_json:foldl(fun include_sub_refs_from_schema/3, Acc, SchemaJObj).

include_sub_refs_from_schema(<<"properties">>, ValueJObj, Acc) ->
    ?LOG_DEBUG("p: ~p", [ValueJObj]),
    kz_json:foldl(fun include_sub_refs_from_schema/3, Acc, ValueJObj);
include_sub_refs_from_schema(<<"patternProperties">>, ValueJObj, Acc) ->
    ?LOG_DEBUG("pp: ~p", [ValueJObj]),
    kz_json:foldl(fun include_sub_refs_from_schema/3, Acc, ValueJObj);
include_sub_refs_from_schema(<<"oneOf">>, Values, Acc) ->
    ?LOG_DEBUG("one-of: ~p", [Values]),
    lists:foldl(fun(JObj, Acc0) ->
                        kz_json:foldl(fun include_sub_refs_from_schema/3, Acc0, JObj)
                end
               ,Acc
               ,Values
               );
include_sub_refs_from_schema(<<"$ref">>, Ref, Acc) ->
    ?LOG_DEBUG("ref: ~p", [Ref]),
    include_sub_ref(Ref, Acc);
include_sub_refs_from_schema(<<"additionalProperties">>, 'false', Acc) ->
    Acc;
include_sub_refs_from_schema(<<"additionalProperties">>, 'true', Acc) ->
    Acc;
include_sub_refs_from_schema(<<"additionalProperties">>, Schema, Acc) ->
    kz_json:foldl(fun include_sub_refs_from_schema/3, Acc, Schema);
include_sub_refs_from_schema(_Key, Value, Acc) ->
    case kz_json:is_json_object(Value) of
        'false' -> Acc;
        'true' ->
            kz_json:foldl(fun include_sub_refs_from_schema/3, Acc, Value)
    end.

-spec load_ref_schema(kz_term:ne_binary()) -> kz_term:api_object().
load_ref_schema(SchemaName) ->
    File = schema_path(<<SchemaName/binary, ".json">>),
    case file:read_file(File) of
        {'ok', SchemaBin} -> kz_json:decode(SchemaBin);
        {'error', _E} -> 'undefined'
    end.

one_of_to_row(Option, Refs) ->
    maybe_add_ref(Refs, Option).

any_of_to_row(Option, Refs) ->
    maybe_add_ref(Refs, Option).

-spec property_to_row(kz_json:object(), kz_term:ne_binary() | kz_term:ne_binaries(), kz_json:object(), {iodata(), kz_term:ne_binaries()}) ->
                             {iodata(), kz_term:ne_binaries()}.
property_to_row(SchemaJObj, Name=?NE_BINARY, Settings, {_, _}=Acc) ->
    property_to_row(SchemaJObj, [Name], Settings, Acc);
property_to_row(SchemaJObj, Names, Settings, {Table, Refs}) ->
    SchemaType =
        try schema_type(Settings)
        catch 'throw':'no_type' ->
                io:format("no schema type in ~s for path ~p: ~p~n"
                         ,[kz_doc:id(SchemaJObj), Names, Settings]
                         ),
                cell_wrap('undefined')
        end,

    maybe_sub_properties_to_row(SchemaJObj
                               ,get_type(Settings)
                               ,Names
                               ,Settings
                               ,{[?TABLE_ROW(cell_wrap(kz_binary:join(Names, <<".">>))
                                            ,settings_description(Settings)
                                            ,SchemaType
                                            ,cell_wrap(kz_json:get_value(<<"default">>, Settings))
                                            ,cell_wrap(is_row_required(Names, SchemaJObj))
                                            ,cell_wrap(support_level(Names, SchemaJObj))
                                            )
                                  | Table
                                 ]
                                ,maybe_add_ref(Refs, Settings)
                                }
                               ).

settings_description(Settings) ->
    case kz_json:get_ne_binary_value(<<"description">>, Settings) of
        'undefined' ->
            case kz_json:get_value(<<"$ref">>, Settings) of
                'undefined' -> <<" ">>;
                RefSchemaName ->
                    ?LOG_INFO("using ref ~s description", [RefSchemaName]),
                    {'ok', RefSchema} = kz_json_schema:fload(RefSchemaName),
                    kz_json:get_ne_binary_value(<<"description">>, RefSchema, <<" ">>)
            end;
        Description -> Description
    end.


-spec maybe_add_ref(kz_term:ne_binaries(), kz_json:object()) -> kz_term:ne_binaries().
maybe_add_ref(Refs, Settings) ->
    case get_ref(Settings) of
        'undefined' -> Refs;
        Ref -> lists:usort([Ref | Refs])
    end.

-spec is_row_required([kz_term:ne_binary() | nonempty_string()], kz_json:object()) -> boolean().
is_row_required(Names=[_|_], SchemaJObj) ->
    Path = name_to_path(Names, <<"required">>),
    case lists:last(Names) of
        "[]" -> 'false';
        Name ->
            ARegexSize = byte_size(Name) - 2,
            lists:member(case Name of
                             <<"/", ARegex:ARegexSize/binary, "/">> -> ARegex;
                             _ -> Name
                         end
                        ,kz_json:get_list_value(Path, SchemaJObj, [])
                        )
    end.

name_to_path(Names, Last) ->
    lists:flatten(
      [case "[]" =:= Key of
           'true' -> [<<"items">>];
           'false' ->
               NewSize = byte_size(Key) - 2,
               case Key of
                   <<"/", Regex:NewSize/binary, "/">> -> [<<"patternProperties">>, Regex];
                   _ -> [<<"properties">>, Key]
               end
       end
       || Key <- lists:droplast(Names)
      ] ++ [Last]
     ).

support_level([], SchemaJObj) ->
    kz_json:get_ne_binary_value(<<"support_level">>, SchemaJObj);
support_level(["[]"|Names], SchemaJObj) ->
    ItemSchemaJObj = kz_json:get_json_value([<<"items">>], SchemaJObj),
    support_level(Names, ItemSchemaJObj);
support_level([Name|Names], SchemaJObj) ->
    case kz_json:get_json_value([<<"properties">>, Name], SchemaJObj) of
        'undefined' -> 'undefined';
        NameSchema -> support_level(Names, NameSchema)
    end.

schema_type(Settings) ->
    case schema_type(Settings, get_type(Settings)) of
        <<"[", _/binary>>=Type -> Type;
        Type -> cell_wrap(Type)
    end.

schema_type(Settings, 'undefined') ->
    case get_ref(Settings) of
        'undefined' ->
            maybe_schema_type_from_enum(Settings);
        Def ->
            schema_ref_type(Def)
    end;
schema_type(Settings, <<"array">>) ->
    schema_array_type(Settings);
schema_type(Settings, <<"string">>) ->
    case kz_json:get_value(<<"enum">>, Settings) of
        L when is_list(L) -> schema_enum_type(L);
        _ -> schema_string_type(Settings)
    end;
schema_type(Settings, Types) when is_list(Types) ->
    kz_binary:join([schema_type(Settings, Type) || Type <- Types], <<" | ">>);
schema_type(_Settings, Type) -> <<Type/binary, "()">>.

maybe_schema_type_from_enum(Settings) ->
    case kz_json:get_list_value(<<"enum">>, Settings) of
        L when is_list(L) -> schema_enum_type(L);
        'undefined' ->
            maybe_schema_type_from_oneof(Settings)
    end.

maybe_schema_type_from_oneof(Settings) ->
    case kz_json:get_list_value(<<"oneOf">>, Settings) of
        'undefined' ->
            maybe_schema_type_from_anyof(Settings);
        OneOf ->
            SchemaTypes = [schema_type(OneOfJObj, get_type(OneOfJObj))
                           || OneOfJObj <- OneOf
                          ],
            kz_binary:join(SchemaTypes, <<" | ">>)
    end.

maybe_schema_type_from_anyof(Settings) ->
    case kz_json:get_list_value(<<"anyOf">>, Settings) of
        'undefined' ->
            io:format("no type: ~p~n", [Settings]),
            throw('no_type');
        AnyOf ->
            SchemaTypes = [schema_type(AnyOfJObj, get_type(AnyOfJObj))
                           || AnyOfJObj <- AnyOf
                          ],
            kz_binary:join(SchemaTypes, <<" || ">>)
    end.

schema_ref_type(Def) ->
    <<"[#/definitions/", Def/binary, "](#", (to_anchor_link(Def))/binary, ")">>.

schema_array_type(Settings) ->
    case kz_json:get_ne_value([<<"items">>, <<"type">>], Settings) of
        'undefined' -> schema_array_type_from_ref(Settings);
        Type ->
            ItemType = schema_type(kz_json:get_json_value(<<"items">>, Settings), Type),
            <<"array(", ItemType/binary, ")">>
    end.

schema_array_type_from_ref(Settings) ->
    case kz_json:get_ne_binary_value([<<"items">>, <<"$ref">>], Settings) of
        'undefined' -> <<"array()">>;
        Ref -> ["array(", schema_ref_type(Ref), ")"]
    end.

schema_enum_type(L) ->
    <<"string('", (kz_binary:join(L, <<"' | '">>))/binary, "')">>.

schema_string_type(Settings) ->
    case {kz_json:get_integer_value(<<"minLength">>, Settings)
         ,kz_json:get_integer_value(<<"maxLength">>, Settings)
         }
    of
        {'undefined', 'undefined'} -> <<"string()">>;
        {'undefined', MaxLength} -> <<"string(0..", (kz_term:to_binary(MaxLength))/binary, ")">>;
        {MinLength, 'undefined'} -> <<"string(", (kz_term:to_binary(MinLength))/binary, "..)">>;
        {Length, Length} -> <<"string(", (kz_term:to_binary(Length))/binary, ")">>;
        {MinLength, MaxLength} -> <<"string(", (kz_term:to_binary(MinLength))/binary, "..", (kz_term:to_binary(MaxLength))/binary, ")">>
    end.

to_anchor_link(Bin) ->
    binary:replace(Bin, <<".">>, <<>>).

cell_wrap('undefined') -> <<" ">>;
cell_wrap([]) -> <<"`[]`">>;
cell_wrap(L) when is_list(L) -> [<<"`[\"">>, kz_binary:join(L, <<"\", \"">>), <<"\"]`">>];
cell_wrap(<<>>) -> <<"\"\"">>;
cell_wrap(B) when is_binary(B) -> [<<"`">>, B, <<"`">>];
cell_wrap(Type) ->
    case kz_json:is_json_term(Type) of
        'true' -> [<<"`">>, kz_json:encode(Type), <<"`">>];
        'false' -> [<<"`">>, kz_term:to_binary(Type), <<"`">>]
    end.

maybe_sub_properties_to_row(SchemaJObj, <<"object">>, Names, Settings, {_,_}=Acc0) ->
    ?LOG_DEBUG("names: ~p ~p~n", [Names, Settings]),
    Acc2 = lists:foldl(fun(Key, {_,_}=Acc1) ->
                               maybe_object_properties_to_row(SchemaJObj, Key, Acc1, Names, Settings)
                       end
                      ,Acc0
                      ,[<<"properties">>, <<"patternProperties">>]
                      ),
    maybe_one_of_to_rows(SchemaJObj
                        ,Names
                        ,Settings
                        ,maybe_any_of_to_rows(SchemaJObj, Names, Settings, Acc2)
                        );
maybe_sub_properties_to_row(SchemaJObj, <<"array">>, Names, Settings, {Table, Refs}) ->
    ?LOG_DEBUG("array item type for ~p: ~p", [Names, kz_json:get_ne_binary_value([<<"items">>, <<"type">>], Settings)]),
    case kz_json:get_ne_binary_value([<<"items">>, <<"type">>], Settings) of
        <<"object">> = Type ->
            ?LOG_DEBUG("array(object()): ~p", [Settings]),
            maybe_sub_properties_to_row(SchemaJObj
                                       ,Type
                                       ,Names ++ ["[]"]
                                       ,kz_json:get_json_value(<<"items">>, Settings, kz_json:new())
                                       ,{Table, Refs}
                                       );
        <<"string">> ->
            ?LOG_DEBUG("getting array item types for ~p", [Names]),
            Items = kz_json:get_json_value(<<"items">>, Settings),
            EnumedType = schema_type(Items, get_type(Items)),
            ?LOG_DEBUG("array(~s)", [EnumedType]),
            {[?TABLE_ROW(cell_wrap(kz_binary:join(Names ++ ["[]"], <<".">>))
                        ,<<" ">>
                        ,cell_wrap(EnumedType)
                        ,<<" ">>
                        ,cell_wrap(is_row_required(Names, SchemaJObj))
                        ,cell_wrap(support_level(Names, SchemaJObj))
                        )
              | Table
             ]
            ,Refs
            };
        'undefined' ->
            maybe_array_composite_types(Names, Settings, {Table, Refs});
        _Type ->
            {Table, Refs}
    end;
maybe_sub_properties_to_row(SchemaJObj, [_|_]=Types, Names, Settings, Acc) ->
    ?LOG_DEBUG("multiple types for ~p: ~p", [Names, Types]),
    lists:foldl(fun(Type, Acc0) -> maybe_sub_properties_to_row(SchemaJObj, Type, Names, Settings, Acc0) end
               ,Acc
               ,Types
               );
maybe_sub_properties_to_row(SchemaJObj, 'undefined', Names, Settings, Acc) ->
    ?LOG_DEBUG("no type defined for ~p", [Names]),
    case get_ref(Settings) of
        'undefined' -> Acc;
        RefId ->
            {'ok', RefSchema} = kz_json_schema:fload(RefId),
            ?LOG_DEBUG("$ref ~p type ~p", [RefId, get_type(RefSchema)]),
            maybe_sub_properties_to_row(SchemaJObj, get_type(RefSchema), Names, Settings, Acc)
    end;
maybe_sub_properties_to_row(_SchemaJObj, _Type, _Keys, _Settings, Acc) ->
    ?LOG_DEBUG("ignoring type ~p for ~p", [_Type, _Keys]),
    Acc.

maybe_array_composite_types(Names, Settings, {Table, Refs}) ->
    case kz_json:get_list_value([<<"items">>, <<"oneOf">>], Settings) of
        'undefined' -> {Table, Refs};
        [_|_]=SubSchemas ->
            ?LOG_DEBUG("oneOf schemas: ~p", [SubSchemas]),
            array_composite_types(Names, SubSchemas, {Table, Refs})
    end.

array_composite_types(Names, Schemas, {Table, Refs}) ->
    ?LOG_DEBUG("getting schema types for ~p~n", [Names]),
    EnumedType = kz_binary:join([schema_type(Schema, get_type(Schema)) || Schema <- Schemas], <<"|">>),
    ?LOG_DEBUG("array oneOf: ~p", [EnumedType]),
    {[?TABLE_ROW(cell_wrap(kz_binary:join(Names ++ ["[]"], <<".">>))
                ,<<" ">>
                ,cell_wrap(EnumedType)
                ,<<" ">>
                ,<<" ">>
                ,<<" ">>
                )
      | Table
     ]
    ,Refs
    }.

maybe_object_properties_to_row(SchemaJObj, Key, Acc0, Names, Settings) ->
    SubSchema = kz_json:get_json_value(Key, Settings, kz_json:new()),
    ?LOG_DEBUG("object prop to row: ~p: ~p", [Names, SubSchema]),
    kz_json:foldl(fun(Name, SubSettings, Acc1) ->
                          property_to_row(SchemaJObj, Names ++ [maybe_regex_name(Key, Name)], SubSettings, Acc1)
                  end
                 ,Acc0
                 ,SubSchema
                 ).

maybe_regex_name(<<"patternProperties">>, Name) ->
    <<"/", Name/binary, "/">>;
maybe_regex_name(_Key, Name) ->
    Name.

maybe_one_of_to_rows(SchemaJObj, Names, Settings, Acc0) ->
    lists:foldl(fun(SubSchema, Acc1) ->
                        ?LOG_DEBUG("sub schema for ~p: ~p", [Names, SubSchema]),
                        maybe_one_of_to_row(SchemaJObj, Names, SubSchema, Acc1)
                end
               ,Acc0
               ,kz_json:get_list_value(<<"oneOf">>, Settings, [])
               ).

maybe_one_of_to_row(_SchemaJObj, _Names, Settings, {Table, Refs0}) ->
    {Table, maybe_add_ref(Refs0, Settings)}.

maybe_any_of_to_rows(SchemaJObj, Names, Settings, Acc0) ->
    lists:foldl(fun(SubSchema, Acc1) ->
                        maybe_any_of_to_row(SchemaJObj, Names, SubSchema, Acc1)
                end
               ,Acc0
               ,kz_json:get_list_value(<<"anyOf">>, Settings, [])
               ).

maybe_any_of_to_row(SchemaJObj, Names, Settings, Acc0) ->
    SubSchema =
        case get_ref(Settings) of
            'undefined' -> Settings;
            RefSchemaId -> load_ref_schema(RefSchemaId)
        end,

    PlusPatternProperties = kz_json:merge(get_pattern_properties(SubSchema)
                                         ,get_properties(SubSchema)
                                         ),
    F = fun (K, V, Acc) ->
                property_to_row(SchemaJObj, Names ++ [K], V, Acc)
        end,
    kz_json:foldl(F, Acc0, PlusPatternProperties).

get_type(Schema) ->
    kz_json:get_value(<<"type">>, Schema).

get_ref(Schema) ->
    kz_json:get_ne_binary_value(<<"$ref">>, Schema).

get_properties(Schema) ->
    kz_json:get_json_value(<<"properties">>, Schema, kz_json:new()).

get_pattern_properties(Schema) ->
    kz_json:get_json_value(<<"patternProperties">>, Schema, kz_json:new()).
