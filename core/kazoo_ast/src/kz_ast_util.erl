-module(kz_ast_util).

-export([module_ast/1
        ,add_module_ast/3

        ,binary_match_to_binary/1
        ,smash_snake/1

        ,schema_path/1
        ,api_path/1
        ,ensure_file_exists/1
        ,create_schema/1
        ,schema_to_table/1
        ,load_ref_schema/1

        ,project_apps/0, app_modules/1
        ]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo_ast/include/kz_ast.hrl").

-type ast() :: [erl_parse:abstract_form()].
-type abstract_code() :: {'raw_abstract_v1', ast()}.

-spec module_ast(atom()) -> {atom(), abstract_code()} | 'undefined'.
module_ast(M) ->
    case code:which(M) of
        'non_existing' -> 'undefined';
        'preloaded' -> 'undefined';
        Beam ->
            {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(Beam, ['abstract_code']),
            {Module, AST}
    end.

-spec add_module_ast(module_ast(), module(), abstract_code()) -> module_ast().
add_module_ast(ModAST, Module, {'raw_abstract_v1', Attributes}) ->
    lists:foldl(fun(A, Acc) ->
                        add_module_ast_fold(A, Module, Acc)
                end
               ,ModAST
               ,Attributes
               ).

-spec add_module_ast_fold(ast(), module(), module_ast()) -> module_ast().
add_module_ast_fold(?AST_FUNCTION(F, Arity, Clauses), Module, #module_ast{functions=Fs}=Acc) ->
    Acc#module_ast{functions=[{Module, F, Arity, Clauses}|Fs]};
add_module_ast_fold(?AST_RECORD(Name, Fields), _Module, #module_ast{records=Rs}=Acc) ->
    Acc#module_ast{records=[{Name, Fields}|Rs]};
add_module_ast_fold(_Other, _Module, Acc) ->
    Acc.

-spec binary_match_to_binary(ast()) -> binary().
binary_match_to_binary(?ATOM(A)) -> kz_term:to_binary(A);
binary_match_to_binary(?BINARY_STRING(V)) ->
    kz_term:to_binary(V);
binary_match_to_binary(?BINARY_MATCH(Match)) ->
    binary_match_to_binary(Match);
binary_match_to_binary(?FUN_ARGS(atom_to_binary, [?ATOM(Atom), ?ATOM(utf8)])) ->
    atom_to_binary(Atom, utf8);
binary_match_to_binary(Match) when is_list(Match) ->
    iolist_to_binary(
      [binary_part_to_binary(BP) || BP <- Match]
     ).

binary_part_to_binary(?BINARY_STRING(V)) -> V;
binary_part_to_binary(?SUB_BINARY(V)) -> V;
binary_part_to_binary(?BINARY_MATCH(Ms)) -> binary_match_to_binary(Ms).

%% user_auth -> User Auth
-spec smash_snake(ne_binary()) -> iolist().
smash_snake(BaseName) ->
    case binary:split(BaseName, <<"_">>, ['global']) of
        [Part] -> format_name_part(Part);
        [H|Parts] ->
            [format_name_part(H)
             | [[<<" ">>, format_name_part(Part)] || Part <- Parts]
            ]
    end.

-spec format_name_part(ne_binary()) -> ne_binary().
format_name_part(<<"api">>) -> <<"API">>;
format_name_part(<<"ip">>) -> <<"IP">>;
format_name_part(<<"auth">>) -> <<"Authentication">>;
format_name_part(Part) ->
    kz_binary:ucfirst(Part).

-spec schema_path(binary()) -> file:filename_all().
schema_path(Base) ->
    case filename:join([code:priv_dir('crossbar')
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
        filelib:is_dir(Dir)
    ].

dir_to_app_name(Dir) ->
    kz_term:to_atom(filename:basename(Dir), 'true').

-spec app_modules(atom()) -> [atom()].
app_modules(App) ->
    case application:get_key(App, 'modules') of
        {'ok', Modules} -> Modules;
        'undefined' ->
            'ok' = application:load(App),
            app_modules(App)
    end.


-define(TABLE_ROW(Key, Description, Type, Default, Required)
       ,[kz_binary:join([Key, Description, Type, Default, Required]
                       ,<<" | ">>
                       )
        ,$\n
        ]
       ).
-define(TABLE_HEADER
       ,[?TABLE_ROW(<<"Key">>, <<"Description">>, <<"Type">>, <<"Default">>, <<"Required">>)
        ,?TABLE_ROW(<<"---">>, <<"-----------">>, <<"----">>, <<"-------">>, <<"--------">>)
        ]).

-spec schema_to_table(ne_binary() | kz_json:object()) ->
                             [iodata() | {ne_binary(), iodata()}].
schema_to_table(<<"#/definitions/", _/binary>>=_S) ->
    [];
schema_to_table(Schema=?NE_BINARY) ->
    case kz_json_schema:fload(Schema) of
        {'ok', JObj} -> schema_to_table(JObj);
        {'error', 'not_found'} ->
            io:format("failed to find ~s~n", [Schema]),
            throw({'error', 'no_schema'})
    end;
schema_to_table(SchemaJObj) ->
    schema_to_table(SchemaJObj, []).

schema_to_table(SchemaJObj, BaseRefs) ->
    Description = kz_json:get_binary_value(<<"description">>, SchemaJObj, <<>>),
    Properties = kz_json:get_value(<<"properties">>, SchemaJObj, kz_json:new()),
    F = fun (K, V, Acc) -> property_to_row(SchemaJObj, K, V, Acc) end,
    {Reversed, RefSchemas} = kz_json:foldl(F, {[?TABLE_HEADER], BaseRefs}, Properties),

    OneOfRefs = lists:foldl(fun one_of_to_row/2
                           ,RefSchemas
                           ,kz_json:get_value(<<"oneOf">>, SchemaJObj, [])
                           ),

    WithSubRefs = include_sub_refs(OneOfRefs),

    [[schema_description(Description), lists:reverse(Reversed)]
     |[{RefSchemaName, RefTable}
       || RefSchemaName <- WithSubRefs,
          BaseRefs =:= [],
          (RefSchema = load_ref_schema(RefSchemaName)) =/= 'undefined',
          (RefTable = schema_to_table(RefSchema, WithSubRefs)) =/= []
      ]
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
    kz_json:foldl(fun include_sub_refs_from_schema/3, Acc, ValueJObj);
include_sub_refs_from_schema(<<"patternProperties">>, ValueJObj, Acc) ->
    kz_json:foldl(fun include_sub_refs_from_schema/3, Acc, ValueJObj);
include_sub_refs_from_schema(<<"oneOf">>, Values, Acc) ->
    lists:foldl(fun(JObj, Acc0) ->
                        kz_json:foldl(fun include_sub_refs_from_schema/3, Acc0, JObj)
                end
               ,Acc
               ,Values
               );
include_sub_refs_from_schema(<<"$ref">>, Ref, Acc) ->
    include_sub_ref(Ref, Acc);
include_sub_refs_from_schema(_Key, Value, Acc) ->
    case kz_json:is_json_object(Value) of
        'false' -> Acc;
        'true' ->
            kz_json:foldl(fun include_sub_refs_from_schema/3, Acc, Value)
    end.

-spec load_ref_schema(ne_binary()) -> api_object().
load_ref_schema(SchemaName) ->
    File = schema_path(<<SchemaName/binary, ".json">>),
    case file:read_file(File) of
        {'ok', SchemaBin} -> kz_json:decode(SchemaBin);
        {'error', _E} -> 'undefined'
    end.

one_of_to_row(Option, Refs) ->
    maybe_add_ref(Refs, Option).

-spec property_to_row(kz_json:object(), ne_binary() | ne_binaries(), kz_json:object(), {iodata(), ne_binaries()}) ->
                             {iodata(), ne_binaries()}.
property_to_row(SchemaJObj, Name=?NE_BINARY, Settings, {_, _}=Acc) ->
    property_to_row(SchemaJObj, [Name], Settings, Acc);
property_to_row(SchemaJObj, Names, Settings, {Table, Refs}) ->
    RequiredV4 = local_required(Names, SchemaJObj),

    maybe_sub_properties_to_row(SchemaJObj
                               ,kz_json:get_value(<<"type">>, Settings)
                               ,Names
                               ,Settings
                               ,{[?TABLE_ROW(cell_wrap(kz_binary:join(Names, <<".">>))
                                            ,kz_json:get_value(<<"description">>, Settings, <<" ">>)
                                            ,schema_type(Settings)
                                            ,cell_wrap(kz_json:get_value(<<"default">>, Settings))
                                            ,cell_wrap(is_row_required(Names, RequiredV4, kz_json:is_true(<<"required">>, Settings)))
                                            )
                                  | Table
                                 ]
                                ,maybe_add_ref(Refs, Settings)
                                }
                               ).

-spec maybe_add_ref(ne_binaries(), kz_json:object()) -> ne_binaries().
maybe_add_ref(Refs, Settings) ->
    case kz_json:get_ne_value(<<"$ref">>, Settings) of
        'undefined' -> Refs;
        Ref -> lists:usort([Ref | Refs])
    end.

local_required(Names, SchemaJObj) ->
    kz_json:get_value(path_local_required(Names), SchemaJObj).

path_local_required([_Name]) -> [<<"required">>];
path_local_required(Names) ->
    ExceptLast = lists:reverse(tl(lists:reverse(Names))),
    [<<"properties">> | ExceptLast] ++ [<<"required">>].

%% @private
%% @doc
%% JSON schema draft v3 wants local/nested boolean "required" fields.
%% Draft v4 wants non-empty string array "required" fields.
%% @end
-spec is_row_required(ne_binaries() | ne_binary(), ne_binaries(), api_boolean()) -> ne_binary().
is_row_required(Names=[_|_], V4, V3) ->
    is_row_required(lists:last(Names), V4, V3);
is_row_required(Name=?NE_BINARY, Required=[_|_], _) ->
    lists:member(Name, Required);
is_row_required(_, _, 'true') -> <<"true">>;
is_row_required(_, _, 'false') -> <<"false">>.

schema_type(Settings) ->
    case schema_type(Settings, kz_json:get_value(<<"type">>, Settings)) of
        <<"[", _/binary>>=Type -> Type;
        Type -> cell_wrap(Type)
    end.

schema_type(Settings, 'undefined') ->
    case kz_json:get_value(<<"$ref">>, Settings) of
        'undefined' ->
            maybe_schema_type_from_enum(Settings);
        Def ->
            <<"[#/definitions/", Def/binary, "](#", (to_anchor_link(Def))/binary, ")">>
    end;
schema_type(Settings, <<"array">>) ->
    case kz_json:get_value([<<"items">>, <<"type">>], Settings) of
        'undefined' -> <<"array()">>;
        Type ->
            ItemType = schema_type(kz_json:get_value(<<"items">>, Settings), Type),
            <<"array(", ItemType/binary, ")">>
    end;
schema_type(Settings, <<"string">>) ->
    case kz_json:get_value(<<"enum">>, Settings) of
        L when is_list(L) -> schema_enum_type(L);
        _ -> schema_string_type(Settings)
    end;
schema_type(Settings, Types) when is_list(Types) ->
    kz_binary:join([schema_type(Settings, Type) || Type <- Types], <<", ">>);
schema_type(_Settings, Type) -> Type.

maybe_schema_type_from_enum(Settings) ->
    case kz_json:get_value(<<"enum">>, Settings) of
        L when is_list(L) -> schema_enum_type(L);
        'undefined' ->
            io:format("no type or ref in ~p~n", [Settings]),
            'undefined'
    end.

schema_enum_type(L) ->
    <<"string('", (kz_binary:join(L, <<"', '">>))/binary, "')">>.

schema_string_type(Settings) ->
    case {kz_json:get_integer_value(<<"minLength">>, Settings)
         ,kz_json:get_integer_value(<<"maxLength">>, Settings)
         }
    of
        {'undefined', 'undefined'} -> <<"string">>;
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
cell_wrap(Type) ->
    case Type =:= kz_json:new() of
        'true' -> <<"`{}`">>;
        'false' -> [<<"`">>, kz_term:to_binary(Type), <<"`">>]
    end.

maybe_sub_properties_to_row(SchemaJObj, <<"object">>, Names, Settings, {_,_}=Acc0) ->
    lists:foldl(fun(Key, {_,_}=Acc1) ->
                        maybe_object_properties_to_row(SchemaJObj, Key, Acc1, Names, Settings)
                end
               ,Acc0
               ,[<<"properties">>, <<"patternProperties">>]
               );
maybe_sub_properties_to_row(SchemaJObj, <<"array">>, Names, Settings, {Table, Refs}) ->
    case kz_json:get_value([<<"items">>, <<"type">>], Settings) of
        <<"object">> = Type ->
            maybe_sub_properties_to_row(SchemaJObj
                                       ,Type
                                       ,Names ++ ["[]"]
                                       ,kz_json:get_value(<<"items">>, Settings, kz_json:new())
                                       ,{Table, Refs}
                                       );
        <<"string">> = Type ->
            RequiredV4 = local_required(Names, SchemaJObj),
            {[?TABLE_ROW(cell_wrap(kz_binary:join(Names ++ ["[]"], <<".">>))
                        ,<<" ">>
                        ,cell_wrap(Type)
                        ,<<" ">>
                        ,cell_wrap(is_row_required(Names, RequiredV4, kz_json:is_true(<<"required">>, Settings)))
                        )
              | Table
             ]
            ,Refs
            };
        _Type -> {Table, Refs}
    end;
maybe_sub_properties_to_row(_SchemaJObj, _Type, _Keys, _Settings, Acc) ->
    Acc.

maybe_object_properties_to_row(SchemaJObj, Key, Acc0, Names, Settings) ->
    kz_json:foldl(fun(Name, SubSettings, Acc1) ->
                          property_to_row(SchemaJObj, Names ++ [maybe_regex_name(Key, Name)], SubSettings, Acc1)
                  end
                 ,Acc0
                 ,kz_json:get_value(Key, Settings, kz_json:new())
                 ).

maybe_regex_name(<<"patternProperties">>, Name) ->
    <<"/", Name/binary, "/">>;
maybe_regex_name(_Key, Name) ->
    Name.
