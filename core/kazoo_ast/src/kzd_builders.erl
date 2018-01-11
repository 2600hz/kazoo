-module(kzd_builders).

-export([build_accessors/0
        ,build_accessor/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-spec build_accessors() -> 'ok'.
build_accessors() ->
    filelib:fold_files(kz_ast_util:schema_path(<<>>)
                      ,<<"^[a-z_]+\\.json$">>
                      ,'false'
                      ,fun build_accessor/2
                      ,'ok'
                      ).

-spec build_accessor(file:filename()) -> 'ok'.
build_accessor(SchemaPath) ->
    build_accessor(SchemaPath, 'ok').

-spec build_accessor(file:filename(), 'ok') -> 'ok'.
build_accessor(SchemaPath, 'ok') ->
    {'ok', SchemaJObj} = kz_json_schema:fload(SchemaPath),
    SchemaId = kz_doc:id(SchemaJObj),
    ?LOG_INFO("building ~s~n", [SchemaId]),

    BaseModule = base_module(SchemaId),

    case accessors_from_properties(kz_json:get_json_value(<<"properties">>, SchemaJObj)) of
        'undefined' ->
            ?LOG_INFO("no properties for ~s~n", [SchemaId]);
        {Exports, Accessors} ->
            save_module(SchemaId, [BaseModule
                                  ,lists:reverse(Exports), "\n"
                                  ,base_includes()
                                  ,base_types()
                                  ,lists:reverse(Accessors)
                                  ])
    end.

save_module(Id, FileContents) ->
    SrcDir = code:lib_dir('kazoo_documents', 'src'),
    Filename = filename:join([SrcDir, <<"kzd_", Id/binary, ".erl.src">>]),
    'ok' = file:write_file(Filename, FileContents),
    ?LOG_INFO("wrote ~s~n", [Filename]).

accessors_from_properties('undefined') -> 'undefined';
accessors_from_properties(Properties) ->
    kz_json:foldl(fun accessor_from_properties/3
                 ,{base_exports(), base_accessors()}
                 ,Properties
                 ).

base_module(SchemaName) ->
    ["-module(kzd_", clean_name(SchemaName), ").\n"].

clean_name([_|_]=Names) ->
    [clean_name(Name) || Name <- Names];
clean_name(Name) ->
    binary:replace(Name, <<"-">>, <<"_">>, ['global']).

base_exports() ->
    ["\n"
     "-export([new/0]).\n"
    ].

base_includes() ->
    ["\n"
     "-include(\"kz_documents.hrl\").\n"
    ].

base_types() ->
    ["\n"
     "-type doc() :: kz_json:object().\n"
     "-export_type([doc/0]).\n"
    ].

base_accessors() ->
    ["\n"
     "-spec new() -> doc().\n"
     "new() ->\n"
     "    kz_json_schema:default_object(?MODULE_STRING).\n"
    ].

accessor_from_properties(?NE_BINARY=Property, Schema, Acc) ->
    accessor_from_properties([Property], Schema, Acc);
accessor_from_properties(Property, Schema, {Exports, Accessors}) ->
    Acc = {add_exports(clean_name(Property), Exports)
          ,add_accessors(clean_name(Property), Schema, Accessors)
          },
    maybe_add_sub_properties(Property, Schema, Acc, kz_json:get_value(<<"type">>, Schema)).

maybe_add_sub_properties(Property, Schema, Acc0, <<"object">>) ->
    kz_json:foldl(fun(SubProperty, SubSchema, Acc) ->
                          add_sub_property(SubProperty, SubSchema, Acc, Property)
                  end
                 ,Acc0
                 ,kz_json:get_json_value(<<"properties">>, Schema, kz_json:new())
                 );
maybe_add_sub_properties(_Property, _Schema, Acc, _Type) -> Acc.

add_sub_property(SubProperty, SubSchema, Acc, ParentProperty) ->
    accessor_from_properties(ParentProperty ++ [SubProperty], SubSchema, Acc).

getter_name([_|_]=Properties) ->
    kz_binary:join([getter_name(P) || P <- Properties], <<"_">>);
getter_name(Property) ->
    kz_term:to_lower_binary(Property).

add_exports(Property, Exports) ->
    Getter = getter_name(Property),
    [["-export([", Getter, "/1, ", Getter, "/2, set_", Getter, "/2]).\n"]
     | Exports
    ].

add_accessors(Property, Schema, Accessors) ->
    {JSONGetterFun, ReturnType} = json_getter_fun(Schema),
    Default = default_value(Schema, JSONGetterFun),

    Getter = getter_name(Property),
    SetVar = kz_ast_util:smash_snake(kz_binary:ucfirst(Getter), <<>>),
    JSONPath = json_path(Property),

    [["\n"
      "-spec ", Getter, "(doc()) -> ", default_return_type(ReturnType, Default), ".\n"
      "-spec ", Getter, "(doc(), Default) -> ", ReturnType, " | Default.\n"
     ,Getter, "(Doc) ->\n"
      "    ", Getter, "(Doc, ", Default, ").\n"
     ,Getter, "(Doc, Default) ->\n"
      "    kz_json:", JSONGetterFun, "(", JSONPath, ", Doc, Default).\n\n"
     ,"-spec set_", Getter, "(doc(), ", ReturnType, ") -> doc().\n"
     ,"set_", Getter, "(Doc, ", SetVar, ") ->\n"
     ,"    kz_json:set_value(", JSONPath, ", ", SetVar, ", Doc).\n"
     ]
     | Accessors
    ].

json_path([Parent|Properties]) ->
    ["[", json_path(Parent)
    ,[[", ", json_path(Property)] || Property <- Properties]
    ,"]"
    ];
json_path(Property) ->
    ["<<\"", Property, "\">>"].

json_getter_fun(Schema) ->
    json_getter_fun(Schema, kz_json:get_value(<<"type">>, Schema)).

json_getter_fun(_Schema, <<"object">>) ->
    {"get_json_value", "kz_json:object()"};
json_getter_fun(_Schema, <<"boolean">>) ->
    {"get_boolean_value", "boolean()"};
json_getter_fun(Schema, <<"array">>) ->
    {"get_list_value", list_return_subtype(Schema)};
json_getter_fun(_Schema, <<"integer">>) ->
    {"get_integer_value", "integer()"};
json_getter_fun(_Schema, <<"number">>) ->
    {"get_value", "number()"};
json_getter_fun(Schema, 'undefined') ->
    case kz_json:get_value(<<"$ref">>, Schema) of
        'undefined' ->
            {"get_value", "any()"};
        Ref ->
            {'ok', RefSchema} = kz_json_schema:fload(Ref),
            json_getter_fun(RefSchema, kz_json:get_value(<<"type">>, RefSchema))
    end;
json_getter_fun(Schema, <<"string">>) ->
    case kz_json:get_integer_value(<<"minLength">>, Schema) of
        N when is_integer(N), N > 0 ->
            {"get_ne_binary_value", "kz_term:ne_binary()"};
        _ ->
            {"get_binary_value", "binary()"}
    end;
json_getter_fun(_Schema, [_|_]=_Type) ->
    ?LOG_INFO("composite type ~p~n", [_Type]),
    {"get_value", "any()"}; %% composite type
json_getter_fun(_Schema, _Type) ->
    ?LOG_INFO("unhandled type ~p~n", [_Type]),
    {"get_value", "any()"}.

list_return_subtype(Schema) ->
    list_return_subtype(Schema, kz_json:get_value([<<"items">>, <<"type">>], Schema)).

list_return_subtype(Schema, 'undefined') ->
    case kz_json:get_value([<<"items">>, <<"$ref">>], Schema) of
        'undefined' -> "list()";
        Ref ->
            {'ok', RefSchema} = kz_json_schema:fload(Ref),
            list_return_subtype(RefSchema, kz_json:get_value(<<"type">>, RefSchema))
    end;
list_return_subtype(_Schema, <<"string">>) -> "kz_term:ne_binaries()";
list_return_subtype(_Schema, <<"integer">>) -> "kz_term:integers()";
list_return_subtype(_Schema, <<"number">>) -> "[number()]";
list_return_subtype(_Schema, <<"object">>) -> "kz_json:objects()";
list_return_subtype(_Schema, _Type) ->
    ?LOG_INFO("unhandled subtype ~p~n", [_Type]),
    "list()".

default_value(Schema, JSONGetterFun) ->
    default_value(Schema, JSONGetterFun, kz_json:get_value(<<"default">>, Schema)).

default_value(_Schema, _JSONGetterFun, 'undefined') ->
    "'undefined'";
default_value(_Schema, "get_ne_binary_value", Default) ->
    ["<<\"", Default, "\">>"];
default_value(_Schema, "get_binary_value", Default) ->
    ["<<\"", Default, "\">>"];
default_value(_Schema, "get_list_value", []) ->
    "\[\]";
default_value(_Schema, _JSONGetterFun, ?EMPTY_JSON_OBJECT) ->
    "kz_json:new()";
default_value(_Schema, _JSONGetterFun, Default) ->
    kz_term:to_binary(Default).

default_return_type("kz_json:" ++ Type, "'undefined'") ->
    "kz_term:api_" ++ Type;
default_return_type("any()", _) -> "any()";
default_return_type("kz_term:" ++ Type, "'undefined'") ->
    ["kz_term:api_" | Type];
default_return_type(Type, _Default) -> Type.
