-module(kzd_builders).

-export([build_accessors/0
        ,build_accessor/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

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
    ["-module(kzd_", clean_name(SchemaName), ").\n\n"].

clean_name(SchemaName) ->
    binary:replace(SchemaName, <<"-">>, <<"_">>, ['global']).

base_exports() ->
    ["-export([new/0]).\n\n"].

base_includes() ->
    ["-include(\"kz_documents.hrl\").\n\n"].

base_types() ->
    ["-type doc() :: kz_json:object().\n"
     "-export_type([doc/0]).\n\n"
    ].

base_accessors() ->
    ["-spec new() -> doc().\n"
     "new() ->\n"
     "    kz_json_schema:default_object(?MODULE_STRING).\n\n"
    ].

accessor_from_properties(Property, Schema, {Exports, Accessors}) ->
    {add_exports(clean_name(Property), Exports)
    ,add_accessors(clean_name(Property), Schema, Accessors)
    }.

add_exports(Property, Exports) ->
    Getter = kz_term:to_lower_binary(Property),
    [["-export([", Getter, "/1, ", Getter, "/2, set_", Getter, "/2]).\n"]
     | Exports
    ].

add_accessors(Property, Schema, Accessors) ->
    {JSONGetterFun, ReturnType} = json_getter_fun(Schema),
    Default = default_value(Schema, JSONGetterFun),

    Getter = kz_term:to_lower_binary(Property),
    SetVar = kz_ast_util:smash_snake(kz_binary:ucfirst(Property), <<>>),

    [["\n"
      "-spec ", Getter, "(doc()) -> ", default_return_type(ReturnType, Default), ".\n"
      "-spec ", Getter, "(doc(), Default) -> ", ReturnType, " | Default.\n"
     ,Getter, "(Doc) ->\n"
      "    ", Getter, "(Doc, ", Default, ").\n"
     ,Getter, "(Doc, Default) ->\n"
      "    kz_json:", JSONGetterFun, "(<<\"", Property, "\">>, Doc, Default).\n\n"
     ,"-spec set_", Getter, "(doc(), ", ReturnType, ") -> doc().\n"
     ,"set_", Getter, "(Doc, ", SetVar, ") ->\n"
     ,"    kz_json:set_value(<<\"", Property, "\">>, ", SetVar, ", Doc).\n"
     ]
     | Accessors
    ].

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
json_getter_fun(Schema, 'undefined') ->
    case kz_json:get_value(<<"$ref">>, Schema) of
        'undefined' ->
            {"get_value", "any()"};
        _Ref ->
            {"get_json_value", "kz_json:object()"}
    end;
json_getter_fun(Schema, <<"string">>) ->
    case kz_json:get_integer_value(<<"minLength">>, Schema) of
        N when is_integer(N), N > 0 ->
            {"get_ne_binary_value", "ne_binary()"};
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
        _Ref -> "kz_json:objects()"
    end;
list_return_subtype(_Schema, <<"string">>) -> "ne_binaries()";
list_return_subtype(_Schema, <<"integer">>) -> "integers()";
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
default_value(_Schema, _JSONGetterFun, Default) ->
    kz_term:to_binary(Default).

default_return_type("kz_json:" ++ Type, "'undefined'") ->
    "api_" ++ Type;
default_return_type("any()", _) -> "any()";
default_return_type(Type, "'undefined'") ->
    ["api_" | Type];
default_return_type(Type, _Default) -> Type.
