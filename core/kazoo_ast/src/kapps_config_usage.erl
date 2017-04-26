-module(kapps_config_usage).

-export([process_project/0, process_app/1
        ,to_schema_docs/0, to_schema_docs/1

        ,expression_to_schema/2
        ]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-define(SOURCE, <<"config_usage_source">>).
-define(FIELD_DEFAULT, <<"default">>).
-define(FIELD_PROPERTIES, <<"properties">>).
-define(SYSTEM_CONFIG_DESCRIPTIONS, kz_ast_util:api_path(<<"descriptions.system_config.json">>)).

-spec to_schema_docs() -> 'ok'.
-spec to_schema_docs(kz_json:object()) -> 'ok'.
to_schema_docs() ->
    to_schema_docs(process_project()).

to_schema_docs(Schemas) ->
    kz_json:foreach(fun update_schema/1, Schemas).

-spec update_schema({kz_json:key(), kz_json:json_term()}) -> 'ok'.
update_schema({Name, AutoGenSchema}) ->
    maybe_update_account_schema(Name, AutoGenSchema),
    SchemaPath = kz_ast_util:schema_path(<<"system_config.", Name/binary, ".json">>),

    GeneratedJObj = filter_system(static_fields(Name, remove_source(AutoGenSchema))),
    ExistingJObj = existing_schema(SchemaPath),
    MergedJObj = kz_json:merge(fun kz_json:merge_left/2, ExistingJObj, GeneratedJObj),
    'ok' = file:write_file(SchemaPath, kz_json:encode(kz_json:delete_key(<<"id">>, MergedJObj))).

-spec existing_schema(file:filename_all()) -> kz_json:object().
existing_schema(Name) ->
    case kz_json_schema:fload(Name) of
        {'ok', JObj} -> JObj;
        {'error', _E} -> io:format("failed to find ~s: ~p~n", [Name, _E]), kz_json:new()
    end.

maybe_update_account_schema(Name, AutoGenSchema) ->
    Path = kz_ast_util:schema_path(<<"account_config.", Name/binary, ".json">>),
    case account_properties(AutoGenSchema) of
        ?EMPTY_JSON_OBJECT -> 'ok';
        Properties ->
            GeneratedJObj = filter_system(static_account_fields(Name, remove_source(Properties))),
            ExistingJObj = existing_schema(Path),
            MergedJObj = kz_json:merge(fun kz_json:merge_left/2, ExistingJObj, GeneratedJObj),
            'ok' = file:write_file(Path, kz_json:encode(kz_json:delete_key(<<"id">>, MergedJObj)))
    end.

-spec account_properties(kz_json:object()) -> kz_json:object().
account_properties(JObj0) ->
    Flat = kz_json:to_proplist(kz_json:flatten(JObj0)),
    Keep = [lists:droplast(K)
            || {K, V} <- Flat,
               V =:= <<"kapps_account_config">>
           ],
    kz_json:expand(
      kz_json:from_list(
        [KV
         || {K,_V}=KV <- Flat,
            lists:member(lists:droplast(K), Keep)
        ]
       )
     ).

-spec remove_source(kz_json:object()) -> kz_json:object().
remove_source(JObj) ->
    Filtered = kz_json:filter(fun filter_no_source/1
                             ,kz_json:flatten(JObj)
                             ),
    kz_json:expand(Filtered).

-spec filter_no_source({kz_json:path(), any()}) -> boolean().
filter_no_source({K, _}) -> not lists:member(?SOURCE, K).

filter_system(JObj) ->
    filter_system_fold(kz_json:get_values(JObj), kz_json:new()).

filter_system_fold({[], []}, JObj) -> JObj;
filter_system_fold({['_system' | Vc], [ _| Kc]}, JObj) ->
    filter_system_fold({Vc, Kc}, JObj);
filter_system_fold({[V | Vc], [K | Kc]}, JObj) ->
    case kz_json:is_json_object(V) of
        'true' -> filter_system_fold({Vc, Kc}, kz_json:set_value(K, filter_system(V), JObj));
        'false' -> filter_system_fold({Vc, Kc}, kz_json:set_value(K, V, JObj))
    end.

static_fields(Name, JObj) ->
    Id = <<"system_config.", Name/binary>>,
    Description = <<"Schema for ", Name/binary, " system_config">>,

    Values = [{<<"description">>, Description}
             ,{<<"$schema">>, <<"http://json-schema.org/draft-04/schema#">>}
             ,{<<"type">>, <<"object">>}
             ],
    kz_json:set_values(Values, kz_doc:set_id(JObj, Id)).

static_account_fields(Name, JObj) ->
    Id = <<"account_config.", Name/binary>>,
    Description = <<"Schema for ", Name/binary, " account_config">>,

    Values = [{<<"description">>, Description}
             ,{<<"$schema">>, <<"http://json-schema.org/draft-04/schema#">>}
             ,{<<"type">>, <<"object">>}
             ],
    kz_json:set_values(Values, kz_doc:set_id(JObj, Id)).

-spec process_project() -> kz_json:object().
process_project() ->
    io:format("processing kapps_config/kapps_account_config usage: "),

    Options = [{'expression', fun expression_to_schema/2}
              ,{'module', fun print_dot/2}
              ,{'accumulator', kz_json:new()}
              ],

    Usage = kazoo_ast:walk_project(Options),
    io:format(" done~n"),
    Usage.

-spec process_app(atom()) -> kz_json:object().
-spec process_app(atom(), kz_json:object()) -> kz_json:object().
process_app(App) ->
    process_app(App, kz_json:new()).

process_app(App, Schemas) ->
    Options = [{'expression', fun expression_to_schema/2}
              ,{'module', fun print_dot/2}
              ,{'accumulator', Schemas}
              ],

    kazoo_ast:walk_app(App, Options).

print_dot(_Module, Acc) ->
    io:format("."),
    Acc.

-spec expression_to_schema(any(), kz_json:object()) -> kz_json:object().
expression_to_schema(?MOD_FUN_ARGS('kapps_config', 'set', _), Schemas) ->
    Schemas;
expression_to_schema(?MOD_FUN_ARGS('kapps_config', 'set_default', _), Schemas) ->
    Schemas;
expression_to_schema(?MOD_FUN_ARGS('kapps_config', 'set_node', _), Schemas) ->
    Schemas;
expression_to_schema(?MOD_FUN_ARGS(Source = 'kapps_config', F, Args), Schemas) ->
    config_to_schema(Source, F, Args, Schemas);
expression_to_schema(?MOD_FUN_ARGS('ecallmgr_config', 'set', _), Schemas) ->
    Schemas;
expression_to_schema(?MOD_FUN_ARGS('ecallmgr_config', 'set_default', _), Schemas) ->
    Schemas;
expression_to_schema(?MOD_FUN_ARGS('ecallmgr_config', 'set_node', _), Schemas) ->
    Schemas;
expression_to_schema(?MOD_FUN_ARGS(Source = 'ecallmgr_config', F, Args), Schemas) ->
    config_to_schema(Source, F, [?BINARY_STRING(<<"ecallmgr">>, 0) | Args], Schemas);
expression_to_schema(?MOD_FUN_ARGS(Source = 'kapps_account_config', F='get_global', Args), Schemas) ->
    config_to_schema(Source, F, Args, Schemas);
expression_to_schema(_Expression, Schema) ->
    Schema.

config_to_schema(_, 'get_all_kvs', _Args, Schemas) ->
    Schemas;
config_to_schema(_, 'flush', _Args, Schemas) ->
    Schemas;
config_to_schema(_, 'migrate', _Args, Schemas) ->
    Schemas;
config_to_schema(_, 'get_node_value', _Args, Schemas) ->
    Schemas;
config_to_schema(_, 'get_category', _Args, Schemas) ->
    Schemas;
config_to_schema(Source, F='get_global', [Account, Cat, K], Schemas) ->
    config_to_schema(Source, F, [Account, Cat, K, 'undefined'], Schemas);
config_to_schema(Source, F='get_global', [_Account, Cat, K, Default], Schemas) ->
    Document = category_to_document(Cat),
    case key_to_key_path(K) of
        'undefined' -> Schemas;
        Key -> config_key_to_schema(Source, F, Document, Key, Default, Schemas)
    end;
config_to_schema(Source, F, [Cat, K], Schemas) ->
    config_to_schema(Source, F, [Cat, K, 'undefined'], Schemas);
config_to_schema(Source, F, [Cat, K, Default, _Node], Schemas) ->
    config_to_schema(Source, F, [Cat, K, Default], Schemas);
config_to_schema(Source, F, [Cat, K, Default], Schemas) ->
    Document = category_to_document(Cat),
    case key_to_key_path(K) of
        'undefined' -> Schemas;
        Key -> config_key_to_schema(Source, F, Document, Key, Default, Schemas)
    end.

config_key_to_schema(_Source, _F, 'undefined', _Key, _Default, Schemas) ->
    Schemas;
config_key_to_schema(Source, F, Document, Key, Default, Schemas) ->
    Properties = guess_properties(Document, Source, Key, guess_type(F, Default), Default),
    kz_json:set_value([Document, ?FIELD_PROPERTIES | Key], Properties, Schemas).

category_to_document(?VAR(_)) -> 'undefined';
category_to_document(Cat) ->
    kz_ast_util:binary_match_to_binary(Cat).

key_to_key_path(?ATOM(A)) -> [kz_term:to_binary(A)];
key_to_key_path(?VAR(_)) -> 'undefined';
key_to_key_path(?EMPTY_LIST) -> [];
key_to_key_path(?LIST(?MOD_FUN_ARGS('kapps_config', _F, [_Doc, Field | _]), Tail)) ->
    [kz_ast_util:binary_match_to_binary(Field)
    ,?FIELD_PROPERTIES
     | key_to_key_path(Tail)
    ];
key_to_key_path(?LIST(?MOD_FUN_ARGS('kz_term', 'to_binary', [?VAR(_Name)]), _Tail)) ->
    'undefined';

key_to_key_path(?MOD_FUN_ARGS('kz_term', 'to_binary', [?VAR(_Name)])) ->
    'undefined';

key_to_key_path(?GEN_FUN_ARGS(_F, _Args)) ->
    'undefined';

key_to_key_path(?LIST(?VAR(_Name), _Tail)) ->
    'undefined';
key_to_key_path(?LIST(Head, Tail)) ->
    case key_to_key_path(Tail) of
        'undefined' -> 'undefined';
        TailV -> [kz_ast_util:binary_match_to_binary(Head), ?FIELD_PROPERTIES | TailV]
    end;
key_to_key_path(?BINARY_MATCH(K)) ->
    try [kz_ast_util:binary_match_to_binary(K)]
    catch 'error':'function_clause' -> 'undefined'
    end.

guess_type('get_list', Default) -> guess_type_by_default(Default);
guess_type('is_true', _Default) -> <<"boolean">>;
guess_type('get_is_true', _Default) -> <<"boolean">>;
guess_type('get_boolean', _Default) -> <<"boolean">>;
guess_type('get', Default) -> guess_type_by_default(Default);
guess_type('get_current', Default) -> guess_type_by_default(Default);
guess_type('fetch', Default) -> guess_type_by_default(Default);
guess_type('get_non_empty', Default) -> guess_type_by_default(Default);
guess_type('get_binary', _Default) -> <<"string">>;
guess_type('get_ne_binary', _Default) -> <<"string">>;
guess_type('get_json', _Default) -> <<"object">>;
guess_type('get_string', _Default) -> <<"string">>;
guess_type('get_integer', _Default) -> <<"integer">>;
guess_type('get_float', _Default) -> <<"number">>;
guess_type('get_atom', _Default) -> <<"string">>;
guess_type('get_global', Default) -> guess_type_by_default(Default);
guess_type('set_default', _Default) -> 'undefined';
guess_type('set', Default) -> guess_type_by_default(Default);
guess_type('set_string', _) -> <<"string">>;
guess_type('set_node', Default) -> guess_type_by_default(Default);
guess_type('update_default', Default) -> guess_type_by_default(Default).

guess_type_by_default('undefined') -> 'undefined';
guess_type_by_default(?ATOM('undefined')) -> 'undefined';
guess_type_by_default(?ATOM('true')) -> <<"boolean">>;
guess_type_by_default(?ATOM('false')) -> <<"boolean">>;
guess_type_by_default(?ATOM(_)) -> <<"string">>;
guess_type_by_default(?VAR(_V)) -> 'undefined';
guess_type_by_default(?EMPTY_LIST) -> <<"array">>;
guess_type_by_default(?LIST(?BINARY_MATCH(_), _Tail)) -> <<"array(string)">>;
guess_type_by_default(?LIST(_Head, _Tail)) -> <<"array">>;
guess_type_by_default(?BINARY_MATCH(_V)) -> <<"string">>;
guess_type_by_default(?INTEGER(_I)) -> <<"integer">>;
guess_type_by_default(?FLOAT(_F)) -> <<"number">>;
guess_type_by_default(?BINARY_OP(_Op, Arg1, _Arg2)) ->
    guess_type_by_default(Arg1);
guess_type_by_default(?MOD_FUN_ARGS('kapps_config', F, [_Cat, _Key])) ->
    guess_type(F, 'undefined');
guess_type_by_default(?MOD_FUN_ARGS('kapps_config', F, [_Cat, _Key, Default |_])) ->
    guess_type(F, Default);
guess_type_by_default(?MOD_FUN_ARGS('kz_json', 'new', [])) -> <<"object">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_json', 'from_list', _Args)) -> <<"object">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_json', 'set_value', [_K, V, _J])) ->
    guess_type_by_default(V);
guess_type_by_default(?MOD_FUN_ARGS('kz_privacy', 'anonymous_caller_id_number', _Args)) -> <<"string">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_privacy', 'anonymous_caller_id_name', _Args)) -> <<"string">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_term', 'to_integer', _Args)) -> <<"integer">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_binary', 'rand_hex', _Args)) -> <<"string">>.

guess_properties(Document, Source, Key, Type, Default)
  when is_binary(Key) ->
    DescriptionKey = description_key(Document, Key),
    Description = fetch_description(DescriptionKey),
    case Description of
        'undefined' ->
            io:format("\nYou need to add the key \"~s\" in ~s\n", [DescriptionKey, ?SYSTEM_CONFIG_DESCRIPTIONS]),
            halt(1);
        _ -> 'ok'
    end,
    kz_json:from_list(
      props:filter_undefined(
        [{<<"type">>, Type}
        ,{?SOURCE, erlang:atom_to_binary(Source, utf8)}
        ,{<<"description">>, Description}
        ,{?FIELD_DEFAULT, try default_value(Default) catch _:_ -> 'undefined' end}
        ]
       )
     );
guess_properties(Document, Source, [Key], Type, Default)
  when is_binary(Key) ->
    guess_properties(Document, Source, Key, Type, Default);
guess_properties(Document, Source, [Key, ?FIELD_PROPERTIES], Type, Default) ->
    guess_properties(Document, Source, Key, Type, Default);
guess_properties(Document, Source, [_Key, ?FIELD_PROPERTIES | Rest], Type, Default) ->
    guess_properties(Document, Source, Rest, Type, Default).

description_key(Document, Key) -> <<Document/binary, $., Key/binary>>.
fetch_description(DescriptionKey) ->
    {'ok', Bin} = file:read_file(?SYSTEM_CONFIG_DESCRIPTIONS),
    kz_json:get_ne_binary_value(DescriptionKey, kz_json:decode(Bin)).

default_value('undefined') -> 'undefined';
default_value(?ATOM('true')) -> 'true';
default_value(?ATOM('false')) -> 'false';
default_value(?ATOM('undefined')) -> 'undefined';
default_value(?ATOM(V)) -> kz_term:to_binary(V);
default_value(?VAR(_)) -> 'undefined';
default_value(?STRING(S)) -> kz_term:to_binary(S);
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
default_value(?MOD_FUN_ARGS('kz_json', 'new', [])) ->
    kz_json:new();
default_value(?MOD_FUN_ARGS('kz_util', 'rand_hex_binary', [_Arg])) ->
    '_system';
default_value(?MOD_FUN_ARGS('kz_privacy', 'anonymous_caller_id_number', _Args)) ->
    default_value(kz_privacy:anonymous_caller_id_number());
default_value(?MOD_FUN_ARGS('kz_privacy', 'anonymous_caller_id_name', _Args)) ->
    default_value(kz_privacy:anonymous_caller_id_name());
default_value(?MOD_FUN_ARGS('kz_term', 'to_binary', [Arg])) ->
    default_value(Arg);
default_value(?MOD_FUN_ARGS('kz_term', 'to_integer', [Arg])) ->
    default_value(Arg);
default_value(?MOD_FUN_ARGS(M, 'type', [])) ->
    default_value(M:type());
default_value(?MOD_FUN_ARGS('ecallmgr_config', 'get', [_Key, Default])) ->
    default_value(Default);
%%TODO: support all kapps_config exports
default_value(?MOD_FUN_ARGS('kapps_config', 'get', [_Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_config', 'get_integer', [_Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_config', 'get_binary', [_Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_account_config', 'get_global', [_Account, _Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_account_config', 'get', [_Account, _Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS(_M, _F, _Args)) ->
    '_system';
default_value(?FUN_ARGS(_F, _Args)) ->
    '_system'.

default_values_from_list(KVs) ->
    lists:foldl(fun default_value_from_kv/2, kz_json:new(), KVs).

default_value_from_kv(KV, Acc) ->
    KVs = props:filter_undefined(default_value(KV)),
    kz_json:set_values(KVs, Acc).
