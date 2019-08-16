%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc Generate schema for Account and System configurations.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_config_usage).

-export([process_project/0, process_app/1
        ,to_schema_docs/0, to_schema_docs/1

        ,expression_to_schema/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").


-define(DEBUG(_Fmt, _Args), 'ok').
%%-define(DEBUG(Fmt, Args), io:format([$~, $p, $  | Fmt], [?LINE | Args])).

-define(SOURCE, <<"config_usage_source">>).
-define(FIELD_DEFAULT, <<"default">>).
-define(FIELD_PROPERTIES, <<"properties">>).
-define(FIELD_TYPE, <<"type">>).
-define(SYSTEM_CONFIG_DESCRIPTIONS, kz_ast_util:api_path(<<"descriptions.system_config.json">>)).
-define(UNKNOWN_DEFAULT, 'undefined').

-type acc() :: #{'schema_dir' := 'default' | file:filename_all()
                ,'app_schemas' := kz_json:object()
                ,'project_schemas' := kz_json:object()
                }.

-spec to_schema_docs() -> 'ok'.
to_schema_docs() ->
    kz_json:foreach(fun update_schema/1, process_project()).

-spec to_schema_docs(atom()) -> 'ok'.
to_schema_docs(App) ->
    kz_json:foreach(fun update_schema/1, process_app(App)).

-spec update_schema({file:filename_all(), kz_json:json_term()}) -> 'ok'.
update_schema({PrivDir, Schemas}) ->
    ?DEBUG("adding schemas to priv dir ~s~n~p~n", [PrivDir, Schemas]),

    kz_json:foreach(fun({Name, AutoGenSchema}) ->
                            update_schema(Name, AutoGenSchema, PrivDir)
                    end
                   ,Schemas
                   ).

-spec update_schema(kz_json:key(), kz_json:json_object(), file:filename_all()) -> 'ok'.
update_schema(Name, AutoGenSchema, PrivDir) ->
    ?DEBUG("~s detected ~p~n", [Name, AutoGenSchema]),
    AccountSchema = account_properties(AutoGenSchema),
    ?DEBUG("account schema: ~p~n", [AccountSchema]),
    _ = kz_json:new() =/= AccountSchema
        andalso update_schema(Name, AccountSchema, PrivDir, <<"account_config">>),
    update_schema(Name, AutoGenSchema, PrivDir, <<"system_config">>).

-spec update_schema(kz_term:ne_binary(), kz_json:object(), file:filename_all(), kz_term:ne_binary()) -> 'ok'.
update_schema(Name, AutoGenSchema, PrivDir, ConfigType) ->
    Path = kz_ast_util:schema_path(<<ConfigType/binary, ".", Name/binary, ".json">>, PrivDir),

    ?DEBUG("autogen: ~p~n", [AutoGenSchema]),

    GeneratedJObj = static_fields(ConfigType, Name, remove_source(AutoGenSchema)),

    ?DEBUG("gen: ~p~n", [GeneratedJObj]),

    Existing = existing_schema(Path),

    ?DEBUG("existing: ~p~n", [Existing]),

    MergedJObj = kz_json:merge(fun kz_json:merge_left/2, Existing, GeneratedJObj),

    ?DEBUG("merged: ~p~n", [MergedJObj]),

    UpdatedSchema = kz_json:delete_key(<<"id">>, MergedJObj),
    ?DEBUG("  ensuring ~s exists~n", [Path]),
    'ok' = filelib:ensure_dir(Path),
    ?DEBUG("  writing schema for ~s: ~s~n", [Name, kz_json:encode(UpdatedSchema)]),
    'ok' = file:write_file(Path, kz_json:encode(UpdatedSchema)).

-spec existing_schema(file:filename_all()) -> kz_json:object().
existing_schema(Name) ->
    case kz_json_schema:fload(Name) of
        {'ok', JObj} -> JObj;
        {'error', _E} ->
            ?DEBUG("failed to find ~s: ~p~n", [Name, _E]),
            kz_json:new()
    end.

-spec account_properties(kz_json:object()) -> kz_json:object().
account_properties(AutoGenSchema) ->
    Flat = kz_json:to_proplist(kz_json:flatten(AutoGenSchema)),
    KeepPaths = sets:from_list(
                  [[P1, P2]
                   || {[P1, P2 | _]=Path, V} <- Flat,
                      ?SOURCE =:= lists:last(Path),
                      V =:= 'kapps_account_config'
                  ]),
    kz_json:expand(
      kz_json:from_list(
        [KV
         || {[P1, P2 | _],_}=KV <- Flat,
            sets:is_element([P1, P2], KeepPaths)
        ])).

-spec remove_source(kz_json:object()) -> kz_json:object().
remove_source(JObj) ->
    F = fun ({K, _}) -> not lists:member(?SOURCE, K) end,
    Filtered = kz_json:filter(F, kz_json:flatten(JObj)),
    kz_json:expand(Filtered).

static_fields(ConfigType, Name, JObj) ->
    Id = <<ConfigType/binary, ".", Name/binary>>,
    Values = [{<<"description">>, <<"Schema for ", Name/binary, " ", ConfigType/binary>>}
             ,{<<"$schema">>, <<"http://json-schema.org/draft-04/schema#">>}
             ,{?FIELD_TYPE, <<"object">>}
             ],
    kz_json:set_values(Values, kz_doc:set_id(JObj, Id)).

-spec process_project() -> kz_json:object().
process_project() ->
    io:format("processing kapps_config/kapps_account_config usage: "),
    Options = [{'expression', fun expression_to_schema/2}
              ,{'module', fun print_dot/2}
              ,{'accumulator', new_acc()}
              ,{'application', fun add_app_config/2}
              ,{'after_application', fun add_schemas_to_bucket/2}
              ],
    #{'project_schemas' := Usage} = kazoo_ast:walk_project(Options),
    io:format(" done~n"),
    Usage.

-spec process_app(atom()) -> kz_json:object().
process_app(App) ->
    Options = [{'expression', fun expression_to_schema/2}
              ,{'module', fun print_dot/2}
              ,{'accumulator', new_acc()}
              ,{'application', fun add_app_config/2}
              ,{'after_application', fun add_schemas_to_bucket/2}
              ],
    #{'project_schemas' := Usage} = kazoo_ast:walk_app(App, Options),
    Usage.

print_dot('pqc_cb_system_configs', Acc) ->
    {'skip', Acc};
print_dot(_Module, Acc) ->
    io:format("."),
    Acc.

-spec new_acc() -> acc().
new_acc() ->
    #{'schema_dir' => kz_ast_util:default_schema_priv_dir()
     ,'app_schemas' => kz_json:new() %% SchemaName => SchemaProperties
     ,'project_schemas' => kz_json:from_list([{kz_ast_util:default_schema_priv_dir(), kz_json:new()}]) %% Path => Schemas
     }.

-spec add_app_config(atom(), acc()) -> acc().
add_app_config(App, Acc) ->
    confirm_loaded(application:load(App)),
    case application:get_env(App, 'schemas_to_priv') of
        {'ok', 'true'} ->
            ?DEBUG("detected schemas will go in ~s/priv~n", [code:priv_dir(App)]),
            Acc#{schema_dir => kz_term:to_binary(code:priv_dir(App))};
        _E ->
            ?DEBUG("app ~s using default priv (~p)~n", [App, _E]),
            Acc#{schema_dir => kz_ast_util:default_schema_priv_dir()}
    end.

confirm_loaded('ok') -> 'ok';
confirm_loaded({'error',{'already_loaded',_App}}) -> 'ok'.

add_schemas_to_bucket(_App, #{schema_dir := PrivDir
                             ,app_schemas := AppSchemas
                             ,project_schemas := ProjectSchemas
                             }=Acc) ->
    ProjectSchema = kz_json:get_json_value(PrivDir, ProjectSchemas, kz_json:new()),

    ?DEBUG("merging dir ~s / app ~p into proj ~p~n", [PrivDir, AppSchemas, ProjectSchema]),

    UpdatedSchema = kz_json:merge(ProjectSchema, AppSchemas),
    ?DEBUG("merged: ~p~n", [UpdatedSchema]),

    Acc#{app_schemas => kz_json:new()
        ,project_schemas => kz_json:set_value(PrivDir, UpdatedSchema, ProjectSchemas)
        }.

-spec expression_to_schema(any(), kz_json:object()) -> kz_json:object().
expression_to_schema(?MOD_FUN_ARGS('kapps_config', 'set', _), Schemas) ->
    Schemas;
expression_to_schema(?MOD_FUN_ARGS('kapps_config', 'set_default', _), Schemas) ->
    Schemas;
expression_to_schema(?MOD_FUN_ARGS('kapps_config', 'set_node', _), Schemas) ->
    Schemas;
expression_to_schema(?MOD_FUN_ARGS(Source='kapps_config', F, Args), Schemas) ->
    config_to_schema(Source, F, Args, Schemas);
expression_to_schema(?MOD_FUN_ARGS(Source='kapps_account_config', F='get_global', Args), Schemas) ->
    config_to_schema(Source, F, Args, Schemas);
expression_to_schema(?MOD_FUN_ARGS(Source='kapps_account_config', F='get_hierarchy', Args), Schemas) ->
    config_to_schema(Source, F, Args, Schemas);
expression_to_schema(?MOD_FUN_ARGS(Source='kapps_account_config', F='get_with_strategy', Args), Schemas) ->
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
config_to_schema(_, 'fetch_category', _Args, Schemas) ->
    Schemas;
config_to_schema(Source, F='get_global', [Account, Cat, K], Schemas) ->
    config_to_schema(Source, F, [Account, Cat, K, 'undefined'], Schemas);
config_to_schema(Source, F='get_global', [_Account, Cat, K, Default], Schemas) ->
    Document = category_to_document(Cat),
    case key_to_key_path(K) of
        'undefined' -> Schemas;
        Key -> config_key_to_schema(Source, F, Document, Key, Default, Schemas)
    end;
config_to_schema(Source, F='get_hierarchy', [Account, Cat, K], Schemas) ->
    config_to_schema(Source, F, [Account, Cat, K, 'undefined'], Schemas);
config_to_schema(Source, F='get_hierarchy', [_Account, Cat, K, Default], Schemas) ->
    Document = category_to_document(Cat),
    case key_to_key_path(K) of
        'undefined' -> Schemas;
        Key -> config_key_to_schema(Source, F, Document, Key, Default, Schemas)
    end;
config_to_schema(Source, F='get_with_strategy', [Strategy, Account, Cat, K], Schemas) ->
    config_to_schema(Source, F, [Strategy, Account, Cat, K, 'undefined'], Schemas);
config_to_schema(Source, F='get_with_strategy', [?BINARY_MATCH([?BINARY_STRING(Strategy)]), _Account, Cat, K, Default], Schemas)
  when Strategy =:= "hierarchy_merge";
       Strategy =:= "global";
       Strategy =:= "global_merge" ->
    Document = category_to_document(Cat),
    case key_to_key_path(K) of
        'undefined' -> Schemas;
        Key -> config_key_to_schema(Source, F, Document, Key, Default, Schemas)
    end;
config_to_schema(_, 'get_with_strategy', _Args, Schemas) ->
    Schemas;
config_to_schema(Source, F, [Cat, K], Schemas) ->
    config_to_schema(Source, F, [Cat, K, 'undefined'], Schemas);
config_to_schema(Source, F, [Cat, K, Default, _Node], Schemas) ->
    config_to_schema(Source, F, [Cat, K, Default], Schemas);
config_to_schema(Source, F, [Cat, K, Default], Schemas) ->
    Document = category_to_document(Cat),

    case key_to_key_path(K) of
        'undefined' -> Schemas;
        Key ->
            config_key_to_schema(Source, F, Document, Key, Default, Schemas)
    end.

config_key_to_schema(_Source, _F, 'undefined', _Key, _Default, Schemas) ->
    Schemas;
config_key_to_schema(Source, F, Document, Key, Default, #{app_schemas := Schemas}=Acc) ->
    Properties = guess_properties(Document, Source, Key, guess_type(F, Default), Default),
    Path = [Document, ?FIELD_PROPERTIES | Key],

    ?DEBUG("setting ~s from source ~s~n", [Path, Source]),
    Acc#{app_schemas => kz_json:set_value(Path, Properties, Schemas)}.

category_to_document(?VAR(_)) -> 'undefined';
category_to_document(Cat) ->
    kz_ast_util:binary_match_to_binary(Cat).

key_to_key_path(?ATOM(A)) -> [kz_term:to_binary(A)];
key_to_key_path(?VAR(_)) -> 'undefined';
key_to_key_path(?EMPTY_LIST) -> [];
key_to_key_path(?LIST(?MOD_FUN_ARGS('kapps_config', _F, [_Doc, Field | _]), ?EMPTY_LIST)) ->
    [kz_ast_util:binary_match_to_binary(Field)];
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
key_to_key_path(?LIST(Head, ?EMPTY_LIST)) ->
    [kz_ast_util:binary_match_to_binary(Head)];
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
guess_type('is_true', _) -> <<"boolean">>;
guess_type('get_is_true', _) -> <<"boolean">>;
guess_type('get_is_false', _) -> <<"boolean">>;
guess_type('get_boolean', _) -> <<"boolean">>;
guess_type('get', Default) -> guess_type_by_default(Default);
guess_type('get_current', Default) -> guess_type_by_default(Default);
guess_type('fetch', Default) -> guess_type_by_default(Default);
guess_type('fetch_current', Default) -> guess_type_by_default(Default);
guess_type('get_binary', _) -> <<"string">>;
guess_type('get_ne_binary', _) -> <<"string">>;
guess_type('get_ne_binaries', _) -> [<<"string">>];
guess_type('get_pos_integer', _) -> <<"pos_integer">>;
guess_type('get_non_neg_integer', _) -> <<"non_neg_integer">>;
guess_type('get_ne_binary_or_ne_binaries', _) -> {<<"string">>, [<<"string">>]};
guess_type('get_json', _) -> <<"object">>;
guess_type('get_jsons', _) -> [<<"object">>];
guess_type('get_string', _) -> <<"string">>;
guess_type('get_integer', _) -> <<"integer">>;
guess_type('get_float', _) -> <<"float">>;
guess_type('get_atom', _) -> <<"string">>;
guess_type('get_global', Default) -> guess_type_by_default(Default);
guess_type('get_hierarchy', Default) -> guess_type_by_default(Default);
guess_type('get_with_strategy', Default) -> guess_type_by_default(Default);
guess_type('set_default', _) -> 'undefined';
guess_type('set', Default) -> guess_type_by_default(Default);
guess_type('set_string', _) -> <<"string">>;
guess_type('set_json', _) -> <<"object">>;
guess_type('set_node', Default) -> guess_type_by_default(Default);
guess_type('update_default', Default) -> guess_type_by_default(Default).

guess_type_by_default('undefined') -> 'undefined';
guess_type_by_default(?ATOM('undefined')) -> 'undefined';
guess_type_by_default(?ATOM('true')) -> <<"boolean">>;
guess_type_by_default(?ATOM('false')) -> <<"boolean">>;
guess_type_by_default(?ATOM(_)) -> <<"string">>;
guess_type_by_default(?VAR(_V)) -> 'undefined';
guess_type_by_default(?EMPTY_LIST) -> <<"array">>;
guess_type_by_default(?LIST(?BINARY_MATCH(_), {nil, _})) -> [<<"string">>];
guess_type_by_default(?LIST(Head, {nil, _})) -> [guess_type_by_default(Head)];
guess_type_by_default(?LIST(Head, Tail)) ->
    H = [guess_type_by_default(Head)],
    T = guess_type_by_default(Tail),
    case H =:= T of
        'true' -> H;
        'false' -> ['undefined']
    end;
guess_type_by_default(?BINARY_MATCH(_V)) -> <<"string">>;
guess_type_by_default(?INTEGER(_I)) -> <<"integer">>;
guess_type_by_default(?FLOAT(_F)) -> <<"float">>;
guess_type_by_default(?BINARY_OP(_Op, Arg1, _Arg2)) ->
    guess_type_by_default(Arg1);
guess_type_by_default(?MOD_FUN_ARGS(M, F, [_Cat, _Key]))
  when M =:= kapps_config;
       M =:= kapps_account_config ->
    guess_type(F, 'undefined');
guess_type_by_default(?MOD_FUN_ARGS(M, F, [_Cat, _Key, Default |_]))
  when M =:= kapps_config;
       M =:= kapps_account_config ->
    guess_type(F, Default);
guess_type_by_default(?MOD_FUN_ARGS('kz_json', 'new', [])) -> <<"object">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_json', 'from_list', _Args)) -> <<"object">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_json', 'from_list_recursive', _Args)) -> <<"object">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_json', 'set_value', [_K, V, _J])) ->
    guess_type_by_default(V);
guess_type_by_default(?MOD_FUN_ARGS('kz_privacy', 'anonymous_caller_id_number', _Args)) -> <<"string">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_privacy', 'anonymous_caller_id_name', _Args)) -> <<"string">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_account', 'type', [])) -> <<"string">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_term', 'to_integer', _Args)) -> <<"integer">>;
guess_type_by_default(?MOD_FUN_ARGS('kz_binary', 'rand_hex', _Args)) -> <<"string">>;
guess_type_by_default(?MOD_FUN_ARGS(_Mod, 'type', [])) -> <<"string">>.

guess_properties(Document, SourceModule, Key=?NE_BINARY, Type, Default) ->
    DescriptionKey = description_key(Document, Key),

    Description =
        case fetch_description(DescriptionKey) of
            'undefined' ->
                kz_binary:join(binary:split(DescriptionKey, <<".">>, ['global']), <<" ">>);
            D -> D
        end,
    kz_json:from_list(
      [{?SOURCE, SourceModule}
      ,{<<"description">>, Description}
      ,{?FIELD_DEFAULT, try default_value(Default) catch _:_ -> 'undefined' end}
       | type(Type)
      ]);

guess_properties(Document, Source, [Key], Type, Default)
  when is_binary(Key) ->
    guess_properties(Document, Source, Key, Type, Default);
guess_properties(Document, Source, [_Key, ?FIELD_PROPERTIES|_]=Keys, Type, Default) ->
    JustKeys = [K || K <- Keys, ?FIELD_PROPERTIES =/= K],
    guess_properties(Document, Source, kz_binary:join(JustKeys, $.), Type, Default).

type(['undefined']) ->
    [{?FIELD_TYPE, <<"array">>}];
type({Type, [OrArrayType]}) ->
    [{<<"oneOf">>
     ,[kz_json:from_list([{?FIELD_TYPE, Type}])
      ,kz_json:from_list([{?FIELD_TYPE, <<"array">>}
                         ,{<<"items">>, kz_json:from_list([{?FIELD_TYPE, OrArrayType}])}
                         ])
      ]
     }
    ];
type([Type]) ->
    [{?FIELD_TYPE, <<"array">>}
    ,{<<"items">>, kz_json:from_list([{?FIELD_TYPE, Type}])}
    ];
type(<<"pos_integer">>) ->
    [{?FIELD_TYPE, <<"integer">>}
    ,{<<"minimum">>, 1}
    ];
type(<<"non_neg_integer">>) ->
    [{?FIELD_TYPE, <<"integer">>}
    ,{<<"minimum">>, 0}
    ];
type(<<"float">>) -> [{?FIELD_TYPE, <<"number">>}];
type(Type) -> [{?FIELD_TYPE, Type}].

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
    ?UNKNOWN_DEFAULT;
default_value(?MOD_FUN_ARGS('kz_privacy', 'anonymous_caller_id_number', _Args)) ->
    kz_privacy:anonymous_caller_id_number();
default_value(?MOD_FUN_ARGS('kz_privacy', 'anonymous_caller_id_name', _Args)) ->
    kz_privacy:anonymous_caller_id_name();
default_value(?MOD_FUN_ARGS('kz_term', 'to_binary', [Arg])) ->
    default_value(Arg);
default_value(?MOD_FUN_ARGS('kz_term', 'to_integer', [Arg])) ->
    default_value(Arg);
default_value(?MOD_FUN_ARGS(M, 'type', [])) ->
    default_value(M:type());
%%TODO: support all kapps_config exports
default_value(?MOD_FUN_ARGS('kapps_config', 'get', [_Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_config', 'get_integer', [_Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_config', 'get_binary', [_Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_config', 'get_ne_binary', [_Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_config', 'get_ne_binaries', [_Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_config', 'get_jsons', [_Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_account_config', 'get_global', [_Account, _Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_account_config', 'get', [_Account, _Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_account_config', 'get_ne_binary', [_Account, _Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS('kapps_account_config', 'get_ne_binaries', [_Account, _Category, _Key, Default])) ->
    default_value(Default);
default_value(?MOD_FUN_ARGS(_M, _F, _Args)) ->
    ?UNKNOWN_DEFAULT;
default_value(?FUN_ARGS(_F, _Args)) ->
    ?UNKNOWN_DEFAULT.

default_values_from_list(KVs) ->
    lists:foldl(fun default_value_from_kv/2, kz_json:new(), KVs).

default_value_from_kv(KV, Acc) ->
    KVs = props:filter_undefined(default_value(KV)),
    kz_json:set_values(KVs, Acc).
