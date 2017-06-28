-module(kapps_config_doc).
-include_lib("kazoo_types/include/kz_types.hrl").
-include_lib("kazoo_types/include/kz_databases.hrl").
-compile({no_auto_import,[get_keys/1]}).
-export([
         get_keys/1
        ,get_config/1
        ,get_node/2
        ,apply_default_values/3
        ,apply_default_node/1, apply_schema_defaults/2, apply_schema_defaults_to_default/2
        ,schema_defaults/1
        ,config_with_defaults/1, config_with_default_node/1
        ,diff_from_default/2
        ,default_config/2, stored_config/2, build_default/2
        ,default_node/2, stored_node/2, diff_node_from_default/3
        ,list_configs/0
        ]).

-define(DEFAULT, <<"default">>).

-spec maybe_new({ok, kz_json:object()}) -> kz_json:object().
maybe_new({ok, JObj}) -> JObj;
maybe_new(_) -> kz_json:new().

-spec get_keys(kz_json:object()) -> [ne_binary()].
get_keys(Config) ->
    kz_json:get_keys(kz_doc:public_fields(Config)) -- [?DEFAULT].

-spec get_config(ne_binary()) -> kz_json:object().
get_config(Id) ->
    JObj = kz_doc:public_fields(maybe_new(kapps_config:get_category(Id))),
    kz_json:filter(fun({_, V}) -> kz_json:is_json_object(V) end, JObj).

-spec get_node(kz_json:object(), ne_binary()) -> kz_json:object().
get_node(Config, Node) ->
    kz_json:get_value(Node, Config, kz_json:new()).

-spec apply_default_values(kz_json:object(), ne_binary(), kz_json:object()) -> kz_json:object().
apply_default_values(Config, Node, Default) ->
    NodeValue = get_node(Config, Node),
    kz_json:set_value(Node, kz_json:merge(Default, NodeValue), Config).

-spec apply_default_node(kz_json:object()) -> kz_json:object().
apply_default_node(Config) ->
    Default = kz_json:get_value(?DEFAULT, Config, kz_json:new()),
    lists:foldl(fun(K,A) -> apply_default_values(A, K, Default) end, Config, get_keys(Config)).

-spec apply_schema_defaults(ne_binary(), kz_json:object()) -> kz_json:object().
apply_schema_defaults(Id, Config) ->
    Default = schema_defaults(Id),
    lists:foldl(fun(K,A) -> apply_default_values(A, K, Default) end, Config, [?DEFAULT|get_keys(Config)]).

-spec apply_schema_defaults_to_default(ne_binary(), kz_json:object()) -> kz_json:object().
apply_schema_defaults_to_default(Id, Config) ->
    apply_default_values(Config, ?DEFAULT, schema_defaults(Id)).

-spec schema_defaults(ne_binary()) -> kz_json:object().
schema_defaults(Id) ->
    kz_json_schema:default_object(kapps_config_util:system_schema(Id)).

-spec maybe_insert_default_node(kz_json:object()) -> kz_json:object().
maybe_insert_default_node(Config) ->
    case kz_json:get_value(?DEFAULT, Config) of
        undefined -> kz_json:set_value(?DEFAULT, kz_json:new(), Config);
        _Else -> Config
    end.

-spec config_with_defaults(ne_binary()) -> kz_json:object().
config_with_defaults(Id) ->
    apply_schema_defaults(Id, maybe_insert_default_node(apply_default_node(get_config(Id)))).

-spec default_config(ne_binary(), [ne_binary()]) -> kz_json:object().
default_config(Id, Keys) ->
    DefaultNode = schema_defaults(Id),
    lists:foldl(fun(Key,A) -> kz_json:set_value(Key, DefaultNode, A) end, kz_json:new(), [?DEFAULT|Keys]).

-spec stored_config(ne_binary(), kz_json:keys()) -> kz_json:object().
stored_config(Id, Keys) ->
    Config = config_with_default_node(Id),
    Default = kz_json:get_value(?DEFAULT, Config, kz_json:new()),
    lists:foldl(fun(K,A) -> apply_default_values(A, K, Default) end, Config, Keys).

-spec config_with_default_node(ne_binary()) -> kz_json:object().
config_with_default_node(Id) ->
    Config = get_config(Id),
    apply_default_values(Config, ?DEFAULT, schema_defaults(Id)).

-spec diff_from_default(ne_binary(), kz_json:object()) -> kz_json:object().
diff_from_default(Id, JObj) ->
    kz_json:diff(JObj, build_default(Id, JObj)).

-spec build_default(ne_binary(), kz_json:object()) -> kz_json:object().
build_default(Id, JObj) ->
    Default = kz_json:get_value(?DEFAULT, JObj, kz_json:new()),
    Config = lists:foldl(fun(K,A) -> apply_default_values(A, K, Default) end, kz_json:new(), get_keys(JObj)),
    apply_schema_defaults(Id, Config).

%% ----------------------------------------------------------
%% Node API
%% ----------------------------------------------------------

-spec default_node(ne_binary(), ne_binary()) -> kz_json:object().
default_node(Id, ?DEFAULT) ->
    schema_defaults(Id);
default_node(Id, _Node) ->
    stored_node(Id, ?DEFAULT).

-spec stored_node(ne_binary(), ne_binary()) -> kz_json:object().
stored_node(Id, Node) ->
    get_node(stored_config(Id, [Node]), Node).

-spec diff_node_from_default(ne_binary(), ne_binary(), kz_json:object()) -> kz_json:object().
diff_node_from_default(Id, ?DEFAULT, JObj) ->
    kz_json:diff(JObj, schema_defaults(Id));
diff_node_from_default(Id, _Node, JObj) ->
    kz_json:diff(JObj, stored_node(Id, ?DEFAULT)).

-spec list_configs() -> [ne_binary()].
list_configs() ->
    case kz_datamgr:get_results(?KZ_CONFIG_DB, <<"system_configs/crossbar_listing">>, []) of
        {ok, List} -> [ kz_json:get_value(<<"id">>, JSON) || JSON <- List ];
        _ -> []
    end.
