%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author Roman Galeev
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_config_doc).

-export([get_keys/1
        ,get_config/1
        ,get_node/2
        ,apply_default_values/3
        ,apply_default_node/1, apply_schema_defaults/2, apply_schema_defaults_to_default/2
        ,schema_defaults/1
        ,config_with_defaults/1, config_with_default_node/1
        ,diff_from_default/2
        ,default_config/2, stored_config/2
        ,build_default/1, build_default/2
        ,default_node/2, stored_node/2, diff_node_from_default/3
        ,list_configs/0
        ,node_config/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-compile({'no_auto_import', [get_keys/1]}).

-define(DEFAULT, <<"default">>).

-spec maybe_new({'ok', kzd_system_configs:doc()}) -> kzd_system_configs:doc().
maybe_new({'ok', JObj}) -> JObj;
maybe_new(_) -> kzd_system_configs:new().

-spec get_keys(kzd_system_configs:doc()) -> kz_term:ne_binaries().
get_keys(Config) ->
    kz_json:get_keys(kz_doc:public_fields(Config)) -- [?DEFAULT].

-spec get_config(kz_term:ne_binary()) -> kzd_system_configs:doc().
get_config(Id) ->
    SystemConfig = kz_doc:public_fields(maybe_new(kapps_config:fetch_category(Id))),
    kz_json:filter(fun({_, V}) -> kz_json:is_json_object(V) end, SystemConfig).

-spec get_node(kzd_system_configs:doc(), kz_term:ne_binary()) -> kzd_system_configs:node_config().
get_node(Config, Node) ->
    kzd_system_configs:node(Config, Node, kz_json:new()).

-spec apply_default_values(kzd_system_configs:doc(), kz_term:ne_binary(), kzd_system_configs:doc()) ->
                                  kzd_system_configs:doc().
apply_default_values(Config, Node, Defaults) ->
    NodeSection = get_node(Config, Node),
    DefaultSection = get_node(Config, ?DEFAULT),

    DefaultsSection = get_node(Defaults, ?DEFAULT),

    Merged = kz_json:merge_recursive([DefaultsSection, DefaultSection, NodeSection]),
    kz_json:set_value(Node, Merged, Config).

-spec apply_default_node(kzd_system_configs:doc()) -> kzd_system_configs:doc().
apply_default_node(Config) ->
    lists:foldl(fun(NodeOrZoneKey, ConfigAcc) ->
                        apply_default_values(ConfigAcc, NodeOrZoneKey, kz_json:new())
                end
               ,Config
               ,get_keys(Config)
               ).

-spec apply_schema_defaults(kz_term:ne_binary(), kzd_system_configs:doc()) ->
                                   kzd_system_configs:doc().
apply_schema_defaults(Id, Config) ->
    SchemaDefaults = schema_defaults(Id),

    lists:foldl(fun(NodeOrZoneKey, ConfigAcc) ->
                        apply_default_values(ConfigAcc, NodeOrZoneKey, SchemaDefaults)
                end
               ,Config
               ,[?DEFAULT|get_keys(Config)]
               ).

-spec apply_schema_defaults_to_default(kz_term:ne_binary(), kzd_system_configs:doc()) ->
                                              kzd_system_configs:doc().
apply_schema_defaults_to_default(Id, Config) ->
    SchemaDefaults = schema_defaults(Id),
    apply_default_values(Config, ?DEFAULT, SchemaDefaults).

-spec schema_defaults(kz_term:ne_binary()) -> kzd_system_configs:doc().
schema_defaults(Id) ->
    SchemaJObj = kapps_config_util:system_config_document_schema(Id),
    kz_json_schema:default_object(SchemaJObj).

-spec maybe_insert_default_node(kzd_system_configs:doc()) -> kzd_system_configs:doc().
maybe_insert_default_node(Config) ->
    kz_json:insert_value(?DEFAULT, kz_json:new(), Config).

-spec config_with_defaults(kz_term:ne_binary()) -> kzd_system_configs:doc().
config_with_defaults(Id) ->
    ConfigJObj = get_config(Id),
    WithDefaults = apply_default_node(ConfigJObj),
    WithDefaultNode = maybe_insert_default_node(WithDefaults),

    apply_schema_defaults(Id, WithDefaultNode).

-spec default_config(kz_term:ne_binary(), kz_term:ne_binaries()) -> kzd_system_configs:doc().
default_config(Id, Keys) ->
    DefaultNode = schema_defaults(Id),
    lists:foldl(fun(Key, ConfigsAcc) ->
                        kz_json:set_value(Key, DefaultNode, ConfigsAcc)
                end
               ,kzd_system_configs:new()
               ,[?DEFAULT|Keys]
               ).

-spec node_config(kz_term:ne_binary(), kz_term:ne_binary()) -> kzd_system_configs:node_config().
node_config(Id, Node) ->
    Config = config_with_default_node(Id),
    NodeConfig = get_node(Config, Node),
    DefaultConfig = get_node(Config, ?DEFAULT),
    kz_json:merge_recursive(DefaultConfig, NodeConfig).

-spec stored_config(kz_term:ne_binary(), kz_json:key() | kz_json:keys()) -> kzd_system_configs:doc().
stored_config(Id, Node)
  when is_binary(Node) ->
    stored_config(Id, [Node]);
stored_config(Id, Nodes) ->
    Config = config_with_default_node(Id),
    lists:foldl(fun(Node, ConfigAcc) ->
                        apply_default_values(ConfigAcc, Node, Config)
                end
               ,Config
               ,Nodes
               ).

-spec config_with_default_node(kz_term:ne_binary() | kz_json:object()) -> kz_json:object().
config_with_default_node(?NE_BINARY = Id) ->
    Config = get_config(Id),
    SysConfigDefaults = schema_defaults(Id),
    apply_default_values(Config, ?DEFAULT, SysConfigDefaults);
config_with_default_node(JObj) ->
    SysConfigDefaults = schema_defaults(kz_doc:id(JObj)),
    apply_default_values(JObj, ?DEFAULT, SysConfigDefaults).

-spec diff_from_default(kz_term:ne_binary(), kzd_system_configs:doc()) -> kz_json:object().
diff_from_default(Id, Config) ->
    kz_json:diff(Config, build_default(Id, Config)).

-spec build_default(kz_term:ne_binary(), kz_json:object()) -> kzd_system_configs:doc().
build_default(Id, Config) ->
    UpdatedConfig = lists:foldl(fun(Node, ConfigAcc) ->
                                        apply_default_values(ConfigAcc, Node, Config)
                                end
                               ,kz_json:new()
                               ,get_keys(Config)
                               ),
    apply_schema_defaults(Id, UpdatedConfig).

-spec build_default(kz_term:ne_binary()) -> kzd_system_configs:doc().
build_default(Id) ->
    kz_json_schema:default_object(kapps_config_util:system_config_document_schema(Id)).

%%%=============================================================================
%%% Node API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec default_node(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
default_node(Id, ?DEFAULT) ->
    schema_defaults(Id);
default_node(Id, _Node) ->
    stored_node(Id, ?DEFAULT).

-spec stored_node(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
stored_node(Id, Node) ->
    get_node(stored_config(Id, [<<"default">>, Node]), Node).

-spec diff_node_from_default(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
diff_node_from_default(Id, ?DEFAULT, JObj) ->
    kz_json:diff(JObj, schema_defaults(Id));
diff_node_from_default(Id, _Node, JObj) ->
    kz_json:diff(JObj, stored_node(Id, ?DEFAULT)).

-spec list_configs() -> kz_term:ne_binaries().
list_configs() ->
    case kz_datamgr:get_results(?KZ_CONFIG_DB, <<"system_configs/crossbar_listing">>, []) of
        {'ok', List} -> [kz_doc:id(JSON) || JSON <- List];
        _ -> []
    end.
