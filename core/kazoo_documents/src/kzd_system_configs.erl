%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Accessors for `system_configs' document.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_system_configs).

-export([new/0]).
-export([default/1, default/2, set_default/2]).
-export([node/2, node/3, set_node/3]).
-export([zone/2, zone/3, set_zone/3]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type node_config() :: kz_json:object().
-export_type([doc/0
             ,node_config/0
             ]).

-define(SCHEMA, <<"system_configs">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec default(doc()) -> node_config() | 'undefined'.
default(Doc) ->
    default(Doc, 'undefined').

-spec default(doc(), Default) -> node_config() | Default.
default(Doc, Default) ->
    kz_json:get_json_value([<<"default">>], Doc, Default).

-spec set_default(doc(), node_config()) -> doc().
set_default(Doc, Default) ->
    kz_json:set_value([<<"default">>], Default, Doc).

-spec node(doc(), kz_json:key()) -> node_config() | 'undefined'.
node(Doc, Node) ->
    node(Doc, Node, 'undefined').

-spec node(doc(), kz_json:key(), Default) -> node_config() | Default.
node(Doc, Node, Default) ->
    kz_json:get_json_value([Node], Doc, Default).

-spec set_node(doc(), kz_json:key(), node_config()) -> doc().
set_node(Doc, Node, Value) ->
    kz_json:set_value([Node], Value, Doc).

-spec zone(doc(), kz_json:key()) -> node_config() | 'undefined'.
zone(Doc, Zone) ->
    zone(Doc, Zone, 'undefined').

-spec zone(doc(), kz_json:key(), Default) -> node_config() | Default.
zone(Doc, Zone, Default) ->
    kz_json:get_json_value([Zone], Doc, Default).

-spec set_zone(doc(), kz_json:key(), node_config()) -> doc().
set_zone(Doc, Zone, Value) ->
    kz_json:set_value([Zone], Value, Doc).
