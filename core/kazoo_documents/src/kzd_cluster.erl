%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2020-, 2600Hz
%%% @doc Accessors for cluster document
%%% @author Kalyan Krause
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_cluster).

-define(MOD_CONFIG_CLUSTER, <<"cluster">>).

-export([id/0, zones/0, zones/1]).

-include("kz_documents.hrl").

-spec id() -> kz_term:ne_binary().
id() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CLUSTER, <<"cluster_id">>, kz_binary:rand_hex(16), <<"default">>).

-spec zones() -> kz_term:api_object().
zones() ->
    kapps_config:get_json(?MOD_CONFIG_CLUSTER, <<"zones">>, kz_json:new(), <<"default">>).

-spec zones(kz_term:text() | kz_json:object()) -> {'ok', kz_json:object()}.
zones(Value) ->
    kapps_config:set_json(?MOD_CONFIG_CLUSTER, <<"zones">>, Value).
