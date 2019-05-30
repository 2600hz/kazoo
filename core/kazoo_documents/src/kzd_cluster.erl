%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc Accessors for cluster document
%%% @author Kalyan Krause
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_cluster).

-define(MOD_CONFIG_CLUSTER, <<"cluster">>).

-export([id/0]).

-include("kz_documents.hrl").

-spec id() -> kz_term:ne_binary().
id() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CLUSTER, <<"cluster_id">>, kz_binary:rand_hex(16), <<"default">>).
