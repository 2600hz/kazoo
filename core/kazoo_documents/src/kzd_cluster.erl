%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019, 2600Hz
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
    case kapps_config:get_ne_binary_value(?MOD_CONFIG_CLUSTER, <<"cluster_id">>) of
        'undefined' ->
            ClusterId = kz_binary:rand_hex(16),
            {'ok', _JObj} = kapps_config:set_default(?MOD_CONFIG_CLUSTER, <<"cluster_id">>, ClusterId),
            ClusterId;
        ClusterId -> ClusterId
    end.
