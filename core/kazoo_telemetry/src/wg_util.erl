%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(wg_util).

-export([activation_ts/0
        ,cluster_id/0
        ,days_remaining/0
        ]).

-include("waveguide.hrl").

%%------------------------------------------------------------------------------
%% @doc return cluster_id, anonymized if configured.
%% @end
%%------------------------------------------------------------------------------
-spec activation_ts() -> non_neg_integer() | 'undefined'.
activation_ts() -> ?WG_ACTIVATION.

%%------------------------------------------------------------------------------
%% @doc return cluster_id, anonymized if configured.
%% @end
%%------------------------------------------------------------------------------
-spec cluster_id() -> kz_term:ne_binary().
cluster_id() ->
    maybe_anonymize_cluster().

%%------------------------------------------------------------------------------
%% @doc return days remaining before automatic activation
%% @end
%%------------------------------------------------------------------------------
-spec days_remaining() -> non_neg_integer().
days_remaining() ->
    ActSecs  = kz_time:elapsed_s(?WG_ACTIVATION),
    Remaining = (?WG_GRACE_PERIOD - ActSecs) / ?DAY_IN_SECONDS,
    days_remaining(Remaining).

-spec days_remaining(float()) -> non_neg_integer().
days_remaining(Days) when Days > 0 ->
    trunc(Days);
days_remaining(_) -> 0.

%%------------------------------------------------------------------------------
%% @doc maybe anonymize cluster metadata
%% @end
%%------------------------------------------------------------------------------
-spec maybe_anonymize_cluster() -> kz_term:ne_binary().
maybe_anonymize_cluster() ->
    maybe_anonymize_cluster(kzd_cluster:id()).

-spec maybe_anonymize_cluster(kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_anonymize_cluster(ClusterId) ->
    MaybeAnonymize = kapps_config:get_boolean(?TELEMETRY_CAT
                                             ,<<"cluster_id_anonymized">>
                                             ,?ANONYMIZE_CLUSTER
                                             ,<<"default">>),
    maybe_anonymize_cluster(ClusterId, MaybeAnonymize).

%%------------------------------------------------------------------------------
%% @doc anonymize cluster_id
%% @end
%%------------------------------------------------------------------------------
-spec maybe_anonymize_cluster(kz_term:ne_binary(), boolean()) -> kz_term:ne_binary().
maybe_anonymize_cluster(ClusterId, 'true') -> ?ANONYMIZE(ClusterId);
maybe_anonymize_cluster(ClusterId, _) -> ClusterId.
