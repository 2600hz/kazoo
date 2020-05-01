%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_config).

-export([should_permanently_delete/0
        ,released_state/0
        ,locality_url/0
        ,should_age/0
        ,should_force_outbound/0
        ,should_force_local_outbound/0
        ,should_force_port_in_outbound/0
        ,should_force_port_out_outbound/0
        ,should_fetch_account_from_ports/0
        ]).

-include("knm.hrl").

-define(LOCALITY_CONFIG_CAT, <<"number_manager.locality">>).

-define(DEFAULT_LOCALITY_URL
       ,kapps_config:get_ne_binary(<<"number_manager.other">>, <<"phonebook_url">>)
       ).

-spec should_age() -> boolean().
should_age() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"should_age">>, 'false').

-spec should_permanently_delete() -> boolean().
should_permanently_delete() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"should_permanently_delete">>, 'false').

-spec released_state() -> kz_term:ne_binary().
released_state() ->
    kapps_config:get_ne_binary(?KNM_CONFIG_CAT, <<"released_state">>, ?NUMBER_STATE_AVAILABLE).

-spec locality_url() -> kz_term:api_ne_binary().
locality_url() ->
    kapps_config:get_ne_binary(?LOCALITY_CONFIG_CAT, <<"url">>, ?DEFAULT_LOCALITY_URL).

-spec should_force_outbound() -> boolean().
should_force_outbound() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"default_force_outbound">>, 'false').

-spec should_force_local_outbound() -> boolean().
should_force_local_outbound() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_local_outbound">>, 'true').

-spec should_force_port_in_outbound() -> boolean().
should_force_port_in_outbound() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_port_in_outbound">>, 'true').

-spec should_force_port_out_outbound() -> boolean().
should_force_port_out_outbound() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"force_port_out_outbound">>, 'true').

-spec should_fetch_account_from_ports() -> boolean().
should_fetch_account_from_ports() ->
    kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"fetch_account_from_ports">>, 'true').
