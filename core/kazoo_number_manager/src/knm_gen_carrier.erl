%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc When implementing carrier modules, these callbacks are a must!
%%% @author Karl Anderson
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_gen_carrier).

-include("knm.hrl").

-callback find_numbers(kz_term:ne_binary(), pos_integer(), knm_carriers:options()) ->
    {'ok', knm_number:knm_numbers()} |
    {'bulk', knm_number:knm_numbers()} |
    {'error', any()}.

-callback acquire_number(knm_number:knm_number()) ->
    knm_number:knm_number().

-callback disconnect_number(knm_number:knm_number()) ->
    knm_number:knm_number().

-callback should_lookup_cnam() ->
    boolean().

-callback is_number_billable(knm_phone_number:knm_phone_number()) ->
    boolean().

-callback is_local() ->
    boolean().

-callback check_numbers(kz_term:ne_binaries()) ->
    {'ok', kz_json:object()} |
    {'error', any()}.
