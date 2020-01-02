%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
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

-callback find_numbers(kz_term:ne_binary(), pos_integer(), knm_search:options()) ->
    knm_search:mod_response().

-callback acquire_number(knm_phone_number:record()) ->
    knm_phone_number:record().

-callback disconnect_number(knm_phone_number:record()) ->
    knm_phone_number:record().

-callback should_lookup_cnam() ->
    boolean().

-callback is_number_billable(knm_phone_number:record()) ->
    boolean().

-callback is_local() ->
    boolean().

-callback check_numbers(kz_term:ne_binaries()) ->
    {'ok', kz_json:object()} |
    {'error', any()}.
