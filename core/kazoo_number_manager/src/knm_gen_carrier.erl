%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% When implementing carrier modules, these callbacks are a must!
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_gen_carrier).

-include_lib("kazoo_number_manager/src/knm.hrl").

-callback find_numbers(ne_binary(), pos_integer(), kz_proplist()) ->
    {'error', any()} |
    {'bulk', knm_number:knm_numbers()} |
    {'ok', knm_number:knm_numbers()}.

-callback acquire_number(knm_number:knm_number()) ->
    knm_number:knm_number().

-callback disconnect_number(knm_number:knm_number()) ->
    knm_number:knm_number().

-callback should_lookup_cnam() ->
    boolean().

-callback is_number_billable(knm_number:knm_number()) ->
    boolean().

-callback is_local() ->
    boolean().
