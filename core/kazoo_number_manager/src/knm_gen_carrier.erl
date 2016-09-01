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

-include("knm.hrl").

-callback find_numbers(ne_binary(), pos_integer(), knm_carriers:options()) ->
    {'ok', knm_number:knm_numbers()} |
    {'bulk', knm_number:knm_numbers()} |
    {'error', any()}.

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
