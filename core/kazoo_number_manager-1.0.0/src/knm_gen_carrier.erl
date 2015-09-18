%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% When implementing carrier modules, these callbacks are a must!
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(knm_gen_carrier).

-include("knm.hrl").

-callback find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
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
