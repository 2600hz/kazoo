%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz INC
%%% @doc
%%%
%%% When implementing provider modules, these callbacks are a must!
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_gen_provider).

-include("knm.hrl").

-callback save(knm_number:knm_number()) ->
    knm_number:knm_number().

-callback delete(knm_number:knm_number()) ->
    knm_number:knm_number().
