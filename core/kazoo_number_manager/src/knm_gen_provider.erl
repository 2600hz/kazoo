%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc When implementing provider modules, these callbacks are a must!
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_gen_provider).

-include("knm.hrl").

-callback save(knm_number:knm_number()) ->
    knm_number:knm_number().

-callback delete(knm_number:knm_number()) ->
    knm_number:knm_number().
