%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc runtime options for bindings
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_bindings_rt).


%%==============================================================================
%% API functions
%%==============================================================================

-export([candidates/2
        ,matches/3
        ]).

-include("kazoo_bindings.hrl").

-spec candidates(kazoo_bindings:kz_rt_options(), kz_term:ne_binary()) -> kazoo_bindings:kz_bindings().
candidates(Options, Routing) ->
    Fun = props:get_value('candidates', Options),
    Fun(Routing).

-spec matches(kazoo_bindings:kz_rt_options(), kz_term:binaries(), kz_term:binaries()) -> boolean().
matches(Options, AParts, BParts) ->
    Fun = props:get_value('matches', Options),
    Fun(AParts, BParts).
