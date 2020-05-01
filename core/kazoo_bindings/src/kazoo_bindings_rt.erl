%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc runtime options for bindings
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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

-spec candidates(kazoo_bindings:kz_rt_options(), kz_term:ne_binary()) -> [kazoo_bindings:kz_bindings()].
candidates(Options, Routing) ->
    Fun = props:get_value('candidates', Options),
    Fun(Routing).

-spec matches(kazoo_bindings:kz_rt_options(), kz_term:binaries(), kz_term:binaries()) -> boolean().
matches(Options, AParts, BParts) ->
    Fun = props:get_value('matches', Options),
    Fun(AParts, BParts).
