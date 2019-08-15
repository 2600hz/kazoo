%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ci_analyzers).

-export([new_chunk/2]).

-include("call_inspector.hrl").

-spec new_chunk(kz_term:ne_binary(), ci_chunk:chunk()) -> 'ok'.
new_chunk(_CallId, _Chunk) ->
    'ok'.
