%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ci_analyzers).

-export([new_chunk/2]).

-include("call_inspector.hrl").

-spec new_chunk(kz_term:ne_binary(), ci_chunk:chunk()) -> 'ok'.
new_chunk(_CallId, _Chunk) ->
    'ok'.
