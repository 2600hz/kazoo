%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_analyzers).

-export([new_chunk/2]).

-include("../call_inspector.hrl").

-spec new_chunk(ne_binary(), ci_chunk:chunk()) -> 'ok'.
new_chunk(_CallId, _Chunk) ->
    'ok'.
