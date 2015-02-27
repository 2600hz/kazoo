%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_chunk).

-export([new/0]).
-export([set_call_id/2
         ,call_id/1
        ]).
-export([append_data/2
        ,set_data/2
        ,data/1
        ]).
-export([to_json/1]).
-export([is_chunk/1]).

-record(ci_chunk, {call_id
                  ,data = []
                  }).
-type ci_chunk() :: #ci_chunk{}.

-export_type([ci_chunk/0]).

-include("../call_inspector.hrl").

-spec new() -> ci_chunk().
new() -> #ci_chunk{}.

-spec set_call_id(ci_chunk(), ne_binary()) -> ci_chunk().
set_call_id(Chunk, CallId) ->
    Chunk#ci_chunk{call_id=CallId}.

-spec call_id(ci_chunk()) -> api_binary().
call_id(#ci_chunk{call_id=CallId}) ->
    CallId.

-spec append_data(ci_chunk(), ne_binary()) -> ci_chunk().
append_data(#ci_chunk{data=D}=Chunk, Data) ->
    Chunk#ci_chunk{data=[Data|D]}.

-spec set_data(ci_chunk(), ne_binaries()) -> ci_chunk().
set_data(Chunk, Data) ->
    Chunk#ci_chunk{data=Data}.

-spec data(ci_chunk()) -> ne_binaries().
data(#ci_chunk{data=Data}) ->
    Data.

-spec to_json(ci_chunk()) -> ne_binaries().
to_json(Chunk) ->
    data(Chunk).

-spec is_chunk(_) -> boolean().
is_chunk(#ci_chunk{}) -> 'true';
is_chunk(_) -> 'false'.
