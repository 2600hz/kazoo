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
-export([set_timestamp/2
         ,timestamp/1]).
-export([set_to/2
         ,to/1]).
-export([set_from/2
         ,from/1]).
-export([set_parser/2
         ,parser/1]).
-export([set_label/2
         ,label/1]).
-export([to_json/1]).
-export([is_chunk/1]).
-export([sort_by_timestamp/1]).

-record(ci_chunk, {call_id = 'undefined'
                  ,data = []
                  ,timestamp = 'undefined'
                  ,to = 'undefined'
                  ,from = 'undefined'
                  ,parser :: atom()
                  ,label = 'undefined'
                 }).
-type chunk() :: #ci_chunk{}.
-type chunks() :: [chunk(),...].

-export_type([chunk/0
              ,chunks/0
             ]).

-include("../call_inspector.hrl").

-spec new() -> chunk().
new() -> #ci_chunk{}.

-spec set_call_id(chunk(), ne_binary()) -> chunk().
set_call_id(Chunk, CallId) ->
    Chunk#ci_chunk{call_id=CallId}.

-spec call_id(chunk()) -> api_binary().
call_id(#ci_chunk{call_id=CallId}) ->
    CallId.

-spec append_data(chunk(), ne_binary()) -> chunk().
append_data(#ci_chunk{data=D}=Chunk, Data) ->
    Chunk#ci_chunk{data=[Data|D]}.

-spec set_data(chunk(), ne_binaries()) -> chunk().
set_data(Chunk, Data) ->
    Chunk#ci_chunk{data=Data}.

-spec data(chunk()) -> ne_binaries().
data(#ci_chunk{data=Data}) ->
    Data.

-spec set_timestamp(chunk(), ne_binary()) -> chunk().
set_timestamp(Chunk, Timestamp) ->
    Chunk#ci_chunk{timestamp=Timestamp}.

-spec timestamp(chunk()) -> api_binary().
timestamp(#ci_chunk{timestamp=Timestamp}) ->
    Timestamp.

-spec set_to(chunk(), ne_binary()) -> chunk().
set_to(Chunk, To) ->
    Chunk#ci_chunk{to=To}.

-spec to(chunk()) -> api_binary().
to(#ci_chunk{to=To}) ->
    To.

-spec set_from(chunk(), ne_binary()) -> chunk().
set_from(Chunk, From) ->
    Chunk#ci_chunk{from=From}.

-spec from(chunk()) -> api_binary().
from(#ci_chunk{from=From}) ->
    From.

-spec set_parser(chunk(), atom()) -> chunk().
set_parser(Chunk, Parser) ->
    Chunk#ci_chunk{parser=Parser}.

-spec parser(chunk()) -> atom().
parser(#ci_chunk{parser=Parser}) ->
    Parser.

-spec set_label(chunk(), ne_binary()) -> chunk().
set_label(Chunk, Label) ->
    Chunk#ci_chunk{label=Label}.

-spec label(chunk()) -> api_binary().
label(#ci_chunk{label=Label}) ->
    Label.

-spec to_json(chunk()) -> ne_binaries().
to_json(Chunk) ->
    wh_json:from_list(
      [{<<"from">>, from(Chunk)}
       ,{<<"to">>, to(Chunk)}
       ,{<<"call-id">>, call_id(Chunk)}
       ,{<<"timestamp">>, timestamp(Chunk)}
       ,{<<"raw">>, data(Chunk)}
      ]
     ).

-spec is_chunk(_) -> boolean().
is_chunk(#ci_chunk{}) -> 'true';
is_chunk(_) -> 'false'.

-spec sort_by_timestamp(chunks()) -> chunks().
sort_by_timestamp(Chunks) ->
    F = fun(C1, C2) -> timestamp(C1) < timestamp(C2) end,
    lists:sort(F, Chunks).
