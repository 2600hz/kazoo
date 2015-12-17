-module(wsock_http_message_data).
-include("wsock.hrl").

-export([new/0, new/1]).
-export([update/2]).
-export([headers/1]).

new() ->
  new([]).

new(Options) ->
  update(#http_message{}, Options).

update(HttpMessage, Options) ->
  HttpMessage#http_message{
    type       = proplists:get_value(type, Options, HttpMessage#http_message.type),
    start_line = proplists:get_value(start_line, Options, HttpMessage#http_message.start_line),
    headers    = proplists:get_value(headers, Options, HttpMessage#http_message.headers)
  }.

headers(#http_message{ headers = Headers }) -> Headers.
