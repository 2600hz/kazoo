%Copyright [2012] [Farruco Sanjurjo Arcay]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

%% @hidden

-module(wsock_handshake).
-include("wsock.hrl").

-export([open/3, handle_response/2]).
-export([handle_open/1, response/1]).

-define(VERSION, 13).
-define(GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

-define(INVALID_CLIENT_OPEN, invalid_handshake_opening).
-define(INVALID_SERVER_RESPONSE, invalid_server_response).

-spec handle_open(Message::#http_message{}) -> {ok, #handshake{}} | {error, ?INVALID_CLIENT_OPEN}.
handle_open(Message) ->
  case validate_handshake_open(Message) of
    true ->
      {ok , #handshake{ type = handle_open, message = Message}};
    false ->
      {error, ?INVALID_CLIENT_OPEN}
  end.

-spec handle_response(Response::#http_message{}, Handshake::#handshake{}) -> {ok, #handshake{}} | {error, ?INVALID_SERVER_RESPONSE}.
handle_response(Response, Handshake) ->
  case validate_handshake_response(Response, Handshake) of
    true ->
      {ok, #handshake{ type = handle_response, message = Response}};
    false ->
      {error, ?INVALID_SERVER_RESPONSE}
  end.

-spec response(ClientWebsocketKey::string()) -> {ok, #handshake{}}.
response(ClientWebsocketKey) ->
  BinaryKey = list_to_binary(ClientWebsocketKey),
  HttpMessage = #http_message{
    start_line = [
      {version, "1.1"},
      {status, "101"},
      {reason, "Switching protocols"}
    ],
    headers = [
      {"upgrade", "Websocket"},
      {"connection", "Upgrade"},
      {"sec-websocket-accept", base64:encode_to_string(crypto:sha(<<BinaryKey/binary, ?GUID>>)) }
    ],
    type = response
  },

  {ok, #handshake{ type = response, message = HttpMessage}}.

-spec open(Resource ::string(), Host ::string(), Port::integer()) -> {ok, #handshake{}}.
open(Resource, Host, Port) ->
  RequestLine = [
    {method, "GET"},
    {version, "1.1"},
    {resource, Resource}
  ],

  Headers =[
    {"Host", Host ++ ":" ++ integer_to_list(Port)},
    {"Upgrade", "websocket"},
    {"Connection", "upgrade"},
    {"Sec-Websocket-Key", wsock_key:generate()},
    {"Sec-Websocket-Version", integer_to_list(?VERSION)}
  ],

  Message = wsock_http:build(request, RequestLine, Headers),
  {ok, #handshake{ version = ?VERSION, type = open, message = Message}}.


%=======================
% INTERNAL FUNCTIONS
%=======================

-spec validate_startline(StartLine::list({atom(), term()})) -> true | false.
validate_startline(StartLine) ->
  Matchers = [{method, "GET"}, {version, "1\.1"}],
  lists:all(fun({Key, Value}) ->
        match == re:run(proplists:get_value(Key, StartLine), Value, [caseless, {capture, none}])
    end, Matchers).

validate_headers(Headers) ->
  Matchers = [
    {"host", ".+", required},
    {"upgrade", "websocket", required},
    {"connection", "upgrade", required},
    {"sec-websocket-key", "[a-z0-9\+\/]{22}==", required},
    {"sec-websocket-version", "13", required},
    {"origin", ".+", optional}],

  lists:all(fun({HeaderName, HeaderValue, Type}) ->
        case get_value_insensitive(HeaderName, Headers) of
          undefined when (Type == optional) ->
            true;
          undefined ->
            false;
          Value ->
            match == re:run(Value, HeaderValue, [caseless, {capture, none}])
        end
    end, Matchers).

-spec validate_handshake_response(Response::#http_message{}, OpenHandshake::#handshake{}) -> true | false.
validate_handshake_response(Response, OpenHandshake) ->
  validate_http_status(Response)
  andalso
  validate_upgrade_header(Response)
  andalso
  validate_connection_header(Response)
  andalso
  validate_sec_websocket_accept_header(Response, OpenHandshake).

-spec validate_handshake_open(OpenHandshake::#http_message{}) -> true | false.
validate_handshake_open(OpenHandshake) ->
  validate_startline(OpenHandshake#http_message.start_line)
  andalso
  validate_headers(OpenHandshake#http_message.headers).

get_value_insensitive(Key, [{Name, Value} | Tail]) ->
  case re:run(Name, "^" ++ Key ++ "$", [caseless, {capture, first, list}]) of
    {match, _} ->
      Value;
    nomatch ->
        get_value_insensitive(Key, Tail)
    end;

get_value_insensitive(_, []) ->
  undefined.




-spec validate_http_status(Response::#http_message{}) -> boolean().
validate_http_status(Response) ->
  "101" == wsock_http:get_start_line_value(status, Response).

-spec validate_upgrade_header(Response ::#http_message{}) -> boolean().
validate_upgrade_header(Response) ->
  "websocket" == string:to_lower(wsock_http:get_header_value("upgrade", Response)).

-spec validate_connection_header(Response ::#http_message{}) -> boolean().
validate_connection_header(Response) ->
  "upgrade" == string:to_lower(wsock_http:get_header_value("connection", Response)).

-spec validate_sec_websocket_accept_header(Response::#http_message{}, Handshake::#handshake{}) -> boolean().
validate_sec_websocket_accept_header(Response, Handshake) ->
  ClientKey         = wsock_http:get_header_value("sec-websocket-key", Handshake#handshake.message),
  BinaryClientKey   = list_to_binary(ClientKey),
  ExpectedHashedKey = base64:encode_to_string(crypto:sha(<<BinaryClientKey/binary, ?GUID>>)),
  HashedKey         = wsock_http:get_header_value("sec-websocket-accept", Response),

  ExpectedHashedKey == HashedKey.
