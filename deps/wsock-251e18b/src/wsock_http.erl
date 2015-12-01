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

-module(wsock_http).
-include("wsock.hrl").

-export([build/3, get_start_line_value/2, get_header_value/2]).
-export([decode/2, encode/1]).

-define(CTRL, "\r\n").

-spec decode(Data::binary(), Type::request | response) -> Result when
      Result :: {ok, #http_message{}}
              | {error, fragmented_http_message}
              | {error, malformed_request}.
decode(Data, Type) ->
  case process_startline(Data, Type) of
    fragmented ->
      {error, fragmented_http_message};
    {error, _} ->
      {error, malformed_request};
    {ok, StartlineFields, Rest} ->
      case process_headers(Rest) of
        fragmented ->
          {error, fragmented_http_message};
        {error, _} ->
          {error, malformed_request};
        {ok, HeadersFields} ->
          {ok, wsock_http:build(Type, StartlineFields, HeadersFields)}
      end
  end.

-spec build(Type::atom(), StartLine::list({atom(), string()}), Headers::list({string(), string()})) -> #http_message{}.
build(Type, StartLine, Headers) ->
  #http_message{type = Type, start_line = StartLine, headers = Headers}.

-spec encode(Message::#http_message{}) -> list(string()).
encode(Message) ->
  Startline = Message#http_message.start_line,
  Headers = Message#http_message.headers,
  encode(Startline, Headers, Message#http_message.type).

-spec encode(Startline::list({atom(), string()}), Headers::list({string(), string()}), Type:: request | response) -> list(string()).
encode(Startline, Headers, request) ->
  encode_message("{{method}} {{resource}} HTTP/{{version}}", Startline, Headers);

encode(Startline, Headers, response) ->
  encode_message("HTTP/{{version}} {{status}} {{reason}}", Startline, Headers).


-spec get_start_line_value(Key::atom(), Message::#http_message{}) -> string().
get_start_line_value(Key, Message) ->
  proplists:get_value(Key, Message#http_message.start_line).

-spec get_header_value(Key::string(), Message::#http_message{}) -> string().
get_header_value(Key, Message) ->
  LowerCasedKey = string:to_lower(Key),
  get_header_value_case_insensitive(LowerCasedKey, Message#http_message.headers).

-spec get_header_value_case_insensitive(Key::string(), list()) ->  undefined | string().
get_header_value_case_insensitive(_, []) ->
  undefined;

get_header_value_case_insensitive(Key, [{Name, Value} | Tail]) ->
  LowerCaseName = string:to_lower(Name),
  case Key == LowerCaseName of
    true ->
      Value;
    false ->
      get_header_value_case_insensitive(Key, Tail)
  end.

%=============
% Helpers
%=============
-spec ensure_string(Data :: list()) -> list()
  ;                (Data :: binary()) -> list()
  ;                (Data :: atom()) -> list().
ensure_string(Data) when is_list(Data) -> Data;
ensure_string(Data) when is_binary(Data) -> erlang:binary_to_list(Data);
ensure_string(Data) when is_integer(Data) -> erlang:integer_to_list(Data);
ensure_string(Data) -> erlang:atom_to_list(Data).

-spec process_startline(
  StartLine::binary(),
  Type:: request | response
  ) ->
    fragmented |
    {ok, list({atom(), string()}), binary()} |
    {error, term()}.
process_startline(StartLine, request) ->
  decode_http_message(http_bin, start_line, StartLine);

process_startline(StartLine, response) ->
  decode_http_message(http_bin, status_line, StartLine).

-spec decode_http_message(
  Type  :: atom(),
  Chunk :: atom(),
  Data  :: binary()
  ) ->
  fragmented |
  {error, invalid_http_message} |
  {error, unexpected_http_message} |
  {ok, [{method, string()} | [{resource, string()} |  {version, string()}]], binary()} |
  {ok, [{version, string()} | [{status, string()} |  {reason, string()}]], binary()} |
  {ok, {string(), string()}, binary()} |
  ok.
decode_http_message(Type, Chunk, Data) ->
  case erlang:decode_packet(Type, Data, []) of
    {more, _} ->
      fragmented;
    {error, _} -> 
      {error, invalid_http_message};
    {ok, {http_error, _}} ->
      {error, invalid_http_message};
    {ok, {http_request, Method, Resource, Version}, Rest} when Chunk == start_line ->
      {ok, [{method, ensure_string(Method)}, {resource, process_http_uri(Resource)}, {version, process_http_version(Version)}], Rest};
    {ok, {http_response, Version, Status, Reason}, Rest} when Chunk == status_line ->
      {ok, [{version, process_http_version(Version)}, {status, ensure_string(Status)}, {reason, ensure_string(Reason)}], Rest};
    {ok, {http_header, _, Field, _, Value}, Rest} when Chunk == header->
      {ok, {ensure_string(Field), ensure_string(Value)}, Rest};
    {ok, http_eoh, _} when Chunk == header ->
      ok;
    _ ->
      {error, unexpected_http_message}
  end.

-spec process_http_uri(
    '*'
  ) -> string()
  ;
  ({
    absoluteURI,
    Protocol :: http | http,
    Host :: string() | binary(),
    Port :: inet:port_number() | undefined,
    Path :: string() | binary()
  }) -> string()
  ;
  ({
    scheme,
    Scheme :: string() | binary(),
    string() | binary()
  }) -> string()
  ;
  ({
    abs_path,
    string() | binary()
  }) -> string().
process_http_uri('*') ->
  "*";
process_http_uri({absoluteURI, Protocol, Host, Port, Path}) ->
  PortString = case Port of
    undefined  -> "";
    Number -> ":" ++ ensure_string(Number)
  end,

  ensure_string(Protocol) ++ "://" ++ ensure_string(Host) ++  PortString  ++ "/" ++ ensure_string(Path) ;
process_http_uri({scheme, Scheme, Path}) ->
  ensure_string(Scheme) ++ Path;
process_http_uri({abs_path, Path}) ->
  ensure_string(Path);
process_http_uri(Uri) ->
  ensure_string(Uri).

-spec process_http_version({Major :: pos_integer(), Minor :: pos_integer()}) -> string().
process_http_version({Major, Minor}) ->
  ensure_string(Major) ++ "." ++ ensure_string(Minor).

-spec process_headers(Headers::list(binary())) -> {ok, list({string(), string()})} | {error, nomatch}.
process_headers(Headers) ->
  process_headers(Headers, []).

-spec process_headers(
  Headers::binary(),
  Acc::list({list(), list()})
) ->
  fragmented |
  {ok, list({string(), string()})} |
  {error, invalid_http_message}.
process_headers(Data, Acc) ->
  case decode_http_message(httph_bin, header, Data) of
    {ok, Header, Rest} ->
      process_headers(Rest, [Header | Acc]);
    ok ->
      {ok, Acc};
    Other ->
      Other
  end.

encode_message(StartlineExpr, StartlineFields, Headers) ->
  SL = build_start_line(StartlineExpr, StartlineFields),
  H= build_headers(Headers),

  lists:foldr(fun(El, Acc) ->
        [El++"\r\n" | Acc]
    end, ["\r\n"], [SL | H]).

build_start_line(StartlineExpr, StartlineFields) ->
  lists:foldr(fun({Key, Value}, Acc) ->
        re:replace(Acc, "{{" ++ atom_to_list(Key) ++ "}}", Value, [{return, list}])
    end, StartlineExpr, StartlineFields).

-spec build_headers(list({HeaderName::string(), HeaderValue::string()})) -> list(string()).
build_headers(Headers) ->
  lists:map(fun({Key, Value}) ->
        Key ++ ": " ++ Value
    end, Headers).
