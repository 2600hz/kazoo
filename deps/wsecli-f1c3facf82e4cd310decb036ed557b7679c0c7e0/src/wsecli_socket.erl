%% @hidden
-module(wsecli_socket).
-include("wsecli.hrl").

-export([open/4]).
-export([send/2]).
-export([close/1]).

-export([notify_client/2]).

%%========================================
%% Types
%%========================================
-type socket_type()       :: plain | ssl.
-type client_notification() :: close | {data, binary()} | {error, term()}.


%%========================================
%% Constants
%%========================================
-define(DEFAULT_SOCKET_OPTIONS, [binary, {reuseaddr, true}, {packet, raw}]).

%%========================================
%% Client API
%%========================================
-spec open(
  Host    :: string(),
  Port    :: inet:port_number(),
  Type    :: socket_type(),
  Client  :: pid()
  ) ->
  {ok, socket()} |
  {error, term()}.
open(Host, Port, plain, Client) ->
  wsecli_socket_plain:start_link(Host, Port, Client, ?DEFAULT_SOCKET_OPTIONS);
open(Host, Port, ssl, Client) ->
  wsecli_socket_ssl:start_link(Host, Port, Client, ?DEFAULT_SOCKET_OPTIONS).

-spec send(
  Data   :: iolist(),
  Socket :: socket()
  ) -> ok.
send(Data, Socket) ->
  Socket ! {socket, send, Data},
  ok.

-spec close(
  Socket :: socket()
  ) -> ok.
close(Socket) ->
  Socket ! {socket, close},
  ok.

%%========================================
%% Socket API to interact with client
%%========================================
-spec notify_client(
  What   :: client_notification(),
  Client :: pid()
  ) -> ok.
notify_client(What, Client) ->
  Client ! {socket, What},
  ok.
