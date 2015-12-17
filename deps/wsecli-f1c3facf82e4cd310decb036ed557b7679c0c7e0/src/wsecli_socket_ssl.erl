%% @hidden
-module(wsecli_socket_ssl).
-include("wsecli.hrl").

-export([start_link/4]).
-export([init/4]).
-export([loop/1]).


%%========================================
%% Data
%%========================================
-record(state, {
    client :: pid(),
    socket :: socket()
  }).

%%========================================
%% API
%%========================================
-spec start_link(
  Host    :: string(),
  Port    :: inet:port_number(),
  Client  :: pid(),
  Options :: list(ssl:connect_option())
) -> {ok, socket()}.
start_link(Host, Port, Client, Options) ->
  proc_lib:start_link(?MODULE, init, [Host, Port, Client, Options]).

-spec init(
  Host    :: string(),
  Port    :: inet:port_number(),
  Client  :: pid(),
  Options :: list(ssl:connect_option())
  ) -> ok.
init(Host, Port, Client, Options) ->
  {ok, Socket} = ssl:connect(Host, Port, Options),
  State        = #state{ client = Client, socket = Socket },
  proc_lib:init_ack({ok, self()}),
  loop(State).

%%========================================
%% Internal
%%========================================
loop(State) ->
  receive
    {socket, send, Data}   ->
      ssl:send(State#state.socket, Data),
      loop(State);
    {socket, close}        ->
      ssl:close(State#state.socket);
    {ssl, _, Data}         -> % Received data
      wsecli_socket:notify_client({data, Data}, State#state.client),
      loop(State);
    {ssl_closed, _}        -> % Close
      wsecli_socket:notify_client(close, State#state.client);
    {ssl_error, _, Reason} -> % Error
      wsecli_socket:notify_client({error, Reason}, State#state.client)
  end.
