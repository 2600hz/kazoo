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

%% @author Farruco Sanjurjo <madtrick@gmail.com>
%% @copyright 2012, Farruco Sanjurjo
%% @doc Websocket Client

-module(wsecli).
-behaviour(gen_fsm).

-include_lib("wsock/include/wsock.hrl").

-export([start/2, start/4, stop/0, stop/1, send/1, send/2, send/3]).
-export([on_open/1, on_open/2, on_error/1, on_error/2, on_message/1, on_message/2, on_close/1, on_close/2]).
-export([init/1, connecting/2, open/2, closing/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(callbacks, {
    on_open    :: on_open_callback(),
    on_error   :: on_error_callback(),
    on_message :: on_message_callback(),
    on_close   :: on_close_callback()
  }).

-record(http_fragment, {data :: binary()}).

-record(data, {
    socket                         :: inet:socket(),
    handshake                      :: undefined | #handshake{},
    cb = default_callbacks()       :: callbacks(),
    fragmented_message = undefined :: undefined | #message{},
    http_fragment = undefined      :: undefined | http_fragment()
  }).

%%========================================
%% Types
%%========================================
-type callbacks()           :: #callbacks{}.
-type data()                :: #data{}.
-type encoding()            :: text | binary.
-type client()              :: atom() | pid().
-type on_open_callback()    :: fun(() -> any()).
-type on_error_callback()   :: fun((string()) -> any()).
-type data_callback()       :: fun((text, string()) -> any()) | fun((binary, binary()) -> any()).
-type on_message_callback() :: data_callback().
-type on_close_callback()   :: fun((undefined) -> any()).
-type http_fragment()       :: #http_fragment{}.

%%========================================
%% Constants
%%========================================
-define(CLOSE_HANDSHAKE_TIMEOUT, 2000).
-define(TCP_CLOSE_TIMEOUT, 500).
-define(DEFAULT_REG_NAME, ?MODULE).
-define(DEFAULT_PORT_WS, 80).
-define(DEFAULT_PORT_WSS, 443).

%%========================================
%% Public API
%%========================================

%% @doc Start the websocket client.
%%
%% This function will open a connection with the specified remote endpoint.
%%
%% If the option `ssl' is given it will be overwritten by the scheme type of the URI.
-spec start(
  URI     :: string(),
  Options :: list({atom(), any()})
  ) ->
  {error, any()} |
  {ok, pid()}.
start(URI, Options) ->
  case http_uri:parse(URI, [{scheme_defaults, [{wss, ?DEFAULT_PORT_WSS}, {ws, ?DEFAULT_PORT_WS}]}]) of
    {ok, {Scheme, _, Host, Port, Path, Query}} ->
      UseSSL = case Scheme of
        ws  -> false;
        wss -> true
      end,
      UpdatedOptions = [{ssl, UseSSL} | proplists:delete(ssl, Options)],
      start(Host, Port, string:concat(Path, Query), UpdatedOptions);
    {error, Reason} ->
      {error, Reason}
  end.


%% @doc Start the websocket client
%%
%% This function will open a connection with the specified remote endpoint.
%% The options to this method can be the following:
%%
%% <dl>
%%
%%  <dt>`ssl'</dt>
%%  <dd>Connect to the server using ssl or not. Values: `true' or `false'</dd>
%%
%%  <dt>`register'</dt>
%%  <dd>Register the client in the Erlang runtime. Values: `true' to register using the default name `wsecli', `false' to not register and any other atom to register it with other name</dd>
%%
%% </dl>
%%
%% If no options are given the process will not be registered and it will not try to connect using SSL.
-spec start(
  Host     :: string(),
  Port     :: inet:port_number(),
  Resource :: string(),
  Options  :: list({atom(), any()})
) -> {ok, pid()}.
start(Host, Port, Path, Options) ->
  UseSSL        = proplists:get_value(ssl, Options, false),
  GenOptions    = [{timeout, 5000}],
  ClientOptions = {Host, Port, Path, UseSSL},
  case proplists:get_value(register, Options, false) of
      false ->
        gen_fsm:start_link(?MODULE, ClientOptions, GenOptions);
      true ->
        gen_fsm:start_link({local, ?DEFAULT_REG_NAME}, ?MODULE, ClientOptions, GenOptions);
      Name ->
        gen_fsm:start_link({local, Name}, ?MODULE, ClientOptions, GenOptions)
    end.


%% @doc Stop the websocket client
%%
%% This is the same as calling {@link stop/1} with the atom `wsecli' as a parameter.
-spec stop() -> ok.
stop() ->
  stop(?DEFAULT_REG_NAME).

%% @doc Stop the websocket client
%%
%% Calling this function doesn't mean that the connection will be inmediatelly closed. A
%% closing handshake is tryed and only after that the connection is closed and the client stopped. Use
%% the callback {@link on_close/2} to know when the connection has ben closed and the client has stopped.
-spec stop(
  Client :: client()
  ) -> ok.
stop(Client) ->
  gen_fsm:sync_send_all_state_event(Client, stop).

%% @doc Send data to a remote endpoint
%%
%% This is the same as calling {@link send/2} with the atom `wsecli' (see {@link start/4}) as first parameter
-spec send(
  Data :: binary() | string()
  ) -> ok.
send(Data) ->
  send(?DEFAULT_REG_NAME, Data).

%% @doc Send data to a remote endpoint
%%
%% This is the same as calling {@link send/3} whit the third parameter being the type of the Data sent. The type will
%% determined by `is_binary' and `is_list' NIFs. So if you want to send a `text' message which is stored in a binary value you
%% will have to use {@link send/3} and specify the type as `text'. Otherwise it will be sent as `binary' type
-spec send(
  Client :: client(),
  Data   :: binary() | string()
  ) -> ok.
send(Client, Data) when is_binary(Data) ->
  send(Client, Data, binary);
send(Client, Data) when is_list(Data) ->
  send(Client, Data, text).

%% @doc Send data to a remote endpoint
%%
%% Sends data using the specified client, encoding the data as the give type.
-spec send(
  Client :: client(),
  Data :: binary() | string(),
  Type :: encoding()
  ) -> ok.
send(Client, Data, Type) ->
  gen_fsm:send_event(Client, {send, Data, Type}).

%% @doc Add a callback to be called when a connection is opened
%%
%% This is the same as calling {@link on_open/2} with the atom `wsecli' (see {@link start/4}) as first parameter.
-spec on_open(
  Callback :: on_open_callback()
  ) -> any().
on_open(Callback) ->
  on_open(?DEFAULT_REG_NAME, Callback).

%% @doc Add a callback to be called when a connection is opened
%%
%% This callback will be called when the connection is opened (after a successful websocket handshake)
%%or inmediatelly if added while in the open state. Adding it in any other state will
%%raise an error.
-spec on_open(
  Client   :: client(),
  Callback :: on_open_callback()
  ) -> any().
on_open(Client, Callback) ->
  gen_fsm:send_event(Client, {on_open, Callback}).

%% @doc Add a callback to be called when an error occurs
%%
%% This is the same as calling {@link on_error/2} with the atom `wsecli' (see {@link start/4}) as the first parameter.
-spec on_error(
  Callback :: on_error_callback()
  ) -> any().
on_error(Callback) ->
  on_error(?DEFAULT_REG_NAME, Callback).

%% @doc Add a callback to be called when an error occurs
-spec on_error(
  Client   :: client(),
  Callback :: on_error_callback()
  ) -> any().
on_error(Client, Callback) ->
  gen_fsm:send_all_state_event(Client, {on_error, Callback}).

%% @doc Add a callback to be called when a message arrives
%%
%% This is the same as calling {@link on_message/2} with the atom `wsecli' (see {@link start/4}) as the first parameter.
-spec on_message(
  Callback :: on_message_callback()
  ) -> any().
on_message(Callback) ->
  on_message(?DEFAULT_REG_NAME, Callback).

%% @doc Add a callback to be called when a message arrives
-spec on_message(
  Client   :: client(),
  Callback :: on_message_callback()
  ) -> any().
on_message(Client, Callback) ->
  gen_fsm:send_all_state_event(Client, {on_message, Callback}).

%% @doc Add a callback to be called when then connection was closed
%%
%% This is the same as calling {@link on_close/2} with the atom `wsecli' (see {@link start/4}) as the first parameter.
-spec on_close(
  Callback :: on_close_callback()
  ) -> any().
on_close(Callback) ->
  on_close(?DEFAULT_REG_NAME, Callback).

%% @doc Add a callback to be called when then connection was closed
%%
%% This callback will be called when the connection is successfully closed, i.e. after performing the
%%websocket closing handshake.
-spec on_close(
  Client   :: client(),
  Callback :: on_close_callback()
  )-> any().
on_close(Client, Callback) ->
  gen_fsm:send_all_state_event(Client, {on_close, Callback}).

%%========================================
%% Gen FSM functions
%%========================================
%% @hidden
-spec init(
  {
    Host     :: string(),
    Port     :: inet:port_number(),
    Resource :: string(),
    SSL      :: boolean()
  }
  ) -> {ok, connecting, #data{}}.
init({Host, Port, Resource, SSL}) ->
  SocketType = case SSL of true -> ssl; false -> plain end,
  {ok, Socket}    = wsecli_socket:open(Host, Port, SocketType, self()),
  {ok, Handshake} = wsock_handshake:open(Resource, Host, Port),
  Request         = wsock_http:encode(Handshake#handshake.message),

  wsecli_socket:send(Request, Socket),

  {ok, connecting, #data{ socket = Socket, handshake = Handshake }}.


%% @hidden
-spec connecting(
  {on_open, Callback :: fun()},
  StateData :: data()
  ) -> term()
    ;
  (
  {send, Data :: binary()},
  StateData :: data()
  ) -> term().
connecting({on_open, Callback}, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_open = Callback},
  {next_state, connecting, StateData#data{cb = Callbacks}};

connecting({send, _Data}, StateData) ->
  (StateData#data.cb#callbacks.on_error)("Can't send data while in connecting state"),
  {next_state, connecting, StateData}.

%% @hidden
-spec open(
  Event     :: term(),
  StateData :: data()
  ) -> term().
open({on_open, Callback}, StateData) ->
  spawn(Callback),
  {next_state, open, StateData};
open({send, Data, Type}, StateData) ->
  Message = wsock_message:encode(Data, [mask, Type]),
  wsecli_socket:send(Message, StateData#data.socket),
  {next_state, open, StateData}.


%% @hidden
-spec closing(
  Event     :: term(),
  StateData :: data()
  ) -> term().
closing({send, _Data}, StateData) ->
  (StateData#data.cb#callbacks.on_error)("Can't send data while in closing state"),
  {next_state, closing, StateData};
closing({timeout, _Ref, waiting_tcp_close}, StateData) ->
  %The tcp connection hasn't been close so, kill them all
  {stop, normal, StateData};
closing({timeout, _Ref, waiting_close_reply}, StateData) ->
  %The websocket close handshake hasn't been properly done, kill them all
  {stop, normal, StateData}.

%%========================================
%% GEN FSM CALLBACK FUNCTIONS
%%========================================
%% @hidden
handle_event({on_error, Callback}, StateName, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_error = Callback},
  {next_state, StateName, StateData#data{cb = Callbacks} };
handle_event({on_message, Callback}, StateName, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_message = Callback},
  {next_state, StateName, StateData#data{cb = Callbacks}};
handle_event({on_close, Callback}, StateName, StateData) ->
  Callbacks = StateData#data.cb#callbacks{on_close = Callback},
  {next_state, StateName, StateData#data{cb = Callbacks}}.

%% @hidden
-spec handle_sync_event(
  Event     :: stop,
  From      :: {pid(), reference()},
  StateName :: atom(),
  StateData :: data()
  ) -> {reply, term(), atom(), term()} |
       {stop, term(), term(), #data{}}.
handle_sync_event(stop, _From, closing, StateData) ->
  {reply, {ok, closing}, closing, StateData};
handle_sync_event(stop, _From, connecting, StateData) ->
  {stop, normal, {ok, closing}, StateData};
handle_sync_event(stop, _From, open, StateData) ->
  Message = wsock_message:encode([], [mask, close]),
  wsecli_socket:send(Message, StateData#data.socket),

  gen_fsm:start_timer(?CLOSE_HANDSHAKE_TIMEOUT, waiting_close_reply),
  {reply, {ok, closing}, closing, StateData}.

%% @hidden
-spec handle_info(
  {socket, Value :: term()},
  StateName :: connecting,
  StateData :: data()
  ) -> {next_state, atom(), #data{}}.
handle_info({socket, {data, Data}}, connecting, StateData = #data{http_fragment = undefined}) ->
  do_wsock_httpdecode(Data, StateData);
handle_info({socket, {data, DataIn}}, connecting,
            StateData = #data{http_fragment = #http_fragment{data = Previous}}) ->
  Data = <<Previous/binary, DataIn/binary>>,
  do_wsock_httpdecode(Data, StateData);
handle_info({socket, {data, Data}}, open, StateData) ->
  {Messages, State} = case StateData#data.fragmented_message of
    undefined ->
      {wsock_message:decode(Data, []), StateData};
    Message ->
      {wsock_message:decode(Data, Message, []), StateData#data{fragmented_message = undefined}}
  end,
  NewStateData = process_messages(Messages, State),
  {next_state, open, NewStateData};
handle_info({socket, {data, Data}}, closing, StateData) ->
  [Message] = wsock_message:decode(Data, []),
  case Message#message.type of
    close ->
      % if we don't receive a tcp_closed message, move to closed state anyway
      gen_fsm:start_timer(?TCP_CLOSE_TIMEOUT, waiting_tcp_close),
      {next_state, closing, StateData};
    _ ->
      {next_state, closing, StateData}
  end;
handle_info({socket, close}, _StateName, StateData) ->
  {stop, normal, StateData}.

do_wsock_httpdecode(Data, StateData) ->
    case wsock_http:decode(Data, response) of
        {ok, Response} ->
            Resp = wsock_handshake:handle_response(Response, StateData#data.handshake),
            do_handle_response(Resp, StateData#data{http_fragment = undefined});
        {error, fragmented_http_message} ->
            {next_state, connecting, StateData#data{http_fragment = http_fragment(Data)}};
        {error, malformed_request} ->
            erlang:exit(malformed_request)
    end.

do_handle_response({ok, _Handshake}, StateData) ->
    spawn(StateData#data.cb#callbacks.on_open),
    {next_state, open, StateData#data{fragmented_message = undefined}};
do_handle_response({error, _Error}, StateData) ->
    {stop, failed_handshake, StateData}.

%% @hidden
-spec terminate(
  Reason::atom(),
  StateName::atom(),
  StateData :: data()
  ) -> pid().
terminate(_Reason, _StateName, StateData) ->
  wsecli_socket:close(StateData#data.socket),
  spawn(fun() -> (StateData#data.cb#callbacks.on_close)(undefined) end).

%% @hidden
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%%========================================
%% Internal
%%========================================
-spec process_messages(
  Messages  :: list(#message{}),
  StateData :: data()
  ) -> #data{}.
process_messages([], StateData) ->
  StateData;
process_messages([Message | Messages], StateData) ->
  case Message#message.type of
    text ->
      spawn(fun() -> (StateData#data.cb#callbacks.on_message)(text, Message#message.payload) end),
      process_messages(Messages, StateData);
    binary ->
      spawn(fun() -> (StateData#data.cb#callbacks.on_message)(binary, Message#message.payload) end),
      process_messages(Messages, StateData);
    fragmented ->
      NewStateData = StateData#data{fragmented_message = Message},
      process_messages(Messages, NewStateData)
  end.

-spec default_callbacks() -> callbacks().
default_callbacks() ->
    #callbacks{on_open = fun() -> undefined end,
               on_error = fun(_Reason)-> undefined end,
               on_message = fun(_Type, _Message) -> undefined end,
               on_close = fun(_Reason) -> undefined end}.

-spec http_fragment(binary()) -> http_fragment().
http_fragment(Data) -> #http_fragment{data = Data}.
