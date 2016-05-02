%%%-------------------------------------------------------------------
%%% @doc apns4erl connection process
%%% @end
%%%-------------------------------------------------------------------
-module(apns_connection).
-author('Brujo Benavides <elbrujohalcon@inaka.net>').

-behaviour(gen_server).

-include("apns.hrl").
-include("localized.hrl").

-define(SERVER, ?MODULE).

-export([start_link/1, start_link/2, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-export([send_message/2, stop/1]).
-export([build_payload/1]).

-record(state, {out_socket        :: tuple(),
                in_socket         :: tuple(),
                connection        :: apns:connection(),
                in_buffer = <<>>  :: binary(),
                out_buffer = <<>> :: binary(),
                queue             :: pid()}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc  Sends a message to apple through the connection
-spec send_message(apns:conn_id(), apns:msg()) -> ok.
send_message(ConnId, Msg) ->
  gen_server:cast(ConnId, Msg).

%% @doc  Stops the connection
-spec stop(apns:conn_id()) -> ok.
stop(ConnId) ->
  gen_server:cast(ConnId, stop).

%% @hidden
-spec start_link(atom(), apns:connection()) -> startlink_ret().
start_link(Name, Connection) ->
    gen_server:start_link({'local', Name}, ?MODULE, Connection, []).
%% @hidden
-spec start_link(apns:connection()) -> startlink_ret().
start_link(Connection) ->
    gen_server:start_link(?SERVER, Connection, []).

%% @hidden
-spec build_payload(apns:msg()) -> iodata().
build_payload(Msg) ->
  #apns_msg{ alert = Alert
           , badge = Badge
           , sound = Sound
           , apns_extra=Apns_Extra
           , content_available = Content_Available
           , extra = Extra} = Msg,
  build_payload(
    [ {alert, Alert}
    , {badge, Badge}
    , {sound, Sound}] ++ Apns_Extra, Extra, Content_Available).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server implementation, a.k.a.: callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init(apns:connection()) -> {ok, state()} | {stop, any()}.
init(Connection) ->
  try
    {ok, QID} = apns_queue:start_link(),
    case open_out(Connection) of
      {ok, OutSocket} -> case open_feedback(Connection) of
          {ok, InSocket} ->
            {ok, #state{ out_socket = OutSocket
                       , in_socket  = InSocket
                       , connection = Connection
                       , queue      = QID
                       }};
          {error, Reason} -> {stop, Reason}
        end;
      {error, Reason} -> {stop, Reason}
    end
  catch
    _:{error, Reason2} -> {stop, Reason2}
  end.

%% @hidden
ssl_opts(Connection) ->
  Opts = case Connection#apns_connection.key_file of
    undefined -> [];
    KeyFile -> [{keyfile, filename:absname(KeyFile)}]
  end ++
    case Connection#apns_connection.cert_file of
      undefined -> [];
      CertFile -> [{certfile, filename:absname(CertFile)}]
    end ++
    case Connection#apns_connection.key of
      undefined -> [];
      Key -> [{key, Key}]
    end ++
    case Connection#apns_connection.cert of
      undefined -> [];
      Cert -> [{cert, Cert}]
    end ++
    case Connection#apns_connection.cert_password of
      undefined -> [];
      Password -> [{password, Password}]
    end,
  [{mode, binary} | Opts].

%% @hidden
open_out(Connection) ->
  case ssl:connect(
    Connection#apns_connection.apple_host,
    Connection#apns_connection.apple_port,
    ssl_opts(Connection),
    Connection#apns_connection.timeout
  ) of
    {ok, OutSocket} -> {ok, OutSocket};
    {error, Reason} -> {error, Reason}
  end.

%% @hidden
open_feedback(Connection) ->
  case ssl:connect(
    Connection#apns_connection.feedback_host,
    Connection#apns_connection.feedback_port,
    ssl_opts(Connection),
    Connection#apns_connection.timeout
  ) of
    {ok, InSocket} -> {ok, InSocket};
    {error, Reason} -> {error, Reason}
  end.

%% @hidden
-spec handle_call(X, {pid(), reference()}, state()) ->
  {stop, {unknown_request, X}, {unknown_request, X}, state()}.
handle_call(Request, _From, State) ->
  {stop, {unknown_request, Request}, {unknown_request, Request}, State}.

%% @hidden
-spec handle_cast(stop | apns:msg(), state()) ->
  {noreply, state()} | {stop, normal | {error, any()}, state()}.
handle_cast(Msg, State=#state{ out_socket = undefined
                             , connection = Connection}) ->
  try
    error_logger:info_msg("Reconnecting to APNS...~n"),
    case open_out(Connection) of
      {ok, Socket} -> handle_cast(Msg, State#state{out_socket=Socket});
      {error, Reason} -> {stop, Reason}
    end
  catch
    _:{error, Reason2} -> {stop, Reason2}
  end;

handle_cast(Msg, State) when is_record(Msg, apns_msg) ->
  Socket = State#state.out_socket,
  Payload = build_payload(Msg),
  BinToken = hexstr_to_bin(Msg#apns_msg.device_token),
  apns_queue:in(State#state.queue, Msg),
  case send_payload(
        Socket, Msg#apns_msg.id, Msg#apns_msg.expiry, BinToken, Payload, Msg#apns_msg.priority) of
    ok ->
      {noreply, State};
    {error, Reason} ->
      apns_queue:fail(State#state.queue, Msg#apns_msg.id),
      {stop, {error, Reason}, State}
  end;

handle_cast(stop, State) ->
  {stop, normal, State}.

%% @hidden
-spec handle_info(
    {ssl, tuple(), binary()} | {ssl_closed, tuple()} | X, state()) ->
      {noreply, state()} | {stop, ssl_closed | {unknown_request, X}, state()}.
handle_info( {ssl, SslSocket, Data}
           , State = #state{ out_socket = SslSocket
                           , connection = #apns_connection{error_fun = Error}
                           , out_buffer = CurrentBuffer
                           }) ->
  case <<CurrentBuffer/binary, Data/binary>> of
    <<Command:1/unit:8, StatusCode:1/unit:8, MsgId:4/binary, Rest/binary>> ->
      case Command of
        8 -> %% Error
          Status = parse_status(StatusCode),
          {_MsgFailed, RestMsg} = apns_queue:fail(State#state.queue, MsgId),
          _ = [send_message(self(), M) || M <- RestMsg],
          try Error(MsgId, Status) of
            stop -> throw({stop, {msg_error, MsgId, Status}, State});
            _ -> noop
          catch
            _:ErrorResult ->
              error_logger:error_msg(
                "Error trying to inform error (~p) msg ~p:~n\t~p~n",
                [Status, MsgId, ErrorResult])
          end,
          case erlang:size(Rest) of
            0 -> %% It was a whole package
              {noreply, State#state{out_buffer = <<>>}};
            _ ->
              handle_info(
                {ssl, SslSocket, Rest}, State#state{out_buffer = <<>>})
          end;
        Command ->
          throw({stop, {unknown_command, Command}, State})
      end;
    NextBuffer -> %% We need to wait for the rest of the message
      {noreply, State#state{out_buffer = NextBuffer}}
  end;
handle_info( {ssl, SslSocket, Data}
           , State = #state{ in_socket  = SslSocket
                           , connection =
                              #apns_connection{feedback_fun = Feedback}
                           , in_buffer  = CurrentBuffer
                           }) ->
  case <<CurrentBuffer/binary, Data/binary>> of
    <<TimeT:4/big-unsigned-integer-unit:8,
      Length:2/big-unsigned-integer-unit:8,
      Token:Length/binary,
      Rest/binary>> ->
      try Feedback({apns:timestamp(TimeT), bin_to_hexstr(Token)})
      catch
        _:Error ->
          error_logger:error_msg(
            "Error trying to inform feedback token ~p:~n\t~p~n", [Token, Error])
      end,
      case erlang:size(Rest) of
        0 -> {noreply, State#state{in_buffer = <<>>}}; %% It was a whole package
        _ -> handle_info({ssl, SslSocket, Rest}, State#state{in_buffer = <<>>})
      end;
    NextBuffer -> %% We need to wait for the rest of the message
      {noreply, State#state{in_buffer = NextBuffer}}
  end;

handle_info({ssl_closed, SslSocket}, State = #state{in_socket = SslSocket,
                                                    connection= Connection}) ->
  error_logger:info_msg(
    "Feedback server disconnected. Waiting ~p millis to connect again...~n",
    [Connection#apns_connection.feedback_timeout]),
  _Timer =
    erlang:send_after(
      Connection#apns_connection.feedback_timeout, self(), reconnect),
  {noreply, State#state{in_socket = undefined}};

handle_info(reconnect, State = #state{connection = Connection}) ->
  error_logger:info_msg("Reconnecting the Feedback server...~n"),
  case open_feedback(Connection) of
    {ok, InSocket} -> {noreply, State#state{in_socket = InSocket}};
    {error, Reason} -> {stop, {in_closed, Reason}, State}
  end;

handle_info({ssl_closed, SslSocket}, State = #state{out_socket = SslSocket}) ->
  error_logger:info_msg("APNS disconnected~n"),
  {noreply, State#state{out_socket=undefined}};

handle_info(Request, State) ->
  {stop, {unknown_request, Request}, State}.

%% @hidden
-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) -> ok.

%% @hidden
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_payload(Params, Extra, Content_Available) ->
  kz_json:encode(
    {[{<<"aps">>, do_build_payload(Params, Content_Available)} | Extra]}).

do_build_payload(Params, Content_Available) when Content_Available ->
  do_build_payload(Params, [{<<"content-available">>, 1}]);

do_build_payload(Params, Content_Available) when Content_Available == false ->
  do_build_payload(Params, []);

do_build_payload([{Key, Value} | Params], Payload) ->
  case Value of
    Value when is_list(Value); is_binary(Value) ->
      do_build_payload(
        Params,
        [ {atom_to_binary(Key, utf8), unicode:characters_to_binary(Value)}
        | Payload]);
    Value when is_integer(Value) ->
      do_build_payload(Params, [{atom_to_binary(Key, utf8), Value} | Payload]);
    #loc_alert{action = Action,
               args   = Args,
               body   = Body,
               image  = Image,
               key    = LocKey} ->
      Json = {case Body of
                none -> [];
                Body -> [{<<"body">>, unicode:characters_to_binary(Body)}]
              end ++ case Action of
                       none -> [];
                       Action ->
                        [{<<"action-loc-key">>,
                          unicode:characters_to_binary(Action)}]
                     end ++ case Image of
                              none -> [];
                              Image ->
                                [{<<"launch-image">>,
                                  unicode:characters_to_binary(Image)}]
                            end ++
                [{<<"loc-key">>, unicode:characters_to_binary(LocKey)},
                 {<<"loc-args">>,
                    lists:map(fun unicode:characters_to_binary/1, Args)}
                ]},
      do_build_payload(Params, [{atom_to_binary(Key, utf8), Json} | Payload]);
    _ ->
      do_build_payload(Params, Payload)
  end;
do_build_payload([], Payload) ->
  {Payload}.

-spec send_payload(tuple(), binary(), non_neg_integer(), binary(), binary(), integer()) ->
  ok | {error, any()}.
send_payload(Socket, MsgId, Expiry, BinToken, Payload, Priority) ->
    Frame = build_frame(MsgId, Expiry, BinToken, Payload, Priority),
    FrameLength = erlang:size(Frame),
    Packet = [<<2:8,
                FrameLength:32/big,
                Frame/binary>>],
    error_logger:info_msg("Sending msg ~p (expires on ~p)~n",
                         [MsgId, Expiry]),
    ssl:send(Socket, Packet).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([$\s|T], Acc) ->
    hexstr_to_bin(T, Acc);
hexstr_to_bin([X, Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X, Y]),
  hexstr_to_bin(T, [V | Acc]).

bin_to_hexstr(Binary) ->
    L = size(Binary),
    Bits = L * 8,
    <<X:Bits/big-unsigned-integer>> = Binary,
    F = lists:flatten(io_lib:format("~~~B.16.0B", [L * 2])),
    lists:flatten(io_lib:format(F, [X])).

parse_status(0) -> no_errors;
parse_status(1) -> processing_error;
parse_status(2) -> missing_token;
parse_status(3) -> missing_topic;
parse_status(4) -> missing_payload;
parse_status(5) -> missing_token_size;
parse_status(6) -> missing_topic_size;
parse_status(7) -> missing_payload_size;
parse_status(8) -> invalid_token;
parse_status(10) -> shutdown;
parse_status(_) -> unknown.
%
build_frame(MsgId, Expiry, BinToken, Payload, Priority) ->
  PayloadLength = erlang:size(Payload),
  <<1:8, 32:16/big, BinToken/binary,
    2:8, PayloadLength:16/big, Payload/binary,
    3:8, 4:16/big, MsgId/binary,
    4:8, 4:16/big, Expiry:4/big-unsigned-integer-unit:8,
    5:8, 1:16/big, Priority:8>>.
