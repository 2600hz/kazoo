-module(demo).

-export([start/0, open/3, recv/4, handle_info/4, close/3]).

-record(session_state, {}).

start() ->
    ok = application:start(sasl),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(socketio),

    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/socket.io/1/[...]", socketio_handler, [socketio_session:configure([{heartbeat, 5000},
                                                                                                                   {heartbeat_timeout, 30000},
                                                                                                                   {session_timeout, 30000},
                                                                                                                   {callback, ?MODULE},
                                                                                                                   {protocol, socketio_data_protocol}])]},
                                             {"/[...]", cowboy_static, [
                                                                        {directory, <<"./priv">>},
                                                                        {mimetypes, [
                                                                                     {<<".html">>, [<<"text/html">>]},
                                                                                     {<<".css">>, [<<"text/css">>]},
                                                                                     {<<".js">>, [<<"application/javascript">>]}]}
                                                                       ]}
                                            ]}
                                     ]),

    demo_mgr:start_link(),

    cowboy:start_http(socketio_http_listener, 100, [{host, "127.0.0.1"},
                                                    {port, 8080}], [{env, [{dispatch, Dispatch}]}]).

%% ---- Handlers
open(Pid, Sid, _Opts) ->
    erlang:send_after(5000, self(), tick),
    error_logger:info_msg("open ~p ~p~n", [Pid, Sid]),
    demo_mgr:add_session(Pid),
    {ok, #session_state{}}.

recv(_Pid, _Sid, {json, <<>>, Json}, SessionState = #session_state{}) ->
    error_logger:info_msg("recv json ~p~n", [Json]),
    demo_mgr:publish_to_all(Json),
    {ok, SessionState};

recv(Pid, _Sid, {message, <<>>, Message}, SessionState = #session_state{}) ->
    socketio_session:send_message(Pid, Message),
    {ok, SessionState};

recv(Pid, Sid, Message, SessionState = #session_state{}) ->
    error_logger:info_msg("recv ~p ~p ~p~n", [Pid, Sid, Message]),
    {ok, SessionState}.

handle_info(_Pid, _Sid, tick, SessionState = #session_state{}) ->
    error_logger:info_msg("Tick...", []),
    {ok, SessionState};

handle_info(_Pid, _Sid, _Info, SessionState = #session_state{}) ->
    {ok, SessionState}.

close(Pid, Sid, _SessionState = #session_state{}) ->
    error_logger:info_msg("close ~p ~p~n", [Pid, Sid]),
    demo_mgr:remove_session(Pid),
    ok.
