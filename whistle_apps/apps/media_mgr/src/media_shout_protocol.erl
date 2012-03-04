%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Lifted a lot from cowboy_http_protocol
%%% Implements a shout protocol handler
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_shout_protocol).

-behaviour(cowboy_protocol).

-export([start_link/4]). %% API.
-export([init/4, parse_request/1, handler_loop/3]). %% FSM.

-include("media.hrl").

%% -define(CHUNKSIZE, 24576).
-define(END_OF_REQUEST, <<"\r\n\r\n">>).

-record(state, {
          listener :: pid()
          ,socket :: inet:socket()
          ,transport :: module()
          ,file_server :: pid()
          ,file_server_ref :: reference()
          ,buffer = <<>> :: binary()
          ,timeout = ?MILLISECONDS_IN_DAY :: pos_integer()
          ,dispatch = [] :: cowboy_displatch:dispatch_rules()
         }).

%% API

-spec start_link(pid(), inet:socket(), module(), any()) -> {'ok', pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

%% FSM.

%% @private
-spec init(pid(), inet:socket(), module(), any()) -> 'ok'.
init(ListenerPid, Socket, Transport, Opts) ->
    lager:debug("init shout protocol"),

    SrvPid = props:get_value(file_server, Opts),
    lager:debug("file server at ~p", [SrvPid]),

    Ref = erlang:monitor(process, SrvPid),

    Timeout = props:get_value(timeout, Opts, ?MILLISECONDS_IN_DAY),
    Dispatch = props:get_value(dispatch, Opts, []),

    ok = cowboy:accept_ack(ListenerPid),
    wait_request(#state{listener=ListenerPid
                        ,socket=Socket
                        ,transport=Transport
                        ,file_server=SrvPid
                        ,file_server_ref=Ref
                        ,timeout=Timeout
                        ,dispatch=Dispatch
                       }).

-spec wait_request/1 :: (#state{}) -> 'ok'.
wait_request(#state{socket=Socket
                    ,transport=Transport
                    ,buffer=Buffer
                    ,timeout=Timeout
                   }=State) ->
    case Transport:recv(Socket, 0, Timeout) of
        {ok, Data} ->
            parse_request(State#state{
                            buffer = << Buffer/binary, Data/binary >>
                           });
        {error, _Reason} -> terminate(State)
    end.

parse_request(#state{buffer=Buff}=State) ->
    case binary:match(Buff, ?END_OF_REQUEST) of
        nomatch ->
            lager:debug("more data needed"),
            wait_request(State);
        _ ->
            lager:debug("data ready"),
            [Request | _] = binary:split(Buff, ?END_OF_REQUEST),
            request(Request, State#state{buffer = <<>>})
    end.

request(Request, #state{socket=Socket}=State) ->
    lager:debug("request: ~s", [Request]),

    %% find dispatcher by host/path?

    % send resp headers
    % send resp content
    ok.

handler_loop(_,_,_) ->
    ok.

-spec terminate/1 :: (#state{}) -> 'ok'.
terminate(#state{socket=Socket, transport=Transport}) ->
    Transport:close(Socket),
    ok.
