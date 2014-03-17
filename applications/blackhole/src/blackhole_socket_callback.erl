%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_socket_callback).

-include("blackhole.hrl").

-export([open/3
        ,recv/4
        ,close/3
        ]).

-record(state, {}).

open(SessionPid, SessionId, _Opts) ->
    lager:debug("opening socket ~p", [SessionId]),
    blackhole_resource:connect_socket(SessionId, SessionPid),
    {'ok', #state{}}.
 
recv(SessionPid, SessionId, {message, <<>>, Message}, State) ->
    lager:debug("received message ~p on socket ~p", [Message, SessionId]),
    blackhole_resource:handle_message(Message, SessionId, SessionPid),
    {'ok', State};
recv(SessionPid, SessionId, {event, <<>>, Event, Data}, State) ->
    lager:debug("received event: ~p on socket ~p with data payload", [Event, SessionId]),
    blackhole_resource:handle_event(Event, Data, SessionId, SessionPid),
    {'ok', State};
recv(_SessionPid, SessionId, Message, State) ->
    lager:info("receive unknown message ~p on socket ~p", [Message, SessionId]),
    {'ok', State}.

close(SessionPid, SessionId, _State) ->
    lager:debug("closing socket ~p", [SessionId]),
    blackhole_resource:disconnect_socket(SessionId, SessionPid).
