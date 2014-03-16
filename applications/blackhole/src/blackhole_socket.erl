%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_socket).

-include("blackhole.hrl").

-export([open/3
        ,recv/4
        ,close/3
        ]).

-record(state, {'listener'}).

-spec open(pid(), ne_binary(), wh_proplist()) -> 'ok'.
open(SessionPid, SessionId, _Opts) ->
    lager:debug("opening socket ~p", [SessionId]),
    blackhole_handler:connect_socket(SessionId, SessionPid),
    'ok'.
 
recv(_Pid, _SId, {message, <<>>, Message}, State) ->
    lager:debug("receive message ~p on socket ~p", [Message, _SId]),
    blackhole_handler:handle_message(Event, Data, SessionId, SessionPid),
    'ok';
recv(SessionPid, SessionId, {event, <<>>, Event, Data}, State) ->
    lager:debug("receive message ~p on socket ~p with event ~p", [Message, SessionId, Event]),
    blackhole_handler:handle_event(Event, Data, SessionId, SessionPid),
    'ok';
recv(_Pid, _SId, Message, State) ->
    lager:info("receive unknown message ~p on socket ~p", [Message, _SId]),
    'ok'.

-spec close(pid(), api_binary(), 
close(SessionPid, SessionId, _State) ->
    lager:debug("closing socket ~p", [SessionId]),
    blackhole_handler:disconnect_socket(SessionId, SessionPid),
    'ok'.
