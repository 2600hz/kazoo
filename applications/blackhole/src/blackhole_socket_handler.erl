%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
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

open(_Pid, _SId, _Opts) ->
    lager:debug("opening socket ~p", [_SId]),
    {'ok', #state{}}.

%% Msg
recv(_Pid, _SId, {message, <<>>, Message}, State) ->
    lager:debug("receive message ~p on socket ~p", [Message, _SId]),
    {'ok', State};
%% Custom Events
recv(Pid, _SId, {event, <<>>, Event, Message}, State) ->
    lager:debug("receive message ~p on socket ~p with event ~p", [Message, _SId, Event]),
    {'ok', handle_event(Event, Message, Pid)};
%% Catch all
recv(_Pid, _SId, Message, State) ->
    lager:info("receive unknown message ~p on socket ~p", [Message, _SId]),
    {'ok', State}.

close(_Pid, _SId, #state{listener='undefined'}) ->
    lager:debug("closing socket ~p", [_SId]),
    'ok';
close(_Pid, _SId, #state{listener=PidListener}) ->
    lager:debug("closing socket ~p", [_SId]),
    'ok' = gen_server:call(PidListener, {'disconnect_socket'}).

handle_event(Event, Data, SessionPid) ->
    blackhole_dispatcher:handle_event(Event, Data, SessionPid);
%% Unknown Event
handle_event(Event, Data, SessionPid) ->
    lager:info("receive unknown event ~p", [Event]),
    Unknown = [{<<"event">>, Event}
              ,{<<"data">>, Data}
              ],
    blackhole_data_emitter:emit(SessionPid, <<"unknown_event">>, Unknown).
