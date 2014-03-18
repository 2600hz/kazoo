%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_data_emitter).

-include("blackhole.hrl").

-export([emit/3]).

-spec emit(pids(), api_binary(), json:object()) -> 'ok'.
emit(SessionPid, Event, Data) when is_pid(SessionPid) ->
    socketio_session:send_event(SessionPid, Event, Data);
emit([_|_]=SessionPids, Event, Data) ->
    lager:debug("event: ~s: Data: ~p", [Event, Data),
    [socketio_session:send_event(SessionPid, Event, Data) || SessionPid <- SessionPids].
