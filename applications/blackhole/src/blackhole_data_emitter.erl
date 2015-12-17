%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_data_emitter).

-include("blackhole.hrl").

-export([emit/3]).

-spec emit([pid()] | pid(), api_binary(), wh_json:object()) -> 'ok'.
emit(SessionPid, Event, Data) when is_pid(SessionPid) ->
    lager:debug("sending event data: ~s", [Event]),
    socketio_session:send_event(SessionPid, Event, Data);
emit(SessionPids, Event, Data) when is_list(SessionPids) ->
    _ = [socketio_session:send_event(SessionPid, Event, Data) || SessionPid <- SessionPids],
    'ok'.
