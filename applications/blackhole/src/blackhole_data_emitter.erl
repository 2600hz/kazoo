%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_data_emitter).

-include("blackhole.hrl").

-export([emit/3]).

-spec emit([pid()] | pid(), api_binary(), kz_json:object()) -> 'ok'.
emit(SessionPid, Event, Data) when is_pid(SessionPid) ->
    lager:debug("sending event data: ~s", [Event]),
    SessionPid ! {'send_event', Event, Data};

emit(SessionPids, Event, Data) when is_list(SessionPids) ->
    _ = [emit(SessionPid, Event, Data) || SessionPid <- SessionPids],
    'ok'.
