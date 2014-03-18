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
emit(SessionPids, Event, Data) ->
    lager:debug("going to send event data"),
    lists:foldl(
      fun(SessionPid, _) ->
              socketio_session:send_event(SessionPid, Event, Data)
      end
               ,'ok'
               ,SessionPids
     ).
