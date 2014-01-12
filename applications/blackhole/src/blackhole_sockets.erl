%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_sockets).

-include("blackhole.hrl").

-export([send_event/3]).

send_event(Pids, Event, Data) ->
    lager:debug("going to send event data"),
    lists:foldl(
      fun(Pid, _) ->
              socketio_session:send_event(Pid, Event, Data)
      end
               ,'ok'
               ,Pids
     ).
