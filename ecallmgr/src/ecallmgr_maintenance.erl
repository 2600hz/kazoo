%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%%
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_maintenance).

-export([show_calls/0]).

-spec show_calls/0 :: () -> 'ok'.
show_calls() ->
    EventWorkers = ecallmgr_call_event_sup:workers(),
    io:format("Call Event Process: ~p ~n", [length(EventWorkers)]),
    _ = [io:format("    ~p: ~s ~s~n", [EventWorker
                                       ,ecallmgr_call_events:node(EventWorker)
                                       ,ecallmgr_call_events:callid(EventWorker)
                                      ])
         || EventWorker <- EventWorkers],
    ControlWorkers = ecallmgr_call_control_sup:workers(),
    io:format("Call Control Process: ~p~n", [length(ControlWorkers)]),
    _ = [io:format("    ~p: ~s ~s~n", [ControlWorker
                                       ,ecallmgr_call_control:node(ControlWorker)
                                       ,ecallmgr_call_control:callid(ControlWorker)
                                      ])
         || ControlWorker <- ControlWorkers],    
    ok.
    
