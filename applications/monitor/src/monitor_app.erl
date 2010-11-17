-module(monitor_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    monitor_master:start_link(),
    monitor_agent_network:start_link().
    %% monitor_sup:start_link().

stop(_State) ->
    ok.
