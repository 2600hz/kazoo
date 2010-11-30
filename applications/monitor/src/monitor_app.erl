-module(monitor_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("../include/monitor_amqp.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    monitor:start_link(?AMQP_HOST).

stop(_State) ->
    ok.
