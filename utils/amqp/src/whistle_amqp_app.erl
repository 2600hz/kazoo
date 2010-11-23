-module(whistle_amqp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    whistle_amqp:start_link().

stop(_State) ->
    ok.
