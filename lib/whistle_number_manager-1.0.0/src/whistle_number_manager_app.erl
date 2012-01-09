-module(whistle_number_manager_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    whistle_number_manager_sup:start_link().

stop(_State) ->
    ok.
