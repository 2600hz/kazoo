-module(whistle_amqp_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    wh_amqp_sup:start_link().

-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
