-module(kazoo_amqp_app).

-behaviour(application).

-include_lib("kazoo/include/kz_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    kz_amqp_sup:start_link().

-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
