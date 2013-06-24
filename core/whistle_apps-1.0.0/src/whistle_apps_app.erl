-module(whistle_apps_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(PIDFILE, [code:priv_dir('whistle_apps'), "/whistle_apps.pid"]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    _ = wh_util:write_pid(?PIDFILE),
    whistle_apps:start_link().

stop(_State) ->
    'ok' = file:delete(?PIDFILE).
