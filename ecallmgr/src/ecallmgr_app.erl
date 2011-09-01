-module(ecallmgr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Ref, Args), {Ref, {I, start_link, [{Ref, Args}]}, permanent, 5000, Type, [I]}).

-define(PIDFILE, [code:priv_dir(ecallmgr), "/ecallmgr.pid"]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    wh_util:write_pid(?PIDFILE),
    ecallmgr:start_link().

stop(_State) ->
    ok = file:delete(?PIDFILE).
