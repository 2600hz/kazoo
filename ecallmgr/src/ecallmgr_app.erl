-module(ecallmgr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Ref, Args), {Ref, {I, start_link, [{Ref, Args}]}, permanent, 5000, Type, [I]}).

-define(PIDFILE, "/ecallmgr.pid").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    _ = case code:priv_dir(ecallmgr) of
            {error, _} -> ok;
            Prefix -> wh_util:write_pid([Prefix, ?PIDFILE])
        end,
    ecallmgr:start_link().

stop(_State) ->
    ok = file:delete(?PIDFILE).
