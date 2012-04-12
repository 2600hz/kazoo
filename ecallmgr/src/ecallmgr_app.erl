-module(ecallmgr_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

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
