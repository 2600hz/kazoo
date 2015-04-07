%% @private
%% @doc Supervisor for RADIUS server supervisor tree.
%%    This is a one_for_all supervisor because the server_mon must always die when the server_sup goes down, and vice-versa.
-module(eradius_server_top_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ------------------------------------------------------------------------------------------
%% -- supervisor callbacks
init([]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ServerSup = {sup, {eradius_server_sup, start_link, []}, permanent, infinity, supervisor, [eradius_server_sup]},
    ServerMon = {mon, {eradius_server_mon, start_link, []}, permanent, brutal_kill, worker, [eradius_server_mon]},

    {ok, {SupFlags, [ServerSup, ServerMon]}}.
