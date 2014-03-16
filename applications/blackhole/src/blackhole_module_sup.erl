-module(blackhole_module_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
         ,add_module_supervisor/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("blackhole.hrl").

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec add_module_supervisor() -> 'ok' | wh_std_return().
add_module_supervisor(SupervisorModuleName) ->
    ChildSpec = {SupervisorModuleName
                ,{SupervisorModuleName, 'start_link', []}
                ,'permanent'
                ,5000
                ,'supervisor'
                ,[SupervisorModuleName]
                },
    case supervisor:start_child(?MODULE, ChildSpec) of
        {'error', 'already_present'} ->
            supervisor:restart_child(?MODULE, SupervisorModuleName);
        {'error', _E} -> lager:debug("error starting ~p: ~p", [SupervisorModuleName, _E]);
        {'ok', _} -> 'ok'
    end.
    
