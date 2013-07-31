%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cdr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
         ,start_v3_migrate/0
         ,stop_v3_migrate/0
         ,get_v3_migrate_status/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("cdr.hrl").

-define(CACHE_PROPS, []).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec migrate_server(pid()) -> api_pid().
migrate_server(Super) ->
    hd([P || {_, P, 'worker', _} <- supervisor:which_children(Super)]).

get_v3_migrate_status(Supervisor) ->
    ServerPid = migrate_server(Supervisor),
    case cdr_v3_migrate_server:status(ServerPid) of
        {'number_of_accounts', NumAccountsLeft}} ->
            lager:info("Migrate Status - Accounts Remaining: ~s", [NumAccountsLeft]);
        _ -> lager:debug("No Response from status request")                       
    end.

start_v3_migrate() ->
    ChildSpec = {'cdr_v3_migrator'
                 ,{'cdr_v3_migrate_server', 'start_link', []}
                 ,'transient'
                 ,5000
                 ,'worker'
                 ,['cdr_v3_migrate_server']
                },
    case supervisor:start_child(?MODULE, ChildSpec) of
        {'error', 'already_present'} -> 
            supervisor:restart_child(?MODULE, 'cdr_v3_migrator');
        {'error', _E} -> lager:debug("error starting cdr_v3_migrate: ~p", [_E]);
        {'ok', _} -> 'ok'
    end.

stop_v3_migrate() ->
    supervisor:terminate_child(?MODULE, 'cdr_v3_migrator'),
    supervisor:delete_child(?MODULE, 'cdr_v3_migrator').

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, [?WORKER('cdr_listener')]}}.
