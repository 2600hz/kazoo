
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
         ,get_v3_migrate_status/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("cdr.hrl").

-define(CACHE_PROPS, []).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec get_migrate_server_pid(atom()) -> api_pid().
get_migrate_server_pid(Super) ->
    case child_of_type(Super, 'cdr_v3_migrator') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec child_of_type(pid(), atom()) -> pids().
child_of_type(S, T) ->
    [P || {Ty, P, 'worker', _} <- supervisor:which_children(S), T =:= Ty].


-spec get_v3_migrate_status() -> 'ok'.
get_v3_migrate_status() ->
    ServerPid = get_migrate_server_pid(?MODULE),
    case {is_pid(ServerPid), cdr_v3_migrate_server:status(ServerPid)} of
        {'true', {'num_accounts_left', NumAccountsLeft}} ->
            io:format("cdr_v3_migrator: accts remaining: ~p~n", [NumAccountsLeft]);
        {'true', {'error', _E}} ->
            io:format("cdr_v3_migrator: error ~p~n", [_E]);
        {'true', _E} ->
            io:format("cdr_v3_migrator: no status~p~n", [_E]);
        {'false', _} ->
            io:format("cdr_v3_migrator: migrate server pid not found~n")
    end.

-spec start_v3_migrate() -> 'ok' | wh_std_return().
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
-spec stop_v3_migrate() -> 'ok' | wh_std_return().
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
