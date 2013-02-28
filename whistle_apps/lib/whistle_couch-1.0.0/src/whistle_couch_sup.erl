%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_couch_sup).

-behaviour(supervisor).

-include_lib("wh_couch.hrl").

-export([start_link/0
         ,init/1
         ,compactor_pid/0
        ]).

-define(ORIGIN_BINDINGS, [[]]).
-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).
-define(CHILD(Name, Type), fun(N, 'cache') -> {N, {'wh_cache', 'start_link', [N, ?CACHE_PROPS]}
                                               ,'permanent', 5000, 'worker', ['wh_cache']};
                              (N, 'supervisor'=T) -> {N, {N, 'start_link', []}, 'permanent', 'infinity', T, [N]};
                              (N, T) -> {N, {N, 'start_link', []}, 'permanent', 5000, T, [N]} end(Name, Type)).

-define(CHILDREN, [{?WH_COUCH_CACHE, 'cache'}
                   ,{'wh_couch_connection_sup', 'supervisor'}
                   ,{'wh_change_handler_sup', 'supervisor'}
                   ,{'wh_couch_connections', 'worker'}
                   ,{'wh_couch_bootstrap', 'worker'}
                   ,{'couch_compactor_fsm', 'worker'}
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() -> supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec compactor_pid() -> pid() | 'undefined'.
compactor_pid() ->
    case [P || {'couch_compactor_fsm', P, 'worker', _} <- supervisor:which_children('whistle_couch_sup')] of
        [Pid] -> Pid;
        [] -> 'undefined'
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> sup_init_ret().
init([]) ->
    _ = whistle_couch:start_deps(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = [?CHILD(Name, Type) || {Name, Type} <- ?CHILDREN],

    {'ok', {SupFlags, Children}}.
