%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz, INC
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
-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}
                      ,'new_node_flush'
                      ,'channel_reconnect_flush'
                     ]).

-define(CHILDREN, [?CACHE_ARGS(?WH_COUCH_CACHE, ?CACHE_PROPS)
                   ,?SUPER('wh_couch_connection_sup')
                   ,?SUPER('wh_change_handler_sup')
                   ,?WORKER('wh_couch_connections')
                   ,?WORKER('wh_couch_bootstrap')
                   ,?WORKER('couch_compactor_fsm')
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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
