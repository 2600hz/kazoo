%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_couch_sup).

-behaviour(supervisor).

-include("wh_couch.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0
         ,init/1
         ,compactor_pid/0
        ]).

-define(ORIGIN_BINDINGS, [[]]).
-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}
                      ,'new_node_flush'
                      ,'channel_reconnect_flush'
                     ]).

-define(CHILDREN, [?WORKER('whistle_couch_init')
                   ,?CACHE_ARGS(?WH_COUCH_CACHE, ?CACHE_PROPS)
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
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec compactor_pid() -> api_pid().
compactor_pid() ->
    case [P || {'couch_compactor_fsm', P, 'worker', _} <- supervisor:which_children(?SERVER)] of
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
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
