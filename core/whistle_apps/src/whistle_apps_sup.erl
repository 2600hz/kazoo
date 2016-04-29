%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_apps_sup).

-behaviour(supervisor).

-export([start_link/0
         ,initialize_whapps/1
         ,init/1
         ,start_child/1
        ]).

-include("whapps_call_command.hrl").
-include("whistle_apps.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?SUPER('wh_hooks_listener_sup')
                   ,?WORKER('wh_nodes')
                   ,?WORKER('whistle_apps_init')
                   ,?WORKER('whapps_controller')
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

-spec initialize_whapps(atoms()) -> sup_startchild_ret().
initialize_whapps(Whapps) ->
    supervisor:start_child(?SERVER, ?SUPER_ARGS('whapps_sup', Whapps)).

start_child(Spec) ->
    supervisor:start_child(?SERVER, Spec).

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
    MaxRestarts = 25,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
