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

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include("whapps_call_command.hrl").
-include("whistle_apps.hrl").

-define(ORIGIN_BINDINGS, [[{'type', <<"account">>}
                          ]
                         ]).

-define(CACHE_GETBY_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

-define(CHILDREN, [?CACHE_ARGS(?WHAPPS_GETBY_CACHE, ?CACHE_GETBY_PROPS)
                   ,?WORKER('wh_nodes')
                   ,?WORKER('wh_hooks_listener')
                   ,?WORKER('wh_cache')
                   ,?WORKER('whistle_apps_init')
                   ,?WORKER('whapps_controller')
                   ,?SUPER('whistle_services_sup')
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
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec initialize_whapps(atoms()) -> {'error', any()} |
                                    {'ok', api_pid()} |
                                    {'ok', api_pid(), any()}.
initialize_whapps(Whapps) ->
    supervisor:start_child(?MODULE, ?SUPER_ARGS('whapps_sup', Whapps)).

start_child(Spec) ->
    supervisor:start_child(?MODULE, Spec).

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
    MaxRestarts = 25,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.
