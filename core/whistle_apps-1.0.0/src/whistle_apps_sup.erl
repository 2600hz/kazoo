%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_apps_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include("whapps_call_command.hrl").
-include("whistle_apps.hrl").

-export([start_link/0]).
-export([initialize_whapps/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

-define(POOL_SIZE, 100).
-define(OVERFLOW_POOL_SIZE, 100).

-define(ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                          ,[{'db', ?WH_CONFIG_DB}]
                         ]).
-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).


-define(POOL(N), {N, {'poolboy', 'start_link', [[{'worker_module', 'wh_amqp_worker'}
                                                 ,{'name', {'local', N}}
                                                 ,{'size', ?POOL_SIZE}
                                                 ,{'max_overflow', ?OVERFLOW_POOL_SIZE}
                                                 ,{'neg_resp_threshold', 1}
                                                ]
                                               ]}
                  ,'permanent', 5000, 'worker', ['poolboy']
                 }).

-define(WHAPPS(Whapps), {'whapps_sup', {'whapps_sup', 'start_link', [Whapps]}, 'permanent', 5000, 'supervisor', ['whapps_sup']}).
-define(CHILDREN, [?WORKER('wh_nodes')
                   ,?WORKER('wh_cache')
                   ,?CACHE_ARGS(?WHAPPS_CONFIG_CACHE, ?CACHE_PROPS)
                   ,?SUPER('whistle_couch_sup')
                   ,?WORKER('whistle_apps_init')
                   ,?CACHE_ARGS(?WHAPPS_CALL_CACHE, ?CACHE_PROPS)
                   ,?POOL(?WHAPPS_AMQP_POOL)
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

-spec initialize_whapps(atoms()) -> {'error', term()} | 
                                    {'ok','undefined' | pid()} | 
                                    {'ok','undefined' | pid(), term()}.
initialize_whapps(Whapps) ->
    supervisor:start_child(?SERVER, ?WHAPPS(Whapps)).    

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
