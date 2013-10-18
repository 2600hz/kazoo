%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%% Root supervisor tree for stepswitch routing WhApp
%%% @end
%%%-------------------------------------------------------------------

-module(stepswitch_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").
-include("stepswitch.hrl").

-export([start_link/0]).
-export([init/1]).

-define(POOL(N), {N, {'poolboy', 'start_link', [[{'worker_module', 'stepswitch_cnam'}
                                                 ,{'name', {'local', N}}
                                                 ,{'size', 10}
                                                 ,{'max_overflow', 50}
                                                 ,{'neg_resp_threshold', 1}
                                                ]
                                               ]}
                  ,'permanent', 5000, 'worker', ['poolboy']
                 }).

-define(CHILDREN, [?CACHE(?STEPSWITCH_CACHE)
                   ,?POOL(?STEPSWITCH_CNAM_POOL)
                   ,?SUPER('stepswitch_request_sup')
                   ,?WORKER('stepswitch_listener')
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
