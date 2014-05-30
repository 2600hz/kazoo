%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(doodle_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("doodle.hrl").

%% Helper macro for declaring children of supervisor

-define(POOL(N),  {N, {'poolboy', 'start_link', [[{'name', {'local', N}}
                                                  ,{'worker_module', 'doodle_worker'}
                                                  ,{'size', whapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 5)}
                                                  ,{'max_overflow', 0}
                                                 ]]}
                   ,'permanent', 5000, 'worker', ['poolboy']}).

-define(CHILDREN, [?POOL('doodle_worker_pool')
                   ,?CACHE('doodle_cache')
                   ,?WORKER('doodle_jobs')
                   ,?WORKER('doodle_listener')
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
