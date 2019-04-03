%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(tasks_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("tasks.hrl").

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER('kz_tasks_scheduler')
                  ,?WORKER('tasks_listener')
                   %% Jobs not generating CSV output
                  ,?WORKER('kz_tasks_trigger')
                   %% Numbers
                  ,?WORKER('knm_port_request_crawler')
                  ,?WORKER('knm_number_crawler')
                   %% DB
                  ,?WORKER('kz_account_crawler')
                   %% Standalone tasks
                  ,?WORKER('kz_notify_resend')
                   %% Compaction jobs reporter
                  ,?WORKER('kt_compaction_reporter')
                  ]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
