%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Manage the bucket servers
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_stats_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,stats_srv/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_NAME_ARGS('acdc_stats_etsmgr', 'acdc_stats_call', [acdc_stats:call_table_id(), acdc_stats:call_table_opts()])
                  ,?WORKER_NAME_ARGS('acdc_stats_etsmgr', 'acdc_stats_status', [acdc_agent_stats:status_table_id(), acdc_agent_stats:status_table_opts()])
                  ,?WORKER('acdc_stats')
                  ]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec stats_srv() ->
                       {'ok', pid()} |
                       {'error', 'not_found'}.
stats_srv() ->
    case [P || {'acdc_stats', P, _, _} <- supervisor:which_children(?SERVER)] of
        [P] when is_pid(P) -> {'ok', P};
        _ -> {'error', 'not_found'}
    end.

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
