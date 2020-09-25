%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_events_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("kazoo_events.hrl").
-include("kz_hooks.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER('kz_hooks_listener')
                  ,?WORKER('kz_hooks_shared_listener')
                  ,?CACHE(?HOOKS_CACHE_NAME)
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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 25,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
