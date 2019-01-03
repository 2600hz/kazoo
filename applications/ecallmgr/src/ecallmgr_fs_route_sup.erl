%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_route_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-export([start_link/1, start_link/2]).
-export([init/1]).

-define(CHILDREN(Args), [?WORKER_ARGS_TYPE('ecallmgr_fs_route', Args, 'transient')
                        ,?WORKER_ARGS_TYPE('ecallmgr_fs_router_call', Args, 'transient')
                        ,?WORKER_ARGS_TYPE('ecallmgr_fs_router_text', Args, 'transient')
                        ]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------

-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Node) -> start_link(Node, []).

-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) ->
    supervisor:start_link(?MODULE, [Node, Options]).

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
init(Args) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN(Args)}}.
