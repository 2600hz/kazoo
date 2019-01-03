%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author OnNet (Kirill Sysoev github.com/onnet)
%%% @end
%%%-----------------------------------------------------------------------------
-module(cccp_platform_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,new/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("cccp.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_TYPE('cccp_platform_listener', 'temporary')]).

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

-spec new(kapps_call:call()) -> kz_types:sup_startchild_ret().
new(Call) ->
    supervisor:start_child(?SERVER, [Call]).

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
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
