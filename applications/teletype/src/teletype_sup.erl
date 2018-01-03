%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(teletype_sup).
-behaviour(supervisor).

-export([start_link/0
        ]).
-export([init/1]).

-include("teletype.hrl").

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER('teletype_listener')
                  ,?WORKER('teletype_shared_listener')
                  ,?SUPER(teletype_farms_sup)
                  ,?WORKER('teletype_bindings')
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
    kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
