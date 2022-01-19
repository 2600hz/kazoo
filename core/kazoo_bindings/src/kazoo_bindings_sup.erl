%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_bindings_sup).

-behaviour(supervisor).

-export([start_link/0
        ]).
-export([init/1]).

-include("kazoo_bindings.hrl").

-define(SERVER, ?MODULE).

-define(ID, 'kazoo_bindings').

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER_ARGS('kazoo_etsmgr_srv'
                               ,[
                                 [{'table_id', kazoo_bindings:table_id()}
                                 ,{'table_options', kazoo_bindings:table_options()}
                                 ,{'find_me_function', fun kazoo_bindings:find_me_function/0}
                                 ,{'gift_data', kazoo_bindings:gift_data()}
                                 ]
                                ])
                  ,?WORKER(?ID)
                  ,?WORKER('kazoo_bindings_init')
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
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
