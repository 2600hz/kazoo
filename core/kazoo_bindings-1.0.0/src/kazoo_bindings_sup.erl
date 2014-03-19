%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_bindings_sup).

-behaviour(supervisor).

-export([start_link/0
         ,stop/0
        ]).
-export([init/1]).

-include("kazoo_bindings.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER('kazoo_bindings')
                   ,?WORKER_ARGS('kazoo_etsmgr_srv', [[{'table_id', kazoo_bindings:table_id()}
                                                       ,{'table_options', kazoo_bindings:table_options()}
                                                       ,{'find_me_function', fun kazoo_bindings:find_me_function/0}
                                                       ,{'gift_data', kazoo_bindings:gift_data()}
                                                      ]])
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

-spec stop() -> 'ok' | {'error', 'not_found'}.
stop() ->
    supervisor:terminate_child(?MODULE).

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
