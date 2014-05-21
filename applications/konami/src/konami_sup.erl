%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(konami_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("konami.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER_ARGS('kazoo_etsmgr_srv', [[{'table_id', konami_tracker:table_id()}
                                                      ,{'table_options', konami_tracker:table_options()}
                                                      ,{'find_me_function', fun konami_tracker:find_me_function/0}
                                                      ,{'gift_data', 'ok'}
                                                     ]])
                   ,?WORKER('konami_tracker')
                   ,?WORKER('konami_listener')
                   ,?WORKER('konami_event_listener')
                   ,?WORKER('konami_init')
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
