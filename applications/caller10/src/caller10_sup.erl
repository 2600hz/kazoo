%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(caller10_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("caller10.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?CACHE('caller10_cache')
                   ,?WORKER('caller10_listener')
                   ,?WORKER('caller10_contests')
                   ,?WORKER_ARGS('kazoo_etsmgr_srv', [[{'table_id', caller10_contests:table_id()}
                                                       ,{'table_options', caller10_contests:table_options()}
                                                       ,{'find_me_function', fun caller10_contests:find_me_function/0}
                                                       ,{'gift_data', caller10_contests:gift_data()}
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
