%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_globals_sup).

-behaviour(supervisor).

-export([start_link/0
        ,init/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, ?MODULE).

-define(ETSMGR_ARGS
       ,[[{'table_id', kz_globals:table_id()}
         ,{'find_me_function', fun kz_globals:find_me/0}
         ,{'table_options', kz_globals:table_options()}
         ,{'gift_data', kz_globals:gift_data()}
         ]]
       ).

-define(CHILDREN, [?WORKER('kz_globals')
                  ,?WORKER_ARGS('kazoo_etsmgr_srv', ?ETSMGR_ARGS)
                  ,?WORKER('kazoo_globals_init')
                  ,?WORKER('kz_nodes')
                  ,?SUPER('kz_global_proxies_sup')
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
    MaxRestarts = 25,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
