%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
                  ,?SUPER('kz_global_proxies_sup')
                  ,?WORKER('kazoo_globals_init')
                  ,?WORKER('kz_nodes')
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
    _ = kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 25,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
