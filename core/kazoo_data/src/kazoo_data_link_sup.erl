%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_data_link_sup).

-behaviour(supervisor).

-include("kz_data.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0
        ,init/1
        ]).

-ifdef(TEST).

-define(CACHE_PROPS, []).
-define(DP_CACHE_PROPS, []).

-else.

-define(ORIGIN_BINDINGS, [[]]).
-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

-define(DP_ORIGIN_BINDINGS, [[{'db', ?KZ_DATA_DB}]]).
-define(DP_CACHE_PROPS, [{'origin_bindings', ?DP_ORIGIN_BINDINGS}]).

-endif.

-define(CHILDREN, [?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)
                  ,?CACHE_ARGS(?KAZOO_DATA_PLAN_CACHE, ?DP_CACHE_PROPS)
                  ,?SUPER('kz_dataconnection_sup')
                  ,?WORKER('kz_dataconnections')
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
