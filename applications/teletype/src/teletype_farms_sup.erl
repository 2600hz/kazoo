%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_farms_sup).
-behaviour(supervisor).

-export([start_link/0
        ,render_farm_name/0
        ]).
-export([init/1]).

-include("teletype.hrl").

-define(SERVER, ?MODULE).

-define(POOL_NAME, 'teletype_render_farm').
-define(POOL_SIZE, kapps_config:get_integer(?APP_NAME, <<"render_farm_workers">>, 50)).
-define(POOL_OVERFLOW, 50).

-define(POOL_ARGS, [[{'worker_module', 'teletype_renderer'}
                    ,{'name', {'local', ?POOL_NAME}}
                    ,{'size', ?POOL_SIZE}
                    ,{'max_overflow', ?POOL_OVERFLOW}
                    ]]).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?CACHE(?CACHE_NAME)
                  ,?WORKER_NAME_ARGS('poolboy', ?POOL_NAME, ?POOL_ARGS)
                  ,?WORKER('teletype_bindings')
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

-spec render_farm_name() -> ?POOL_NAME.
render_farm_name() ->
    ?POOL_NAME.

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
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
