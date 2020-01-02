%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Supervisor for Kazoo Token Bucket Servers
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_buckets_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,start_bucket/3
        ,stop_bucket/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("kz_buckets.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_TYPE('kz_token_bucket', 'temporary')]).

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

-spec start_bucket(pos_integer(), pos_integer(), kz_token_bucket:fill_rate_time()) ->
          kz_types:sup_startchild_ret().
start_bucket(MaxTokens, FillRate, FillTime) ->
    supervisor:start_child(?SERVER, [MaxTokens, FillRate, 'true', FillTime]).

-spec stop_bucket(kz_types:server_ref()) -> 'ok' | {'error', any()}.
stop_bucket(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

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
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
