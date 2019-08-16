%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_token_buckets_sup).

-behaviour(supervisor).

-export([start_link/0
        ,buckets_srv/0
        ]).
-export([init/1]).

-include("kz_buckets.hrl").

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?SUPER('kz_buckets_sup')
                  ,?WORKER_ARGS('kazoo_etsmgr_srv'
                               ,[
                                 [{'table_id', kz_buckets:table_id()}
                                 ,{'table_options', kz_buckets:table_options()}
                                 ,{'find_me_function', fun buckets_srv/0}
                                 ,{'gift_data', kz_buckets:gift_data()}
                                 ]
                                ])
                  ,?WORKER('kz_buckets')
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

-spec buckets_srv() -> kz_term:api_pid().
buckets_srv() ->
    case [P || {'kz_buckets', P, _, _} <- supervisor:which_children(?SERVER)] of
        [] -> 'undefined';
        [P] -> P
    end.

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
