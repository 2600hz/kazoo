%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_sup).

-behaviour(supervisor).

-export([start_link/0
        ,listener/0
        ,shared_listener/0
        ]).
-export([init/1]).

-include("webhooks.hrl").

-define(SERVER, ?MODULE).

-define(ETSMGR_ARGS
       ,[[{'table_id', webhooks_util:table_id()}
         ,{'find_me_function', fun listener/0}
         ,{'table_options', webhooks_util:table_options()}
         ,{'gift_data', webhooks_util:gift_data()}
         ]]
       ).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?CACHE(?CACHE_NAME)
                  ,?WORKER_ARGS('kazoo_etsmgr_srv', ?ETSMGR_ARGS)
                  ,?WORKER('webhooks_disabler')
                  ,?WORKER('webhooks_listener')
                  ,?WORKER('webhooks_shared_listener')
                  ,?WORKER('webhooks_init')
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

-spec listener() -> kz_term:api_pid().
listener() ->
    case child_of_type(?SERVER, 'webhooks_listener') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec shared_listener() -> kz_term:api_pid().
shared_listener() ->
    case child_of_type(?SERVER, 'webhooks_shared_listener') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec child_of_type(pid() | atom(), atom()) -> kz_term:pids().
child_of_type(S, T) ->
    [P || {Ty, P, 'worker', _} <- supervisor:which_children(S),
          T =:= Ty
    ].

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
