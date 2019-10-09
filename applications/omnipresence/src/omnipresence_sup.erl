%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(omnipresence_sup).

-behaviour(supervisor).

-export([start_link/0
        ,subscriptions_srv/0
        ]).
-export([init/1]).

-include("omnipresence.hrl").

-define(SERVER, ?MODULE).

-define(SUBS_ETS_OPTS, [{'table_id', omnip_subscriptions:table_id()}
                       ,{'table_options', omnip_subscriptions:table_config()}
                       ,{'find_me_function', fun subscriptions_srv/0}
                       ]).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER_NAME_ARGS('kazoo_etsmgr_srv', 'omnipresence_subscriptions_tbl', [?SUBS_ETS_OPTS])
                  ,?WORKER('omnip_subscriptions')
                  ,?WORKER('omnipresence_listener')
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

-spec subscriptions_srv() -> kz_term:api_pid().
subscriptions_srv() ->
    case [P || {_, P, 'worker', ['omnip_subscriptions']} <- supervisor:which_children(?SERVER)] of
        [] -> 'undefined';
        [Pid] -> Pid
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
    _ = kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
