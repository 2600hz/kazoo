%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Manage onnet calls
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ts_onnet_sup).
-behaviour(supervisor).

%% API
-export([start_link/0
        ,start_handler/3
        ,stop_handler/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, []).

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

-spec start_handler(kz_term:ne_binary(), kapi_route:req(), pid()) -> kz_types:sup_startchild_ret().
start_handler(CallID, RouteReqJObj, AMQPWorker) ->
    supervisor:start_child(?SERVER, ?WORKER_NAME_ARGS_TYPE(<<"onnet-", CallID/binary>>
                                                          ,'ts_from_onnet'
                                                          ,[RouteReqJObj, AMQPWorker]
                                                          ,'temporary'
                                                          )).

-spec stop_handler(kz_term:ne_binary()) -> 'ok'.
stop_handler(CallID) ->
    'ok' = supervisor:terminate_child(?SERVER, <<"onnet-", CallID/binary>>),
    _ = supervisor:delete_child(?SERVER, <<"onnet-", CallID/binary>>),
    'ok'.

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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
