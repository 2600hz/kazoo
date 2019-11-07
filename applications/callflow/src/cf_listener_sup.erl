%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_listener_sup).

-behaviour(supervisor).

-include("callflow.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1]).
-export([forward/1]).

-define(CHILDREN(I), [?WORKER_ARGS_TYPE('cf_listener', [I], 'temporary')]).

%% ===================================================================
%% API functions
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    {'ok', Pid} = supervisor:start_link({'local', ?SERVER}, ?MODULE, []),
    Workers = kapps_config:get_integer(?CF_CONFIG_CAT, <<"callflow_listeners">>, 5),
    _ = kz_process:spawn(fun() -> [begin
                                       _ = supervisor:start_child(Pid, []),
                                       timer:sleep(500)
                                   end
                                   || _N <- lists:seq(1, Workers)
                                  ]
                         end),
    {'ok', Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Instance = kz_binary:rand_hex(16),
    {'ok', {SupFlags, ?CHILDREN(Instance)}}.

-spec forward(term()) -> any().
forward(Msg) ->
    Listeners = supervisor:which_children(?MODULE),
    Size = length(Listeners),
    Selected = rand:uniform(Size),
    {_, Listener, _, _} = lists:nth(Selected, Listeners),
    Listener ! {forward, Msg}.
