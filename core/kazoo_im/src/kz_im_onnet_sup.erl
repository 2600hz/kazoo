%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_im_onnet_sup).

-behaviour(supervisor).

-include("kazoo_im.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).

-export([worker/0]).

-export([init/1]).

-define(CHILDREN, [?WORKER_ARGS_TYPE('kz_im_onnet', [], 'temporary')]).

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
    _ = init_workers(Pid),
    {'ok', Pid}.

%%------------------------------------------------------------------------------
%% @doc Random Worker.
%% @end
%%------------------------------------------------------------------------------
-spec worker() -> {'error', 'no_connections'} | pid().
worker() ->
    Pids = gproc:lookup_pids({'p', 'l', 'im_onnet'}),
    case length(Pids) of
        0 -> {'error', 'no_connections'};
        Size ->
            Selected = rand:uniform(Size),
            lists:nth(Selected, Pids)
    end.

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
    {'ok', {SupFlags, ?CHILDREN}}.

init_workers(Pid) ->
    Workers = kapps_config:get_integer(?CONFIG_CAT, <<"onnet_listeners">>, 1),
    _ = kz_util:spawn(fun() -> [begin
                                    _ = supervisor:start_child(Pid, []),
                                    timer:sleep(500)
                                end
                                || _N <- lists:seq(1, Workers)
                               ]
                      end).
