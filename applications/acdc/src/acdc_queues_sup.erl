%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queues_sup).

-behaviour(supervisor).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0
        ,new/2
        ,workers/0
        ,find_acct_supervisors/1
        ,find_queue_supervisor/2
        ,queues_running/0
        ,status/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?SUPER_TYPE('acdc_queue_sup', 'transient')]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_term:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec new(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:startlink_ret().
new(AccountId, QueueId) ->
    case find_queue_supervisor(AccountId, QueueId) of
        P when is_pid(P) -> {'ok', P};
        'undefined' -> supervisor:start_child(?SERVER, [AccountId, QueueId])
    end.

-spec workers() -> kz_term:pids().
workers() ->
    [Pid || {_, Pid, 'supervisor', _} <- supervisor:which_children(?SERVER), is_pid(Pid)].

-spec find_acct_supervisors(kz_term:ne_binary()) -> kz_term:pids().
find_acct_supervisors(AccountId) ->
    [Super || Super <- workers(), is_queue_in_acct(Super, AccountId)].

-spec is_queue_in_acct(pid(), kz_term:ne_binary()) -> boolean().
is_queue_in_acct(Super, AccountId) ->
    case catch acdc_queue_manager:config(acdc_queue_sup:manager(Super)) of
        {'EXIT', _} -> 'false';
        {AccountId, _} -> 'true';
        _ -> 'false'
    end.

-spec find_queue_supervisor(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_pid().
find_queue_supervisor(AccountId, QueueId) ->
    find_queue_supervisor(AccountId, QueueId, workers()).

-spec find_queue_supervisor(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:pids()) -> kz_term:api_pid().
find_queue_supervisor(_AccountId, _QueueId, []) -> 'undefined';
find_queue_supervisor(AccountId, QueueId, [Super|Rest]) ->
    case catch acdc_queue_manager:config(acdc_queue_sup:manager(Super)) of
        {'EXIT', _} -> find_queue_supervisor(AccountId, QueueId, Rest);
        {AccountId, QueueId} -> Super;
        _ -> find_queue_supervisor(AccountId, QueueId, Rest)
    end.

-spec status() -> 'ok'.
status() ->
    ?PRINT("ACDc Queues Status"),
    Ws = workers(),
                                                %    _ = kz_process:spawn(fun() -> lists:foreach(fun acdc_queue_sup:status/1, Ws) end),
    lists:foreach(fun acdc_queue_sup:status/1, Ws),
    'ok'.

-spec queues_running() -> [{pid(), any()}].
queues_running() ->
    [{W, catch acdc_queue_manager:config(acdc_queue_sup:manager(W))} || W <- workers()].

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
