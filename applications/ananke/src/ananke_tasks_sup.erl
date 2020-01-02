%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ananke_tasks_sup).
-behaviour(supervisor).

-export([start_link/0
        ,start_task/3
        ,delete_child/1
        ,delete_child/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("ananke.hrl").

-define(SERVER, ?MODULE).
-define(CHILDREN, []).
-define(TASK_WORKER_SPEC(N, I, Args)
       ,{N, {I, 'start_link', Args}, 'transient', 'brutal_kill', 'worker', [I]}).

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

-spec start_task(any(), atom(), list()) -> ok | kz_types:sup_startchild_ret().
start_task(Id, Module, Args) ->
    case supervisor:start_child(?SERVER, ?TASK_WORKER_SPEC(Id, Module, Args)) of
        {'error', 'already_present'} ->
            _ = supervisor:delete_child(?SERVER, Id),
            start_task(Id, Module, Args);
        {'ok', _P} -> lager:debug("child ~p started", [_P]);
        {'ok', _P, _} -> lager:debug("child ~p started", [_P]);
        {'error', {'already_started', _P}} -> lager:info("child ~p already started", [_P]);
        {'error', Error} -> lager:warning("error start child: ~p", [Error])
    end.


-spec delete_child(any()) -> 'ok' | {'error', any()}.
delete_child(Pid) when is_pid(Pid) ->
    case [Id || {Id, Child, _Type, _Modules} <- supervisor:which_children(?SERVER),
                Child =:= Pid
         ]
    of
        [] -> 'ok';
        [Id] -> delete_child(Id)
    end;
delete_child(Id) ->
    supervisor:delete_child(?SERVER, Id).

-spec delete_child(any(), non_neg_integer()) -> 'ok'.
delete_child(Id, Timeout) ->
    _ = kz_process:spawn(delete_child_after_timeout(Id, Timeout)),
    'ok'.

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
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_child_after_timeout(any(), non_neg_integer()) ->
          fun(() -> 'ok' | {'error', any()}).
delete_child_after_timeout(Id, Timeout) ->
    fun() ->
            timer:sleep(Timeout),
            delete_child(Id)
    end.
