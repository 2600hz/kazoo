%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(caller10_contest_sup).

-behaviour(supervisor).

-include("caller10.hrl").

%% API
-export([start_link/1
         ,listener/1
         ,fsm/1
         ,stop/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link(caller10_contests:contest()) -> startlink_ret().
start_link(Contest) -> supervisor:start_link(?MODULE, [Contest]).

-spec stop(pid()) -> 'ok' | {'error', 'not_found'}.
stop(Supervisor) ->
    supervisor:terminate_child('caller10_contests_sup', Supervisor).

-spec listener(pid()) -> api_pid().
listener(Super) ->
    case child_of_type(Super, 'caller10_contest_listener') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec fsm(pid()) -> api_pid().
fsm(Super) ->
    case child_of_type(Super, 'caller10_contest_fsm') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec child_of_type(pid(), atom()) -> pids().
child_of_type(S, T) ->
    [P
     || {Ty, P, 'worker', _} <- supervisor:which_children(S),
        T =:= Ty
    ].

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init(list()) -> sup_init_ret().
init(Args) ->
    RestartStrategy = 'rest_for_one',
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 2,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [?WORKER_ARGS('caller10_contest_listener', [self() | Args])
                       ,?WORKER_ARGS('caller10_contest_fsm', [self() | Args])
                      ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
