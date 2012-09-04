%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/2
         ,queue/0
         ,shared_queue/0, start_shared_queue/4
         ,fsm/0, start_fsm/2
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Args),
        {Name, {Name, start_link, Args}, transient, 5000, worker, [Name]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/2 :: (ne_binary(), wh_json:json_object()) -> startlink_ret().
start_link(AcctDb, QueueJObj) ->
    supervisor:start_link(?MODULE, [AcctDb, QueueJObj]).

-spec queue/0 :: () -> pid() | 'undefined'.
queue() ->
    case child_of_type(acdc_queue) of
        [] -> undefined;
        [P] -> P
    end.

-spec shared_queue/0 :: () -> pid() | 'undefined'.
shared_queue() ->
    case child_of_type(acdc_queue_shared) of
        [] -> undefined;
        [P] -> P
    end.

-spec start_shared_queue/4 :: (pid(), pid(), ne_binary(), ne_binary()) -> sup_startchild_ret().
start_shared_queue(Super, FSMPid, AcctId, QueueId) ->
    supervisor:start_child(Super, ?CHILD(acdc_queue_shared, [FSMPid, AcctId, QueueId])).

-spec fsm/0 :: () -> pid() | 'undefined'.
fsm() ->
    case child_of_type(acdc_queue_fsm) of
        [] -> undefined;
        [P] -> P
    end.

-spec start_fsm/2 :: (pid(), wh_json:json_object()) -> sup_startchild_ret().
start_fsm(Super, QueueJObj) ->
    Parent = self(),
    supervisor:start_child(Super, ?CHILD(acdc_queue_fsm, [Parent, QueueJObj])).

-spec child_of_type/1 :: (atom()) -> list(pid()).
child_of_type(T) ->
    [ Pid || {Type, Pid, worker, [_]} <- supervisor:which_children(?MODULE), T =:= Type].

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init/1 :: (list()) -> sup_init_ret().
init(Args) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 2,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [?CHILD(acdc_queue, [self() | Args])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
