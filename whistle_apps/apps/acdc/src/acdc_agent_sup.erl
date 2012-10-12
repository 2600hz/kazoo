%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/1
         ,agent/1
         ,fsm/1, start_fsm/3
         ,stop/1
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
-spec start_link/1 :: (wh_json:json_object()) -> startlink_ret().
start_link(AgentJObj) ->
    case supervisor:start_link(?MODULE, [AgentJObj]) of
        {ok, Super}=OK ->
            acdc_agent_manager:new_agent(Super),
            OK;
        Other -> Other
    end.

-spec stop/1 :: (pid()) -> 'ok' | {'error', 'not_found'}.
stop(Supervisor) ->
    supervisor:terminate_child(acdc_agents_sup, Supervisor).

-spec agent/1 :: (pid()) -> pid() | 'undefined'.
agent(Super) ->
    case child_of_type(Super, acdc_agent) of
        [] -> undefined;
        [P] -> P
    end.

-spec fsm/1 :: (pid()) -> pid() | 'undefined'.
fsm(Super) ->
    case child_of_type(Super, acdc_agent_fsm) of
        [] -> undefined;
        [P] -> P
    end.

-spec start_fsm/3 :: (pid(), ne_binary(), ne_binary()) -> sup_startchild_ret().
start_fsm(Super, AcctId, AgentId) ->
    case fsm(Super) of
        undefined ->
            Parent = self(),
            supervisor:start_child(Super, ?CHILD(acdc_agent_fsm, [AcctId, AgentId, Parent]));
        P when is_pid(P) -> {ok, P}
    end.

-spec child_of_type/2 :: (pid(), atom()) -> list(pid()).
child_of_type(Super, T) ->
    [ Pid || {Type, Pid, worker, [_]} <- supervisor:which_children(Super), T =:= Type].

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

    {ok, {SupFlags, [?CHILD(acdc_agent, [self() | Args])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
