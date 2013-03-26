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
-export([start_link/1, start_link/2
         ,agent/1
         ,fsm/1, start_fsm/3, start_fsm/4
         ,stop/1
         ,status/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Args),
        {Name, {Name, 'start_link', Args}, 'transient', 5000, 'worker', [Name]}).

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
-spec start_link(wh_json:object()) -> startlink_ret().
-spec start_link(whapps_call:call(), ne_binary()) -> startlink_ret().
start_link(AgentJObj) -> supervisor:start_link(?MODULE, [AgentJObj]).

start_link(ThiefCall, QueueId) -> supervisor:start_link(?MODULE, [ThiefCall, QueueId]).

-spec stop(pid()) -> 'ok' | {'error', 'not_found'}.
stop(Supervisor) -> supervisor:terminate_child('acdc_agents_sup', Supervisor).

-spec status(pid()) -> 'ok'.
status(Supervisor) ->
    case {agent(Supervisor), fsm(Supervisor)} of
        {LPid, FSM} when is_pid(LPid), is_pid(FSM) ->
            {AcctId, AgentId, Q} = acdc_agent:config(LPid),
            Status = acdc_agent_fsm:status(FSM),

            lager:info("Agent ~s (Account ~s)", [AgentId, AcctId]),
            lager:info("  Supervisor: ~p", [Supervisor]),
            lager:info("  Listener: ~p (~s)", [LPid, Q]),
            lager:info("  FSM: ~p", [FSM]),
            print_status(augment_status(Status, LPid));
        _ ->
            lager:info("Agent Supervisor ~p is dead, stopping", [Supervisor]),
            ?MODULE:stop(Supervisor)
    end.

-define(AGENT_INFO_FIELDS, whapps_config:get(?CONFIG_CAT, <<"agent_info_fields">>
                                                 ,[<<"first_name">>, <<"last_name">>, <<"username">>, <<"email">>]
                                            )).

augment_status(Status, LPid) ->
    Fs = ?AGENT_INFO_FIELDS,
    [{F, acdc_agent:agent_info(LPid, F)} || F <- Fs] ++ Status.

print_status([]) -> 'ok';
print_status([{_, 'undefined'}|T]) -> print_status(T);
print_status([{K, V}|T]) when is_binary(V) ->
    lager:info("  ~s: ~s", [K, V]),
    print_status(T);
print_status([{K, V}|T]) ->
    lager:info("  ~s: ~p", [K, V]),
    print_status(T).

-spec agent(pid()) -> pid() | 'undefined'.
agent(Super) ->
    case child_of_type(Super, 'acdc_agent') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec fsm(pid()) -> pid() | 'undefined'.
fsm(Super) ->
    case child_of_type(Super, 'acdc_agent_fsm') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec start_fsm(pid(), ne_binary(), ne_binary()) -> sup_startchild_ret().
-spec start_fsm(pid(), ne_binary(), ne_binary(), wh_proplist()) -> sup_startchild_ret().
start_fsm(Super, AcctId, AgentId) -> start_fsm(Super, AcctId, AgentId, []).
start_fsm(Super, AcctId, AgentId, Props) ->
    case fsm(Super) of
        'undefined' ->
            Parent = self(),
            supervisor:start_child(Super, ?CHILD('acdc_agent_fsm', [AcctId, AgentId, Parent, Props]));
        P when is_pid(P) -> {'ok', P}
    end.

-spec child_of_type(pid(), atom()) -> list(pid()).
child_of_type(S, T) -> [P || {Ty, P, 'worker', _} <- supervisor:which_children(S), T =:= Ty].

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
-spec init(list()) -> sup_init_ret().
init(Args) ->
    RestartStrategy = 'one_for_all',
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 2,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [?CHILD('acdc_agent', [self() | Args])
                       ,?CHILD('acdc_agent_fsm', [self() | Args])
                      ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
