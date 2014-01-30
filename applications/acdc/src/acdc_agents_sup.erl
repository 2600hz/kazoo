%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agents_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/0
         ,new/1, new/2, new/4
         ,new_thief/2
         ,workers/0
         ,find_acct_supervisors/1
         ,find_agent_supervisor/2
         ,status/0
         ,restart_acct/1
         ,restart_agent/2
        ]).

%% Supervisor callbacks
-export([init/1]).

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
-spec start_link() -> startlink_ret().
start_link() -> supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec status() -> 'ok'.
status() ->
    lager:info("ACDc Agents Status"),
    Ws = workers(),
    _ = spawn(fun() -> [acdc_agent_sup:status(Sup) || Sup <- Ws] end),
    'ok'.

-spec new(wh_json:object()) -> sup_startchild_ret().
-spec new(ne_binary(), ne_binary()) -> sup_startchild_ret().
new(JObj) ->
    case find_agent_supervisor(wh_json:get_value(<<"pvt_account_id">>, JObj)
                               ,wh_json:get_value(<<"_id">>, JObj)
                              )
    of
        'undefined' -> supervisor:start_child(?MODULE, [JObj]);
        P when is_pid(P) -> lager:debug("agent already started here: ~p", [P])
    end.

new(AcctId, AgentId) ->
    case find_agent_supervisor(AcctId, AgentId) of
        'undefined' ->
            {'ok', Agent} = couch_mgr:open_doc(wh_util:format_account_id(AcctId, 'encoded'), AgentId),
            supervisor:start_child(?MODULE, [Agent]);
        P when is_pid(P) -> lager:debug("agent already started here: ~p", [P])
    end.

new(AcctId, AgentId, AgentJObj, Queues) ->
    case find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> supervisor:start_child(?MODULE, [AgentJObj, AcctId, AgentId, Queues]);
        P when is_pid(P) -> lager:debug("agent already started here: ~p", [P])
    end.

-spec new_thief(whapps_call:call(), ne_binary()) -> sup_startchild_ret().
new_thief(Call, QueueId) -> supervisor:start_child(?MODULE, [Call, QueueId]).

-spec workers() -> pids().
workers() -> [Pid || {_, Pid, 'supervisor', [_]} <- supervisor:which_children(?MODULE)].

restart_acct(AcctId) -> [acdc_agent_sup:restart(S) || S <- workers(), is_agent_in_acct(S, AcctId)].
restart_agent(AcctId, AgentId) ->
    case find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> lager:info("no supervisor for agent ~s(~s) to restart", [AgentId, AcctId]);
        S -> acdc_agent_sup:restart(S)
    end.

-spec find_acct_supervisors(ne_binary()) -> pids().
find_acct_supervisors(AcctId) -> [S || S <- workers(), is_agent_in_acct(S, AcctId)].

-spec is_agent_in_acct(pid(), ne_binary()) -> boolean().
is_agent_in_acct(Super, AcctId) ->
    case catch acdc_agent_listener:config(acdc_agent_sup:listener(Super)) of
        {'EXIT', _} -> 'false';
        {AcctId, _, _} -> 'true';
        _ -> 'false'
    end.

-spec find_agent_supervisor(api_binary(), api_binary()) -> api_pid().
-spec find_agent_supervisor(api_binary(), api_binary(), pids()) -> api_pid().
find_agent_supervisor(AcctId, AgentId) -> find_agent_supervisor(AcctId, AgentId, workers()).

find_agent_supervisor(_AcctId, _AgentId, []) -> lager:debug("ran out of supers"), 'undefined';
find_agent_supervisor(AcctId, AgentId, _) when AcctId =:= 'undefined' orelse
                                               AgentId =:= 'undefined' ->
    lager:debug("failed to get good data: ~s ~s", [AcctId, AgentId]),
    'undefined';
find_agent_supervisor(AcctId, AgentId, [Super|Rest]) ->
    case catch acdc_agent_listener:config(acdc_agent_sup:listener(Super)) of
        {'EXIT', _E} -> find_agent_supervisor(AcctId, AgentId, Rest);
        {AcctId, AgentId, _} -> Super;
        _E -> find_agent_supervisor(AcctId, AgentId, Rest)
    end.

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
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [?SUPER('acdc_agent_sup', 'transient')]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
