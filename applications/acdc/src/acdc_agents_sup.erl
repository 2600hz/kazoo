%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_agents_sup).

-behaviour(supervisor).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0
        ,new/1, new/2, new/4
        ,new_thief/2
        ,workers/0
        ,find_acct_supervisors/1
        ,find_agent_supervisor/2
        ,status/0
        ,agents_running/0
        ,restart_acct/1
        ,restart_agent/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?SUPER_TYPE('acdc_agent_sup', 'transient')
                  ]).

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

-spec status() -> 'ok'.
status() ->
    ?PRINT("ACDc Agents Status"),
    Ws = workers(),
    _ = kz_process:spawn(fun() -> lists:foreach(fun acdc_agent_sup:status/1, Ws) end),
    'ok'.

-spec new(kz_json:object()) -> kz_term:sup_startchild_ret().
new(JObj) ->
    acdc_agent_manager:start_agent(kz_doc:account_id(JObj)
                                  ,kz_doc:id(JObj)
                                  ,[JObj]
                                  ).

-spec new(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:sup_startchild_ret().
new(AccountId, AgentId) ->
    {'ok', Agent} = kz_datamgr:open_doc(kzs_util:format_account_db(AccountId), AgentId),
    acdc_agent_manager:start_agent(AccountId
                                  ,AgentId
                                  ,[Agent]
                                  ).

-spec new(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binaries()) -> kz_term:sup_startchild_ret() | 'ok'.
new(AccountId, AgentId, AgentJObj, Queues) ->
    acdc_agent_manager:start_agent(AccountId
                                  ,AgentId
                                  ,[AgentJObj, AccountId, AgentId, Queues]
                                  ).

-spec new_thief(kapps_call:call(), kz_term:ne_binary()) -> kz_term:sup_startchild_ret().
new_thief(Call, QueueId) -> supervisor:start_child(?SERVER, [Call, QueueId]).

-spec workers() -> kz_term:pids().
workers() -> [Pid || {_, Pid, 'supervisor', [_]} <- supervisor:which_children(?SERVER)].

-spec restart_acct(kz_term:ne_binary()) -> [kz_term:sup_startchild_ret()].
restart_acct(AccountId) ->
    [restart_agent(AccountId, AgentId)
     || {_, {AccountId1, AgentId, _}} <- agents_running()
            ,AccountId =:= AccountId1
    ].

-spec restart_agent(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:sup_startchild_ret().
restart_agent(AccountId, AgentId) ->
    case find_agent_supervisor(AccountId, AgentId) of
        'undefined' ->
            lager:info("no supervisor for agent ~s(~s) to restart", [AgentId, AccountId]),
            new(AccountId, AgentId);
        S ->
            _ = acdc_agent_sup:stop(S),
            new(AccountId, AgentId)
    end.

-spec find_acct_supervisors(kz_term:ne_binary()) -> kz_term:pids().
find_acct_supervisors(AccountId) -> [S || S <- workers(), is_agent_in_acct(S, AccountId)].

-spec is_agent_in_acct(pid(), kz_term:ne_binary()) -> boolean().
is_agent_in_acct(Super, AccountId) ->
    case catch acdc_agent_listener:config(acdc_agent_sup:listener(Super)) of
        {'EXIT', _} -> 'false';
        {AccountId, _, _} -> 'true';
        _ -> 'false'
    end.

-spec agents_running() -> [{pid(), acdc_agent_listener:config()}].
agents_running() ->
    [{W, catch acdc_agent_listener:config(acdc_agent_sup:listener(W))} || W <- workers()].

-spec find_agent_supervisor(kz_term:api_binary(), kz_term:api_binary()) -> kz_term:api_pid().
find_agent_supervisor(AccountId, AgentId) -> find_agent_supervisor(AccountId, AgentId, workers()).

-spec find_agent_supervisor(kz_term:api_binary(), kz_term:api_binary(), kz_term:pids()) -> kz_term:api_pid().
find_agent_supervisor(_AccountId, _AgentId, []) ->
    lager:debug("ran out of supers"),
    'undefined';
find_agent_supervisor(AccountId, AgentId, _) when AccountId =:= 'undefined';
                                                  AgentId =:= 'undefined' ->
    lager:debug("failed to get good data: ~s ~s", [AccountId, AgentId]),
    'undefined';
find_agent_supervisor(AccountId, AgentId, [Super|Rest]) ->
    case catch acdc_agent_listener:config(acdc_agent_sup:listener(Super)) of
        {'EXIT', _E} -> find_agent_supervisor(AccountId, AgentId, Rest);
        {AccountId, AgentId, _} -> Super;
        _E -> find_agent_supervisor(AccountId, AgentId, Rest)
    end.

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
