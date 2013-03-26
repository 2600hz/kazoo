%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_maintenance).

-export([acct_restart/1
         ,agent_restart/2

         ,status/0
         ,acct_status/1
         ,agent_status/2
        ]).

status() -> acdc_agents_sup:status().

acct_status(AcctId) when not is_binary(AcctId) ->
    acct_status(wh_util:to_binary(AcctId));
acct_status(AcctId) ->
    case acdc_agents_sup:find_acct_supervisors(AcctId) of
        [] -> lager:info("no agents with account id ~s available", [AcctId]);
        As ->
            lager:info("Agent Statuses in ~s", [AcctId]),
            [acdc_agent_sup:status(Sup) || Sup <- As],
            'ok'
    end.

agent_status(AcctId, AgentId) when (not is_binary(AcctId)) orelse (not is_binary(AgentId)) ->
    agent_status(wh_util:to_binary(AcctId), wh_util:to_binary(AgentId));
agent_status(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> lager:info("no agent ~s in account ~s available", [AgentId, AcctId]);
        S -> acdc_agent_sup:status(S), 'ok'
    end.

acct_restart(AcctId) when not is_binary(AcctId) ->
    acct_restart(wh_util:to_binary(AcctId));
acct_restart(AcctId) ->
    case acdc_agents_sup:find_acct_supervisors(AcctId) of
        [] -> lager:info("no agents with account id ~s available", [AcctId]);
        As ->
            lager:debug("Terminating existing agent processes in ~s", [AcctId]),
            [exit(Sup, 'kill') || Sup <- As],
            lager:info("Restarting agents in ~s", [AcctId]),
            acdc_init:init_acct_agents(AcctId),
            'ok'
    end.

agent_restart(AcctId, AgentId) when (not is_binary(AcctId)) orelse (not is_binary(AgentId)) ->
    agent_restart(wh_util:to_binary(AcctId), wh_util:to_binary(AgentId));
agent_restart(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> lager:info("no agent ~s in account ~s available", [AgentId, AcctId]);
        S ->
            lager:info("Terminating existing agent process ~p", [S]),
            exit(S, 'kill'),
            lager:info("Restarting agent ~s in ~s", [AgentId, AcctId]),
            acdc_agents_sup:new(AcctId, AgentId),
            'ok'
    end.
