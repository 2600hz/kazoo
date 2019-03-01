%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_agent_maintenance).

-export([acct_restart/1
        ,agent_restart/2

        ,status/0
        ,acct_status/1
        ,agent_status/2
        ]).

-include("acdc.hrl").


-spec status() -> 'ok'.
status() -> acdc_agents_sup:status().

-spec acct_status(kz_term:text()) -> 'ok'.
acct_status(AcctId) when not is_binary(AcctId) ->
    acct_status(kz_term:to_binary(AcctId));
acct_status(AcctId) ->
    case acdc_agents_sup:find_acct_supervisors(AcctId) of
        [] -> lager:info("no agents with account id ~s available", [AcctId]);
        As ->
            lager:info("agent Statuses in ~s", [AcctId]),
            lists:foreach(fun acdc_agent_sup:status/1, As)
    end.

-spec agent_status(kz_term:text(), kz_term:text()) -> 'ok'.
agent_status(AcctId, AgentId) when not is_binary(AcctId);
                                   not is_binary(AgentId) ->
    agent_status(kz_term:to_binary(AcctId), kz_term:to_binary(AgentId));
agent_status(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> lager:info("no agent ~s in account ~s available", [AgentId, AcctId]);
        S -> acdc_agent_sup:status(S)
    end.

-spec acct_restart(kz_term:text()) -> 'ok'.
acct_restart(AcctId) when not is_binary(AcctId) ->
    acct_restart(kz_term:to_binary(AcctId));
acct_restart(AcctId) ->
    case acdc_agents_sup:find_acct_supervisors(AcctId) of
        [] -> lager:info("no agents with account id ~s available", [AcctId]);
        As ->
            lager:debug("terminating existing agent processes in ~s", [AcctId]),
            _ = [exit(Sup, 'kill') || Sup <- As],
            lager:info("restarting agents in ~s", [AcctId]),
            acdc_init:init_acct_agents(AcctId),
            'ok'
    end.

-spec agent_restart(kz_term:text(), kz_term:text()) -> 'ok'.
agent_restart(AcctId, AgentId) when not is_binary(AcctId);
                                    not is_binary(AgentId) ->
    agent_restart(kz_term:to_binary(AcctId), kz_term:to_binary(AgentId));
agent_restart(AcctId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
        'undefined' -> lager:info("no agent ~s in account ~s available", [AgentId, AcctId]);
        S ->
            lager:info("terminating existing agent process ~p", [S]),
            exit(S, 'kill'),
            lager:info("restarting agent ~s in ~s", [AgentId, AcctId]),
            _ = acdc_agents_sup:new(AcctId, AgentId),
            'ok'
    end.
