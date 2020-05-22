%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
acct_status(AccountId) when not is_binary(AccountId) ->
    acct_status(kz_term:to_binary(AccountId));
acct_status(AccountId) ->
    case acdc_agents_sup:find_acct_supervisors(AccountId) of
        [] -> lager:info("no agents with account id ~s available", [AccountId]);
        As ->
            lager:info("agent Statuses in ~s", [AccountId]),
            lists:foreach(fun acdc_agent_sup:status/1, As)
    end.

-spec agent_status(kz_term:text(), kz_term:text()) -> 'ok'.
agent_status(AccountId, AgentId) when not is_binary(AccountId);
                                      not is_binary(AgentId) ->
    agent_status(kz_term:to_binary(AccountId), kz_term:to_binary(AgentId));
agent_status(AccountId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> lager:info("no agent ~s in account ~s available", [AgentId, AccountId]);
        S -> acdc_agent_sup:status(S)
    end.

-spec acct_restart(kz_term:text()) -> 'ok'.
acct_restart(AccountId) when not is_binary(AccountId) ->
    acct_restart(kz_term:to_binary(AccountId));
acct_restart(AccountId) ->
    case acdc_agents_sup:find_acct_supervisors(AccountId) of
        [] -> lager:info("no agents with account id ~s available", [AccountId]);
        As ->
            lager:debug("terminating existing agent processes in ~s", [AccountId]),
            _ = [exit(Sup, 'kill') || Sup <- As],
            lager:info("restarting agents in ~s", [AccountId]),
            _ = acdc_init:init_acct_agents(AccountId),
            'ok'
    end.

-spec agent_restart(kz_term:text(), kz_term:text()) -> 'ok'.
agent_restart(AccountId, AgentId) when not is_binary(AccountId);
                                       not is_binary(AgentId) ->
    agent_restart(kz_term:to_binary(AccountId), kz_term:to_binary(AgentId));
agent_restart(AccountId, AgentId) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' -> lager:info("no agent ~s in account ~s available", [AgentId, AccountId]);
        S ->
            lager:info("terminating existing agent process ~p", [S]),
            exit(S, 'kill'),
            lager:info("restarting agent ~s in ~s", [AgentId, AccountId]),
            acdc_agents_sup:new(AccountId, AgentId),
            'ok'
    end.
