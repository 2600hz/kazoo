%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_maintenance).

-export([local_summary/0]).

-include("acdc.hrl").

-define(AGENT_FORMAT, "| ~20.s | ~15.s |").

local_summary() ->
    Summary = acdc_agents:summary(),
    Agents = props:get_value(agents, Summary),
    Strategy = props:get_value(strategy, Summary),

    lager:info("Summary of ACDc on ~s", [node()]),
    lager:info("Strategy employeed: ~s", [Strategy]),
    lager:info("Agents active:"),

    lager:info(?AGENT_FORMAT, [<<"Agent ID">>, <<"Last Call">>]),

    [lager:info(?AGENT_FORMAT, [acdc_agent:get_agent_id(P), wh_util:to_binary(T)]) || {P, T} <- Agents].
