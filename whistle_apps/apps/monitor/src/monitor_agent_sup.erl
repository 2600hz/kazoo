%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Responsible for supervising the whistle monitoring agents
%%% @end
%%% Created : 29 Nov 2010 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(monitor_agent_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(AHost) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AHost]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([AHost]) ->
    NetworkAgent = {monitor_agent_network, {monitor_agent_network, start_link, [AHost]},
                     permanent, 2000, worker, [monitor_agent_network]},
    CallAgent    = {monitor_agent_call, {monitor_agent_call, start_link, [AHost]},
                     permanent, 2000, worker, [monitor_agent_call]},
    Children     = [NetworkAgent, CallAgent],
    Restart      = {one_for_one, 5, 10},
    {ok, {Restart, Children}}.
