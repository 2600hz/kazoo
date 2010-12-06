%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for supervising the whistle monitoring application
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("../include/monitor_amqp.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [?AMQP_HOST]).
start_link(AHost) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AHost]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    init([""]);
init([AHost]) ->
    MonitorMaster = {monitor_master, {monitor_master, start_link, [AHost]},
                    permanent, 2000, worker, [monitor_master]},
    MonitorImport = {monitor_import, {monitor_import, start_link, []},
                    permanent, 2000, worker, [monitor_import]},
    AgentSup = {monitor_agent_sup, {monitor_agent_sup, start_link, [AHost]},
                    permanent, 2000, supervisor, [monitor_agent_sup]},
    JobSup = {monitor_job_sup, {monitor_job_sup, start_link, []},
                    permanent, 2000, supervisor, [monitor_job_sup]},
    Children = [MonitorMaster, MonitorImport, AgentSup, JobSup],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.
