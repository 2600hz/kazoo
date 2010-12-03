%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for supervising the whistle monitoring jobs
%%% @end
%%% Created : 29 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_job_sup).

-behaviour(supervisor).

-include("../include/monitor_amqp.hrl").

%% API
-export([start_link/0]).
-export([start_job/1, start_job/2, start_job/3]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_job(Job_ID) ->
    start_job(Job_ID, [], ?AMQP_HOST).

start_job(Job_ID, Tasks) ->
    start_job(Job_ID, Tasks, ?AMQP_HOST).

start_job(Job_ID, Tasks, AHost) ->
    supervisor:start_child(?MODULE, [Job_ID, Tasks, AHost]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Job = {monitor_job, {monitor_job, start_link, []},
           temporary, brutal_kill, worker, [monitor_job]},
    Children = [Job],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
