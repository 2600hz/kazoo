%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("acdc.hrl").

-define(CHILDREN, [?CACHE(?ACDC_CACHE)
                   ,?SUPER('acdc_agents_sup')
                   ,?SUPER('acdc_queues_sup')
                   ,?SUPER('acdc_stats_sup')
                   ,?WORKER('acdc_agent_manager')
                   ,?WORKER('acdc_init')
                   ,?WORKER('acdc_listener')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.
