%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Timeout), {I, {I, start_link, []}, permanent, Timeout, Type, [I]}).

-define(CHILDREN, [{webseq, worker, 5000}
                   ,{acdc_agents_sup, supervisor, infinity}
                   ,{acdc_queues_sup, supervisor, infinity}
                   ,{acdc_stats, worker, 5000}
                   ,{acdc_agent_manager, worker, 5000}
                   ,{acdc_init, worker, 5000}
                   ,{acdc_listener, worker, 5000}
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = [?CHILD(Name, Type, Timeout) || {Name, Type, Timeout} <- ?CHILDREN],
    {ok, {SupFlags, Children}}.
