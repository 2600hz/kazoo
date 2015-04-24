%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_number_manager_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("wnm.hrl").

-define(ORIGIN_BINDINGS, [[{'type', <<"number">>}]]).

-define(CACHE_NUMBER_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?CACHE_ARGS(?WNM_NUMBER_CACHE, ?CACHE_NUMBER_PROPS)
                   ,?WORKER('wh_port_request_crawler')
                   ,?WORKER('wnm_number_crawler')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
