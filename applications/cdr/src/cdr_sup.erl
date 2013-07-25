%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cdr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("cdr.hrl").

-define(CACHE_PROPS, []).
-define(CHILDREN, [?CACHE_ARGS(?CDR_CACHE, ?CACHE_PROPS)
                   ,?WORKER('cdr_listener')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

start_v3_migrate() ->
    ChildSpec = {'cdr_v3_migrate'
                 ,{'cdr_v3_migrate_server', 'start_link', []}
                 ,'permanent'
                 ,5000
                 ,['cdr_v3_migrate_server']
                },
    supervisor:start_child(?MODULE, ChildSpec).

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
