%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(trunkstore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("ts.hrl").

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link() ->
    _ = trunkstore:start_deps(),
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {'ok', { {'one_for_one', 5, 10}
             ,[?SUPER('ts_onnet_sup') %% handles calls originating on-net (customer)
               ,?WORKER('ts_offnet_sup') %% handles calls originating off-net (carrier)
               ,?WORKER('ts_responder')
              ]}
    }.
