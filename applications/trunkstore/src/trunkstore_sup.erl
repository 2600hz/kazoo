%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
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

-define(ORIGIN_BINDINGS, [[{'type', <<"account">>}
                           ,{'type', <<"connectivity">>}
                           ,{'type', <<"sys_info">>}
                           ,{'type', <<"number">>}
                          ]
                         ]).
-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> startlink_ret().
start_link() ->
    _ = trunkstore:start_deps(),
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    wh_util:set_startup(),
    {'ok', { {'one_for_one', 5, 10}
             ,[?SUPER('ts_onnet_sup') %% handles calls originating on-net (customer)
               ,?WORKER('ts_offnet_sup') %% handles calls originating off-net (carrier)
               ,?CACHE_ARGS(?TRUNKSTORE_CACHE, ?CACHE_PROPS)
               ,?WORKER('ts_responder')
               ,?WORKER('trunkstore_listener')
              ]}
    }.
