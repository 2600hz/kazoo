%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(trunkstore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("ts.hrl").

-define(SERVER, ?MODULE).

-define(ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                         ,[{'type', <<"connectivity">>}]
                         ,[{'type', <<"sys_info">>}]
                         ,[{'type', <<"number">>}]
                         ]).

-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}
                     ]).

-define(CHILDREN, [?SUPER('ts_onnet_sup') %% handles calls originating on-net (customer)
                  ,?WORKER('ts_offnet_sup') %% handles calls originating off-net (carrier)
                  ,?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)
                  ,?WORKER('ts_responder')
                  ,?WORKER('trunkstore_listener')
                  ]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    kz_util:set_startup(),

    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
