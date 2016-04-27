%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_media_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER_APP_INIT('whistle_media_init', 20 * ?SECONDS_IN_MINUTE)
                   ,?CACHE('media_mgr_cache')
                   ,?WORKER_ARGS('kazoo_etsmgr_srv'
                                ,[
                                  [{'table_id', wh_media_map:table_id()}
                                  ,{'table_options', wh_media_map:table_options()}
                                  ,{'find_me_function', fun wh_media_map:find_me_function/0}
                                  ,{'gift_data', wh_media_map:gift_data()}
                                  ]
                                 ])
                   ,?WORKER('wh_media_map')
                   ,?SUPER('wh_media_cache_sup')
                   ,?WORKER('wh_media_proxy')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
