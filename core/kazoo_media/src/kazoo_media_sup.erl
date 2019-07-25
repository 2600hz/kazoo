%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_media_sup).
-behaviour(supervisor).

-include("kazoo_media.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?SUPER('kz_media_cache_sup')
                  ,?WORKER_APP_INIT('kazoo_media_init', 20 * ?SECONDS_IN_MINUTE)
                  ,?WORKER_ARGS('kazoo_etsmgr_srv'
                               ,[
                                 [{'table_id', kz_media_map:table_id()}
                                 ,{'table_options', kz_media_map:table_options()}
                                 ,{'find_me_function', fun kz_media_map:find_me_function/0}
                                 ,{'gift_data', kz_media_map:gift_data()}
                                 ]
                                ])
                  ,?WORKER('kz_media_map')
                  ]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
