%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_apps_sup).
-behaviour(supervisor).

-export([start_link/0
        ,init/1
        ,start_child/1
        ]).

-include("kazoo_apps.hrl").

-define(SERVER, ?MODULE).

-define(KAPPS_GETBY_ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                                     ]).

-define(KAPPS_GETBY_PROPS, [{'origin_bindings', ?KAPPS_GETBY_ORIGIN_BINDINGS}]).

-define(KAPPS_CONFIG_ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                                      ,[{'type', <<"account_config">>}]
                                      ,[{'db', ?KZ_CONFIG_DB}]
                                      ]).

-define(KAPPS_CONFIG_PROPS, [{'origin_bindings', ?KAPPS_CONFIG_ORIGIN_BINDINGS}]).

-ifdef(TEST).
-define(CHILDREN, [?CACHE_ARGS(?KAPPS_CONFIG_CACHE, [])
                  ,?CACHE_ARGS(?KAPPS_GETBY_CACHE, [])
                  ]).
-else.
-define(CHILDREN, [?WORKER('kazoo_apps_init')
                  ,?CACHE_ARGS(?KAPPS_CONFIG_CACHE, ?KAPPS_CONFIG_PROPS)
                  ,?WORKER('kapps_controller')
                  ,?CACHE_ARGS(?KAPPS_GETBY_CACHE, ?KAPPS_GETBY_PROPS)
                  ,?WORKER('kz_epmd')
                  ]).
-endif.

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

-spec start_child(supervisor:child_spec()) -> kz_types:sup_startchild_ret().
start_child(Spec) ->
    supervisor:start_child(?SERVER, Spec).

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
    MaxRestarts = 25,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
