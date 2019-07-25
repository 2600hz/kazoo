%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_services_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("services.hrl").

-define(SERVER, ?MODULE).

-define(ORIGIN_BINDINGS, [[{'db', ?KZ_SERVICES_DB}, {'type', kzd_services:type()}]]).

-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

%% Helper macro for declaring children of supervisor
-ifdef(TEST).
-define(CHILDREN, [?CACHE_ARGS(?CACHE_NAME, [])]).
-else.
-define(CHILDREN, [?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)]).
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
