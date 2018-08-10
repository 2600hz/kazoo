%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(conference_sup).

-behaviour(supervisor).

-include("conference.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1]).

-define(ORIGIN_BINDINGS, [[{'type', <<"conference">>}]]).

-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

-define(CHILDREN, [?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)
                  ,?SUPER('conf_participant_sup')
                  ,?WORKER('conference_shared_listener')
                  ,?WORKER('conference_listener')
                  ]).

%%==============================================================================
%% API functions
%%==============================================================================


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
    _ = kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
