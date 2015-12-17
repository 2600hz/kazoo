%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(doodle_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("doodle.hrl").

-define(ORIGIN_BINDINGS, [ [{'db', ?WH_SIP_DB }
                            ,{'type', <<"device">>}
                           ]
                         ]).
-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

-define(CHILDREN, [?CACHE_ARGS(?DOODLE_CACHE, ?CACHE_PROPS)
                   ,?WORKER('doodle_listener')
                   ,?WORKER('doodle_shared_listener')
                   ,?SUPER('doodle_event_handler_sup')
                   ,?SUPER('doodle_exe_sup')
                   ,?SUPER('doodle_inbound_listener_sup')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

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
-spec init([]) -> sup_init_ret().
init([]) ->
    wh_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
