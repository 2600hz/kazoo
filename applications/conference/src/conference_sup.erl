%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conference_sup).

-behaviour(supervisor).

-include("conference.hrl").

-export([start_link/0]).
-export([init/1]).

-define(CACHE, {?CONFERENCE_CACHE, {'wh_cache', 'start_link', [?CONFERENCE_CACHE]}
                ,'permanent', 5000, 'worker', ['wh_cache']
               }).

-define(CHILDREN, [?CACHE
                   ,?SUPER('conf_participant_sup')
                   ,?WORKER('conf_discovery')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================


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
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
