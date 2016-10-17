%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_conference_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1]).
-export([start_conference_control/3
        ]).

-define(CHILDREN, [?WORKER_TYPE('ecallmgr_conference_control', 'transient')]).

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

-spec start_conference_control(node(), ne_binary(), ne_binary()) -> sup_startchild_ret().
start_conference_control(Node, ConferenceId, InstanceId) ->
    supervisor:start_child(?SERVER, [Node, ConferenceId, InstanceId]).


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
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.
