%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
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
        ,stop_conference_control/3
        ]).

-define(CHILDREN, [?WORKER_TYPE('ecallmgr_conference_control', 'transient')]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_conference_control(node(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:sup_startchild_ret().
start_conference_control(Node, ConferenceId, InstanceId) ->
    supervisor:start_child(?SERVER, [Node, ConferenceId, InstanceId]).

-spec stop_conference_control(node(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
stop_conference_control(Node, ConferenceId, InstanceId) ->
    [Pid ! {'stop', {Node, ConferenceId, InstanceId}}
     || {_, Pid, _, _}
            <- supervisor:which_children(?SERVER), is_pid(Pid)
    ].

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
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.
