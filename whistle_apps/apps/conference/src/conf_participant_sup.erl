%%%-------------------------------------------------------------------
%%% @copyright (C) 2012 VoIP Inc
%%% @doc
%%% Supervisor for running conference participant processes
%%% @end
%%% Created : 20 Feb 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(conf_participant_sup).

-behaviour(supervisor).

-include("conference.hrl").

%% API
-export([start_link/0]).
-export([start_participant/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Restart, Shutdown, Type),
        {Name, {Name, 'start_link', []}, Restart, Shutdown, Type, [Name]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() -> supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_participant(whapps_call:call()) -> sup_startchild_ret().
start_participant(Call) -> supervisor:start_child(?MODULE, [Call]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [?CHILD('conf_participant', 'temporary', 2000, 'worker')]}}.
