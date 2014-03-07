%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(caller10_contests_sup).

-behaviour(supervisor).

-include("caller10.hrl").

%% API
-export([start_link/0
         ,new/1
         ,workers/0
        ]).

%% Supervisor callbacks
-export([init/1]).

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
start_link() -> supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec new(wh_json:object()) -> sup_startchild_ret().
new(Contest) ->
    supervisor:start_child(?MODULE, [Contest]).

-spec workers() -> pids().
workers() ->
    [Pid
     || {_, Pid, 'supervisor', [_]} <- supervisor:which_children(?MODULE)
    ].

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(list()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [?SUPER('caller10_contest_sup', 'temporary')]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
