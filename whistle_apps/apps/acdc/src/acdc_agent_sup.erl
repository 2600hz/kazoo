%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(acdc_agent_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/0]).
-export([new/3]).
-export([workers/0]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec new/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> sup_startchild_ret().
new(AccountDb, AgentId, Queues) ->
    Agent = {AgentId
             ,{acdc_agent, start_link, [AccountDb, AgentId, Queues]}
             ,permanent, 5000, worker
             ,[acdc_agent]
            },
    supervisor:start_child(?MODULE, Agent).

-spec workers/0 :: () -> [pid(),...] | [].
workers() ->
    [ Pid || {_, Pid, worker, [_]} <- supervisor:which_children(?MODULE)].

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
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.
