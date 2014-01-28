%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_amqp_connection_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([add/1]).
-export([remove/1]).
-export([init/1]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).

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
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec add/1 :: (#wh_amqp_connection{}) -> {'error', _} | {'ok', api_pid()} | {'ok', api_pid(), _}.
add(#wh_amqp_connection{}=Connection) ->
    supervisor:start_child(?SERVER, [Connection]).

-spec remove/1 :: (pid()) -> 'ok' | {'error', 'running' | 'not_found' | 'simple_one_for_one'}.
remove(Connection) when is_pid(Connection) ->
    supervisor:terminate_child(?SERVER, Connection).

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
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, [?WORKER('wh_amqp_connection')]}}.
