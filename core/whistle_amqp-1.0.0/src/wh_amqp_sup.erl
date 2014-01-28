%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_amqp_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([stop_bootstrap/0]).
-export([init/1]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(CHILDREN, [?WORKER('wh_amqp_connections')
                   ,?SUPER('wh_amqp_connection_sup')
                   ,?WORKER('wh_amqp_assignments')
                   ,?WORKER('wh_amqp_history')
                   ,?WORKER('wh_amqp_bootstrap')
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
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec stop_bootstrap/0 :: () -> 'ok' | {'error', 'running' | 'not_found' | 'simple_one_for_one'}.
stop_bootstrap() ->
    _ = supervisor:terminate_child(?SERVER, 'wh_amqp_bootstrap').

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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.
