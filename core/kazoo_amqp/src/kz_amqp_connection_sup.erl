%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_connection_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([add/1]).
-export([remove/1]).
-export([init/1]).

-include("kz_amqp_util.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER('kz_amqp_connection')]).

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

-spec add(#kz_amqp_connection{}) -> kz_types:sup_startchild_ret().
add(#kz_amqp_connection{}=Connection) ->
    supervisor:start_child(?SERVER, [Connection]).

-spec remove(pid()) -> 'ok' | {'error', any()}.
remove(Connection) when is_pid(Connection) ->
    supervisor:terminate_child(?SERVER, Connection).

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
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
