%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_amqp_connection_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/0]).
-export([add/1]).
-export([remove/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(Name, Type, Args), fun(N, T, A) -> {N, {wh_amqp_connection, start_link, [A]}, permanent, 5000, T, [N]} end(Name, Type, Args)).
-define(CHILDREN, []).

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

-spec add/1 :: (wh_amqp_broker:broker()) -> {'ok', pid()}.
add(Broker) ->
    Name = wh_amqp_broker:name(Broker),
    supervisor:start_child(?SERVER, ?CHILD(Name, worker, Broker)).

-spec remove/1 :: (wh_amqp_broker:broker()) -> 'ok' | {'error', 'running' | 'not_found' | 'simple_one_for_one'}.
remove(Broker) ->
    Name = wh_amqp_broker:name(Broker),
    _ = supervisor:terminate_child(?SERVER, Name),
    supervisor:delete_child(?SERVER, Name).

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
    Children = [?CHILD(Name, Type, []) || {Name, Type} <- ?CHILDREN],

    {ok, {SupFlags, Children}}.
