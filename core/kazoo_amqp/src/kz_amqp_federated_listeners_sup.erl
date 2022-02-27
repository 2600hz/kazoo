%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_federated_listeners_sup).

-behaviour(supervisor).

-export([start_link/0
        ,start_child/3
        ,init/1
        ]).

-include("kz_amqp_util.hrl").

-define(CHILDREN, [?WORKER_TYPE('listener_federator', 'transient')]).

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec start_child(pid(), kz_term:ne_binary(), kz_term:proplist()) -> kz_types:sup_startchild_ret().
start_child(Parent, Broker, FederateParams) ->
    ParentCallId = kz_util:get_callid(),
    supervisor:start_child(?MODULE, [Parent, ParentCallId, Broker, FederateParams]).

-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
