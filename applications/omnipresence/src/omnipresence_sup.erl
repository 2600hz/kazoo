%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnipresence_sup).

-behaviour(supervisor).

-export([start_link/0
         ,subscriptions_srv/0
        ]).
-export([init/1]).

-include("omnipresence.hrl").

-define(ETS_OPTS, [{'table_id', omnip_subscriptions:table_id()}
                   ,{'table_options', omnip_subscriptions:table_config()}
                   ,{'find_me_function', fun ?MODULE:subscriptions_srv/0}
                  ]).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER_ARGS('kazoo_etsmgr_srv', [?ETS_OPTS])
                   ,?WORKER('omnip_subscriptions')
                   ,?WORKER('omnipresence_listener')
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
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec subscriptions_srv() -> pid() | 'undefined'.
subscriptions_srv() ->
    case [P || {_, P, 'worker', ['omnip_subscriptions']} <- supervisor:which_children(?MODULE)] of
        [] -> 'undefined';
        [Pid] -> Pid
    end.

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
