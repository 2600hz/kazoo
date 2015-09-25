%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(webhooks_sup).

-behaviour(supervisor).

-export([start_link/0
         ,listener/0
         ,shared_listener/0
        ]).
-export([init/1]).

-include("webhooks.hrl").

-define(ETSMGR_ARGS
        ,[[{'table_id', webhooks_util:table_id()}
           ,{'find_me_function', fun ?MODULE:listener/0}
           ,{'table_options', webhooks_util:table_options()}
           ,{'gift_data', webhooks_util:gift_data()}
          ]]
       ).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?CACHE(?CACHE_NAME)
                   ,?WORKER_ARGS('kazoo_etsmgr_srv', ?ETSMGR_ARGS)
                   ,?WORKER('webhooks_init')
                   ,?WORKER('webhooks_disabler')
                   ,?WORKER('webhooks_listener')
                   ,?WORKER('webhooks_shared_listener')
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

-spec listener() -> api_pid().
listener() ->
    case child_of_type(?MODULE, 'webhooks_listener') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec shared_listener() -> api_pid().
shared_listener() ->
    case child_of_type(?MODULE, 'webhooks_shared_listener') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec child_of_type(pid() | atom(), atom()) -> pids().
child_of_type(S, T) ->
    [P || {Ty, P, 'worker', _} <- supervisor:which_children(S),
          T =:= Ty
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
-spec init([]) -> sup_init_ret().
init([]) ->
    wh_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
