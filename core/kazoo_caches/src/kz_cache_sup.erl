%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Supervisor for started caches
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_sup).

-behaviour(supervisor).

-export([start_link/1, start_link/2, start_link/3
        ,stop/1
        ]).
-export([init/1]).

-include("kz_caches.hrl").

-define(SERVER, ?MODULE).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Name) when is_atom(Name) ->
    start_link(Name, ?EXPIRE_PERIOD_MS, []).

-spec start_link(atom(), kz_cache:start_options() | timeout()) -> kz_types:startlink_ret().
start_link(Name, Props) when is_list(Props) ->
    start_link(Name, ?EXPIRE_PERIOD_MS, Props);
start_link(Name, ExpirePeriod) when is_integer(ExpirePeriod), ExpirePeriod > 0 ->
    start_link(Name, ExpirePeriod, []).

-spec start_link(atom(), timeout(), kz_cache:start_options()) -> kz_types:startlink_ret().
start_link(Name, ExpirePeriod, Props) ->
    supervisor:start_link({'local', sup_name(Name)}, ?MODULE, [Name, ExpirePeriod, Props]).

-spec stop(atom()) -> 'ok'.
stop(Name) ->
    gen_server:stop(sup_name(Name)).

-spec sup_name(atom()) -> atom().
sup_name(Name) ->
    kz_term:to_atom(atom_to_list(Name) ++ "_sup", 'true').

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
init([Name, ExpirePeriod, Props]) ->
    RestartStrategy = 'rest_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = lists:foldl(fun(Child, Acc) -> maybe_add_child_spec(Child, Name, Props, Acc) end
                          ,[?WORKER_ARGS('kz_cache_lru', [Name, ExpirePeriod])
                           ,?WORKER_ARGS('kz_cache_ets', [Name])
                           ]
                          ,['kz_cache_listener', 'kz_cache_nodes']
                          ),

    {'ok', {SupFlags, lists:reverse(Children)}}.


-spec maybe_add_child_spec(atom(), atom(), kz_cache:start_options(), kz_types:sup_child_specs()) ->
                                  kz_types:sup_child_specs().
maybe_add_child_spec('kz_cache_listener', Name, Props, Children) ->
    case props:get_value('origin_bindings', Props) of
        'undefined' -> Children;
        _BindingProps ->
            [?WORKER_ARGS('kz_cache_listener', [Name, Props]) | Children]
    end;
maybe_add_child_spec('kz_cache_nodes', Name, Props, Children) ->
    case props:is_true('new_node_flush', Props, 'false')
        orelse props:is_true('expire_node_flush', Props, 'false')
    of
        'false' -> Children;
        'true' ->
            [?WORKER_ARGS('kz_cache_nodes', [Name, Props]) | Children]
    end.
