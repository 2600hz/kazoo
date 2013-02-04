%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(fax_sup).

-behaviour(supervisor).

-include("fax.hrl").

-export([start_link/0]).
-export([cache_proc/0]).
-export([listener_proc/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Type), fun(N, cache) -> {N, {wh_cache, start_link, [N]}, permanent, 5000, worker, [wh_cache]};
                              (N, pool) -> {N, {poolboy, start_link, [[{name, {local, N}}
                                                                       ,{worker_module, fax_worker}
                                                                       ,{size, 5}
                                                                       ,{max_overflow, 0}
                                                                      ]]}
                                            ,permanent, 5000, worker, [poolboy]};
                              (N, T) -> {N, {N, start_link, []}, permanent, 5000, T, [N]} end(Name, Type)).

-define(CHILDREN, [{fax_worker_pool, pool}
                   ,{fax_cache, cache}
                   ,{fax_jobs, worker}
                   ,{fax_requests_sup, supervisor}
                   ,{fax_listener, worker}
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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec cache_proc() -> {'ok', ?FAX_CACHE}.
cache_proc() ->
    {ok, ?FAX_CACHE}.

-spec listener_proc() -> {'ok', pid()}.
listener_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= fax_listener],
    {ok, P}.

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
    Children = [?CHILD(Name, Type) || {Name, Type} <- ?CHILDREN],

    {ok, {SupFlags, Children}}.
