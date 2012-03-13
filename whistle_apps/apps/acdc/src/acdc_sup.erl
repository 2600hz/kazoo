
-module(acdc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, cache_proc/0, listener_proc/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CACHE(Name), {Name, {wh_cache, start_link, [Name]}, permanent, 5000, worker, [wh_cache]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(POOL(Mod), {Mod, {poolboy, start_link, [
                                                [{name, {local, Mod}}
                                                 ,{worker_module, ecallmgr_amqp_worker}
                                                ]
                                               ]}
                    ,permanent, 5000, worker, [poolboy]
                   }).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec cache_proc/0 :: () -> {'ok', pid()}.
cache_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= acdc_cache],
    {ok, P}.

-spec listener_proc/0 :: () -> {'ok', pid()}.
listener_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= acdc_listener],
    {ok, P}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, { {one_for_one, 5, 10}, [
                                  ?CACHE(acdc_cache) %% generally, we create a local cache process per whapps
                                  ,?POOL(acdc_agent_pool)
                                  ,?CHILD(acdc_listener, worker) %% the listener, we always want this running
                                 ]} }.
