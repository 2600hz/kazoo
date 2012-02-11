
-module(whistle_apps_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, config_cache_proc/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CACHE(Name), {Name, {wh_cache, start_link, [Name]}, permanent, 5000, worker, [wh_cache]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec config_cache_proc/0 :: () -> {'ok', pid()}.
config_cache_proc() ->
    {ok, whereis(config_cache)}.
    %% [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
    %%          Mod =:= config_cache],
    %% {ok, P}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}
           , [?CHILD(wh_alert, worker)
              ,?CHILD(wh_cache, worker)
              ,?CACHE(config_cache)
              ,?CHILD(whistle_couch_sup, worker)
              %% ,?CHILD(wh_timer, worker)
              ,?CHILD(whapps_sup, supervisor)
              ,?CHILD(whapps_controller, worker)
             ]
         } }.
