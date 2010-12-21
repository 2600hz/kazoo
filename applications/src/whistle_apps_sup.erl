
-module(whistle_apps_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_app/1, stop_app/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec(start_app/1 :: (App :: atom()) -> tuple(ok, pid() | undefined) | tuple(ok, pid() | undefined, term()) | tuple(error, term())).
start_app(App) -> supervisor:start_child(?MODULE, ?CHILD(App, supervisor)).

stop_app(App) ->
    supervisor:terminate_child(?MODULE, App),
    supervisor:delete_child(?MODULE, App).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(whistle_controller, worker)]} }.
