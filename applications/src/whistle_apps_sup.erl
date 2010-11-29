
-module(whistle_apps_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_app/1]).

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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Config = lists:concat([filename:dirname(filename:dirname(code:which(whistle_apps))), "/priv/startup.config"]),
    Startup = case file:consult(Config) of
		  {ok, Ts} ->
		      lists:map(fun(App) -> ?CHILD(App, supervisor) end, proplists:get_value(start, Ts, []));
		  _ -> []
	      end,
    io:format("Starting up ~p~n", [Startup]),
    {ok, { {one_for_one, 5, 10}, Startup} }.
