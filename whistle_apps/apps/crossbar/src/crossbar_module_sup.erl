%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010 James Aimonetti
%%% @doc
%%% Add crossbar modules dynamically
%%% @end
%%% Created :  Tue, 07 Dec 2010 19:26:22 GMT: James Aimonetti <james@2600hz.org>
-module(crossbar_module_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, upgrade/0, start_mod/1, start_mod/2, stop_mod/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(CONF_FILE, [code:lib_dir(crossbar, priv), "/crossbar.conf"]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec(start_mod/1 :: (Mod :: atom()) -> no_return()).
start_mod(Mod) -> start_mod(Mod, []).

-spec(start_mod/2 :: (Mod :: atom(), Args :: list(term())) -> no_return()).
start_mod(Mod, Args) ->
    supervisor:start_child(?MODULE, ?CHILD(Mod, worker, Args)).

-spec(stop_mod/1 :: (Mod :: atom()) -> no_return()).
stop_mod(Mod) ->
    _ = supervisor:terminate_child(?MODULE, Mod),
    supervisor:delete_child(?MODULE, Mod).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    lists:foreach(fun(Id) ->
			  _ = supervisor:terminate_child(?MODULE, Id),
			  supervisor:delete_child(?MODULE, Id)
		  end, sets:to_list(Kill)),

    lists:foreach(fun(Spec) -> supervisor:start_child(?MODULE, Spec) end, Specs),
    ok.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Startup} = file:consult(?CONF_FILE),
    {ok, { {one_for_one, 10, 10}
	   ,[ ?CHILD(M, worker, []) || M <- props:get_value(autoload_modules, Startup, []) ]
	 } }.
