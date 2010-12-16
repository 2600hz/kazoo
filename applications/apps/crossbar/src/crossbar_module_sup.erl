%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010 James Aimonetti
%%% @doc
%%% Add crossbar modules dynamically
%%% @end
%%% Created :  Tue, 07 Dec 2010 19:26:22 GMT: James Aimonetti <james@2600hz.org>
-module(crossbar_module_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, upgrade/0, start_mod/2, stop_mod/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_mod(Mod, Args) ->
    supervisor:start_child(?MODULE, ?CHILD(Mod, worker, Args)),
    case erlang:function_exported(Mod, dispatch_config, 0) of
	true -> webmachine_router:add_route(Mod:dispatch_config());
	_ -> ok
    end.

stop_mod(Mod) ->
    supervisor:terminate_child(?MODULE, Mod),
    supervisor:delete_child(?MODULE, Mod).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Startup} = file:consult(filename:join(
				    [filename:dirname(code:which(?MODULE)),
				     "..", "priv", "crossbar.conf"])),
    {ok, { {one_for_one, 10, 10}
	   , lists:map(fun(M) -> ?CHILD(M, worker, []) end, props:get_value(autoload_modules, Startup, []))
	 } }.
