%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010 James Aimonetti
%%% @doc
%%% Add crossbar modules dynamically
%%% @end
%%% Created :  Tue, 07 Dec 2010 19:26:22 GMT: James Aimonetti <james@2600hz.org>
-module(crossbar_module_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_log.hrl").

%% API
-export([start_link/0, upgrade/0, start_mod/1, start_mod/2, stop_mod/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec(start_mod/1 :: (Mod :: atom()) -> no_return()).
start_mod(Mod) ->
    ?LOG("starting crossbar module ~s", [Mod]),
    start_mod(Mod, []).

-spec(start_mod/2 :: (Mod :: atom(), Args :: list(term())) -> no_return()).
start_mod(Mod, Args) ->
    ?LOG("starting crossbar module ~s", [Mod]),
    supervisor:start_child(?MODULE, ?CHILD(Mod, worker, Args)).

-spec(stop_mod/1 :: (Mod :: atom()) -> no_return()).
stop_mod(Mod) ->
    ?LOG("stopping crossbar module ~s", [Mod]),
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
    lists:foreach(fun(Mod) -> stop_mod(Mod) end, sets:to_list(Kill)),
    lists:foreach(fun(Spec) -> supervisor:start_child(?MODULE, Spec) end, Specs),
    ok.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    StartModules = whapps_config:get(<<"crossbar">>, <<"autoload_modules">>, []),
    Children = [begin
                    ?LOG("initializing crossbar module ~s", [M]),
                    ?CHILD(wh_util:to_atom(M, true), worker, [])
                end || M <- StartModules
               ],
    {ok, { {one_for_one, 10, 10}, Children } }.
