%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(jonny5_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, upgrade/0, start_child/1, get_blacklist_server/0]).

%% Supervisor callbacks
-export([init/1]).

-include("jonny5.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(BL(I), {?BLACKLIST_SERVER, {I, start_link, []}, temporary, 5000, worker, [I]}).
-define(CACHE(Name), {Name, {wh_cache, start_link, [Name]}, permanent, 5000, worker, [wh_cache]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Module) ->
    supervisor:start_child(?MODULE, ?BL(Module)).

-spec get_blacklist_server/0 :: () -> {atom(), pid()}.
get_blacklist_server() ->
    [{_, _}=Pair] = [ {Mod, Pid} || {?BLACKLIST_SERVER, Pid, worker, [Mod]} <- supervisor:which_children(?MODULE)],
    Pair.

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    lists:foreach(fun (Id) ->
			  _ = supervisor:terminate_child(?MODULE, Id),
			  supervisor:delete_child(?MODULE, Id)
		  end, sets:to_list(Kill)),
    lists:foreach(fun(Spec) -> supervisor:start_child(?MODULE, Spec) end, Specs),
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 2, 5}
	   ,[
	     ?CACHE(j5_cache)
	     ,?CHILD(jonny5_acct_sup, supervisor)
	     ,?CHILD(jonny5_listener, worker)
	    ]
	 } }.
