%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(jonny5_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("jonny5/src/jonny5.hrl").

-export([start_link/0]).
-export([start_child/1]).
-export([get_blacklist_server/0]).
-export([upgrade/0]).
-export([listener_proc/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Type), fun(N, cache) -> {N, {wh_cache, start_link, [N]}, permanent, 5000, worker, [wh_cache]};
                              (N, T) -> {N, {N, start_link, []}, permanent, 5000, T, [N]} end(Name, Type)).
-define(BL(I), {?BLACKLIST_SERVER, {I, start_link, []}, temporary, 5000, worker, [I]}).
-define(CHILDREN, [{?JONNY5_CACHE, cache}
                   ,{j5_call_monitor_sup, supervisor}
                   ,{jonny5_acct_sup, supervisor}
                   ,{jonny5_listener, worker}
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
-spec start_link/0 :: () -> startlink_ret().
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

-spec listener_proc/0 :: () -> {'ok', pid()}.
listener_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= skel_listener],
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
