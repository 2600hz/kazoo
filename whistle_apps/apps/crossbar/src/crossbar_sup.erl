%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(crossbar_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, upgrade/0]).
-export([cache_proc/0, child_spec/1, find_proc/1]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("crossbar/include/crossbar.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start, [Args]}, permanent, 5000, Type, dynamic}).
-define(CACHE(Name), {Name, {wh_cache, start_link, [Name]}, permanent, 5000, worker, [wh_cache]}).
-define(DISPATCH_FILE, [code:lib_dir(crossbar, priv), "/dispatch.conf"]).
-define(DEFAULT_LOG_DIR, wh_util:to_binary(code:lib_dir(crossbar, log))).

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

-spec child_spec/1 :: (atom()) -> ?CHILD(atom, worker).
child_spec(Mod) ->
    ?CHILD(Mod, worker).

-spec find_proc/1 :: (atom()) -> pid().
find_proc(Mod) ->
    [P] = [P || {Mod1, P, _, _} <- supervisor:which_children(?MODULE), Mod =:= Mod1],
    P.

-spec cache_proc/0 :: () -> {'ok', pid()}.
cache_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= crossbar_cache],
    {ok, P}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec upgrade/0 :: () -> ok.
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> sup_init_ret() when
      Args :: [].
init([]) ->
    {ok, {{one_for_one, 10, 10}, [?CACHE(crossbar_cache)
                                  ,?CHILD(crossbar_bindings, worker)
                                 ]}}.
