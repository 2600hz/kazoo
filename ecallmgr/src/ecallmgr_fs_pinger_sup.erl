%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_pinger_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").
-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([add_node/2]).
-export([find_pinger/1]).
-export([remove_node/1]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Type, Args), fun(N, cache) -> {N, {wh_cache, start_link, [N]}, permanent, 5000, worker, [wh_cache]};
                                    (N, T, A) -> {N, {N, start_link, A}, permanent, 5000, T, [N]} end(Name, Type, Args)).
-define(PINGER(Node, Opts), {Node, {ecallmgr_fs_pinger, start_link, [Node, Opts]}, transient, 5000, worker, [Node]}).
-define(CHILDREN, []).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec add_node(atom(), proplist()) -> {'error', term()} | 
                                            {'ok','undefined' | pid()} | 
                                            {'ok','undefined' | pid(), term()}.
add_node(Node, Options) ->
    supervisor:start_child(?SERVER, ?PINGER(Node, Options)).

find_pinger(Node) ->
    Workers = supervisor:which_children(ecallmgr_fs_pinger_sup),
    find_pinger(Workers, Node).

find_pinger([], _) -> undefined;
find_pinger([{Node, Pid, worker, _}|_], Node) -> Pid;
find_pinger([_|Workers], Node) ->
    find_pinger(Workers, Node).

-spec remove_node/1 :: (atom()) -> 'ok' | {'error', 'running' | 'not_found' | 'simple_one_for_one'}.
remove_node(Node) ->
    _T = supervisor:terminate_child(?SERVER, Node),
    lager:debug("terminated pinger: ~p", [_T]),
    supervisor:delete_child(?SERVER, Node).

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
    Children = [],

    {ok, {SupFlags, Children}}.
