%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_pinger_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([add_node/2]).
-export([find_pinger/1]).
-export([remove_node/1]).
-export([init/1]).

-define(CHILDREN, []).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec add_node(atom(), kz_proplist()) -> sup_startchild_ret().
add_node(Node, Options) ->
    ChildSpec = ?WORKER_NAME_ARGS_TYPE(Node, 'ecallmgr_fs_pinger', [Node, Options], 'transient'),
    supervisor:start_child(?SERVER, ChildSpec).

-spec find_pinger(atom()) -> api_pid().
find_pinger(Node) ->
    Workers = supervisor:which_children(?MODULE),
    find_pinger(Workers, Node).

find_pinger([], _) -> 'undefined';
find_pinger([{Node, Pid, 'worker', _}|_], Node) -> Pid;
find_pinger([_|Workers], Node) ->
    find_pinger(Workers, Node).

-spec remove_node(atom()) -> 'ok' | {'error', any()}.
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
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
