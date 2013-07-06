%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([add_node/2]).
-export([remove_node/1]).
-export([find_node/1]).
-export([init/1]).

-define(CHILD(Name, Type), fun(N, 'worker'=T) ->
                                   {N, {N, 'start_link', []}, 'permanent', 5000, T, [N]};
                              (N, 'supervisor'=T) ->
                                   {N, {N, 'start_link', []}, 'permanent', 'infinity', T, [N]}
                           end(Name, Type)).
-define(NODE(N, As), {N, {'ecallmgr_fs_node_sup', 'start_link', As}
                      ,'transient', 50000, 'supervisor', ['ecallmgr_fs_node_sup']
                     }).
-define(CHILDREN, [{'ecallmgr_fs_pinger_sup', 'supervisor'}
                   ,{'ecallmgr_fs_nodes', 'worker'}
                   ,{'ecallmgr_fs_channels', 'worker'}
                   ,{'ecallmgr_fs_calls', 'worker'}
                   ,{'ecallmgr_fs_conferences', 'worker'}
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
-spec start_link() -> startlink_ret().
start_link() -> supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec add_node(atom(), wh_proplist()) ->
                      {'error', term()} | 
                      {'ok','undefined' | pid()} | 
                      {'ok','undefined' | pid(), term()}.
add_node(Node, Options) -> supervisor:start_child(?SERVER, ?NODE(Node, [Node, Options])).

find_node(Node) -> find_node(supervisor:which_children(?MODULE), Node).

find_node([], _) -> 'undefined';
find_node([{Node, Pid, 'supervisor', _}|_], Node) -> Pid;
find_node([_|Workers], Node) -> find_node(Workers, Node).

-spec remove_node(atom()) -> 'ok' | {'error', 'running' | 'not_found' | 'simple_one_for_one'}.
remove_node(Node) ->
    _ = supervisor:terminate_child(?SERVER, Node),
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
-spec init(list()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = [?CHILD(Name, Type) || {Name, Type} <- ?CHILDREN],

    {'ok', {SupFlags, Children}}.
