%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_conference_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([add_node/2]).
-export([remove_node/1]).
-export([find_node/1]).
-export([init/1]).

-define(CHILDREN, [?SUPER('ecallmgr_conference_control_sup')
                  ,?WORKER('ecallmgr_fs_conferences_shared')
                  ,?WORKER('ecallmgr_fs_conferences')
                  ]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec add_node(atom(), kz_term:proplist()) -> kz_types:sup_startchild_ret().
add_node(Node, Options) ->
    Args = [Node, Options],
    ChildSpec = ?SUPER_NAME_ARGS_TYPE(Node, 'ecallmgr_fs_node_sup', Args, 'transient'),
    supervisor:start_child(?SERVER, ChildSpec).

-spec find_node(atom()) -> kz_term:api_pid().
find_node(Node) ->
    find_node(supervisor:which_children(?SERVER), Node).

find_node([], _) -> 'undefined';
find_node([{Node, Pid, 'supervisor', _}|_], Node) -> Pid;
find_node([_|Workers], Node) -> find_node(Workers, Node).

-spec remove_node(atom()) -> 'ok' | {'error', any()}.
remove_node(Node) ->
    _ = supervisor:terminate_child(?SERVER, Node),
    supervisor:delete_child(?SERVER, Node).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
