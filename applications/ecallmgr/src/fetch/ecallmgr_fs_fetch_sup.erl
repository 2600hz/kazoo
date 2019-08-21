%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_fetch_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-export([start_link/2]).
-export([init/1]).

-define(CHILDREN, [event_child(Node, Section) || Section <- ?FS_FETCH_SECTIONS]).

%% ===================================================================
%% API functions
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) ->
    supervisor:start_link({'local', sup_name(Node)}, ?MODULE, [Node, Options]).

sup_name(Node) ->
    Name = iolist_to_binary([kz_term:to_binary(?MODULE)
                            ,"_"
                            ,kz_term:to_binary(Node)
                            ]),
    kz_term:to_atom(Name, 'true').

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> kz_types:sup_init_ret().
init([Node, _Props]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 6,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

-spec event_child(atom(), atom()) -> kz_types:sup_child_spec().
event_child(Node, Section) ->
    ?WORKER_NAME_ARGS_TYPE(Section, 'ecallmgr_fs_fetch', [Node, Section], 'transient').
