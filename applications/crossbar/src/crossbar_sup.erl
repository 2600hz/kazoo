%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([upgrade/0]).
-export([child_spec/1]).
-export([find_proc/1]).
-export([init/1]).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).

-define(ORIGIN_BINDINGS, [[{'type', kz_notification:pvt_type()}]
                         ]).

-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

-define(CHILDREN, [?WORKER('crossbar_init')
                  ,?SUPER('crossbar_module_sup')
                  ,?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)
                  ,?WORKER('crossbar_bindings')
                  ]
       ).

-define(DISPATCH_FILE, [code:priv_dir(?APP), "/dispatch.conf"]).

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

-spec child_spec(atom()) -> ?WORKER(atom()).
child_spec(Mod) ->
    ?WORKER(Mod).

-spec find_proc(atom()) -> pid().
find_proc(Mod) ->
    [P] = [P || {Mod1, P, _, _} <- supervisor:which_children(?SERVER), Mod =:= Mod1],
    P.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec upgrade() -> 'ok'.
upgrade() ->
    {'ok', {_, Specs}} = init([]),

    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?SERVER)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    lists:foreach(fun (Id) ->
                          _ = supervisor:terminate_child(?SERVER, Id),
                          supervisor:delete_child(?SERVER, Id)
                  end, sets:to_list(Kill)),
    lists:foreach(fun(Spec) -> supervisor:start_child(?SERVER, Spec) end, Specs),
    'ok'.

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
    _ = kz_util:set_startup(),
    {'ok', {{'one_for_one', 10, 10}, ?CHILDREN}}.
