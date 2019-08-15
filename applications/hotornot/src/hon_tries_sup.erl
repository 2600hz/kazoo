%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(hon_tries_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,start_trie/1, restart_trie/1
        ,stop_trie/1
        ,upgrade/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("hotornot.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_NAME_ARGS('hon_trie', Ratedeck, [Ratedeck])
                   || Ratedeck <- kz_services_ratedecks:ratedecks()
                  ]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_trie(kz_term:ne_binary()) -> kz_types:startlink_ret().
start_trie(Ratedeck) ->
    RatedeckDb = kzd_ratedeck:format_ratedeck_db(Ratedeck),
    supervisor:start_child(?SERVER, ?WORKER_NAME_ARGS('hon_trie', RatedeckDb, [RatedeckDb])).

-spec restart_trie(kz_term:ne_binary()) -> kz_types:startlink_ret().
restart_trie(Ratedeck) ->
    RatedeckDb = kzd_ratedeck:format_ratedeck_db(Ratedeck),
    supervisor:restart_child(?SERVER, RatedeckDb).

-spec stop_trie(kz_types:server_ref()) -> 'ok' | {'error', any()}.
stop_trie(Pid) when is_pid(Pid) ->
    case [Id || {Id, P, _, _} <- supervisor:which_children(?SERVER),
                Pid =:= P
         ]
    of
        [Id] -> supervisor:terminate_child(?SERVER, Id);
        [] -> 'ok'
    end;
stop_trie('undefined') -> 'ok';
stop_trie(Name) -> stop_trie(whereis(Name)).


%% @doc Add processes if necessary.
%% @end
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
    lists:foreach(fun(Spec) -> supervisor:start_child(?SERVER, Spec) end, Specs).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    _ = kz_util:set_startup(),

    RestartStrategy = 'one_for_one',
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
