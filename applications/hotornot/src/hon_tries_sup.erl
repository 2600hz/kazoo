%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
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
                   || Ratedeck <- hotornot_config:ratedecks()
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_trie(ne_binary()) -> startlink_ret().
start_trie(Ratedeck) ->
    RatedeckDb = kzd_ratedeck:format_ratedeck_db(Ratedeck),
    supervisor:start_child(?SERVER, ?WORKER_NAME_ARGS('hon_trie', RatedeckDb, [RatedeckDb])).

-spec restart_trie(ne_binary()) -> startlink_ret().
restart_trie(Ratedeck) ->
    RatedeckDb = kzd_ratedeck:format_ratedeck_db(Ratedeck),
    supervisor:restart_child(?SERVER, RatedeckDb).

-spec stop_trie(server_ref()) -> 'ok' | {'error', any()}.
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


%% @doc
%% Add processes if necessary.
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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init(any()) -> sup_init_ret().
init([]) ->
    kz_util:set_startup(),

    RestartStrategy = 'one_for_one',
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
