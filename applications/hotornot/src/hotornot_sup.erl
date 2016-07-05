%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hotornot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, upgrade/0]).

%% Supervisor callbacks
-export([init/1]).

-include("hotornot.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?CACHE(?CACHE_NAME)
                  ,?WORKER('hotornot_listener')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
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
