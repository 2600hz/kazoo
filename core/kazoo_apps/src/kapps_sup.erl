%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kapps_sup).
-behaviour(supervisor).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, ?MODULE).

-export([start_link/1]).
-export([start_app/1]).
-export([restart_app/1]).
-export([stop_app/1]).
-export([init/1]).

-define(CHILDREN, [?SUPER(Whapp) || Whapp <- Whapps]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link(atoms()) -> startlink_ret().
start_link(Whapps) ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, [Whapps]).

-spec start_app(atom()) -> sup_startchild_ret().
start_app(App) ->
    supervisor:start_child(?SERVER, ?SUPER(App)).

-spec restart_app(atom()) -> sup_startchild_ret().
restart_app(App) ->
    _ = supervisor:terminate_child(?SERVER, App),
    supervisor:restart_child(?SERVER, App).

-spec stop_app(atom()) -> 'ok' | {'error', atom()}.
stop_app(App) ->
    _ = supervisor:terminate_child(?SERVER, App),
    supervisor:delete_child(?SERVER, App).

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
-spec init([atoms()]) -> sup_init_ret().
init([Whapps]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
