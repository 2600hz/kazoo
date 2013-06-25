%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whapps_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/1]).
-export([start_app/1]).
-export([restart_app/1]).
-export([stop_app/1]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link(atoms()) -> startlink_ret().
start_link(Whapps) ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, [Whapps]).

-spec start_app(atom()) ->
                       {'ok', pid() | 'undefined'} |
                       {'ok', pid() | 'undefined', term()} |
                       {'error', term()}.
start_app(App) ->
    supervisor:start_child(?MODULE, ?SUPER(App)).

-spec restart_app(atom()) ->
                         {'ok', pid() | 'undefined'} |
                         {'ok', pid() | 'undefined', term()} |
                         {'error', term()}.
restart_app(App) ->
    _ = supervisor:terminate_child(?MODULE, App),
    supervisor:restart_child(?MODULE, App).

stop_app(App) ->
    _ = supervisor:terminate_child(?MODULE, App),
    supervisor:delete_child(?MODULE, App).

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
    Children = [?SUPER(Whapp) || Whapp <- Whapps],

    {'ok', {SupFlags, Children}}.
