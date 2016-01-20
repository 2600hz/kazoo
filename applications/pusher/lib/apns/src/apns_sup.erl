%%%-------------------------------------------------------------------
%%% @hidden
%%% @doc apns4erl main supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(apns_sup).
-author('Brujo Benavides <elbrujohalcon@inaka.net>').

-behaviour(supervisor).

-include("apns.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0, start_connection/1, start_connection/2]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
%% @hidden
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%% @hidden
-spec start_connection(apns:connection()) -> sup_startchild_ret().
start_connection(Connection) ->
    supervisor:start_child(?SERVER, [Connection]).

%% @hidden
-spec start_connection(atom(), apns:connection()) -> sup_startchild_ret().
start_connection(Name, Connection) ->
    supervisor:start_child(?SERVER, [Name, Connection]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @hidden
-spec init(any()) -> sup_init_ret().
init([]) ->
  {'ok',
   {{simple_one_for_one, 5, 10}, [?WORKER_TYPE('connection', 'transient')]}}.
