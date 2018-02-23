%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(kazoo_fixturedb_app).

-behaviour(application).

-include("kz_fixturedb.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_fixturedb_sup:start_link().

%%--------------------------------------------------------------------
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
