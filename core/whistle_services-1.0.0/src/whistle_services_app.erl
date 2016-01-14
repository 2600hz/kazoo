%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_services_app).

-behaviour(application).

-include("whistle_services.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(_, _) -> {'ok', pid()} | {'error', startlink_err()}.
start(_Type, _Args) ->
    whistle_services_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(_) -> 'ok'.
stop(_State) ->
    exit(whereis('whistle_services_sup'), 'shutdown').
