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
    %% TODO: just return ..._sup:start_link().
    io:format("ensure whistle_services deps started~n"),
    application:ensure_all_started('whistle_services'),
    io:format("ensure whistle_services sup started~n"),
    whistle_services_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(_) -> 'ok'.
stop(_State) ->
    'ok'.
