%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(webhooks_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(_, _) ->
                   {'ok', pid()} |
                   {'error', startlink_err()}.
start(_Type, _Args) -> webhooks:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(_) -> 'ok'.
stop(_State) -> webhooks:stop().
