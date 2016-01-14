%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Jon Blanton <jon@2600hz.com>
%%%-------------------------------------------------------------------
-module(whistle_stats_app).

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
start(_Type, _Args) ->
    whistle_stats_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(_) -> 'true'.
stop(_State) ->
    exit(whereis('whistle_stats_sup'), 'shutdown').
