%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(whistle_apps_app).
-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2, stop/1]).

-export([start/0]).


start() ->
    {'ok', _Apps} = application:ensure_all_started('whistle_apps').


%% Application callbacks

%% @public
%% @doc Implement the application start behaviour
-spec start(application:start_type(), any()) -> startlink_ret().
start(_StartType, _StartArgs) ->
    whistle_apps_sup:start_link().

%% @public
%% @doc Implement the application stop behaviour
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
