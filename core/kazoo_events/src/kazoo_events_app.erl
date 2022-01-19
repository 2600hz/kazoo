%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_events_app).
-behaviour(application).

-include("kazoo_events.hrl").

-export([start/2, stop/1]).
-export([start/0]).


-spec start() -> {'ok', kz_types:atoms()}.
start() ->
    {'ok', _Apps} = application:ensure_all_started(?APP).

%% Application callbacks

%% @doc Implement the application start behaviour
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_events_sup:start_link().

%% @doc Implement the application stop behaviour
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
