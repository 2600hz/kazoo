%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_globals_app).
-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).
-export([start/0]).

-spec start() -> {'ok', kz_term:atoms()}.
start() ->
    {'ok', _Apps} = application:ensure_all_started('kazoo_globals').

%% Application callbacks

%% @doc Implement the application start behaviour.
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_globals_sup:start_link().

%% @doc Implement the application stop behaviour.
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
