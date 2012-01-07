%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Promocalling
%%% @end
%%% Created :  8 Nov 2010 by James Aimonetti <james@2600hz.org>

-module(promocalling).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0, set_amqp_host/1, set_couch_host/1]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    promocalling_sup:start_link().

%% @spec start() -> ok
%% @doc Start the callmgr server.
start() ->
    start_deps(),
    application:start(promocalling).

start_deps() ->
    whistle_apps_deps:ensure(?MODULE),
    ensure_started(sasl), % logging
    ensure_started(crypto), % random
    ensure_started(whistle_amqp). % amqp wrapper

%% @spec stop() -> ok
%% @doc Stop the promocalling server.
stop() ->
    application:stop(promocalling).

set_amqp_host(H) ->
    H.

set_couch_host(H) ->
    H.

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
