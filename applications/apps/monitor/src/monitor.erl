%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Service/Server Monitoring
%%% @end
%%% Created :  11 Nov 2010 by Karl Anderson <karl@2600hz.org>

-module(monitor).

-author('Karl Anderson <karl@2600hz.com>').
-export([start/0, start_link/0, start_link/1, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the monitor for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    monitor_sup:start_link().

start_link(AHost) ->
    start_deps(),
    monitor_sup:start_link(AHost).

%% @spec start() -> ok
%% @doc Start the monitor server.
start() ->
    application:start(monitor).
    
start_deps() ->
    monitor_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(whistle_amqp),
    ensure_started(dynamic_compile),
    ensure_started(log_roller),
    ensure_started(couchbeam),
    ensure_started(whistle_couch).

%% @spec stop() -> ok
%% @doc Stop the monitor server.
stop() ->
    application:stop(monitor).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
