-module(whistle_amqp).
-behaviour(application).

-author('James Aimonetti <james@2600hz.com>').

-export([start/2, start/0, start_link/0, stop/0, stop/1]).

%%
start(_Type, _Args) ->
    start_deps(),
    whistle_amqp_sup:start_link().

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    whistle_amqp_sup:start_link().

%% @spec start() -> ok
%% @doc Start the amqp server.
start() ->
    start_deps(),
    application:start(whistle_amqp).

start_deps() ->
    whistle_amqp_deps:ensure(?MODULE),
    reloader:start(),
    logger:start(),

    ensure_started(sasl),
    ensure_started(riak_err),
    ensure_started(amqp_client).

%% @spec stop() -> ok
%% @doc Stop the amqp server.
stop() ->
    application:stop(whistle_amqp).

stop(_) ->
    ok.

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok	    
    end.
