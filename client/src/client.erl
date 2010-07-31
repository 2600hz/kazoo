%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 30 Jul 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(client).

-export([start/0, start_link/0, stop/1]).

start_link() ->
    ensure_started(amqp),
    client_sup:start_link().

start() ->
    ensure_started(amqp),
    application:start(client).

stop(_State) ->
    ok.

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
