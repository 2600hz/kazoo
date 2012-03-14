%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% CLI interface to Whistle Apps (whapps) Container
%%% @end
%%% Created : 10 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(whistle_apps_cli).

-export([help/2, set_amqp_host/2, set_couch_host/2, start_app/2, stop_app/2, running_apps/2]).

-include_lib("../../lib/erlctl/lib/erlctl-0.3/include/erlctl.hrl").

help(always,_) ->
  lists:foreach(
    fun({E,D}) ->
            format(E ++ "~n", D);
       (E) ->
            format(E ++ "~n", [])
    end, usage()),
  ok.

usage() ->
    Opts = erlctl:get_opts(),
    App = proplists:get_value(app,Opts),
    Script = proplists:get_value(script,Opts),
    [
     {"Usage for ~s:",[App]}
     ,{" ~s <command> ...",[Script]}
     ,""
     ,"Commands:"
     ,{" set_amqp_host <host>  Set the amqp host (e.g. ~p)", [net_adm:localhost()]}
     ,{" set_couch_host <host>  Set the amqp host (e.g. ~p)", [net_adm:localhost()]}
     ," start_app <whapp>  Start (if not already) the whapp"
     ," stop_app <whapp>  Stop (if started) the whapp"
     ," running_apps  List of whapps currently running"
    ].

set_amqp_host(always, [Host]=Arg) ->
    Node = list_to_atom(lists:flatten(["whistle_apps@", net_adm:localhost()])),
    format("Setting AMQP host to ~p on ~p~n", [Host, Node]),
    case rpc_call(Node, whapps_controller, set_amqp_host, Arg) of
        {ok, ok} ->
            {ok, "Set whistle controller's amqp host to ~p", [Host]};
        {ok, Other} ->
            {ok, "Something unexpected happened while setting the amqp host: ~p", [Other]};
        {error, Fmt, Args} ->
            format(Fmt, Args)
    end.

set_couch_host(always, [Host]) ->
    Node = list_to_atom(lists:flatten(["whistle_apps@", net_adm:localhost()])),
    format("Setting CouchDB Host to ~p on ~p~n", [Host, Node]),
    User = io:get_line("CouchDB Username: "),
    Pass = io:get_line("CouchDB Password: "),
    case rpc_call(Node, whapps_controller, set_couch_host, [Host, string:strip(User, right, $\n), string:strip(Pass, right, $\n)]) of
        {ok, ok} ->
            {ok, "Set whistle controller's couch host to ~p", [Host]};
        {ok, Other} ->
            {ok, "Something unexpected happened while setting the couch host: ~p", [Other]};
        {error, Fmt, Args} ->
            format(Fmt, Args)
    end.

start_app(always, [Whapp]) ->
    Node = list_to_atom(lists:flatten(["whistle_apps@", net_adm:localhost()])),
    format("Starting whapp ~p on ~p~n", [Whapp, Node]),
    case rpc_call(Node, whapps_controller, start_app, [list_to_atom(Whapp)]) of
        {ok, ok} ->
            {ok, "~p started successfully", [Whapp]};
        {ok, Other} ->
            {ok, "Something unexpected happened while starting ~p: ~p", [Whapp, Other]};
        {error, Fmt, Args} ->
            format(Fmt, Args)
    end.

stop_app(always, [Whapp]) ->
    Node = list_to_atom(lists:flatten(["whistle_apps@", net_adm:localhost()])),
    format("Stopping whapp ~p on ~p~n", [Whapp, Node]),
    case rpc_call(Node, whapps_controller, stop_app, [list_to_atom(Whapp)]) of
        {ok, ok} ->
            {ok, "~p stopped successfully", [Whapp]};
        {ok, Other} ->
            {ok, "Something unexpected happened while stopping ~p: ~p", [Whapp, Other]};
        {error, Fmt, Args} ->
            format(Fmt, Args)
    end.

running_apps(always, []) ->
    Node = list_to_atom(lists:flatten(["whistle_apps@", net_adm:localhost()])),
    format("Searching for running whapps on ~p~n", [Node]),
    case rpc_call(Node, whapps_controller, running_apps, []) of
        {ok, Apps} when is_list(Apps) ->
            {ok, "~p~n", [Apps]};
        {ok, Other} ->
            {ok, "Something unexpected happened while looking for running whapps: ~p", [Other]};
        {error, Fmt, Args} ->
            format(Fmt, Args)
    end.

rpc_call(Node, M, F, A) ->
    case net_adm:ping(Node) of
        pong ->
            {ok, rpc:call(Node, M, F, A)};
        pang ->
            {error, "The whapps node ~p is not reachable. Have you started it yet?~nTry ./start.sh or ./start-dev.sh~n~n", [Node]}
    end.
