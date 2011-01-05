%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% CLI interface to Call Manager
%%% @end
%%% Created : 19 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_cli).

-export([help/2, diagnostics/2, set_amqp_host/2, add_fs_node/2, rm_fs_node/2]).

-include_lib("erlctl/include/erlctl.hrl").

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
     ,{" diagnostics  Print data concerning the ~s_fs_handler server",[App]}
     ,{" set_amqp_host <host>  Set the amqp host (e.g. ~p)", [net_adm:localhost()]}
     ," add_fs_node <node>  Add a FreeSWITCH node to ecallmgr (e.g. 'freeswitch@server.com')"
     ," rm_fs_node <node>  Remove a FreeSWITCH node from ecallmgr (e.g. 'freeswitch@server.com')"
    ].

diagnostics(always, _) ->
    Node = list_to_atom(lists:flatten(["ecallmgr@", net_adm:localhost()])),
    Res = rpc_call(Node, ecallmgr_fs_handler, diagnostics, []),
    case Res of 
	{ok, _, _} -> Res;
	{ok, Data} ->
	    diagnostics_server:display_fs_data(Data),
	    ok;
	_ ->
	    format("Diagnostics error: ~p~n", [Res])
    end.

set_amqp_host(always, [Host]=Arg) ->
    Node = list_to_atom(lists:flatten(["ecallmgr@", net_adm:localhost()])),
    format("Setting AMQP host to ~p on ~p~n", [Host, Node]),
    case rpc_call(Node, ecallmgr_fs_handler, set_amqp_host, Arg) of
	{ok, ok} ->
	    {ok, "Set ecallmgr's amqp host to ~p", [Host]};
	{ok, Other} ->
	    {ok, "Something unexpected happened while setting the amqp host: ~p", [Other]}
    end.

add_fs_node(always, [FSNode]) ->
    Node = list_to_atom(lists:flatten(["ecallmgr@", net_adm:localhost()])),
    case rpc_call(Node, ecallmgr_fs_handler, add_fs_node, [list_to_atom(FSNode)]) of
	{ok, ok} ->
	    {ok, "Added ~p successfully~n", [FSNode]};
	{ok, Res} ->
	    {ok, "Failed to add node(~p): ~p~n", [FSNode, Res]}
    end.

rm_fs_node(always, [FSNode]) ->
    Node = list_to_atom(lists:flatten(["ecallmgr@", net_adm:localhost()])),
    case rpc_call(Node, ecallmgr_fs_handler, rm_fs_node, [list_to_atom(FSNode)]) of
	{ok, _} ->
	    {ok, "Removed ~p successfully~n", [FSNode]};
	Other -> %{ok, Res} ->
	    {ok, "Failed to remove node(~p): ~p~n", [FSNode, Other]}
    end.

rpc_call(Node, M, F, A) ->
    case net_adm:ping(Node) of
	pong ->
	    {ok, rpc:call(Node, M, F, A)};
	pang ->
	    {ok, "~p not reachable", [Node]}
    end.
