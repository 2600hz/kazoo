%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for preforming the ping requests for Monitor Agent Ping
%%% @end
%%% Created : 11 Nov 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_icmp).

-export([reachable/1, reachable/2]).

reachable(Dest) ->
    reachable(Dest, 1).

reachable(Dest, Count) ->
    Cmd = io_lib:format("ping -c ~p ~p &>/dev/null; echo $?", [Count, Dest]),
    os:cmd(Cmd) == "0\n".
