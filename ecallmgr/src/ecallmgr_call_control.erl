%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive call control commands from amqp queue, send to freeswitch
%%% @end
%%% Created : 26 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_control).

-export([start/3, init/3]).

-include("../include/amqp_client/include/amqp_client.hrl").

-import(logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

%% Node, UUID, {Channel, Ticket, CtlQueue}
start(Node, UUID, Amqp) ->
    spawn(ecallmgr_call_control, init, [Node, UUID, Amqp]).

init(Node, UUID, {Channel, Ticket, CtlQueue}) ->
    BC = amqp_util:basic_consume(Ticket, CtlQueue),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, BC, self()),
    loop(Node, UUID).

loop(Node, UUID) ->
    receive
	{#'basic.deliver'{}, #amqp_msg{props = Props, payload = Payload}} ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    format_log(info, "CONTROL(~p): Recv Content ~p Data:~n~p~n", [Props#'P_basic'.content_type, Prop]),
	    loop(Node, UUID);
	#'basic.consume_ok'{} ->
	    loop(Node, UUID);
	{hangup, UUID} ->
	    format_log(info, "CONTROL(~p): Received hangup, exiting...~n", [self()]);
	_Msg ->
	    format_log(info, "CONTROL(~p): Recv Unknown Msg:~n~p~n", [_Msg]),
	    loop(Node, UUID)
    end.
