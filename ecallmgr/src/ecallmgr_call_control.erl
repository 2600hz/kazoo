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
    loop(Node, UUID, queue:new(), true).

%% SendReady - whether FS is ready for the next command
loop(Node, UUID, CmdQ, SendReady) ->
    format_log(info, "CONTROL(~p): entered loop(~p)~n", [self(), SendReady]),
    receive
	{#'basic.deliver'{}, #amqp_msg{props = Props, payload = Payload}} ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    format_log(info, "CONTROL(~p): Recv Content ~p Data:~n~p~n", [self(), Props#'P_basic'.content_type, Prop]),
	    case get_value(<<"Application-Name">>, Prop) of
		<<"queue">> -> %% list of commands that need to be added
		    DefProp = whistle_api:extract_defaults(Prop), %% each command lacks the default headers
		    CmdQ1 = lists:foldl(fun({struct, Cmd}, TmpQ) ->
						queue:in(DefProp ++ Cmd, TmpQ)
					end, CmdQ, get_value(<<"Commands">>, Prop)),
		    case queue:is_empty(CmdQ) andalso not queue:is_empty(CmdQ1) andalso SendReady =:= true of
			true ->
			    {{value, Cmd}, CmdQ2} = queue:out(CmdQ1),
			    ecallmgr_call_command:exec_cmd(Node, UUID, Cmd),
			    loop(Node, UUID, CmdQ2, false);
			false ->
			    loop(Node, UUID, CmdQ1, SendReady)
		    end;
		_AppName ->
		    case queue:is_empty(CmdQ) andalso SendReady =:= true of
			true ->
			    ecallmgr_call_command:exec_cmd(Node, UUID, Prop),
			    loop(Node, UUID, CmdQ, false);
			false ->
			    loop(Node, UUID, queue:in(Prop, CmdQ), SendReady)
		    end
	    end;
	#'basic.consume_ok'{} ->
	    loop(Node, UUID, CmdQ, SendReady);
	{execute_complete, UUID} ->
	    format_log(info, "CONTROL(~p): Received execute_complete~n", [self()]),
	    case queue:out(CmdQ) of
		{empty, _CmdQ1} -> loop(Node, UUID, CmdQ, true);
		{{value, Cmd}, CmdQ1} ->
		    ecallmgr_call_command:exec_cmd(Node, UUID, Cmd),
		    loop(Node, UUID, CmdQ1, queue:is_empty(CmdQ1))
	    end;
	{hangup, UUID} ->
	    format_log(info, "CONTROL(~p): Received hangup, exiting...~n", [self()]);
	_Msg ->
	    format_log(info, "CONTROL(~p): Recv Unknown Msg:~n~p~n", [_Msg]),
	    loop(Node, UUID, CmdQ, SendReady)
    end.
