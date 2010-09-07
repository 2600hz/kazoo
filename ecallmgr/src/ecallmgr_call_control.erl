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
    loop(Node, UUID, queue:new(), undefined, CtlQueue).

%% CurrApp is the Whistle Application that is currently running in FS (or undefined)
%% the next command in CmdQ doesn't run until CurrApp translates to the Event Name
%% passed from the Event queue on the {command_execute_complete, ...} message
loop(Node, UUID, CmdQ, CurrApp, CtlQ) ->
    format_log(info, "CONTROL(~p): entered loop(~p)~n", [self(), CurrApp]),
    receive
	{#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">> }
				       ,payload = Payload}} ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    format_log(info, "CONTROL(~p): Recv App Cmd:~p~n"
		       ,[self(), get_value(<<"Application-Name">>, Prop)]),
	    case get_value(<<"Application-Name">>, Prop) of
		<<"queue">> -> %% list of commands that need to be added
		    DefProp = whistle_api:extract_defaults(Prop), %% each command lacks the default headers
		    CmdQ1 = lists:foldl(fun({struct, Cmd}, TmpQ) ->
						queue:in(DefProp ++ Cmd, TmpQ)
					end, CmdQ, get_value(<<"Commands">>, Prop)),
		    case queue:is_empty(CmdQ) andalso not queue:is_empty(CmdQ1) andalso CurrApp =:= undefined of
			true ->
			    {{value, Cmd}, CmdQ2} = queue:out(CmdQ1),
			    ecallmgr_call_command:exec_cmd(Node, UUID, Cmd),
			    loop(Node, UUID, CmdQ2, get_value(<<"Application-Name">>, Cmd), CtlQ);
			false ->
			    loop(Node, UUID, CmdQ1, CurrApp, CtlQ)
		    end;
		_AppName ->
		    case queue:is_empty(CmdQ) andalso CurrApp =:= undefined of
			true ->
			    ecallmgr_call_command:exec_cmd(Node, UUID, Prop),
			    loop(Node, UUID, CmdQ, get_value(<<"Application-Name">>, Prop), CtlQ);
			false ->
			    loop(Node, UUID, queue:in(Prop, CmdQ), CurrApp, CtlQ)
		    end
	    end;
	#'basic.consume_ok'{}=BC ->
	    format_log(info, "CONTROL(~p): Curr(~p) received BC ~p~n", [self(), CurrApp, BC]),
	    loop(Node, UUID, CmdQ, CurrApp, CtlQ);
	{execute_complete, UUID, EvtName} ->
	    format_log(info, "CONTROL(~p): CurrApp: ~p execute_complete: ~p~n", [self(), CurrApp, EvtName]),
	    case whistle_api:convert_whistle_app_name(CurrApp) of
		undefined ->
		    loop(Node, UUID, CmdQ, CurrApp, CtlQ);
		EvtName ->
		    case queue:out(CmdQ) of
			{empty, _CmdQ1} -> loop(Node, UUID, CmdQ, undefined, CtlQ);
			{{value, Cmd}, CmdQ1} ->
			    ecallmgr_call_command:exec_cmd(Node, UUID, Cmd),
			    loop(Node, UUID, CmdQ1, get_value(<<"Application-Name">>, Cmd), CtlQ)
		    end;
		_OtherEvt ->
		    format_log(info, "CONTROL(~p): CurrApp: ~p Other: ~p~n", [self(), CurrApp, _OtherEvt]),
		    loop(Node, UUID, CmdQ, CurrApp, CtlQ)
	    end;
	{execute, UUID, EvtName} ->
	    format_log(info, "CONTROL(~p): CurrApp: ~p Received execute: ~p~n", [self(), CurrApp, EvtName]),
	    loop(Node, UUID, CmdQ, CurrApp, CtlQ);
	{hangup, UUID} ->
	    ecallmgr_amqp:delete_queue(CtlQ),
	    format_log(info, "CONTROL(~p): Received hangup, exiting...~n", [self()]);
	_Msg ->
	    format_log(info, "CONTROL(~p): Recv Unknown Msg:~n~p~n", [_Msg]),
	    loop(Node, UUID, CmdQ, CurrApp, CtlQ)
    end.
