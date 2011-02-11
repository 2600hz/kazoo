%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Created when a call hits a fetch_handler in ecallmgr_route.
%%% A Control Queue is created by the lookup_route function in the
%%% fetch_handler. On initialization, besides adding itself as the 
%%% consumer for the AMQP messages, Call Control creates an empty queue
%%% object (not to be confused with AMQP queues), sets the current
%%% application running on the switch to the empty binary, and records
%%% the timestamp of when the initialization finishes. The process then
%%% enters its loop to wait.
%%%
%%% When receiving an AMQP message, after decoding the JSON into a proplist,
%%% we check if the application is "queue" or not; if it is "queue", we
%%% extract the default headers out, iterate through the Commands portion,
%%% and append the default headers to the application-specific portions, and
%%% insert these commands into the CmdQ. We then check whether the old CmdQ is
%%% empty AND the new CmdQ is not, and that the current App is the empty
%%% binary. If so, we dequeue the next command, execute it, and loop; otherwise
%%% we loop with the CmdQ.
%%% If just a single application is sent in the message, we check the CmdQ's
%%% size and the current App's status; if both are empty, we fire the command
%%% immediately; otherwise we add the command to the CmdQ and loop.
%%%
%%% When receiving an {execute_complete, UUID, EvtName} tuple from
%%% the corresponding ecallmgr_call_events process tracking the call,
%%% we convert the CurrApp name from Whistle parlance to FS, matching
%%% it against what application name we got from FS via the events
%%% process. If CurrApp is empty, we just loop since the completed
%%% execution probably wasn't related to our stuff (perhaps FS internal);
%%% if the converted Whistle name matches the passed FS name, we know
%%% the CurrApp cmd has finished and can execute the next command in the
%%% queue. If there are no commands in the queue, set CurrApp to <<>> and
%%% loop; otherwise take the next command, execute it, and look with it as
%%% the CurrApp. If EvtName and the converted Whistle name don't match,
%%% something else executed that might have been related to the main
%%% application's execute (think set commands, like playback terminators);
%%% we can note the event happended, and continue looping as we were.
%%%
%%% @end
%%% Created : 26 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_control).

-export([start_link/3, init/3]).

-include("../include/amqp_client/include/amqp_client.hrl").

-import(logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

%% Node, UUID, {AmqpHost, CtlQueue}
start_link(Node, UUID, Amqp) ->
    {ok, spawn_link(ecallmgr_call_control, init, [Node, UUID, Amqp])}.

init(Node, UUID, {AmqpHost, CtlQueue}) ->
    amqp_util:basic_consume(AmqpHost, CtlQueue),
    format_log(info, "CONTROL(~p): initial loop call, consuming on ~p~n", [self(), CtlQueue]),
    loop(Node, UUID, queue:new(), <<>>, CtlQueue, erlang:now(), AmqpHost).

%% CurrApp is the Whistle Application that is currently running in FS (or empty)
%% the next command in CmdQ doesn't run until CurrApp translates to the Event Name
%% passed from the Event queue on the {command_execute_complete, ...} message
loop(Node, UUID, CmdQ, CurrApp, CtlQ, StartT, AmqpHost) ->
    format_log(info, "CONTROL(~p): entered loop(~p)~nUUID: ~p~n", [self(), CurrApp, UUID]),
    receive
	{#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">> }
				       ,payload = Payload}} ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    NewCmdQ = case get_value(<<"Application-Name">>, Prop) of
			  <<"queue">> -> %% list of commands that need to be added
			      format_log(info, "CONTROL(~p): Recv App Cmd: Queue~n", [self()]),
			      DefProp = whistle_api:extract_defaults(Prop), %% each command lacks the default headers
			      lists:foldl(fun({struct, []}, TmpQ) -> TmpQ;
					     ({struct, Cmd}, TmpQ) ->
						  format_log(info, "CONTROL.queue: Cmd: ~p~n", [DefProp ++ Cmd]),
						  queue:in(DefProp ++ Cmd, TmpQ)
					  end, CmdQ, get_value(<<"Commands">>, Prop));
			  _AppName ->
			      queue:in(Prop, CmdQ)
		      end,
	    case (not queue:is_empty(NewCmdQ)) andalso CurrApp =:= <<>> of
		true ->
		    {{value, Cmd}, NewCmdQ1} = queue:out(NewCmdQ),
		    format_log(info, "CONTROL(~p): CmdQ not empty, running ~p~n", [self(), Cmd]),
		    ecallmgr_call_command:exec_cmd(Node, UUID, Cmd, AmqpHost),
		    AppName = get_value(<<"Application-Name">>, Cmd),
		    loop(Node, UUID, NewCmdQ1, AppName, CtlQ, StartT, AmqpHost);
		false ->
		    loop(Node, UUID, NewCmdQ, CurrApp, CtlQ, StartT, AmqpHost)
	    end;
	{execute_complete, UUID, EvtName} ->
	    format_log(info, "CONTROL(~p): CurrApp: ~p(~p) execute_complete: ~p~n", [self(), CurrApp, whistle_api:convert_whistle_app_name(CurrApp), EvtName]),
	    case whistle_api:convert_whistle_app_name(CurrApp) of
		<<>> ->
		    loop(Node, UUID, CmdQ, CurrApp, CtlQ, StartT, AmqpHost);
		EvtName ->
		    case queue:out(CmdQ) of
			{empty, _CmdQ1} -> loop(Node, UUID, CmdQ, <<>>, CtlQ, StartT, AmqpHost);
			{{value, Cmd}, CmdQ1} ->
			    ecallmgr_call_command:exec_cmd(Node, UUID, Cmd, AmqpHost),
			    loop(Node, UUID, CmdQ1, get_value(<<"Application-Name">>, Cmd), CtlQ, StartT, AmqpHost)
		    end;
		_OtherEvt ->
		    format_log(info, "CONTROL(~p): CurrApp: ~p Other: ~p~n", [self(), CurrApp, _OtherEvt]),
		    loop(Node, UUID, CmdQ, CurrApp, CtlQ, StartT, AmqpHost)
	    end;
	{execute, UUID, EvtName} ->
	    format_log(info, "CONTROL(~p): CurrApp: ~p Received execute: ~p~n", [self(), CurrApp, EvtName]),
	    loop(Node, UUID, CmdQ, CurrApp, CtlQ, StartT, AmqpHost);
	{hangup, EvtPid, UUID} ->
	    amqp_util:unbind_q_from_callctl(AmqpHost, CtlQ),
	    amqp_util:delete_queue(AmqpHost, CtlQ), %% stop receiving messages
	    format_log(info, "CONTROL(~p): Received hangup, exiting (Time since process started: ~pms)~n"
		       ,[self(), timer:now_diff(erlang:now(), StartT) div 1000]),
	    EvtPid ! {ctl_down, self()};
	#'basic.consume_ok'{}=BC ->
	    format_log(info, "CONTROL(~p): Curr(~p) received BC ~p~n", [self(), CurrApp, BC]),
	    loop(Node, UUID, CmdQ, CurrApp, CtlQ, StartT, AmqpHost);
	_Msg ->
	    format_log(info, "CONTROL(~p): Recv Unknown Msg:~n~p~n", [self(), _Msg]),
	    loop(Node, UUID, CmdQ, CurrApp, CtlQ, StartT, AmqpHost)
    end.
