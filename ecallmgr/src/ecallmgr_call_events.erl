%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive call events from freeSWITCH, publish to the call's event
%%% queue
%%% @end
%%% Created : 25 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_events).

-export([start/4, init/4]).

-import(logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

%% Node, UUID, {Channel, Ticket, EvtQueue}
start(Node, UUID, Amqp, CtlPid) ->
    spawn(ecallmgr_call_events, init, [Node, UUID, Amqp, CtlPid]).

init(Node, UUID, Amqp, CtlPid) ->
    freeswitch:handlecall(Node, UUID),
    loop(UUID, Amqp, CtlPid).

%% Amqp = {Channel, Ticket, EvtQueue}
loop(UUID, Amqp, CtlPid) ->
    receive
	{call, {event, [UUID | Data]}} ->
	    format_log(info, "EVT(~p): {Call, {Event}} for ~p: ~p~n", [self(), UUID, get_value(<<"Event-Name">>, Data)]),
	    publish_msg(Amqp, Data),
	    loop(UUID, Amqp, CtlPid);
	{call_event, {event, [ UUID | Data ] } } ->
	    format_log(info, "EVT(~p): {Call_Event, {Event}} for ~p: ~p~n"
		       ,[self(), UUID, get_value(<<"Event-Name">>, Data)]),
	    publish_msg(Amqp, Data),
	    send_ctl_event(CtlPid, UUID, get_value(<<"Event-Name">>, Data), get_value(<<"Application">>, Data)),
	    loop(UUID, Amqp, CtlPid);
	call_hangup ->
	    CtlPid ! {hangup, UUID},
	    format_log(info, "EVT(~p): Call Hangup~n", [self()]);
	_Msg ->
	    format_log(error, "EVT(~p): Unhandled FS Msg: ~n~p~n", [self(), _Msg]),
	    loop(UUID, Amqp, CtlPid)
    end.

%% let the ctl process know a command finished executing
send_ctl_event(CtlPid, UUID, <<"CHANNEL_EXECUTE_COMPLETE">>, AppName) ->
    CtlPid ! {execute_complete, UUID, AppName};
send_ctl_event(CtlPid, UUID, <<"CHANNEL_EXECUTE">>, AppName) ->
    CtlPid ! {execute, UUID, AppName};
send_ctl_event(_CtlPid, _UUID, _Evt, _Data) ->
    ok.

publish_msg({Channel, Ticket, EvtQueue}, Prop) ->
    DefProp = whistle_api:default_headers(EvtQueue, <<"Call-Event">>, <<"ecallmgr.event">>, <<"0.1">>
					      , get_value(<<"Event-Date-Timestamp">>, Prop)),
    Data = Prop ++ DefProp,
    case whistle_api:call_event(Data) of
	{ok, JSON} ->
	    {BP, AmqpMsg} = amqp_util:callevt_publish(Ticket
						      ,EvtQueue
						      ,list_to_binary(JSON)
						      ,<<"application/json">>
						     ),
	    %% execute the publish command
	    amqp_channel:call(Channel, BP, AmqpMsg);
	{error, Msg} ->
	    format_log(error, "EVT(~p): Bad event API ~p~n", [self(), Msg])
    end.
