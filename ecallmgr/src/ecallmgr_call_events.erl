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

-include("whistle_api.hrl").

-import(logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-define(APPNAME, <<"ecallmgr.call.event">>).
-define(APPVER, <<"0.4.0">>).
-define(EVENT_CAT, <<"Call-Event">>).

%% Node, UUID, AmqpHost, CtlPid
start(Node, UUID, Amqp, CtlPid) ->
    spawn(ecallmgr_call_events, init, [Node, UUID, Amqp, CtlPid]).

init(Node, UUID, Amqp, CtlPid) ->
    freeswitch:handlecall(Node, UUID),
    loop(UUID, Amqp, CtlPid).

-spec(loop/3 :: (UUID :: binary(), Amqp :: string(), CtlPid :: pid()) -> no_return()).
loop(UUID, Amqp, CtlPid) ->
    receive
	{call, {event, [UUID | Data]}} ->
	    format_log(info, "EVT(~p): {Call, {Event}} for ~p: ~p~n", [self(), UUID, get_value(<<"Event-Name">>, Data)]),
	    publish_msg(Amqp, UUID, Data),
	    loop(UUID, Amqp, CtlPid);
	{call_event, {event, [ UUID | Data ] } } ->
	    EvtName = get_value(<<"Event-Name">>, Data),
	    AppName = get_value(<<"Application">>, Data),
	    format_log(info, "EVT(~p): {Call_Event, {Event}} for ~p(~p): ~p~n"
		       ,[self(), UUID, AppName, EvtName]),

	    publish_msg(Amqp, UUID, Data),
	    send_ctl_event(CtlPid, UUID, EvtName, AppName),
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
send_ctl_event(_CtlPid, _UUID, _Evt, _Data) ->
    ok.

publish_msg(AmqpHost, UUID, Prop) ->
    EvtName = get_value(<<"Event-Name">>, Prop),
    case lists:member(EvtName, ?FS_EVENTS) of
	true ->
	    EvtProp = [{<<"Msg-ID">>, get_value(<<"Event-Date-Timestamp">>, Prop)}
		       ,{<<"Event-Timestamp">>, get_value(<<"Event-Date-Timestamp">>, Prop)}
		       ,{<<"Call-ID">>, UUID}
		       ,{<<"Channel-Call-State">>, get_value(<<"Channel-Call-State">>, Prop)}
		       | event_specific(EvtName, Prop)
		      ] ++
		whistle_api:default_headers(<<>>, ?EVENT_CAT, EvtName, ?APPNAME, ?APPVER),
	    EvtProp1 = case ecallmgr_util:custom_channel_vars(Prop) of
			   [] -> EvtProp;
			   CustomProp -> [{<<"Custom-Channel-Vars">>, {struct, CustomProp}} | EvtProp]
		       end,

	    case whistle_api:call_event(EvtProp1) of
		{ok, JSON} ->
		    amqp_util:callevt_publish(AmqpHost, UUID, JSON, <<"application/json">>);
		{error, Msg} ->
		    format_log(error, "EVT(~p): Bad event API ~p~n", [self(), Msg])
	    end;
	false ->
	    format_log(info, "EVT(~p): Skipped event ~p~n", [self(), EvtName]),
	    ok
    end.

-spec(event_specific/2 :: (EventName :: binary(), Prop :: proplist()) -> proplist()).
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, Prop) ->
    Application = get_value(<<"Application">>, Prop),
    case get_value(Application, ?SUPPORTED_APPLICATIONS) of
	undefined ->
	    io:format("WHISTLE_API: Didn't find ~p in supported~n", [Application]),
	    [{<<"Application-Name">>, <<"">>}, {<<"Application-Response">>, <<"">>}];
	AppName ->
	    [{<<"Application-Name">>, AppName}
	     ,{<<"Application-Response">>, get_value(<<"Application-Response">>, Prop, <<"">>)}
	    ]
    end;
event_specific(<<"CHANNEL_EXECUTE">>, Prop) ->
    Application = get_value(<<"Application">>, Prop),
    case get_value(Application, ?SUPPORTED_APPLICATIONS) of
	undefined ->
	    io:format("WHISTLE_API: Didn't find ~p in supported~n", [Application]),
	    [{<<"Application-Name">>, <<"">>}, {<<"Application-Response">>, <<"">>}];
	AppName ->
	    [{<<"Application-Name">>, AppName}
	     ,{<<"Application-Response">>, get_value(<<"Application-Response">>, Prop, <<"">>)}
	    ]
    end;
event_specific(_Evt, _Prop) ->
    [].
