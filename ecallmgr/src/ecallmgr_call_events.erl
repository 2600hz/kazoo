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

-export([start_link/4, init/4]).

-include("whistle_api.hrl").
-include("ecallmgr.hrl").

-import(logger, [log/2, format_log/3]).
-import(props, [get_value/2, get_value/3]).

-define(APPNAME, <<"ecallmgr.call.event">>).
-define(APPVER, <<"0.4.1">>).
-define(EVENT_CAT, <<"call_event">>).

%% Node, UUID, Host, CtlPid
-spec(start_link/4 :: (Node :: atom(), UUID :: binary(), Aqmp :: string(), CtlPid :: pid() | undefined) -> tuple(ok, pid())).
start_link(Node, UUID, Host, CtlPid) ->
    {ok, spawn_link(ecallmgr_call_events, init, [Node, UUID, Host, CtlPid])}.

init(Node, UUID, Host, CtlPid) ->
    freeswitch:handlecall(Node, UUID),
    erlang:monitor_node(Node, true),

    case CtlPid of
	undefined -> ok;
	_ -> link(CtlPid) %% CtlPid goes down if we go down
    end,

    add_amqp_listener(Host, UUID),
    loop(Node, UUID, Host, CtlPid, infinity).

-spec(loop/5 :: (Node :: atom(), UUID :: binary(), Host :: string(), CtlPid :: pid() | undefined, Timeout :: infinity | integer()) -> no_return()).
loop(Node, UUID, Host, CtlPid, Timeout) ->
    receive
	{call, {event, [UUID | Data]}} ->
	    format_log(info, "EVT(~p): {Call, {Event}} for ~p: ~p~n", [self(), UUID, get_value(<<"Event-Name">>, Data)]),
	    publish_msg(Host, UUID, Data),
	    loop(Node, UUID, Host, CtlPid, Timeout);
	{call_event, {event, [ UUID | Data ] } } ->
	    EvtName = get_value(<<"Event-Name">>, Data),
	    AppName = get_value(<<"Application">>, Data),
	    format_log(info, "EVT(~p): {Call_Event, {Event}} for ~p(~p): ~p~n"
		       ,[self(), UUID, AppName, EvtName]),

	    Timeout1 = case EvtName of
			   <<"CHANNEL_HANGUP_COMPLETE">> ->
			       spawn(fun() -> ecallmgr_call_cdr:new_cdr(UUID, Host, Data) end),
			       5000;
			   <<"CHANNEL_BRIDGE">> ->
			       case get_value(<<"Other-Leg-Unique-ID">>, Data) of
				   undefined -> ok;
				   OtherUUID ->
				       _Pid = ecallmgr_call_sup:start_event_process(Node, OtherUUID, Host, undefined),
				       format_log(info, "EVT(~p): New Evt Listener for ~p: ~p~n", [self(), OtherUUID, _Pid])
			       end,
			       Timeout;
			   _ -> Timeout
		       end,

	    spawn(fun() -> publish_msg(Host, UUID, Data) end),
	    send_ctl_event(CtlPid, UUID, EvtName, AppName),
	    loop(Node, UUID, Host, CtlPid, Timeout1);
	call_hangup ->
	    shutdown(CtlPid, UUID);
	{nodedown, Node} ->
	    shutdown(CtlPid, UUID);
	{amqp_host_down, H} ->
	    format_log(info, "EVT(~p): AmqpHost ~s went down, so we are too~n", [self(), H]),
	    self() ! call_hangup,
	    loop(Node, UUID, Host, CtlPid, Timeout);
	{#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">> }
				       ,payload = Payload}} ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    format_log(info, "EVT(~p): AMQP Msg ~p~n", [self(), Prop]),
	    spawn(fun() -> handle_amqp_prop(get_value(<<"Event-Name">>, Prop), Prop, Host) end),
	    loop(Node, UUID, Host, CtlPid, Timeout);
	_Msg ->
	    format_log(error, "EVT(~p): Unhandled FS Msg: ~n~p~n", [self(), _Msg]),
	    loop(Node, UUID, Host, CtlPid, Timeout)
    after
	Timeout -> shutdown(CtlPid, UUID)
    end.

shutdown(CtlPid, UUID) ->
    case CtlPid of
	undefined -> ok;
	_ -> CtlPid ! {hangup, self(), UUID}
    end,

    receive {ctl_down, CtlPid} -> ok
    after 500 -> ok end,

    format_log(info, "EVT(~p): Call Hangup for ~p, going down now~n", [self(), UUID]).

%% let the ctl process know a command finished executing
-spec(send_ctl_event/4 :: (CtlPid :: pid() | undefined, UUID :: binary(), Evt :: binary(), AppName :: binary()) -> no_return()).
send_ctl_event(undefined, _, _, _) -> ok;
send_ctl_event(CtlPid, UUID, <<"CHANNEL_EXECUTE_COMPLETE">>, AppName) when is_pid(CtlPid) ->
    case erlang:is_process_alive(CtlPid) of
	true ->
	    format_log(info, "EVT.send_ctl(~p): Pid: ~p UUID: ~p ExecComplete App: ~p~n", [self(), CtlPid, UUID, AppName]),
	    CtlPid ! {execute_complete, UUID, AppName};
	false ->
	    format_log(info, "EVT.send_ctl(~p): Pid: ~p(dead) UUID: ~p ExecComplete App: ~p~n", [self(), CtlPid, UUID, AppName])
    end;
send_ctl_event(_, _, _, _) -> ok.

-spec(publish_msg/3 :: (Host :: string(), UUID :: binary(), Prop :: proplist()) -> no_return()).
publish_msg(Host, UUID, Prop) ->
    EvtName = get_value(<<"Event-Name">>, Prop),

    case lists:member(EvtName, ?FS_EVENTS) of
	true ->
	    EvtProp0 = [{<<"Msg-ID">>, get_value(<<"Event-Date-Timestamp">>, Prop)}
		       ,{<<"Timestamp">>, get_value(<<"Event-Date-Timestamp">>, Prop)}
		       ,{<<"Call-ID">>, UUID}
		       ,{<<"Call-Direction">>, get_value(<<"Call-Direction">>, Prop)}
		       ,{<<"Channel-Call-State">>, get_value(<<"Channel-Call-State">>, Prop)}
		       | event_specific(EvtName, Prop) ],
	    EvtProp1 = EvtProp0 ++ whistle_api:default_headers(<<>>, ?EVENT_CAT, EvtName, ?APPNAME, ?APPVER),
	    EvtProp2 = case ecallmgr_util:custom_channel_vars(Prop) of
			   [] -> EvtProp1;
			   CustomProp -> [{<<"Custom-Channel-Vars">>, {struct, CustomProp}} | EvtProp1]
		       end,

	    case whistle_api:call_event(EvtProp2) of
		{ok, JSON} ->
		    amqp_util:callevt_publish(Host, UUID, JSON, event);
		{error, Msg} ->
		    format_log(error, "EVT(~p): Bad event API ~p~n", [self(), Msg])
	    end;
	false ->
	    format_log(info, "EVT(~p): Skipped event ~p~n", [self(), EvtName]),
	    ok
    end.

%% Setup process to listen for call.status_req api calls and respond in the affirmative
add_amqp_listener(Host, CallID) ->
    Q = amqp_util:new_queue(Host, <<>>),
    amqp_util:bind_q_to_callevt(Host, Q, CallID, status_req),
    amqp_util:basic_consume(Host, Q).

%% return a proplist of k/v pairs specific to the event
-spec(event_specific/2 :: (EventName :: binary(), Prop :: proplist()) -> proplist()).
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, Prop) ->
    Application = get_value(<<"Application">>, Prop),
    case get_value(Application, ?SUPPORTED_APPLICATIONS) of
	undefined ->
	    io:format("WHISTLE_API: Didn't find ~p in supported~n", [Application]),
	    [{<<"Application-Name">>, <<"">>}, {<<"Application-Response">>, <<"">>}];
        <<"play_and_collect_digits">> ->
	    [{<<"Application-Name">>, <<"play_and_collect_digits">>} 
	     ,{<<"Application-Response">>, get_value(<<"variable_collected_digits">>, Prop, <<"">>)}
	    ];
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
event_specific(<<"CHANNEL_BRIDGE">>, Prop) ->
    [{<<"Other-Leg-Direction">>, get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>,get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, get_value(<<"Other-Leg-Unique-ID">>, Prop, <<>>)}];
event_specific(<<"CHANNEL_UNBRIDGE">>, Prop) ->
    [{<<"Other-Leg-Direction">>, get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>,get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, get_value(<<"Other-Leg-Unique-ID">>, Prop, <<>>)}];
event_specific(<<"CHANNEL_HANGUP">>, Prop) ->
    [{<<"Other-Leg-Direction">>, get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>,get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, get_value(<<"Other-Leg-Unique-ID">>, Prop, <<>>)}
     ,{<<"Hangup-Cause">>, get_value(<<"Hangup-Cause">>, Prop, <<>>)}
    ];
event_specific(<<"CHANNEL_HANGUP_COMPLETE">>, Prop) ->
    [{<<"Other-Leg-Direction">>, get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>,get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, get_value(<<"Other-Leg-Unique-ID">>, Prop)}
     ,{<<"Hangup-Cause">>, get_value(<<"Hangup-Cause">>, Prop, <<>>)}
    ];
event_specific(<<"RECORD_STOP">>, Prop) ->
    [{<<"Application-Name">>, <<"record">>}
     ,{<<"Application-Response">>, get_value(<<"Record-File-Path">>, Prop, <<>>)}
     ,{<<"Terminator">>, get_value(<<"variable_playback_terminator_used">>, Prop, <<>>)}
    ];
event_specific(<<"DETECTED_TONE">>, Prop) ->
    [{<<"Detected-Tone">>, get_value(<<"Detected-Tone">>, Prop, <<>>)}];
event_specific(<<"DTMF">>, Prop) ->
    [{<<"DTMF-Digit">>, get_value(<<"DTMF-Digit">>, Prop, <<>>)}
     ,{<<"DTMF-Duration">>, get_value(<<"DTMF-Duration">>, Prop, <<>>)}
    ];
event_specific(_Evt, _Prop) ->
    [].

handle_amqp_prop(<<"status_req">>, Prop, AmqpHost) ->
    try
    true = whistle_api:call_status_req_v(Prop),
    CallID = get_value(<<"Call-ID">>, Prop),
    format_log(info, "EVT.call_status for ~p is up, responding~n", [CallID]),
    RespProp = [{<<"Call-ID">>, CallID}
		,{<<"Status">>, <<"active">>}
		| whistle_api:default_headers(<<>>, <<"call_event">>, <<"status_resp">>, <<?APPNAME/binary, ".status">>, ?APPVER) ],
    {ok, JSON} = whistle_api:call_status_resp(RespProp),
    SrvID = get_value(<<"Server-ID">>, Prop),
    format_log(info, "EVT.vall_status(~p): ~s", [CallID, JSON]),

    amqp_util:targeted_publish(AmqpHost, SrvID, JSON)
    catch
	E:R ->
	    format_log(error, "EVT.call_status err ~p: ~p", [E, R])
    end.
