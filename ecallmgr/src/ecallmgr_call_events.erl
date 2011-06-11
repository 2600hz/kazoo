%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive call events from freeSWITCH, publish to the call's event
%%% queue
%%% @end
%%% Created : 25 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_events).
-behaviour(gen_server).

-include("ecallmgr.hrl").

-define(EVENT_CAT, <<"call_event">>).
-define(MAX_FAILED_NODE_CHECKS, 5).

%% API
-export([start_link/3, publish_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HANGUP_EVENT_NAME, <<"CHANNEL_HANGUP_COMPLETE">>).

-record(state, {
	  node = undefined :: atom()
          ,uuid = <<>> :: binary()
          ,amqp_q = <<>> :: binary() | tuple(error, term())
          ,ctlpid = undefined :: undefined | pid()
	  ,is_node_up = true :: boolean()
	  ,is_amqp_up = true :: boolean()
	  ,queued_events = [] :: list(proplist()) | []
	  ,failed_node_checks = 0 :: integer()
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec(start_link/3 :: (Node :: atom(), UUID :: binary(), CtlPid :: pid() | undefined) -> tuple(ok, pid())).
start_link(Node, UUID, CtlPid) ->
    gen_server:start_link(?MODULE, [Node, UUID, CtlPid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Node, UUID, CtlPid]) ->
    process_flag(trap_exit, true),
    put(callid, UUID),
    is_pid(CtlPid) andalso link(CtlPid),
    {ok, #state{node=Node, uuid=UUID, ctlpid=CtlPid}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{node=Node, uuid=UUID}=State) ->
    erlang:monitor_node(Node, true),
    case freeswitch:handlecall(Node, UUID) of
	ok ->
	    Q = add_amqp_listener(UUID),
	    {noreply, State#state{amqp_q = Q, is_amqp_up = is_binary(Q)}};
	_ ->
	    {stop, normal, State}
    end;

handle_info({nodedown, Node}, #state{node=Node, is_node_up=true}=State) ->
    ?LOG("Node ~p down", [Node]),
    erlang:monitor_node(Node, false),
    {ok, _} = timer:send_after(0, self(), {is_node_up, 100}),
    {noreply, State#state{is_node_up=false}};

handle_info({is_node_up, Timeout}, #state{node=Node, is_node_up=false, failed_node_checks=FNC}=State) ->
    ?LOG("Node ~p down", [Node]),
    case ecallmgr_fs_handler:is_node_up(Node) of
	true ->
	    ?LOG("Node ~p is back up", [Node]),
	    {noreply, State#state{is_node_up=true, failed_node_checks=0}, 0};
	false ->
	    case Timeout >= ?MAX_TIMEOUT_FOR_NODE_RESTART of
		true ->
		    case FNC > ?MAX_FAILED_NODE_CHECKS of
			true ->
			    ?LOG("Node ~p still not up after ~p checks, giving up", [Node, FNC]),
			    {stop, normal, State};
			false ->
			    ?LOG("Node ~p still not up after ~p checks, trying again", [Node, FNC]),
			    {ok, _} = timer:send_after(?MAX_TIMEOUT_FOR_NODE_RESTART, self(), {is_node_up, ?MAX_TIMEOUT_FOR_NODE_RESTART}),
			    {noreply, State#state{is_node_up=false, failed_node_checks=FNC+1}}
		    end;
		false ->
		    ?LOG("Node ~p still not up, waiting ~p seconds to test again", [Node, Timeout]),
		    {ok, _} = timer:send_after(Timeout, self(), {is_node_up, Timeout*2}),
		    {noreply, State}
	    end
    end;

handle_info({call, {event, [UUID | Data]}}, #state{uuid=UUID, is_amqp_up=IsAmqpUp, queued_events=QEs}=State) ->
    ?LOG("Call Event: ~s", [props:get_value(<<"Event-Name">>, Data)]),

    case IsAmqpUp of
	true ->
	    spawn(fun() -> publish_msg(UUID, Data) end),
	    {noreply, State};
	false ->
	    {noreply, State#state{queued_events = [Data | QEs]}}
    end;

handle_info({call_event, {event, [ UUID | Data ] } }, #state{node=Node, uuid=UUID, ctlpid=CtlPid, is_amqp_up=IsAmqpUp, queued_events=QEs}=State) ->
    EvtName = props:get_value(<<"Event-Name">>, Data),
    AppName = props:get_value(<<"Application">>, Data),

    ?LOG("Call Event for app(~s): ~s", [AppName, EvtName]),

    case EvtName of
	<<"CHANNEL_BRIDGE">> ->
	    case props:get_value(<<"Other-Leg-Unique-ID">>, Data) of
		undefined -> ok;
		OtherUUID ->
		    _Pid = ecallmgr_call_sup:start_event_process(Node, OtherUUID, undefined),
		    ?LOG("Bridge event: new Call-ID: ~s", [OtherUUID]),
		    ?LOG("Bridge event: listener for leg: ~p", [_Pid])
	    end;
	_ -> ok
    end,

    case IsAmqpUp of
	true ->
	    spawn(fun() -> send_ctl_event(CtlPid, UUID, EvtName, AppName), publish_msg(UUID, Data) end),
	    {noreply, State};
	false ->
	    send_ctl_event(CtlPid, UUID, EvtName, AppName),
	    {noreply, State#state{queued_events = [Data | QEs]}}
    end;

handle_info(call_hangup, #state{uuid=UUID, ctlpid=CtlPid, is_amqp_up=false, queued_events=Evts}=State) ->
    ?LOG("Call Hangup received, but AMQP is down, sending queued events separately"),
    spawn(fun() -> send_queued(UUID, Evts) end),
    shutdown(CtlPid, UUID),
    {stop, normal, State};

handle_info(call_hangup, #state{uuid=UUID, ctlpid=CtlPid}=State) ->
    ?LOG("Call Hangup received, everything looks normal, so going down"),
    shutdown(CtlPid, UUID),
    {stop, normal, State};

handle_info({amqp_host_down, H}, State) ->
    ?LOG("Amqp Host ~s went down; queueing events", [H]),
    {ok, _} = timer:send_after(1000, self(), is_amqp_up),
    {noreply, State#state{amqp_q={error, amqp_host_down}, is_amqp_up=false}};

handle_info(is_amqp_up, #state{uuid=UUID, amqp_q={error, _}, queued_events=Evts}=State) ->
    Q1 = add_amqp_listener(UUID),
    case is_binary(Q1) of
	true ->
	    spawn(fun() -> send_queued(UUID, Evts) end),
	    {noreply, State#state{amqp_q = Q1, is_amqp_up = true, queued_events=[]}};
	false ->
	    {ok, _} = timer:send_after(1000, self(), is_amqp_up),
	    {noreply, State}
    end;

handle_info({#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload = Payload}}
	    ,#state{node=Node, uuid=UUID, failed_node_checks=FNC}=State) ->
    JObj = mochijson2:decode(binary_to_list(Payload)),
    ?LOG("Received AMQP: ~s", [Payload]),
    IsUp = is_node_up(Node, UUID),

    spawn(fun() -> handle_amqp_prop(wh_json:get_value(<<"Event-Name">>, JObj), JObj, Node, IsUp) end),

    case IsUp of
	true ->
	    {noreply, State#state{is_node_up=IsUp, failed_node_checks=0}};
	false ->
	    case FNC > ?MAX_FAILED_NODE_CHECKS of
		true ->
		    {stop, normal, State};
		false ->
		    {noreply, State#state{is_node_up=IsUp, failed_node_checks=FNC+1}}
	    end
    end;

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    ?LOG("Unhandled Message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{ctlpid=undefined}) ->
    ?LOG("Terminating: ~p, no ctlpid", [_Reason]);
terminate(_Reason, #state{uuid=UUID, ctlpid=CtlPid}) ->
    ?LOG("Terminating: ~p, and ctl ~p", [_Reason, CtlPid]),
    shutdown(CtlPid, UUID).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(shutdown/2 :: (CtlPid :: pid() | undefined, UUID :: binary()) -> no_return()).
shutdown(undefined, _) -> ok;
shutdown(CtlPid, UUID) ->
    ?LOG(UUID, "Shutdown CTL ~p", [CtlPid]),
    is_pid(CtlPid) andalso CtlPid ! {hangup, self(), UUID}.

%% let the ctl process know a command finished executing
-spec(send_ctl_event/4 :: (CtlPid :: pid() | undefined, UUID :: binary(), Evt :: binary(), AppName :: binary()) -> no_return()).
send_ctl_event(undefined, _, _, _) -> ok;
send_ctl_event(CtlPid, UUID, <<"CHANNEL_EXECUTE_COMPLETE">>, AppName) when is_pid(CtlPid) ->
    erlang:is_process_alive(CtlPid) andalso CtlPid ! {execute_complete, UUID, AppName};
send_ctl_event(_, _, _, _) -> ok.

-spec(publish_msg/2 :: (UUID :: binary(), Prop :: proplist() | []) -> ok).
publish_msg(_, []) -> ok;
publish_msg(UUID, Prop) ->
    EvtName = props:get_value(<<"Event-Name">>, Prop),

    (EvtName =:= ?HANGUP_EVENT_NAME) andalso spawn(fun() -> ecallmgr_call_cdr:new_cdr(UUID, Prop) end),

    case lists:member(EvtName, ?FS_EVENTS) of
	true ->
	    EvtProp0 = [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Prop)}
                        ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, Prop)}
                        ,{<<"Call-ID">>, UUID}
                        ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Prop)}
                        ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Prop)}
		       | event_specific(EvtName, Prop) ],
	    EvtProp1 = EvtProp0 ++ whistle_api:default_headers(<<>>, ?EVENT_CAT, EvtName, ?APP_NAME, ?APP_VERSION),
	    EvtProp2 = case ecallmgr_util:custom_channel_vars(Prop) of
			   [] -> EvtProp1;
			   CustomProp -> [{<<"Custom-Channel-Vars">>, {struct, CustomProp}} | EvtProp1]
		       end,

	    {ok, JSON} = whistle_api:call_event(EvtProp2),
	    amqp_util:callevt_publish(UUID, JSON, event);
	false ->
	    ?LOG("Skipped event ~p", [EvtName])
    end.

%% Setup process to listen for call.status_req api calls and respond in the affirmative
-spec(add_amqp_listener/1 :: (CallID :: binary()) -> binary() | tuple(error, term())).
add_amqp_listener(CallID) ->
    case amqp_util:new_queue(<<>>) of
	{error, _} = E -> E;
	Q ->
	    _ = amqp_util:bind_q_to_callevt(Q, CallID, status_req),
	    _ = amqp_util:basic_consume(Q),
	    Q
    end.

%% return a proplist of k/v pairs specific to the event
-spec(event_specific/2 :: (EventName :: binary(), Prop :: proplist()) -> proplist()).
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, Prop) ->
    Application = props:get_value(<<"Application">>, Prop),
    case props:get_value(Application, ?SUPPORTED_APPLICATIONS) of
	undefined ->
	    io:format("WHISTLE_API: Didn't find ~p in supported", [Application]),
	    [{<<"Application-Name">>, <<"">>}, {<<"Application-Response">>, <<"">>}];
        <<"play_and_collect_digits">> ->
	    [{<<"Application-Name">>, <<"play_and_collect_digits">>} 
	     ,{<<"Application-Response">>, props:get_value(<<"variable_collected_digits">>, Prop, <<"">>)}
	    ];
        <<"bridge">> ->
	    [{<<"Application-Name">>, <<"bridge">>} 
	     ,{<<"Application-Response">>, props:get_value(<<"variable_originate_disposition">>, Prop, <<"">>)}
	    ];
	AppName ->
	    [{<<"Application-Name">>, AppName}
	     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop, <<"">>)}
	    ]
    end;
event_specific(<<"CHANNEL_EXECUTE">>, Prop) ->
    Application = props:get_value(<<"Application">>, Prop),
    case props:get_value(Application, ?SUPPORTED_APPLICATIONS) of
	undefined ->
	    ?LOG("WHISTLE_API: Didn't find ~s in supported apps", [Application]),
	    [{<<"Application-Name">>, <<"">>}, {<<"Application-Response">>, <<"">>}];
	AppName ->
	    [{<<"Application-Name">>, AppName}
	     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop, <<"">>)}
	    ]
    end;
event_specific(<<"CHANNEL_BRIDGE">>, Prop) ->
    [{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Prop, <<>>)}];
event_specific(<<"CHANNEL_UNBRIDGE">>, Prop) ->
    [{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>,props:get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Prop, <<>>)}];
event_specific(<<"CHANNEL_HANGUP">>, Prop) ->
    [{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Prop, <<>>)}
     ,{<<"Hangup-Cause">>, props:get_value(<<"Hangup-Cause">>, Prop, <<>>)}
    ];
event_specific(<<"CHANNEL_HANGUP_COMPLETE">>, Prop) ->
    [{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Prop)}
     ,{<<"Hangup-Cause">>, props:get_value(<<"Hangup-Cause">>, Prop, <<>>)}
    ];
event_specific(<<"RECORD_STOP">>, Prop) ->
    [{<<"Application-Name">>, <<"record">>}
     ,{<<"Application-Response">>, props:get_value(<<"Record-File-Path">>, Prop, <<>>)}
     ,{<<"Terminator">>, props:get_value(<<"variable_playback_terminator_used">>, Prop, <<>>)}
    ];
event_specific(<<"DETECTED_TONE">>, Prop) ->
    [{<<"Detected-Tone">>, props:get_value(<<"Detected-Tone">>, Prop, <<>>)}];
event_specific(<<"DTMF">>, Prop) ->
    [{<<"DTMF-Digit">>, props:get_value(<<"DTMF-Digit">>, Prop, <<>>)}
     ,{<<"DTMF-Duration">>, props:get_value(<<"DTMF-Duration">>, Prop, <<>>)}
    ];
event_specific(_Evt, _Prop) ->
    [].

handle_amqp_prop(<<"status_req">>, JObj, Node, IsNodeUp) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallID),

    try
	true = whistle_api:call_status_req_v(JObj),
	?LOG("Call Status request received"),

	{Status, ErrMsg} = case IsNodeUp of
			       true -> query_call(Node, CallID);
			       false -> {<<"tmpdown">>, {<<"Error-Msg">>, <<"Handling switch is currently not responding">>}}
			   end,

	RespJObj = [{<<"Call-ID">>, CallID}
		    ,{<<"Status">>, Status}
		    | whistle_api:default_headers(<<>>, <<"call_event">>, <<"status_resp">>, ?APP_NAME, ?APP_VERSION) ],
	{ok, JSON} = whistle_api:call_status_resp([ ErrMsg | RespJObj ]),
	SrvID = wh_json:get_value(<<"Server-ID">>, JObj),
	?LOG("Status response: ~s", [JSON]),

	amqp_util:targeted_publish(SrvID, JSON)
    catch
	E:R ->
	    ?LOG("Call Status exception: ~s:~w", [E, R]),
	    ?LOG("Stackstrace: ~w", [erlang:get_stacktrace()])
    end.

query_call(Node, CallID) ->
    case freeswitch:api(Node, uuid_exists, whistle_util:to_list(CallID)) of
	{ok, Result} ->
	    case whistle_util:is_true(Result) of
		true -> {<<"active">>, {ignore, me}};
		false -> {<<"down">>, {<<"Error-Msg">>, <<"Call is no longer active">>}}
	    end;
	_ ->
	    {<<"tmpdown">>, {<<"Error-Msg">>, <<"Switch did not respond to query">>}}
    end.

%% if the call went down but we had queued events to send, try for up to 10 seconds to send them

-spec(send_queued/2 :: (UUID :: binary(), Evts :: list()) -> no_return()).
send_queued(UUID, Evts) ->
    send_queued(UUID, lists:reverse(Evts), 0).

-spec(send_queued/3 :: (UUID :: binary(), Evts :: list(), Tries :: integer()) -> no_return()).
send_queued(_UUID, _, 10=Tries) ->
    ?LOG(_UUID, "Failed to send queued events after ~b times, going down", [Tries]);
send_queued(_UUID, [], _) ->
    ?LOG(_UUID, "No queued events to send", []);
send_queued(UUID, [_|_]=Evts, Tries) ->
    case amqp_util:is_host_available() of
	false ->
	    receive after 1000 -> send_queued(UUID, Evts, Tries+1) end;
	true ->
	    ?LOG(UUID, "Sending queued events on try ~b", [Tries]),
	    [ publish_msg(UUID, E) || E <- Evts ]
    end.

-spec(is_node_up/2 :: (Node :: atom(), UUID :: binary()) -> boolean()).
is_node_up(Node, UUID) ->
    case ecallmgr_fs_handler:is_node_up(Node) andalso freeswitch:api(Node, uuid_exists, whistle_util:to_list(UUID)) of
	{ok, IsUp} -> whistle_util:to_boolean(IsUp);
	_ -> false
    end.
