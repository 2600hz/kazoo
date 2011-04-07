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
-behaviour(gen_server).

-include("ecallmgr.hrl").

-import(logger, [log/2, format_log/3]).

-define(EVENT_CAT, <<"call_event">>).
-define(MAX_FAILED_NODE_CHECKS, 5).

%% API
-export([start_link/3, publish_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

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
    case CtlPid of
	undefined -> ok;
	_ -> link(CtlPid) %% CtlPid goes down if we go down
    end,

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
    Reply = ok,
    {reply, Reply, State}.

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
    format_log(error, "EVT(~p): nodedown ~p~n", [self(), Node]),
    erlang:monitor_node(Node, false),
    {ok, _} = timer:send_after(0, self(), {is_node_up, 100}),
    {noreply, State#state{is_node_up=false}};

handle_info({is_node_up, Timeout}, #state{node=Node, is_node_up=false}=State) ->
    format_log(error, "EVT(~p): nodedown ~p, trying ping, then waiting ~p if it fails~n", [self(), Node, Timeout]),
    case ecallmgr_fs_handler:is_node_up(Node) of
	true ->
	    {noreply, State#state{is_node_up=true, failed_node_checks=0}, 0};
	false ->
	    case Timeout >= ?MAX_TIMEOUT_FOR_NODE_RESTART of
		true ->
		    case State#state.failed_node_checks > ?MAX_FAILED_NODE_CHECKS of
			true ->
			    {stop, normal, State};
			false ->
			    timer:send_after(?MAX_TIMEOUT_FOR_NODE_RESTART, self(), {is_node_up, ?MAX_TIMEOUT_FOR_NODE_RESTART}),
			    {noreply, State#state{is_node_up=false, failed_node_checks=State#state.failed_node_checks+1}}
		    end;
		false ->
		    timer:send_after(Timeout, self(), {is_node_up, Timeout*2}),
		    {noreply, State}
	    end
    end;

handle_info({call, {event, [UUID | Data]}}, #state{uuid=UUID}=State) ->
    format_log(info, "EVT(~p): {Call, {Event}} for ~p: ~p~n", [self(), UUID, props:get_value(<<"Event-Name">>, Data)]),

    case State#state.is_amqp_up of
	true ->
	    spawn(fun() -> publish_msg(UUID, Data) end),
	    {noreply, State};
	false ->
	    {noreply, State#state{queued_events = [Data | State#state.queued_events]}}
    end;

handle_info({call_event, {event, [ UUID | Data ] } }, #state{node=Node, uuid=UUID, ctlpid=CtlPid}=State) ->
    EvtName = props:get_value(<<"Event-Name">>, Data),
    AppName = props:get_value(<<"Application">>, Data),
    format_log(info, "EVT(~p): {Call_Event, {Event}} for ~p(~p): ~p. Sendable: ~p~n"
	       ,[self(), UUID, AppName, EvtName, State#state.is_amqp_up]),

    case EvtName of
	<<"CHANNEL_BRIDGE">> ->
	    case props:get_value(<<"Other-Leg-Unique-ID">>, Data) of
		undefined -> ok;
		OtherUUID ->
		    _Pid = ecallmgr_call_sup:start_event_process(Node, OtherUUID, undefined),
		    format_log(info, "EVT(~p): New Evt Listener for ~p: ~p~n", [self(), OtherUUID, _Pid])
	    end;
	_ -> ok
    end,

    case State#state.is_amqp_up of
	true ->
	    spawn(fun() -> send_ctl_event(CtlPid, UUID, EvtName, AppName), publish_msg(UUID, Data) end),
	    {noreply, State};
	false ->
	    send_ctl_event(CtlPid, UUID, EvtName, AppName),
	    {noreply, State#state{queued_events = [Data | State#state.queued_events]}}
    end;

handle_info(call_hangup, #state{is_node_up=false}=State) ->
    format_log(info, "EVT(~p): call_hangup received, is_node_up is false; let's wait and see if FS restarts in time (tried ~s times already)~n", [self(), State#state.failed_node_checks]),
    {noreply, State};

handle_info(call_hangup, #state{uuid=UUID, ctlpid=CtlPid, is_amqp_up=false, queued_events=Evts}=State) ->
    format_log(info, "EVT(~p): call_hangup received, is_amqp_up is false; sending queued if amqp comes back up.~n", [self()]),
    spawn(fun() -> send_queued(UUID, Evts, 0) end),
    shutdown(CtlPid, UUID),
    {stop, normal, State};

handle_info(call_hangup, #state{uuid=UUID, ctlpid=CtlPid}=State) ->
    format_log(info, "EVT(~p): call_hangup received, everything looks normal, so going down~n", [self()]),
    shutdown(CtlPid, UUID),
    {stop, normal, State};

handle_info({amqp_host_down, H}, State) ->
    format_log(info, "EVT(~p): AmqpHost ~s went down; queueing events~n", [self(), H]),
    {ok, _} = timer:send_after(1000, self(), is_amqp_up),
    {noreply, State#state{amqp_q={error, amqp_host_down}, is_amqp_up=false}};

handle_info(is_amqp_up, #state{uuid=UUID, amqp_q={error, _}}=State) ->
    Q1 = add_amqp_listener(UUID),
    case is_binary(Q1) of
	true ->
	    {noreply, State#state{amqp_q = Q1, is_amqp_up = true}};
	false ->
	    {ok, _} = timer:send_after(1000, self(), is_amqp_up),
	    {noreply, State}
    end;

handle_info({#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload = Payload}}, State) ->
    JObj = mochijson2:decode(binary_to_list(Payload)),
    format_log(info, "EVT(~p): AMQP Msg ~s~n", [self(), Payload]),
    IsUp = is_node_up(State#state.node, State#state.uuid),
    spawn(fun() -> handle_amqp_prop(whapps_json:get_value(<<"Event-Name">>, JObj), JObj, IsUp) end),

    case IsUp of
	true ->
	    {noreply, State#state{is_node_up=IsUp, failed_node_checks=0}};
	false ->
	    case State#state.failed_node_checks > ?MAX_FAILED_NODE_CHECKS of
		true ->
		    {stop, normal, State};
		false ->
		    {noreply, State#state{is_node_up=IsUp, failed_node_checks=State#state.failed_node_checks+1}}
	    end
    end;

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    format_log(info, "EVT(~p): unhandled info: ~p~n", [self(), _Info]),
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
terminate(_Reason, #state{uuid=UUID, ctlpid=undefined}) ->
    format_log(info, "EVT(~p): Terminating ~p (~p), no ctlpid~n", [self(), UUID, _Reason]),
    ok;
terminate(_Reason, #state{uuid=UUID, ctlpid=CtlPid}) ->
    format_log(info, "EVT(~p): Terminating ~p (~p)~n", [self(), UUID, _Reason]),
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
shutdown(CtlPid, UUID) ->
    case CtlPid of
	undefined ->
	    ok;
	_ ->
	    CtlPid ! {hangup, self(), UUID},
	    ok
    end,
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

-spec(publish_msg/2 :: (UUID :: binary(), Prop :: proplist()) -> no_return()).
publish_msg(UUID, Prop) ->
    EvtName = props:get_value(<<"Event-Name">>, Prop),

    send_cdr(UUID, EvtName, Prop),

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
	    format_log(info, "EVT(~p): Skipped event ~p~n", [self(), EvtName])
    end.

%% Setup process to listen for call.status_req api calls and respond in the affirmative
-spec(add_amqp_listener/1 :: (CallID :: binary()) -> binary() | tuple(error, term())).
add_amqp_listener(CallID) ->
    case amqp_util:new_queue(<<>>) of
	{error, _} = E -> E;
	Q ->
	    amqp_util:bind_q_to_callevt(Q, CallID, status_req),
	    amqp_util:basic_consume(Q),
	    Q
    end.

%% return a proplist of k/v pairs specific to the event
-spec(event_specific/2 :: (EventName :: binary(), Prop :: proplist()) -> proplist()).
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, Prop) ->
    Application = props:get_value(<<"Application">>, Prop),
    case props:get_value(Application, ?SUPPORTED_APPLICATIONS) of
	undefined ->
	    io:format("WHISTLE_API: Didn't find ~p in supported~n", [Application]),
	    [{<<"Application-Name">>, <<"">>}, {<<"Application-Response">>, <<"">>}];
        <<"play_and_collect_digits">> ->
	    [{<<"Application-Name">>, <<"play_and_collect_digits">>} 
	     ,{<<"Application-Response">>, props:get_value(<<"variable_collected_digits">>, Prop, <<"">>)}
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
	    io:format("WHISTLE_API: Didn't find ~p in supported~n", [Application]),
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

handle_amqp_prop(<<"status_req">>, JObj, IsNodeUp) ->
    try
	true = whistle_api:call_status_req_v(JObj),
	CallID = whapps_json:get_value(<<"Call-ID">>, JObj),
	format_log(info, "EVT.call_status for ~p is up, responding~n", [CallID]),

	{Status, ErrMsg} = case IsNodeUp of
			       true -> {<<"active">>, {ignore, me}};
			       false -> {<<"tmpdown">>, {<<"Error-Msg">>, <<"Handling switch is currently not responding">>}}
			   end,

	RespJObj = [{<<"Call-ID">>, CallID}
		    ,{<<"Status">>, Status}
		    | whistle_api:default_headers(<<>>, <<"call_event">>, <<"status_resp">>, ?APP_NAME, ?APP_VERSION) ],
	{ok, JSON} = whistle_api:call_status_resp([ ErrMsg | RespJObj ]),
	SrvID = whapps_json:get_value(<<"Server-ID">>, JObj),
	format_log(info, "EVT.call_status(~p): ~s", [CallID, JSON]),

	amqp_util:targeted_publish(SrvID, JSON)
    catch
	E:R ->
	    format_log(error, "EVT.call_status err ~p: ~p", [E, R])
    end.

-spec(send_cdr/3 :: (UUID :: binary(), EvtName :: binary(), Data :: proplist()) -> no_return()).
send_cdr(UUID, <<"CHANNEL_HANGUP_COMPLETE">>, Data) ->
    spawn(fun() -> ecallmgr_call_cdr:new_cdr(UUID, Data) end);
send_cdr( _, _, _) -> ok.

%% if the call went down but we had queued events to send, try for up to 10 seconds to send them
-spec(send_queued/3 :: (UUID :: binary(), Evts :: list(proplist()) | [], Tries :: integer()) -> no_return()).
send_queued(_, _, 10) ->
    format_log(info, "EVT.send_queued(~p): Failed after 10 times, going down~n", [self()]);
send_queued(_, [], _) ->
    format_log(info, "EVT.send_queued(~p): No queued events.~n", [self()]);
send_queued(UUID, Evts, Tries) ->
    case amqp_util:is_host_available() of
	{error, _} ->
	    ok = timer:sleep(1000),
	    send_queued(UUID, Evts, Tries+1);
	Q ->
	    format_log(info, "EVT.send_queued(~p): Sending queued events on try ~p~n", [self(), Tries]),
	    amqp_util:queue_delete(Q),
	    lists:foreach(fun(E) -> publish_msg(UUID, E) end, lists:reverse(Evts)),
	    ok
    end.

-spec(is_node_up/2 :: (Node :: atom(), UUID :: binary()) -> boolean()).
is_node_up(Node, UUID) ->
    case freeswitch:api(Node, uuid_exists, whistle_util:to_list(UUID)) of
	{ok, IsUp} -> whistle_util:to_boolean(IsUp);
	_ -> false
    end.
