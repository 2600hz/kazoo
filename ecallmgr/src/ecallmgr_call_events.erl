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
-define(MAX_WAIT_FOR_EVENT, 10000). %% how long to wait for an event from FS before checking status of call

%% API
-export([start_link/3, publish_msg/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HANGUP_EVENT_NAME, <<"CHANNEL_HANGUP_COMPLETE">>).
-record(state, {
	  node = undefined :: atom()
          ,uuid = <<>> :: binary()
          ,amqp_q = <<>> :: binary() | {'error', term()}
          ,ctlpid = undefined :: 'undefined' | pid()
	  ,is_node_up = true :: boolean()
	  ,is_amqp_up = true :: boolean()
	  ,queued_events = [] :: [proplist(),...] | []
	  ,failed_node_checks = 0 :: integer()
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {'ok', Pid} | ignore | {'error', Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/3 :: (Node, UUID, CtlPid) -> {'ok', pid()} when
      Node :: atom(),
      UUID :: binary(),
      CtlPid :: pid() | 'undefined'.
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
%% @spec init(Args) -> {'ok', State} |
%%                     {'ok', State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init/1 :: ([atom() | binary() | pid(),...]) -> {'ok', #state{}}.
init([Node, UUID, CtlPid]) ->
    process_flag(trap_exit, true),
    put(callid, UUID),
    self() ! startup,
    ?LOG_START("starting new call events listener"),
    is_pid(CtlPid) andalso link(CtlPid),
    ?LOG("linked to control listener ~p",[CtlPid]),
    {'ok', #state{node=Node, uuid=UUID, ctlpid=CtlPid}}.

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
    {reply, 'ok', State}.

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
-spec handle_info/2 :: ('startup', #state{}) -> handle_info_ret();
		       ({'nodedown', atom()}, #state{}) -> {'noreply', #state{}};
                       ({'is_node_up', non_neg_integer()}, #state{}) -> handle_info_ret();
		       ({'call', {'event', [binary() | proplist()]}}, #state{}) -> {'noreply', #state{}, 'hibernate'};
		       ({'call_event', {'event', [binary() | proplist()]}}, #state{}) -> {'noreply', #state{}, 'hibernate'};
		       ('call_hangup', #state{}) -> {'stop', 'normal', #state{}};
		       ({'amqp_host_down', binary()}, #state{}) -> {'noreply', #state{}};
		       ('is_amqp_up', #state{}) -> {'noreply', #state{}};
		       ({#'basic.deliver'{}, #amqp_msg{}}, #state{}) -> {'noreply', #state{}, 'hibernate'} | {'stop', 'normal', #state{}};
		       (#'basic.consume_ok'{}, #state{}) -> {'noreply', #state{}, 'hibernate'}.
handle_info({call, {event, [UUID | Data]}}, #state{uuid=UUID, is_amqp_up=true, node=Node}=State) ->
    spawn(fun() -> put(callid, UUID), publish_msg(Node, UUID, Data) end),
    {'noreply', State};

handle_info({call, {event, [UUID | Data]}}, #state{uuid=UUID, is_amqp_up=false, queued_events=QEs}=State) ->
    {'noreply', State#state{queued_events = [Data | QEs]}, hibernate};

handle_info({call_event, {event, [ UUID | Data ] } }, #state{node=Node, uuid=UUID, ctlpid=CtlPid, is_amqp_up=IsAmqpUp, queued_events=QEs}=State) ->
    EvtName = props:get_value(<<"Event-Name">>, Data),
    AppName = props:get_value(<<"Application">>, Data),

    case EvtName of
	<<"CHANNEL_BRIDGE">> ->
	    case props:get_value(<<"Other-Leg-Unique-ID">>, Data) of
		undefined -> 'ok';
		OtherUUID ->
		    ?LOG("event was a bridged to ~s", [OtherUUID]),
		    _Pid = ecallmgr_call_sup:start_event_process(Node, OtherUUID, undefined),
		    ?LOG("started event listener for other leg as ~p", [_Pid])
	    end;
	_ -> 'ok'
    end,

    case IsAmqpUp of
	true ->
	    spawn(fun() -> put(callid, UUID), send_ctl_event(CtlPid, UUID, EvtName, Data), publish_msg(Node, UUID, Data) end),
	    {'noreply', State};
	false ->
	    send_ctl_event(CtlPid, UUID, EvtName, AppName),
	    {'noreply', State#state{queued_events = [Data | QEs]}, hibernate}
    end;

handle_info(call_hangup, #state{uuid=UUID, ctlpid=CtlPid, is_amqp_up=false, queued_events=Evts, node=Node}=State) ->
    ?LOG("call hangup received, but AMQP is down, sending queued events separately"),
    spawn(fun() -> put(callid, UUID), send_queued(Node, UUID, Evts) end),
    shutdown(CtlPid, UUID),
    {'stop', 'normal', State};

handle_info(call_hangup, #state{uuid=UUID, ctlpid=CtlPid}=State) ->
    ?LOG("normal call hangup received, going down"),
    shutdown(CtlPid, UUID),
    {'stop', 'normal', State};

handle_info({nodedown, Node}, #state{node=Node, is_node_up=true}=State) ->
    ?LOG_SYS("lost connection to node ~s, waiting for reconnection", [Node]),
    erlang:monitor_node(Node, false),
    _Ref = erlang:send_after(0, self(), {is_node_up, 100}),
    {'noreply', State#state{is_node_up=false}, hibernate};

handle_info({is_node_up, Timeout}, #state{node=Node, uuid=UUID, is_node_up=false, failed_node_checks=FNC}=State) ->
    case ecallmgr_util:is_node_up(Node, UUID) of
	true ->
            ?LOG("reconnected to node ~s and call is active", [Node]),
	    {'noreply', State#state{is_node_up=true, failed_node_checks=0}, hibernate};
	false ->
	    case Timeout >= ?MAX_TIMEOUT_FOR_NODE_RESTART of
		true ->
		    case FNC > ?MAX_FAILED_NODE_CHECKS of
			true ->
			    ?LOG("node ~p still not up after ~p checks, giving up", [Node, FNC]),
			    self() ! call_hangup,
			    {'noreply', State};
			false ->
			    ?LOG("node ~p still not up after ~p checks, trying again", [Node, FNC]),
			    _Ref = erlang:send_after(?MAX_TIMEOUT_FOR_NODE_RESTART, self(), {is_node_up, ?MAX_TIMEOUT_FOR_NODE_RESTART}),
			    {'noreply', State#state{is_node_up=false, failed_node_checks=FNC+1}, hibernate}
		    end;
		false ->
		    ?LOG("node ~s still not up, waiting ~p seconds to test again", [Node, Timeout]),
		    _Ref = erlang:send_after(Timeout, self(), {is_node_up, Timeout*2}),
		    {'noreply', State}
	    end
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    _Ref = erlang:send_after(1000, self(), is_amqp_up),
    {'noreply', State#state{amqp_q={'error', amqp_host_down}, is_amqp_up=false}, hibernate};

handle_info(is_amqp_up, #state{uuid=UUID, amqp_q={'error', _}, queued_events=Evts, node=Node}=State) ->
    Q1 = add_amqp_listener(UUID),
    case is_binary(Q1) of
	true ->
	    spawn(fun() -> put(callid, UUID), send_queued(Node, UUID, Evts) end),
	    {'noreply', State#state{amqp_q = Q1, is_amqp_up = true, queued_events=[]}, hibernate};
	false ->
	    _Ref = erlang:send_after(1000, self(), is_amqp_up),
	    {'noreply', State}
    end;

handle_info({#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload = Payload}}
	    ,#state{node=Node, uuid=UUID, failed_node_checks=FNC}=State) ->
    JObj = mochijson2:decode(Payload),
    IsUp = ecallmgr_util:is_node_up(Node, UUID),

    spawn(fun() -> put(callid, UUID), handle_amqp_prop(wh_json:get_value(<<"Event-Name">>, JObj), JObj, Node, IsUp) end),

    case IsUp of
	true ->
	    {'noreply', State#state{is_node_up=IsUp, failed_node_checks=0}, hibernate};
	false ->
	    case FNC > ?MAX_FAILED_NODE_CHECKS of
		true ->
		    ?LOG(UUID, "node ~s appears down, and we've checked ~b times now; going down", [Node, FNC]),
		    {'stop', 'normal', State};
		false ->
		    ?LOG(UUID, "node ~s appears down, and we've checked ~b times now; will check again", [Node, FNC]),
		    _Ref = erlang:send_after(?MAX_TIMEOUT_FOR_NODE_RESTART, self(), {is_node_up, ?MAX_TIMEOUT_FOR_NODE_RESTART}),
		    {'noreply', State#state{is_node_up=IsUp, failed_node_checks=FNC+1}, hibernate}
	    end
    end;

handle_info(startup, #state{node=Node, uuid=UUID}=State) ->
    erlang:monitor_node(Node, true),
    ?LOG("starting handler on ~s for ~s", [Node, UUID]),
    case freeswitch:handlecall(Node, UUID) of
        ok ->
            Q = add_amqp_listener(UUID),
            ?LOG("call event handler setup complete"),
            {'noreply', State#state{amqp_q = Q, is_amqp_up = is_binary(Q)}, hibernate};
        timeout ->
            ?LOG("timed out trying to handle events for ~s, trying again", [Node]),
            self() ! startup,
            {'noreply', State};
        {'error', badsession} ->
            ?LOG("bad session received when handling events for ~s", [Node]),
            {'stop', 'normal', State};
        _E ->
            ?LOG("failed to handle call events for ~s: ~p", [Node, _E]),
            {'stop', 'normal', State}
    end;

handle_info(#'basic.consume_ok'{}, State) ->
    {'noreply', State};

handle_info(_Info, State) ->
    {'noreply', State}.

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
terminate(_Reason, #state{uuid=UUID, ctlpid=CtlPid}) ->
    ?LOG("call events ~p termination", [_Reason]),
    shutdown(CtlPid, UUID).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {'ok', NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec shutdown/2 :: (CtlPid, UUID) -> 'ok' when
      CtlPid :: pid() | undefined,
      UUID :: binary().
shutdown(undefined, _) -> 'ok';
shutdown(CtlPid, UUID) ->
    ?LOG("sending hangup for call ~s", [UUID]),
    is_pid(CtlPid) andalso CtlPid ! {hangup, self(), UUID},
    'ok'.

%% let the ctl process know a command finished executing
-spec send_ctl_event/4 :: (CtlPid, UUID, EventName, Event) -> 'ok' when
      CtlPid :: pid() | undefined,
      UUID :: binary(),
      EventName :: binary(),
      Event :: proplist().
send_ctl_event(undefined, _, _, _) -> 'ok';
send_ctl_event(CtlPid, UUID, <<"CHANNEL_EXECUTE_COMPLETE">>, Props) when is_pid(CtlPid) ->
    AppName = props:get_value(<<"Application">>, Props),

    ?LOG("sending execution completion to control queue"),
    erlang:is_process_alive(CtlPid) andalso CtlPid ! {execute_complete, UUID, AppName},
    'ok';
send_ctl_event(CtlPid, UUID, <<"CUSTOM">>, Props) when is_pid(CtlPid) ->
    case props:get_value(<<"Event-Subclass">>, Props) of
	<<"whistle::noop">> ->
	    ?LOG("sending noop completion to control queue"),
	    erlang:is_process_alive(CtlPid) andalso CtlPid ! {execute_complete, UUID, <<"noop">>},
	    'ok';
	_ ->
	    'ok'
    end;
send_ctl_event(_, _, _, _) -> 'ok'.

-spec publish_msg/3 :: (Node, UUID, Prop) -> 'ok' when
      Node :: atom(),
      UUID :: binary(),
      Prop :: proplist().
publish_msg(_, _, []) -> 'ok';
publish_msg(Node, UUID, Prop) when is_list(Prop) ->
    FSEvtName = props:get_value(<<"Event-Name">>, Prop),
    FSAppName = props:get_value(<<"Application">>, Prop),

    %% whistle generates custom events that should masquerade as standard events
    %% for simplicity in the high level code (like bridge which "completes" during
    %% transfers even though the bridge still continues).
    EvtName = case FSEvtName of
                  <<"CUSTOM">> ->
                      props:get_value(<<"whistle_event_name">>, Prop);
                  ?HANGUP_EVENT_NAME ->
                      spawn(fun() -> put(callid, UUID), ecallmgr_call_cdr:new_cdr(UUID, Prop) end),
                      ?HANGUP_EVENT_NAME;
                  Evt ->
                      Evt
              end,

    Prop1 = [{<<"Application-Response">>, application_response(FSAppName, Prop, Node, UUID)}
             | props:delete(<<"Application-Response">>, Prop)],

    %% suppress certain events (like bridge completion) as whistle will generate proper masquerades
    %% at more appropriate times using custom events
    case should_publish(FSEvtName, FSAppName, EvtName) of
        true ->
            AppName = case FSEvtName of
                          <<"CUSTOM">> ->
                              props:get_value(<<"whistle_application_name">>, Prop);
                          _ ->
                              case FSAppName of
                                  <<"event">> ->
                                      Args = ecallmgr_util:varstr_to_proplist(props:get_value(<<"Application-Data">>, Prop, <<>>)),
                                      props:get_value(<<"whistle_application_name">>, Args);
                                  App ->
                                      App
                              end
                      end,

            %% some event are 'remapped' to be of other types, when that happens log it differently
            case EvtName =/= FSEvtName orelse AppName =/= FSAppName of
                true ->
                    ?LOG("published event masquerade ~s ~s as ~s ~s", [FSEvtName, FSAppName, EvtName, AppName]);
                false ->
                    ?LOG("published event ~s ~s", [EvtName, AppName])
            end,

	    EvtProp0 = [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Prop1)}
                        ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, Prop1)}
                        ,{<<"Call-ID">>, UUID}
                        ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Prop1)}
                        ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Prop1)}
                        ,{<<"Channel-State">>, get_channel_state(Prop1)}
		       | event_specific(EvtName, AppName, Prop1) ],
	    EvtProp1 = EvtProp0 ++ wh_api:default_headers(<<>>, ?EVENT_CAT, EvtName, ?APP_NAME, ?APP_VERSION),
	    EvtProp2 = case ecallmgr_util:custom_channel_vars(Prop1) of
			   [] -> EvtProp1;
			   CustomProp -> [{<<"Custom-Channel-Vars">>, {struct, CustomProp}} | EvtProp1]
		       end,
	    {'ok', JSON} = wh_api:call_event(EvtProp2),
	    amqp_util:callevt_publish(UUID, JSON, event);
	_ ->
            ok
%%	    ?LOG("skipped event ~s ~s", [FSEvtName, FSAppName])
    end.

-spec get_channel_state/1 :: (Prop) -> binary() when
      Prop :: proplist().
get_channel_state(Prop) ->
    case props:get_value(props:get_value(<<"Channel-State">>, Prop), ?FS_CHANNEL_STATES) of
        undefined -> <<"unknown">>;
        ChannelState -> ChannelState
    end.

%% Setup process to listen for call.status_req api calls and respond in the affirmative
-spec add_amqp_listener/1 :: (CallID) -> binary() | {'error', 'amqp_error'} when
      CallID :: binary().
add_amqp_listener(CallID) ->
    case amqp_util:new_queue(<<>>) of
	{'error', _} = E -> E;
	Q ->
	    _ = amqp_util:bind_q_to_callevt(Q, CallID, status_req),
	    _ = amqp_util:basic_consume(Q),
	    Q
    end.

% gets the appropriate application response value for the type of application
-spec application_response/4 :: (AppName, Prop, Node, UUID) -> binary() when
      AppName :: binary(),
      Prop :: proplist(),
      Node :: atom(),
      UUID :: binary().
application_response(<<"play_and_get_digits">>, Prop, _Node, _UUID) ->
    props:get_value(<<"variable_collected_digits">>, Prop, <<"">>);
application_response(<<"bridge">>, Prop, _Node, _UUID) ->
    props:get_value(<<"variable_originate_disposition">>, Prop, <<"">>);
application_response(<<"conference">>, _Prop, Node, UUID) ->
    get_fs_var(Node, UUID, <<"conference_member_id">>, <<"0">>);
application_response(_AppName, Prop, _Node, _UUID) ->
    props:get_value(<<"Application-Response">>, Prop, <<"">>).

%% return a proplist of k/v pairs specific to the event
-spec event_specific/3 :: (EventName, Application, Prop) -> proplist() when
      EventName :: binary(),
      Application :: binary(),
      Prop :: proplist().
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, Application, Prop) ->
    case props:get_value(Application, ?SUPPORTED_APPLICATIONS) of
	undefined ->
            ?LOG("~s is not a supported application", [Application]),
	    [{<<"Application-Name">>, <<"">>}, {<<"Application-Response">>, <<"">>}];
        <<"noop">> ->
	    [{<<"Application-Name">>, props:get_value(<<"whistle_application_name">>, Prop, <<"">>)}
	     ,{<<"Application-Response">>, props:get_value(<<"whistle_application_response">>, Prop, <<"">>)}
	    ];
        <<"bridge">> ->
            [{<<"Application-Name">>, <<"bridge">>}
	     ,{<<"Application-Response">>, props:get_value(<<"variable_originate_disposition">>, Prop, <<"">>)}
	     ,{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
	     ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
	     ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
	     ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
	     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Prop, <<>>)}
             ,{<<"Hangup-Cause">>, props:get_value(<<"Hangup-Cause">>, Prop, <<>>)}
	     ,{<<"Hangup-Code">>, props:get_value(<<"variable_proto_specific_hangup_cause">>, Prop, <<"">>)}
	    ];
	AppName ->
	    [{<<"Application-Name">>, AppName}
	     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop, <<"">>)}
	    ]
    end;
event_specific(<<"CHANNEL_EXECUTE">>, Application, Prop) ->
    case props:get_value(Application, ?SUPPORTED_APPLICATIONS) of
	undefined ->
            ?LOG("~s is not a supported application", [Application]),
	    [{<<"Application-Name">>, <<"">>}, {<<"Application-Response">>, <<"">>}];
	AppName ->
	    [{<<"Application-Name">>, AppName}
	     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop, <<"">>)}
	    ]
    end;
event_specific(<<"CHANNEL_BRIDGE">>, _, Prop) ->
    [{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Prop, <<>>)}];
event_specific(<<"CHANNEL_UNBRIDGE">>, _, Prop) ->
    [{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>,props:get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Prop, <<>>)}
     ,{<<"Hangup-Cause">>, props:get_value(<<"Hangup-Cause">>, Prop, <<>>)}
     ,{<<"Hangup-Code">>, props:get_value(<<"variable_proto_specific_hangup_cause">>, Prop, <<>>)}];
event_specific(<<"CHANNEL_HANGUP">>, _, Prop) ->
    [{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Prop, <<>>)}
     ,{<<"Hangup-Cause">>, props:get_value(<<"Hangup-Cause">>, Prop, <<>>)}
     ,{<<"Hangup-Code">>, props:get_value(<<"variable_proto_specific_hangup_cause">>, Prop, <<>>)}
    ];
event_specific(?HANGUP_EVENT_NAME, _, Prop) ->
    [{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Prop, <<>>)}
     ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Prop, <<>>)}
     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Prop)}
     ,{<<"Hangup-Cause">>, props:get_value(<<"Hangup-Cause">>, Prop, <<>>)}
     ,{<<"Hangup-Code">>, props:get_value(<<"variable_proto_specific_hangup_cause">>, Prop, <<>>)}
    ];
event_specific(<<"RECORD_STOP">>, _, Prop) ->
    [{<<"Application-Name">>, <<"record">>}
     ,{<<"Application-Response">>, props:get_value(<<"Record-File-Path">>, Prop, <<>>)}
     ,{<<"Terminator">>, props:get_value(<<"variable_playback_terminator_used">>, Prop, <<>>)}
    ];
event_specific(<<"DETECTED_TONE">>, _, Prop) ->
    [{<<"Detected-Tone">>, props:get_value(<<"Detected-Tone">>, Prop, <<>>)}];
event_specific(<<"DTMF">>, _, Prop) ->
    Pressed = props:get_value(<<"DTMF-Digit">>, Prop, <<>>),
    Duration = props:get_value(<<"DTMF-Duration">>, Prop, <<>>),
    ?LOG("DTMF: ~s (~s)", [Pressed, Duration]),
    [{<<"DTMF-Digit">>, Pressed}
     ,{<<"DTMF-Duration">>, Duration}
    ];
event_specific(_Evt, _App, _Prop) ->
    [].

handle_amqp_prop(<<"status_req">>, JObj, Node, IsNodeUp) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallID),

    try
	true = wh_api:call_status_req_v(JObj),
	?LOG_START("call status request received"),

	{Status, ErrMsg} = case IsNodeUp of
			       true -> query_call(Node, CallID);
			       false -> {<<"tmpdown">>, {<<"Error-Msg">>, <<"Handling switch is currently not responding">>}}
			   end,
        ?LOG_END("call status of ~s is ~s", [CallID, Status]),

	RespJObj = [{<<"Call-ID">>, CallID}
		    ,{<<"Status">>, Status}
		    ,{<<"Node">>, Node}
		    | wh_api:default_headers(<<>>, <<"call_event">>, <<"status_resp">>, ?APP_NAME, ?APP_VERSION) ],
	{'ok', Payload} = wh_api:call_status_resp([ ErrMsg | RespJObj ]),
	amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload)
    catch
	E:R ->
	    ?LOG("Call Status exception: ~s:~w", [E, R]),
	    ?LOG("Stackstrace: ~w", [erlang:get_stacktrace()])
    end.

query_call(Node, CallID) ->
    case freeswitch:api(Node, uuid_exists, wh_util:to_list(CallID)) of
	{'ok', Result} ->
	    case wh_util:is_true(Result) of
		true -> {<<"active">>, {ignore, me}};
		false -> {<<"down">>, {<<"Error-Msg">>, <<"Call is no longer active">>}}
	    end;
	_ ->
	    {<<"tmpdown">>, {<<"Error-Msg">>, <<"Switch did not respond to query">>}}
    end.

%% if the call went down but we had queued events to send, try for up to 10 seconds to send them

-spec send_queued/3 :: (Node, UUID, Evts) -> 'ok' when
      Node :: atom(),
      UUID :: binary(),
      Evts :: [proplist(),...].
send_queued(Node, UUID, Evts) ->
    send_queued(Node, UUID, lists:reverse(Evts), 0).

-spec send_queued/4 :: (Node, UUID, Evts, Tries) -> 'ok' when
      Node :: atom(),
      UUID :: binary(),
      Evts :: list(proplist()),
      Tries :: non_neg_integer().
send_queued(_Node, UUID, [], _) ->
    ?LOG(UUID, "no queued events to send", []);
send_queued(_Node, UUID, _, 10=Tries) ->
    ?LOG(UUID, "failed to send queued events after ~b times, going down", [Tries]);
send_queued(Node, UUID, [_|_]=Evts, Tries) ->
    case amqp_util:is_host_available() of
	false ->
	    receive after 1000 -> send_queued(Node, UUID, Evts, Tries + 1) end;
	true ->
	    ?LOG(UUID, "sending queued events on try ~b", [Tries]),
	    _ = [ publish_msg(Node, UUID, E) || E <- Evts ],
	    'ok'
    end.

-spec get_fs_var/4 :: (Node, UUID, Var, Default) -> binary() when
      Node :: atom(),
      UUID :: binary(),
      Var :: binary(),
      Default :: binary().
get_fs_var(Node, UUID, Var, Default) ->
    case freeswitch:api(Node, uuid_getvar, wh_util:to_list(<<UUID/binary, " ", Var/binary>>)) of
        {'ok', <<"_undef_">>} -> Default;
        {'ok', <<"_none_">>} -> Default;
        {'ok', Value} -> Value;
        _ -> Default
    end.

-spec should_publish/3 :: (FSEvtName, FSAppName, EvtName) -> boolean() when
      FSEvtName :: binary(),
      FSAppName :: binary(),
      EvtName :: binary().
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>, _) ->
    false;
should_publish(_, <<"event">>, _) ->
    false;
should_publish(_, _, EvtName) ->
    lists:member(EvtName, ?FS_EVENTS).
