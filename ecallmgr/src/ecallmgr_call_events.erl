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
-export([start_link/3, publish_msg/3, swap_call_legs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  node = undefined :: atom()
          ,uuid = <<>> :: binary()
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
    ?LOG("linked to control listener process ~p",[CtlPid]),
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
		    ?LOG("started event listener for other leg as process ~p", [_Pid])
	    end;
	_ -> 'ok'
    end,

    case IsAmqpUp of
	true ->
	    spawn(fun() -> put(callid, UUID), send_ctl_event(EvtName, UUID, CtlPid, Data), publish_msg(Node, UUID, Data) end),
	    {'noreply', State};
	false ->
	    send_ctl_event(EvtName, UUID, CtlPid, AppName),
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

handle_info(startup, #state{node=Node, uuid=UUID}=State) ->
    erlang:monitor_node(Node, true),
    case freeswitch:handlecall(Node, UUID) of
        ok ->
            ?LOG("handling events from ~s", [Node]),
            {'noreply', State, hibernate};
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

-spec shutdown/2 :: (pid() | 'undefined', ne_binary()) -> 'ok'.
shutdown(undefined, _) -> 'ok';
shutdown(CtlPid, UUID) ->
    ?LOG("sending hangup for call ~s", [UUID]),
    is_pid(CtlPid) andalso CtlPid ! {hangup, self(), UUID},
    'ok'.

%% let the ctl process know a command finished executing
-spec send_ctl_event/4 :: (EventName, UUID, CtlPid, Event) -> 'ok' when
      CtlPid :: pid() | undefined,
      UUID :: binary(),
      EventName :: binary(),
      Event :: proplist().
send_ctl_event(_, _, undefined, _) -> 'ok';
send_ctl_event(<<"CHANNEL_EXECUTE_COMPLETE">>, UUID, CtlPid, Props) when is_pid(CtlPid) ->
    AppName = props:get_value(<<"Application">>, Props),

    ?LOG("sending execution completion for ~s to control queue", [wh_util:binary_to_lower(AppName)]),
    ecallmgr_call_control:event_execute_complete(CtlPid, UUID, AppName);
send_ctl_event(<<"CUSTOM">>, UUID, CtlPid, Props) when is_pid(CtlPid) ->
    case props:get_value(<<"Event-Subclass">>, Props) of
	<<"whistle::noop">> ->
	    ?LOG("sending execution completion for noop to control queue"),
	    ecallmgr_call_control:event_execute_complete(CtlPid, UUID, <<"noop">>);
	_ ->
	    'ok'
    end;
send_ctl_event(_, _, _, _) -> 'ok'.

-spec publish_msg/3 :: (Node, UUID, Prop) -> 'ok' when
      Node :: atom(),
      UUID :: undefined | binary(),
      Prop :: proplist().
publish_msg(_, _, []) -> 'ok';
publish_msg(_, undefined, _) -> ok;
publish_msg(Node, UUID, Prop) when is_list(Prop) ->
    FSEvtName = props:get_value(<<"Event-Name">>, Prop),
    FSAppName = props:get_value(<<"Application">>, Prop),

    %% whistle generates custom events that should masquerade as standard events
    %% for simplicity in the high level code (like bridge which "completes" during
    %% transfers even though the bridge still continues).
    EvtName = case FSEvtName of
                  <<"CUSTOM">> ->
                      props:get_value(<<"whistle_event_name">>, Prop);
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
            ?LOG("publishing call event ~s ~s", [wh_util:binary_to_lower(EvtName)
                                                 ,wh_util:binary_to_lower(AppName)]),
	    EvtProp0 = [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Prop1)}
                        ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, Prop1)}
                        ,{<<"Call-ID">>, UUID}
                        ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Prop1)}
                        ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Prop1)}
                        ,{<<"Channel-State">>, get_channel_state(Prop1)}
                        ,{<<"Transfer-History">>, get_transfer_history(Prop1)}
                        ,{<<"Hangup-Cause">>, get_hangup_cause(Prop1)}
                        ,{<<"Hangup-Code">>, get_hangup_code(Prop1)}
                        ,{<<"Disposition">>, get_disposition(Prop1)}
                        ,{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Prop1)}
                        ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Prop1)}
                        ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Prop1)}
                        ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Prop1)}
                        ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Prop1)}                        
                        | event_specific(EvtName, AppName, Prop1) 
                       ],
	    EvtProp1 = wh_api:default_headers(<<>>, ?EVENT_CAT, EvtName, ?APP_NAME, ?APP_VERSION) ++ EvtProp0,
	    EvtProp2 = case ecallmgr_util:custom_channel_vars(Prop1) of
			   [] -> EvtProp1;
			   CustomProp -> [{<<"Custom-Channel-Vars">>, wh_json:from_list(CustomProp)} | EvtProp1]
		       end,
	    wapi_call:publish_event(UUID, EvtProp2);
	_ ->
            ok
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
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>, Prop) ->
    [{<<"Application-Name">>, props:get_value(<<"whistle_application_name">>, Prop)}
     ,{<<"Application-Response">>, props:get_value(<<"whistle_application_response">>, Prop)}
    ];
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>, Prop) ->
    [{<<"Application-Name">>, <<"bridge">>}
     ,{<<"Application-Response">>, props:get_value(<<"variable_originate_disposition">>, Prop, <<"FAIL">>)}
    ];
event_specific(<<"RECORD_STOP">>, _, Prop) ->
    [{<<"Application-Name">>, <<"record">>}
     ,{<<"Application-Response">>, props:get_value(<<"Record-File-Path">>, Prop)}
     ,{<<"Terminator">>, props:get_value(<<"variable_playback_terminator_used">>, Prop)}
    ];
event_specific(<<"DETECTED_TONE">>, _, Prop) ->
    [{<<"Detected-Tone">>, props:get_value(<<"Detected-Tone">>, Prop)}];
event_specific(<<"DTMF">>, _, Prop) ->
    Pressed = props:get_value(<<"DTMF-Digit">>, Prop),
    Duration = props:get_value(<<"DTMF-Duration">>, Prop),
    ?LOG("received DTMF ~s (~s)", [Pressed, Duration]),
    [{<<"DTMF-Digit">>, Pressed}
     ,{<<"DTMF-Duration">>, Duration}
    ];
event_specific(_Evt, Application, Prop) ->
    [{<<"Application-Name">>, props:get_value(Application, ?SUPPORTED_APPLICATIONS)}
     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop)}
    ].

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
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"intercept">>, _) ->
    false;
should_publish(<<"CHANNEL_EXECUTE", _/binary>>, Application, _) ->
    props:get_value(Application, ?SUPPORTED_APPLICATIONS) =/= undefined;
should_publish(_, <<"event">>, _) ->
    false;
should_publish(_, _, EvtName) ->
    lists:member(EvtName, ?FS_EVENTS).

-spec get_transfer_history/1 :: (proplist()) -> json_object().
get_transfer_history(Props) ->
    SerializedHistory = props:get_value(<<"variable_transfer_history">>, Props),
    Hist = [HistJObj 
            || Trnsf <- ecallmgr_util:unserialize_fs_array(SerializedHistory)
                   ,(HistJObj = create_trnsf_history_object(binary:split(Trnsf, <<":">>, [global]))) =/= undefined],
    wh_json:from_list(Hist).

-spec create_trnsf_history_object/1 :: (list()) -> {binary, json_object} | undefined.
create_trnsf_history_object([Epoch, UUID, <<"att_xfer">>, Data]) ->
    [Transferee, Transferer] = binary:split(Data, <<"/">>),
    Props = [{<<"uuid">>, UUID}
             ,{<<"type">>, <<"attended">>}
             ,{<<"transferee">>, Transferee}
             ,{<<"transferer">>, Transferer}
            ],
    {Epoch, wh_json:from_list(Props)};
create_trnsf_history_object([Epoch, UUID, <<"bl_xfer">> | Data]) ->            
    %% This looks confusing but FS uses the same delimiter to in the array
    %% as it does for inline dialplan actions (like those created during partial attended)
    %% so we have to put it together to take it apart... I KNOW! ARRRG
    Dialplan = lists:last(binary:split(wh_util:join_binary(Data, <<":">>), <<",">>)),
    [Exten | _] = binary:split(Dialplan, <<"/">>, [global]),    
    Props = [{<<"uuid">>, UUID}
             ,{<<"type">>, <<"blind">>}
             ,{<<"extension">>, Exten}
            ],
    {Epoch, wh_json:from_list(Props)};        
create_trnsf_history_object(_) ->
    undefined.

-spec get_channel_state/1 :: (Prop) -> binary() when
      Prop :: proplist().
get_channel_state(Prop) ->
    case props:get_value(props:get_value(<<"Channel-State">>, Prop), ?FS_CHANNEL_STATES) of
        undefined -> <<"unknown">>;
        ChannelState -> ChannelState
    end.

-spec get_hangup_cause/1 :: (proplist()) -> undefined | ne_binary().
get_hangup_cause(Props) ->
    Causes = case props:get_value(<<"variable_current_application">>, Props) of
                 <<"bridge">> ->
                     [<<"variable_bridge_hangup_cause">>, <<"variable_hangup_cause">>, <<"Hangup-Cause">>];
                 _ ->
                     [<<"variable_hangup_cause">>, <<"variable_bridge_hangup_cause">>, <<"Hangup-Cause">>]
             end,
    find_event_value(Causes, Props).

-spec get_disposition/1 :: (proplist()) -> undefined | ne_binary().
get_disposition(Props) ->
    find_event_value([<<"variable_endpoint_disposition">>
                          ,<<"variable_originate_disposition">>
                     ], Props).

-spec get_hangup_code/1 :: (proplist()) -> undefined | ne_binary().
get_hangup_code(Props) ->
    find_event_value([<<"variable_proto_specific_hangup_cause">>
                          ,<<"variable_last_bridge_proto_specific_hangup_cause">>
                     ], Props).
    
-spec find_event_value/2 :: ([ne_binary(),...], proplist()) -> undefined | ne_binary().
find_event_value(Keys, Props) ->
    find_event_value(Keys, Props, undefined).

-spec find_event_value/3 :: ([ne_binary(),...], proplist(), term()) -> term().
find_event_value([], _, Default) ->
    Default;
find_event_value([H|T], Props, Default) ->
    Value = props:get_value(H, Props),
    case wh_util:is_empty(Value) of
        true -> find_event_value(T, Props, Default);
        false -> Value
    end.

swap_call_legs(Props) ->
    swap_call_legs(Props, []).

swap_call_legs([], Swap) ->
    Swap;
swap_call_legs([{<<"Caller-", Key/binary>>, Value}|T], Swap) ->
    swap_call_legs(T, [{<<"Other-Leg-", Key/binary>>, Value}|Swap]);
swap_call_legs([{<<"Other-Leg-", Key/binary>>, Value}|T], Swap) ->
    swap_call_legs(T, [{<<"Caller-", Key/binary>>, Value}|Swap]);
swap_call_legs([Prop|T], Swap) ->
    swap_call_legs(T, [Prop|Swap]).
