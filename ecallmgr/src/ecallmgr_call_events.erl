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
-define(MAX_FAILED_NODE_CHECKS, 10).
-define(NODE_CHECK_PERIOD, 1000).

%% API
-export([start_link/3]).
-export([swap_call_legs/1]).
-export([process_channel_event/1]).
-export([create_event/3, publish_event/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([queue_name/1, callid/1]).

-define(SERVER, ?MODULE).

-record(state, {
          node = undefined :: atom()
          ,callid = <<>> :: binary()
          ,ctlpid = undefined :: 'undefined' | pid()
          ,is_node_up = true :: boolean()
          ,failed_node_checks = 0 :: integer()
          ,node_down_tref = undefined
          ,sanity_check_tref = undefined
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
-spec start_link/3 :: (atom(), ne_binary(), undefined | pid()) -> {'ok', pid()}.
start_link(Node, CallId, CtlPid) ->
    gen_server:start_link(?MODULE, [Node, CallId, CtlPid], []).

-spec callid/1 :: (pid()) -> ne_binary().
callid(Srv) ->
    gen_server:call(Srv, {callid}).

-spec queue_name/1 :: (pid()) -> ne_binary().
queue_name(Srv) ->
    gen_listener:queue_name(Srv).

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
init([Node, CallId, CtlPid]) ->
    process_flag(trap_exit, true),
    put(callid, CallId),
    ?LOG_START("starting call events listener"),
    TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), {sanity_check}),
    {'ok', #state{node=Node, callid=CallId, ctlpid=CtlPid, sanity_check_tref=TRef}, 0}.

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
handle_call({callid}, _From, #state{callid=CallId}=State) ->
    {reply, CallId, State};
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
handle_cast({channel_destroyed, _}, State) ->
    ?LOG("channel destroyed, goodbye and thanks for all the fish"),
    {stop, normal, State};
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
handle_info({call, {event, [CallId | Props]}}, #state{callid=CallId}=State) ->
    spawn(fun() -> process_channel_event(Props) end),
    {'noreply', State};
handle_info({call_event, {event, [CallId | Props]}}, #state{callid=CallId}=State) ->
    spawn(fun() -> process_channel_event(Props) end),
    {'noreply', State};
handle_info({nodedown, _}, #state{node=Node, is_node_up=true}=State) ->
    ?LOG_SYS("lost connection to node ~s, waiting for reconnection", [Node]),
    erlang:monitor_node(Node, false),
    TRef = erlang:send_after(?NODE_CHECK_PERIOD, self(), {check_node_status}),
    {'noreply', State#state{node_down_tref=TRef, is_node_up=false}, hibernate};
handle_info({check_node_status}, #state{is_node_up=false, failed_node_checks=FNC}=State) when (FNC+1) > ?MAX_FAILED_NODE_CHECKS ->
    ?LOG("node still not up after ~p checks, giving up", [FNC]),
    {stop, normal, State};
handle_info({check_node_status}, #state{node=Node, callid=CallId, is_node_up=false, failed_node_checks=FNC}=State) ->
    case ecallmgr_util:is_node_up(Node, CallId) of
        true ->
            ?LOG("reconnected to node ~s and call is active", [Node]),
            {'noreply', State#state{node_down_tref=undefined, is_node_up=true, failed_node_checks=0}, hibernate};
        false ->
            ?LOG("node ~s still not up, waiting ~pms to test again", [Node, ?NODE_CHECK_PERIOD]),
            TRef = erlang:send_after(?NODE_CHECK_PERIOD, self(), {check_node_status}),
            {'noreply', State#state{node_down_tref=TRef, failed_node_checks=FNC+1}}
    end;
handle_info(timeout, #state{failed_node_checks=FNC}=State) when (FNC+1) > ?MAX_FAILED_NODE_CHECKS ->
    ?LOG("unable to establish initial connectivity to the media node, laterz"),
    {stop, normal, State};
handle_info(timeout, #state{node=Node, callid=CallId, failed_node_checks=FNC}=State) ->
    erlang:monitor_node(Node, true),
    case freeswitch:handlecall(Node, CallId) of
        ok ->
            ?LOG("listening to channel events from ~s", [Node]),
            {'noreply', State, hibernate};
        timeout ->
            ?LOG("timed out trying to listen to channel events from ~s, trying again", [Node]),
            {'noreply', State#state{failed_node_checks=FNC+1}, 1000};
        {'error', badsession} ->
            ?LOG("bad session received when setting up listener for events from ~s", [Node]),
            {stop, normal, State};
        _E ->
            ?LOG("failed to setup listener for channel events from ~s: ~p", [Node, _E]),
            {stop, normal, State}
    end;
handle_info({sanity_check}, #state{node=Node, callid=CallId}=State) ->
    case freeswitch:api(Node, uuid_exists, wh_util:to_list(CallId)) of
        {'ok', <<"true">>} -> 
            ?LOG("listener passed sanity check, call is still up"),
            TRef = erlang:send_after(?SANITY_CHECK_PERIOD, self(), {sanity_check}),
            {'noreply', State#state{sanity_check_tref=TRef}};
        _E ->
            io:format("~p~n", [_E]),
            ?LOG("But I tried, didn't I? Goddamnit, at least I did that."),
            {stop, normal, State#state{sanity_check_tref=undefined}}
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
terminate(_Reason, #state{node_down_tref=NDTRef, sanity_check_tref=SCTRef}) ->   
    catch (erlang:cancel_timer(SCTRef)), 
    catch (erlang:cancel_timer(NDTRef)), 
    ?LOG("terminating call events listener: ~s", [_Reason]).

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
-spec process_channel_event/1 :: (proplist()) -> ok.
process_channel_event(Props) ->
    %% pull everything we need out of the event, since
    %% we might be processing events from external sources,
    %% or call ids that we were not aware we were listening to (xfer)
    CallId = props:get_value(<<"Caller-Unique-ID">>, Props,
                            props:get_value(<<"Unique-ID">>, Props)),
    put(callid, CallId),
    Masqueraded = is_masquerade(Props),
    EventName = get_event_name(Props, Masqueraded),
    ApplicationName = get_event_application(Props, Masqueraded),
    case EventName of 
        %% if we are processing a channel destroy event, find out if there
        %% is an active call event worker and send them a message that
        %% their channel has been destroyed
        <<"CHANNEL_DESTROY">> ->
            case ecallmgr_call_event_sup:find_worker(CallId) of
                {error, _} -> ok;
                {ok, EvtSrv} ->
                    gen_server:cast(EvtSrv, {channel_destroyed, Props})
            end;
        %% if we are processing a channel complete see if there is a
        %% control queue listening for this call and relay the raw
        %% application name to it as a command completed message
        <<"CHANNEL_EXECUTE_COMPLETE">> ->
            case ecallmgr_call_control_sup:find_worker(CallId) of
                {error, _} -> ok;
                {ok, CtrlSrv} ->
                    RawApplication = props:get_value(<<"Application">>, Props, ApplicationName),
                    ?LOG("sending control queue ~p execute completion for '~s'", [CtrlSrv, RawApplication]),
                    ecallmgr_call_control:event_execute_complete(CtrlSrv, CallId, RawApplication)
            end;
        _ ->
            ok
    end,
    case should_publish(EventName, ApplicationName, Masqueraded) of
        false -> 
            ok;
        true ->
            Event = create_event(EventName, ApplicationName, Props),
            publish_event(Event)
    end.

-spec is_masquerade/1 :: (proplist()) -> boolean().
is_masquerade(Props) ->
    case props:get_value(<<"Event-Subclass">>, Props) of
        %% If this is a event created by whistle, then use
        %% the flag it as masqueraded
        <<"whistle::", _/binary>> ->
            true;
        %% otherwise proces as the genuine article 
        _Else ->
            false
    end.

-spec get_event_name/2 :: (proplist(), boolean()) -> undefined | ne_binary().
get_event_name(Props, Masqueraded) ->
    case Masqueraded of
        true ->
            %% when the even is masqueraded override the actual event name
            %% with what whistle wants the event to be
            props:get_value(<<"whistle_event_name">>, Props);
        false ->
            props:get_value(<<"Event-Name">>, Props)
    end.

-spec get_event_application/2 :: (proplist(), boolean()) -> undefined | ne_binary().
get_event_application(Props, Masqueraded) ->
    case Masqueraded of
        %% when the even is masqueraded override the actual application
        %% with what whistle wants the event to be
        true ->
            props:get_value(<<"whistle_application_name">>, Props);
        false ->
            props:get_value(<<"Application">>, Props)
    end.

-spec create_event/3 :: (ne_binary(), ne_binary(), proplist()) -> proplist().
create_event(EventName, ApplicationName, Props) ->
    CCVs = ecallmgr_util:custom_channel_vars(Props),
    Event = [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Props)}
             ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, Props)}
             ,{<<"Call-ID">>, props:get_value(<<"Caller-Unique-ID">>, Props)}
             ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Props)}
             ,{<<"Channel-Call-State">>, props:get_value(<<"Channel-Call-State">>, Props)}
             ,{<<"Channel-State">>, get_channel_state(Props)}
             ,{<<"Transfer-History">>, get_transfer_history(Props)}
             ,{<<"Hangup-Cause">>, get_hangup_cause(Props)}
             ,{<<"Hangup-Code">>, get_hangup_code(Props)}
             ,{<<"Disposition">>, get_disposition(Props)}
             ,{<<"Other-Leg-Direction">>, props:get_value(<<"Other-Leg-Direction">>, Props)}
             ,{<<"Other-Leg-Caller-ID-Name">>, props:get_value(<<"Other-Leg-Caller-ID-Name">>, Props)}
             ,{<<"Other-Leg-Caller-ID-Number">>, props:get_value(<<"Other-Leg-Caller-ID-Number">>, Props)}
             ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Props)}
             ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Props)}                        
             ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
             | event_specific(EventName, ApplicationName, Props) 
            ],
    wh_api:default_headers(<<>>, ?EVENT_CAT, EventName, ?APP_NAME, ?APP_VERSION) ++ Event.

-spec publish_event/1 :: (proplist()) -> ok.
publish_event(Props) ->
    %% call_control publishes channel create/destroy on the control
    %% events queue by calling create_event then this directly.
    EventName = props:get_value(<<"Event-Name">>, Props),
    ApplicationName = props:get_value(<<"Application-Name">>, Props),
    CallId = props:get_value(<<"Call-ID">>, Props),
    put(callid, CallId),
    case props:get_value(<<"Application-Name">>, Props) of
        undefined ->
            ?LOG("publishing channel event ~s", [EventName]);
        ApplicationName -> 
            ?LOG("publishing channel command ~s ~s", [ApplicationName, EventName])
    end,
    wapi_call:publish_event(CallId, Props).

%% return a proplist of k/v pairs specific to the event
-spec event_specific/3 :: (ne_binary(), ne_binary(), proplist()) -> proplist().
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
event_specific(_, <<"conference">>, _) ->
%%    MemberId = get_fs_var(Node, CallId, <<"conference_member_id">>, <<"0">>);
    MemberId = <<"0">>,
    [{<<"Application-Name">>, <<"conference">>}
     ,{<<"Application-Response">>, MemberId}
    ];
event_specific(_, <<"play_and_get_digits">>, Prop) ->
    [{<<"Application-Name">>, <<"play_and_collect_digits">>}
     ,{<<"Application-Response">>, props:get_value(<<"variable_collected_digits">>, Prop, <<"">>)}
    ];
event_specific(_Evt, Application, Prop) ->
    [{<<"Application-Name">>, props:get_value(Application, ?SUPPORTED_APPLICATIONS)}
     ,{<<"Application-Response">>, props:get_value(<<"Application-Response">>, Prop)}
    ].

-spec get_fs_var/4 :: (atom(), ne_binary(), ne_binary(), binary()) -> binary().
get_fs_var(Node, CallId, Var, Default) ->
    case freeswitch:api(Node, callid_getvar, wh_util:to_list(<<CallId/binary, " ", Var/binary>>)) of
        {'ok', <<"_undef_">>} -> Default;
        {'ok', <<"_none_">>} -> Default;
        {'ok', Value} -> Value;
        _ -> Default
    end.

-spec should_publish/3 :: (ne_binary(), ne_binary(), boolean()) -> boolean().
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>, false) ->
    ?LOG("suppressing bridge execute complete in favour the whistle masquerade of this event"),
    false;
should_publish(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"intercept">>, false) ->
    ?LOG("suppressing intercept execute complete in favour the whistle masquerade of this event"),
    false;
should_publish(<<"CHANNEL_EXECUTE", _/binary>>, Application, _) ->
    props:get_value(Application, ?SUPPORTED_APPLICATIONS) =/= undefined;
should_publish(EventName, _, _) ->
    lists:member(EventName, ?FS_EVENTS).

-spec get_transfer_history/1 :: (proplist()) -> json_object().
get_transfer_history(Props) ->
    SerializedHistory = props:get_value(<<"variable_transfer_history">>, Props),
    Hist = [HistJObj 
            || Trnsf <- ecallmgr_util:unserialize_fs_array(SerializedHistory)
                   ,(HistJObj = create_trnsf_history_object(binary:split(Trnsf, <<":">>, [global]))) =/= undefined],
    wh_json:from_list(Hist).

-spec create_trnsf_history_object/1 :: (list()) -> {binary, json_object} | undefined.
create_trnsf_history_object([Epoch, CallId, <<"att_xfer">>, Props]) ->
    [Transferee, Transferer] = binary:split(Props, <<"/">>),
    Props = [{<<"callid">>, CallId}
             ,{<<"type">>, <<"attended">>}
             ,{<<"transferee">>, Transferee}
             ,{<<"transferer">>, Transferer}
            ],
    {Epoch, wh_json:from_list(Props)};
create_trnsf_history_object([Epoch, CallId, <<"bl_xfer">> | Props]) ->            
    %% This looks confusing but FS uses the same delimiter to in the array
    %% as it does for inline dialplan actions (like those created during partial attended)
    %% so we have to put it together to take it apart... I KNOW! ARRRG
    Dialplan = lists:last(binary:split(wh_util:join_binary(Props, <<":">>), <<",">>)),
    [Exten | _] = binary:split(Dialplan, <<"/">>, [global]),    
    Props = [{<<"callid">>, CallId}
             ,{<<"type">>, <<"blind">>}
             ,{<<"extension">>, Exten}
            ],
    {Epoch, wh_json:from_list(Props)};        
create_trnsf_history_object(_) ->
    undefined.

-spec get_channel_state/1 :: (proplist()) -> binary().
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

-spec swap_call_legs/1 :: (proplist()) -> proplist().
-spec swap_call_legs/2 :: (proplist(), proplist()) -> proplist().

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
