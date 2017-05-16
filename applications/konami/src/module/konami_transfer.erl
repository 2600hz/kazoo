%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz
%%% @doc
%%% Transfers caller to the extension extracted in the regex
%%% Data = {
%%%   "takeback_dtmf":"2" // Transferor can cancel the transfer request
%%%   ,"moh":"media_id" // custom music on hold
%%%   ,"target":"1000" // extension/DID to transfer to
%%%   ,"ringback":"%(2000,4000,440,480)" // ringback to play to transferor
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_transfer).
-behaviour(gen_fsm).

-export([handle/2
        ,pattern_builder/1
        ,number_builder/1
        ]).

-export([pre_originate/2, pre_originate/3
        ,attended_wait/2, attended_wait/3
        ,partial_wait/2, partial_wait/3
        ,attended_answer/2, attended_answer/3
        ,finished/2, finished/3
        ,takeback/2, takeback/3

        ,init/1
        ,handle_event/3
        ,handle_sync_event/4
        ,handle_info/3
        ,terminate/3
        ,code_change/4
        ]).

-include("konami.hrl").

-define(WSD_ID, ?WSD_ENABLED
        andalso {'file', <<(get('callid'))/binary, "_transfer">>}).

-define(WSD_EVT(Fr, T, E), ?WSD_ENABLED
        andalso webseq:evt(?WSD_ID, Fr, T, <<(kz_term:to_binary(?LINE))/binary, "-", E/binary>>)).

-define(WSD_NOTE(W, D, N), ?WSD_ENABLED
        andalso webseq:note(?WSD_ID, W, D, <<(kz_term:to_binary(?LINE))/binary, "-", N/binary>>)).

-define(WSD_TITLE(T), ?WSD_ENABLED
        andalso webseq:title(?WSD_ID, T)).

-define(WSD_START(), ?WSD_ENABLED
        andalso webseq:start(?WSD_ID)).

-define(WSD_STOP(), ?WSD_ENABLED
        andalso webseq:stop(?WSD_ID)).

-record(state, {transferor :: api_ne_binary()
               ,transferee :: api_ne_binary()
               ,target :: api_ne_binary() %% this is the real b-leg, if any
               ,target_a_leg :: api_ne_binary() %% loopback-a
               ,target_b_leg :: api_ne_binary() %% loopback-b
               ,target_legs = [] :: ne_binaries()
               ,call :: kapps_call:call()
               ,target_call = kapps_call:new() :: kapps_call:call()
               ,takeback_dtmf :: api_ne_binary()
               ,transferor_dtmf = <<>> :: binary()
               ,ringback :: api_binary()
               ,moh :: api_binary()
               ,extension :: api_binary()
               ,purgatory_ref :: api_reference()
               ,event_node :: api_ne_binary()
               }).
-type state() :: #state{}.

-define(DEFAULT_TAKEBACK_DTMF,
        kapps_config:get_ne_binary(?CONFIG_CAT, [<<"transfer">>, <<"default_takeback_dtmf">>], <<"*1">>)).

-define(DEFAULT_TARGET_TIMEOUT,
        kapps_config:get_integer(?CONFIG_CAT, [<<"transfer">>, <<"default_target_timeout_ms">>], 20 * ?MILLISECONDS_IN_SECOND)).

-define(DEFAULT_RINGBACK, kapps_config:get_ne_binary(<<"ecallmgr">>, <<"default_ringback">>, <<"%(2000,4000,440,480)">>)).

-define(TRANSFEROR_CALL_EVENTS, [<<"CHANNEL_BRIDGE">>, <<"CHANNEL_UNBRIDGE">>
                                ,<<"DTMF">>
                                ,<<"CHANNEL_DESTROY">>
                                ,<<"dialplan">>
                                ]).

-define(TRANSFEREE_CALL_EVENTS, ?TRANSFEROR_CALL_EVENTS).

-define(TARGET_CALL_EVENTS, [<<"CHANNEL_CREATE">>
                            ,<<"CHANNEL_BRIDGE">>
                            ,<<"CHANNEL_DESTROY">>
                            ,<<"DTMF">>, <<"CHANNEL_REPLACED">>
                            ,<<"dialplan">>
                            ,<<"LEG_CREATED">>, <<"LEG_DESTROYED">>
                            ]).

-spec handle(kz_json:object(), kapps_call:call()) -> no_return().
handle(Data, Call) ->
    kapps_call:put_callid(Call),
    Transferor = kz_json:get_value(<<"dtmf_leg">>, Data),
    Transferee =
        case kapps_call:call_id(Call) of
            Transferor -> kapps_call:other_leg_call_id(Call);
            CallId -> CallId
        end,

    lager:info("first, we need to receive call events for our two legs"),
    add_transferor_bindings(Transferor),
    add_transferee_bindings(Transferee),

    ?WSD_START(),

    ?WSD_TITLE(["Transferee: ", Transferee, " and Transferor: ", Transferor]),

    ?WSD_EVT(Transferor, Transferee, <<"bridged">>),

    lager:info("unbridge transferee ~s and transferor ~s", [Transferee, Transferor]),
    unbridge(Call),

    try gen_fsm:enter_loop(?MODULE, [], 'pre_originate'
                          ,#state{transferor = Transferor
                                 ,transferee = Transferee
                                 ,call = kapps_call:set_controller_queue(konami_event_listener:queue_name(), Call)
                                 ,takeback_dtmf = kz_json:get_value(<<"takeback_dtmf">>, Data, ?DEFAULT_TAKEBACK_DTMF)
                                 ,ringback = to_tonestream(kz_json:get_value(<<"ringback">>, Data, ?DEFAULT_RINGBACK))
                                 ,moh = find_moh(Data, Call)
                                 ,extension = get_extension(kz_json:get_first_defined([<<"captures">>, <<"target">>], Data))
                                 }
                          )
    of
        _ -> 'ok'
    catch
        'exit':'normal' -> 'ok';
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:info("FSM terminated abnormally: ~s: ~p", [_E, _R]),
            kz_util:log_stacktrace(ST)
    end.

-spec get_extension(ne_binaries() | ne_binary()) -> ne_binary().
get_extension([Ext|_]) -> Ext;
get_extension(<<_/binary>> = Ext) -> Ext.

-spec pre_originate(any(), state()) -> handle_fsm_ret(state()).
-spec pre_originate(any(), atom(), state()) -> handle_fsm_ret(state()).
pre_originate(?EVENT(UUID, <<"CHANNEL_UNBRIDGE">>, _Evt)
             ,#state{call=Call
                    ,moh=MOH
                    ,transferee=Transferee
                    ,transferor=Transferor
                    ,extension=Extension
                    }=State
             )
  when UUID =:= Transferee;
       UUID =:= Transferor ->
    MOHToPlay = kz_media_util:media_path(MOH, Call),
    lager:info("putting transferee ~s on hold with MOH ~s", [Transferee, MOHToPlay]),
    HoldCommand = kapps_call_command:hold_command(MOHToPlay, Transferee),
    kapps_call_command:send_command(HoldCommand, Call),

    lager:info("ok, now we need to originate to the requested number ~s", [Extension]),

    Target = originate_to_extension(Extension, Transferor, Call),
    lager:info("originating to target 'a' ~s", [Target]),
    {'next_state', 'attended_wait', State#state{target_a_leg=Target}};
pre_originate(?EVENT(_CallId, <<"CHANNEL_DESTROY">>, _Evt), State) ->
    lager:debug("~s has hungup before we originated, done here", [_CallId]),
    {'stop', 'normal', State};
pre_originate(?EVENT(_CallId, _EventName, _Evt), State) ->
    case kz_call_event:other_leg_call_id(_Evt) of
        'undefined' ->
            lager:info("unhandled event ~s for ~s", [_EventName, _CallId]),
            ?WSD_NOTE(_CallId, 'right', <<"unhandled ", _EventName/binary>>);
        _OtherLeg ->
            lager:info("unhandled event ~s for ~s to ~s", [_EventName, _CallId, _OtherLeg]),
            ?WSD_NOTE(_CallId, _OtherLeg, <<"unhandled ", _EventName/binary>>)
    end,
    {'next_state', 'pre_originate', State}.

pre_originate(_Msg, _From, State) ->
    {'next_state', 'pre_originate', State}.

-spec attended_wait(any(), state()) -> handle_fsm_ret(state()).
-spec attended_wait(any(), atom(), state()) -> handle_fsm_ret(state()).
attended_wait(?EVENT(Transferor, <<"DTMF">>, Evt), #state{transferor=Transferor}=State) ->
    handle_transferor_dtmf(Evt, 'attended_wait', State);
attended_wait(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{transferee=Transferee}=State
             ) ->
    lager:info("transferee ~s hungup (~s) before target could be reached"
              ,[Transferee, kz_call_event:hangup_cause(_Evt)]
              ),
    lager:info("transferor and target are on their own"),
    ?WSD_NOTE(Transferee, 'right', <<"Transferee down in attended_wait">>),
    {'stop', 'normal', State};
attended_wait(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{transferor=Transferor
                    ,target=Target
                    ,transferee=Transferee
                    }=State
             ) ->
    lager:info("transferor ~s hungup (~s), connecting transferee ~s and target ~s (or tbd)"
              ,[Transferor, kz_call_event:hangup_cause(_Evt), Transferee, Target]
              ),
    ?WSD_NOTE(Transferor, 'right', <<"Transferor down in attended wait">>),
    {'next_state', 'partial_wait', State};
attended_wait(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
             ,#state{transferor=Transferor
                    ,transferee=Transferee
                    ,target=Target
                    ,target_call=TargetCall
                    }=State
             ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Target ->
            lager:info("transferor and target ~s are connected, moving to attended_answer", [Target]),
            ?WSD_EVT(Transferor, Target, <<"Bridged to target">>),
            {'next_state', 'attended_answer', State#state{
                                                target_call=kapps_call:exec(
                                                              [{fun kapps_call:set_call_id/2, Target}
                                                              ,{fun kapps_call:set_other_leg_call_id/2, Transferor}
                                                              ]
                                                                           ,TargetCall
                                                             )
                                               }};
        Transferee ->
            lager:info("transferor and transferee have reconnected"),
            ?WSD_EVT(Transferor, Transferee, <<"Bridged to transferee">>),
            hangup_target(TargetCall),
            {'stop', 'normal', State}
    end;
attended_wait(?EVENT(TargetA, <<"CHANNEL_ANSWER">>, _Evt)
             ,#state{target_a_leg=TargetA}=State
             ) ->
    case kz_call_event:other_leg_call_id(_Evt) of
        'undefined' ->
            ?WSD_NOTE(TargetA, 'right', <<"target 'a' answered">>),
            lager:info("target 'a' leg ~s has answered", [TargetA]);
        TargetA ->
            ?WSD_NOTE(TargetA, 'right', <<"target 'a' answered">>),
            lager:info("target 'a' leg ~s has answered", [TargetA]);
        _OtherLeg ->
            ?WSD_EVT(TargetA, _OtherLeg, <<"target 'a' answered">>),
            lager:info("target 'a' leg ~s has answered, attached to ~s", [TargetA, _OtherLeg])
    end,
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
             ,#state{target=Target
                    ,transferor=Transferor
                    ,purgatory_ref=Ref
                    }=State
             ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Transferor ->
            lager:info("recv CHANNEL_BRIDGE on target ~s to transferor ~s, moving to attended_answer"
                      ,[Target, Transferor]
                      ),
            ?WSD_EVT(Target, Transferor, <<"bridged">>),
            maybe_cancel_timer(Ref),
            {'next_state', 'attended_answer', State#state{purgatory_ref='undefined'}};
        _CallId ->
            lager:info("recv CHANNEL_BRIDGE on target ~s to call id ~s", [Target, _CallId]),
            {'next_state', 'attended_wait', State}
    end;
attended_wait(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{target=Target}=State
             ) ->
    lager:debug("target ~s hungup before bridging to transferor", [Target]),
    ?WSD_NOTE(Target, 'right', <<"hungup">>),
    Ref = erlang:start_timer(?MILLISECONDS_IN_SECOND, self(), 'purgatory'),
    {'next_state', 'attended_wait', State#state{purgatory_ref=Ref}};
attended_wait(?EVENT(TargetA, <<"originate_uuid">>, Evt)
             ,#state{target_a_leg=TargetA
                    ,target_call=TargetCall
                    }=State
             ) ->
    Node = kz_json:get_value(<<"Node">>, Evt),
    lager:info("recv control for target 'a' ~s from node ~s", [TargetA, Node]),
    TargetCall1 = kapps_call:from_originate_uuid(Evt, TargetCall),
    ?WSD_NOTE(TargetA, 'right', <<"control for target recv">>),
    {'next_state', 'attended_wait', State#state{target_call=TargetCall1
                                               ,event_node=Node
                                               }};
attended_wait(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
             ,#state{transferor=Transferor
                    ,transferee=Transferee
                    ,target=Target
                    ,target_call=TargetCall
                    }=State
             ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Target ->
            ?WSD_EVT(Transferor, Target, <<"bridge">>),
            lager:info("transferor and target are connected, moving to attended_answer"),
            {'next_state', 'attended_answer', State};
        Transferee ->
            ?WSD_EVT(Transferor, Transferee, <<"bridge">>),
            lager:info("transferor and transferee have reconnected"),
            hangup_target(TargetCall),
            {'stop', 'normal', State};
        _CallId ->
            ?WSD_EVT(Transferor, _CallId, <<"unknown bridge">>),
            lager:info("transferor ~s bridged to ~s, moving to attended_answer"
                      ,[Transferor, _CallId]
                      ),
            {'next_state', 'attended_answer', State}
    end;
attended_wait(?EVENT(TargetA, <<"CHANNEL_CREATE">>, _Evt)
             ,#state{target_a_leg=TargetA
                    ,transferor=Transferor
                    ,ringback=Ringback
                    ,call=Call
                    }=State
             ) ->
    lager:info("transfer target 'a' ~s channel created", [TargetA]),
    ?WSD_NOTE(TargetA, 'right', <<"created">>),
    maybe_start_transferor_ringback(Call, Transferor, Ringback),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(TargetA, <<"LEG_CREATED">>, Evt)
             ,#state{target_a_leg=TargetA
                    ,target_b_leg='undefined'
                    }=State
             ) ->
    TargetB = kz_call_event:other_leg_call_id(Evt),
    lager:debug("target 'b' started: ~s", [TargetB]),
    konami_event_listener:add_call_binding(TargetB, ?TARGET_CALL_EVENTS),

    ?WSD_EVT(TargetA, TargetB, <<"target b leg created">>),

    {'next_state', 'attended_wait', State#state{target_b_leg=TargetB}};
attended_wait(?EVENT(TargetA, <<"originate_resp">>, _Evt)
             ,#state{target_a_leg=TargetA}=State) ->
    lager:info("originate has responded for target ~s", [TargetA]),
    ?WSD_NOTE(TargetA, 'right', <<"originated">>),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(TargetA, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{target_a_leg=TargetA
                    ,target='undefined'
                    ,purgatory_ref='undefined'
                    }=State
             ) ->
    Ref = erlang:start_timer(?MILLISECONDS_IN_SECOND, self(), 'purgatory'),
    lager:debug("target 'a' ~s has gone done (~s), going to purgatory in ~p"
               ,[TargetA, kz_call_event:hangup_cause(_Evt), Ref]
               ),
    {'next_state', 'attended_wait', State#state{purgatory_ref=Ref}};
attended_wait(?EVENT(TargetB, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{target_b_leg=TargetB}=State
             ) ->
    lager:debug("target 'b' ~s has gone done: ~s"
               ,[TargetB, kz_call_event:hangup_cause(_Evt)]
               ),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(TargetA, <<"CHANNEL_REPLACED">>, Evt)
             ,#state{target_a_leg=TargetA
                    ,target='undefined'
                    }=State
             ) ->
    {'next_state', 'attended_wait', handle_real_target(State, kz_call_event:replaced_by(Evt))};
attended_wait(?EVENT(TargetA, <<"CHANNEL_REPLACED">>, Evt)
             ,#state{target_a_leg=TargetA
                    ,target=Target
                    }=State
             ) ->
    case kz_call_event:replaced_by(Evt) of
        Target ->
            lager:debug("target 'a' ~s replaced by known target ~s", [TargetA, Target]),
            {'next_state', 'attended_wait', State};
        ReplacementId ->
            {'next_state', 'attended_wait', handle_real_target(State, ReplacementId)}
    end;
attended_wait(?EVENT(TargetB, <<"CHANNEL_ANSWER">>, _Evt)
             ,#state{target_b_leg=TargetB
                    ,target_a_leg=TargetA
                    ,target_legs=[]
                    }=State
             ) ->
    lager:debug("target 'b' ~s answered with no target legs, connecting to transferor", [TargetB]),
    ?WSD_NOTE(TargetB, 'right', <<"answered with no target legs">>),
    {'next_state', 'attended_wait', handle_real_target(State, TargetA)};
attended_wait(?EVENT(TargetB, <<"CHANNEL_ANSWER">>, _Evt)
             ,#state{target_b_leg=TargetB}=State
             ) ->
    lager:debug("target 'b' ~s answered: ~s", [TargetB, kz_json:encode(_Evt)]),
    ?WSD_NOTE(TargetB, 'right', <<"answered">>),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(TargetB, <<"LEG_CREATED">>, Evt)
             ,#state{target_b_leg=TargetB
                    ,target_legs=TargetLegs
                    }=State
             ) ->
    OtherLeg = kz_call_event:other_leg_call_id(Evt),
    lager:debug("target 'b' ~s has leg ~s starting"
               ,[TargetB, OtherLeg]
               ),
    ?WSD_EVT(TargetB, OtherLeg, <<"created">>),
    {'next_state', 'attended_wait', State#state{target_legs=[OtherLeg | TargetLegs]}};
attended_wait(?EVENT(TargetB, <<"LEG_DESTROYED">>, Evt)
             ,#state{target_b_leg=TargetB
                    ,target_legs=TargetLegs
                    }=State
             ) ->
    OtherLeg = kz_call_event:other_leg_call_id(Evt),
    lager:debug("target 'b' ~s has leg ~s ending"
               ,[TargetB, OtherLeg]
               ),
    ?WSD_EVT(TargetB, OtherLeg, <<"created">>),
    {'next_state', 'attended_wait', State#state{target_legs=props:delete(OtherLeg, TargetLegs)}};
attended_wait(?EVENT(TargetB, <<"CHANNEL_BRIDGE">>, Evt)
             ,#state{target_b_leg=TargetB
                    ,target=Target
                    }=State
             ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Target ->
            lager:debug("target 'b' ~s bridged to target ~s", [TargetB, Target]),
            ?WSD_EVT(TargetB, Target, <<"bridged to target">>),
            {'next_state', 'attended_wait', State};
        OtherLeg ->
            lager:debug("target 'b' ~s bridged to other leg ~s", [TargetB, OtherLeg]),
            ?WSD_EVT(TargetB, OtherLeg, <<"bridged to target">>),
            {'next_state', 'attended_wait', handle_real_target(State, OtherLeg)}
    end;
attended_wait(?EVENT(_CallId, <<"LEG_CREATED">>, _Evt), State) ->
    lager:debug("ignoring leg_created for ~s and ~s"
               ,[_CallId, kz_call_event:other_leg_call_id(_Evt)]
               ),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(TargetA, _EventName, _Evt)
             ,#state{target_a_leg=TargetA}=State
             ) ->
    lager:debug("ignoring event for target 'a' ~s: ~s", [TargetA, _EventName]),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(_CallId, _EventName, _Evt), State) ->
    case kz_call_event:other_leg_call_id(_Evt) of
        'undefined' ->
            lager:info("unhandled event ~s for ~s", [_EventName, _CallId]),
            ?WSD_NOTE(_CallId, 'right', <<"unhandled ", _EventName/binary>>);
        _OtherLeg ->
            lager:info("unhandled event ~s for ~s to ~s", [_EventName, _CallId, _OtherLeg]),
            ?WSD_EVT(_CallId, _OtherLeg, <<"unhandled ", _EventName/binary>>)
    end,
    {'next_state', 'attended_wait', State};
attended_wait(Msg, State) ->
    lager:info("attended_wait: unhandled msg ~p", [Msg]),
    {'next_state', 'attended_wait', State}.

attended_wait(_Msg, _From, State) ->
    {'next_state', 'attended_wait', State}.

-spec partial_wait(any(), state()) -> handle_fsm_ret(state()).
-spec partial_wait(any(), atom(), state()) -> handle_fsm_ret(state()).
partial_wait(?EVENT(Transferee, <<"CHANNEL_BRIDGE">>, Evt)
            ,#state{transferee=Transferee
                   ,target=Target
                   }=State
            ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Target ->
            lager:debug("transfreee has bridged to target ~s", [Target]),
            {'next_state', 'finished', State};
        _OID ->
            lager:debug("transferee has bridged to unknown ~s", [_OID]),
            {'stop', 'normal', State}
    end;
partial_wait(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
            ,#state{transferee=Transferee
                   ,target_call=TargetCall
                   }=State
            ) ->
    lager:info("transferee ~s hungup (~s) while target was being rung"
              ,[Transferee, kz_call_event:hangup_cause(_Evt)]
              ),
    hangup_target(TargetCall),
    {'stop', 'normal', State};
partial_wait(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
            ,#state{transferor=Transferor}=State
            ) ->
    lager:info("transferor ~s hungup(~s), still waiting on target and transferee"
              ,[Transferor, kz_call_event:hangup_cause(_Evt)]
              ),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(TargetA, <<"CHANNEL_DESTROY">>, _Evt)
            ,#state{target_a_leg=TargetA
                   ,target='undefined'
                   ,purgatory_ref='undefined'
                   }=State
            ) ->
    Ref = erlang:start_timer(?MILLISECONDS_IN_SECOND, self(), 'purgatory'),
    lager:debug("target 'a' ~s has gone done (~s), going to purgatory in ~p"
               ,[TargetA, kz_call_event:hangup_cause(_Evt), Ref]
               ),
    {'next_state', 'partial_wait', State#state{purgatory_ref=Ref}};
partial_wait(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
            ,#state{target=Target
                   ,purgatory_ref='undefined'
                   }=State
            ) ->
    Ref = erlang:start_timer(?MILLISECONDS_IN_SECOND, self(), 'purgatory'),
    {'next_state', 'partial_wait', State#state{purgatory_ref=Ref}};
partial_wait(?EVENT(Target, <<"CHANNEL_ANSWER">>, _Evt)
            ,#state{target=Target}=State
            ) ->
    lager:info("target ~s has answered (with ~s)"
              ,[Target, kz_call_event:other_leg_call_id(_Evt)]
              ),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(Target, <<"originate_uuid">>, Evt)
            ,#state{target=Target
                   ,target_call=TargetCall
                   }=State
            ) ->
    lager:info("recv control for target ~s", [Target]),
    ?WSD_NOTE(Target, 'right', <<"control for target recv">>),
    {'next_state', 'partial_wait', State#state{target_call=kapps_call:from_originate_uuid(Evt, TargetCall)}};

partial_wait(?EVENT(TargetB, <<"CHANNEL_ANSWER">>, _Evt)
            ,#state{target_b_leg=TargetB
                   ,target_a_leg=TargetA
                   ,target_legs=[]
                   }=State
            ) ->
    lager:debug("target 'b' ~s answered with no target legs, connecting to transferor", [TargetB]),
    ?WSD_NOTE(TargetB, 'right', <<"answered with no target legs">>),
    {'next_state', 'partial_wait', handle_real_target(State, TargetA, 'transferee')};
partial_wait(?EVENT(TargetB, <<"CHANNEL_ANSWER">>, _Evt)
            ,#state{target_b_leg=TargetB}=State
            ) ->
    lager:debug("target 'b' ~s answered: ~s", [TargetB, kz_json:encode(_Evt)]),
    ?WSD_NOTE(TargetB, 'right', <<"answered">>),
    {'next_state', 'partial_wait', State};

partial_wait(?EVENT(TargetB, <<"CHANNEL_BRIDGE">>, Evt)
            ,#state{target_b_leg=TargetB
                   ,target=Target
                   }=State
            ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Target ->
            lager:debug("target 'b' ~s bridged to target ~s", [TargetB, Target]),
            ?WSD_EVT(TargetB, Target, <<"bridged to target">>),
            {'next_state', 'partial_wait', State};
        OtherLeg ->
            lager:debug("target 'b' ~s bridged to other leg ~s", [TargetB, Target]),
            ?WSD_EVT(TargetB, OtherLeg, <<"bridged to target">>),
            {'next_state', 'partial_wait', handle_real_target(State, OtherLeg, 'transferee')}
    end;
partial_wait(?EVENT(TargetA, <<"CHANNEL_REPLACED">>, Evt)
            ,#state{target_a_leg=TargetA
                   ,target='undefined'
                   }=State
            ) ->
    {'next_state', 'partial_wait', handle_real_target(State, kz_call_event:replaced_by(Evt), 'transferee')};
partial_wait(?EVENT(TargetA, <<"CHANNEL_REPLACED">>, Evt)
            ,#state{target_a_leg=TargetA
                   ,target=Target
                   }=State
            ) ->
    case kz_call_event:replaced_by(Evt) of
        Target ->
            lager:debug("target 'a' ~s replaced by known target ~s", [TargetA, Target]),
            {'next_state', 'partial_wait', State};
        ReplacementId ->
            {'next_state', 'partial_wait', handle_real_target(State, ReplacementId, 'transferee')}
    end;
partial_wait(?EVENT(TargetA, <<"CHANNEL_DESTROY">>, _Evt)
            ,#state{target_a_leg=TargetA}=State
            ) ->
    lager:debug("target 'a' ~s hungup", [TargetA]),
    ?WSD_NOTE(TargetA, 'right', <<"hungup">>),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(TargetB, <<"CHANNEL_DESTROY">>, _Evt)
            ,#state{target_b_leg=TargetB}=State
            ) ->
    lager:debug("target 'b' ~s hungup", [TargetB]),
    ?WSD_NOTE(TargetB, 'right', <<"hungup">>),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(TargetA, _EventName, _Evt)
            ,#state{target_a_leg=TargetA}=State
            ) ->
    lager:debug("ignoring target 'a' ~s: ~s", [TargetA, _EventName]),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(TargetB, _EventName, _Evt)
            ,#state{target_b_leg=TargetB}=State
            ) ->
    lager:debug("ignoring target 'b' ~s: ~s", [TargetB, _EventName]),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(_CallId, _EventName, _Evt), State) ->
    case kz_call_event:other_leg_call_id(_Evt) of
        'undefined' ->
            lager:info("unhandled event ~s for ~s", [_EventName, _CallId]),
            ?WSD_NOTE(_CallId, 'right', <<"unhandled ", _EventName/binary>>);
        _OtherLeg ->
            lager:info("unhandled event ~s for ~s to ~s", [_EventName, _CallId, _OtherLeg]),
            ?WSD_EVT(_CallId, _OtherLeg, <<"unhandled ", _EventName/binary>>)
    end,
    {'next_state', 'partial_wait', State};
partial_wait(Msg, State) ->
    lager:info("partial_wait: unhandled msg ~p", [Msg]),
    {'next_state', 'partial_wait', State}.

partial_wait(_Msg, _From, State) ->
    {'next_state', 'partial_wait', State}.

-spec attended_answer(any(), state()) -> handle_fsm_ret(state()).
-spec attended_answer(any(), atom(), state()) -> handle_fsm_ret(state()).
attended_answer(?EVENT(Transferor, <<"DTMF">>, Evt), #state{transferor=Transferor}=State) ->
    handle_transferor_dtmf(Evt, 'attended_answer', State);
attended_answer(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
               ,#state{transferor=Transferor
                      ,transferee=Transferee
                      ,target=Target
                      ,target_call=TargetCall
                      }=State
               ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Target ->
            lager:info("transferor and target ~s are connected", [Target]),
            ?WSD_EVT(Transferor, Target, <<"transferor bridged to target">>),
            {'next_state', 'attended_answer', State};
        Transferee ->
            ?WSD_EVT(Transferor, Target, <<"transferor bridged to transferee">>),
            lager:info("transferor and transferee have reconnected"),
            hangup_target(TargetCall),
            {'stop', 'normal', State};
        _CallId ->
            lager:info("transferor ~s bridged to unknown ~s", [Transferor, _CallId]),
            {'next_state', 'attended_answer', State}
    end;
attended_answer(?EVENT(Target, <<"originate_uuid">>, Evt)
               ,#state{target=Target
                      ,target_call=TargetCall
                      }=State
               ) ->
    lager:info("recv control for target ~s", [Target]),
    ?WSD_NOTE(Target, 'right', <<"control for target recv">>),
    TargetCall1 = kapps_call:from_originate_uuid(Evt, TargetCall),
    {'next_state', 'attended_answer', State#state{target_call=TargetCall1}};
attended_answer(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
               ,#state{transferor=Transferor
                      ,target=Target
                      }=State
               ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Transferor ->
            lager:info("transferor and target are connected"),
            ?WSD_EVT(Target, Transferor, <<"bridged">>),
            {'next_state', 'attended_answer', State};
        _CallId ->
            lager:info("target ~s bridged to ~s", [Target, _CallId]),
            ?WSD_EVT(Target, _CallId, <<"target bridged to unknown">>),
            {'next_state', 'attended_answer', State}
    end;
attended_answer(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
               ,#state{transferee=Transferee}=State
               ) ->
    lager:info("transferee ~s hungup(~s) while transferor and target were talking"
              ,[Transferee, kz_call_event:hangup_cause(_Evt)]
              ),
    lager:info("transferor and target are on their own"),
    ?WSD_NOTE(Transferee, 'right', <<"channel done">>),
    {'next_state', 'finished', State};
attended_answer(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
               ,#state{transferor=Transferor
                      ,transferee=Transferee
                      ,target=Target
                      ,call=OriginalCall
                      ,target_call=TargetCall
                      }=State
               ) ->
    lager:info("transferor ~s hungup(~s), connecting transferee ~s and target ~s"
              ,[Transferor, kz_call_event:hangup_cause(_Evt), Transferee, Target]
              ),
    ?WSD_NOTE(Transferor, 'right', <<"channel done">>),
    {Leg, Call} = how_to_transfer(OriginalCall, TargetCall, Transferor, Target, Transferee),
    connect_transferee_to_target(Leg, Call),
    {'next_state', 'finished', State};
attended_answer(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
               ,#state{target=Target
                      ,transferor=Transferor
                      ,transferee=Transferee
                      ,call=Call
                      }=State
               ) ->
    lager:info("target ~s hungup(~s), reconnecting transferor ~s to transferee ~s"
              ,[Target, kz_call_event:hangup_cause(_Evt), Transferor, Transferee]
              ),
    ?WSD_NOTE(Target, 'right', <<"target done">>),

    connect_to_transferee(Call),
    {'next_state', 'finished', State};
attended_answer(?EVENT(TargetA, <<"CHANNEL_DESTROY">>, _Evt)
               ,#state{target_a_leg=TargetA}=State
               ) ->
    lager:debug("target 'a' ~s destroyed(~s)", [TargetA, kz_call_event:hangup_cause(_Evt)]),
    ?WSD_EVT(TargetA, 'right', <<"destroyed">>),
    {'next_state', 'attended_answer', State};
attended_answer(?EVENT(TargetB, <<"CHANNEL_DESTROY">>, _Evt)
               ,#state{target_b_leg=TargetB}=State
               ) ->
    lager:debug("target 'b' ~s destroyed: ~s", [TargetB, kz_call_event:hangup_cause(_Evt)]),
    ?WSD_EVT(TargetB, 'right', <<"destroyed">>),
    {'next_state', 'attended_answer', State};
attended_answer(?EVENT(_CallId, _EventName, _Evt), State) ->
    case kz_call_event:other_leg_call_id(_Evt) of
        'undefined' ->
            lager:info("unhandled event ~s for ~s", [_EventName, _CallId]),
            ?WSD_NOTE(_CallId, 'right', <<"unhandled ", _EventName/binary>>);
        _OtherLeg ->
            lager:info("unhandled event ~s for ~s to ~s", [_EventName, _CallId, _OtherLeg]),
            ?WSD_NOTE(_CallId, _OtherLeg, <<"unhandled ", _EventName/binary>>)
    end,
    {'next_state', 'attended_answer', State};
attended_answer(Msg, State) ->
    lager:info("attended_answer: unhandled msg ~p", [Msg]),
    {'next_state', 'attended_answer', State}.

attended_answer(_Msg, _From, State) ->
    {'next_state', 'attended_answer', State}.

-spec finished(any(), state()) -> handle_fsm_ret(state()).
-spec finished(any(), atom(), state()) -> handle_fsm_ret(state()).
finished(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
        ,#state{transferee=_Transferee
               ,target=Target
               ,transferor=Transferor
               }=State
        ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Target ->
            ?WSD_EVT(Target, Transferor, <<"bridged">>),
            lager:debug("transferor and target are bridged"),
            {'stop', 'normal', State};
        _Transferee ->
            ?WSD_EVT(Transferor, _Transferee, <<"bridged">>),
            lager:debug("transferor and transferee bridged"),
            {'stop', 'normal', State};
        _CallId ->
            ?WSD_EVT(_CallId, Transferor, <<"bridged">>),
            lager:debug("transferor bridged to unknown callid ~s", [_CallId]),
            {'next_state', 'finished', State, 5 * ?MILLISECONDS_IN_SECOND}
    end;
finished(?EVENT(Transferee, <<"CHANNEL_BRIDGE">>, Evt)
        ,#state{transferee=Transferee
               ,target=Target
               ,transferor=_Transferor
               }=State
        ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Target ->
            ?WSD_EVT(Target, Transferee, <<"bridged">>),
            lager:debug("transferee and target are bridged"),
            {'stop', 'normal', State};
        _Transferor ->
            ?WSD_EVT(_Transferor, Transferee, <<"bridged">>),
            lager:debug("transferor and transferee bridged"),
            {'stop', 'normal', State};
        _CallId ->
            ?WSD_EVT(_CallId, Transferee, <<"bridged">>),
            lager:debug("transferee bridged to unknown callid ~s", [_CallId]),
            {'next_state', 'finished', State, 5 * ?MILLISECONDS_IN_SECOND}
    end;
finished(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
        ,#state{target=Target
               ,transferee=Transferee
               }=State
        ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Transferee ->
            ?WSD_EVT(Target, Transferee, <<"bridged">>),
            lager:debug("target ~s bridged to transferee ~s", [Target, Transferee]),
            {'stop', 'normal', State};
        _CallId ->
            ?WSD_EVT(Target, _CallId, <<"bridged">>),
            lager:debug("target ~s bridged to ~s", [Target, _CallId]),
            {'next_state', 'finished', State, 5 * ?MILLISECONDS_IN_SECOND}
    end;
finished(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
        ,#state{target=Target}=State
        ) ->
    ?WSD_NOTE(Target, 'right', <<"target hungup">>),
    lager:info("target ~s has hungup: ~s", [Target, kz_call_event:hangup_cause(_Evt)]),
    {'stop', 'normal', State};
finished(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
        ,#state{transferor=Transferor}=State
        ) ->
    ?WSD_NOTE(Transferor, 'right', <<"transferor hungup">>),
    lager:info("transferor ~s has hungup: ~s", [Transferor, kz_call_event:hangup_cause(_Evt)]),
    {'next_state', 'finished', State, 5 * ?MILLISECONDS_IN_SECOND};
finished(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
        ,#state{transferee=Transferee
               ,target_call=TargetCall
               }=State
        ) ->
    lager:info("transferee ~s has hungup: ~s", [Transferee, kz_call_event:hangup_cause(_Evt)]),
    ?WSD_NOTE(Transferee, 'right', <<"transferee hungup">>),
    hangup_target(TargetCall),
    {'stop', 'normal', State};
finished(?EVENT(_CallId, _EventName, _Evt)
        ,State
        ) ->
    case kz_call_event:other_leg_call_id(_Evt) of
        'undefined' ->
            lager:info("unhandled event ~s for ~s", [_EventName, _CallId]),
            ?WSD_NOTE(_CallId, 'right', <<"unhandled ", _EventName/binary>>);
        _OtherLeg ->
            lager:info("unhandled event ~s for ~s to ~s", [_EventName, _CallId, _OtherLeg]),
            ?WSD_NOTE(_CallId, 'right', <<"unhandled ", _EventName/binary>>)
    end,
    {'next_state', 'finished', State};
finished('timeout', State) ->
    lager:info("haven't received anything in a while, going down"),
    {'stop', 'normal', State};
finished(_Msg, State) ->
    lager:info("unhandled message ~p", [_Msg]),
    {'next_state', 'finished', State, 5 * ?MILLISECONDS_IN_SECOND}.

finished(_Req, _From, State) ->
    {'next_state', 'finished', State}.

-spec takeback(any(), state()) -> handle_fsm_ret(state()).
-spec takeback(any(), atom(), state()) -> handle_fsm_ret(state()).
takeback(?EVENT(Transferor, <<"CHANNEL_UNBRIDGE">>, _Evt)
        ,#state{transferor=Transferor
               ,call=Call
               }=State) ->
    lager:debug("transferor ~s unbridged: ~p", [_Evt]),
    lager:debug("now connect to the transferee"),

    connect_to_transferee(Call),
    {'next_state', 'takeback', State};
takeback(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
        ,#state{transferor=Transferor
               ,target=Target
               ,target_call=TargetCall
               }=State
        ) ->
    _OtherLeg = kz_call_event:other_leg_call_id(Evt),
    lager:debug("transferor ~s bridged to ~s, tearing down target ~s"
               ,[Transferor
                ,_OtherLeg
                ,Target
                ]),
    ?WSD_EVT(Transferor, _OtherLeg, <<"bridged">>),
    hangup_target(TargetCall),
    {'stop', 'normal', State};
takeback(?EVENT(Transferee, <<"CHANNEL_BRIDGE">>, Evt)
        ,#state{transferee=Transferee
               ,target=Target
               ,target_call=TargetCall
               }=State
        ) ->
    _OtherLeg = kz_call_event:other_leg_call_id(Evt),
    lager:debug("transferee ~s bridged to ~s, tearing down target ~s"
               ,[Transferee
                ,_OtherLeg
                ,Target
                ]),
    ?WSD_EVT(Transferee, _OtherLeg, <<"bridged">>),
    hangup_target(TargetCall),
    {'stop', 'normal', State};
takeback(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
        ,#state{target=Target
               ,target_call=TargetCall
               ,transferor=Transferor
               }=State
        ) ->
    case kz_call_event:other_leg_call_id(Evt) of
        Transferor ->
            lager:debug("target ~s is bridged to transferor ~s while in takeback"
                       ,[Target, Transferor]
                       ),
            ?WSD_EVT(Target, Transferor, <<"bridged, need takeback again">>),
            hangup_target(TargetCall);
        _CallId ->
            ?WSD_EVT(Target, _CallId, <<"bridged">>),
            lager:debug("ignoring bridge from target ~s to ~s", [Target, _CallId])
    end,
    {'next_state', 'takeback', State};
takeback(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
        ,#state{target=Target
               ,call=Call
               }=State
        ) ->
    lager:debug("target ~s has ended: ~s", [Target, kz_call_event:hangup_cause(_Evt)]),
    ?WSD_NOTE(Target, 'right', <<"hungup">>),
    connect_to_transferee(Call),
    {'next_state', 'takeback', State, 5 * ?MILLISECONDS_IN_SECOND};
takeback(?EVENT(TargetA, <<"CHANNEL_DESTROY">>, _Evt)
        ,#state{target_a_leg=TargetA
               ,call=Call
               }=State
        ) ->
    lager:debug("target 'a' ~s has ended: ~s", [TargetA, kz_call_event:hangup_cause(_Evt)]),
    ?WSD_NOTE(TargetA, 'right', <<"hungup">>),
    connect_to_transferee(Call),
    {'next_state', 'takeback', State, 5 * ?MILLISECONDS_IN_SECOND};
takeback(?EVENT(TargetB, <<"CHANNEL_DESTROY">>, _Evt)
        ,#state{target_b_leg=TargetB}=State
        ) ->
    lager:debug("target 'a' ~s has ended: ~s", [TargetB, kz_call_event:hangup_cause(_Evt)]),
    ?WSD_NOTE(TargetB, 'right', <<"hungup">>),
    {'next_state', 'takeback', State};
takeback(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
        ,#state{transferor=Transferor}=State
        ) ->
    ?WSD_NOTE(Transferor, 'right', <<"hungup">>),
    lager:debug("transferor ~s has ended: ~s", [Transferor, kz_call_event:hangup_cause(_Evt)]),
    {'stop', 'normal', State};
takeback(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
        ,#state{transferee=Transferee}=State
        ) ->
    ?WSD_NOTE(Transferee, 'right', <<"hungup">>),
    lager:debug("transferee ~s has ended: ~s", [Transferee, kz_call_event:hangup_cause(_Evt)]),
    {'stop', 'normal', State};
takeback(?EVENT(TargetA, _EventName, _Evt)
        ,#state{target_a_leg=TargetA}=State
        ) ->
    lager:debug("ignoring target 'a' ~s: ~s", [TargetA, _EventName]),
    {'next_state', 'takeback', State};
takeback(?EVENT(_CallId, _EventName, _Evt), State) ->
    case kz_call_event:other_leg_call_id(_Evt) of
        'undefined' ->
            lager:info("unhandled event ~s for ~s", [_EventName, _CallId]),
            ?WSD_NOTE(_CallId, 'right', <<"unhandled ", _EventName/binary>>);
        _OtherLeg ->
            lager:info("unhandled event ~s for ~s to ~s", [_EventName, _CallId, _OtherLeg]),
            ?WSD_NOTE(_CallId, _OtherLeg, <<"unhandled ", _EventName/binary>>)
    end,
    {'next_state', 'takeback', State};
takeback('timeout', #state{call=Call}=State) ->
    lager:debug("we haven't heard from anyone during takeback, trying to connect again"),
    connect_to_transferee(Call),
    {'next_state', 'takeback', State, 2600};
takeback(_Msg, State) ->
    lager:debug("unhandled message ~p", [_Msg]),
    {'next_state', 'takeback', State}.

takeback(_Req, _From, State) ->
    {'next_state', 'takeback', State}.

-spec handle_event(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_event(_Event, StateName, State) ->
    lager:info("unhandled event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

-spec handle_sync_event(any(), {pid(),any()}, atom(), state()) -> handle_sync_event_ret(state()).
handle_sync_event(_Event, _From, StateName, State) ->
    lager:info("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

-spec handle_info(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_info({'amqp_msg', JObj}, StateName, #state{event_node='undefined'}=State) ->
    send_event(JObj),
    {'next_state', StateName, State};
handle_info({'amqp_msg', JObj}, StateName, #state{event_node=EventNode
                                                 ,target_a_leg=TargetA
                                                 ,target_b_leg=TargetB
                                                 ,target=Target
                                                 }=State) ->
    case {kz_call_event:call_id(JObj)
         ,kz_api:node(JObj)
         }
    of
        {_CallId, 'undefined'} -> send_event(JObj);
        {_CallId, EventNode} -> send_event(JObj);
        {Target, OtherNode} -> suppress_event(JObj, EventNode, OtherNode);
        {TargetA, OtherNode} -> suppress_event(JObj, EventNode, OtherNode);
        {TargetB, OtherNode} -> suppress_event(JObj, EventNode, OtherNode);
        {_CallId, _Node} -> send_event(JObj)
    end,
    {'next_state', StateName, State};
handle_info({'timeout', Ref, 'purgatory'}
           ,'attended_wait'
           ,#state{purgatory_ref=Ref
                  ,target=Target
                  ,call=Call
                  }=State
           ) ->
    lager:info("target ~s didn't answer, reconnecting transferor and transferee", [Target]),
    ?WSD_NOTE(Target, 'right', <<"hungup">>),
    connect_to_transferee(Call),
    {'next_state', 'finished', State};
handle_info({'timeout', Ref, 'purgatory'}
           ,'partial_wait'
           ,#state{purgatory_ref=Ref
                  ,target=Target
                  ,transferee=_Transferee
                  ,call=Call
                  }=State
           ) ->
    lager:info("target ~s hungup, sorry transferee ~s"
              ,[Target, _Transferee]
              ),
    kapps_call_command:hangup(Call),
    {'stop', 'normal', State};
handle_info(_Info, StateName, State) ->
    lager:info("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

-spec send_event(kz_json:object()) -> 'ok'.
send_event(JObj) ->
    gen_fsm:send_event(self()
                      ,?EVENT(kz_json:get_first_defined([<<"Call-ID">>
                                                        ,<<"Outbound-Call-ID">>
                                                        ], JObj)
                             ,kz_json:get_value(<<"Event-Name">>, JObj)
                             ,JObj
                             )
                      ).

-spec terminate(any(), atom(), state()) -> 'ok'.
terminate(_Reason, _StateName, #state{transferor=Transferor
                                     ,transferee=Transferee
                                     ,target=Target
                                     }) ->
    konami_event_listener:rm_call_binding(Transferor, ?TRANSFEROR_CALL_EVENTS),
    konami_event_listener:rm_call_binding(Transferee, ?TRANSFEREE_CALL_EVENTS),
    konami_event_listener:rm_call_binding(Target, ?TARGET_CALL_EVENTS),
    ?WSD_NOTE(Transferor, 'right', <<"eot while in ", (kz_term:to_binary(_StateName))/binary>>),
    ?WSD_STOP(),
    lager:info("fsm terminating while in ~s: ~p", [_StateName, _Reason]).

-spec code_change(any(), atom(), state(), any()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

-spec init(any()) -> {ok, atom(), state()}.
init(_) -> {'ok', 'attended_wait', #state{}}.

-spec add_transferor_bindings(ne_binary()) -> 'ok'.
add_transferor_bindings(CallId) ->
    konami_event_listener:add_call_binding(CallId, ?TRANSFEROR_CALL_EVENTS).

-spec add_transferee_bindings(ne_binary()) -> 'ok'.
add_transferee_bindings(CallId) ->
    konami_event_listener:add_call_binding(CallId, ?TRANSFEREE_CALL_EVENTS).

-spec originate_to_extension(ne_binary(), ne_binary(), kapps_call:call()) -> ne_binary().
originate_to_extension(Extension, TransferorLeg, Call) ->
    MsgId = kz_binary:rand_hex(4),

    CallerIdNumber = caller_id_number(Call, TransferorLeg),

    CCVs = props:filter_undefined(
             [{<<"Account-ID">>, kapps_call:account_id(Call)}
             ,{<<"Authorizing-Type">>, kapps_call:authorizing_type(Call)}
             ,{<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)}
             ,{<<"Channel-Authorized">>, 'true'}
             ,{<<"From-URI">>, <<CallerIdNumber/binary, "@", (kapps_call:account_realm(Call))/binary>>}
             ,{<<"Metaflow-App">>, ?APP_NAME}
             ,{<<"Ignore-Early-Media">>, 'true'}
             ]),

    TargetCallId = create_call_id(),

    Endpoint = kz_json:from_list(
                 props:filter_undefined(
                   [{<<"Invite-Format">>, <<"loopback">>}
                   ,{<<"Route">>,  Extension}
                   ,{<<"To-DID">>, Extension}
                   ,{<<"To-Realm">>, kapps_call:account_realm(Call)}
                   ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                   ,{<<"Outbound-Call-ID">>, TargetCallId}
                   ,{<<"Outbound-Caller-ID-Name">>, caller_id_name(Call, TransferorLeg)}
                   ,{<<"Outbound-Caller-ID-Number">>, caller_id_number(Call, TransferorLeg)}
                   ,{<<"Caller-ID-Name">>, caller_id_name(Call, TransferorLeg)}
                   ,{<<"Caller-ID-Number">>, CallerIdNumber}
                   ,{<<"Ignore-Early-Media">>, 'true'}
                   ])),

    Request = props:filter_undefined(
                [{<<"Endpoints">>, [Endpoint]}
                ,{<<"Outbound-Call-ID">>, TargetCallId}
                ,{<<"Dial-Endpoint-Method">>, <<"single">>}
                ,{<<"Msg-ID">>, MsgId}
                ,{<<"Continue-On-Fail">>, 'true'}
                ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>
                                                    ,<<"Authorizing-Type">>, <<"Authorizing-ID">>
                                                    ,<<"Channel-Authorized">>, <<"Metaflow-App">>
                                                    ]}
                ,{<<"Application-Name">>, <<"park">>}
                ,{<<"Timeout">>, ?DEFAULT_TARGET_TIMEOUT}

                ,{<<"Outbound-Caller-ID-Name">>, caller_id_name(Call, TransferorLeg)}
                ,{<<"Outbound-Caller-ID-Number">>, caller_id_number(Call, TransferorLeg)}
                ,{<<"Caller-ID-Name">>, caller_id_name(Call, TransferorLeg)}
                ,{<<"Caller-ID-Number">>, caller_id_number(Call, TransferorLeg)}

                ,{<<"Existing-Call-ID">>, TransferorLeg}
                ,{<<"Resource-Type">>, <<"originate">>}
                ,{<<"Originate-Immediate">>, 'true'}
                ,{<<"Simplify-Loopback">>, 'true'}
                 | kz_api:default_headers(konami_event_listener:queue_name(), ?APP_NAME, ?APP_VERSION)
                ]),
    ?WSD_NOTE(TargetCallId, 'right', <<"originating to target ", Extension/binary>>),
    konami_event_listener:originate(Request),
    TargetCallId.

-spec create_call_id() -> ne_binary().
create_call_id() ->
    TargetCallId = <<"konami-transfer-", (kz_binary:rand_hex(4))/binary>>,
    bind_target_call_events(TargetCallId),
    TargetCallId.

-spec bind_target_call_events(ne_binary()) -> 'ok'.
bind_target_call_events(CallId) ->
    konami_event_listener:add_call_binding(CallId, ?TARGET_CALL_EVENTS).

-spec caller_id_name(kapps_call:call(), ne_binary()) -> ne_binary().
caller_id_name(Call, CallerLeg) ->
    case kapps_call:call_id(Call) of
        CallerLeg -> kapps_call:caller_id_name(Call);
        _CalleeLeg -> kapps_call:callee_id_name(Call)
    end.

-spec caller_id_number(kapps_call:call(), ne_binary()) -> ne_binary().
caller_id_number(Call, CallerLeg) ->
    case kapps_call:call_id_direct(Call) of
        CallerLeg -> kapps_call:caller_id_number(Call);
        _CalleeLeg -> kapps_call:callee_id_number(Call)
    end.

-spec connect_transferee_to_target(ne_binary(), kapps_call:call()) -> 'ok'.
connect_transferee_to_target(Target, Call) ->
    issue_transferee_event(Target, Call),
    Flags = [{<<"Target-Call-ID">>, Target}
            ,{<<"Continue-On-Fail">>, 'false'}
            ,{<<"Continue-On-Cancel">>, 'false'}
            ,{<<"Park-After-Pickup">>, 'false'}
            ,{<<"Hangup-After-Pickup">>, 'true'}
            ,{<<"B-Leg-Events">>, ?TARGET_CALL_EVENTS}
            ],
    lager:debug("connecting transferee to target ~s, hangup is ~s", [Target, 'true']),
    konami_util:listen_on_other_leg(Call, ?TARGET_CALL_EVENTS),
    connect(Flags, Call).

-spec connect_transferor_to_target(ne_binary(), kapps_call:call()) -> 'ok'.
connect_transferor_to_target(Transferor, TargetCall) ->
    Flags = [{<<"Target-Call-ID">>, Transferor}
            ,{<<"Continue-On-Fail">>, 'true'}
            ,{<<"Continue-On-Cancel">>, 'true'}
            ,{<<"Park-After-Pickup">>, 'true'}
            ,{<<"Hangup-After-Pickup">>, 'false'}
            ,{<<"Publish-Usurp">>, 'false'}
            ,{<<"B-Leg-Events">>, ?TRANSFEROR_CALL_EVENTS}
            ],
    konami_util:listen_on_other_leg(TargetCall, ?TRANSFEROR_CALL_EVENTS),
    lager:debug("connecting transferor to target ~s, hangup is ~s", [kapps_call:call_id(TargetCall), 'false']),
    connect(Flags, TargetCall).

-spec connect_to_transferee(kapps_call:call()) -> 'ok'.
connect_to_transferee(Call) ->
    Flags = [{<<"Target-Call-ID">>, kapps_call:other_leg_call_id(Call)}
            ,{<<"Continue-On-Fail">>, 'false'}
            ,{<<"Continue-On-Cancel">>, 'false'}
            ,{<<"Park-After-Pickup">>, 'false'}
            ,{<<"Hangup-After-Pickup">>, 'true'}
            ,{<<"B-Leg-Events">>, ?TRANSFEROR_CALL_EVENTS}
            ],
    lager:debug("reconnecting transferor/ee: ~s and ~s, hangup is ~s"
               ,[kapps_call:call_id(Call)
                ,kapps_call:other_leg_call_id(Call)
                ,'true'
                ]),
    connect(Flags, Call).

-spec connect(kz_proplist(), kapps_call:call()) -> 'ok'.
connect(Flags, Call) ->
    Command = [{<<"Application-Name">>, <<"connect_leg">>}
              ,{<<"Call-ID">>, kapps_call:call_id(Call)}
              ,{<<"Insert-At">>, <<"now">>}
               | Flags
              ],
    ?WSD_EVT(props:get_value(<<"Call-ID">>, Command)
            ,props:get_value(<<"Target-Call-ID">>, Command)
            ,<<"Connect Legs">>
            ),
    kapps_call_command:send_command(Command, Call).

-type dtmf_next_state() :: 'attended_wait' | 'attended_answer' | 'takeback'.
-spec handle_transferor_dtmf(kz_json:object(), dtmf_next_state(), state()) ->
                                    {'stop', 'normal', state()} |
                                    {'next_state', dtmf_next_state(), state()}.
handle_transferor_dtmf(Evt
                      ,NextState
                      ,#state{target_call=TargetCall
                             ,takeback_dtmf=TakebackDTMF
                             ,transferor_dtmf=DTMFs
                             ,transferor=_Transferor
                             ,transferee=_Transferee
                             ,target=_Target
                             }=State
                      ) ->
    Digit = kz_json:get_value(<<"DTMF-Digit">>, Evt),
    lager:info("recv transferor dtmf '~s', adding to '~s'", [Digit, DTMFs]),

    Collected = <<DTMFs/binary, Digit/binary>>,

    case kz_binary:suffix(TakebackDTMF, Collected) of
        'true' ->
            lager:info("takeback dtmf sequence (~s) engaged!", [TakebackDTMF]),
            ?WSD_NOTE(_Transferor, 'right', <<"takeback sequence engaged">>),
            hangup_target(TargetCall),
            {'next_state', 'takeback', State};
        'false' ->
            {'next_state', NextState, State#state{transferor_dtmf=Collected}}
    end.

-spec unbridge(kapps_call:call()) -> 'ok'.
-spec unbridge(kapps_call:call(), ne_binary()) -> 'ok'.
unbridge(Call) ->
    unbridge(Call, kapps_call:call_id(Call)).
unbridge(Call, CallId) ->
    Command = [{<<"Application-Name">>, <<"unbridge">>}
              ,{<<"Insert-At">>, <<"now">>}
              ,{<<"Leg">>, <<"Both">>}
              ,{<<"Call-ID">>, CallId}
              ],
    ?WSD_NOTE(CallId, 'right', <<"unbridging">>),
    kapps_call_command:send_command(Command, Call).

-spec pattern_builder(kz_json:object()) -> kz_json:object().
pattern_builder(DefaultJObj) ->
    io:format("Let's add a transfer metaflow using a regex to capture the extension/DID~n", []),

    pattern_builder_regex(DefaultJObj).

-spec pattern_builder_regex(kz_json:object()) -> kz_json:object().
pattern_builder_regex(DefaultJObj) ->
    {'ok', [Regex]} = io:fread("First, what regex should invoke the 'transfer'? ", "~s"),
    case re:compile(Regex) of
        {'ok', _} ->
            K = [<<"patterns">>, kz_term:to_binary(Regex)],
            case pattern_builder_check(kz_json:get_value(K, DefaultJObj)) of
                'undefined' -> kz_json:delete_key(K, DefaultJObj);
                PatternJObj -> kz_json:set_value(K, PatternJObj, DefaultJObj)
            end;
        {'error', _E} ->
            io:format("We were unable to compile the regex supplied: ~p~n", [_E]),
            pattern_builder_regex(DefaultJObj)
    end.

-spec pattern_builder_check(api_object()) -> api_object().
pattern_builder_check('undefined') ->
    builder_takeback_dtmf(kz_json:new(), 'undefined');
pattern_builder_check(PatternJObj) ->
    io:format("  Existing config for this pattern: ~s~n", [kz_json:encode(PatternJObj)]),
    io:format("  e. Edit Pattern~n", []),
    io:format("  d. Delete Pattern~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    builder_check_option(PatternJObj
                        ,Option
                        ,fun pattern_builder_check/1
                        ,fun(JObj) ->
                                 builder_takeback_dtmf(JObj, 'undefined')
                         end
                        ).

-spec number_builder(kz_json:object()) -> kz_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's add a transfer metaflow to a specific extension/DID~n", []),

    {'ok', [Number]} = io:fread("First, what number should invoke 'transfer'? ", "~d"),

    K = [<<"numbers">>, kz_term:to_binary(Number)],
    case number_builder_check(kz_json:get_value(K, DefaultJObj)) of
        'undefined' -> kz_json:delete_key(K, DefaultJObj);
        NumberJObj -> kz_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(api_object()) -> api_object().
number_builder_check('undefined') ->
    builder_target(kz_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [kz_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    builder_check_option(NumberJObj
                        ,Option
                        ,fun number_builder_check/1
                        ,fun builder_target/1
                        ).

-type check_fun() :: fun((api_object()) -> api_object()).
-type build_fun() :: fun((kz_json:object()) -> kz_json:object()).

-spec builder_check_option(kz_json:object(), string(), check_fun(), build_fun()) -> api_object().
builder_check_option(JObj, "e", _CheckFun, BuilderFun) ->
    BuilderFun(JObj);
builder_check_option(_JObj, "d", _CheckFun, _BuilderFun) ->
    'undefined';
builder_check_option(JObj, _Option, CheckFun, _BuilderFun) ->
    io:format("invalid selection~n", []),
    CheckFun(JObj).

-spec builder_target(kz_json:object()) -> kz_json:object().
builder_target(JObj) ->
    {'ok', [Target]} = io:fread("What is the target extension/DID to transfer to? ", "~s"),
    builder_takeback_dtmf(JObj, kz_term:to_binary(Target)).

-spec builder_takeback_dtmf(kz_json:object(), api_binary()) -> kz_json:object().
builder_takeback_dtmf(JObj, Target) ->
    {'ok', [Takeback]} = io:fread("What is the takeback DTMF ('n' to use the default)? ", "~s"),
    builder_moh(JObj, Target, kz_term:to_binary(Takeback)).

-spec builder_moh(kz_json:object(), api_binary(), ne_binary()) -> kz_json:object().
builder_moh(JObj, Target, Takeback) ->
    {'ok', [MOH]} = io:fread("Any custom music-on-hold ('n' for none, 'h' for help')? ", "~s"),
    metaflow_jobj(JObj, Target, Takeback, kz_term:to_binary(MOH)).

-spec metaflow_jobj(kz_json:object(), api_binary(), api_binary(), api_binary()) -> kz_json:object().
metaflow_jobj(JObj, Target, Takeback, <<"h">>) ->
    io:format("To set a system_media file as MOH, enter: /system_media/{MEDIA_ID}~n", []),
    io:format("To set an account's media file as MOH, enter: /{ACCOUNT_ID}/{MEDIA_ID}~n", []),
    io:format("To set an third-party HTTP url, enter: http://other.server.com/moh.mp3~n~n", []),
    builder_moh(JObj, Target, Takeback);
metaflow_jobj(JObj, Target, Takeback, MOH) ->
    kz_json:set_values([{<<"module">>, <<"transfer">>}
                       ,{<<"data">>, transfer_data(Target, Takeback, MOH)}
                       ], JObj).

-spec transfer_data(api_binary(), api_binary(), api_binary()) -> kz_json:object().
transfer_data(Target, Takeback, <<"n">>) ->
    transfer_data(Target, Takeback, 'undefined');
transfer_data(Target, <<"n">>, MOH) ->
    transfer_data(Target, 'undefined', MOH);
transfer_data(Target, Takeback, MOH) ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"target">>, Target}
        ,{<<"takeback_dtmf">>, Takeback}
        ,{<<"moh">>, MOH}
        ])).

-spec find_moh(kz_json:object(), kapps_call:call()) -> api_binary().
-spec find_moh(kapps_call:call()) -> api_binary().
find_moh(Data, Call) ->
    case kz_json:get_value(<<"moh">>, Data) of
        'undefined' -> find_moh(Call);
        MOH -> MOH
    end.
find_moh(Call) ->
    {'ok', JObj} = kz_account:fetch(kapps_call:account_id(Call)),
    kz_json:get_value([<<"music_on_hold">>, <<"media_id">>], JObj).

-spec issue_transferee_event(ne_binary(), kapps_call:call()) -> 'ok'.
issue_transferee_event(Target, Call) ->
    API =
        [{<<"Event-Name">>, <<"CHANNEL_TRANSFEREE">>}
        ,{<<"Call-ID">>, kapps_call:call_id(Call)}
        ,{<<"DISPOSITION">>, <<"SUCCESS">>}
        ,{<<"Raw-Application-Name">>,<<"sofia::transferee">>}
         %%,{<<"Direction">>, kapps_call:direction(Call)}
        ,{<<"Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
        ,{<<"Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
        ,{<<"Callee-ID-Name">>, kapps_call:callee_id_name(Call)}
        ,{<<"Callee-ID-Number">>, kapps_call:callee_id_number(Call)}
        ,{<<"Other-Leg-Call-ID">>, kapps_call:other_leg_call_id(Call)}
        ,{<<"Custom-Channel-Vars">>, kapps_call:custom_channel_vars(Call)}
        ,{<<"Target-Call-ID">>, Target}
         | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
        ],
    kapi_call:publish_event(API).

%% We need to figure out which kapps call to issue the connect_leg against
%% When the A-leg is the transferor, its ecallmgr control will be down at this
%% point so use Target's call record; otherwise use the A-leg's.
-spec how_to_transfer(kapps_call:call(), kapps_call:call(), ne_binary(), ne_binary(), ne_binary()) ->
                             {ne_binary(), kapps_call:call()}.
how_to_transfer(OriginalCall, TargetCall, Transferor, Target, Transferee) ->
    case kapps_call:call_id(OriginalCall) of
        Transferor ->
            lager:debug("transferor ~s is in control, but has died; use target call", [Transferor]),
            konami_util:listen_on_other_leg(TargetCall, ?TRANSFEREE_CALL_EVENTS),
            {Transferee, TargetCall};
        Transferee ->
            lager:debug("transferee ~s is in control, use original call", [Transferee]),
            {Target, OriginalCall}
    end.

-spec hangup_target(kapps_call:call()) -> 'ok'.
hangup_target(Call) ->
    ?WSD_NOTE(kapps_call:call_id(Call), 'right', <<"hangup sent">>),
    kapps_call_command:hangup(Call).

-spec to_tonestream(api_binary()) -> api_binary().
to_tonestream('undefined') -> 'undefined';
to_tonestream(<<"tone_stream://", _/binary>> = TS) -> <<TS/binary, ";loops=-1">>;
to_tonestream(Ringback) -> <<"tone_stream://", Ringback/binary, ";loops=-1">>.

-spec maybe_start_transferor_ringback(kapps_call:call(), ne_binary(), api_binary()) -> 'ok'.
maybe_start_transferor_ringback(_Call, _Transferor, 'undefined') -> 'ok';
maybe_start_transferor_ringback(Call, Transferor, Ringback) ->
    Command = kapps_call_command:play_command(Ringback, Transferor),
    lager:debug("playing ringback on ~s to ~s", [Transferor, Ringback]),
    kapps_call_command:send_command(kz_json:set_values([{<<"Insert-At">>, <<"now">>}], Command), Call).

-spec maybe_cancel_timer(api_reference()) -> 'ok'.
maybe_cancel_timer('undefined') -> 'ok';
maybe_cancel_timer(Ref) -> erlang:cancel_timer(Ref).

-spec suppress_event(kz_json:object(), api_binary(), ne_binary()) -> 'ok'.
suppress_event(JObj, _EventNode, _OtherNode) ->
    lager:debug("supressing event ~s from ~s (we want events from ~s): ~s"
               ,[kz_call_event:event_name(JObj)
                ,_OtherNode
                ,_EventNode
                ,kz_json:encode(JObj)
                ]).

-type connect_to() :: 'transferee' | 'transferor'.
-spec handle_real_target(state(), ne_binary()) -> state().
-spec handle_real_target(state(), ne_binary(), connect_to()) -> state().
handle_real_target(State, ReplacementId) ->
    handle_real_target(State, ReplacementId, 'transferor').

handle_real_target(#state{target_a_leg=TargetA
                         ,target_call=TargetCall
                         ,purgatory_ref=Ref
                         }=State
                  ,ReplacementId
                  ,ConnectTo
                  ) ->
    maybe_cancel_timer(Ref),

    lager:debug("target 'a' ~s being replaced by ~s", [TargetA, ReplacementId]),
    ?WSD_EVT(TargetA, ReplacementId, <<"replaced target 'a'">>),

    konami_event_listener:add_call_binding(ReplacementId, ?TARGET_CALL_EVENTS),

    State1 = State#state{target_call=kapps_call:set_call_id(ReplacementId, TargetCall)
                        ,target=ReplacementId
                        ,purgatory_ref='undefined'
                        },

    connect_to(State1, ConnectTo),
    State1.

-spec connect_to(state(), connect_to()) -> 'ok'.
connect_to(#state{transferor=Transferor
                 ,target_call=TargetCall
                 }
          ,'transferor'
          ) ->
    lager:debug("connecting transferor ~s to target", [Transferor]),
    connect_transferor_to_target(Transferor, TargetCall);
connect_to(#state{call=OriginalCall
                 ,target_call=TargetCall
                 ,transferor=Transferor
                 ,target=Target
                 ,transferee=Transferee
                 }
          ,'transferee'
          ) ->
    lager:debug("connecting transferee ~s to target", [Transferee]),
    {Leg, Call} = how_to_transfer(OriginalCall, TargetCall, Transferor, Target, Transferee),
    connect_transferee_to_target(Leg, Call).
