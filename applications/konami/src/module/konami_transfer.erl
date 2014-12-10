%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Transfers caller to the extension extracted in the regex
%%% Data = {
%%%   "takeback_dtmf":"2" // Transferor can cancel the transfer request
%%%   ,"moh":"media_id" // custom music on hold
%%%   ,"target":"1000" // extension/DID to transfer to
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

-export([attended_wait/2, attended_wait/3
         ,partial_wait/2, partial_wait/3
         ,attended_answer/2, attended_answer/3
         ,finished/2, finished/3

         ,init/1
         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4
        ]).

-include("../konami.hrl").

-record(state, {transferor :: ne_binary()
                ,transferee :: ne_binary()
                ,target :: ne_binary()
                ,target_b_legs = [] :: ne_binaries()
                ,call :: whapps_call:call()
                ,target_call = whapps_call:new() :: whapps_call:call() | 'undefined'
                ,takeback_dtmf :: ne_binary()
                ,transferor_dtmf = <<>> :: binary()
               }).
-type state() :: #state{}.

-define(DEFAULT_TAKEBACK_DTMF
        ,whapps_config:get(?CONFIG_CAT, [<<"transfer">>, <<"default_takeback_dtmf">>], <<"*1">>)
       ).

-define(DEFAULT_TARGET_TIMEOUT
        ,whapps_config:get_integer(?CONFIG_CAT, [<<"transfer">>, <<"default_target_timeout_ms">>], 20000)
       ).

-define(TRANSFEROR_CALL_EVENTS, [<<"CHANNEL_BRIDGE">>
                                 ,<<"DTMF">>
                                 ,<<"LEG_CREATED">>
                                 ,<<"CHANNEL_DESTROY">>, <<"LEG_DESTROYED">>
                                ]).

-define(TRANSFEREE_CALL_EVENTS, [<<"CHANNEL_BRIDGE">>
                                 ,<<"LEG_CREATED">>
                                 ,<<"CHANNEL_DESTROY">>, <<"LEG_DESTROYED">>
                                ]).
-define(TARGET_CALL_EVENTS, [<<"CHANNEL_ANSWER">>
                             ,<<"CHANNEL_CREATE">>, <<"LEG_CREATED">>
                             ,<<"CHANNEL_BRIDGE">>
                             ,<<"CHANNEL_DESTROY">>, <<"LEG_DESTROYED">>
                            ]).

-spec handle(wh_json:object(), whapps_call:call()) -> no_return().
handle(Data, Call) ->
    TransferorLeg = wh_json:get_value(<<"dtmf_leg">>, Data),
    TransfereeLeg =
        case whapps_call:call_id(Call) of
            TransferorLeg -> whapps_call:other_leg_call_id(Call);
            CallId -> CallId
        end,

    lager:info("first, we need to receive call events for our two legs"),
    add_transferor_bindings(TransferorLeg),
    add_transferee_bindings(TransfereeLeg),

    lager:info("unbridge and put transferee ~s into hold", [TransfereeLeg]),
    whapps_call_command:unbridge(Call),

    MOH = wh_media_util:media_path(find_moh(Data, Call), Call),
    lager:info("putting transferee ~s on hold with MOH ~s", [TransfereeLeg, MOH]),
    HoldCommand = whapps_call_command:hold_command(MOH, TransfereeLeg),
    whapps_call_command:send_command(HoldCommand, Call),

    Extension =
        case wh_json:get_first_defined([<<"captures">>, <<"target">>], Data) of
            [Ext|_] -> Ext;
            <<_/binary>> = Ext -> Ext
        end,

    lager:info("ok, now we need to originate to the requested number ~s", [Extension]),

    Target = originate_to_extension(Extension, TransferorLeg, Call),
    lager:info("originating to ~s", [Target]),

    try gen_fsm:enter_loop(?MODULE, [], 'attended_wait'
                           ,#state{transferor=TransferorLeg
                                   ,transferee=TransfereeLeg
                                   ,target=Target
                                   ,call=Call
                                   ,takeback_dtmf=wh_json:get_value(<<"takeback_dtmf">>, Data, ?DEFAULT_TAKEBACK_DTMF)
                                  }
                          )
    of
        _ -> 'ok'
    catch
        'exit':'normal' -> 'ok';
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:info("FSM terminated abnormally: ~s: ~p", [_E, _R]),
            wh_util:log_stacktrace(ST)
    end.

attended_wait(?EVENT(Transferor, <<"DTMF">>, Evt), #state{transferor=Transferor}=State) ->
    handle_transferor_dtmf(Evt, 'attended_wait', State);
attended_wait(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{transferee=Transferee}=State
             ) ->
    lager:info("transferee ~s hungup before target could be reached"),
    lager:info("transferor and target are on their own"),
    {'stop', 'normal', State};
attended_wait(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{transferor=Transferor
                      ,target=Target
                      ,transferee=Transferee
                     }=State
             ) ->
    lager:info("transferor ~s hungup, connecting transferee ~s and target ~s once target answers"
               ,[Transferor, Transferee, Target]
              ),
    {'next_state', 'partial_wait', State};
attended_wait(?EVENT(Transferor, <<"LEG_DESTROYED">>, _Evt)
              ,#state{transferor=Transferor
                      ,target=Target
                      ,transferee=Transferee
                     }=State
             ) ->
    lager:info("transferor ~s hungup, connecting transferee ~s and target ~s"
               ,[Transferor, Transferee, Target]
              ),
    {'next_state', 'partial_wait', State};
attended_wait(?EVENT(Target, <<"LEG_CREATED">>, Evt)
              ,#state{target=Target
                      ,target_call=_TargetCall
                      ,target_b_legs=Bs
                     }=State
             ) ->
    BLeg = wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt),
    case lists:member(BLeg, Bs) of
        'true' -> {'next_state', 'attended_wait', State};
        'false' ->
            lager:info("new leg on target ~s: ~s", [Target, BLeg]),
            add_transferor_bindings(BLeg),
            {'next_state', 'attended_wait', State#state{target_b_legs=[BLeg | Bs]}}
    end;
attended_wait(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
              ,#state{transferor=Transferor
                      ,transferee=Transferee
                      ,target=Target
                      ,target_call=TargetCall
                     }=State
             ) ->
    lager:info("transferor ~s bridged to ~s", [Transferor, wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt)]),
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Target ->
            lager:info("transferor and target are connected"),
            {'next_state', 'attended_answer', State};
        Transferee ->
            lager:info("transferor and transferee have reconnected"),
            whapps_call_command:hangup(TargetCall),
            {'stop', 'normal', State}
    end;
attended_wait(?EVENT(Target, <<"CHANNEL_ANSWER">>, _Evt)
              ,#state{transferor=Transferor
                      ,target=Target
                      ,target_call=TargetCall
                     }=State
             ) ->
    lager:info("target ~s has answered, connect to transferor ~s", [Target, Transferor]),
    lager:info("target ctrl ~s", [whapps_call:control_queue(TargetCall)]),

    connect_transferor_to_target(Transferor, TargetCall),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
              ,#state{target=Target
                      ,transferor=Transferor
                     }=State
             ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Transferor ->
            lager:info("recv CHANNEL_BRIDGE on target ~s to transferor ~s", [Target, Transferor]),
            {'next_state', 'attended_answer', State};
        _CallId ->
            lager:info("recv CHANNEL_BRIDGE on target ~s to call id ~s", [Target, _CallId]),
            {'next_state', 'attended_wait', State}
    end;
attended_wait(?EVENT(Target, <<"originate_uuid">>, Evt)
              ,#state{target=Target
                      ,target_call=TargetCall
                     }=State
             ) ->
    lager:info("recv control for target ~s", [Target]),
    TargetCall1 = whapps_call:from_originate_uuid(Evt, TargetCall),
    {'next_state', 'attended_wait', State#state{target_call=TargetCall1}};
attended_wait(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
              ,#state{transferor=Transferor
                      ,transferee=Transferee
                      ,target=Target
                      ,target_call=TargetCall
                     }=State
             ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Target ->
            lager:info("transferor and target are connected"),
            {'next_state', 'attended_answer', State};
        Transferee ->
            lager:info("transferor and transferee have reconnected"),
            whapps_call_command:hangup(TargetCall),
            {'stop', 'normal', State};
        _CallId ->
            lager:info("transferor ~s bridged to ~s", [Transferor, _CallId]),
            {'next_state', 'attended_answer', State}
    end;
attended_wait(?EVENT(B, <<"CHANNEL_BRIDGE">>, _Evt)
              ,#state{target_b_legs=[B|_Bs]}=State
             ) ->
    lager:debug("b leg ~s bridged to ~s", [B, wh_json:get_value(<<"Other-Leg-Call-ID">>, _Evt)]),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(Target, <<"CHANNEL_CREATE">>, _Evt)
              ,#state{target=Target}=State
             ) ->
    lager:info("transfer target ~s channel created", [Target]),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(Target, <<"originate_resp">>, _Evt)
              ,#state{target=Target}=State
             ) ->
    lager:info("originate has responded for target ~s", [Target]),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target=Target
                      ,call=Call
                      ,target_b_legs=[]
                     }=State
             ) ->
    lager:info("target ~s didn't answer, reconnecting transferor and transferee", [Target]),
    connect_to_transferee(Call),
    {'stop', 'normal', State};
attended_wait(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target=Target
                      ,target_b_legs=[B|Bs]
                      ,target_call=TargetCall
                      ,transferor=Transferor
                     }=State
             ) ->
    lager:info("target ~s hungup, still have b-leg ~s (~p)", [Target, B, Bs]),
    TargetCall1 = whapps_call:set_call_id(B, TargetCall),
    connect_transferor_to_target(Transferor, TargetCall1),
    lager:debug("connecting transferor ~s to target ~s", [Transferor, B]),
    {'next_state', 'attended_wait', State#state{target=B
                                                ,target_b_legs=Bs
                                                ,target_call=TargetCall1
                                               }};
attended_wait(?EVENT(CallId, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target_b_legs=[CallId]
                      ,target='undefined'
                      ,call=Call
                     }=State
             ) ->
    lager:info("target b-leg ~s finished and no target, reconnecting transferor and transferee", [CallId]),
    connect_to_transferee(Call),
    {'stop', 'normal', State};
attended_wait(?EVENT(CallId, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target_b_legs=[CallId]}=State
             ) ->
    lager:info("target b-leg ~s finished", [CallId]),
    {'next_state', 'attended_wait', State#state{target_b_legs=[]}};
attended_wait(?EVENT(_CallId, _EventName, _Evt)
              ,#state{transferor=_Transferor
                      ,transferee=_Transferee
                      ,target=_Target
                     }=State) ->
    lager:info("attended_wait: unhandled event ~s for ~s: ~p", [_EventName, _CallId, _Evt]),
    lager:debug("transferor: ~s transferee: ~s target: ~s", [_Transferor, _Transferee, _Target]),
    {'next_state', 'attended_wait', State};
attended_wait(Msg, State) ->
    lager:info("attended_wait: unhandled msg ~p", [Msg]),
    {'next_state', 'attended_wait', State}.

attended_wait(_Msg, _From, State) ->
    {'next_state', 'attended_wait', State}.

partial_wait(?EVENT(Transferee, <<"CHANNEL_BRIDGE">>, Evt)
             ,#state{transferee=Transferee
                     ,target=Target
                     ,target_b_legs=[B|_Bs]
                    }=State
            ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Target ->
            lager:debug("transfreee has bridged to target ~s", [Target]),
            {'next_state', 'finished', State};
        B ->
            lager:debug("transferee has bridged to target b ~s", [B]),
            {'next_state', 'finished', State};
        _OID ->
            lager:debug("transferee has bridged to unknown ~s (~p)", [_OID, _Bs]),
            {'stop', 'normal', State}
    end;
partial_wait(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{transferee=Transferee
                     ,target_call=TargetCall
                    }=State
            ) ->
    lager:info("transferee ~s hungup while target was being rung", [Transferee]),
    whapps_call_command:hangup(TargetCall),
    {'stop', 'normal', State};
partial_wait(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{transferor=Transferor}=State
            ) ->
    lager:info("transferor ~s hungup, still waiting on target and transferee", [Transferor]),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(Transferor, <<"LEG_DESTROYED">>, _Evt)
             ,#state{transferor=Transferor}=State
            ) ->
    lager:info("transferor ~s hungup, still waiting on target and transferee", [Transferor]),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(Target, EventName, _Evt)
             ,#state{target=Target
                     ,target_b_legs=[]
                     ,transferor=_Transferor
                     ,transferee=_Transferee
                    }=State
            )
  when EventName =:= <<"CHANNEL_DESTROY">>
       orelse EventName =:= <<"LEG_DESTROYED">> ->
    lager:info("target ~s hungup, sorry transferee ~s"
               ,[Target, _Transferee]
              ),
    {'stop', 'normal', State};
partial_wait(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{target=Target
                     ,target_b_legs=[B|Bs]
                     ,call=Call
                     ,transferor=Transferor
                     ,transferee=Transferee
                    }=State
            ) ->
    lager:info("target ~s hungup (but ~s is still up)", [Target, B]),

    connect_transferee_to_target(B, Call, Bs =:= []),
    issue_internal_transferee(Call, Transferor, Transferee, B),

    {'next_state', 'partial_wait', State#state{target=B
                                               ,target_b_legs=Bs
                                              }};
partial_wait(?EVENT(Target, <<"CHANNEL_ANSWER">>, _Evt)
             ,#state{transferee=Transferee
                     ,transferor=Transferor
                     ,target=Target
                     ,target_b_legs=Bs
                     ,call=Call
                    }=State
            ) ->
    lager:info("target ~s has answered, connect to transferee ~s", [Target, Transferee]),
    connect_transferee_to_target(Target, Call, Bs =:= []),
    issue_internal_transferee(Call, Transferor, Transferee, Target),
    {'next_state', 'finished', State};
partial_wait(?EVENT(Target, <<"originate_uuid">>, Evt)
             ,#state{target=Target
                     ,target_call=TargetCall
                    }=State
            ) ->
    lager:info("recv control for target ~s", [Target]),
    {'next_state', 'partial_wait', State#state{target_call=whapps_call:from_originate_uuid(Evt, TargetCall)}};
partial_wait(?EVENT(B, <<"CHANNEL_BRIDGE">>, _Evt)
              ,#state{target_b_legs=[B|_Bs]}=State
             ) ->
    lager:debug("b leg ~s bridged to ~s", [B, wh_json:get_value(<<"Other-Leg-Call-ID">>, _Evt)]),
    {'next_state', 'partial_wait', State};

partial_wait(?EVENT(_CallId, _EventName, _Evt), State) ->
    lager:info("partial_wait: unhandled event ~s for ~s: ~p", [_EventName, _CallId, _Evt]),
    {'next_state', 'partial_wait', State};
partial_wait(Msg, State) ->
    lager:info("partial_wait: unhandled msg ~p", [Msg]),
    {'next_state', 'partial_wait', State}.

partial_wait(_Msg, _From, State) ->
    {'next_state', 'partial_wait', State}.

attended_answer(?EVENT(Transferor, <<"DTMF">>, Evt), #state{transferor=Transferor}=State) ->
    handle_transferor_dtmf(Evt, 'attended_answer', State);
attended_answer(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
                ,#state{transferor=Transferor
                        ,transferee=Transferee
                        ,target=Target
                        ,target_call=TargetCall
                       }=State
               ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Target ->
            lager:info("transferor and target are connected"),
            {'next_state', 'attended_answer', State};
        Transferee ->
            lager:info("transferor and transferee have reconnected"),
            whapps_call_command:hangup(TargetCall),
            {'stop', 'normal', State};
        _CallId ->
            lager:info("transferor ~s bridged to ~s", [Transferor, _CallId]),
            {'next_state', 'attended_answer', State}
    end;
attended_answer(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
                ,#state{transferor=Transferor
                        ,target=Target
                       }=State
               ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Transferor ->
            lager:info("transferor and target are connected"),
            {'next_state', 'attended_answer', State};
        _CallId ->
            lager:info("target ~s bridged to ~s", [Target, _CallId]),
            {'next_state', 'attended_answer', State}
    end;
attended_answer(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
                ,#state{transferee=Transferee}=State
               ) ->
    lager:info("transferee ~s hungup while transferor and target were talking", [Transferee]),
    lager:info("transferor and target are on their own"),
    {'next_state', 'finished', State};
attended_answer(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{transferor=Transferor
                      ,transferee=Transferee
                      ,target=Target
                      ,target_b_legs=Bs
                      ,call=Call
                     }=State
             ) ->
    lager:info("transferor ~s hungup, connecting transferee ~s and target ~s"
               ,[Transferor, Transferee, Target]
              ),
    connect_transferee_to_target(Target, Call, Bs =:= []),
    issue_internal_transferee(Call, Transferor, Transferee, Target),
    {'next_state', 'finished', State};
attended_answer(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target=Target
                      ,target_b_legs=[B|Bs]
                      ,target_call=TargetCall
                      ,transferor=Transferor
                     }=State
               ) ->
    lager:info("current target ~s destroyed, but ~s is still around", [Target, B]),
    TargetCall1 = whapps_call:set_call_id(B, TargetCall),
    connect_transferor_to_target(Transferor, TargetCall1),
    {'next_state', 'attended_answer', State#state{target_call=TargetCall1
                                                  ,target=B
                                                  ,target_b_legs=Bs
                                                 }};
attended_answer(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target=Target
                      ,target_b_legs=[]
                      ,transferor=Transferor
                      ,transferee=Transferee
                      ,call=Call
                     }=State
             ) ->
    lager:info("target ~s hungup, reconnecting transferor ~s to transferee ~s"
                ,[Target, Transferor, Transferee]
               ),

    connect_to_transferee(Call),
    {'stop', 'normal', State};
attended_answer(?EVENT(CallId, <<"CHANNEL_DESTROY">>, _Evt)
                ,#state{target_b_legs=Bs
                        ,target=Target
                       }=State
               ) ->
    case lists:member(CallId, Bs) of
        'true' ->
            lager:info("target b leg ~s down", [CallId]),
            gen_fsm:send_event(self(), ?EVENT(Target, <<"CHANNEL_DESTROY">>, wh_json:new())),
            {'next_state', 'attended_answer', State#state{target_b_legs=lists:delete(CallId, Bs)}};
        'false' ->
            lager:info("unknown leg ~s down", [CallId]),
            {'next_state', 'attended_answer', State}
    end;
attended_answer(?EVENT(_CallId, _EventName, _Evt), State) ->
    lager:info("attended_answer: unhandled event ~s for ~s: ~p", [_EventName, _CallId, _Evt]),
    {'next_state', 'attended_answer', State};
attended_answer(Msg, State) ->
    lager:info("attended_answer: unhandled msg ~p", [Msg]),
    {'next_state', 'attended_answer', State}.

attended_answer(_Msg, _From, State) ->
    {'next_state', 'attended_answer', State}.

finished(?EVENT(Transferee, <<"CHANNEL_BRIDGE">>, _Evt)
         ,#state{transferee=Transferee}=State
        ) ->
    lager:debug("transferee bridged to ~s", [wh_json:get_value(<<"Other-Leg-Call-ID">>, _Evt)]),
    {'next_state', 'finished', State};
finished(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
         ,#state{target=Target
                 ,transferee=Transferee
                }=State
         ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Transferee ->
            lager:debug("target ~s bridged to transferee ~s", [Target, Transferee]),
            {'stop', 'normal', State};
        _CallId ->
            lager:debug("target ~s bridged to ~s", [Target, _CallId]),
            {'next_state', 'finished', State}
    end;
finished(?EVENT(TargetBLeg, <<"CHANNEL_BRIDGE">>, Evt)
         ,#state{target_b_legs=[TargetBLeg|_Bs]
                 ,target=Target
                 ,transferee=Transferee
                }=State
        ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Target ->
            lager:debug("target ~s has bridged to target b ~s", [Target, TargetBLeg]);
        Transferee ->
            lager:debug("target b ~s has bridged to transferee ~s", [TargetBLeg, Transferee]);
        _CallId ->
            lager:debug("target b ~s has bridged to unknown ~s", [TargetBLeg, _CallId])
    end,
    {'next_state', 'finished', State};
finished(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
         ,#state{target=Target
                 ,target_b_legs=[]
                }=State
        ) ->
    lager:info("target ~s has hungup", [Target]),
    {'stop', 'normal', State};
finished(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
         ,#state{target=Target
                 ,transferee=Transferee
                 ,transferor=Transferor
                 ,target_b_legs=[B|Bs]
                 ,call=Call
                }=State
        ) ->
    lager:debug("target ~s down, b ~s is up, connecting to transferee ~s", [Target, B, Transferee]),
    connect_transferee_to_target(B, Call, 'true'),
    issue_internal_transferee(Call, Transferor, Transferee, B),
    {'next_state', 'finished', State#state{target=B
                                           ,target_b_legs=Bs
                                          }};
finished(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
         ,#state{transferor=Transferor}=State
        ) ->
    lager:info("transferor ~s has hungup", [Transferor]),
    {'next_state', 'finished', State};
finished(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
         ,#state{transferee=Transferee
                 ,target_call=TargetCall
                }=State
        ) ->
    lager:info("transferee ~s has hungup", [Transferee]),
    whapps_call_command:hangup(TargetCall),
    {'next_state', 'finished', State, 5000};
finished(?EVENT(CallId, <<"CHANNEL_DESTROY">>, _Evt)
         ,#state{target_b_legs=[]}=State
        ) ->
    lager:info("unknown call leg ~s hungup", [CallId]),
    {'stop', 'normal', State};
finished(?EVENT(CallId, <<"CHANNEL_DESTROY">>, _Evt)
         ,#state{target_b_legs=Bs}=State
        ) ->
    case lists:member(CallId, Bs) of
        'true' -> lager:info("target b leg ~s hungup", [CallId]);
        'false' -> lager:info("unknown call leg ~s hungup", [CallId])
    end,
    {'next_state', 'finished', State#state{target_b_legs=lists:delete(CallId, Bs)}, 5000};
finished(?EVENT(_CallId, _EventName, _Evt)
         ,State
        ) ->
    lager:info("unhandled event ~s for ~s", [_EventName, _CallId]),
    {'next_state', 'finished', State};
finished('timeout', State) ->
    lager:info("haven't received anything in a while, going down"),
    {'stop', 'normal', State};
finished(_Msg, State) ->
    lager:info("unhandled message ~p", [_Msg]),
    {'next_state', 'finished', State, 5000}.

finished(_Req, _From, State) ->
    {'next_state', 'finished', State}.

handle_event(_Event, StateName, State) ->
    lager:info("unhandled event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:info("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

handle_info({'amqp_msg', JObj}, StateName, State) ->
    gen_fsm:send_event(self()
                       ,?EVENT(wh_json:get_first_defined([<<"Call-ID">>
                                                          ,<<"Outbound-Call-ID">>
                                                         ], JObj)
                               ,wh_json:get_value(<<"Event-Name">>, JObj)
                               ,JObj
                              )
                      ),
    {'next_state', StateName, State};
handle_info(_Info, StateName, State) ->
    lager:info("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

terminate(_Reason, _StateName, #state{transferor=Transferor
                                      ,transferee=Transferee
                                      ,target=Target
                                     }) ->
    konami_event_listener:rm_call_binding(Transferor, ?TRANSFEROR_CALL_EVENTS),
    konami_event_listener:rm_call_binding(Transferee, ?TRANSFEREE_CALL_EVENTS),
    konami_event_listener:rm_call_binding(Target, ?TARGET_CALL_EVENTS),
    lager:info("fsm terminating while in ~s: ~p", [_StateName, _Reason]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

init(_) -> {'ok', 'attended_wait', #state{}}.

-spec add_transferor_bindings(ne_binary()) -> 'ok'.
add_transferor_bindings(CallId) ->
    konami_event_listener:add_call_binding(CallId, ?TRANSFEROR_CALL_EVENTS).

-spec add_transferee_bindings(ne_binary()) -> 'ok'.
add_transferee_bindings(CallId) ->
    konami_event_listener:add_call_binding(CallId, ?TRANSFEREE_CALL_EVENTS).

-spec originate_to_extension(ne_binary(), ne_binary(), whapps_call:call()) -> ne_binary().
originate_to_extension(Extension, TransferorLeg, Call) ->
    MsgId = wh_util:rand_hex_binary(4),

    CallerIdNumber = caller_id_number(Call, TransferorLeg),

    CCVs = [{<<"Account-ID">>, whapps_call:account_id(Call)}
            ,{<<"Authorizing-ID">>, whapps_call:account_id(Call)}
            ,{<<"Channel-Authorized">>, 'true'}
            ,{<<"From-URI">>, <<CallerIdNumber/binary, "@", (whapps_call:account_realm(Call))/binary>>}
           ],

    TargetCallId = create_call_id(),

    Endpoint = wh_json:from_list(
                 props:filter_undefined(
                   [{<<"Invite-Format">>, <<"loopback">>}
                    ,{<<"Route">>,  Extension}
                    ,{<<"To-DID">>, Extension}
                    ,{<<"To-Realm">>, whapps_call:account_realm(Call)}
                    ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
                    ,{<<"Outbound-Call-ID">>, TargetCallId}
                    ,{<<"Outbound-Caller-ID-Name">>, caller_id_name(Call, TransferorLeg)}
                    ,{<<"Outbound-Caller-ID-Number">>, caller_id_number(Call, TransferorLeg)}
                    ,{<<"Caller-ID-Name">>, caller_id_name(Call, TransferorLeg)}
                    ,{<<"Caller-ID-Number">>, CallerIdNumber}
                   ])),

    Request = props:filter_undefined(
                [{<<"Endpoints">>, [Endpoint]}
                 ,{<<"Outbound-Call-ID">>, TargetCallId}
                 ,{<<"Dial-Endpoint-Method">>, <<"single">>}
                 ,{<<"Msg-ID">>, MsgId}
                 ,{<<"Continue-On-Fail">>, 'true'}
                 ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
                 ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>
                                                      ,<<"Authorizing-Type">>, <<"Authorizing-ID">>
                                                      ,<<"Channel-Authorized">>
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
                 | wh_api:default_headers(konami_event_listener:queue_name(), ?APP_NAME, ?APP_VERSION)
                ]),

    konami_event_listener:originate(Request),
    TargetCallId.

-spec create_call_id() -> ne_binary().
create_call_id() ->
    TargetCallId = <<"konami-transfer-", (wh_util:rand_hex_binary(4))/binary>>,
    konami_event_listener:add_call_binding(TargetCallId, ?TARGET_CALL_EVENTS),
    TargetCallId.

-spec caller_id_name(whapps_call:call(), ne_binary()) -> ne_binary().
caller_id_name(Call, CallerLeg) ->
    case whapps_call:call_id(Call) of
        CallerLeg -> whapps_call:caller_id_name(Call);
        _CalleeLeg -> whapps_call:callee_id_name(Call)
    end.

-spec caller_id_number(whapps_call:call(), ne_binary()) -> ne_binary().
caller_id_number(Call, CallerLeg) ->
    case whapps_call:call_id_direct(Call) of
        CallerLeg -> whapps_call:caller_id_number(Call);
        _CalleeLeg -> whapps_call:callee_id_number(Call)
    end.

-spec connect_transferee_to_target(ne_binary(), whapps_call:call(), boolean()) -> 'ok'.
connect_transferee_to_target(Target, Call, Hangup) ->
    issue_transferee_event(Target, Call),
    Flags = [{<<"Target-Call-ID">>, Target}
             ,{<<"Continue-On-Fail">>, not Hangup}
             ,{<<"Continue-On-Cancel">>, not Hangup}
             ,{<<"Park-After-Pickup">>, not Hangup}
             ,{<<"Hangup-After-Pickup">>, Hangup}
            ],
    konami_util:listen_on_other_leg(Call, [<<"DTMF">>]),
    connect(Flags, Call).

-spec connect_transferor_to_target(ne_binary(), whapps_call:call()) -> 'ok'.
connect_transferor_to_target(Transferor, TargetCall) ->
    Flags = [{<<"Target-Call-ID">>, Transferor}
             ,{<<"Continue-On-Fail">>, 'true'}
             ,{<<"Continue-On-Cancel">>, 'true'}
             ,{<<"Park-After-Pickup">>, 'true'}
            ],
    connect(Flags, TargetCall).

-spec connect_to_transferee(whapps_call:call()) -> 'ok'.
connect_to_transferee(Call) ->
    Flags = [{<<"Target-Call-ID">>, whapps_call:other_leg_call_id(Call)}
             ,{<<"Continue-On-Fail">>, 'false'}
             ,{<<"Continue-On-Cancel">>, 'false'}
             ,{<<"Park-After-Pickup">>, 'false'}
            ],
    konami_util:listen_on_other_leg(Call, [<<"DTMF">>]),
    connect(Flags, Call).

-spec connect(wh_proplist(), whapps_call:call()) -> 'ok'.
connect(Flags, Call) ->
    Command = [{<<"Application-Name">>, <<"connect_leg">>}
               ,{<<"Call-ID">>, whapps_call:call_id(Call)}
               ,{<<"Insert-At">>, <<"now">>}
               | Flags
              ],
    whapps_call_command:send_command(Command, Call).

-spec handle_transferor_dtmf(wh_json:object(), NextState, state()) ->
                                    {'stop', 'normal', state()} |
                                    {'next_state', NextState, state()}.
handle_transferor_dtmf(Evt, NextState
                       ,#state{call=Call
                               ,target_call=TargetCall
                               ,target_b_legs=Bs
                               ,takeback_dtmf=TakebackDTMF
                               ,transferor_dtmf=DTMFs
                              }=State
                      ) ->
    Digit = wh_json:get_value(<<"DTMF-Digit">>, Evt),
    lager:info("recv transferor dtmf '~s', adding to '~s'", [Digit, DTMFs]),

    Collected = <<DTMFs/binary, Digit/binary>>,

    case wh_util:suffix_binary(TakebackDTMF, Collected) of
        'true' ->
            lager:info("takeback dtmf sequence (~s) engaged!", [TakebackDTMF]),
            connect_to_transferee(Call),
            hangup_target(TargetCall, Bs),
            {'stop', 'normal', State};
        'false' ->
            {'next_state', NextState, State#state{transferor_dtmf=Collected}}
    end.

-spec pattern_builder(wh_json:object()) -> wh_json:object().
pattern_builder(DefaultJObj) ->
    io:format("Let's add a transfer metaflow using a regex to capture the extension/DID~n", []),

    pattern_builder_regex(DefaultJObj).

-spec pattern_builder_regex(wh_json:object()) -> wh_json:object().
pattern_builder_regex(DefaultJObj) ->
    {'ok', [Regex]} = io:fread("First, what regex should invoke the 'transfer'? ", "~s"),
    case re:compile(Regex) of
        {'ok', _} ->
            K = [<<"patterns">>, wh_util:to_binary(Regex)],
            case pattern_builder_check(wh_json:get_value(K, DefaultJObj)) of
                'undefined' -> wh_json:delete_key(K, DefaultJObj);
                PatternJObj -> wh_json:set_value(K, PatternJObj, DefaultJObj)
            end;
        {'error', _E} ->
            io:format("We were unable to compile the regex supplied: ~p~n", [_E]),
            pattern_builder_regex(DefaultJObj)
    end.

-spec pattern_builder_check(api_object()) -> api_object().
pattern_builder_check('undefined') ->
    builder_takeback_dtmf(wh_json:new(), 'undefined');
pattern_builder_check(PatternJObj) ->
    io:format("  Existing config for this pattern: ~s~n", [wh_json:encode(PatternJObj)]),
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

-spec number_builder(wh_json:object()) -> wh_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's add a transfer metaflow to a specific extension/DID~n", []),

    {'ok', [Number]} = io:fread("First, what number should invoke 'transfer'? ", "~d"),

    K = [<<"numbers">>, wh_util:to_binary(Number)],
    case number_builder_check(wh_json:get_value(K, DefaultJObj)) of
        'undefined' -> wh_json:delete_key(K, DefaultJObj);
        NumberJObj -> wh_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(api_object()) -> api_object().
number_builder_check('undefined') ->
    builder_target(wh_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [wh_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    builder_check_option(NumberJObj
                         ,Option
                         ,fun number_builder_check/1
                         ,fun builder_target/1
                        ).

-type check_fun() :: fun((api_object()) -> api_object()).
-type build_fun() :: fun((wh_json:object()) -> wh_json:object()).

-spec builder_check_option(wh_json:object(), string(), check_fun(), build_fun()) -> api_object().
builder_check_option(JObj, "e", _CheckFun, BuilderFun) ->
    BuilderFun(JObj);
builder_check_option(_JObj, "d", _CheckFun, _BuilderFun) ->
    'undefined';
builder_check_option(JObj, _Option, CheckFun, _BuilderFun) ->
    io:format("invalid selection~n", []),
    CheckFun(JObj).

-spec builder_target(wh_json:object()) -> wh_json:object().
builder_target(JObj) ->
    {'ok', [Target]} = io:fread("What is the target extension/DID to transfer to? ", "~s"),
    builder_takeback_dtmf(JObj, wh_util:to_binary(Target)).

-spec builder_takeback_dtmf(wh_json:object(), api_binary()) -> wh_json:object().
builder_takeback_dtmf(JObj, Target) ->
    {'ok', [Takeback]} = io:fread("What is the takeback DTMF ('n' to use the default)? ", "~s"),
    builder_moh(JObj, Target, wh_util:to_binary(Takeback)).

-spec builder_moh(wh_json:object(), api_binary(), ne_binary()) -> wh_json:object().
builder_moh(JObj, Target, Takeback) ->
    {'ok', [MOH]} = io:fread("Any custom music-on-hold ('n' for none, 'h' for help')? ", "~s"),
    metaflow_jobj(JObj, Target, Takeback, wh_util:to_binary(MOH)).

-spec metaflow_jobj(wh_json:object(), api_binary(), api_binary(), api_binary()) -> wh_json:object().
metaflow_jobj(JObj, Target, Takeback, <<"h">>) ->
    io:format("To set a system_media file as MOH, enter: /system_media/{MEDIA_ID}~n", []),
    io:format("To set an account's media file as MOH, enter: /{ACCOUNT_ID}/{MEDIA_ID}~n", []),
    io:format("To set an third-party HTTP url, enter: http://other.server.com/moh.mp3~n~n", []),
    builder_moh(JObj, Target, Takeback);
metaflow_jobj(JObj, Target, Takeback, MOH) ->
    wh_json:set_values([{<<"module">>, <<"transfer">>}
                        ,{<<"data">>, transfer_data(Target, Takeback, MOH)}
                       ], JObj).

-spec transfer_data(api_binary(), api_binary(), api_binary()) -> wh_json:object().
transfer_data(Target, Takeback, <<"n">>) ->
    transfer_data(Target, Takeback, 'undefined');
transfer_data(Target, <<"n">>, MOH) ->
    transfer_data(Target, 'undefined', MOH);
transfer_data(Target, Takeback, MOH) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"target">>, Target}
         ,{<<"takeback_dtmf">>, Takeback}
         ,{<<"moh">>, MOH}
        ])).

-spec find_moh(wh_json:object(), whapps_call:call()) -> api_binary().
-spec find_moh(whapps_call:call()) -> api_binary().
find_moh(Data, Call) ->
    case wh_json:get_value(<<"moh">>, Data) of
        'undefined' -> find_moh(Call);
        MOH -> MOH
    end.
find_moh(Call) ->
    {'ok', JObj} = couch_mgr:open_cache_doc(whapps_call:account_db(Call)
                                            ,whapps_call:account_id(Call)
                                           ),
    wh_json:get_value([<<"music_on_hold">>, <<"media_id">>], JObj).

-spec issue_transferee_event(ne_binary(), whapps_call:call()) -> 'ok'.
issue_transferee_event(Target, Call) ->
    API =
        [{<<"Event-Name">>, <<"CHANNEL_TRANSFEREE">>}
         ,{<<"Call-ID">>, whapps_call:call_id(Call)}
         ,{<<"DISPOSITION">>, <<"SUCCESS">>}
         ,{<<"Raw-Application-Name">>,<<"sofia::transferee">>}
         %%,{<<"Direction">>, whapps_call:direction(Call)}
         ,{<<"Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
         ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
         ,{<<"Callee-ID-Name">>, whapps_call:callee_id_name(Call)}
         ,{<<"Callee-ID-Number">>, whapps_call:callee_id_number(Call)}
         ,{<<"Other-Leg-Call-ID">>, whapps_call:other_leg_call_id(Call)}
         ,{<<"Custom-Channel-Vars">>, whapps_call:custom_channel_vars(Call)}
         ,{<<"Target-Call-ID">>, Target}
         | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
        ],
    wapi_call:publish_event(API).

-spec issue_internal_transferee(whapps_call:call(), api_binary(), ne_binary(), ne_binary()) -> 'ok'.
issue_internal_transferee(Call, Transferor, Transferee, Target) ->
    API =
        [{<<"Target">>, Target}
         ,{<<"Transferee">>, Transferee}
         ,{<<"Transferor">>, Transferor}
         ,{<<"Call">>, whapps_call:to_json(Call)}
         | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
        ],
    wapi_konami:publish_transferred(Target, API).

-spec hangup_target(whapps_call:call(), ne_binaries()) -> 'ok'.
hangup_target(Call, []) ->
    whapps_call_command:hangup(Call);
hangup_target(Call, [B|Bs]) ->
    Hangup = [{<<"Call-ID">>, B}
              ,{<<"Application-Name">>, <<"hangup">>}
              ,{<<"Insert-At">>, <<"now">>}
             ],
    whapps_call_command:send_command(Hangup, Call),
    hangup_target(Call, Bs).
