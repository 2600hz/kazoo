%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
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

-include("../konami.hrl").

%% -define(WSD_EVT(F, T, E), 'ok').
-define(WSD_EVT(F, T, E), webseq:evt(F, T, E)).

%%%-define(WSD_NOTE(W, D, N), 'ok').
-define(WSD_NOTE(W, D, N), webseq:note(W, D, N)).

%% -define(WSD_TITLE(T), 'ok').
-define(WSD_TITLE(T), webseq:title(T)).

-record(state, {transferor :: ne_binary()
                ,transferee :: ne_binary()
                ,target :: ne_binary()
                ,call :: whapps_call:call()
                ,target_call = whapps_call:new() :: whapps_call:call()
                ,takeback_dtmf :: ne_binary()
                ,transferor_dtmf = <<>> :: binary()
                ,ringback :: api_binary()
                ,moh :: api_binary()
                ,extension :: api_binary()
               }).
-type state() :: #state{}.

-define(DEFAULT_TAKEBACK_DTMF
        ,whapps_config:get(?CONFIG_CAT, [<<"transfer">>, <<"default_takeback_dtmf">>], <<"*1">>)
       ).

-define(DEFAULT_TARGET_TIMEOUT
        ,whapps_config:get_integer(?CONFIG_CAT, [<<"transfer">>, <<"default_target_timeout_ms">>], 20000)
       ).

-define(DEFAULT_RINGBACK, whapps_config:get(<<"ecallmgr">>, <<"default_ringback">>)).

-define(TRANSFEROR_CALL_EVENTS, [<<"CHANNEL_BRIDGE">>, <<"CHANNEL_UNBRIDGE">>
                                 ,<<"DTMF">>
                                 ,<<"CHANNEL_ANSWER">>
                                 ,<<"CHANNEL_DESTROY">>
                                 ,<<"dialplan">>
                                ]).

-define(TRANSFEREE_CALL_EVENTS, [<<"CHANNEL_BRIDGE">>, <<"CHANNEL_UNBRIDGE">>
                                 ,<<"CHANNEL_DESTROY">>
                                 ,<<"dialplan">>
                                ]).
-define(TARGET_CALL_EVENTS, [<<"CHANNEL_ANSWER">>
                             ,<<"CHANNEL_CREATE">>
                             ,<<"CHANNEL_BRIDGE">>
                             ,<<"CHANNEL_DESTROY">>
                             ,<<"DTMF">>, <<"CHANNEL_REPLACED">>
                             ,<<"dialplan">>
                            ]).

-spec handle(wh_json:object(), whapps_call:call()) -> no_return().
handle(Data, Call) ->
    Transferor = wh_json:get_value(<<"dtmf_leg">>, Data),
    Transferee =
        case whapps_call:call_id(Call) of
            Transferor -> whapps_call:other_leg_call_id(Call);
            CallId -> CallId
        end,

    lager:info("first, we need to receive call events for our two legs"),
    add_transferor_bindings(Transferor),
    add_transferee_bindings(Transferee),

    ?WSD_TITLE(["Transferee: ", Transferee, " and Transferor: ", Transferor]),

    lager:info("unbridge transferee ~s and transferor ~s", [Transferee, Transferor]),
    unbridge(Call),

    try gen_fsm:enter_loop(?MODULE, [], 'pre_originate'
                           ,#state{transferor=Transferor
                                   ,transferee=Transferee
                                   ,call=whapps_call:set_controller_queue(konami_event_listener:queue_name(), Call)
                                   ,takeback_dtmf=wh_json:get_value(<<"takeback_dtmf">>, Data, ?DEFAULT_TAKEBACK_DTMF)
                                   ,ringback=to_tonestream(wh_json:get_value(<<"ringback">>, Data, ?DEFAULT_RINGBACK))
                                   ,moh=find_moh(Data, Call)
                                   ,extension=get_extension(wh_json:get_first_defined([<<"captures">>, <<"target">>], Data))
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

-spec get_extension(ne_binaries() | ne_binary()) -> ne_binary().
get_extension([Ext|_]) -> Ext;
get_extension(<<_/binary>> = Ext) -> Ext.

pre_originate(?EVENT(UUID, <<"CHANNEL_UNBRIDGE">>, _Evt)
              ,#state{call=Call
                      ,moh=MOH
                      ,transferee=Transferee
                      ,transferor=Transferor
                      ,extension=Extension
                     }=State
             )
  when UUID =:= Transferee orelse UUID =:= Transferor ->
    MOH = wh_media_util:media_path(MOH, Call),
    lager:info("putting transferee ~s on hold with MOH ~s", [Transferee, MOH]),
    HoldCommand = whapps_call_command:hold_command(MOH, Transferee),
    whapps_call_command:send_command(HoldCommand, Call),

    lager:info("ok, now we need to originate to the requested number ~s", [Extension]),

    Target = originate_to_extension(Extension, Transferor, Call),
    lager:info("originating to ~s", [Target]),
    {'next_state', 'attended_wait', State#state{target=Target}};
pre_originate(?EVENT(_CallId, <<"CHANNEL_DESTROY">>, _Evt), State) ->
    lager:debug("~s has hungup before we originated, done here", [_CallId]),
    {'stop', 'normal', State};
pre_originate(?EVENT(_CallId, _EventName, _Evt), State) ->
    lager:debug("unhandled event ~s for ~s", [_EventName, _CallId]),
    {'next_state', 'pre_originate', State}.

pre_originate(_Msg, _From, State) ->
    {'next_state', 'partial_wait', State}.

attended_wait(?EVENT(Transferor, <<"DTMF">>, Evt), #state{transferor=Transferor}=State) ->
    handle_transferor_dtmf(Evt, 'attended_wait', State);
attended_wait(?EVENT(Transferee, EventName, _Evt)
              ,#state{transferee=Transferee}=State
             )
  when EventName =:= <<"CHANNEL_DESTROY">>
       orelse EventName =:= <<"LEG_DESTROYED">> ->
    lager:info("transferee ~s hungup before target could be reached", [Transferee]),
    lager:info("transferor and target are on their own"),
    ?WSD_NOTE(Transferee, 'right', <<"Transferee down in attended_wait">>),
    {'stop', 'normal', State};
attended_wait(?EVENT(Transferor, EventName, _Evt)
              ,#state{transferor=Transferor
                      ,target=Target
                      ,transferee=Transferee
                     }=State
             )
  when EventName =:= <<"CHANNEL_DESTROY">>
       orelse EventName =:= <<"LEG_DESTROYED">> ->
    lager:info("transferor ~s hungup, connecting transferee ~s and target ~s"
               ,[Transferor, Transferee, Target]
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
    OtherLeg = wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt),
    lager:info("transferor ~s bridged to ~s", [Transferor, OtherLeg]),
    case OtherLeg of
        Target ->
            lager:info("transferor and target are connected, moving to attended_answer"),
            ?WSD_EVT(Transferor, Target, <<"Bridged">>),
            {'next_state', 'attended_answer', State#state{
                                                target_call=whapps_call:exec(
                                                              [{fun whapps_call:set_call_id/2, Target}
                                                               ,{fun whapps_call:set_other_leg_call_id/2, Transferor}
                                                              ]
                                                              ,TargetCall
                                                             )
                                               }};
        Transferee ->
            lager:info("transferor and transferee have reconnected"),
            ?WSD_EVT(Transferor, Transferee, <<"Bridged">>),
            whapps_call_command:hangup(TargetCall),
            {'stop', 'normal', State}
    end;
attended_wait(?EVENT(Target, <<"CHANNEL_ANSWER">>, _Evt)
              ,#state{target=Target}=State
             ) ->
    lager:info("target ~s has answered", [Target]),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
              ,#state{target=Target
                      ,transferor=Transferor
                     }=State
             ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Transferor ->
            lager:info("recv CHANNEL_BRIDGE on target ~s to transferor ~s, moving to attended_answer"
                       ,[Target, Transferor]
                      ),
            ?WSD_EVT(Target, Transferor, <<"bridged">>),
            {'next_state', 'attended_answer', State};
        _CallId ->
            lager:info("recv CHANNEL_BRIDGE on target ~s to call id ~s", [Target, _CallId]),
            ?WSD_EVT(Target, _CallId, <<"unknown bridged">>),
            {'next_state', 'attended_wait', State}
    end;
attended_wait(?EVENT(Target, <<"originate_uuid">>, Evt)
              ,#state{target=Target
                      ,target_call=TargetCall
                     }=State
             ) ->
    lager:info("recv control for target ~s", [Target]),
    TargetCall1 = whapps_call:from_originate_uuid(Evt, TargetCall),
    ?WSD_NOTE(Target, 'right', <<"control for target recv">>),
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
            ?WSD_EVT(Transferor, Target, <<"bridge">>),
            lager:info("transferor and target are connected, moving to attended_answer"),
            {'next_state', 'attended_answer', State};
        Transferee ->
            ?WSD_EVT(Transferor, Transferee, <<"bridge">>),
            lager:info("transferor and transferee have reconnected"),
            whapps_call_command:hangup(TargetCall),
            {'stop', 'normal', State};
        _CallId ->
            ?WSD_EVT(Transferor, _CallId, <<"unknown bridge">>),
            lager:info("transferor ~s bridged to ~s, moving to attended_answer"
                       ,[Transferor, _CallId]
                      ),
            {'next_state', 'attended_answer', State}
    end;
attended_wait(?EVENT(Target, <<"CHANNEL_CREATE">>, _Evt)
              ,#state{target=Target
                      ,transferor=Transferor
                      ,ringback=Ringback
                      ,call=Call
                     }=State
             ) ->
    lager:info("transfer target ~s channel created", [Target]),
    ?WSD_NOTE(Target, 'right', <<"created">>),
    maybe_start_transferor_ringback(Call, Transferor, Ringback),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(Target, <<"originate_resp">>, _Evt), State) ->
    lager:info("originate has responded for target ~s", [Target]),
    ?WSD_NOTE(Target, 'right', <<"originated">>),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(Target, EventName, _Evt)
              ,#state{target=Target
                      ,call=Call
                     }=State
             )
  when EventName =:= <<"CHANNEL_DESTROY">>
       orelse EventName =:= <<"LEG_DESTROYED">> ->
    lager:info("target ~s didn't answer, reconnecting transferor and transferee", [Target]),
    ?WSD_NOTE(Target, 'right', <<"targets down">>),
    connect_to_transferee(Call),
    {'stop', 'normal', State};
attended_wait(?EVENT(Target, <<"CHANNEL_REPLACED">>, Evt)
              ,#state{target=Target
                      ,target_call=TargetCall
                      ,transferor=Transferor
                     }=State
             ) ->
    ReplacementId = wh_json:get_value(<<"Replaced-By">>, Evt),
    lager:debug("target ~s being replaced by ~s", [Target, ReplacementId]),
    ?WSD_EVT(Target, ReplacementId, <<"replaced">>),

    konami_event_listener:rm_call_binding(Target, ?TARGET_CALL_EVENTS),

    TargetCall1 = whapps_call:set_call_id(ReplacementId, TargetCall),
    connect_transferor_to_target(Transferor, TargetCall1),
    {'next_state', 'attended_wait', State#state{target=ReplacementId
                                                ,target_call=TargetCall1
                                               }};
attended_wait(?EVENT(_CallId, _EventName, _Evt)
              ,#state{transferor=_Transferor
                      ,transferee=_Transferee
                      ,target=_Target
                     }=State) ->
    lager:info("attended_wait: unhandled event ~s for ~s: ~p", [_EventName, _CallId, _Evt]),
    lager:debug("transferor: ~s transferee: ~s target: ~s", [_Transferor, _Transferee, _Target]),
    ?WSD_NOTE(_CallId, 'right', <<"unhandled aw ", _EventName/binary>>),
    {'next_state', 'attended_wait', State};
attended_wait(Msg, State) ->
    lager:info("attended_wait: unhandled msg ~p", [Msg]),
    {'next_state', 'attended_wait', State}.

attended_wait(_Msg, _From, State) ->
    {'next_state', 'attended_wait', State}.

partial_wait(?EVENT(Transferee, <<"CHANNEL_BRIDGE">>, Evt)
             ,#state{transferee=Transferee
                     ,target=Target
                    }=State
            ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Target ->
            lager:debug("transfreee has bridged to target ~s", [Target]),
            {'next_state', 'finished', State};
        _OID ->
            lager:debug("transferee has bridged to unknown ~s", [_OID]),
            {'stop', 'normal', State}
    end;
partial_wait(?EVENT(Transferee, EventName, _Evt)
             ,#state{transferee=Transferee
                     ,target_call=TargetCall
                    }=State
            )
  when EventName =:= <<"CHANNEL_DESTROY">>
       orelse EventName =:= <<"LEG_DESTROYED">> ->
    lager:info("transferee ~s hungup while target was being rung", [Transferee]),
    whapps_call_command:hangup(TargetCall),
    {'stop', 'normal', State};
partial_wait(?EVENT(Transferor, EventName, _Evt)
             ,#state{transferor=Transferor}=State
            )
  when EventName =:= <<"CHANNEL_DESTROY">>
       orelse EventName =:= <<"LEG_DESTROYED">> ->
    lager:info("transferor ~s hungup, still waiting on target and transferee", [Transferor]),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(Transferor, <<"LEG_DESTROYED">>, _Evt)
             ,#state{transferor=Transferor}=State
            ) ->
    lager:info("transferor ~s hungup, still waiting on target and transferee", [Transferor]),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(Target, EventName, _Evt)
             ,#state{target=Target
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
partial_wait(?EVENT(Target, <<"CHANNEL_ANSWER">>, _Evt)
             ,#state{target=Target}=State
            ) ->
    lager:info("target ~s has answered", [Target]),
    {'next_state', 'partial_wait', State};
partial_wait(?EVENT(Target, <<"originate_uuid">>, Evt)
             ,#state{target=Target
                     ,target_call=TargetCall
                    }=State
            ) ->
    lager:info("recv control for target ~s", [Target]),
    ?WSD_NOTE(Target, 'right', <<"control for target recv">>),
    {'next_state', 'partial_wait', State#state{target_call=whapps_call:from_originate_uuid(Evt, TargetCall)}};
partial_wait(?EVENT(Target, <<"CHANNEL_REPLACED">>, Evt)
             ,#state{target=Target
                     ,target_call=TargetCall
                     ,transferor=Transferor
                     ,transferee=Transferee
                     ,call=Call
                    }=State
            ) ->
    ReplacementId = wh_json:get_value(<<"Replaced-By">>, Evt),
    lager:debug("target ~s being replaced by ~s", [Target, ReplacementId]),
    ?WSD_EVT(Target, ReplacementId, <<"replaced">>),

    konami_event_listener:rm_call_binding(Target, ?TARGET_CALL_EVENTS),

    TargetCall1 = whapps_call:set_call_id(ReplacementId, TargetCall),
    connect_transferee_to_target(Target, Call, 'true'),
    issue_internal_transferee(Call, Transferor, Transferee, Target),

    {'next_state', 'attended_wait', State#state{target=ReplacementId
                                                ,target_call=TargetCall1
                                               }};
partial_wait(?EVENT(_CallId, _EventName, _Evt), State) ->
    lager:info("partial_wait: unhandled event ~s for ~s: ~p", [_EventName, _CallId, _Evt]),
    ?WSD_NOTE(_CallId, 'right', <<"unhandled pw ", _EventName/binary>>),
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
            lager:info("transferor and target ~s are connected", [Target]),
            ?WSD_EVT(Transferor, Target, <<"transferor bridged to target">>),
            {'next_state', 'attended_answer', State};
        Transferee ->
            ?WSD_EVT(Transferor, Target, <<"transferor bridged to transferee">>),
            lager:info("transferor and transferee have reconnected"),
            whapps_call_command:hangup(TargetCall),
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
    TargetCall1 = whapps_call:from_originate_uuid(Evt, TargetCall),
    {'next_state', 'attended_answer', State#state{target_call=TargetCall1}};
attended_answer(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
                ,#state{transferor=Transferor
                        ,target=Target
                       }=State
               ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Transferor ->
            lager:info("transferor and target are connected"),
            ?WSD_EVT(Target, Transferor, <<"bridged">>),
            {'next_state', 'attended_answer', State};
        _CallId ->
            lager:info("target ~s bridged to ~s", [Target, _CallId]),
            ?WSD_EVT(Target, _CallId, <<"target bridged to unknown">>),
            {'next_state', 'attended_answer', State}
    end;
attended_answer(?EVENT(Transferee, EventName, _Evt)
                ,#state{transferee=Transferee}=State
               )
  when EventName =:= <<"CHANNEL_DESTROY">>;
       EventName =:= <<"LEG_DESTROYED">>
       ->
    lager:info("transferee ~s hungup while transferor and target were talking", [Transferee]),
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
    lager:info("transferor ~s hungup, connecting transferee ~s and target ~s"
               ,[Transferor, Transferee, Target]
              ),
    ?WSD_NOTE(Transferor, 'right', <<"channel done">>),
    {Leg, Call} = how_to_transfer(OriginalCall, TargetCall, Transferor, Target, Transferee),
    connect_transferee_to_target(Leg, Call, 'true'),
    issue_internal_transferee(Call, Transferor, Transferee, Target),
    {'next_state', 'finished', State};
attended_answer(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target=Target
                      ,transferor=Transferor
                      ,transferee=Transferee
                      ,call=Call
                     }=State
             ) ->
    lager:info("target ~s hungup, reconnecting transferor ~s to transferee ~s"
                ,[Target, Transferor, Transferee]
               ),
    ?WSD_NOTE(Target, 'right', <<"target done">>),

    connect_to_transferee(Call),
    {'stop', 'normal', State};
attended_answer(?EVENT(_CallId, _EventName, _Evt), State) ->
    lager:info("attended_answer: unhandled event ~s for ~s: ~p", [_EventName, _CallId, _Evt]),
    ?WSD_NOTE(_CallId, 'right', <<"unhandled aa ", _EventName/binary>>),
    {'next_state', 'attended_answer', State};
attended_answer(Msg, State) ->
    lager:info("attended_answer: unhandled msg ~p", [Msg]),
    {'next_state', 'attended_answer', State}.

attended_answer(_Msg, _From, State) ->
    {'next_state', 'attended_answer', State}.

finished(?EVENT(Transferee, <<"CHANNEL_BRIDGE">>, Evt)
         ,#state{transferee=Transferee
                 ,target=Target
                }=State
        ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Target ->
            lager:debug("transferee and target are bridged"),
            {'stop', 'normal', State};
        _CallId ->
            lager:debug("transferee bridged to unknown callid ~s", [_CallId]),
            {'next_state', 'finished', State, 5000}
    end;
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
            {'next_state', 'finished', State, 5000}
    end;
finished(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
         ,#state{target=Target}=State
        ) ->
    lager:info("target ~s has hungup", [Target]),
    {'stop', 'normal', State};
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
finished(?EVENT(_CallId, _EventName, _Evt)
         ,State
        ) ->
    lager:info("unhandled event ~s for ~s", [_EventName, _CallId]),
    ?WSD_NOTE(_CallId, 'right', <<"unhandled f ", _EventName/binary>>),
    {'next_state', 'finished', State};
finished('timeout', State) ->
    lager:info("haven't received anything in a while, going down"),
    {'stop', 'normal', State};
finished(_Msg, State) ->
    lager:info("unhandled message ~p", [_Msg]),
    {'next_state', 'finished', State, 5000}.

finished(_Req, _From, State) ->
    {'next_state', 'finished', State}.

takeback(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
         ,#state{transferor=Transferor
                 ,target=Target
                 ,target_call=TargetCall
                }=State
        ) ->
    _OtherLeg = wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt),
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
    _OtherLeg = wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt),
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
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Transferor ->
            lager:debug("target ~s is bridged to transferor ~s while in takeback"
                        ,[Target, Transferor]
                       ),
            ?WSD_EVT(Target, Transferor, <<"bridged, need takeback again">>),
            whapps_call_command:hangup(TargetCall);
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
    lager:debug("target ~s has ended", [Target]),
    connect_to_transferee(Call),
    {'next_state', 'takeback', State, 5000};
takeback(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
         ,#state{transferor=Transferor}=State
        ) ->
    lager:debug("transferor ~s has ended", [Transferor]),
    {'stop', 'normal', State};
takeback(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
         ,#state{transferee=Transferee}=State
        ) ->
    lager:debug("transferee ~s has ended", [Transferee]),
    {'stop', 'normal', State};
takeback(?EVENT(_CallId, _EventName, _Evt)
         ,State
        ) ->
    lager:debug("unhandled event for ~s: ~s", [_CallId, _EventName]),
    ?WSD_NOTE(_CallId, 'right', <<"unhandled t ", _EventName/binary>>),
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

    CCVs = props:filter_undefined(
             [{<<"Account-ID">>, whapps_call:account_id(Call)}
              ,{<<"Authorizing-ID">>, whapps_call:account_id(Call)}
              ,{<<"Channel-Authorized">>, 'true'}
              ,{<<"From-URI">>, <<CallerIdNumber/binary, "@", (whapps_call:account_realm(Call))/binary>>}
              ,{<<"Ignore-Early-Media">>, 'true'}
             ]),

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
                    ,{<<"Ignore-Early-Media">>, 'true'}
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
    ?WSD_NOTE(TargetCallId, 'right', <<"originating to target ", Extension/binary>>),
    konami_event_listener:originate(Request),
    TargetCallId.

-spec create_call_id() -> ne_binary().
create_call_id() ->
    TargetCallId = <<"konami-transfer-", (wh_util:rand_hex_binary(4))/binary>>,
    bind_target_call_events(TargetCallId),
    TargetCallId.

-spec bind_target_call_events(ne_binary()) -> 'ok'.
bind_target_call_events(CallId) ->
    konami_event_listener:add_call_binding(CallId, ?TARGET_CALL_EVENTS).

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
             ,{<<"B-Leg-Events">>, ?TARGET_CALL_EVENTS}
            ],
    lager:debug("connecting transferee to target ~s, hangup is ~s", [Target, Hangup]),
    konami_util:listen_on_other_leg(Call, ?TARGET_CALL_EVENTS),
    connect(Flags, Call).

-spec connect_transferor_to_target(ne_binary(), whapps_call:call()) -> 'ok'.
connect_transferor_to_target(Transferor, TargetCall) ->
    Flags = [{<<"Target-Call-ID">>, Transferor}
             ,{<<"Continue-On-Fail">>, 'true'}
             ,{<<"Continue-On-Cancel">>, 'true'}
             ,{<<"Park-After-Pickup">>, 'true'}
             ,{<<"Publish-Usurp">>, 'false'}
             ,{<<"B-Leg-Events">>, ?TRANSFEROR_CALL_EVENTS}
            ],
    konami_util:listen_on_other_leg(TargetCall, ?TRANSFEROR_CALL_EVENTS),
    lager:debug("connecting transferor to target ~s, hangup is ~s", [whapps_call:call_id(TargetCall), 'false']),
    connect(Flags, TargetCall).

-spec connect_to_transferee(whapps_call:call()) -> 'ok'.
connect_to_transferee(Call) ->
    Flags = [{<<"Target-Call-ID">>, whapps_call:other_leg_call_id(Call)}
             ,{<<"Continue-On-Fail">>, 'false'}
             ,{<<"Continue-On-Cancel">>, 'false'}
             ,{<<"Park-After-Pickup">>, 'false'}
             ,{<<"B-Leg-Events">>, ?TRANSFEROR_CALL_EVENTS}
            ],
    lager:debug("reconnecting transferor/ee: ~s and ~s, hangup is "
                ,[whapps_call:call_id(Call)
                  ,whapps_call:other_leg_call_id(Call)
                  ,'true'
                 ]),
    connect(Flags, Call).

-spec connect(wh_proplist(), whapps_call:call()) -> 'ok'.
connect(Flags, Call) ->
    Command = [{<<"Application-Name">>, <<"connect_leg">>}
               ,{<<"Call-ID">>, whapps_call:call_id(Call)}
               ,{<<"Insert-At">>, <<"now">>}
               | Flags
              ],
    ?WSD_EVT(props:get_value(<<"Call-ID">>, Command)
               ,props:get_value(<<"Target-Call-ID">>, Command)
               ,<<"Connect Legs">>
              ),
    whapps_call_command:send_command(Command, Call).

-type dtmf_next_state() :: 'attended_wait' | 'attended_answer' | 'takeback'.
-spec handle_transferor_dtmf(wh_json:object(), dtmf_next_state(), state()) ->
                                    {'stop', 'normal', state()} |
                                    {'next_state', dtmf_next_state(), state()}.
handle_transferor_dtmf(Evt
                       ,NextState
                       ,#state{target_call=TargetCall
                               ,takeback_dtmf=TakebackDTMF
                               ,transferor_dtmf=DTMFs
                               ,transferor=_Transferor
                               ,transferee=_Transferee
                              }=State
                      ) ->
    Digit = wh_json:get_value(<<"DTMF-Digit">>, Evt),
    lager:info("recv transferor dtmf '~s', adding to '~s'", [Digit, DTMFs]),

    Collected = <<DTMFs/binary, Digit/binary>>,

    case wh_util:suffix_binary(TakebackDTMF, Collected) of
        'true' ->
            lager:info("takeback dtmf sequence (~s) engaged!", [TakebackDTMF]),
            ?WSD_NOTE(_Transferor, 'right', <<"takeback sequence engaged">>),
            whapps_call_command:hangup(TargetCall),
            {'next_state', 'takeback', State};
        'false' ->
            {'next_state', NextState, State#state{transferor_dtmf=Collected}}
    end.

-spec unbridge(whapps_call:call()) -> 'ok'.
-spec unbridge(whapps_call:call(), ne_binary()) -> 'ok'.
unbridge(Call) ->
    unbridge(Call, whapps_call:call_id(Call)).
unbridge(Call, CallId) ->
    Command = [{<<"Application-Name">>, <<"unbridge">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Leg">>, <<"Both">>}
               ,{<<"Call-ID">>, CallId}
              ],
    ?WSD_NOTE(CallId, 'right', <<"unbridging">>),
    whapps_call_command:send_command(Command, Call).

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

%% We need to figure out which whapps call to issue the connect_leg against
%% When the A-leg is the transferor, its ecallmgr control will be down at this
%% point so use Target's call record; otherwise use the A-leg's.
-spec how_to_transfer(whapps_call:call(), whapps_call:call(), ne_binary(), ne_binary(), ne_binary()) ->
                             {ne_binary(), whapps_call:call()}.
how_to_transfer(OriginalCall, TargetCall, Transferor, Target, Transferee) ->
    case whapps_call:call_id(OriginalCall) of
        Transferor ->
            lager:debug("transferor ~s is in control, but has died; use target call", [Transferor]),
            konami_util:listen_on_other_leg(TargetCall, [<<"DTMF">>, <<"CHANNEL_BRIDGE">>]),
            {Transferee, TargetCall};
        Transferee ->
            lager:debug("transferee ~s is in control, use original call", [Transferee]),
            {Target, OriginalCall}
    end.

-spec hangup_target(whapps_call:call()) -> 'ok'.
hangup_target(Call) ->
    whapps_call_command:hangup(Call).

-spec to_tonestream(api_binary()) -> api_binary().
to_tonestream('undefined') -> 'undefined';
to_tonestream(<<"tone_stream://", _/binary>> = TS) -> <<TS/binary, ";loops=-1">>;
to_tonestream(Ringback) -> <<"tone_stream://", Ringback/binary, ";loops=-1">>.

-spec maybe_start_transferor_ringback(whapps_call:call(), ne_binary(), api_binary()) -> 'ok'.
maybe_start_transferor_ringback(_Call, _Transferor, 'undefined') -> 'ok';
maybe_start_transferor_ringback(Call, Transferor, Ringback) ->
    Command = whapps_call_command:play_command(Ringback, Transferor),
    lager:debug("playing ringback on ~s to ~s", [Transferor, Ringback]),
    whapps_call_command:send_command(wh_json:set_values([{<<"Insert-At">>, <<"now">>}], Command), Call).
