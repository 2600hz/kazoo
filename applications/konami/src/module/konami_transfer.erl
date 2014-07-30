%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Transfers caller to the extension extracted in the regex
%%% Data = {
%%%   "takeback_dtmf":"2" // Transferor can cancel the transfer request
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_transfer).

-behaviour(gen_fsm).

-export([handle/2]).

-export([attended_wait/2, attended_wait/3
         ,partial_wait/2, partial_wait/3
         ,attended_answer/2, attended_answer/3
         ,transfer/2, transfer/3
         ,cancel/2, cancel/3

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
                ,call :: whapps_call:call()
                ,target_call = whapps_call:new() :: whapps_call:call() | 'undefined'
               }).

-spec handle(wh_json:object(), whapps_call:call()) ->
                    no_return().
handle(Data, Call) ->
    TransferorLeg = wh_json:get_value(<<"dtmf_leg">>, Data),
    TransfereeLeg =
        case whapps_call:call_id(Call) of
            TransferorLeg -> whapps_call:other_leg_call_id(Call);
            CallId -> CallId
        end,

    lager:debug("first, we need to receive call events for our two legs"),
    add_transferor_bindings(TransferorLeg),
    add_transferee_bindings(TransfereeLeg),

    lager:debug("unbridge and put transferee ~s into park", [TransfereeLeg]),
    whapps_call_command:unbridge(Call),
    %% ParkCommand = whapps_call_command:park_command(TransfereeLeg),
    %% whapps_call_command:send_command(ParkCommand, Call),

    [Extension|_] = wh_json:get_value(<<"captures">>, Data),
    lager:debug("ok, now we need to originate to the requested number ~s", [Extension]),

    Target = originate_to_extension(Extension, TransferorLeg, Call),
    lager:debug("originating to ~s", [Target]),

    try gen_fsm:enter_loop(?MODULE, [], 'attended_wait'
                           ,#state{transferor=TransferorLeg
                                   ,transferee=TransfereeLeg
                                   ,target=Target
                                   ,call=Call
                                  }
                          )
    of
        _ -> 'ok'
    catch
        'exit':'normal' -> 'ok';
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("FSM terminated abnormally: ~s: ~p", [_E, _R]),
            wh_util:log_stacktrace(ST)
    end.

attended_wait(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{transferee=Transferee}=State
             ) ->
    lager:debug("transferee ~s hungup before target could be reached"),
    lager:debug("transferor and target are on their own"),
    {'stop', 'normal', State};
attended_wait(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{transferor=Transferor}=State
             ) ->
    lager:debug("transferor ~s hungup, going to a partial transfer", [Transferor]),
    {'next_state', 'partial_wait', State};
attended_wait(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target=Target
                      ,call=Call
                     }=State
             ) ->
    lager:debug("target ~s didn't answer, reconnecting", [Target]),
    connect_to_target(Call),
    {'stop', 'normal', State};
attended_wait(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
              ,#state{transferor=Transferor
                      ,transferee=Transferee
                      ,target=Target
                     }=State
             ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Target ->
            lager:debug("transferor and target are connected"),
            {'next_state', 'attended_answer', State};
        Transferee ->
            lager:debug("transferor and transferee have reconnected"),
            {'stop', 'normal', State}
    end;
attended_wait(?EVENT(Target, <<"CHANNEL_ANSWER">>, _Evt)
              ,#state{transferor=Transferor
                      ,target=Target
                      ,target_call=TargetCall
                     }=State
             ) ->
    lager:debug("target ~s has answered, connect to transferor ~s", [Target, Transferor]),
    lager:debug("target ctrl ~s", [whapps_call:control_queue(TargetCall)]),
    connect_to_target(Transferor, TargetCall),
    {'next_state', 'attended_answer', State};
attended_wait(?EVENT(Target, <<"originate_uuid">>, Evt)
              ,#state{target=Target
                      ,target_call=TargetCall
                     }=State
             ) ->
    lager:debug("recv control for target ~s", [Target]),
    {'next_state', 'attended_wait', State#state{target_call=whapps_call:from_originate_uuid(Evt, TargetCall)}};
attended_wait(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
              ,#state{transferor=Transferor
                      ,transferee=Transferee
                      ,target=Target
                     }=State
             ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Target ->
            lager:debug("transferor and target are connected"),
            {'next_state', 'attended_answer', State};
        Transferee ->
            lager:debug("transferor and transferee have reconnected"),
            {'stop', 'normal', State};
        _CallId ->
            lager:debug("transferor ~s bridged to ~s", [Transferor, _CallId]),
            {'next_state', 'attended_answer', State}
    end;
attended_wait(?EVENT(_CallId, _EventName, _Evt), State) ->
    lager:debug("attanded_wait: unhandled event ~s for ~s: ~p", [_EventName, _CallId, _Evt]),
    {'next_state', 'attended_wait', State};
attended_wait(Msg, State) ->
    lager:debug("attended_wait: unhandled msg ~p", [Msg]),
    {'next_state', 'attended_wait', State}.

attended_wait(Msg, From, State) ->
    lager:debug("attended_wait: unhandled msg from ~p: ~p", [From, Msg]),
    {'reply', {'error', 'not_implemented'}, 'attended_wait', State}.

partial_wait(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{transferee=Transferee}=State
            ) ->
    lager:debug("transferee ~s hungup while transferor and target were talking"),
    lager:debug("transferor and target are on their own"),
    {'stop', 'normal', State};
partial_wait(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{transferor=Transferor
                     ,transferee=Transferee
                     ,target=Target
                     ,target_call=TargetCall
                    }=State
            ) ->
    lager:debug("transferor ~s hungup, connected transferee ~s and target ~s"
                ,[Transferor, Transferee, Target]
               ),
    connect_to_target(Transferee, TargetCall),
    {'stop', 'normal', State};
partial_wait(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
             ,#state{target=Target
                     ,transferor=_Transferor
                     ,transferee=_Transferee
                     ,call=Call
                    }=State
            ) ->
    lager:debug("target ~s hungup, reconnecting transferor ~s to transferee ~s"
                ,[Target, _Transferor, _Transferee]
               ),
    connect_to_target(Call),
    {'stop', 'normal', State};
partial_wait(?EVENT(Target, <<"CHANNEL_ANSWER">>, _Evt)
             ,#state{transferee=Transferee
                     ,target=Target
                     ,target_call=TargetCall
                    }=State
            ) ->
    lager:debug("target ~s has answered, connect to transferee ~s", [Target, Transferee]),
    lager:debug("target ctrl ~s", [whapps_call:control_queue(TargetCall)]),
    connect_to_target(Transferee, TargetCall),
    {'stop', 'normal', State};
partial_wait(?EVENT(Target, <<"originate_uuid">>, Evt)
             ,#state{target=Target
                     ,target_call=TargetCall
                    }=State
            ) ->
    lager:debug("recv control for target ~s", [Target]),
    {'next_state', 'partial_wait', State#state{target_call=whapps_call:from_originate_uuid(Evt, TargetCall)}};

partial_wait(?EVENT(_CallId, _EventName, _Evt), State) ->
    lager:debug("partial_wait: unhandled event ~s for ~s: ~p", [_EventName, _CallId, _Evt]),
    {'next_state', 'partial_wait', State};
partial_wait(Msg, State) ->
    lager:debug("partial_wait: unhandled msg ~p", [Msg]),
    {'next_state', 'partial_wait', State}.

partial_wait(Msg, From, State) ->
    lager:debug("partial_wait: unhandled msg from ~p: ~p", [From, Msg]),
    {'reply', {'error', 'not_implemented'}, 'partial_wait', State}.

attended_answer(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
                ,#state{transferor=Transferor
                        ,transferee=Transferee
                        ,target=Target
                       }=State
               ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Target ->
            lager:debug("transferor and target are connected"),
            {'next_state', 'attended_answer', State};
        Transferee ->
            lager:debug("transferor and transferee have reconnected"),
            {'stop', 'normal', State};
        _CallId ->
            lager:debug("transferor ~s bridged to ~s", [Transferor, _CallId]),
            {'next_state', 'attended_answer', State}
    end;
attended_answer(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
                ,#state{transferee=Transferee}=State
               ) ->
    lager:debug("transferee ~s hungup while transferor and target were talking"),
    lager:debug("transferor and target are on their own"),
    {'stop', 'normal', State};
attended_answer(?EVENT(Transferor, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{transferor=Transferor
                      ,transferee=Transferee
                      ,target=Target
                      ,target_call=TargetCall
                     }=State
             ) ->
    lager:debug("transferor ~s hungup, connecting transferee ~s and target ~s"
                ,[Transferor, Transferee, Target]
               ),
    lager:debug("target ctrl ~s", [whapps_call:control_queue(TargetCall)]),
    connect_to_target(Transferee, TargetCall),
    {'stop', 'normal', State};
attended_answer(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target=Target
                      ,transferor=_Transferor
                      ,transferee=Transferee
                      ,call=Call
                     }=State
             ) ->
    lager:debug("target ~s hungup, reconnecting transferor ~s to transferee ~s"
                ,[Target, _Transferor, Transferee]
               ),
    connect_to_target(Transferee, Call),
    {'stop', 'normal', State};
attended_answer(?EVENT(_CallId, _EventName, _Evt), State) ->
    lager:debug("attanded_answer: unhandled event ~s for ~s: ~p", [_EventName, _CallId, _Evt]),
    {'next_state', 'attended_answer', State};
attended_answer(Msg, State) ->
    lager:debug("attended_answer: unhandled msg ~p", [Msg]),
    {'next_state', 'attended_answer', State}.

attended_answer(Msg, From, State) ->
    lager:debug("attended_answer: unhandled msg from ~p: ~p", [From, Msg]),
    {'reply', {'error', 'not_implemented'}, 'attended_answer', State}.

transfer(Msg, State) ->
    lager:debug("transfer: unhandled msg ~p", [Msg]),
    {'next_state', 'transfer', State}.

transfer(Msg, From, State) ->
    lager:debug("transfer: unhandled msg from ~p: ~p", [From, Msg]),
    {'reply', {'error', 'not_implemented'}, 'transfer', State}.

cancel(Msg, State) ->
    lager:debug("cancel: unhandled msg ~p", [Msg]),
    {'next_state', 'cancel', State}.

cancel(Msg, From, State) ->
    lager:debug("cancel: unhandled msg from ~p: ~p", [From, Msg]),
    {'reply', {'error', 'not_implemented'}, 'cancel', State}.

handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

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
    lager:debug("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

terminate(_Reason, _StateName, #state{transferor=Transferor
                                      ,transferee=Transferee
                                      ,target=Target
                                     }) ->
    konami_event_listener:rm_call_binding(Transferor),
    konami_event_listener:rm_call_binding(Transferee),
    konami_event_listener:rm_call_binding(Target),
    lager:debug("fsm terminating while in ~s: ~p", [_StateName, _Reason]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

init(_) -> 'ok'.

-spec add_transferor_bindings(ne_binary()) -> 'ok'.
add_transferor_bindings(CallId) ->
    konami_event_listener:add_call_binding(CallId, ['CHANNEL_DESTROY'
                                                    ,'CHANNEL_BRIDGE'
                                                    ,'DTMF'
                                                   ]).

-spec add_transferee_bindings(ne_binary()) -> 'ok'.
add_transferee_bindings(CallId) ->
    konami_event_listener:add_call_binding(CallId, ['CHANNEL_DESTROY'
                                                    ,'CHANNEL_BRIDGE'
                                                   ]).

-spec originate_to_extension(ne_binary(), ne_binary(), whapps_call:call()) -> ne_binary().
originate_to_extension(Extension, TransferorLeg, Call) ->
    %% don't forget to usurp the callflow exe for the call if C-leg answers and transfers
    MsgId = wh_util:rand_hex_binary(4),

    CCVs = [{<<"Account-ID">>, whapps_call:account_id(Call)}
            ,{<<"Auto-Answer">>, 'true'}
            ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
            ,{<<"Authorizing-Type">>, <<"device">>}
           ],

    TargetCallId = create_call_id(),

    Endpoint = wh_json:from_list(
                 [{<<"Invite-Format">>, <<"loopback">>}
                  ,{<<"Route">>,  Extension}
                  ,{<<"To-DID">>, Extension}
                  ,{<<"To-Realm">>, whapps_call:account_realm(Call)}
                  ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
                  ,{<<"Outbound-Call-ID">>, TargetCallId}
                 ]),

    Request = props:filter_undefined(
                [{<<"Endpoints">>, [Endpoint]}
                 ,{<<"Outbound-Call-ID">>, TargetCallId}
                 ,{<<"Dial-Endpoint-Method">>, <<"single">>}
                 ,{<<"Msg-ID">>, MsgId}
                 ,{<<"Continue-On-Fail">>, 'true'}
                 ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
                 ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
                 ,{<<"Application-Name">>, <<"park">>}
                 ,{<<"Timeout">>, 20000}

                 ,{<<"Outbound-Caller-ID-Name">>, caller_id_name(Call, TransferorLeg)}
                 ,{<<"Outbound-Caller-ID-Number">>, caller_id_number(Call, TransferorLeg)}
                 ,{<<"Caller-ID-Name">>, caller_id_name(Call, TransferorLeg)}
                 ,{<<"Caller-ID-Number">>, caller_id_number(Call, TransferorLeg)}

                 ,{<<"Existing-Call-ID">>, TransferorLeg}
                 ,{<<"Resource-Type">>, <<"originate">>}
                 ,{<<"Originate-Immediate">>, 'true'}
                 | wh_api:default_headers(konami_event_listener:queue_name(), ?APP_NAME, ?APP_VERSION)
                ]),

    wh_amqp_worker:cast(Request
                        ,fun wapi_resource:publish_originate_req/1
                       ),
    TargetCallId.

create_call_id() ->
    TargetCallId = <<"konami-transfer-", (wh_util:rand_hex_binary(4))/binary>>,
    konami_event_listener:add_call_binding(TargetCallId, ['CHANNEL_ANSWER'
                                                          ,'CHANNEL_DESTROY'
                                                          ,'CHANNEL_CREATE'
                                                         ]),
    TargetCallId.

-spec caller_id_name(whapps_call:call(), ne_binary()) -> ne_binary().
caller_id_name(Call, CallerLeg) ->
    case whapps_call:call_id(Call) of
        CallerLeg -> whapps_call:caller_id_name(Call);
        _CalleeLeg -> whapps_call:callee_id_name(Call)
    end.

-spec caller_id_number(whapps_call:call(), ne_binary()) -> ne_binary().
caller_id_number(Call, CallerLeg) ->
    case whapps_call:call_id(Call) of
        CallerLeg -> whapps_call:caller_id_number(Call);
        _CalleeLeg -> whapps_call:callee_id_number(Call)
    end.

-spec connect_to_target(whapps_call:call()) -> 'ok'.
-spec connect_to_target(ne_binary(), whapps_call:call()) -> 'ok'.
connect_to_target(Call) ->
    connect_to_target(whapps_call:other_leg_call_id(Call), Call).
connect_to_target(Leg, Call) ->
    Command = [{<<"Application-Name">>, <<"connect_leg">>}
               ,{<<"Call-ID">>, whapps_call:call_id(Call)}
               ,{<<"Target-Call-ID">>, Leg}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Continue-On-Fail">>, 'true'}
               ,{<<"Continue-On-Cancel">>, 'true'}
               ,{<<"Park-After-Pickup">>, 'true'}
              ],
    whapps_call_command:send_command(Command, Call).
