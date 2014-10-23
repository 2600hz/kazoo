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

-spec handle(wh_json:object(), whapps_call:call()) -> no_return().
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

    lager:debug("unbridge and put transferee ~s into hold", [TransfereeLeg]),
    whapps_call_command:unbridge(Call),

    MOH = wh_media_util:media_path(wh_json:get_value(<<"moh">>, Data), Call),
    lager:debug("putting transferee ~s on hold with MOH ~s", [TransfereeLeg, MOH]),
    HoldCommand = whapps_call_command:hold_command(MOH, TransfereeLeg),
    whapps_call_command:send_command(HoldCommand, Call),

    Extension =
        case wh_json:get_first_defined([<<"captures">>, <<"target">>], Data) of
            [Ext|_] -> Ext;
            <<_/binary>> = Ext -> Ext
        end,

    lager:debug("ok, now we need to originate to the requested number ~s", [Extension]),

    Target = originate_to_extension(Extension, TransferorLeg, Call),
    lager:debug("originating to ~s", [Target]),

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
            lager:debug("FSM terminated abnormally: ~s: ~p", [_E, _R]),
            wh_util:log_stacktrace(ST)
    end.

attended_wait(?EVENT(Transferor, <<"DTMF">>, Evt), #state{transferor=Transferor}=State) ->
    handle_transferor_dtmf(Evt, 'attended_wait', State);
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
attended_wait(?EVENT(Target, <<"LEG_CREATED">>, Evt)
              ,#state{target=Target
                      ,target_b_legs=Bs
                     }=State
             ) ->
    BLeg = wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt),
    case lists:member(BLeg, Bs) of
        'true' -> {'next_state', 'attended_wait', State};
        'false' ->
            lager:debug("new leg on target ~s: ~s", [Target, BLeg]),
            add_transferor_bindings(BLeg),
            {'next_state', 'attended_wait', State#state{target_b_legs=[BLeg | Bs]}}
    end;
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
    lager:debug("transferor ~s bridged to ~s", [Transferor, wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt)]),
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
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
              ,#state{target=Target
                      ,transferor=Transferor
                      ,target_call=TargetCall
                     }=State
             ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Transferor ->
            lager:debug("recv CHANNEL_BRIDGE on target ~s to transferor ~s", [Target, Transferor]),
            {'next_state', 'attended_answer', State};
        _CallId ->
            lager:debug("recv CHANNEL_BRIDGE on target ~s to call id ~s", [Target, Transferor]),
            connect_to_target(Transferor, TargetCall),
            {'next_state', 'attended_wait', State}
    end;
attended_wait(?EVENT(Target, <<"originate_uuid">>, Evt)
              ,#state{target=Target
                      ,target_call=TargetCall
                     }=State
             ) ->
    lager:debug("recv control for target ~s", [Target]),
    TargetCall1 = whapps_call:from_originate_uuid(Evt, TargetCall),
    {'next_state', 'attended_wait', State#state{target_call=TargetCall1}};
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
attended_wait(?EVENT(Target, <<"CHANNEL_CREATE">>, _Evt)
              ,#state{target=Target}=State
             ) ->
    lager:debug("transfer target ~s channel created", [Target]),
    {'next_state', 'attended_wait', State};
attended_wait(?EVENT(Target, <<"originate_resp">>, _Evt)
              ,#state{target=Target}=State
             ) ->
    lager:debug("originate has responded for target ~s", [Target]),
    {'next_state', 'attended_wait', State};
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

attended_answer(?EVENT(Transferor, <<"DTMF">>, Evt), #state{transferor=Transferor}=State) ->
    handle_transferor_dtmf(Evt, 'attended_answer', State);
attended_answer(?EVENT(Transferor, <<"CHANNEL_BRIDGE">>, Evt)
                ,#state{transferor=Transferor
                        ,transferee=Transferee
                        ,target=Target
                       }=State
               ) ->
    lager:debug("transferor ~s bridged: ~p", [Transferor, Evt]),
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
attended_answer(?EVENT(Target, <<"CHANNEL_BRIDGE">>, Evt)
                ,#state{transferor=Transferor
                        ,target=Target
                       }=State
               ) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
        Transferor ->
            lager:debug("transferor and target are connected"),
            {'next_state', 'attended_answer', State};
        _CallId ->
            lager:debug("target ~s bridged to ~s", [Target, _CallId]),
            {'next_state', 'attended_answer', State}
    end;
attended_answer(?EVENT(Transferee, <<"CHANNEL_DESTROY">>, _Evt)
                ,#state{transferee=Transferee}=State
               ) ->
    lager:debug("transferee ~s hungup while transferor and target were talking", [Transferee]),
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
                      ,target_b_legs=[BLeg | _Bs]
                     }=State
               ) ->
    lager:debug("current target ~s destroyed, but ~s is still around", [Target, BLeg]),
    {'next_state', 'attended_answer', State};
attended_answer(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target=Target
                      ,target_b_legs=[]
                      ,transferor=Transferor
                      ,transferee=Transferee
                      ,call=Call
                     }=State
             ) ->
    lager:debug("target ~s hungup, reconnecting transferor ~s to transferee ~s"
                ,[Target, Transferor, Transferee]
               ),

    case whapps_call:call_id(Call) of
        Transferee -> connect_to_target(Transferor, Call);
        Transferor -> connect_to_target(Transferee, Call)
    end,
    {'stop', 'normal', State};
attended_answer(?EVENT(CallId, <<"CHANNEL_DESTROY">>, _Evt)
                ,#state{target_b_legs=Bs
                        ,target=Target
                       }=State
               ) ->
    case lists:member(CallId, Bs) of
        'true' ->
            lager:debug("target b leg ~s down", [CallId]),
            gen_fsm:send_event(self(), ?EVENT(Target, <<"CHANNEL_DESTROY">>, wh_json:new())),
            {'next_state', 'attended_answer', State#state{target_b_legs=lists:delete(CallId, Bs)}};
        'false' ->
            lager:debug("unknown leg ~s down", [CallId]),
            {'next_state', 'attended_answer', State}
    end;
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

init(_) -> {'ok', 'attended_wait', #state{}}.

-spec add_transferor_bindings(ne_binary()) -> 'ok'.
add_transferor_bindings(CallId) ->
    konami_event_listener:add_call_binding(CallId, ['CHANNEL_DESTROY'
                                                    ,'CHANNEL_BRIDGE'
                                                    ,'DTMF'
                                                    ,'LEG_CREATED'
                                                    ,'LEG_DESTROYED'
                                                   ]).

-spec add_transferee_bindings(ne_binary()) -> 'ok'.
add_transferee_bindings(CallId) ->
    konami_event_listener:add_call_binding(CallId, ['CHANNEL_DESTROY'
                                                    ,'CHANNEL_BRIDGE'
                                                    ,'LEG_CREATED'
                                                    ,'LEG_DESTROYED'
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

-spec create_call_id() -> ne_binary().
create_call_id() ->
    TargetCallId = <<"konami-transfer-", (wh_util:rand_hex_binary(4))/binary>>,
    konami_event_listener:add_call_binding(TargetCallId, ['CHANNEL_ANSWER'
                                                          ,'CHANNEL_DESTROY'
                                                          ,'CHANNEL_CREATE'
                                                          ,'CHANNEL_BRIDGE'
                                                          ,'LEG_CREATED'
                                                          ,'LEG_DESTROYED'
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

-spec handle_transferor_dtmf(wh_json:object(), NextState, state()) ->
                                    {'stop', 'normal', state()} |
                                    {'next_state', NextState, state()}.
handle_transferor_dtmf(Evt, NextState
                       ,#state{call=Call
                               ,target_call=TargetCall
                               ,takeback_dtmf=TakebackDTMF
                               ,transferor_dtmf=DTMFs
                              }=State
                      ) ->
        Digit = wh_json:get_value(<<"DTMF-Digit">>, Evt),
    lager:debug("recv transferor dtmf '~s', adding to '~s'", [Digit, DTMFs]),

    Collected = <<DTMFs/binary, Digit/binary>>,

    case wh_util:suffix_binary(TakebackDTMF, Collected) of
        'true' ->
            lager:debug("takeback dtmf sequence (~s) engaged!", [TakebackDTMF]),
            connect_to_target(Call),
            whapps_call_command:hangup(TargetCall),
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
