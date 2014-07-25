%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Transfers caller to the extension extracted in the regex
%%% Data = {
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

    lager:debug("unbridge and put transferee ~s on hold", [TransfereeLeg]),
    konami_hold:handle(Data, Call),

    [Extension|_] = wh_json:get_value(<<"captures">>, Data),
    lager:debug("ok, now we need to originate to the requested number ~s", [Extension]),

    'ok' = originate_to_extension(Extension, TransferorLeg, Call),

    try gen_fsm:enter_loop(?MODULE, [], 'attended_wait'
                           ,#state{transferor=TransferorLeg
                                   ,transferee=TransfereeLeg
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
    _ = konami_resume:handle(wh_json:new(), Call),
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
                      ,transferee=Transferee
                      ,target='undefined'
                      ,call=Call
                     }=State
             ) when Target =/= Transferor,
                    Target =/= Transferee
                    ->
    lager:debug("target ~s has answered, connect to transferor ~s", [Target, Transferor]),
    whapps_call_command:pickup(Target, <<"now">>, whapps_call:set_call_id(Transferor, Call)),
    {'next_state', 'attended_answer', State#state{target=Target}};
attended_wait(?EVENT(Target, <<"CHANNEL_ANSWER">>, _Evt)
              ,#state{transferor=Transferor
                      ,target=Target
                      ,call=Call
                     }=State
             ) ->
    lager:debug("target ~s has answered, connect to transferor ~s", [Target, Transferor]),
    whapps_call_command:pickup(Target, <<"now">>, whapps_call:set_call_id(Transferor, Call)),
    {'next_state', 'attended_answer', State};

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
                     ,call=Call
                    }=State
            ) ->
    lager:debug("transferor ~s hungup, connected transferee ~s and target ~s"
                ,[Transferor, Transferee, Target]
               ),
    whapps_call_command:pickup(Transferee, whapps_call:set_call_id(Target, Call)),
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
    _ = konami_resume:handle(wh_json:new(), Call),
    {'stop', 'normal', State};
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
            {'stop', 'normal', State}
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
                      ,call=Call
                     }=State
             ) ->
    lager:debug("transferor ~s hungup, connected transferee ~s and target ~s"
                ,[Transferor, Transferee, Target]
               ),
    whapps_call_command:pickup(Transferee, <<"now">>, whapps_call:set_call_id(Target, Call)),
    {'stop', 'normal', State};
attended_answer(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target=Target
                      ,transferor=_Transferor
                      ,transferee=_Transferee
                      ,call=Call
                     }=State
             ) ->
    lager:debug("target ~s hungup, reconnecting transferor ~s to transferee ~s"
                ,[Target, _Transferor, _Transferee]
               ),
    _ = konami_resume:handle(wh_json:new(), Call),
    {'stop', 'normal', State};
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
                       ,?EVENT(wh_json:get_value(<<"Call-ID">>, JObj)
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
                                                   ]).

-spec add_transferee_bindings(ne_binary()) -> 'ok'.
add_transferee_bindings(CallId) ->
    konami_event_listener:add_call_binding(CallId, ['CHANNEL_DESTROY'
                                                    ,'CHANNEL_BRIDGE'
                                                   ]).

-spec originate_to_extension(ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec originate_to_extension(ne_binary(), ne_binary(), whapps_call:call(), wh_json:object()) -> 'ok'.
originate_to_extension(Extension, TransferorLeg, Call) ->
    case cf_util:lookup_callflow(Extension, whapps_call:account_id(Call)) of
        {'ok', Flow, 'false'} ->
            lager:debug("found flow for extension ~s", [Extension]),
            originate_to_extension(Extension, TransferorLeg, Call, wh_json:get_value(<<"flow">>, Flow));
        {'ok', _Flow, 'true'} ->
            lager:debug("only the no-match flow was found, not currently allowed"),
            {'error', 'no_flow'};
        {'error', _E} ->
            lager:debug("unable to find flow for extension ~s", [Extension]),
            {'error', 'no_flow'}
    end.

originate_to_extension(_Extension, TransferorLeg, Call, Flow) ->
    case find_endpoints(Call, Flow) of
        [] ->
            lager:debug("no endpoints found in flow ~s: ~p", [_Extension, Flow]),
            {'error', 'no_endpoints'};
        Endpoints ->
            originate_to_endpoints(TransferorLeg, Call, Endpoints)
    end.

-spec originate_to_endpoints(ne_binary(), whapps_call:call(), wh_json:objects()) -> 'ok'.
originate_to_endpoints(TransferorLeg, Call, Endpoints) ->
    %% don't forget to usurp the callflow exe for the call if C-leg answers and transfers
    MsgId = wh_util:rand_hex_binary(4),

    Request = props:filter_undefined(
                add_call_id(
                  [{<<"Application-Name">>, <<"park">>}
                   ,{<<"Msg-ID">>, MsgId}
                   ,{<<"Endpoints">>, update_endpoints(Endpoints)}

                   ,{<<"Timeout">>, 20000}

                   ,{<<"Outbound-Caller-ID-Name">>, caller_id_name(Call, TransferorLeg)}
                   ,{<<"Outbound-Caller-ID-Number">>, caller_id_number(Call, TransferorLeg)}
                   ,{<<"Caller-ID-Name">>, caller_id_name(Call, TransferorLeg)}
                   ,{<<"Caller-ID-Number">>, caller_id_number(Call, TransferorLeg)}

                   ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
                   ,{<<"Continue-On-Fail">>, 'true'}
                   ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
                   ,{<<"Existing-Call-ID">>, TransferorLeg}
                   ,{<<"Resource-Type">>, <<"originate">>}
                   ,{<<"Originate-Immediate">>, 'true'}
                   | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                  ])),

    wh_amqp_worker:cast(Request
                        ,fun wapi_resource:publish_originate_req/1
                       ).

update_endpoints(Endpoints) ->
    [update_endpoint(Endpoint) || Endpoint <- Endpoints].
update_endpoint(Endpoint) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, Endpoint),
    wh_json:set_value(<<"Custom-Channel-Vars">>
                      ,wh_json:set_values([{<<"Hangup-After-Pickup">>, 'false'}
                                           ,{<<"Park-After-Pickup">>, 'true'}
                                           ,{<<"Unbridged-Only">>, 'true'}
                                          ]
                                          ,CCVs
                                         )
                      ,add_call_id(Endpoint)
                     ).
-spec add_call_id(api_terms()) -> api_terms().
add_call_id([_|_]=Endpoint) ->
    TargetCallId = <<"konami-transfer-", (wh_util:rand_hex_binary(4))/binary>>,
    konami_event_listener:add_call_binding(TargetCallId, ['CHANNEL_ANSWER'
                                                          ,'CHANNEL_DESTROY'
                                                         ]),
    props:set_value(<<"Outbound-Call-ID">>, TargetCallId, Endpoint);
add_call_id(Endpoint) ->
    TargetCallId = <<"konami-transfer-", (wh_util:rand_hex_binary(4))/binary>>,
    konami_event_listener:add_call_binding(TargetCallId, ['CHANNEL_ANSWER'
                                                          ,'CHANNEL_DESTROY'
                                                         ]),
    wh_json:set_value(<<"Outbound-Call-ID">>, TargetCallId, Endpoint).

find_endpoints(Call, Flow) ->
    case wh_json:get_value(<<"module">>, Flow) of
        <<"device">> ->
            Data = wh_json:get_value(<<"data">>, Flow),
            EndpointId = wh_json:get_value(<<"id">>, Data),

            lager:debug("building device ~s endpoint", [EndpointId]),

            case cf_endpoint:build(EndpointId, Data, new_call(Call)) of
                {'error', _} -> [];
                {'ok', Endpoints} -> Endpoints
            end;
        <<"user">> ->
            Data = wh_json:get_value(<<"data">>, Flow),
            UserId = wh_json:get_value(<<"id">>, Data),

            lager:debug("getting user ~s endpoints", [UserId]),

            cf_user:get_endpoints(UserId, Data, new_call(Call));
        _ ->
            case wh_json:get_value([<<"children">>, <<"_">>], Flow) of
                'undefined' -> [];
                SubFlow -> find_endpoints(Call, SubFlow)
            end
    end.

caller_id_name(Call, CallerLeg) ->
    case whapps_call:call_id(Call) of
        CallerLeg -> whapps_call:caller_id_name(Call);
        _CalleeLeg -> whapps_call:callee_id_name(Call)
    end.

caller_id_number(Call, CallerLeg) ->
    case whapps_call:call_id(Call) of
        CallerLeg -> whapps_call:caller_id_number(Call);
        _CalleeLeg -> whapps_call:callee_id_number(Call)
    end.

new_call(Call) ->
    whapps_call:exec([{fun whapps_call:set_account_id/2, whapps_call:account_id(Call)}
                      ,{fun whapps_call:set_account_db/2, whapps_call:account_db(Call)}
                     ]
                     ,whapps_call:new()
                    ).
