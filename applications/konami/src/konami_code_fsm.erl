%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_code_fsm).

-behaviour(gen_fsm).

%% API
-export([start_fsm/2
         ,event/4
         ,transfer_to/2
        ]).

%% gen_fsm callbacks
-export([init/1

         ,unarmed/2, unarmed/3
         ,armed/2, armed/3

         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4
        ]).

-include("konami.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-record(state, {numbers :: api_object()
                ,patterns :: api_object()
                ,binding_digit = konami_config:binding_digit() :: ne_binary()
                ,digit_timeout = konami_config:timeout() :: pos_integer()
                ,call :: whapps_call:call()

                ,listen_on = 'a' :: 'a' | 'b' | 'ab'

                ,a_digit_timeout_ref :: api_reference()
                ,a_collected_dtmf = <<>> :: binary()
                ,a_endpoint_id :: api_binary()

                ,b_digit_timeout_ref :: api_reference()
                ,b_collected_dtmf = <<>> :: binary()
                ,b_endpoint_id :: api_binary()

                ,call_id :: ne_binary()

                ,other_leg :: api_binary()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_fsm(whapps_call:call(), wh_json:object()) -> any().
start_fsm(Call, JObj) ->
    wh_hooks:register(whapps_call:account_id(Call), <<"CHANNEL_DESTROY">>),
    wh_hooks:register(whapps_call:account_id(Call), <<"CHANNEL_ANSWER">>),

    ListenOn = listen_on(Call, JObj),

    _ = maybe_start_leg_listeners(Call, ListenOn),

    BEndpointId = case is_a_leg(Call, JObj) of
                    'true' -> 'undefined';
                    'false' -> wh_json:get_value(<<"Endpoint-ID">>, JObj)
                end,

    konami_tracker:track(whapps_call:call_id(Call), whapps_call:other_leg_call_id(Call), ListenOn),

    lager:debug("starting code FSM, listening on ~s leg for binding digit ~s", [ListenOn
                                                                              ,binding_digit(Call, JObj)
                                                                             ]),
    lager:debug("a endpoint: ~s b endpoint: ~s", [whapps_call:authorizing_id(Call), BEndpointId]),

    gen_fsm:enter_loop(?MODULE, [], 'unarmed'
                       ,#state{numbers=numbers(Call, JObj)
                               ,patterns=patterns(Call, JObj)
                               ,binding_digit=binding_digit(Call, JObj)
                               ,digit_timeout=digit_timeout(Call, JObj)
                               ,listen_on=ListenOn

                               ,call=whapps_call:clear_helpers(
                                       whapps_call:kvs_store(?MODULE, self(), Call)
                                      )
                               ,call_id=whapps_call:call_id_direct(Call)

                               ,a_endpoint_id = whapps_call:authorizing_id(Call)
                               ,b_endpoint_id = BEndpointId
                              }).

-spec event(pid(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
event(FSM, CallId, <<"DTMF">>, JObj) ->
    gen_fsm:send_event(FSM, {'dtmf'
                             ,CallId
                             ,wh_json:get_value(<<"DTMF-Digit">>, JObj)
                            });
event(FSM, CallId, Event, JObj) ->
    gen_fsm:send_all_state_event(FSM, {'event', CallId, Event, JObj}).

-spec transfer_to(whapps_call:call(), 'a' | 'b') -> 'ok'.
transfer_to(Call, Leg) ->
    gen_fsm:send_all_state_event(whapps_call:kvs_fetch(?MODULE, Call)
                                 ,{'transfer_to', Call, Leg}
                                ).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([]) ->
    {'ok', 'unarmed', #state{}}.

unarmed({'dtmf', CallId, BindingDigit}, #state{call_id=CallId
                                             ,listen_on='a'
                                             ,binding_digit=BindingDigit
                                             ,digit_timeout=Timeout
                                            }=State) ->
    lager:debug("recv binding digit ~s, arming (~bms)", [BindingDigit, Timeout]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{a_digit_timeout_ref = Ref
                                        ,a_collected_dtmf = <<>>
                                       }};
unarmed({'dtmf', CallId, BindingDigit}, #state{call_id=CallId
                                             ,listen_on='ab'
                                             ,binding_digit=BindingDigit
                                             ,digit_timeout=Timeout
                                            }=State) ->
    lager:debug("recv binding digit ~s, arming (~bms)", [BindingDigit, Timeout]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{a_digit_timeout_ref = Ref
                                        ,a_collected_dtmf = <<>>
                                       }};
unarmed({'dtmf', CallId, BindingDigit}, #state{other_leg=CallId
                                             ,listen_on='ab'
                                             ,binding_digit=BindingDigit
                                             ,digit_timeout=Timeout
                                            }=State) ->
    lager:debug("recv binding digit '~s' for other leg ~s arming (~bms)", [BindingDigit, CallId, Timeout]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{b_digit_timeout_ref = Ref
                                        ,b_collected_dtmf = <<>>
                                       }};
unarmed({'dtmf', CallId, BindingDigit}, #state{other_leg=CallId
                                             ,listen_on='b'
                                             ,binding_digit=BindingDigit
                                             ,digit_timeout=Timeout
                                            }=State) ->
    lager:debug("recv binding digit '~s' for other leg ~s arming (~bms)", [BindingDigit, CallId, Timeout]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{b_digit_timeout_ref = Ref
                                        ,b_collected_dtmf = <<>>
                                       }};
unarmed({'dtmf', _CallId, _DTMF}, #state{call_id=_Id
                                         ,other_leg=_Oleg
                                         ,listen_on=_ListenOn
                                        }=State) ->
    lager:debug("ignoring dtmf '~s' from ~s while unarmed", [_DTMF, _CallId]),
    lager:debug("call id '~s' other_leg '~s' listen_on '~s'", [_Id, _Oleg, _ListenOn]),
    {'next_state', 'unarmed', State};
unarmed(_Event, State) ->
    lager:debug("unhandled unarmed/2: ~p", [_Event]),
    {'next_state', 'unarmed', State}.

unarmed(_Event, _From, State) ->
    lager:debug("unhandled unarmed/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'unarmed', State}.

armed({'timeout', Ref, 'digit_timeout'}, #state{numbers=Ns
                                                ,patterns=Ps
                                                ,listen_on=ListenOn
                                                ,a_collected_dtmf = Collected
                                                ,a_digit_timeout_ref = Ref
                                                ,call=Call
                                               }=State)
  when ListenOn =:= 'a' orelse ListenOn =:= 'ab' ->
    lager:debug("a DTMF timeout, let's check '~s'", [Collected]),
    case has_metaflow(Collected, Ns, Ps) of
        'false' ->
            lager:debug("no handler for '~s', unarming", [Collected]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
        {'number', N} ->
            Metaflow = wh_json:set_values([{[<<"data">>, <<"dtmf_leg">>], whapps_call:call_id(Call)}], N),
            _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
            lager:debug("number exe in ~p: ~p", [_Pid, Metaflow]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
        {'pattern', P} ->
            Metaflow = wh_json:set_values([{[<<"data">>, <<"dtmf_leg">>], whapps_call:call_id(Call)}], P),
            _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
            lager:debug("pattern exe in ~p: ~p", [_Pid, Metaflow]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'}
    end;
armed({'timeout', Ref, 'digit_timeout'}, #state{numbers=Ns
                                                ,patterns=Ps
                                                ,listen_on=ListenOn
                                                ,b_collected_dtmf = Collected
                                                ,b_digit_timeout_ref = Ref
                                                ,call=Call
                                               }=State)
  when ListenOn =:= 'b' orelse ListenOn =:= 'ab' ->
    lager:debug("b DTMF timeout, let's check '~s'", [Collected]),
    case has_metaflow(Collected, Ns, Ps) of
        'false' ->
            lager:debug("no handler for '~s', unarming", [Collected]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
        {'number', N} ->
            Metaflow = wh_json:set_values([{[<<"data">>, <<"dtmf_leg">>], whapps_call:other_leg_call_id(Call)}], N),
            _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
            lager:debug("number exe in ~p: ~p", [_Pid, Metaflow]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
        {'pattern', P} ->
            Metaflow = wh_json:set_values([{[<<"data">>, <<"dtmf_leg">>], whapps_call:other_leg_call_id(Call)}], P),
            _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
            lager:debug("pattern exe in ~p: ~p", [_Pid, Metaflow]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'}
    end;

armed({'dtmf', CallId, DTMF}, #state{call_id=CallId
                                     ,a_collected_dtmf=Collected
                                     ,digit_timeout=Timeout
                                     ,a_digit_timeout_ref=OldRef
                                    }=State) ->
    gen_fsm:cancel_timer(OldRef),
    lager:debug("a recv dtmf '~s' while armed, adding to '~s'", [DTMF, Collected]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{a_digit_timeout_ref=Ref
                                        ,a_collected_dtmf = <<Collected/binary, DTMF/binary>>
                                       }};
armed({'dtmf', CallId, DTMF}, #state{other_leg=CallId
                                     ,b_collected_dtmf=Collected
                                     ,digit_timeout=Timeout
                                     ,b_digit_timeout_ref=OldRef
                                    }=State) ->
    gen_fsm:cancel_timer(OldRef),
    lager:debug("b recv dtmf '~s' while armed, adding to '~s'", [DTMF, Collected]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{b_digit_timeout_ref=Ref
                                        ,b_collected_dtmf = <<Collected/binary, DTMF/binary>>
                                       }};
armed(_Event, State) ->
    lager:debug("unhandled armed/2: ~p", [_Event]),
    {'next_state', 'armed', State}.

armed(_Event, _From, State) ->
    lager:debug("unhandled armed/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'armed', State}.

handle_event({'transfer_to', NewCall, 'a'}, StateName, #state{listen_on=ListenOn
                                                              ,call=Call
                                                             }=State) ->
    lager:debug("updating call to ~s in ~s", [whapps_call:call_id_direct(NewCall), StateName]),
    whapps_call:put_callid(NewCall),
    _ = maybe_start_leg_listeners(NewCall, ListenOn),
    _ = stop_leg_listeners(Call),
    {'next_state', StateName, State#state{call=NewCall
                                          ,call_id=whapps_call:call_id_direct(NewCall)
                                          ,a_endpoint_id='undefined'
                                         }};
handle_event({'transfer_to', NewCall, 'b'}, StateName, #state{listen_on=ListenOn
                                                              ,call=Call
                                                             }=State) ->
    OldOtherLeg = whapps_call:other_leg_call_id(Call),
    lager:debug("updating call to ~s (was ~s) in ~s"
                ,[whapps_call:other_leg_call_id(NewCall), OldOtherLeg, StateName]
               ),

    _ = maybe_start_leg_listeners(NewCall, ListenOn),
    _ = stop_leg_listeners(OldOtherLeg),

    {'next_state', StateName, State#state{call=NewCall
                                          ,other_leg=whapps_call:other_leg_call_id(NewCall)
                                          ,b_endpoint_id='undefined'
                                         }};

handle_event({'event', CallId, <<"CHANNEL_BRIDGE">>, JObj}, StateName, State) ->
    lager:debug("channel_bridge recv for ~s", [CallId]),
    {'next_state', StateName
     ,handle_channel_event(CallId
                           ,wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj)
                           ,wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], JObj)
                           ,State
                          )
    };
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

handle_info(?HOOK_EVT(_AccountId, <<"CHANNEL_ANSWER">>, Evt), StateName, State) ->
    AuthorizingId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], Evt),
    CallId = wh_json:get_value(<<"Call-ID">>, Evt),
    OtherLeg = wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt),
    lager:debug("answer: ~s (~s) endpoint id: ~s", [CallId, OtherLeg, AuthorizingId]),

    {'next_state', StateName, handle_channel_event(CallId, OtherLeg, AuthorizingId, State)};
handle_info(?HOOK_EVT(_AccountId, <<"CHANNEL_DESTROY">>, Evt), StateName, State) ->
    handle_channel_destroy(wh_json:get_value(<<"Call-ID">>, Evt)
                           ,wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], Evt)
                           ,State
                          ),
    {'next_state', StateName, State};
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

terminate(_Reason, _StateName, #state{call_id=CallId
                                      ,other_leg=OtherLeg
                                     }) ->
    konami_tracker:untrack(),
    konami_event_listener:rm_call_binding(CallId),
    konami_event_listener:rm_call_binding(OtherLeg),
    lager:debug("fsm terminating while in ~s: ~p", [_StateName, _Reason]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

-spec handle_channel_event(ne_binary(), api_binary(), api_binary(), state()) -> state().
handle_channel_event(CallId, 'undefined', AEndpoint, #state{call_id=CallId
                                                            ,a_endpoint_id=AEndpoint
                                                           }=State)->
    lager:debug("a leg is answered but not bridged"),
    State;
handle_channel_event(CallId, OtherLeg, AEndpoint, #state{call_id=CallId
                                                         ,other_leg=OtherLeg
                                                         ,a_endpoint_id=AEndpoint
                                                        }=State) ->
    konami_tracker:update_other_leg(OtherLeg),
    lager:debug("a leg is answered and bridged to ~s", [OtherLeg]),
    State;
handle_channel_event(CallId, NewOtherLeg, AEndpoint, #state{call_id=CallId
                                                            ,other_leg=_OtherLeg
                                                            ,a_endpoint_id=AEndpoint
                                                            ,call=Call
                                                           }=State) ->
    lager:debug("a leg is answered and now bridged to ~s (was ~s)", [NewOtherLeg, _OtherLeg]),
    konami_event_listener:add_call_binding(NewOtherLeg),
    konami_tracker:update_other_leg(NewOtherLeg),
    State#state{other_leg=NewOtherLeg
                ,call=whapps_call:set_other_leg_call_id(NewOtherLeg, Call)
               };
handle_channel_event(CallId, OtherLeg, NewAEndpointId, #state{call_id=CallId
                                                              ,other_leg=OtherLeg
                                                              ,a_endpoint_id='undefined'
                                                             }=State) ->
    lager:debug("our a-leg ~s came in with an endpoint ~s", [OtherLeg, NewAEndpointId]),
    State#state{a_endpoint_id=NewAEndpointId};
handle_channel_event(OtherLeg, CallId, BEndpointId, #state{call_id=CallId
                                                           ,other_leg=OtherLeg
                                                           ,b_endpoint_id=BEndpointId
                                                          }=State) ->
    lager:debug("b leg ~s is answered for b endpoint ~s", [OtherLeg, BEndpointId]),
    konami_tracker:update_other_leg(OtherLeg),
    State;
handle_channel_event(OtherLeg, YetAnotherLeg, BEndpointId, #state{call_id=_CallId
                                                                  ,other_leg=OtherLeg
                                                                  ,b_endpoint_id=BEndpointId
                                                                 }=State) ->
    lager:debug("yet another leg ~s for b leg ~s and b endpoint ~s", [YetAnotherLeg, OtherLeg, BEndpointId]),
    State;
handle_channel_event(NewOtherLeg, CallId, BEndpointId, #state{call_id=CallId
                                                              ,other_leg=OtherLeg
                                                              ,b_endpoint_id=BEndpointId
                                                              ,call=Call
                                                             }=State) ->
    konami_event_listener:add_call_binding(NewOtherLeg),
    konami_event_listener:rm_call_binding(OtherLeg),
    lager:debug("new b leg ~s endpoint ~s is answered (was ~s)", [NewOtherLeg, BEndpointId, OtherLeg]),

    konami_tracker:update_other_leg(NewOtherLeg),

    State#state{other_leg=NewOtherLeg
                ,call=whapps_call:set_other_leg_call_id(NewOtherLeg, Call)
                ,b_endpoint_id=BEndpointId
               };
handle_channel_event(OtherLeg, CallId, NewBEndpointId, #state{call_id=CallId
                                                              ,other_leg=OtherLeg
                                                              ,b_endpoint_id='undefined'
                                                             }=State) ->
    lager:debug("our b-leg ~s came in with an endpoint ~s", [OtherLeg, NewBEndpointId]),
    State#state{b_endpoint_id=NewBEndpointId};
handle_channel_event(NewOtherLeg, CallId, NewBEndpointId, #state{call_id=CallId
                                                                 ,other_leg=OtherLeg
                                                                 ,b_endpoint_id=BEndpointId
                                                                }=State) ->
    lager:debug("new b leg ~s endpoint ~s is bridged to a leg (we want b leg ~s endpoint ~s)"
                ,[NewOtherLeg, NewBEndpointId, OtherLeg, BEndpointId]
               ),
    State;
handle_channel_event(NewOtherLeg, 'undefined', EndpointId, #state{call_id=CallId
                                                                  ,other_leg=OtherLeg
                                                                  ,b_endpoint_id=BEndpointId
                                                                 }=State) ->
    lager:debug("recv answer for leg ~s endpoint ~s", [NewOtherLeg, EndpointId]),
    lager:debug("tracking a leg ~s and b leg ~s b endpoint ~s", [CallId, OtherLeg, BEndpointId]),
    State;
handle_channel_event(_EvtCallId, _EvtOtherLeg, _EvtEndpointId, #state{call_id=_CallId
                                                                      ,other_leg=_OtherLeg
                                                                      ,a_endpoint_id=_AEndpointId
                                                                      ,b_endpoint_id=_BEndpointId
                                                                     }=State) ->
    lager:debug("recv answer for leg ~s endpoint ~s (other ~s)", [_EvtCallId, _EvtEndpointId, _EvtOtherLeg]),
    lager:debug("tracking a leg ~s endpoint ~s and b leg ~s endpoint ~s", [_CallId, _AEndpointId
                                                                           ,_OtherLeg, _BEndpointId
                                                                          ]),
    State.

-spec handle_channel_destroy(ne_binary(), api_binary(), state()) -> 'ok'.
handle_channel_destroy(CallId, EndpointId, #state{call_id=CallId
                                                  ,a_endpoint_id=EndpointId
                                                  ,listen_on=ListenOn
                                                 }) when ListenOn =:= 'a';
                                                         ListenOn =:= 'ab' ->
    lager:debug("a leg has died and we were listening on it"),
    exit('normal');
handle_channel_destroy(CallId, EndpointId, #state{call_id=CallId
                                                  ,other_leg=OtherLeg
                                                  ,a_endpoint_id=EndpointId
                                                 }) ->
    lager:debug("a leg has died, but we only care about the b leg ~s", [OtherLeg]);
handle_channel_destroy(OtherLeg, EndpointId, #state{other_leg=OtherLeg
                                                   ,b_endpoint_id=EndpointId
                                                   ,listen_on=ListenOn
                                                   }) when ListenOn =:= 'b';
                                                           ListenOn =:= 'ab' ->
    lager:debug("b leg ~s endpoint ~s has died and we were listening on it", [OtherLeg, EndpointId]),
    exit('normal');
handle_channel_destroy(OtherLeg, EndpointId, #state{other_leg=OtherLeg
                                                    ,b_endpoint_id=EndpointId
                                                   }) ->
    lager:debug("b leg ~s endpoint ~s has died but we weren't listening on it", [OtherLeg, EndpointId]);
handle_channel_destroy(OtherLeg, EndpointId, #state{other_leg='undefined'
                                                    ,b_endpoint_id=EndpointId
                                                   }) ->
    lager:debug("b leg ~s for endpoint ~s has died, so we're done", [OtherLeg, EndpointId]),
    exit('normal');
handle_channel_destroy(CallId, _EndpointId, #state{call_id=CallId
                                                   ,a_endpoint_id='undefined'
                                                   ,listen_on=ListenOn
                                                  }) when ListenOn =:= 'a';
                                                          ListenOn =:= 'ab' ->
    lager:debug("a leg ~s went down (endpoint ~s) but we're not tracking an endpoint, so go down"
                ,[CallId, _EndpointId]
               ),
    exit('normal');
handle_channel_destroy(CallId, _EndpointId, #state{other_leg=CallId
                                                   ,b_endpoint_id='undefined'
                                                   ,listen_on=ListenOn
                                                  }) when ListenOn =:= 'b';
                                                          ListenOn =:= 'ab' ->
    lager:debug("b leg ~s went down (endpoint ~s) but we're not tracking an endpoint, so go down"
                ,[CallId, _EndpointId]
               ),
    exit('normal');
handle_channel_destroy(_CallId, _EndpointId, #state{call_id=CallId
                                                    ,a_endpoint_id=AE
                                                    ,other_leg=OtherLeg
                                                    ,b_endpoint_id=BE
                                                   }) ->
    lager:debug("unhandled destroy for leg ~s endpoint ~s", [_CallId, _EndpointId]),
    lager:debug("a ~s endpoint ~s", [CallId, AE]),
    lager:debug("b ~s endpoint ~s", [OtherLeg, BE]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec binding_digit(whapps_call:call(), wh_json:object()) -> ne_binary().
binding_digit(Call, JObj) ->
    case wh_json:get_value(<<"Binding-Digit">>, JObj) of
        'undefined' ->
            konami_config:binding_digit(whapps_call:account_id(Call));
        BindingDigit ->
            lager:debug("using custom binding digit '~s'", [BindingDigit]),
            BindingDigit
    end.

-spec numbers(whapps_call:call(), wh_json:object()) -> wh_json:object().
numbers(Call, JObj) ->
    case wh_json:get_value(<<"Numbers">>, JObj) of
        'undefined' ->
            lager:debug("loading default account metaflow numbers"),
            konami_config:numbers(whapps_call:account_id(Call));
        Numbers ->
            lager:debug("loading numbers from api: ~p", [Numbers]),
            Numbers
    end.

-spec patterns(whapps_call:call(), wh_json:object()) -> wh_json:object().
patterns(Call, JObj) ->
    case wh_json:get_value(<<"Patterns">>, JObj) of
        'undefined' -> konami_config:patterns(whapps_call:account_id(Call));
        Patterns -> Patterns
    end.

-spec digit_timeout(whapps_call:call(), wh_json:object()) -> pos_integer().
digit_timeout(Call, JObj) ->
    case wh_json:get_integer_value(<<"Digit-Timeout">>, JObj) of
        'undefined' -> konami_config:timeout(whapps_call:account_id(Call));
        Timeout -> Timeout
    end.

-spec is_a_leg(whapps_call:call(), api_object() | ne_binary()) -> boolean().
is_a_leg(_Call, 'undefined') -> 'true';
is_a_leg(Call, <<_/binary>> = EndpointId) ->
    EndpointId =:= whapps_call:authorizing_id(Call);
is_a_leg(Call, JObj) ->
    is_a_leg(Call, wh_json:get_value(<<"Endpoint-ID">>, JObj)).

-spec listen_on(whapps_call:call(), wh_json:object()) -> 'a' | 'b' | 'ab'.
listen_on(Call, JObj) ->
    EndpointId = wh_json:get_value(<<"Endpoint-ID">>, JObj),
    IsALegEndpoint = is_a_leg(Call, EndpointId),

    lager:debug("endpoint ~s is~s a-leg"
                ,[EndpointId, case IsALegEndpoint of 'true' -> <<>>; 'false' -> <<" not">> end]
               ),

    case wh_json:get_value(<<"Listen-On">>, JObj) of
        <<"both">> -> 'ab';
        <<"self">> when IsALegEndpoint -> 'a';
        <<"self">> -> 'b';
        <<"peer">> when IsALegEndpoint -> 'b';
        <<"peer">> -> 'a';
        _ when IsALegEndpoint -> 'a';
        _ -> 'b'
    end.

-spec has_metaflow(ne_binary(), wh_json:object(), wh_json:object()) ->
                          'false' |
                          {'number', wh_json:object()} |
                          {'patterm', wh_json:object()}.
has_metaflow(Collected, Ns, Ps) ->
    case has_number(Collected, Ns) of
        'false' -> has_pattern(Collected, Ps);
        N -> N
    end.

-spec has_number(ne_binary(), wh_json:object()) ->
                        'false' |
                        {'number', wh_json:object()}.
has_number(Collected, Ns) ->
    case wh_json:get_value(Collected, Ns) of
        'undefined' -> 'false';
        N -> {'number', N}
    end.

-spec has_pattern(ne_binary(), wh_json:object()) ->
                         'false' |
                         {'pattern', wh_json:object()}.
has_pattern(Collected, Ps) ->
    Regexes = wh_json:get_keys(Ps),
    has_pattern(Collected, Ps, Regexes).
has_pattern(_Collected, _Ps, []) ->
    'false';
has_pattern(Collected, Ps, [Regex|Regexes]) ->
    case re:run(Collected, Regex, [{'capture', 'all_but_first', 'binary'}]) of
        'nomatch' -> has_pattern(Collected, Ps, Regexes);
        {'match', Captured} ->
            P = wh_json:get_value(Regex, Ps),
            {'pattern', wh_json:set_values([{[<<"data">>, <<"collected">>], Collected}
                                            ,{[<<"data">>, <<"captures">>], Captured}
                                           ], P)
            }
    end.

-spec disarm_state(state()) -> state().
disarm_state(#state{a_digit_timeout_ref=ARef
                    ,b_digit_timeout_ref=BRef
                   }=State) ->
    lager:debug("disarming state"),
    maybe_cancel_timer(ARef),
    maybe_cancel_timer(BRef),

    State#state{a_digit_timeout_ref='undefined'
                ,a_collected_dtmf = <<>>
                ,b_digit_timeout_ref='undefined'
                ,b_collected_dtmf = <<>>
               }.

-spec maybe_cancel_timer(term()) -> 'ok'.
maybe_cancel_timer(Ref) when is_reference(Ref) ->
    catch erlang:cancel_timer(Ref),
    'ok';
maybe_cancel_timer(_) ->
    'ok'.

-spec maybe_start_leg_listeners(whapps_call:call(), 'a' | 'b' | 'ab') -> 'ok'.
maybe_start_leg_listeners(Call, 'a') ->
    lager:debug("starting call bindings for a leg ~s", [whapps_call:call_id(Call)]),
    konami_event_listener:add_call_binding(Call);
maybe_start_leg_listeners(Call, 'b') ->
    lager:debug("starting call bindings for b leg ~s", [whapps_call:other_leg_call_id(Call)]),
    start_b_leg_listener(Call);
maybe_start_leg_listeners(Call, 'ab') ->
    lager:debug("starting call bindings for both legs ~s and ~s", [whapps_call:call_id(Call)
                                                                   ,whapps_call:other_leg_call_id(Call)
                                                                  ]),
    konami_event_listener:add_call_binding(Call),
    start_b_leg_listener(Call).

-spec stop_leg_listeners(whapps_call:call() | api_binary()) -> 'ok'.
stop_leg_listeners('undefined') -> 'ok';
stop_leg_listeners(<<_/binary>> = CallId) ->
    konami_event_listener:rm_call_binding(CallId);
stop_leg_listeners(Call) ->
    stop_leg_listeners(whapps_call:call_id(Call)).

-spec start_b_leg_listener(whapps_call:call()) -> 'ok'.
start_b_leg_listener(Call) ->
    API = [{<<"Application-Name">>, <<"noop">>}
           ,{<<"B-Leg-Events">>, [<<"DTMF">>]}
           ,{<<"Insert-At">>, <<"now">>}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending noop for b leg events"),
    whapps_call_command:send_command(API, Call),
    konami_event_listener:add_call_binding(whapps_call:other_leg_call_id(Call)).
