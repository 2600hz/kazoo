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

-type listen_on() :: 'a' | 'b' | 'ab'.

-record(state, {numbers :: api_object()
                ,patterns :: api_object()
                ,binding_digit = konami_config:binding_digit() :: ne_binary()
                ,digit_timeout = konami_config:timeout() :: pos_integer()
                ,call :: whapps_call:call() | 'undefined'

                ,listen_on = 'a' :: listen_on()

                ,a_digit_timeout_ref :: api_reference()
                ,a_collected_dtmf = <<>> :: binary()

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
    ListenOn = listen_on(Call, JObj),

    maybe_add_call_event_bindings(Call, ListenOn),

    lager:debug("starting Konami FSM, listening on '~s'", [ListenOn]),

    BEndpointId = b_endpoint_id(JObj, ListenOn),

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

                               ,b_endpoint_id=BEndpointId
                              }).

-spec event(pid(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
event(FSM, CallId, <<"DTMF">>, JObj) ->
    gen_fsm:send_event(FSM, {'dtmf'
                             ,CallId
                             ,wh_json:get_value(<<"DTMF-Digit">>, JObj)
                            });
event(FSM, CallId, Event, JObj) ->
    gen_fsm:send_all_state_event(FSM, ?EVENT(CallId, Event, JObj)).

-spec transfer_to(whapps_call:call(), 'a' | 'b') -> 'ok'.
transfer_to(Call, Leg) ->
    gen_fsm:send_all_state_event(whapps_call:kvs_fetch(?MODULE, Call)
                                 ,{'transfer_to', Call, Leg}
                                ).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-spec init([]) -> {'ok', 'unarmed', state()}.
init([]) ->
    {'ok', 'unarmed', #state{}}.

unarmed({'dtmf', CallId, BindingDigit}, #state{call_id=CallId
                                               ,listen_on='a'
                                               ,binding_digit=BindingDigit
                                              }=State) ->
    lager:debug("recv binding digit ~s, arming a-leg", [BindingDigit]),
    {'next_state', 'armed', arm_aleg(State)};
unarmed({'dtmf', CallId, BindingDigit}, #state{call_id=CallId
                                               ,listen_on='ab'
                                               ,binding_digit=BindingDigit
                                              }=State) ->
    lager:debug("recv binding digit ~s, arming a leg", [BindingDigit]),
    {'next_state', 'armed', arm_aleg(State)};

unarmed({'dtmf', CallId, BindingDigit}, #state{other_leg=CallId
                                               ,listen_on='ab'
                                               ,binding_digit=BindingDigit
                                              }=State) ->
    lager:debug("recv binding digit '~s', arming b leg ~s", [BindingDigit, CallId]),
    {'next_state', 'armed', arm_bleg(State)};
unarmed({'dtmf', CallId, BindingDigit}, #state{other_leg=CallId
                                               ,listen_on='b'
                                               ,binding_digit=BindingDigit
                                              }=State) ->
    lager:debug("recv binding digit '~s', arming b leg ~s", [BindingDigit, CallId]),
    {'next_state', 'armed', arm_bleg(State)};

unarmed({'dtmf', _CallId, _DTMF}, #state{call_id=_Id
                                         ,other_leg=_Oleg
                                         ,listen_on=_ListenOn
                                        }=State) ->
    lager:debug("ignoring dtmf '~s' from ~s while unarmed", [_DTMF, _CallId]),
    lager:debug("call id '~s' other_leg '~s' listen_on '~s'", [_Id, _Oleg, _ListenOn]),
    {'next_state', 'unarmed', State};
unarmed(_Event, #state{call_id=_CallId
                       ,other_leg=_OtherLeg
                       ,listen_on=_LO
                      }=State) ->
    lager:debug("unhandled unarmed/2: ~p", [_Event]),
    lager:debug("listen_on: '~s' call id: '~s' other leg: '~s'", [_LO, _CallId, _OtherLeg]),
    {'next_state', 'unarmed', State, 'hibernate'}.

unarmed(_Event, _From, State) ->
    lager:debug("unhandled unarmed/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'unarmed', State}.

armed({'dtmf', CallId, DTMF}, #state{call_id=CallId}=State) ->
    {'next_state', 'armed', add_aleg_dtmf(State, DTMF)};
armed({'dtmf', CallId, DTMF}, #state{other_leg=CallId}=State) ->
    {'next_state', 'armed', add_bleg_dtmf(State, DTMF)};
armed({'timeout', Ref, 'digit_timeout'}, #state{a_digit_timeout_ref = Ref}=State) ->
    _ = maybe_handle_aleg_code(State),
    {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
armed({'timeout', Ref, 'digit_timeout'}, #state{b_digit_timeout_ref = Ref}=State) ->
    _ = maybe_handle_bleg_code(State),
    {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
armed(_Event, State) ->
    lager:debug("unhandled armed/2: ~p", [_Event]),
    {'next_state', 'armed', State}.

armed(_Event, _From, State) ->
    lager:debug("unhandled armed/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'armed', State}.

handle_event(?EVENT(CallId, <<"CHANNEL_BRIDGE">>, Evt)
             ,StateName
             ,#state{call_id=CallId}=State
            ) ->
    OtherLeg = wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt),
    {'next_state', StateName, handle_channel_bridged(State, Evt, CallId, OtherLeg)};
handle_event(?EVENT(OtherLeg, <<"CHANNEL_BRIDGE">>, Evt)
             ,StateName
             ,#state{other_leg=OtherLeg}=State
            ) ->
    CallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt),
    {'next_state', StateName, handle_channel_bridged(State, Evt, CallId, OtherLeg)};

handle_event(?EVENT(_AccountId, <<"CHANNEL_ANSWER">>, Evt)
             ,StateName
             ,State
            ) ->
    CallId = wh_json:get_value(<<"Call-ID">>, Evt),
    {'next_state', StateName, handle_channel_answered(CallId, Evt, State)};

handle_event(?EVENT(CallId, <<"CHANNEL_TRANSFEREE">>, Evt)
             ,StateName
             ,#state{call_id=CallId}=State
            ) ->
    lager:debug("a-leg ~s has been transferred", [CallId]),
    {'next_state', StateName, handle_channel_transferee(CallId, Evt, State)};

handle_event(?EVENT(OtherLeg, <<"CHANNEL_TRANSFEREE">>, Evt)
             ,StateName
             ,#state{other_leg=OtherLeg}=State
            ) ->
    lager:debug("b-leg ~s has been transferred", [OtherLeg]),
    {'next_state', StateName, handle_channel_transferee(OtherLeg, Evt, State)};

handle_event(?EVENT(CallId, EventName, _Evt)
             ,_StateName
             ,#state{call_id=CallId
                     ,listen_on='a'
                    }=State
            )
  when EventName =:= <<"CHANNEL_DESTROY">>;
       EventName =:= <<"LEG_DESTROYED">> ->
    lager:debug("a leg destroyed, finished here"),
    {'stop', 'normal', State};
handle_event(?EVENT(CallId, EventName, _Evt)
             ,_StateName
             ,#state{call_id=CallId
                     ,other_leg='undefined'
                    }=State
            )
  when EventName =:= <<"CHANNEL_DESTROY">>;
       EventName =:= <<"LEG_DESTROYED">> ->
    lager:debug("a leg destroyed, other leg down too, finished here"),
    {'stop', 'normal', State};
handle_event(?EVENT(CallId, EventName, _Evt)
             ,StateName
             ,#state{call_id=CallId}=State
            )
  when EventName =:= <<"CHANNEL_DESTROY">>;
       EventName =:= <<"LEG_DESTROYED">> ->
    lager:debug("a leg destroyed but we're not interested, ignoring"),
    {'next_state', StateName, State#state{call_id='undefined'}};

handle_event(?EVENT(OtherLeg, EventName, _Evt)
             ,_StateName
             ,#state{other_leg=OtherLeg
                     ,listen_on='b'
                    }=State
            )
  when EventName =:= <<"CHANNEL_DESTROY">>;
       EventName =:= <<"LEG_DESTROYED">> ->
    lager:debug("b leg destroyed, finished here"),
    {'stop', 'normal', State};
handle_event(?EVENT(OtherLeg, EventName, _Evt)
             ,_StateName
             ,#state{other_leg=OtherLeg
                     ,call_id='undefined'
                     ,listen_on='ab'
                    }=State
            )
  when EventName =:= <<"CHANNEL_DESTROY">>;
       EventName =:= <<"LEG_DESTROYED">> ->
    lager:debug("b leg destroyed, a leg is down too, finished here"),
    {'stop', 'normal', State};
handle_event(?EVENT(OtherLeg, EventName, _Evt)
             ,StateName
             ,#state{other_leg=OtherLeg}=State
            )
  when EventName =:= <<"CHANNEL_DESTROY">>;
       EventName =:= <<"LEG_DESTROYED">> ->
    lager:debug("b leg destroyed but we're not interested, ignoring"),
    {'next_state', StateName, State#state{other_leg='undefined'}};
handle_event(?EVENT(CallId, <<"metaflow_exe">>, Metaflow), StateName, #state{call=Call}=State) ->
    _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
    lager:debug("recv metaflow request for ~s, processing in ~p", [CallId, _Pid]),
    {'next_state', StateName, State};
handle_event(?EVENT(Target, <<"transferred">>, Evt)
             ,StateName
             ,#state{call_id=_OldCall
                     ,other_leg=Target
                     ,listen_on=ListenOn
                    }=State
            ) ->
    Transferee = wh_json:get_value(<<"Transferee">>, Evt),
    wh_util:put_callid(Transferee),

    lager:debug("other leg ~s transferred to ~s from ~s, updating to new control", [Target, Transferee, _OldCall]),

    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, Evt)),
    maybe_add_call_event_bindings(Call, ListenOn),

    {'next_state', StateName, State#state{call=whapps_call:set_other_leg_call_id(Target, Call)
                                          ,call_id=Transferee
                                         }};
handle_event(?EVENT(CallId, <<"CHANNEL_UNBRIDGE">>, Evt)
             ,StateName
             ,#state{call_id=CallId
                     ,other_leg=OtherLeg
                    }=State
            ) ->
    case wh_json:get_value(<<"Other-Leg-Call-Id">>, Evt) of
        OtherLeg ->
            lager:debug("a leg ~s has unbridged from our leg ~s", [CallId, OtherLeg]);
        _OL ->
            lager:debug("a leg ~s has unbridged from b leg ~s (ours: ~s)", [CallId, _OL, OtherLeg])
    end,
    {'next_state', StateName, handle_unbridge(CallId, State)};
handle_event(?EVENT(_CallId, _EventName, _Evt)
             ,StateName
             ,#state{call_id=_CallId
                     ,other_leg=_OtherLeg
                     ,listen_on=_LO
                    }=State
            ) ->
    lager:debug("unhandled event ~s for ~s", [_EventName, _CallId]),
    lager:debug("listen_on: '~s' call id: '~s' other leg: '~s'", [_LO, _CallId, _OtherLeg]),
    {'next_state', StateName, State};
handle_event(_Event
             ,StateName
             ,#state{call_id=_CallId
                     ,other_leg=_OtherLeg
                     ,listen_on=_LO
                    }=State
            ) ->
    lager:debug("unhandled event in ~s: ~p", [StateName, _Event]),
    lager:debug("listen_on: '~s' call id: '~s' other leg: '~s'", [_LO, _CallId, _OtherLeg]),
    {'next_state', StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

handle_info(_Info, StateName, State) ->
    lager:debug("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

terminate(_Reason, _StateName, #state{call_id=CallId
                                      ,other_leg=OtherLeg
                                     }) ->
    konami_event_listener:rm_call_binding(CallId),
    konami_event_listener:rm_call_binding(OtherLeg),
    konami_event_listener:rm_konami_binding(CallId),
    konami_event_listener:rm_konami_binding(OtherLeg),
    lager:debug("fsm terminating while in ~s: ~p", [_StateName, _Reason]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

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
        Patterns ->
            lager:debug("loading patterns from api: ~p", [Patterns]),
            Patterns
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

-define(B_LEG_EVENTS, [<<"DTMF">>, <<"CHANNEL_ANSWER">>
                       ,<<"CHANNEL_BRIDGE">>, <<"CHANNEL_TRANSFEREE">>
                       ,<<"CHANNEL_REPLACED">>
                      ]).

-spec listen_on(whapps_call:call(), wh_json:object()) -> 'a' | 'b' | 'ab'.
listen_on(Call, JObj) ->
    IsALegEndpoint = is_a_leg(Call, JObj),

    case wh_json:get_value(<<"Listen-On">>, JObj) of
        <<"both">> ->
            konami_util:listen_on_other_leg(Call, ?B_LEG_EVENTS),
            'ab';
        <<"self">> when IsALegEndpoint -> 'a';
        <<"self">> ->
            konami_util:listen_on_other_leg(Call, ?B_LEG_EVENTS),
            'b';
        <<"peer">> when IsALegEndpoint ->
            konami_util:listen_on_other_leg(Call, ?B_LEG_EVENTS),
            'b';
        <<"peer">> -> 'a';
        _ when IsALegEndpoint -> 'a';
        _ ->
            konami_util:listen_on_other_leg(Call, ?B_LEG_EVENTS),
            'b'
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

has_pattern(_Collected, _Ps, []) -> 'false';
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
maybe_cancel_timer(_) -> 'ok'.

-spec maybe_handle_aleg_code(state()) -> 'ok'.
maybe_handle_aleg_code(#state{numbers=Ns
                              ,patterns=Ps
                              ,a_collected_dtmf = Collected
                              ,call=Call
                              ,call_id=CallId
                             }) ->
    lager:debug("a DTMF timeout, let's check '~s'", [Collected]),
    case has_metaflow(Collected, Ns, Ps) of
        'false' -> lager:debug("no handler for '~s', unarming", [Collected]);
        {'number', N} -> handle_number_metaflow(Call, N, CallId);
        {'pattern', P} -> handle_pattern_metaflow(Call, P, CallId)
    end.

-spec maybe_handle_bleg_code(state()) -> 'ok'.
maybe_handle_bleg_code(#state{numbers=Ns
                              ,patterns=Ps
                              ,b_collected_dtmf = Collected
                              ,call=Call
                              ,other_leg=OtherLeg
                             }) ->
    lager:debug("b DTMF timeout, let's check '~s'", [Collected]),
    case has_metaflow(Collected, Ns, Ps) of
        'false' -> lager:debug("no handler for '~s', unarming", [Collected]);
        {'number', N} -> handle_number_metaflow(Call, N, OtherLeg);
        {'pattern', P} -> handle_pattern_metaflow(Call, P, OtherLeg)
    end.

-spec handle_number_metaflow(whapps_call:call(), wh_json:object(), ne_binary()) -> 'ok'.
handle_number_metaflow(Call, N, DTMFLeg) ->
    Metaflow = wh_json:set_values([{[<<"data">>, <<"dtmf_leg">>], DTMFLeg}], N),
    _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
    lager:debug("number exe in ~p: ~p", [_Pid, Metaflow]).

-spec handle_pattern_metaflow(whapps_call:call(), wh_json:object(), ne_binary()) -> 'ok'.
handle_pattern_metaflow(Call, P, DTMFLeg) ->
    Metaflow = wh_json:set_values([{[<<"data">>, <<"dtmf_leg">>], DTMFLeg}], P),
    _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
    lager:debug("pattern exe in ~p: ~p", [_Pid, Metaflow]).

-spec arm_aleg(state()) -> state().
arm_aleg(#state{digit_timeout=Timeout}=State) ->
    State#state{a_digit_timeout_ref = gen_fsm:start_timer(Timeout, 'digit_timeout')
                ,a_collected_dtmf = <<>>
               }.

-spec arm_bleg(state()) -> state().
arm_bleg(#state{digit_timeout=Timeout}=State) ->
    State#state{b_digit_timeout_ref = gen_fsm:start_timer(Timeout, 'digit_timeout')
                ,b_collected_dtmf = <<>>
               }.

-spec add_aleg_dtmf(state(), ne_binary()) -> state().
add_aleg_dtmf(#state{a_collected_dtmf=Collected
                     ,a_digit_timeout_ref=OldRef
                     ,digit_timeout=Timeout
                    }=State, DTMF) ->
    lager:debug("a recv dtmf '~s' while armed, adding to '~s'", [DTMF, Collected]),
    gen_fsm:cancel_timer(OldRef),
    State#state{a_digit_timeout_ref = gen_fsm:start_timer(Timeout, 'digit_timeout')
                ,a_collected_dtmf = <<Collected/binary, DTMF/binary>>
               }.

-spec add_bleg_dtmf(state(), ne_binary()) -> state().
add_bleg_dtmf(#state{b_collected_dtmf=Collected
                     ,b_digit_timeout_ref=OldRef
                     ,digit_timeout=Timeout
                    }=State, DTMF) ->
    lager:debug("b recv dtmf '~s' while armed, adding to '~s'", [DTMF, Collected]),
    gen_fsm:cancel_timer(OldRef),
    State#state{b_digit_timeout_ref = gen_fsm:start_timer(Timeout, 'digit_timeout')
                ,b_collected_dtmf = <<Collected/binary, DTMF/binary>>
               }.

-spec handle_channel_bridged(state(), wh_json:object(), ne_binary(), ne_binary()) -> state().
handle_channel_bridged(#state{call=Call
                              ,call_id=CallId
                              ,listen_on=ListenOn
                              ,other_leg=OtherLeg
                              ,b_endpoint_id=BEndpointId
                             }=State
                       ,_JObj
                       ,CallId
                       ,OtherLeg
                      ) ->
    lager:debug("a leg ~s bridged to our b leg ~s (~s)", [CallId, OtherLeg, BEndpointId]),
    maybe_bind_to_other_leg(OtherLeg, ListenOn),
    State#state{other_leg=OtherLeg
                ,call=whapps_call:set_other_leg_call_id(OtherLeg, Call)
               };
handle_channel_bridged(#state{other_leg='undefined', call_id=_CL}, _JObj, _CallId, _OtherLeg) ->
    lager:debug("ignoring bridge from ~s -> ~s when tracking ~s and no b leg, exiting"
                ,[_CallId, _OtherLeg, _CL]
               ),
    exit('normal');
handle_channel_bridged(#state{other_leg=_OL, call_id=_CL}=State, _JObj, _CallId, _OtherLeg) ->
    lager:debug("ignoring bridge from ~s -> ~s when tracking ~s and ~s"
                ,[_CallId, _OtherLeg, _CL, _OL]
               ),
    State.


-spec maybe_bind_to_other_leg(ne_binary(), listen_on()) -> 'ok'.
maybe_bind_to_other_leg(_OtherLeg, 'a') -> 'ok';
maybe_bind_to_other_leg(OtherLeg, _ListenOn) ->
    maybe_add_call_event_bindings(OtherLeg),
    konami_event_listener:add_konami_binding(OtherLeg),
    lager:debug("bound for call and konami events").

-spec handle_channel_answered(ne_binary(), wh_json:object(), state()) -> state().
handle_channel_answered(CallId, _Evt, #state{call_id=CallId}=State) ->
    State;
handle_channel_answered(CallId, Evt, #state{call_id='undefined'
                                            ,other_leg=CallId
                                           }=State) ->
    OL = wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt),
    lager:debug("no a-leg call-id"),
    lager:debug("other leg: ~s evt other: ~s", [CallId, OL]),
    State;
handle_channel_answered(OtherLeg, Evt, #state{b_endpoint_id=EndpointId
                                              ,call=Call
                                             }=State) ->
    case authorizing_id(Evt) of
        EndpointId ->
            lager:debug("b leg ~s for endpoint ~s has answered", [OtherLeg, EndpointId]),
            maybe_add_call_event_bindings(OtherLeg),
            State#state{other_leg=OtherLeg
                        ,call=whapps_call:set_other_leg_call_id(OtherLeg, Call)
                       };
        _EID ->
            lager:debug("b leg ~s for other endpoint ~s answered", [OtherLeg, _EID]),
            maybe_track_other_leg(OtherLeg, State)
    end.

-spec maybe_track_other_leg(ne_binary(), state()) -> state().
maybe_track_other_leg(OtherLeg, #state{listen_on=ListenOn
                                       ,b_endpoint_id='undefined'
                                       ,other_leg='undefined'
                                       ,call=Call
                                      }=State)
  when ListenOn =:= 'a';
       ListenOn =:= 'ab' ->
    lager:debug("tracking a leg events, so noting b leg ~s", [OtherLeg]),
    maybe_add_call_event_bindings(OtherLeg),
    State#state{other_leg=OtherLeg
                ,call=whapps_call:set_other_leg_call_id(OtherLeg, Call)
               };
maybe_track_other_leg(_OtherLeg, #state{other_leg=_OurLeg}=State) ->
    lager:debug("not tracking ~s since we have ~s", [_OtherLeg, _OurLeg]),
    State.

-spec handle_channel_transferee(ne_binary(), wh_json:object(), state()) -> state().
handle_channel_transferee(CallId, Evt, #state{call_id=CallId
                                              ,other_leg=OtherLeg
                                              ,call=Call
                                             }=State) ->
    case {wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt)
          ,wh_json:get_value(<<"Target-Call-ID">>, Evt)
         }
    of
        {_OL, 'undefined'} -> State;
        {'undefined', TargetLeg} ->
            lager:debug("transferring to ~s", [TargetLeg]),

            maybe_add_call_event_bindings(TargetLeg),
            konami_event_listener:add_konami_binding(TargetLeg),

            State#state{other_leg=TargetLeg
                        ,call=whapps_call:set_other_leg_call_id(TargetLeg, Call)
                       };
        {OtherLeg, TargetLeg} ->
            lager:debug("transferring from ~s to ~s", [OtherLeg, TargetLeg]),
            maybe_remove_call_event_bindings(OtherLeg),

            maybe_add_call_event_bindings(TargetLeg),
            konami_event_listener:add_konami_binding(TargetLeg),

            State#state{other_leg=TargetLeg
                        ,call=whapps_call:set_other_leg_call_id(TargetLeg, Call)
                       };
        {_OL, _TL} ->
            lager:debug("ignoring transfer of ~s to ~s", [_OL, _TL]),
            State
    end;
handle_channel_transferee(OtherLeg, Evt, #state{call_id=Call
                                                ,other_leg=OtherLeg
                                                ,call=Call
                                               }=State) ->
    case {wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt)
          ,wh_json:get_value(<<"Target-Call-ID">>, Evt)
         }
    of
        {_OL, 'undefined'} -> State;
        {'undefined', TargetLeg} ->
            lager:debug("transferring to ~s", [TargetLeg]),

            maybe_add_call_event_bindings(TargetLeg),
            konami_event_listener:add_konami_binding(TargetLeg),

            State#state{call_id=TargetLeg
                        ,call=whapps_call:set_call_id(TargetLeg, Call)
                       };
        {OtherLeg, TargetLeg} ->
            lager:debug("transferring from ~s to ~s", [OtherLeg, TargetLeg]),
            maybe_remove_call_event_bindings(OtherLeg),

            maybe_add_call_event_bindings(TargetLeg),
            konami_event_listener:add_konami_binding(TargetLeg),

            State#state{call_id=TargetLeg
                        ,call=whapps_call:set_call_id(TargetLeg, Call)
                       };
        {_OL, _TL} ->
            lager:debug("ignoring transfer of ~s to ~s", [_OL, _TL]),
            State
    end.

-spec maybe_add_call_event_bindings(api_binary() | whapps_call:call()) -> 'ok'.
-spec maybe_add_call_event_bindings(whapps_call:call(), listen_on()) -> 'ok'.
maybe_add_call_event_bindings('undefined') -> 'ok';
maybe_add_call_event_bindings(<<_/binary>> = Leg) -> konami_event_listener:add_call_binding(Leg);
maybe_add_call_event_bindings(Call) -> konami_event_listener:add_call_binding(Call).

maybe_add_call_event_bindings(Call, 'a') ->
    konami_event_listener:add_konami_binding(whapps_call:call_id(Call)),
    maybe_add_call_event_bindings(Call);
maybe_add_call_event_bindings(Call, 'b') ->
    konami_event_listener:add_konami_binding(whapps_call:other_leg_call_id(Call)),
    maybe_add_call_event_bindings(Call);
maybe_add_call_event_bindings(Call, 'ab') ->
    konami_event_listener:add_konami_binding(whapps_call:call_id(Call)),
    konami_event_listener:add_konami_binding(whapps_call:other_leg_call_id(Call)),
    maybe_add_call_event_bindings(Call).

-spec maybe_remove_call_event_bindings(api_binary()) -> 'ok'.
maybe_remove_call_event_bindings('undefined') -> 'ok';
maybe_remove_call_event_bindings(Leg) ->
    konami_event_listener:rm_call_binding(Leg).

-spec b_endpoint_id(wh_json:object(), listen_on()) -> api_binary().
b_endpoint_id(_JObj, 'a') -> 'undefined';
b_endpoint_id(JObj, _ListenOn) -> wh_json:get_value(<<"Endpoint-ID">>, JObj).

-spec authorizing_id(wh_json:object()) -> api_binary().
authorizing_id(JObj) ->
    wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], JObj).

handle_unbridge(CallId, #state{listen_on='b'
                               ,call_id=CallId
                               ,other_leg=OtherLeg
                               ,call=Call
                              }=State) ->
    lager:debug("a leg ~s unbridged, stop listening for its events", [CallId]),
    _ = maybe_remove_call_event_bindings(OtherLeg),
    State#state{call_id='undefined'
                ,call=whapps_call:set_call_id('undefined', Call)
               };
handle_unbridge(CallId, #state{call_id=CallId
                               ,other_leg=OtherLeg
                               ,call=Call
                              }=State) ->
    lager:debug("stopping other leg ~s events", [OtherLeg]),
    _ = maybe_remove_call_event_bindings(OtherLeg),
    State#state{other_leg='undefined'
                ,call=whapps_call:set_other_leg_call_id('undefined', Call)
               }.
