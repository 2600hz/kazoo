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
                ,binding_key = konami_config:binding_key() :: ne_binary()
                ,digit_timeout = konami_config:timeout() :: pos_integer()
                ,call :: whapps_call:call()

                ,listen_on = 'a' :: 'a' | 'b' | 'ab'

                ,a_digit_timeout_ref :: api_reference()
                ,a_collected_dtmf = <<>> :: binary()

                ,b_digit_timeout_ref :: api_reference()
                ,b_collected_dtmf = <<>> :: binary()

                ,call_id :: ne_binary()
                ,other_leg :: api_binary()
                ,other_leg_endpoint_id :: api_binary()
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

    konami_tracker:track(whapps_call:call_id(Call), whapps_call:other_leg_call_id(Call), ListenOn),

    lager:debug("starting code FSM, listening on ~s(~s)", [ListenOn, whapps_call:bridge_id(Call)]),
    gen_fsm:enter_loop(?MODULE, [], 'unarmed'
                       ,#state{numbers=numbers(Call, JObj)
                               ,patterns=patterns(Call, JObj)
                               ,binding_key=binding_key(Call, JObj)
                               ,digit_timeout=digit_timeout(Call, JObj)
                               ,listen_on=ListenOn

                               ,call=whapps_call:clear_helpers(
                                       whapps_call:kvs_store(?MODULE, self(), Call)
                                      )
                               ,call_id=whapps_call:call_id_direct(Call)
                               ,other_leg_endpoint_id=wh_json:get_value(<<"Endpoint-ID">>, JObj)
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

unarmed({'dtmf', CallId, BindingKey}, #state{call_id=CallId
                                             ,listen_on='a'
                                             ,binding_key=BindingKey
                                             ,digit_timeout=Timeout
                                            }=State) ->
    lager:debug("recv binding key ~s, arming (~bms)", [BindingKey, Timeout]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{a_digit_timeout_ref = Ref
                                        ,a_collected_dtmf = <<>>
                                       }};
unarmed({'dtmf', CallId, BindingKey}, #state{call_id=CallId
                                             ,listen_on='ab'
                                             ,binding_key=BindingKey
                                             ,digit_timeout=Timeout
                                            }=State) ->
    lager:debug("recv binding key ~s, arming (~bms)", [BindingKey, Timeout]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{a_digit_timeout_ref = Ref
                                        ,a_collected_dtmf = <<>>
                                       }};
unarmed({'dtmf', CallId, BindingKey}, #state{other_leg=CallId
                                             ,listen_on='ab'
                                             ,binding_key=BindingKey
                                             ,digit_timeout=Timeout
                                            }=State) ->
    lager:debug("recv binding key '~s' for other leg ~s arming (~bms)", [BindingKey, CallId, Timeout]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{b_digit_timeout_ref = Ref
                                        ,b_collected_dtmf = <<>>
                                       }};
unarmed({'dtmf', CallId, BindingKey}, #state{other_leg=CallId
                                             ,listen_on='b'
                                             ,binding_key=BindingKey
                                             ,digit_timeout=Timeout
                                            }=State) ->
    lager:debug("recv binding key '~s' for other leg ~s arming (~bms)", [BindingKey, CallId, Timeout]),
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
                                         }};

handle_event({'event', CallId, <<"CHANNEL_BRIDGE">>, JObj}, StateName, #state{call_id=CallId
                                                                              ,other_leg='undefined'
                                                                              ,call=Call
                                                                             }=State) ->
    OtherLeg = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
    konami_tracker:update_other_leg(OtherLeg),
    lager:debug("channel has been bridged to ~s", [OtherLeg]),
    {'next_state', StateName, State#state{other_leg=OtherLeg
                                          ,call=whapps_call:set_other_leg_call_id(OtherLeg, Call)
                                         }};
handle_event({'event', CallId, <<"CHANNEL_BRIDGE">>, JObj}, StateName, #state{call_id=CallId
                                                                              ,other_leg=OtherLeg
                                                                              ,call=Call
                                                                             }=State) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj) of
        OtherLeg ->
            konami_tracker:update_other_leg(OtherLeg),
            lager:debug("channel is bridged to ~s", [OtherLeg]),
            {'next_state', StateName, State};
        NewOtherLeg ->
            lager:debug("channel is now bridged to ~s (was ~s)", [NewOtherLeg, OtherLeg]),
            konami_event_listener:add_call_binding(NewOtherLeg),
            konami_event_listener:rm_call_binding(OtherLeg),
            konami_tracker:update_other_leg(NewOtherLeg),
            {'next_state', StateName, State#state{other_leg=NewOtherLeg
                                                  ,call=whapps_call:set_other_leg_call_id(NewOtherLeg, Call)
                                                 }}
    end;

handle_event({'event', OtherLeg, <<"CHANNEL_BRIDGE">>, JObj}, StateName, #state{other_leg='undefined'
                                                                                ,call=Call
                                                                                ,call_id=CallId
                                                                               }=State) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj) of
        CallId ->
            lager:debug("b leg ~s bridged to us", [OtherLeg]),
            {'next_state', StateName, State#state{other_leg=OtherLeg
                                                  ,call=whapps_call:set_other_leg_call_id(OtherLeg, Call)
                                                 }};
        _Id -> {'next_state', StateName, State}
    end;
handle_event({'event', OtherLeg, <<"CHANNEL_BRIDGE">>, _JObj}, StateName, #state{other_leg=OtherLeg}=State) ->
    lager:debug("b leg ~s bridged to us", [OtherLeg]),
    {'next_state', StateName, State};

handle_event({'event', NewOtherLeg, <<"CHANNEL_BRIDGE">>, JObj}, StateName, #state{call_id=CallId
                                                                                ,other_leg=OldOtherLeg
                                                                                ,call=Call
                                                                               }=State) ->
    case wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj) of
        CallId ->
            lager:debug("we have a new b-leg ~s (was ~s)", [NewOtherLeg, OldOtherLeg]),
            konami_event_listener:add_call_binding(NewOtherLeg),
            konami_event_listener:rm_call_binding(OldOtherLeg),
            konami_tracker:update_other_leg(NewOtherLeg),
            {'next_state', StateName, State#state{other_leg=NewOtherLeg
                                                  ,call=whapps_call:set_other_leg_call_id(NewOtherLeg, Call)
                                                 }};
        _Id -> {'next_state', StateName, State}
    end;
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

handle_info(?HOOK_EVT(_AccountId, <<"CHANNEL_ANSWER">>, Evt), StateName, #state{call_id=CallId
                                                                                ,other_leg=OtherLeg
                                                                                ,call=Call
                                                                               }=State) ->
    lager:debug("answer: ~s(~s) other ~s", [wh_json:get_value(<<"Call-ID">>, Evt)
                                            ,wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Bridge-ID">>], Evt)
                                            ,wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt)
                                           ]),

    case {wh_json:get_value(<<"Call-ID">>, Evt)
          ,wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt)
         }
    of
        {CallId, 'undefined'} ->
            lager:debug("channel is answered (~s)", [OtherLeg]),
            {'next_state', StateName, State};
        {CallId, OtherLeg} ->
            konami_tracker:update_other_leg(OtherLeg),
            lager:debug("channel is answered (~s)", [OtherLeg]),
            {'next_state', StateName, State};
        {CallId, NewOtherLeg} ->
            lager:debug("channel is now bridged to ~s (was ~s)", [NewOtherLeg, OtherLeg]),
            konami_event_listener:add_call_binding(NewOtherLeg),
            konami_tracker:update_other_leg(NewOtherLeg),
            {'next_state', StateName, State#state{other_leg=NewOtherLeg
                                                  ,call=whapps_call:set_other_leg_call_id(NewOtherLeg, Call)
                                                 }};

        {OtherLeg, CallId} ->
            lager:debug("b leg ~s is answered", [OtherLeg]),
            konami_tracker:update_other_leg(OtherLeg),
            {'next_state', StateName, State};
        {OtherLeg, YetAnotherLeg} ->
            lager:debug("yet another leg ~s for other leg ~s", [YetAnotherLeg, OtherLeg]),
            {'next_state', StateName, State};
        {NewOtherLeg, CallId} ->
            konami_event_listener:add_call_binding(NewOtherLeg),
            konami_event_listener:rm_call_binding(OtherLeg),
            lager:debug("new b leg ~s is answered (was ~s)", [NewOtherLeg, OtherLeg]),

            konami_tracker:update_other_leg(NewOtherLeg),

            {'next_state', StateName, State#state{other_leg=NewOtherLeg
                                                  ,call=whapps_call:set_other_leg_call_id(NewOtherLeg, Call)
                                                 }};
        {NewOtherLeg, 'undefined'} ->
            lager:debug("recv new other leg ~s while bridge = ~s", [NewOtherLeg, whapps_call:bridge_id(Call)]),
            {'next_state', StateName, State}
    end;
handle_info(?HOOK_EVT(_AccountId, <<"CHANNEL_DESTROY">>, Evt), StateName, #state{call_id=CallId
                                                                                 ,other_leg=OtherLeg
                                                                                 ,listen_on=ListenOn
                                                                                }=State) ->
    case should_stop_fsm(CallId, OtherLeg, ListenOn
                         ,wh_json:get_value(<<"Call-ID">>, Evt)
                        )
    of
        'true' -> {'stop', 'normal', State};
        'false' -> {'next_state', StateName, State}
    end;
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec binding_key(whapps_call:call(), wh_json:object()) -> ne_binary().
binding_key(Call, JObj) ->
    case wh_json:get_value(<<"Binding-Key">>, JObj) of
        'undefined' ->
            konami_config:binding_key(whapps_call:account_id(Call));
        BindingKey ->
            lager:debug("using custom binding key '~s'", [BindingKey]),
            BindingKey
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

-spec should_stop_fsm(ne_binary(), api_binary(), 'a' | 'ab' | 'b', ne_binary()) -> boolean().
should_stop_fsm(EventCallId, _OtherLeg, 'a', EventCallId) ->
    lager:debug("recv a-leg channel_destroy, going down"),
    konami_event_listener:rm_call_binding(EventCallId),
    'true';
should_stop_fsm(EventCallId, OtherLeg, 'ab', EventCallId) ->
    lager:debug("recv a-leg channel_destroy, going down"),
    konami_event_listener:rm_call_binding(EventCallId),
    konami_event_listener:rm_call_binding(OtherLeg),
    'true';
should_stop_fsm(_CallId, EventLeg, 'b', EventLeg) ->
    lager:debug("recv b-leg ~s channel_destroy, going down", [EventLeg]),
    konami_event_listener:rm_call_binding(EventLeg),
    'true';
should_stop_fsm(_CallId, _OtherLeg, _ListenOn, _EventLeg) ->
    lager:debug("recv event-leg ~s channel_destroy while listening on ~s for ~s->~s, ignoring"
                ,[_EventLeg, _ListenOn, _CallId, _OtherLeg]
               ),
    'false'.
