%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz
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
        ,format_status/2
        ]).

-include("konami.hrl").

-type listen_on() :: 'a' | 'b' | 'ab'.

-record(state, {numbers :: api_object()
               ,patterns :: api_object()
               ,binding_digit = konami_config:binding_digit() :: ne_binary()
               ,digit_timeout = konami_config:timeout() :: pos_integer()
               ,call :: kapps_call:call() | 'undefined'

               ,listen_on = 'a' :: listen_on()

               ,a_digit_timeout_ref :: api_reference()
               ,a_collected_dtmf = <<>> :: binary()
               ,a_leg_armed = 'false' :: boolean()

               ,b_digit_timeout_ref :: api_reference()
               ,b_collected_dtmf = <<>> :: binary()
               ,b_endpoint_id :: api_binary()
               ,b_leg_armed = 'false' :: boolean()

               ,call_id :: ne_binary()
               ,other_leg :: api_binary()
               }).
-type state() :: #state{}.

-define(WSD_ID, ?WSD_ENABLED
        andalso {'file', get('callid')}
       ).

-define(WSD_EVT(Fr, T, E), ?WSD_ENABLED
        andalso webseq:evt(?WSD_ID, Fr, T, <<(kz_util:to_binary(?LINE))/binary, "-", E/binary>>)
       ).

-define(WSD_NOTE(W, D, N), ?WSD_ENABLED
        andalso webseq:note(?WSD_ID, W, D, <<(kz_util:to_binary(?LINE))/binary, "-", N/binary>>)
       ).

-define(WSD_TITLE(T), ?WSD_ENABLED
        andalso webseq:title(?WSD_ID, [T, " in ", kz_util:to_binary(self())])
       ).
-define(WSD_START(), ?WSD_ENABLED
        andalso webseq:start(?WSD_ID)
       ).

-define(WSD_STOP(), ?WSD_ENABLED
        andalso webseq:stop(?WSD_ID)
       ).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_fsm(kapps_call:call(), kz_json:object()) -> any().
start_fsm(Call, JObj) ->
    gen_fsm:start_link(?MODULE, {Call, JObj}, []).

-spec event(pid(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
event(FSM, CallId, <<"DTMF">>, JObj) ->
    gen_fsm:send_event(FSM, {'dtmf'
                            ,CallId
                            ,kz_call_event:dtmf_digit(JObj)
                            });
event(FSM, CallId, Event, JObj) ->
    gen_fsm:send_all_state_event(FSM, ?EVENT(CallId, Event, JObj)).

-spec transfer_to(kapps_call:call(), 'a' | 'b') -> 'ok'.
transfer_to(Call, Leg) ->
    gen_fsm:send_all_state_event(kapps_call:kvs_fetch(?MODULE, Call)
                                ,{'transfer_to', Call, Leg}
                                ).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-spec init({kapps_call:call(), kz_json:object()}) -> {'ok', 'unarmed', state()}.
init({Call, JObj}) ->
    ListenOn = listen_on(Call, JObj),

    kapps_call:put_callid(Call),

    maybe_add_call_event_bindings(Call, ListenOn),

    lager:debug("starting Konami FSM, listening on '~s'", [ListenOn]),

    BEndpointId = b_endpoint_id(JObj, ListenOn),

    lager:debug("a endpoint: ~s b endpoint: ~s", [kapps_call:authorizing_id(Call), BEndpointId]),

    ?WSD_START(),
    ?WSD_TITLE(["FSM: ", kapps_call:call_id(Call), " listen on: ", kz_util:to_list(ListenOn)]),

    {'ok', 'unarmed', #state{numbers=numbers(Call, JObj)
                            ,patterns=patterns(Call, JObj)
                            ,binding_digit=binding_digit(Call, JObj)
                            ,digit_timeout=digit_timeout(Call, JObj)

                            ,listen_on=ListenOn

                            ,call=kapps_call:clear_helpers(
                                    kapps_call:kvs_store(?MODULE, self(), Call)
                                   )
                            ,call_id=kapps_call:call_id_direct(Call)

                            ,b_endpoint_id=BEndpointId
                            }}.

-spec unarmed(any(), state()) -> handle_fsm_ret(state()).
-spec unarmed(any(), any(), state()) -> handle_fsm_ret(state()).
unarmed({'dtmf', CallId, BindingDigit}, #state{call_id=CallId
                                              ,listen_on='a'
                                              ,binding_digit=BindingDigit
                                              }=State) ->
    lager:debug("recv binding digit ~s, arming a-leg", [BindingDigit]),
    ?WSD_NOTE(CallId, 'right', <<"arming">>),
    {'next_state', 'armed', arm_aleg(State)};
unarmed({'dtmf', CallId, BindingDigit}, #state{call_id=CallId
                                              ,listen_on='ab'
                                              ,binding_digit=BindingDigit
                                              }=State) ->
    lager:debug("recv binding digit ~s, arming a leg", [BindingDigit]),
    ?WSD_NOTE(CallId, 'right', <<"arming">>),
    {'next_state', 'armed', arm_aleg(State)};

unarmed({'dtmf', CallId, BindingDigit}, #state{other_leg=CallId
                                              ,listen_on='ab'
                                              ,binding_digit=BindingDigit
                                              }=State) ->
    lager:debug("recv binding digit '~s', arming b leg ~s", [BindingDigit, CallId]),
    ?WSD_NOTE(CallId, 'right', <<"arming">>),
    {'next_state', 'armed', arm_bleg(State)};
unarmed({'dtmf', CallId, BindingDigit}, #state{other_leg=CallId
                                              ,listen_on='b'
                                              ,binding_digit=BindingDigit
                                              }=State) ->
    lager:debug("recv binding digit '~s', arming b leg ~s", [BindingDigit, CallId]),
    ?WSD_NOTE(CallId, 'right', <<"arming">>),
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

-spec armed(any(), state()) -> handle_fsm_ret(state()).
-spec armed(any(), any(), state()) -> handle_fsm_ret(state()).
armed({'dtmf', CallId, DTMF}, #state{call_id=CallId
                                    ,a_leg_armed='true'
                                    }=State) ->
    {'next_state', 'armed', add_aleg_dtmf(State, DTMF)};
armed({'dtmf', CallId, DTMF}, #state{other_leg=CallId
                                    ,b_leg_armed='true'
                                    }=State) ->
    {'next_state', 'armed', add_bleg_dtmf(State, DTMF)};
armed({'timeout', Ref, 'digit_timeout'}, #state{a_digit_timeout_ref = Ref
                                               ,call_id=_CallId
                                               }=State) ->
    lager:debug("disarming 'a' leg"),
    _ = maybe_handle_aleg_code(State),
    ?WSD_NOTE(_CallId, 'right', <<"disarming">>),
    {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
armed({'timeout', Ref, 'digit_timeout'}, #state{b_digit_timeout_ref = Ref
                                               ,other_leg=_OtherLeg
                                               }=State) ->
    lager:debug("disarming 'b' leg"),
    _ = maybe_handle_bleg_code(State),
    ?WSD_NOTE(_OtherLeg, 'right', <<"disarming">>),
    {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
armed(_Event, State) ->
    lager:debug("unhandled armed/2: ~p", [_Event]),
    {'next_state', 'armed', State}.

armed(_Event, _From, State) ->
    lager:debug("unhandled armed/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'armed', State}.

-spec handle_event(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_event(?EVENT(CallId, <<"metaflow_exe">>, Metaflow), StateName, #state{call=Call}=State) ->
    _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
    lager:debug("recv metaflow exe request for ~s, processing in ~p", [CallId, _Pid]),
    {'next_state', StateName, State};
handle_event(?EVENT(_CallId, <<"CHANNEL_ANSWER">>, Evt)
            ,StateName
            ,State
            ) ->
    {'next_state', StateName, handle_channel_answer(State, kz_call_event:call_id(Evt), Evt)};
handle_event(?EVENT(CallId, <<"CHANNEL_BRIDGE">>, Evt)
            ,StateName
            ,#state{call_id=CallId}=State
            ) ->
    {'next_state', StateName, handle_channel_bridge(State, CallId, kz_call_event:other_leg_call_id(Evt))};
handle_event(?EVENT(OtherLeg, <<"CHANNEL_BRIDGE">>, Evt)
            ,StateName
            ,#state{other_leg=OtherLeg}=State
            ) ->
    {'next_state', StateName, handle_channel_bridge(State, kz_call_event:other_leg_call_id(Evt), OtherLeg)};
handle_event(?EVENT(CallId, <<"CHANNEL_DESTROY">>, _Evt)
            ,StateName
            ,#state{call_id=CallId}=State
            ) ->
    {'next_state', StateName, handle_channel_destroy(State, CallId)};
handle_event(?EVENT(OtherLeg, <<"CHANNEL_DESTROY">>, _Evt)
            ,StateName
            ,#state{other_leg=OtherLeg}=State
            ) ->
    {'next_state', StateName, handle_channel_destroy(State, OtherLeg)};
handle_event(?EVENT(_UUID, _EventName, _Evt)
            ,StateName
            ,#state{call_id=_CallId
                   ,other_leg=_OtherLeg
                   ,listen_on=_LO
                   }=State
            ) ->
    lager:debug("unhandled event ~s for ~s (~s)"
               ,[_EventName, _UUID, kz_call_event:other_leg_call_id(_Evt)]
               ),
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

-spec handle_sync_event(any(), {pid(),any()}, atom(), state()) -> handle_sync_event_ret(state()).
handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

-spec handle_info(state(), atom(), state()) -> handle_fsm_ret(state()).
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

-spec terminate(any(), atom(), state()) -> 'ok'.
terminate(_Reason, _StateName, #state{call_id=CallId
                                     ,other_leg=OtherLeg
                                     }) ->
    konami_event_listener:rm_call_binding(CallId),
    konami_event_listener:rm_call_binding(OtherLeg),
    konami_event_listener:rm_konami_binding(CallId),
    konami_event_listener:rm_konami_binding(OtherLeg),
    ?WSD_STOP(),
    lager:debug("fsm terminating while in ~s: ~p", [_StateName, _Reason]).

-spec code_change(any(), atom(), state(), any()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

-spec format_status(any(), any()) -> any().
format_status(_, [_Dict, #state{call_id=CallId
                               ,other_leg=OtherLeg
                               }]) ->
    [{'data', [{"StateData", {CallId, OtherLeg}}]}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec binding_digit(kapps_call:call(), kz_json:object()) -> ne_binary().
binding_digit(Call, JObj) ->
    case kz_json:get_value(<<"Binding-Digit">>, JObj) of
        'undefined' ->
            konami_config:binding_digit(kapps_call:account_id(Call));
        BindingDigit ->
            lager:debug("using custom binding digit '~s'", [BindingDigit]),
            BindingDigit
    end.

-spec numbers(kapps_call:call(), kz_json:object()) -> kz_json:object().
numbers(Call, JObj) ->
    case kz_json:get_value(<<"Numbers">>, JObj) of
        'undefined' ->
            lager:debug("loading default account metaflow numbers"),
            konami_config:numbers(kapps_call:account_id(Call));
        Numbers ->
            lager:debug("loading numbers from api: ~p", [Numbers]),
            Numbers
    end.

-spec patterns(kapps_call:call(), kz_json:object()) -> kz_json:object().
patterns(Call, JObj) ->
    case kz_json:get_value(<<"Patterns">>, JObj) of
        'undefined' -> konami_config:patterns(kapps_call:account_id(Call));
        Patterns ->
            lager:debug("loading patterns from api: ~p", [Patterns]),
            Patterns
    end.

-spec digit_timeout(kapps_call:call(), kz_json:object()) -> pos_integer().
digit_timeout(Call, JObj) ->
    case kz_json:get_integer_value(<<"Digit-Timeout">>, JObj) of
        'undefined' -> konami_config:timeout(kapps_call:account_id(Call));
        Timeout -> Timeout
    end.

-spec is_a_leg(kapps_call:call(), api_object() | ne_binary()) -> boolean().
is_a_leg(_Call, 'undefined') -> 'true';
is_a_leg(Call, <<_/binary>> = EndpointId) ->
    EndpointId =:= kapps_call:authorizing_id(Call);
is_a_leg(Call, JObj) ->
    is_a_leg(Call, kz_json:get_value(<<"Endpoint-ID">>, JObj)).

-define(B_LEG_EVENTS, [<<"DTMF">>, <<"CHANNEL_ANSWER">>
                      ,<<"CHANNEL_BRIDGE">>, <<"CHANNEL_TRANSFEREE">>
                      ,<<"CHANNEL_REPLACED">>
                      ]).

-spec listen_on(kapps_call:call(), kz_json:object()) -> 'a' | 'b' | 'ab'.
listen_on(Call, JObj) ->
    IsALegEndpoint = is_a_leg(Call, JObj),

    case kz_json:get_value(<<"Listen-On">>, JObj) of
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

-spec has_metaflow(ne_binary(), kz_json:object(), kz_json:object()) ->
                          'false' |
                          {'number', kz_json:object()} |
                          {'patterm', kz_json:object()}.
has_metaflow(Collected, Ns, Ps) ->
    case has_number(Collected, Ns) of
        'false' -> has_pattern(Collected, Ps);
        N -> N
    end.

-spec has_number(ne_binary(), kz_json:object()) ->
                        'false' |
                        {'number', kz_json:object()}.
has_number(Collected, Ns) ->
    case kz_json:get_value(Collected, Ns) of
        'undefined' -> 'false';
        N -> {'number', N}
    end.

-spec has_pattern(ne_binary(), kz_json:object()) ->
                         'false' |
                         {'pattern', kz_json:object()}.
has_pattern(Collected, Ps) ->
    Regexes = kz_json:get_keys(Ps),
    has_pattern(Collected, Ps, Regexes).

has_pattern(_Collected, _Ps, []) -> 'false';
has_pattern(Collected, Ps, [Regex|Regexes]) ->
    case re:run(Collected, Regex, [{'capture', 'all_but_first', 'binary'}]) of
        'nomatch' -> has_pattern(Collected, Ps, Regexes);
        {'match', Captured} ->
            P = kz_json:get_value(Regex, Ps),
            {'pattern', kz_json:set_values([{[<<"data">>, <<"collected">>], Collected}
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
               ,a_leg_armed = 'false'
               ,b_digit_timeout_ref='undefined'
               ,b_collected_dtmf = <<>>
               ,b_leg_armed = 'false'
               }.

-spec maybe_cancel_timer(any()) -> 'ok'.
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

-spec handle_number_metaflow(kapps_call:call(), kz_json:object(), ne_binary()) -> 'ok'.
handle_number_metaflow(Call, N, DTMFLeg) ->
    Metaflow = kz_json:set_values([{[<<"data">>, <<"dtmf_leg">>], DTMFLeg}], N),
    _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
    ?WSD_NOTE(DTMFLeg, 'right', <<"executing number metaflow">>),
    lager:debug("number exe in ~p: ~p", [_Pid, Metaflow]).

-spec handle_pattern_metaflow(kapps_call:call(), kz_json:object(), ne_binary()) -> 'ok'.
handle_pattern_metaflow(Call, P, DTMFLeg) ->
    Metaflow = kz_json:set_values([{[<<"data">>, <<"dtmf_leg">>], DTMFLeg}], P),
    _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
    ?WSD_NOTE(DTMFLeg, 'right', <<"executing pattern metaflow">>),
    lager:debug("pattern exe in ~p: ~p", [_Pid, Metaflow]).

-spec arm_aleg(state()) -> state().
arm_aleg(#state{digit_timeout=Timeout}=State) ->
    State#state{a_digit_timeout_ref = gen_fsm:start_timer(Timeout, 'digit_timeout')
               ,a_collected_dtmf = <<>>
               ,a_leg_armed='true'
               }.

-spec arm_bleg(state()) -> state().
arm_bleg(#state{digit_timeout=Timeout}=State) ->
    State#state{b_digit_timeout_ref = gen_fsm:start_timer(Timeout, 'digit_timeout')
               ,b_collected_dtmf = <<>>
               ,b_leg_armed='true'
               }.

-spec maybe_fast_rearm(ne_binary(), ne_binary(), binary()) -> binary().
-spec maybe_fast_rearm(ne_binary(), ne_binary(), binary(), boolean()) -> binary().

maybe_fast_rearm(DTMF, BindingDigit, Collected) ->
    maybe_fast_rearm(DTMF, BindingDigit, Collected
                    ,kapps_config:get_is_true(?CONFIG_CAT, <<"use_fast_rearm">>, 'false')
                    ).

maybe_fast_rearm(DTMF, _BindingDigit, Collected, 'false') -> <<Collected/binary, DTMF/binary>>;
maybe_fast_rearm(DoubleBindingDigit, DoubleBindingDigit, <<>>, 'true') -> DoubleBindingDigit;
maybe_fast_rearm(DTMFisBindingDigit, DTMFisBindingDigit, _Collected, 'true') -> <<>>;
maybe_fast_rearm(DTMF, _BindingDigit, Collected, 'true') -> <<Collected/binary, DTMF/binary>>.

-spec add_aleg_dtmf(state(), ne_binary()) -> state().
add_aleg_dtmf(#state{a_collected_dtmf=Collected
                    ,a_digit_timeout_ref=OldRef
                    ,digit_timeout=Timeout
                    ,binding_digit=BindingDigit
                    }=State, DTMF) ->
    lager:debug("a recv dtmf '~s' while armed, adding to '~s'", [DTMF, Collected]),
    maybe_cancel_timer(OldRef),
    State#state{a_digit_timeout_ref = gen_fsm:start_timer(Timeout, 'digit_timeout')
               ,a_collected_dtmf = maybe_fast_rearm(DTMF, BindingDigit, Collected)
               }.

-spec add_bleg_dtmf(state(), ne_binary()) -> state().
add_bleg_dtmf(#state{b_collected_dtmf=Collected
                    ,b_digit_timeout_ref=OldRef
                    ,digit_timeout=Timeout
                    ,binding_digit=BindingDigit
                    }=State, DTMF) ->
    lager:debug("b recv dtmf '~s' while armed, adding to '~s'", [DTMF, Collected]),
    maybe_cancel_timer(OldRef),
    State#state{b_digit_timeout_ref = gen_fsm:start_timer(Timeout, 'digit_timeout')
               ,b_collected_dtmf = maybe_fast_rearm(DTMF, BindingDigit, Collected)
               }.

-spec maybe_add_call_event_bindings(api_binary() | kapps_call:call()) -> 'ok'.
-spec maybe_add_call_event_bindings(kapps_call:call(), listen_on()) -> 'ok'.
maybe_add_call_event_bindings('undefined') -> 'ok';
maybe_add_call_event_bindings(<<_/binary>> = Leg) -> konami_event_listener:add_call_binding(Leg);
maybe_add_call_event_bindings(Call) -> konami_event_listener:add_call_binding(Call).

maybe_add_call_event_bindings(Call, 'a') ->
    konami_event_listener:add_konami_binding(kapps_call:call_id(Call)),
    maybe_add_call_event_bindings(Call);
maybe_add_call_event_bindings(Call, 'b') ->
    konami_event_listener:add_konami_binding(kapps_call:other_leg_call_id(Call)),
    maybe_add_call_event_bindings(Call);
maybe_add_call_event_bindings(Call, 'ab') ->
    konami_event_listener:add_konami_binding(kapps_call:call_id(Call)),
    konami_event_listener:add_konami_binding(kapps_call:other_leg_call_id(Call)),
    maybe_add_call_event_bindings(Call).

-spec b_endpoint_id(kz_json:object(), listen_on()) -> api_binary().
b_endpoint_id(_JObj, 'a') -> 'undefined';
b_endpoint_id(JObj, _ListenOn) -> kz_json:get_value(<<"Endpoint-ID">>, JObj).

-spec handle_channel_answer(state(), ne_binary(), kz_json:object()) -> state().
handle_channel_answer(#state{call_id=CallId}=State, CallId, _Evt) ->
    lager:debug("'a' leg ~s answered", [CallId]),
    State;
handle_channel_answer(#state{other_leg=OtherLeg}=State, OtherLeg, _Evt) ->
    lager:debug("'b' leg ~s answered", [OtherLeg]),
    State;
handle_channel_answer(#state{other_leg='undefined'}=State
                     ,CallId
                     ,Evt
                     ) ->
    maybe_other_leg_answered(State
                            ,CallId
                            ,kz_call_event:authorizing_id(Evt)
                            );
handle_channel_answer(#state{call_id=_CallId
                            ,other_leg=_OtherLeg
                            }=State
                     ,_AnsweredId
                     ,_Evt
                     ) ->
    lager:debug("channel ~s answered while on ~s and ~s", [_AnsweredId, _CallId, _OtherLeg]),
    State.

-spec maybe_other_leg_answered(state(), ne_binary(), ne_binary()) -> state().
maybe_other_leg_answered(#state{b_endpoint_id=EndpointId
                               ,call=Call
                               }=State
                        ,OtherLeg
                        ,EndpointId
                        ) ->
    lager:debug("yay, our endpoint ~s answered on ~s", [EndpointId, OtherLeg]),
    maybe_add_call_event_bindings(OtherLeg),
    State#state{other_leg=OtherLeg
               ,call=kapps_call:set_other_leg_call_id(OtherLeg, Call)
               };
maybe_other_leg_answered(State, _CallId, _EndpointId) ->
    lager:debug("ignoring channel ~s answering for endpoint ~s", [_CallId, _EndpointId]),
    State.

-spec handle_channel_bridge(state(), ne_binary(), ne_binary()) -> state().
handle_channel_bridge(#state{call_id=CallId
                            ,other_leg=OtherLeg
                            }=State, CallId, OtherLeg) ->
    lager:debug("joy, 'a' and 'b' legs bridged"),
    State;
handle_channel_bridge(#state{call_id=CallId
                            ,listen_on='a'
                            ,call=Call
                            }=State, CallId, OtherLeg) ->
    lager:debug("joy, 'a' is bridged to ~s", [OtherLeg]),
    maybe_add_call_event_bindings(OtherLeg),
    State#state{call=kapps_call:set_other_leg_call_id(OtherLeg, Call)
               ,other_leg=OtherLeg
               };
handle_channel_bridge(#state{other_leg='undefined'}
                     ,_CallId
                     ,_OtherLeg
                     ) ->
    lager:debug("'a' leg has bridged to other leg ~s...done here", [_OtherLeg]),
    exit('normal');
handle_channel_bridge(#state{call_id=_CallId
                            ,other_leg=OtherLeg
                            ,listen_on='a'
                            }
                     ,UUID
                     ,OtherLeg
                     ) ->
    lager:debug("our 'b' leg ~s bridged to ~s instead of ~s", [OtherLeg, UUID, _CallId]),
    exit('normal');
handle_channel_bridge(#state{call_id=_CallId
                            ,other_leg=OtherLeg
                            ,call=Call
                            }=State
                     ,UUID
                     ,OtherLeg
                     ) ->
    lager:debug("our 'b' leg ~s bridged to ~s instead of ~s", [OtherLeg, UUID, _CallId]),
    State#state{call_id=UUID
               ,call=kapps_call:set_call_id(UUID, Call)
               }.

-spec handle_channel_destroy(state(), ne_binary()) -> state().
handle_channel_destroy(#state{call_id=CallId
                             ,listen_on='a'
                             }
                      ,CallId
                      ) ->
    lager:debug("'a' leg has ended, so should we"),
    exit('normal');
handle_channel_destroy(#state{call_id=CallId
                             ,other_leg='undefined'
                             }
                      ,CallId
                      ) ->
    lager:debug("'a' leg has ended and we have no 'b' leg"),
    exit('normal');
handle_channel_destroy(#state{call_id=CallId
                             ,other_leg=_OtherLeg
                             ,call=Call
                             }=State
                      ,CallId
                      ) ->
    lager:debug("'a' leg has ended but we're interested in the 'b' ~s", [_OtherLeg]),
    State#state{call_id='undefined'
               ,call=kapps_call:set_call_id('undefined', Call)
               };
handle_channel_destroy(#state{other_leg=OtherLeg
                             ,listen_on='b'
                             }
                      ,OtherLeg
                      ) ->
    lager:debug("'b' ~s has ended, so should we", [OtherLeg]),
    exit('normal');
handle_channel_destroy(#state{other_leg=OtherLeg
                             ,call=Call
                             }=State
                      ,OtherLeg
                      ) ->
    lager:debug("'b' ~s has ended but we're still interested in the 'a'", [OtherLeg]),
    State#state{other_leg='undefined'
               ,call=kapps_call:set_other_leg_call_id('undefined', Call)
               }.
