%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Daniel Finke
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_code_statem).

-behaviour(gen_statem).

%% API
-export([start/2
        ,event/4
        ,transfer_to/2
        ]).

%% gen_statem callbacks
-export([init/1
        ,callback_mode/0

        ,unarmed/3
        ,armed/3

        ,terminate/3
        ,code_change/4
        ]).

-export([format_status/2]).

-include("konami.hrl").

-type listen_on() :: 'a' | 'b' | 'ab'.

-record(state, {numbers :: kz_term:api_object()
               ,patterns :: kz_term:api_object()
               ,binding_digit = konami_config:binding_digit() :: kz_term:ne_binary()
               ,digit_timeout = konami_config:timeout() :: pos_integer()
               ,call :: kapps_call:call() | 'undefined'

               ,listen_on = 'a' :: listen_on()

               ,a_digit_timeout_ref :: kz_term:api_reference()
               ,a_collected_dtmf = <<>> :: binary()
               ,a_leg_armed = 'false' :: boolean()

               ,b_digit_timeout_ref :: kz_term:api_reference()
               ,b_collected_dtmf = <<>> :: binary()
               ,b_endpoint_id :: kz_term:api_binary()
               ,b_leg_armed = 'false' :: boolean()

               ,call_id :: kz_term:api_ne_binary()
               ,other_leg :: kz_term:api_binary()
               }).
-type state() :: #state{}.

-define(WSD_ID, ?WSD_ENABLED
        andalso {'file', get('callid')}
       ).

-define(WSD_EVT(Fr, T, E), ?WSD_ENABLED
        andalso webseq:evt(?WSD_ID, Fr, T, <<(kz_term:to_binary(?LINE))/binary, "-", E/binary>>)
       ).

-define(WSD_NOTE(W, D, N), ?WSD_ENABLED
        andalso webseq:note(?WSD_ID, W, D, <<(kz_term:to_binary(?LINE))/binary, "-", N/binary>>)
       ).

-define(WSD_TITLE(T), ?WSD_ENABLED
        andalso webseq:title(?WSD_ID, [T, " in ", kz_term:to_binary(self())])
       ).
-define(WSD_START(), ?WSD_ENABLED
        andalso webseq:start(?WSD_ID)
       ).

-define(WSD_STOP(), ?WSD_ENABLED
        andalso webseq:stop(?WSD_ID)
       ).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start(kapps_call:call(), kz_json:object()) -> any().
start(Call, JObj) ->
    gen_statem:start_link(?MODULE, {Call, JObj}, []).

-spec event(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
event(ServerRef, CallId, <<"DTMF">>, JObj) ->
    gen_statem:cast(ServerRef, {'dtmf'
                               ,CallId
                               ,kz_call_event:dtmf_digit(JObj)
                               });
event(ServerRef, CallId, Event, JObj) ->
    gen_statem:cast(ServerRef, ?EVENT(CallId, Event, JObj)).

-spec transfer_to(kapps_call:call(), 'a' | 'b') -> 'ok'.
transfer_to(Call, Leg) ->
    gen_statem:cast(kapps_call:kvs_fetch(?MODULE, Call)
                   ,{'transfer_to', Call, Leg}
                   ).

%%%=============================================================================
%%% gen_statem callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init({kapps_call:call(), kz_json:object()}) -> {'ok', 'unarmed', state()}.
init({Call, JObj}) ->
    ListenOn = listen_on(Call, JObj),
    AuthorizingId = kapps_call:authorizing_id(Call),

    kapps_call:put_callid(Call),

    maybe_add_call_event_bindings(Call, ListenOn),

    lager:debug("starting Konami statem, listening on '~s'", [ListenOn]),

    BEndpointId = b_endpoint_id(JObj, ListenOn, AuthorizingId),

    lager:debug("a endpoint: ~s b endpoint: ~s", [AuthorizingId, BEndpointId]),

    ?WSD_START(),
    ?WSD_TITLE(["FSM: ", kapps_call:call_id(Call), " listen on: ", kz_term:to_list(ListenOn)]),

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

-spec callback_mode() -> 'state_functions'.
callback_mode() ->
    'state_functions'.

-spec unarmed(gen_statem:event_type(), any(), state()) -> kz_types:handle_fsm_ret(state()).
unarmed('cast', {'dtmf', CallId, BindingDigit}, #state{call_id=CallId
                                                      ,listen_on='a'
                                                      ,binding_digit=BindingDigit
                                                      }=State) ->
    lager:debug("recv binding digit ~s, arming a-leg", [BindingDigit]),
    ?WSD_NOTE(CallId, 'right', <<"arming">>),
    {'next_state', 'armed', arm_aleg(State)};
unarmed('cast', {'dtmf', CallId, BindingDigit}, #state{call_id=CallId
                                                      ,listen_on='ab'
                                                      ,binding_digit=BindingDigit
                                                      }=State) ->
    lager:debug("recv binding digit ~s, arming a leg", [BindingDigit]),
    ?WSD_NOTE(CallId, 'right', <<"arming">>),
    {'next_state', 'armed', arm_aleg(State)};

unarmed('cast', {'dtmf', CallId, BindingDigit}, #state{other_leg=CallId
                                                      ,listen_on='ab'
                                                      ,binding_digit=BindingDigit
                                                      }=State) ->
    lager:debug("recv binding digit '~s', arming b leg ~s", [BindingDigit, CallId]),
    ?WSD_NOTE(CallId, 'right', <<"arming">>),
    {'next_state', 'armed', arm_bleg(State)};
unarmed('cast', {'dtmf', CallId, BindingDigit}, #state{other_leg=CallId
                                                      ,listen_on='b'
                                                      ,binding_digit=BindingDigit
                                                      }=State) ->
    lager:debug("recv binding digit '~s', arming b leg ~s", [BindingDigit, CallId]),
    ?WSD_NOTE(CallId, 'right', <<"arming">>),
    {'next_state', 'armed', arm_bleg(State)};

unarmed('cast', {'dtmf', _CallId, _DTMF}, #state{call_id=_Id
                                                ,other_leg=_Oleg
                                                ,listen_on=_ListenOn
                                                }=State) ->
    lager:debug("ignoring dtmf '~s' from ~s while unarmed", [_DTMF, _CallId]),
    lager:debug("call id '~s' other_leg '~s' listen_on '~s'", [_Id, _Oleg, _ListenOn]),
    {'next_state', 'unarmed', State};
unarmed('cast', Evt, State) ->
    handle_event(Evt, ?FUNCTION_NAME, State);
unarmed('info', _, State) ->
    {'next_state', ?FUNCTION_NAME, State}.

-spec armed(gen_statem:event_type(), any(), state()) -> kz_types:handle_fsm_ret(state()).
armed('cast', {'dtmf', CallId, DTMF}, #state{call_id=CallId
                                            ,a_leg_armed='true'
                                            }=State) ->
    {'next_state', 'armed', add_aleg_dtmf(State, DTMF)};
armed('cast', {'dtmf', CallId, DTMF}, #state{other_leg=CallId
                                            ,b_leg_armed='true'
                                            }=State) ->
    {'next_state', 'armed', add_bleg_dtmf(State, DTMF)};
armed('cast', Evt, State) ->
    handle_event(Evt, ?FUNCTION_NAME, State);
armed('info', {'timeout', Ref, 'digit_timeout'}, #state{a_digit_timeout_ref = Ref
                                                       ,call_id=_CallId
                                                       }=State) ->
    lager:debug("disarming 'a' leg"),
    _ = maybe_handle_aleg_code(State),
    ?WSD_NOTE(_CallId, 'right', <<"disarming">>),
    {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
armed('info', {'timeout', Ref, 'digit_timeout'}, #state{b_digit_timeout_ref = Ref
                                                       ,other_leg=_OtherLeg
                                                       }=State) ->
    lager:debug("disarming 'b' leg"),
    _ = maybe_handle_bleg_code(State),
    ?WSD_NOTE(_OtherLeg, 'right', <<"disarming">>),
    {'next_state', 'unarmed', disarm_state(State), 'hibernate'}.

-spec handle_event(any(), atom(), state()) -> kz_types:handle_fsm_ret(state()).
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

-spec terminate(any(), atom(), state()) -> 'ok'.
terminate(_Reason, _StateName, #state{call_id=CallId
                                     ,other_leg=OtherLeg
                                     }) ->
    konami_event_listener:rm_call_binding(CallId),
    konami_event_listener:rm_call_binding(OtherLeg),
    konami_event_listener:rm_konami_binding(CallId),
    konami_event_listener:rm_konami_binding(OtherLeg),
    ?WSD_STOP(),
    lager:debug("statem terminating while in ~s: ~p", [_StateName, _Reason]).

-spec code_change(any(), atom(), state(), any()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

-spec format_status(any(), any()) -> any().
format_status(_, [_Dict, #state{call_id=CallId
                               ,other_leg=OtherLeg
                               }]) ->
    [{'data', [{"StateData", {CallId, OtherLeg}}]}].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec binding_digit(kapps_call:call(), kz_json:object()) -> kz_term:ne_binary().
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

-spec is_a_leg(kapps_call:call(), kz_term:api_object() | kz_term:ne_binary()) -> boolean().
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

-spec has_metaflow(kz_term:ne_binary(), kz_json:object(), kz_json:object()) ->
          'false' |
          {'number', kz_json:object()} |
          {'pattern', kz_json:object()}.
has_metaflow(Collected, Ns, Ps) ->
    case has_number(Collected, Ns) of
        'false' -> has_pattern(Collected, Ps);
        N -> N
    end.

-spec has_number(kz_term:ne_binary(), kz_json:object()) ->
          'false' |
          {'number', kz_json:object()}.
has_number(Collected, Ns) ->
    case kz_json:get_value(Collected, Ns) of
        'undefined' -> 'false';
        N -> {'number', N}
    end.

-spec has_pattern(kz_term:ne_binary(), kz_json:object()) ->
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

-spec handle_number_metaflow(kapps_call:call(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
handle_number_metaflow(Call, N, DTMFLeg) ->
    Metaflow = kz_json:set_values([{[<<"data">>, <<"dtmf_leg">>], DTMFLeg}], N),
    _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
    ?WSD_NOTE(DTMFLeg, 'right', <<"executing number metaflow">>),
    lager:debug("number exe in ~p: ~p", [_Pid, Metaflow]).

-spec handle_pattern_metaflow(kapps_call:call(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
handle_pattern_metaflow(Call, P, DTMFLeg) ->
    Metaflow = kz_json:set_values([{[<<"data">>, <<"dtmf_leg">>], DTMFLeg}], P),
    _Pid = proc_lib:spawn('konami_code_exe', 'handle', [Metaflow, Call]),
    ?WSD_NOTE(DTMFLeg, 'right', <<"executing pattern metaflow">>),
    lager:debug("pattern exe in ~p: ~p", [_Pid, Metaflow]).

-spec arm_aleg(state()) -> state().
arm_aleg(#state{digit_timeout=Timeout}=State) ->
    State#state{a_digit_timeout_ref = erlang:start_timer(Timeout, self(), 'digit_timeout')
               ,a_collected_dtmf = <<>>
               ,a_leg_armed='true'
               }.

-spec arm_bleg(state()) -> state().
arm_bleg(#state{digit_timeout=Timeout}=State) ->
    State#state{b_digit_timeout_ref = erlang:start_timer(Timeout, self(), 'digit_timeout')
               ,b_collected_dtmf = <<>>
               ,b_leg_armed='true'
               }.


-spec maybe_fast_rearm(kz_term:ne_binary(), kz_term:ne_binary(), binary()) -> binary().
maybe_fast_rearm(DTMF, BindingDigit, Collected) ->
    maybe_fast_rearm(DTMF, BindingDigit, Collected
                    ,kapps_config:get_is_true(?CONFIG_CAT, <<"use_fast_rearm">>, 'false')
                    ).

-spec maybe_fast_rearm(kz_term:ne_binary(), kz_term:ne_binary(), binary(), boolean()) -> binary().
maybe_fast_rearm(DTMF, _BindingDigit, Collected, 'false') -> <<Collected/binary, DTMF/binary>>;
maybe_fast_rearm(DoubleBindingDigit, DoubleBindingDigit, <<>>, 'true') -> DoubleBindingDigit;
maybe_fast_rearm(DTMFisBindingDigit, DTMFisBindingDigit, _Collected, 'true') -> <<>>;
maybe_fast_rearm(DTMF, _BindingDigit, Collected, 'true') -> <<Collected/binary, DTMF/binary>>.

-spec add_aleg_dtmf(state(), kz_term:ne_binary()) -> state().
add_aleg_dtmf(#state{a_collected_dtmf=Collected
                    ,a_digit_timeout_ref=OldRef
                    ,digit_timeout=Timeout
                    ,binding_digit=BindingDigit
                    }=State, DTMF) ->
    lager:debug("a recv dtmf '~s' while armed, adding to '~s'", [DTMF, Collected]),
    maybe_cancel_timer(OldRef),
    State#state{a_digit_timeout_ref = erlang:start_timer(Timeout, self(), 'digit_timeout')
               ,a_collected_dtmf = maybe_fast_rearm(DTMF, BindingDigit, Collected)
               }.

-spec add_bleg_dtmf(state(), kz_term:ne_binary()) -> state().
add_bleg_dtmf(#state{b_collected_dtmf=Collected
                    ,b_digit_timeout_ref=OldRef
                    ,digit_timeout=Timeout
                    ,binding_digit=BindingDigit
                    }=State, DTMF) ->
    lager:debug("b recv dtmf '~s' while armed, adding to '~s'", [DTMF, Collected]),
    maybe_cancel_timer(OldRef),
    State#state{b_digit_timeout_ref = erlang:start_timer(Timeout, self(), 'digit_timeout')
               ,b_collected_dtmf = maybe_fast_rearm(DTMF, BindingDigit, Collected)
               }.

-spec maybe_add_call_event_bindings(kz_term:api_ne_binary() | kapps_call:call()) -> 'ok'.
maybe_add_call_event_bindings('undefined') -> 'ok';
maybe_add_call_event_bindings(<<_/binary>> = Leg) -> konami_event_listener:add_call_binding(Leg);
maybe_add_call_event_bindings(Call) -> konami_event_listener:add_call_binding(Call).

-spec maybe_add_call_event_bindings(kapps_call:call(), listen_on()) -> 'ok'.
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

-spec b_endpoint_id(kz_json:object(), listen_on(), kz_term:ne_binary()) -> kz_term:api_binary().
b_endpoint_id(_JObj, 'a', _AuthorizingId) -> 'undefined';
b_endpoint_id(JObj, _ListenOn, AuthorizingId) ->
    case kz_json:get_value(<<"Endpoint-ID">>, JObj) of
        AuthorizingId -> 'undefined'; %% This is a-leg case
        Value -> Value
    end.

-spec handle_channel_answer(state(), kz_term:ne_binary(), kz_json:object()) -> state().
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

-spec maybe_other_leg_answered(state(), kz_term:ne_binary(), kz_term:ne_binary()) -> state().
maybe_other_leg_answered(#state{listen_on='b'
                               ,call_id=CallId
                               }=State
                        ,CallId
                        ,EndpointId
                        ) ->
    lager:debug("ignoring channel -s answering for endpoint ~s", [CallId, EndpointId]),
    State;
maybe_other_leg_answered(#state{listen_on='b'
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
maybe_other_leg_answered(#state{listen_on='ab'
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

-spec handle_channel_bridge(state(), kz_term:ne_binary(), kz_term:ne_binary()) -> state().
handle_channel_bridge(#state{call_id=CallId
                            ,listen_on='a'
                            ,call=Call
                            }=State, CallId, OtherLeg) ->
    lager:debug("joy, 'a' is bridged to ~s", [OtherLeg]),
    State#state{call=kapps_call:set_other_leg_call_id(OtherLeg, Call)
               ,other_leg=OtherLeg
               };
handle_channel_bridge(#state{call_id=CallId
                            ,listen_on='b'
                            }=State, CallId, _OtherLeg) ->
    lager:debug("joy, 'b' is bridged to ~s. Removing binding to a-leg", [CallId]),
    konami_event_listener:rm_call_binding(CallId),
    konami_event_listener:rm_konami_binding(CallId),
    State;
handle_channel_bridge(#state{call_id=CallId
                            ,other_leg=OtherLeg
                            }=State, CallId, OtherLeg) ->
    lager:debug("joy, 'a' and 'b' legs bridged"),
    State;
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

-spec handle_channel_destroy(state(), kz_term:ne_binary()) -> state().
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
