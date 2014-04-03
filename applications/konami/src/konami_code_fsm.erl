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
    _ = maybe_start_b_leg_listener(Call, ListenOn),

    gen_fsm:enter_loop(?MODULE, [], 'unarmed'
                       ,#state{numbers=numbers(Call, JObj)
                               ,patterns=patterns(Call, JObj)
                               ,binding_key=binding_key(Call, JObj)
                               ,digit_timeout=digit_timeout(Call, JObj)
                               ,listen_on=ListenOn

                               ,call=whapps_call:clear_helpers(Call)
                               ,call_id=whapps_call:call_id_direct(Call)
                               ,other_leg_endpoint_id=wh_json:get_value(<<"Endpoint-ID">>, JObj)
                              }).

-spec event(pid(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
event(FSM, CallId, <<"DTMF">>, JObj) ->
    gen_fsm:send_event(FSM, {'dtmf'
                             ,CallId
                             ,wh_json:get_value(<<"DTMF-Digit">>, JObj)
                             ,wh_json:get_value(<<"Custom-Channel-Vars">>, JObj)
                            });
event(FSM, CallId, Event, JObj) ->
    gen_fsm:send_event(FSM, {'event', CallId, Event, JObj}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([]) ->
    {'ok', 'unarmed', #state{}}.

unarmed({'dtmf', CallId, BindingKey, _CCVs}, #state{call_id=CallId
                                                    ,listen_on='a'
                                                    ,binding_key=BindingKey
                                                    ,digit_timeout=Timeout
                                                   }=State) ->
    lager:debug("recv binding key ~s, arming (~bms)", [BindingKey, Timeout]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{a_digit_timeout_ref = Ref
                                        ,a_collected_dtmf = <<>>
                                       }};
unarmed({'dtmf', CallId, BindingKey, _CCVs}, #state{call_id=CallId
                                                    ,listen_on='ab'
                                                    ,binding_key=BindingKey
                                                    ,digit_timeout=Timeout
                                                   }=State) ->
    lager:debug("recv binding key ~s, arming (~bms)", [BindingKey, Timeout]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{a_digit_timeout_ref = Ref
                                        ,a_collected_dtmf = <<>>
                                       }};
unarmed({'dtmf', CallId, BindingKey, CCVs}, #state{other_leg=CallId
                                                   ,other_leg_endpoint_id=EndpointId
                                                   ,listen_on='ab'
                                                   ,binding_key=BindingKey
                                                   ,digit_timeout=Timeout
                                                  }=State) ->
    case wh_json:get_value(<<"Endpoint-ID">>, CCVs) of
        EndpointId ->
            lager:debug("recv binding key ~s for endpoint ~s, arming (~bms)", [BindingKey, EndpointId, Timeout]),
            Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
            {'next_state', 'armed', State#state{b_digit_timeout_ref = Ref
                                                ,b_collected_dtmf = <<>>
                                               }};
        'undefined' ->
            lager:debug("no endpoint id was on ccvs, using the dtmf"),
            lager:debug("recv binding key ~s, arming (~bms)", [BindingKey, Timeout]),
            Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
            {'next_state', 'armed', State#state{b_digit_timeout_ref = Ref
                                                ,b_collected_dtmf = <<>>
                                               }};
        _OtherEndpointId ->
            lager:debug("recv dtmf '~s' for b leg ~s but for endpoint ~s while we want ~s"
                        ,[BindingKey, CallId, _OtherEndpointId, EndpointId]
                       ),
            {'next_state', 'unarmed', State}
    end;
unarmed({'dtmf', CallId, BindingKey, CCVs}, #state{other_leg=CallId
                                                   ,other_leg_endpoint_id=EndpointId
                                                   ,listen_on='b'
                                                   ,binding_key=BindingKey
                                                   ,digit_timeout=Timeout
                                                  }=State) ->
    case wh_json:get_value(<<"Endpoint-ID">>, CCVs) of
        EndpointId ->
            lager:debug("recv binding key ~s, arming (~bms)", [BindingKey, Timeout]),
            Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
            {'next_state', 'armed', State#state{b_digit_timeout_ref = Ref
                                                ,b_collected_dtmf = <<>>
                                               }};
        'undefined' ->
            lager:debug("no endpoint id was on ccvs, using the dtmf"),
            lager:debug("recv binding key ~s, arming (~bms)", [BindingKey, Timeout]),
            Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
            {'next_state', 'armed', State#state{b_digit_timeout_ref = Ref
                                                ,b_collected_dtmf = <<>>
                                               }};
        _OtherEndpointId ->
            lager:debug("recv dtmf '~s' for b leg ~s but for endpoint ~s while we want ~s"
                        ,[BindingKey, CallId, _OtherEndpointId, EndpointId]
                       ),
            lager:debug("ccvs: ~p", [CCVs]),
            {'next_state', 'unarmed', State}
    end;
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
            _Pid = proc_lib:spawn('konami_code_exe', 'handle', [N, Call]),
            lager:debug("number exe in ~p: ~p", [_Pid, N]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
        {'pattern', P} ->
            _Pid = proc_lib:spawn('konami_code_exe', 'handle', [P, Call]),
            lager:debug("pattern exe in ~p: ~p", [_Pid, P]),
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
            _Pid = proc_lib:spawn('konami_code_exe', 'handle', [N, Call]),
            lager:debug("number exe in ~p: ~p", [_Pid, N]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
        {'pattern', P} ->
            _Pid = proc_lib:spawn('konami_code_exe', 'handle', [P, Call]),
            lager:debug("pattern exe in ~p: ~p", [_Pid, P]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'}
    end;

armed({'dtmf', CallId, DTMF, _CCVs}, #state{call_id=CallId
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
armed({'dtmf', CallId, DTMF, _CCVs}, #state{other_leg=CallId
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

handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

handle_info(?HOOK_EVT(_AccountId, <<"CHANNEL_ANSWER">>, Evt), StateName, #state{call_id=CallId
                                                                                ,other_leg=OtherLeg
                                                                               }=State) ->
    case wh_json:get_value(<<"Call-ID">>, Evt) of
        CallId ->
            case wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt) of
                'undefined' ->
                    lager:debug("channel is answered (~s)", [OtherLeg]),
                    {'next_state', StateName, State};
                OtherLeg ->
                    lager:debug("channel is answered (~s)", [OtherLeg]),
                    {'next_state', StateName, State};
                NewOtherLeg ->
                    lager:debug("channel is bridged to ~s", [NewOtherLeg]),
                    konami_event_listener:add_call_binding(NewOtherLeg),
                    {'next_state', StateName, State#state{other_leg=NewOtherLeg}}
            end;
        OtherLeg ->
            lager:debug("b leg ~s is answered", [OtherLeg]),
            {'next_state', StateName, State};
        NewOtherLeg ->
            konami_event_listener:add_call_binding(NewOtherLeg),
            lager:debug("b leg ~s is answered, bridged to us ~s", [NewOtherLeg, wh_json:get_value(<<"Other-Leg-Call-ID">>, Evt)]),
            {'next_state', StateName, State#state{other_leg=NewOtherLeg}}
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

listen_on(Call, JObj) ->
    EndpointId = wh_json:get_value(<<"Endpoint-ID">>, JObj),
    IsALegEndpoint = (EndpointId =:= 'undefined') orelse
        (EndpointId =:= whapps_call:authorizing_id(Call)),

    lager:debug("ep: '~s' is a leg: ~s lo: ~s", [EndpointId, IsALegEndpoint, wh_json:get_value(<<"Listen-On">>, JObj)]),

    case wh_json:get_value(<<"Listen-On">>, JObj) of
        'undefined' when IsALegEndpoint ->
            Default = konami_config:listen_on(whapps_call:account_id(Call)),
            lager:debug("lo default: ~p", [Default]),
            Default;
        'undefined' -> 'b';
        <<"a">> -> 'a';
        <<"b">> -> 'b';
        <<"ab">> -> 'ab';
        <<"both">> -> 'ab';
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

-spec maybe_start_b_leg_listener(whapps_call:call(), 'a' | 'b' | 'ab') -> 'ok'.
maybe_start_b_leg_listener(_Call, 'a') ->
    'ok';
maybe_start_b_leg_listener(Call, _) ->
    API = [{<<"Application-Name">>, <<"noop">>}
           ,{<<"B-Leg-Events">>, [<<"DTMF">>]}
           ,{<<"Insert-At">>, <<"now">>}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending noop for b leg events"),
    whapps_call_command:send_command(API, Call).

-spec should_stop_fsm(ne_binary(), api_binary(), 'a' | 'ab' | 'b', ne_binary()) -> boolean().
should_stop_fsm(CallId, _OtherLeg, 'a', CallId) ->
    lager:debug("recv a-leg channel_destroy, going down"),
    konami_event_listener:rm_call_binding(CallId),
    'true';
should_stop_fsm(CallId, _OtherLeg, 'ab', CallId) ->
    lager:debug("recv a-leg channel_destroy, going down"),
    konami_event_listener:rm_call_binding(CallId),
    'true';
should_stop_fsm(CallId, _OtherLeg, _ListenOn, CallId) ->
    lager:debug("recv a-leg channel_destroy, ignoring"),
    'false';
should_stop_fsm(_CallId, 'undefined', _ListenOn, _DownCallId) ->
    lager:debug("no b-leg, ignoring destroy for ~s", [_DownCallId]),
    'false';
should_stop_fsm(_CallId, OtherLeg, 'b', OtherLeg) ->
    lager:debug("recv b-leg channel_destroy, going down"),
    konami_event_listener:rm_call_binding(OtherLeg),
    'true';
should_stop_fsm(_CallId, OtherLeg, _ListenOn, OtherLeg) ->
    lager:debug("recv b-leg channel_destroy, ignoring"),
    'false'.
