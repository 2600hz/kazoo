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
         ,dtmf/3
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
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_fsm(whapps_call:call(), wh_json:object()) -> any().
start_fsm(Call, JObj) ->
    wh_hooks:register(whapps_call:account_id(Call), <<"CHANNEL_DESTROY">>),
    wh_hooks:register(whapps_call:account_id(Call), <<"CHANNEL_ANSWER">>),

    gen_fsm:enter_loop(?MODULE, [], 'unarmed'
                       ,#state{numbers=numbers(Call, JObj)
                               ,patterns=patterns(Call, JObj)
                               ,binding_key=binding_key(Call, JObj)
                               ,digit_timeout=digit_timeout(Call, JObj)
                               ,listen_on=listen_on(Call, JObj)

                               ,call=Call
                               ,call_id=whapps_call:call_id_direct(Call)
                              }).

-spec dtmf(pid(), ne_binary(), ne_binary()) -> 'ok'.
dtmf(FSM, CallId, DTMF) ->
    gen_fsm:send_event(FSM, {'dtmf', CallId, DTMF}).

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
    lager:debug("recv binding key ~s, arming (~bms)", [BindingKey, Timeout]),
    Ref = gen_fsm:start_timer(Timeout, 'digit_timeout'),
    {'next_state', 'armed', State#state{b_digit_timeout_ref = Ref
                                        ,b_collected_dtmf = <<>>
                                       }};
unarmed({'dtmf', CallId, BindingKey}, #state{other_leg=CallId
                                             ,listen_on='b'
                                             ,binding_key=BindingKey
                                             ,digit_timeout=Timeout
                                            }=State) ->
    lager:debug("recv binding key ~s, arming (~bms)", [BindingKey, Timeout]),
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
            _Pid = spawn('konami_code_exe', 'handle', [N, Call]),
            lager:debug("number exe in ~p: ~p", [_Pid, N]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
        {'pattern', P} ->
            _Pid = spawn('konami_code_exe', 'handle', [P, Call]),
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
            _Pid = spawn('konami_code_exe', 'handle', [N, Call]),
            lager:debug("number exe in ~p: ~p", [_Pid, N]),
            {'next_state', 'unarmed', disarm_state(State), 'hibernate'};
        {'pattern', P} ->
            _Pid = spawn('konami_code_exe', 'handle', [P, Call]),
            lager:debug("pattern exe in ~p: ~p", [_Pid, P]),
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

handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

handle_info(?HOOK_EVT(_AccountId, <<"CHANNEL_DESTROY">>, Evt), StateName, #state{call_id=CallId
                                                                                 ,other_leg=OtherLeg
                                                                                }=State) ->
    case wh_json:get_value(<<"Call-ID">>, Evt) of
        CallId ->
            lager:debug("recv a-leg channel_destroy while in ~s, going down", [StateName]),
            {'stop', 'normal', State};
        OtherLeg ->
            lager:debug("recv b-leg channel_destroy while in ~s", [StateName]),
            {'next_state', StateName, State};
        _CallId ->
            lager:debug("unknown call leg ~s went down", [_CallId]),
            {'next_state', StateName, State}
    end;
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

terminate(_Reason, _StateName, #state{call_id=CallId}) ->
    konami_dtmf_listener:rm_call_binding(CallId),
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
        'undefined' -> konami_config:numbers(whapps_call:account_id(Call));
        Numbers -> Numbers
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
    case wh_json:get_value(<<"Listen-On">>, JObj) of
        'undefined' -> konami_config:listen_on(whapps_call:account_id(Call));
        <<"a">> -> 'a';
        <<"b">> -> 'b';
        <<"ab">> -> 'ab';
        _ -> 'a'
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
    case re:run(Collected, Regex, [{'capture', 'all', 'binary'}]) of
        'nomatch' -> has_pattern(Collected, Ps, Regexes);
        {'match', Captured} ->
            P = wh_json:get_value(Regex, Ps),
            {'pattern', wh_json:set_values([{[<<"data">>, <<"collected">>], Collected}
                                            ,{[<<"data">>, <<"captured">>], Captured}
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

maybe_cancel_timer(Ref) when is_reference(Ref) ->
    catch erlang:cancel_timer(Ref),
    'ok';
maybe_cancel_timer(_) ->
    'ok'.
