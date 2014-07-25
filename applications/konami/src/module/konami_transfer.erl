%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Transfers caller to the extension extracted in the regex
%%% Data = {}
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

    {'ok', TargetLeg} = originate_to_extension(Extension, TransferorLeg, Call),

    try gen_fsm:enter_loop(?MODULE, [], 'attended_wait'
                           ,#state{transferor=TransferorLeg
                                   ,transferee=TransfereeLeg
                                   ,target=TargetLeg
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
    lager:debug("transferor ~s hungup, going to a partial transfer"),
    {'next_state', 'partial_wait', State};
attended_wait(?EVENT(Target, <<"CHANNEL_DESTROY">>, _Evt)
              ,#state{target=Target
                      ,call=Call
                     }=State
             ) ->
    lager:debug("target ~s didn't answer, reconnecting"),
    _ = konami_resume:handle(wh_json:new(), Call),
    {'stop', 'normal', State};
attended_wait(Msg, State) ->
    lager:debug("attended_wait: unhandled msg ~p", [Msg]),
    {'next_state', 'attended_wait', State}.

attended_wait(Msg, From, State) ->
    lager:debug("attended_wait: unhandled msg from ~p: ~p", [From, Msg]),
    {'reply', {'error', 'not_implemented'}, 'attended_wait', State}.

partial_wait(Msg, State) ->
    lager:debug("partial_wait: unhandled msg ~p", [Msg]),
    {'next_state', 'partial_wait', State}.

partial_wait(Msg, From, State) ->
    lager:debug("partial_wait: unhandled msg from ~p: ~p", [From, Msg]),
    {'reply', {'error', 'not_implemented'}, 'partial_wait', State}.

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

originate_to_extension(Extension, TransferorLeg, _Call) ->
    lager:debug("originating to ~s from ~s", [Extension, TransferorLeg]),

    %% don't forget to usurp the callflow exe for the call if C-leg answers and transfers

    {'ok', <<"foo">>}.
