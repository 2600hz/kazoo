%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% Data: {
%%%   "id":"queue id"
%%% }
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_acdc_member).

-export([handle/2]).

-include("../callflow.hrl").

-type max_wait() :: pos_integer() | 'infinity'.

-record(member_call, {call              :: whapps_call:call()
                      ,queue_id         :: api_binary()
                      ,config_data = [] :: wh_proplist()
                      ,max_wait = 60000 :: max_wait()
                     }).
-type member_call() :: #member_call{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    QueueId = wh_json:get_value(<<"id">>, Data),
    lager:info("sending call to queue ~s", [QueueId]),

    MemberCall = props:filter_undefined(
                   [{<<"Account-ID">>, whapps_call:account_id(Call)}
                    ,{<<"Queue-ID">>, QueueId}
                    ,{<<"Call">>, whapps_call:to_json(Call)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ]),

    lager:info("loading ACDc queue: ~s", [QueueId]),
    {ok, QueueJObj} = couch_mgr:open_cache_doc(whapps_call:account_db(Call), QueueId),

    MaxWait = max_wait(wh_json:get_integer_value(<<"connection_timeout">>, QueueJObj, 3600)),
    MaxQueueSize = max_queue_size(wh_json:get_integer_value(<<"max_queue_size">>, QueueJObj, 0)),

    Call1 = whapps_call:kvs_store('caller_exit_key', wh_json:get_value(<<"caller_exit_key">>, QueueJObj, <<"#">>), Call),

    CurrQueueSize = wapi_acdc_queue:queue_size(whapps_call:account_id(Call1), QueueId),

    lager:info("max size: ~p curr size: ~p", [MaxQueueSize, CurrQueueSize]),

    maybe_enter_queue(#member_call{call=Call1
                                   ,config_data=MemberCall
                                   ,queue_id=QueueId
                                   ,max_wait=MaxWait
                                  }
                      ,is_queue_full(MaxQueueSize, CurrQueueSize)
                     ).

-spec maybe_enter_queue(member_call(), boolean()) -> any().
maybe_enter_queue(#member_call{call=Call}, 'true') ->
    lager:info("queue has reached max size"),
    cf_exe:continue(Call);
maybe_enter_queue(#member_call{call=Call
                               ,config_data=MemberCall
                               ,queue_id=QueueId
                               ,max_wait= MaxWait
                              }=MC
                  ,'false') ->
    lager:info("asking for an agent, waiting up to ~p ms", [MaxWait]),

    cf_exe:send_amqp(Call, MemberCall, fun wapi_acdc_queue:publish_member_call/1),
    _ = whapps_call_command:flush_dtmf(Call),
    wait_for_bridge(MC#member_call{call=whapps_call:kvs_store('queue_id', QueueId, Call)}
                    ,MaxWait
                   ).

-spec wait_for_bridge(member_call(), max_wait()) -> 'ok'.
-spec wait_for_bridge(member_call(), max_wait(), wh_now()) -> 'ok'.
wait_for_bridge(MC, Timeout) ->
    wait_for_bridge(MC, Timeout, erlang:now()).
wait_for_bridge(#member_call{call=Call}=MC, Timeout, Start) ->
    Wait = erlang:now(),
    receive
        {'amqp_msg', JObj} ->
            process_message(MC, Timeout, Start, Wait, JObj, wh_util:get_event_type(JObj))
    after Timeout ->
            lager:info("failed to handle the call in time, proceeding"),
            cancel_member_call(Call, <<"member_timeout">>),

            stop_hold_music(Call),
            cf_exe:continue(Call)
    end.

-spec process_message(member_call(), max_wait(), wh_now()
                      ,wh_now(), wh_json:object()
                      ,{ne_binary(), ne_binary()}
                     ) -> 'ok'.
process_message(#member_call{call=Call}, _, Start, _Wait, _JObj, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}) ->
    lager:info("member was bridged to agent, yay! took ~b s", [wh_util:elapsed_s(Start)]),
    cf_exe:control_usurped(Call);
process_message(#member_call{call=Call}, _, Start, _Wait, _JObj, {<<"call_event">>,<<"CHANNEL_HANGUP_COMPLETE">>}) ->
    lager:info("member hungup while waiting in the queue (was there ~b s)", [wh_util:elapsed_s(Start)]),
    cancel_member_call(Call, <<"member_hungup">>),
    cf_exe:stop(Call);
process_message(#member_call{call=Call
                             ,queue_id=QueueId
                            }=MC, Timeout, Start, Wait, JObj, {<<"member">>, <<"call_fail">>}) ->
    case QueueId =:= wh_json:get_value(<<"Queue-ID">>, JObj) of
        'true' ->
            Failure = wh_json:get_value(<<"Failure-Reason">>, JObj),
            lager:info("call failed to be processed: ~s (took ~b s)"
                       ,[Failure, wh_util:elapsed_s(Start)]
                      ),
            cancel_member_call(Call, Failure),
            stop_hold_music(Call),
            cf_exe:continue(Call);
        'false' ->
            lager:info("failure json was for a different queue, ignoring"),
            wait_for_bridge(MC, reduce_timeout(Timeout, wh_util:elapsed_ms(Wait)), Start)
    end;
process_message(#member_call{call=Call}=MC, Timeout, Start, Wait, JObj, {<<"call_event">>, <<"DTMF">>}) ->
    DigitPressed = wh_json:get_value(<<"DTMF-Digit">>, JObj),
    case DigitPressed =:= whapps_call:kvs_fetch('caller_exit_key', Call) of
        'true' ->
            lager:info("caller pressed the exit key(~s), moving to next callflow action", [DigitPressed]),
            cancel_member_call(Call, <<"dtmf_exit">>),
            _ = whapps_call_command:flush_dtmf(Call),
            timer:sleep(1000),
            cf_exe:continue(Call);
        'false' ->
            lager:info("caller pressed ~s, ignoring", [DigitPressed]),
            wait_for_bridge(MC, reduce_timeout(Timeout, wh_util:elapsed_ms(Wait)), Start)
    end;
process_message(#member_call{call=Call}, _, Start, _Wait, _JObj, {<<"member">>, <<"call_success">>}) ->
    lager:info("call was processed by queue (took ~b s)", [wh_util:elapsed_s(Start)]),
    cf_exe:control_usurped(Call);
process_message(MC, Timeout, Start, Wait, _JObj, _Type) ->
    wait_for_bridge(MC, reduce_timeout(Timeout, wh_util:elapsed_ms(Wait)), Start).

-spec reduce_timeout(max_wait(), integer()) -> max_wait().
reduce_timeout('infinity', _) -> 'infinity';
reduce_timeout(T, R) -> T-R.

%% convert from seconds to milliseconds, or infinity
-spec max_wait(integer()) -> max_wait().
max_wait(N) when N < 1 -> 'infinity';
max_wait(N) -> N * 1000.

max_queue_size(N) when is_integer(N), N > 0 -> N;
max_queue_size(_) -> 0.

-spec is_queue_full(non_neg_integer(), non_neg_integer()) -> boolean().
is_queue_full(0, _) -> 'false';
is_queue_full(MaxQueueSize, CurrQueueSize) -> CurrQueueSize >= MaxQueueSize.

cancel_member_call(Call, Reason) ->
    AcctId = whapps_call:account_id(Call),
    {'ok', QueueId} = whapps_call:kvs_find('queue_id', Call),
    CallId = whapps_call:call_id(Call),

    Req = props:filter_undefined(
            [{<<"Account-ID">>, AcctId}
             ,{<<"Queue-ID">>, QueueId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Reason">>, Reason}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    wapi_acdc_queue:publish_member_call_cancel(Req).

stop_hold_music(Call) ->
    Cmd = [{<<"Application-Name">>, <<"play">>}
           ,{<<"Call-ID">>, whapps_call:call_id(Call)}
           ,{<<"Media-Name">>, <<"silence_stream://50">>}
           ,{<<"Insert-At">>, <<"now">>}
          ],
    whapps_call_command:send_command(Cmd, Call).
