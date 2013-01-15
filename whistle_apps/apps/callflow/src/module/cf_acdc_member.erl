%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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

-type max_wait() :: integer() | 'infinity'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
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

    Call1 = whapps_call:kvs_store(caller_exit_key, wh_json:get_value(<<"caller_exit_key">>, QueueJObj, <<"#">>), Call),

    CurrQueueSize = wapi_acdc_queue:queue_size(whapps_call:account_id(Call1), QueueId),

    lager:info("max size: ~p curr size: ~p", [MaxQueueSize, CurrQueueSize]),

    maybe_enter_queue(Call1, MemberCall, QueueId, MaxWait, is_queue_full(MaxQueueSize, CurrQueueSize)).

-spec maybe_enter_queue/5 :: (whapps_call:call(), wh_proplist(), ne_binary(), max_wait(), boolean()) -> 'ok'.
maybe_enter_queue(Call, _, _, _, true) ->
    lager:info("queue has reached max size"),
    cf_exe:continue(Call);
maybe_enter_queue(Call, MemberCall, QueueId, MaxWait, false) ->
    lager:info("asking for an agent, waiting up to ~p ms", [MaxWait]),

    cf_exe:send_amqp(Call, MemberCall, fun wapi_acdc_queue:publish_member_call/1),
    wait_for_bridge(whapps_call:kvs_store(queue_id, QueueId, Call), MaxWait).

-spec wait_for_bridge/2 :: (whapps_call:call(), max_wait()) -> 'ok'.
-spec wait_for_bridge/3 :: (whapps_call:call(), max_wait(), wh_now()) -> 'ok'.
wait_for_bridge(Call, Timeout) ->
    wait_for_bridge(Call, Timeout, erlang:now()).
wait_for_bridge(Call, Timeout, Start) ->
    Wait = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            process_message(Call, Timeout, Start, Wait, JObj, wh_util:get_event_type(JObj));
        _Msg ->
            lager:info("popping msg off: ~p", [_Msg]),
            wait_for_bridge(Call, Timeout, Start)
    after Timeout ->
            lager:info("failed to handle the call in time, proceeding"),
            cancel_member_call(Call, <<"member_timeout">>),
            cf_exe:continue(Call)
    end.

-spec process_message/6 :: (whapps_call:call(), max_wait(), wh_now()
                            ,wh_now(), wh_json:json_object()
                            ,{ne_binary(), ne_binary()}
                           ) -> 'ok'.
process_message(Call, _, Start, _Wait, _JObj, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}) ->
    lager:info("member was bridged to agent, yay! took ~b s", [wh_util:elapsed_s(Start)]),
    cf_exe:control_usurped(Call);
process_message(Call, _, Start, _Wait, _JObj, {<<"call_event">>,<<"CHANNEL_HANGUP_COMPLETE">>}) ->
    lager:info("member hungup while waiting in the queue (was there ~b s)", [wh_util:elapsed_s(Start)]),
    cancel_member_call(Call, <<"member_hungup">>),
    cf_exe:stop(Call);
process_message(Call, _, Start, _Wait, JObj, {<<"member">>, <<"call_fail">>}) ->
    Failure = wh_json:get_value(<<"Failure-Reason">>, JObj),
    lager:info("call failed to be processed: ~s (took ~b s)"
                ,[Failure, wh_util:elapsed_s(Start)]
               ),
    cancel_member_call(Call, Failure),
    cf_exe:continue(Call);
process_message(Call, Timeout, Start, Wait, JObj, {<<"call_event">>, <<"DTMF">>}) ->
    DigitPressed = wh_json:get_value(<<"DTMF-Digit">>, JObj),
    case DigitPressed =:= whapps_call:kvs_fetch(caller_exit_key, Call) of
        true ->
            lager:info("caller pressed the exit key(~s), moving to next callflow action", [DigitPressed]),
            cancel_member_call(Call, <<"dtmf_exit">>),
            cf_exe:continue(Call);
        false ->
            lager:info("caller pressed ~s, ignoring", [DigitPressed]),
            wait_for_bridge(Call, reduce_timeout(Timeout, wh_util:elapsed_ms(Wait)), Start)
    end;
process_message(Call, _, Start, _Wait, _JObj, {<<"member">>, <<"call_success">>}) ->
    lager:info("call was processed by queue (took ~b s)", [wh_util:elapsed_s(Start)]),
    cf_exe:control_usurped(Call);
process_message(Call, _, Start, _Wait, JObj, {<<"member">>, <<"call_cancel">>}) ->
    lager:info("call was cancelled by queue (took ~b s): ~s, continuing callflow"
                ,[wh_util:elapsed_s(Start), wh_json:get_value(<<"Reason">>, JObj)]
               ),
    cf_exe:continue(Call);
process_message(Call, Timeout, Start, Wait, _JObj, _Type) ->
    %%lager:info("recv unhandled message: ~p: ~s", [_Type, wh_json:get_value(<<"Application-Name">>, _JObj)]),
    wait_for_bridge(Call, reduce_timeout(Timeout, wh_util:elapsed_ms(Wait)), Start).

-spec reduce_timeout/2 :: (max_wait(), integer()) -> max_wait().
reduce_timeout('infinity', _) -> 'infinity';
reduce_timeout(T, R) -> T-R.

%% convert from seconds to milliseconds, or infinity
-spec max_wait/1 :: (integer()) -> max_wait().
max_wait(N) when N < 1 -> infinity;
max_wait(N) -> N * 1000.

max_queue_size(N) when is_integer(N), N > 0 -> N;
max_queue_size(_) -> 0.

-spec is_queue_full/2 :: (non_neg_integer(), non_neg_integer()) -> boolean().
is_queue_full(0, _) -> false;
is_queue_full(MaxQueueSize, CurrQueueSize) -> CurrQueueSize >= MaxQueueSize.

cancel_member_call(Call, Reason) ->
    AcctId = whapps_call:account_id(Call),
    {ok, QueueId} = whapps_call:kvs_find(queue_id, Call),
    CallId = whapps_call:call_id(Call),

    Req = props:filter_undefined([
                                  {<<"Account-ID">>, AcctId}
                                  ,{<<"Queue-ID">>, QueueId}
                                  ,{<<"Call-ID">>, CallId}
                                  ,{<<"Reason">>, Reason}
                                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    wapi_acdc_queue:publish_member_call_cancel(Req).
