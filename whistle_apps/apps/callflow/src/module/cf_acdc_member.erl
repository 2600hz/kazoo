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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    QueueId = wh_json:get_value(<<"id">>, Data),

    MemberCall = props:filter_undefined(
                   [{<<"Account-ID">>, whapps_call:account_id(Call)}
                    ,{<<"Queue-ID">>, QueueId}
                    ,{<<"Call">>, whapps_call:to_json(Call)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ]),

    lager:debug("loading queue ~s", [QueueId]),
    {ok, QueueJObj} = couch_mgr:open_cache_doc(whapps_call:account_db(Call), QueueId),

    MaxWait = max_wait(wh_json:get_integer_value(<<"connection_timeout">>, QueueJObj, 3600)),
    MaxQueueSize = max_queue_size(wh_json:get_integer_value(<<"max_queue_size">>, QueueJObj, 0)),

    CurrQueueSize = wapi_acdc_queue:queue_size(whapps_call:account_id(Call), QueueId),

    lager:debug("max size: ~p curr size: ~p", [MaxQueueSize, CurrQueueSize]),

    maybe_enter_queue(Call, MemberCall, MaxWait, is_queue_full(MaxQueueSize, CurrQueueSize)).

maybe_enter_queue(Call, _, _, true) ->
    lager:debug("queue has reached max size"),
    cf_exe:continue(Call);
maybe_enter_queue(Call, MemberCall, MaxWait, false) ->
    lager:debug("asking for an agent, waiting up to ~p ms", [MaxWait]),

    cf_exe:send_amqp(Call, MemberCall, fun wapi_acdc_queue:publish_member_call/1),

    wait_for_bridge(Call, MaxWait).

-spec wait_for_bridge/2 :: (whapps_call:call(), integer()) -> 'ok'.
-spec wait_for_bridge/3 :: (whapps_call:call(), integer(), wh_now()) -> 'ok'.
wait_for_bridge(Call, Timeout) ->
    wait_for_bridge(Call, Timeout, erlang:now()).
wait_for_bridge(Call, Timeout, Start) ->
    Wait = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            process_message(Call, Timeout, Start, Wait, JObj, wh_util:get_event_type(JObj))
    after Timeout ->
            lager:debug("failed to handle the call in time, proceeding"),
            cf_exe:continue(Call)
    end.

-spec process_message/6 :: (whapps_call:call(), integer(), wh_now(), wh_now(), wh_json:json_object(), {ne_binary(), ne_binary()}) -> 'ok'.
process_message(Call, _, Start, _Wait, _JObj, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}) ->
    lager:debug("member was bridged to agent, yay! took ~b s", [wh_util:elapsed_s(Start)]),
    cf_exe:control_usurped(Call);
process_message(Call, _, Start, _Wait, _JObj, {<<"call_event">>,<<"CHANNEL_HANGUP_COMPLETE">>}) ->
    lager:debug("member hungup while waiting in the queue (was there ~b s)", [wh_util:elapsed_s(Start)]),
    cf_exe:stop(Call);
process_message(Call, _, Start, _Wait, JObj, {<<"member">>, <<"call_fail">>}) ->
    lager:debug("call failed to be processed: ~s (took ~b s)"
                ,[wh_json:get_value(<<"Failure-Reason">>, JObj), wh_util:elapsed_s(Start)]
               ),
    cf_exe:continue(Call);
process_message(Call, _, Start, _Wait, _JObj, {<<"member">>, <<"call_success">>}) ->
    lager:debug("call was processed by queue (took ~b s)", [wh_util:elapsed_s(Start)]),
    cf_exe:control_usurped(Call);
process_message(Call, Timeout, Start, Wait, _JObj, _Type) ->
    lager:debug("unknown message type: ~p", [_Type]),
    wait_for_bridge(Call, Timeout - wh_util:elapsed_ms(Wait), Start).

%% convert from seconds to milliseconds, or infinity
-spec max_wait/1 :: (integer()) -> pos_integer() | 'infinity'.
max_wait(N) when N < 1 -> infinity;
max_wait(N) -> N * 1000.

max_queue_size(N) when is_integer(N), N > 0 -> N;
max_queue_size(_) -> 0.

-spec is_queue_full/2 :: (non_neg_integer(), non_neg_integer()) -> boolean().
is_queue_full(0, _) -> false;
is_queue_full(MaxQueueSize, CurrQueueSize) -> CurrQueueSize >= MaxQueueSize.
