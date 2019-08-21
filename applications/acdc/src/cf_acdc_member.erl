%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Data: {
%%%   "id":"queue id"
%%% }
%%%
%%%
%%% @author James Aimonetti
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_acdc_member).

-export([handle/2]).

-include_lib("callflow/src/callflow.hrl").

-type max_wait() :: pos_integer() | 'infinity'.

-define(MEMBER_TIMEOUT, <<"member_timeout">>).
-define(MEMBER_HANGUP, <<"member_hangup">>).

-record(member_call, {call             :: kapps_call:call()
                     ,queue_id         :: kz_term:api_binary()
                     ,config_data = [] :: kz_term:proplist()
                     ,max_wait = 60 :: max_wait()
                     }).
-type member_call() :: #member_call{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    QueueId = kz_json:get_ne_binary_value(<<"id">>, Data),
    lager:info("sending call to queue ~s", [QueueId]),

    Priority = lookup_priority(Data, Call),

    MemberCall = props:filter_undefined(
                   [{<<"Account-ID">>, kapps_call:account_id(Call)}
                   ,{<<"Queue-ID">>, QueueId}
                   ,{<<"Call">>, kapps_call:to_json(Call)}
                   ,{<<"Member-Priority">>, Priority}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ]),

    lager:info("loading ACDc queue: ~s", [QueueId]),
    {'ok', QueueJObj} = kz_datamgr:open_cache_doc(kapps_call:account_db(Call), QueueId),

    MaxWait = max_wait(kz_json:get_integer_value(<<"connection_timeout">>, QueueJObj, 3600)),
    MaxQueueSize = max_queue_size(kz_json:get_integer_value(<<"max_queue_size">>, QueueJObj, 0)),

    Call1 = kapps_call:kvs_store('caller_exit_key', kz_json:get_value(<<"caller_exit_key">>, QueueJObj, <<"#">>), Call),

    CurrQueueSize = kapi_acdc_queue:queue_size(kapps_call:account_id(Call1), QueueId),

    lager:info("max size: ~p curr size: ~p", [MaxQueueSize, CurrQueueSize]),

    maybe_enter_queue(#member_call{call=Call1
                                  ,config_data=MemberCall
                                  ,queue_id=QueueId
                                  ,max_wait=MaxWait
                                  }
                     ,is_queue_full(MaxQueueSize, CurrQueueSize)
                     ).

-spec lookup_priority(kz_json:object(), kapps_call:call()) -> kz_term:api_binary().
lookup_priority(Data, Call) ->
    FromData = kz_json:get_integer_value(<<"priority">>, Data),
    FromCall = kapps_call:custom_channel_var(<<"Call-Priority">>, Call),
    case {FromData, FromCall} of
        {FromData, _} when is_integer(FromData) -> FromData;
        {_, FromCall} when is_binary(FromCall) -> kz_term:to_integer(FromCall);
        _ -> 'undefined'
    end.

-spec maybe_enter_queue(member_call(), boolean()) -> any().
maybe_enter_queue(#member_call{call=Call}, 'true') ->
    lager:info("queue has reached max size"),
    cf_exe:continue(Call);
maybe_enter_queue(#member_call{call=Call
                              ,config_data=MemberCall
                              ,queue_id=QueueId
                              ,max_wait=MaxWait
                              }=MC
                 ,'false') ->
    lager:info("asking for an agent, waiting up to ~p ms", [MaxWait]),

    cf_exe:amqp_send(Call, MemberCall, fun kapi_acdc_queue:publish_member_call/1),
    _ = kapps_call_command:flush_dtmf(Call),
    wait_for_bridge(MC#member_call{call=kapps_call:kvs_store('queue_id', QueueId, Call)}
                   ,MaxWait
                   ).

-spec wait_for_bridge(member_call(), max_wait()) -> 'ok'.
wait_for_bridge(MC, Timeout) ->
    wait_for_bridge(MC, Timeout, kz_time:start_time()).

-spec wait_for_bridge(member_call(), max_wait(), kz_time:start_time()) -> 'ok'.
wait_for_bridge(#member_call{call=Call}, Timeout, _Start) when Timeout < 0 ->
    lager:debug("timeout is less than 0: ~p", [Timeout]),
    end_member_call(Call);
wait_for_bridge(#member_call{call=Call}=MC, Timeout, Start) ->
    Wait = kz_time:start_time(),
    TimeoutMs = case Timeout of
                    'infinity' -> 'infinity';
                    _ -> Timeout * ?MILLISECONDS_IN_SECOND
                end,
    receive
        {'amqp_msg', JObj} ->
            process_message(MC, Timeout, Start, Wait, JObj, kz_util:get_event_type(JObj))
    after TimeoutMs ->
            lager:info("failed to handle the call in time, proceeding"),
            end_member_call(Call)
    end.

end_member_call(Call) ->
    cancel_member_call(Call, ?MEMBER_TIMEOUT),
    stop_hold_music(Call),
    cf_exe:continue(Call).

-spec process_message(member_call(), max_wait(), kz_time:start_time()
                     ,kz_time:start_time(), kz_json:object()
                     ,{kz_term:ne_binary(), kz_term:ne_binary()}
                     ) -> 'ok'.
process_message(#member_call{call=Call}, _, Start, _Wait, _JObj, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}) ->
    lager:info("member was bridged to agent, yay! took ~b s", [kz_time:elapsed_s(Start)]),
    cf_exe:control_usurped(Call);
process_message(#member_call{call=Call}, _, Start, _Wait, _JObj, {<<"call_event">>,<<"CHANNEL_DESTROY">>}) ->
    lager:info("member hungup while waiting in the queue (was there ~b s)", [kz_time:elapsed_s(Start)]),
    cancel_member_call(Call, ?MEMBER_HANGUP),
    cf_exe:stop(Call);
process_message(#member_call{call=Call
                            ,queue_id=QueueId
                            }=MC, Timeout, Start, Wait, JObj, {<<"member">>, <<"call_fail">>}) ->
    case QueueId =:= kz_json:get_value(<<"Queue-ID">>, JObj) of
        'true' ->
            Failure = kz_json:get_value(<<"Failure-Reason">>, JObj),
            lager:info("call failed to be processed: ~s (took ~b s)"
                      ,[Failure, kz_time:elapsed_s(Start)]
                      ),
            cancel_member_call(Call, Failure),
            stop_hold_music(Call),
            cf_exe:continue(Call);
        'false' ->
            lager:info("failure json was for a different queue, ignoring"),
            wait_for_bridge(MC, kz_time:decr_timeout(Timeout, Wait), Start)
    end;
process_message(#member_call{call=Call}=MC, Timeout, Start, Wait, JObj, {<<"call_event">>, <<"DTMF">>}) ->
    DigitPressed = kz_json:get_value(<<"DTMF-Digit">>, JObj),
    case DigitPressed =:= kapps_call:kvs_fetch('caller_exit_key', Call) of
        'true' ->
            lager:info("caller pressed the exit key(~s), moving to next callflow action", [DigitPressed]),
            cancel_member_call(Call, <<"dtmf_exit">>),
            _ = kapps_call_command:flush_dtmf(Call),
            timer:sleep(?MILLISECONDS_IN_SECOND),
            cf_exe:continue(Call);
        'false' ->
            lager:info("caller pressed ~s, ignoring", [DigitPressed]),
            wait_for_bridge(MC, kz_time:decr_timeout(Timeout, Wait), Start)
    end;
process_message(#member_call{call=Call}, _, Start, _Wait, _JObj, {<<"member">>, <<"call_success">>}) ->
    lager:info("call was processed by queue (took ~b s)", [kz_time:elapsed_s(Start)]),
    cf_exe:control_usurped(Call);
process_message(MC, Timeout, Start, Wait, _JObj, _Type) ->
    wait_for_bridge(MC, kz_time:decr_timeout(Timeout, Wait), Start).

-spec max_wait(integer()) -> max_wait().
max_wait(N) when N < 1 -> 'infinity';
max_wait(N) -> N.

max_queue_size(N) when is_integer(N), N > 0 -> N;
max_queue_size(_) -> 0.

-spec is_queue_full(non_neg_integer(), non_neg_integer()) -> boolean().
is_queue_full(0, _) -> 'false';
is_queue_full(MaxQueueSize, CurrQueueSize) -> CurrQueueSize >= MaxQueueSize.

-spec cancel_member_call(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
cancel_member_call(Call, <<"timeout">>) ->
    lager:info("update reason from `timeout` to `member_timeout`"),
    cancel_member_call(Call, ?MEMBER_TIMEOUT);
cancel_member_call(Call, Reason) ->
    AcctId = kapps_call:account_id(Call),
    {'ok', QueueId} = kapps_call:kvs_find('queue_id', Call),
    CallId = kapps_call:call_id(Call),

    Req = props:filter_undefined(
            [{<<"Account-ID">>, AcctId}
            ,{<<"Queue-ID">>, QueueId}
            ,{<<"Call-ID">>, CallId}
            ,{<<"Reason">>, Reason}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    kapi_acdc_queue:publish_member_call_cancel(Req).

stop_hold_music(Call) ->
    Cmd = [{<<"Application-Name">>, <<"play">>}
          ,{<<"Call-ID">>, kapps_call:call_id(Call)}
          ,{<<"Media-Name">>, <<"silence_stream://50">>}
          ,{<<"Insert-At">>, <<"now">>}
          ],
    kapps_call_command:send_command(Cmd, Call).
