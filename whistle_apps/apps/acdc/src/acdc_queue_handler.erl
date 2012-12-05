%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_handler).

-export([handle_call_event/2
         ,handle_member_call/3
         ,handle_member_resp/2
         ,handle_member_accepted/2
         ,handle_member_retry/2
         ,handle_config_change/2
         ,handle_presence_probe/2
        ]).

-include("acdc.hrl").

handle_call_event(JObj, Props) ->
    true = wapi_call:event_v(JObj),
    {Cat, Name} = wh_util:get_event_type(JObj),
    acdc_queue_fsm:call_event(props:get_value(fsm_pid, Props), Cat, Name, JObj).

handle_member_call(JObj, Props, Delivery) ->
    true = wapi_acdc_queue:member_call_v(JObj),
    acdc_queue_fsm:member_call(props:get_value(fsm_pid, Props), JObj, Delivery).

handle_member_resp(JObj, Props) ->
    true = wapi_acdc_queue:member_connect_resp_v(JObj),
    acdc_queue_fsm:member_connect_resp(props:get_value(fsm_pid, Props), JObj).

handle_member_accepted(JObj, Props) ->
    true = wapi_acdc_queue:member_connect_accepted_v(JObj),
    acdc_queue_fsm:member_accepted(props:get_value(fsm_pid, Props), JObj).

handle_member_retry(JObj, Props) ->
    true = wapi_acdc_queue:member_connect_retry_v(JObj),
    acdc_queue_fsm:member_connect_retry(props:get_value(fsm_pid, Props), JObj).

handle_config_change(JObj, _Props) ->
    true = wapi_conf:doc_update_v(JObj),
    handle_queue_change(wh_json:get_value(<<"Doc">>, JObj)
                        ,wh_json:get_value(<<"pvt_account_id">>, JObj)
                        ,wh_json:get_value(<<"_id">>, JObj)
                        ,wh_json:get_value(<<"Event-Name">>, JObj)
                       ).

handle_queue_change(_JObj, AcctId, QueueId, <<"doc_created">>) ->
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        undefined -> acdc_queues_sup:new(AcctId, QueueId);
        P when is_pid(P) -> ok
    end;
handle_queue_change(JObj, AcctId, QueueId, <<"doc_edited">>) ->
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        undefined -> acdc_queues_sup:new(AcctId, QueueId);
        QueueSup when is_pid(QueueSup) ->
            WorkersSup = acdc_queue_sup:workers_sup(QueueSup),
            [acdc_queue_fsm:refresh(acdc_queue_worker_sup:fsm(WorkerSup), JObj)
             || WorkerSup <- acdc_queue_workers_sup:workers(WorkersSup)
            ]
    end;
handle_queue_change(_JObj, AcctId, QueueId, <<"doc_deleted">>) ->
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        undefined -> lager:debug("no queue(~s) started for account ~s", [QueueId, AcctId]);
        P when is_pid(P) ->
            lager:debug("stopping queue(~s) in account ~s (deleted): ~p", [QueueId, AcctId, P]),
            acdc_queue_sup:stop(P)
    end.

handle_presence_probe(JObj, _Props) ->
    true = wapi_notifications:presence_probe_v(JObj),

    FromRealm = wh_json:get_value(<<"From-Realm">>, JObj),
    case whapps_util:get_account_by_realm(FromRealm) of
        {ok, AcctDb} -> maybe_respond_to_presence_probe(JObj, AcctDb);
        _ -> ok
    end.

maybe_respond_to_presence_probe(JObj, AcctDb) ->
    case wh_json:get_value(<<"To-User">>, JObj) of
        undefined ->
            lager:debug("no to-user found on json: ~p", [JObj]);
        QueueId ->
            case couch_mgr:open_doc(AcctDb, QueueId) of
                {ok, Doc} ->
                    AcctId = wh_util:format_account_id(AcctDb, raw),
                    lager:debug("looking for probe for queue ~s(~s)", [QueueId, AcctId]),
                    maybe_update_probe(JObj, AcctId, QueueId, wh_json:get_value(<<"pvt_type">>, Doc));
                _ -> ok
            end
    end.

maybe_update_probe(JObj, AcctId, QueueId, <<"queue">>) ->
    update_probe(JObj, wapi_acdc_queue:queue_size(AcctId, QueueId));
maybe_update_probe(_, _, _, _) ->
    ok.

update_probe(JObj, 0) ->
    lager:debug("no calls in queue, greenify!"),
    send_probe(JObj, ?PRESENCE_GREEN);
update_probe(JObj, N) when is_integer(N), N > 0 ->
    lager:debug("~b calls in queue, redify!", [N]),
    send_probe(JObj, ?PRESENCE_RED_FLASH);
update_probe(_JObj, _Size) ->
    lager:debug("~p for queue size, ignore!", [_Size]).

send_probe(JObj, State) ->
    To = wh_json:get_value(<<"To">>, JObj),

    PresenceUpdate =
        [{<<"State">>, State}
         ,{<<"Presence-ID">>, To}
         ,{<<"Call-ID">>, wh_util:to_hex_binary(crypto:md5(To))}
         ,{<<"Switch-Nodename">>, wh_json:get_ne_value(<<"Switch-Nodename">>, JObj)}
         ,{<<"Subscription-Call-ID">>, wh_json:get_ne_value(<<"Subscription-Call-ID">>, JObj)}
         | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
        ],
    wapi_notifications:publish_presence_update(PresenceUpdate).
