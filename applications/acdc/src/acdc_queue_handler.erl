%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
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

-spec handle_call_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    'true' = wapi_call:event_v(JObj),
    {Cat, Name} = wh_util:get_event_type(JObj),
    handle_call_event(Cat, Name, JObj, Props).

handle_call_event(Category, <<"CHANNEL_DESTROY">> = Name, JObj, Props) ->
    case acdc_queue_fsm:cdr_url(props:get_value('fsm_pid', Props)) of
        'undefined' -> 'ok';
        Url ->
            acdc_util:send_cdr(Url, JObj)
    end,

    Srv = props:get_value('server', Props),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    acdc_util:unbind_from_call_events(CallId, Srv),
    acdc_queue_fsm:call_event(props:get_value('fsm_pid', Props)
                              ,Category
                              ,Name
                              ,JObj);
handle_call_event(Category, Name, JObj, Props) ->
    acdc_queue_fsm:call_event(props:get_value('fsm_pid', Props)
                              ,Category
                              ,Name
                              ,JObj).

handle_member_call(JObj, Props, Delivery) ->
    'true' = wapi_acdc_queue:member_call_v(JObj),
    acdc_queue_fsm:member_call(props:get_value('fsm_pid', Props), JObj, Delivery).

handle_member_resp(JObj, Props) ->
    'true' = wapi_acdc_queue:member_connect_resp_v(JObj),
    acdc_queue_fsm:member_connect_resp(props:get_value('fsm_pid', Props), JObj).

handle_member_accepted(JObj, Props) ->
    'true' = wapi_acdc_queue:member_connect_accepted_v(JObj),
    acdc_queue_fsm:member_accepted(props:get_value('fsm_pid', Props), JObj).

handle_member_retry(JObj, Props) ->
    'true' = wapi_acdc_queue:member_connect_retry_v(JObj),
    acdc_queue_fsm:member_connect_retry(props:get_value('fsm_pid', Props), JObj).

handle_config_change(JObj, Props) ->
    'true' = wapi_conf:doc_update_v(JObj),

    acdc_queue_manager:handle_config_change(props:get_value('server', Props), JObj),

    handle_queue_change(wh_json:get_value(<<"Database">>, JObj)
                        ,wh_json:get_value(<<"Account-ID">>, JObj)
                        ,wh_json:get_value(<<"ID">>, JObj)
                        ,wh_json:get_value(<<"Event-Name">>, JObj)
                       ).

handle_queue_change(_, AccountId, QueueId, <<"doc_created">>) ->
    lager:debug("maybe starting new queue for ~s: ~s", [AccountId, QueueId]),
    case acdc_queues_sup:find_queue_supervisor(AccountId, QueueId) of
        'undefined' -> acdc_queues_sup:new(AccountId, QueueId);
        P when is_pid(P) -> 'ok'
    end;
handle_queue_change(AccountDb, AccountId, QueueId, <<"doc_edited">>) ->
    lager:debug("maybe updating existing queue for ~s: ~s", [AccountId, QueueId]),
    case acdc_queues_sup:find_queue_supervisor(AccountId, QueueId) of
        'undefined' -> acdc_queues_sup:new(AccountId, QueueId);
        QueueSup when is_pid(QueueSup) ->
            {'ok', JObj} = couch_mgr:open_doc(AccountDb, QueueId),
            WorkersSup = acdc_queue_sup:workers_sup(QueueSup),
            [acdc_queue_fsm:refresh(acdc_queue_worker_sup:fsm(WorkerSup), JObj)
             || WorkerSup <- acdc_queue_workers_sup:workers(WorkersSup)
            ],
            Mgr = acdc_queue_sup:manager(QueueSup),
            acdc_queue_manager:refresh(Mgr, JObj)
    end;
handle_queue_change(_, AccountId, QueueId, <<"doc_deleted">>) ->
    lager:debug("maybe stopping existing queue for ~s: ~s", [AccountId, QueueId]),
    case acdc_queues_sup:find_queue_supervisor(AccountId, QueueId) of
        'undefined' -> lager:debug("no queue(~s) started for account ~s", [QueueId, AccountId]);
        P when is_pid(P) ->
            lager:debug("stopping queue(~s) in account ~s (deleted): ~p", [QueueId, AccountId, P]),
            acdc_queue_sup:stop(P)
    end.

handle_presence_probe(JObj, _Props) ->
    'true' = wapi_notifications:presence_probe_v(JObj),

    FromRealm = wh_json:get_value(<<"From-Realm">>, JObj),
    case whapps_util:get_account_by_realm(FromRealm) of
        {'ok', AcctDb} -> maybe_respond_to_presence_probe(JObj, wh_util:format_account_id(AcctDb, raw));
        _ -> 'ok'
    end.

maybe_respond_to_presence_probe(JObj, AcctId) ->
    case wh_json:get_value(<<"To-User">>, JObj) of
        'undefined' -> 'ok';
        QueueId ->
            update_probe(JObj
                         ,acdc_queues_sup:find_queue_supervisor(AcctId, QueueId)
                         ,AcctId, QueueId
                        )
    end.

update_probe(_JObj, 'undefined', _, _) -> 'ok';
update_probe(JObj, _Sup, AcctId, QueueId) ->
    case wapi_acdc_queue:queue_size(AcctId, QueueId) of
        0 ->
            lager:debug("no calls in queue, ignore!"),
            send_probe(JObj, ?PRESENCE_GREEN);
        N when is_integer(N), N > 0 ->
            lager:debug("~b calls in queue, redify!", [N]),
            send_probe(JObj, ?PRESENCE_RED_FLASH);
        _E ->
            lager:debug("unhandled size return: ~p", [_E])
    end.

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

