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
         ,handle_stat_req/2
        ]).

-include("acdc.hrl").

handle_call_event(JObj, Props) ->
    {Cat, Name} = wh_util:get_event_type(JObj),
    acdc_queue_fsm:call_event(props:get_value(fsm_pid, Props), Cat, Name, JObj).

handle_member_call(JObj, Props, Delivery) ->
    acdc_queue_fsm:member_call(props:get_value(fsm_pid, Props), JObj, Delivery).

handle_member_resp(JObj, Props) ->
    acdc_queue_fsm:member_connect_resp(props:get_value(fsm_pid, Props), JObj).

handle_member_accepted(JObj, Props) ->
    acdc_queue_fsm:member_accepted(props:get_value(fsm_pid, Props), JObj).

handle_member_retry(JObj, Props) ->
    acdc_queue_fsm:member_connect_retry(props:get_value(fsm_pid, Props), JObj).

handle_config_change(JObj, _Props) ->
    true = wapi_conf:doc_update_v(JObj),
    handle_queue_change(wh_json:get_value(<<"Doc">>, JObj)
                        ,wh_json:get_value(<<"pvt_account_id">>, JObj)
                        ,wh_json:get_value(<<"_id">>, JObj)
                        ,wh_json:get_value(<<"Event-Name">>, JObj)
                       ).

handle_queue_change(JObj, AcctId, QueueId, <<"doc_created">>) ->
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        undefined -> acdc_queues_sup:new(JObj);
        P when is_pid(P) -> ok
    end;
handle_queue_change(JObj, AcctId, QueueId, <<"doc_edited">>) ->
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        undefined -> acdc_queues_sup:new(JObj);
        P when is_pid(P) -> acdc_queue_fsm:refresh(acdc_queue_sup:fsm(P), JObj)
    end;
handle_queue_change(_JObj, AcctId, QueueId, <<"doc_deleted">>) ->
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        undefined -> ok;
        P when is_pid(P) -> acdc_queue_sup:stop(P)
    end.

handle_stat_req(JObj, _Props) ->
    true = wapi_acdc_queue:stats_req_v(JObj),
    handle_stat_req(wh_json:get_value(<<"Account-ID">>, JObj)
                    ,wh_json:get_value(<<"Queue-ID">>, JObj)
                    ,wh_json:get_value(<<"Server-ID">>, JObj)
                   ).

handle_stat_req(AcctId, undefined, ServerId) ->
    lager:debug("fetch queue stats for all queues in ~s", [AcctId]),
    case acdc_queues_sup:find_acct_supervisors(AcctId) of
        [] -> lager:debug("no queue processes");
        Ps ->
            Resp = [{<<"Account-ID">>, AcctId}
                    ,{<<"Current-Queue-Stats">>, get_queues_stats(Ps)}
                    ,{<<"Current-Account-Stats">>, acdc_stats:acct_stats(AcctId)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_acdc_queue:publish_stats_resp(ServerId, Resp)
    end;
handle_stat_req(AcctId, QueueId, ServerId) ->
    lager:debug("fetch queue stats for ~s in ~s", [QueueId, AcctId]),
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        undefined -> lager:debug("no queue processes");
        P ->
            Resp = [{<<"Account-ID">>, AcctId}
                    ,{<<"Current-Queue-Stats">>, get_queue_stats(P)}
                    ,{<<"Current-Account-Stats">>, acdc_stats:acct_stats(AcctId)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_acdc_queue:publish_stats_resp(ServerId, Resp)
    end.

-spec get_queues_stats/1 :: ([pid(),...]) -> wh_json:json_objects().
get_queues_stats(Ps) ->
    [get_queue_stats(P) || P <- Ps].

-spec get_queue_stats/1 :: (pid()) -> wh_json:json_object().
get_queue_stats(P) ->
    QueuePid = acdc_queue_sup:queue(P),
    {AcctId, QueueId} = acdc_queue:config(QueuePid),
    QueueSize = wapi_acdc_queue:queue_size(AcctId, QueueId),

    wh_json:set_values([{<<"Queue-ID">>, QueueId}
                        ,{<<"Queue-Size">>, QueueSize}
                       ], wh_json:new()).
