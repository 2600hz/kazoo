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
        ]).

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
handle_queue_change(JObj, AcctId, QueueId, <<"doc_deleted">>) ->
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        undefined -> ok;
        P when is_pid(P) -> acdc_queue_sup:stop(P)
    end.
