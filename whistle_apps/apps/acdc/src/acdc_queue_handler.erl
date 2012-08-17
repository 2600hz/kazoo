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
