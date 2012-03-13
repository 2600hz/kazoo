%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_handlers).

-export([handle_new_member/2
        ]).

-include("acdc.hrl").

handle_new_member(JObj, _Props) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),

    QueueId = wapi_queue:get_queue_id(JObj),

    ok.
