%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Call Event JSON Object
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_call_event).

-export([call_id/1
         ,other_leg_call_id/1
         ,custom_channel_vars/1
         ,custom_sip_headers/1
        ]).

-include("kz_documents.hrl").

-spec call_id(wh_json:object()) -> api_binary().
call_id(JObj) ->
    wh_json:get_value(<<"Call-ID">>, JObj).

-spec other_leg_call_id(wh_json:object()) -> api_binary().
other_leg_call_id(JObj) ->
    wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj).

-spec custom_channel_vars(wh_json:object()) -> api_object().
custom_channel_vars(JObj) ->
    wh_json:get_value(<<"Custom-Channel-Vars">>, JObj).

-spec custom_sip_headers(wh_json:object()) -> api_object().
custom_sip_headers(JObj) ->
    wh_json:get_value(<<"Custom-SIP-Headers">>, JObj).
