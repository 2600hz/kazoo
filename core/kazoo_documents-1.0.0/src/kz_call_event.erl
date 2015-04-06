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
         ,replaced_by/1
         ,custom_channel_vars/1, custom_channel_var/2
         ,custom_sip_headers/1
         ,authorizing_id/1
         ,dtmf_digit/1
         ,event_name/1
         ,hangup_cause/1, hangup_code/1
         ,account_id/1
         ,timestamp/1
        ]).

-include("kz_documents.hrl").

-spec call_id(wh_json:object()) -> api_binary().
call_id(JObj) ->
    wh_json:get_value(<<"Call-ID">>, JObj).

-spec other_leg_call_id(wh_json:object()) -> api_binary().
other_leg_call_id(JObj) ->
    wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj).

-spec replaced_by(wh_json:object()) -> api_binary().
replaced_by(JObj) ->
    wh_json:get_value(<<"Replaced-By">>, JObj).

-spec custom_channel_vars(wh_json:object()) -> api_object().
custom_channel_vars(JObj) ->
    wh_json:get_value(<<"Custom-Channel-Vars">>, JObj).

-spec custom_channel_var(wh_json:object(), wh_json:key()) -> api_binary().
custom_channel_var(JObj, Key) ->
    wh_json:get_value([<<"Custom-Channel-Vars">>, Key], JObj).

-spec custom_sip_headers(wh_json:object()) -> api_object().
custom_sip_headers(JObj) ->
    wh_json:get_value(<<"Custom-SIP-Headers">>, JObj).

-spec authorizing_id(wh_json:object()) -> api_binary().
authorizing_id(JObj) ->
    custom_channel_var(JObj, <<"Authorizing-ID">>).

-spec dtmf_digit(wh_json:object()) -> api_binary().
dtmf_digit(JObj) ->
    wh_json:get_value(<<"DTMF-Digit">>, JObj).

-spec event_name(wh_json:object()) -> api_binary().
event_name(JObj) ->
    wh_json:get_value(<<"Event-Name">>, JObj).

-spec hangup_cause(wh_json:object()) -> api_binary().
hangup_cause(JObj) ->
    wh_json:get_value(<<"Hangup-Cause">>, JObj).

-spec hangup_code(wh_json:object()) -> api_binary().
hangup_code(JObj) ->
    wh_json:get_value(<<"Hangup-Code">>, JObj).

-spec account_id(wh_json:object()) -> api_binary().
account_id(JObj) ->
    custom_channel_var(JObj, <<"Account-ID">>).

-spec timestamp(wh_json:object()) -> api_integer().
timestamp(JObj) ->
    wh_json:get_integer_value(<<"Timestamp">>, JObj).
