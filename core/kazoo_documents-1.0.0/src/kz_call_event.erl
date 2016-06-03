%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz
%%% @doc
%%% Call Event JSON Object
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_call_event).

-export([call_id/1
         ,other_leg_call_id/1
         ,other_leg_destination_number/1, other_leg_destination_number/2
         ,replaced_by/1
         ,custom_channel_vars/1, custom_channel_var/2, custom_channel_var/3
         ,custom_sip_headers/1
         ,authorizing_id/1, authorizing_type/1, is_authorized/1
         ,dtmf_digit/1
         ,event_name/1
         ,hangup_cause/1, hangup_code/1
         ,application_name/1, application_response/1
         ,response_message/1, response_code/1
         ,account_id/1
         ,owner_id/1
         ,timestamp/1
         ,ringing_seconds/1, billing_seconds/1, duration_seconds/1
         ,is_call_forwarded/1, is_call_forwarded/2
        ]).

-include("kz_documents.hrl").

-spec call_id(wh_json:object()) -> api_binary().
call_id(JObj) ->
    wh_json:get_value(<<"Call-ID">>, JObj).

-spec other_leg_call_id(wh_json:object()) -> api_binary().
other_leg_call_id(JObj) ->
    wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj).

-spec other_leg_destination_number(wh_json:object()) -> api_binary().
-spec other_leg_destination_number(wh_json:object(), Default) -> ne_binary() | Default.
other_leg_destination_number(JObj) ->
    other_leg_destination_number(JObj, 'undefined').
other_leg_destination_number(JObj, Default) ->
    wh_json:get_ne_value(<<"Other-Leg-Destination-Number">>, JObj, Default).

-spec replaced_by(wh_json:object()) -> api_binary().
replaced_by(JObj) ->
    wh_json:get_value(<<"Replaced-By">>, JObj).

-spec custom_channel_vars(wh_json:object()) -> api_object().
custom_channel_vars(JObj) ->
    wh_json:get_value(<<"Custom-Channel-Vars">>, JObj).

-spec custom_channel_var(wh_json:object(), wh_json:key()) ->
                                api_binary().
-spec custom_channel_var(wh_json:object(), wh_json:key(), Default) ->
                                ne_binary() | Default.
custom_channel_var(JObj, Key) ->
    custom_channel_var(JObj, Key, 'undefined').

custom_channel_var(JObj, Key, Default) ->
    wh_json:get_value([<<"Custom-Channel-Vars">>, Key], JObj, Default).

-spec custom_sip_headers(wh_json:object()) -> api_object().
custom_sip_headers(JObj) ->
    wh_json:get_value(<<"Custom-SIP-Headers">>, JObj).

-spec authorizing_id(wh_json:object()) -> api_binary().
authorizing_id(JObj) ->
    custom_channel_var(JObj, <<"Authorizing-ID">>).

-spec is_authorized(wh_json:object()) -> boolean().
is_authorized(JObj) ->
    wh_util:is_true(
      custom_channel_var(JObj, <<"Channel-Authorized">>)
     ).

-spec authorizing_type(wh_json:object()) -> api_binary().
authorizing_type(JObj) ->
    custom_channel_var(JObj, <<"Authorizing-Type">>).

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

-spec application_name(wh_json:object()) -> api_binary().
application_name(JObj) ->
    wh_json:get_value(<<"Application-Name">>, JObj).

-spec application_response(wh_json:object()) -> api_binary().
application_response(JObj) ->
    wh_json:get_value(<<"Application-Response">>, JObj).

-spec response_message(wh_json:object()) -> api_binary().
response_message(JObj) ->
    wh_json:get_value(<<"Response-Message">>, JObj).

-spec response_code(wh_json:object()) -> api_binary().
response_code(JObj) ->
    wh_json:get_value(<<"Response-Code">>, JObj).

-spec account_id(wh_json:object()) -> api_binary().
account_id(JObj) ->
    custom_channel_var(JObj, <<"Account-ID">>).

-spec owner_id(wh_json:object()) -> api_binary().
owner_id(JObj) ->
    custom_channel_var(JObj, <<"Owner-ID">>).

-spec timestamp(wh_json:object()) -> api_integer().
timestamp(JObj) ->
    wh_json:get_integer_value(<<"Timestamp">>, JObj).

-spec ringing_seconds(wh_json:object()) -> api_integer().
ringing_seconds(JObj) ->
    wh_json:get_integer_value(<<"Ringing-Seconds">>, JObj).

-spec billing_seconds(wh_json:object()) -> api_integer().
billing_seconds(JObj) ->
    wh_json:get_integer_value(<<"Billing-Seconds">>, JObj).

-spec duration_seconds(wh_json:object()) -> api_integer().
duration_seconds(JObj) ->
    wh_json:get_integer_value(<<"Duration-Seconds">>, JObj).

-spec is_call_forwarded(wh_json:object()) -> api_boolean().
-spec is_call_forwarded(wh_json:object(), Default) -> boolean() | Default.
is_call_forwarded(JObj) ->
    is_call_forwarded(JObj, 'undefined').
is_call_forwarded(JObj, Default) ->
    case custom_channel_var(JObj, <<"Call-Forward">>, Default) of
        'undefined' -> Default;
        Default -> Default;
        IsForwarded -> wh_util:is_true(IsForwarded)
    end.
