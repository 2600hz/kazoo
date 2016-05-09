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
         ,custom_channel_vars/1, custom_channel_vars/2
         ,custom_channel_var/2, custom_channel_var/3
         ,custom_sip_headers/1
         ,authorizing_id/1, authorizing_type/1
         ,dtmf_digit/1
         ,event_name/1
         ,hangup_cause/1, hangup_code/1, disposition/1
         ,application_name/1, application_response/1
         ,application_event/1, application_data/1
         ,response_message/1, response_code/1
         ,account_id/1
         ,owner_id/1
         ,timestamp/1
         ,ringing_seconds/1, billing_seconds/1, duration_seconds/1
         ,is_call_forwarded/1, is_call_forwarded/2
         ,error_message/1
        ]).

-include("kz_documents.hrl").

-spec call_id(kz_json:object()) -> api_binary().
call_id(JObj) ->
    kz_json:get_value(<<"Call-ID">>, JObj).

-spec other_leg_call_id(kz_json:object()) -> api_binary().
other_leg_call_id(JObj) ->
    kz_json:get_value(<<"Other-Leg-Call-ID">>, JObj).

-spec other_leg_destination_number(kz_json:object()) -> api_binary().
-spec other_leg_destination_number(kz_json:object(), Default) -> ne_binary() | Default.
other_leg_destination_number(JObj) ->
    other_leg_destination_number(JObj, 'undefined').
other_leg_destination_number(JObj, Default) ->
    kz_json:get_ne_value(<<"Other-Leg-Destination-Number">>, JObj, Default).

-spec replaced_by(kz_json:object()) -> api_binary().
replaced_by(JObj) ->
    kz_json:get_value(<<"Replaced-By">>, JObj).

-spec custom_channel_vars(kz_json:object()) -> api_object().
-spec custom_channel_vars(kz_json:object(), Default) -> api_object() | Default.
custom_channel_vars(JObj) ->
    custom_channel_vars(JObj, 'undefined').

custom_channel_vars(JObj, Default) ->
    kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, Default).

-spec custom_channel_var(kz_json:object(), kz_json:key()) ->
                                api_binary().
-spec custom_channel_var(kz_json:object(), kz_json:key(), Default) ->
                                ne_binary() | Default.
custom_channel_var(JObj, Key) ->
    custom_channel_var(JObj, Key, 'undefined').

custom_channel_var(JObj, Key, Default) ->
    kz_json:get_value([<<"Custom-Channel-Vars">>, Key], JObj, Default).

-spec custom_sip_headers(kz_json:object()) -> api_object().
custom_sip_headers(JObj) ->
    kz_json:get_value(<<"Custom-SIP-Headers">>, JObj).

-spec authorizing_id(kz_json:object()) -> api_binary().
authorizing_id(JObj) ->
    custom_channel_var(JObj, <<"Authorizing-ID">>).

-spec authorizing_type(kz_json:object()) -> api_binary().
authorizing_type(JObj) ->
    custom_channel_var(JObj, <<"Authorizing-Type">>).

-spec dtmf_digit(kz_json:object()) -> api_binary().
dtmf_digit(JObj) ->
    kz_json:get_value(<<"DTMF-Digit">>, JObj).

-spec event_name(kz_json:object()) -> api_binary().
event_name(JObj) ->
    kz_json:get_value(<<"Event-Name">>, JObj).

-spec hangup_cause(kz_json:object()) -> api_binary().
hangup_cause(JObj) ->
    kz_json:get_value(<<"Hangup-Cause">>, JObj).

-spec hangup_code(kz_json:object()) -> api_binary().
hangup_code(JObj) ->
    kz_json:get_value(<<"Hangup-Code">>, JObj).

-spec disposition(kz_json:object()) -> api_binary().
disposition(JObj) ->
    kz_json:get_value(<<"Disposition">>, JObj).

-spec application_name(kz_json:object()) -> api_binary().
application_name(JObj) ->
    kz_json:get_value(<<"Application-Name">>, JObj).

-spec application_event(kz_json:object()) -> api_binary().
application_event(JObj) ->
    kz_json:get_value(<<"Application-Event">>, JObj).

-spec application_data(kz_json:object()) -> kz_json:object().
application_data(JObj) ->
    kz_json:get_value(<<"Application-Data">>, JObj, kz_json:new()).

-spec application_response(kz_json:object()) -> api_binary().
application_response(JObj) ->
    kz_json:get_value(<<"Application-Response">>, JObj).

-spec response_message(kz_json:object()) -> api_binary().
response_message(JObj) ->
    kz_json:get_value(<<"Response-Message">>, JObj).

-spec response_code(kz_json:object()) -> api_binary().
response_code(JObj) ->
    kz_json:get_value(<<"Response-Code">>, JObj).

-spec account_id(kz_json:object()) -> api_binary().
account_id(JObj) ->
    custom_channel_var(JObj, <<"Account-ID">>).

-spec owner_id(kz_json:object()) -> api_binary().
owner_id(JObj) ->
    custom_channel_var(JObj, <<"Owner-ID">>).

-spec timestamp(kz_json:object()) -> api_integer().
timestamp(JObj) ->
    kz_json:get_integer_value(<<"Timestamp">>, JObj).

-spec ringing_seconds(kz_json:object()) -> api_integer().
ringing_seconds(JObj) ->
    kz_json:get_integer_value(<<"Ringing-Seconds">>, JObj).

-spec billing_seconds(kz_json:object()) -> api_integer().
billing_seconds(JObj) ->
    kz_json:get_integer_value(<<"Billing-Seconds">>, JObj).

-spec duration_seconds(kz_json:object()) -> api_integer().
duration_seconds(JObj) ->
    kz_json:get_integer_value(<<"Duration-Seconds">>, JObj).

-spec is_call_forwarded(kz_json:object()) -> api_boolean().
-spec is_call_forwarded(kz_json:object(), Default) -> boolean() | Default.
is_call_forwarded(JObj) ->
    is_call_forwarded(JObj, 'undefined').
is_call_forwarded(JObj, Default) ->
    case custom_channel_var(JObj, <<"Call-Forward">>, Default) of
        'undefined' -> Default;
        Default -> Default;
        IsForwarded -> kz_term:is_true(IsForwarded)
    end.

-spec error_message(kz_json:object()) -> api_binary().
error_message(JObj) ->
    kz_json:get_value(<<"Error-Message">>, JObj).
