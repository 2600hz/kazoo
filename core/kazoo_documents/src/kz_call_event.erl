%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz
%%% @doc
%%% Call Event JSON Object
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_call_event).

-export([account_id/1, account_id/2
        ,application_data/1
        ,application_event/1
        ,application_name/1
        ,application_response/1
        ,authorizing_id/1
        ,authorizing_type/1
        ,billing_seconds/1
        ,call_direction/1, call_direction/2
        ,call_id/1
        ,custom_channel_var/2, custom_channel_var/3
        ,custom_channel_vars/1, custom_channel_vars/2
        ,custom_sip_headers/1
        ,disposition/1
        ,dtmf_digit/1
        ,duration_seconds/1
        ,error_message/1
        ,event_name/1
        ,hangup_cause/1, hangup_cause/2
        ,hangup_code/1
        ,is_authorized/1
        ,is_call_forwarded/1, is_call_forwarded/2
        ,other_leg_call_id/1
        ,other_leg_destination_number/1, other_leg_destination_number/2
        ,owner_id/1
        ,replaced_by/1
        ,response_code/1
        ,response_message/1
        ,ringing_seconds/1
        ,timestamp/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec call_direction(doc()) -> api_binary().
-spec call_direction(doc(), Default) -> ne_binary() | Default.
call_direction(JObj) ->
    call_direction(JObj, 'undefined').
call_direction(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Call-Direction">>, JObj, Default).

-spec call_id(doc()) -> api_binary().
call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Call-ID">>, JObj).

-spec other_leg_call_id(doc()) -> api_binary().
other_leg_call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Other-Leg-Call-ID">>, JObj).

-spec other_leg_destination_number(doc()) -> api_binary().
-spec other_leg_destination_number(doc(), Default) -> ne_binary() | Default.
other_leg_destination_number(JObj) ->
    other_leg_destination_number(JObj, 'undefined').
other_leg_destination_number(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Other-Leg-Destination-Number">>, JObj, Default).

-spec replaced_by(doc()) -> api_binary().
replaced_by(JObj) ->
    kz_json:get_ne_binary_value(<<"Replaced-By">>, JObj).

-spec custom_channel_vars(doc()) -> api_object().
-spec custom_channel_vars(doc(), Default) -> kz_json:object() | Default.
custom_channel_vars(JObj) ->
    custom_channel_vars(JObj, 'undefined').

custom_channel_vars(JObj, Default) ->
    kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, Default).

-spec custom_channel_var(doc(), kz_json:key()) ->
                                api_binary().
-spec custom_channel_var(doc(), kz_json:key(), Default) ->
                                ne_binary() | Default.
custom_channel_var(JObj, Key) ->
    custom_channel_var(JObj, Key, 'undefined').

custom_channel_var(JObj, Key, Default) ->
    kz_json:get_ne_binary_value([<<"Custom-Channel-Vars">>, Key], JObj, Default).

-spec custom_sip_headers(doc()) -> api_object().
custom_sip_headers(JObj) ->
    kz_json:get_json_value(<<"Custom-SIP-Headers">>, JObj).

-spec authorizing_id(doc()) -> api_binary().
authorizing_id(JObj) ->
    custom_channel_var(JObj, <<"Authorizing-ID">>).

-spec authorizing_type(doc()) -> api_binary().
authorizing_type(JObj) ->
    custom_channel_var(JObj, <<"Authorizing-Type">>).

-spec is_authorized(doc()) -> boolean().
is_authorized(JObj) ->
    kz_util:is_true(
      custom_channel_var(JObj, <<"Channel-Authorized">>)
     ).

-spec dtmf_digit(doc()) -> api_binary().
dtmf_digit(JObj) ->
    kz_json:get_ne_binary_value(<<"DTMF-Digit">>, JObj).

-spec event_name(doc()) -> api_binary().
event_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Event-Name">>, JObj).

-spec hangup_cause(doc()) -> api_binary().
-spec hangup_cause(doc(), Default) -> ne_binary() | Default.
hangup_cause(JObj) ->
    hangup_cause(JObj, 'undefined').
hangup_cause(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Hangup-Cause">>, JObj, Default).

-spec hangup_code(doc()) -> api_binary().
hangup_code(JObj) ->
    kz_json:get_ne_binary_value(<<"Hangup-Code">>, JObj).

-spec disposition(doc()) -> api_binary().
disposition(JObj) ->
    kz_json:get_ne_binary_value(<<"Disposition">>, JObj).

-spec application_name(doc()) -> api_binary().
application_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Application-Name">>, JObj).

-spec application_event(doc()) -> api_binary().
application_event(JObj) ->
    kz_json:get_ne_binary_value(<<"Application-Event">>, JObj).

-spec application_data(doc()) -> kz_json:object().
application_data(JObj) ->
    kz_json:get_json_value(<<"Application-Data">>, JObj, kz_json:new()).

-spec application_response(doc()) -> api_binary().
application_response(JObj) ->
    kz_json:get_ne_binary_value(<<"Application-Response">>, JObj).

-spec response_message(doc()) -> api_binary().
response_message(JObj) ->
    kz_json:get_ne_binary_value(<<"Response-Message">>, JObj).

-spec response_code(doc()) -> api_binary().
response_code(JObj) ->
    kz_json:get_ne_binary_value(<<"Response-Code">>, JObj).

-spec account_id(doc()) -> api_binary().
-spec account_id(doc(), Default) -> ne_binary() | Default.
account_id(JObj) ->
    account_id(JObj, 'undefined').
account_id(JObj, Default) ->
    custom_channel_var(JObj, <<"Account-ID">>, Default).

-spec owner_id(doc()) -> api_binary().
owner_id(JObj) ->
    custom_channel_var(JObj, <<"Owner-ID">>).

-spec timestamp(doc()) -> api_integer().
timestamp(JObj) ->
    kz_json:get_integer_value(<<"Timestamp">>, JObj).

-spec ringing_seconds(doc()) -> api_integer().
ringing_seconds(JObj) ->
    kz_json:get_integer_value(<<"Ringing-Seconds">>, JObj).

-spec billing_seconds(doc()) -> api_integer().
billing_seconds(JObj) ->
    kz_json:get_integer_value(<<"Billing-Seconds">>, JObj).

-spec duration_seconds(doc()) -> api_integer().
duration_seconds(JObj) ->
    kz_json:get_integer_value(<<"Duration-Seconds">>, JObj).

-spec is_call_forwarded(doc()) -> api_boolean().
-spec is_call_forwarded(doc(), Default) -> boolean() | Default.
is_call_forwarded(JObj) ->
    is_call_forwarded(JObj, 'undefined').
is_call_forwarded(JObj, Default) ->
    case custom_channel_var(JObj, <<"Call-Forward">>, Default) of
        'undefined' -> Default;
        Default -> Default;
        IsForwarded -> kz_util:is_true(IsForwarded)
    end.

-spec error_message(doc()) -> api_binary().
error_message(JObj) ->
    kz_json:get_ne_binary_value(<<"Error-Message">>, JObj).
