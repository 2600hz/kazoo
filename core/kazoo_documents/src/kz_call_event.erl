%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc Call Event JSON Object
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_call_event).

-export([account_id/1, account_id/2
        ,application_data/1
        ,application_event/1
        ,application_name/1
        ,application_response/1
        ,application_uuid/1
        ,authorizing_id/1
        ,authorizing_type/1
        ,billing_seconds/1
        ,call_direction/1, call_direction/2
        ,call_id/1
        ,channel_answer_state/1
        ,channel_call_state/1
        ,channel_name/1
        ,channel_state/1
        ,custom_channel_var/2, custom_channel_var/3
        ,custom_channel_vars/1, custom_channel_vars/2
        ,custom_application_var/2, custom_application_var/3
        ,custom_application_vars/1, custom_application_vars/2
        ,custom_sip_headers/1
        ,disposition/1
        ,dtmf_digit/1
        ,duration_seconds/1
        ,error_message/1, error_message/2
        ,event_name/1
        ,hangup_cause/1, hangup_cause/2
        ,hangup_code/1
        ,is_authorized/1
        ,is_call_forwarded/1, is_call_forwarded/2
        ,other_leg_call_id/1
        ,other_leg_destination_number/1, other_leg_destination_number/2
        ,owner_id/1
        ,replaced_by/1
        ,request/1
        ,response_code/1
        ,response_message/1
        ,ringing_seconds/1
        ,switch_nodename/1
        ,timestamp/1
        ,caller_id/1, caller_id_number/1, caller_id_name/1
        ,callee_id/1, callee_id_number/1, callee_id_name/1
        ,recording_length/1
        ,fetch_id/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec call_direction(doc()) -> kz_term:api_ne_binary().
call_direction(JObj) ->
    call_direction(JObj, 'undefined').

-spec call_direction(doc(), Default) -> kz_term:ne_binary() | Default.
call_direction(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Call-Direction">>, JObj, Default).

-spec call_id(doc()) -> kz_term:api_ne_binary().
call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Call-ID">>, JObj).

-spec other_leg_call_id(doc()) -> kz_term:api_ne_binary().
other_leg_call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Other-Leg-Call-ID">>, JObj).

-spec other_leg_destination_number(doc()) -> kz_term:api_ne_binary().
other_leg_destination_number(JObj) ->
    other_leg_destination_number(JObj, 'undefined').

-spec other_leg_destination_number(doc(), Default) -> kz_term:ne_binary() | Default.
other_leg_destination_number(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Other-Leg-Destination-Number">>, JObj, Default).

-spec replaced_by(doc()) -> kz_term:api_ne_binary().
replaced_by(JObj) ->
    kz_json:get_ne_binary_value(<<"Replaced-By">>, JObj).

-spec request(doc()) -> kz_json:object().
request(JObj) ->
    kz_json:get_json_value(<<"Request">>, JObj).

-spec channel_name(doc()) -> kz_term:api_ne_binary().
channel_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Channel-Name">>, JObj).

-spec channel_state(doc()) -> kz_term:api_ne_binary().
channel_state(JObj) ->
    kz_json:get_ne_binary_value(<<"Channel-State">>, JObj).

-spec channel_call_state(doc()) -> kz_term:api_ne_binary().
channel_call_state(JObj) ->
    kz_json:get_ne_binary_value(<<"Channel-Call-State">>, JObj).

-spec channel_answer_state(doc()) -> kz_term:api_ne_binary().
channel_answer_state(JObj) ->
    kz_json:get_ne_binary_value(<<"Channel-Answer-State">>, JObj).

-spec custom_channel_vars(doc()) -> kz_term:api_object().
custom_channel_vars(JObj) ->
    custom_channel_vars(JObj, 'undefined').

-spec custom_channel_vars(doc(), Default) -> kz_json:object() | Default.
custom_channel_vars(JObj, Default) ->
    kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, Default).

-spec custom_channel_var(doc(), kz_json:key()) ->
                                kz_term:api_ne_binary().
custom_channel_var(JObj, Key) ->
    custom_channel_var(JObj, Key, 'undefined').

-spec custom_channel_var(doc(), kz_json:key(), Default) ->
                                kz_term:ne_binary() | Default.
custom_channel_var(JObj, Key, Default) ->
    kz_json:get_ne_binary_value([<<"Custom-Channel-Vars">>, Key], JObj, Default).

-spec custom_application_vars(doc()) -> kz_term:api_object().
custom_application_vars(JObj) ->
    custom_application_vars(JObj, 'undefined').

-spec custom_application_vars(doc(), Default) -> kz_json:object() | Default.
custom_application_vars(JObj, Default) ->
    kz_json:get_json_value(<<"Custom-Application-Vars">>, JObj, Default).

-spec custom_application_var(doc(), kz_json:key()) ->
                                    kz_term:api_ne_binary().
custom_application_var(JObj, Key) ->
    custom_application_var(JObj, Key, 'undefined').

-spec custom_application_var(doc(), kz_json:key(), Default) ->
                                    kz_term:ne_binary() | Default.
custom_application_var(JObj, Key, Default) ->
    kz_json:get_ne_binary_value([<<"Custom-Application-Vars">>, Key], JObj, Default).

-spec custom_sip_headers(doc()) -> kz_term:api_object().
custom_sip_headers(JObj) ->
    kz_json:get_json_value(<<"Custom-SIP-Headers">>, JObj).

-spec authorizing_id(doc()) -> kz_term:api_binary().
authorizing_id(JObj) ->
    custom_channel_var(JObj, <<"Authorizing-ID">>).

-spec authorizing_type(doc()) -> kz_term:api_binary().
authorizing_type(JObj) ->
    custom_channel_var(JObj, <<"Authorizing-Type">>).

-spec is_authorized(doc()) -> boolean().
is_authorized(JObj) ->
    kz_term:is_true(
      custom_channel_var(JObj, <<"Channel-Authorized">>)
     ).

-spec dtmf_digit(doc()) -> kz_term:api_ne_binary().
dtmf_digit(JObj) ->
    kz_json:get_ne_binary_value(<<"DTMF-Digit">>, JObj).

-spec event_name(doc()) -> kz_term:api_ne_binary().
event_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Event-Name">>, JObj).

-spec hangup_cause(doc()) -> kz_term:api_ne_binary().
hangup_cause(JObj) ->
    hangup_cause(JObj, 'undefined').

-spec hangup_cause(doc(), Default) -> kz_term:ne_binary() | Default.
hangup_cause(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Hangup-Cause">>, JObj, Default).

-spec hangup_code(doc()) -> kz_term:api_ne_binary().
hangup_code(JObj) ->
    kz_json:get_ne_binary_value(<<"Hangup-Code">>, JObj).

-spec disposition(doc()) -> kz_term:api_ne_binary().
disposition(JObj) ->
    kz_json:get_ne_binary_value(<<"Disposition">>, JObj).

-spec application_name(doc()) -> kz_term:api_ne_binary().
application_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Application-Name">>, JObj).

-spec application_event(doc()) -> kz_term:api_ne_binary().
application_event(JObj) ->
    kz_json:get_ne_binary_value(<<"Application-Event">>, JObj).

-spec application_data(doc()) -> kz_json:object().
application_data(JObj) ->
    kz_json:get_json_value(<<"Application-Data">>, JObj, kz_json:new()).

-spec application_uuid(doc()) -> kz_term:api_binary().
application_uuid(JObj) ->
    kz_json:get_ne_binary_value(<<"Application-UUID">>, JObj).

-spec application_response(doc()) -> kz_term:api_ne_binary().
application_response(JObj) ->
    kz_json:get_ne_binary_value(<<"Application-Response">>, JObj).

-spec response_message(doc()) -> kz_term:api_ne_binary().
response_message(JObj) ->
    kz_json:get_ne_binary_value(<<"Response-Message">>, JObj).

-spec response_code(doc()) -> kz_term:api_ne_binary().
response_code(JObj) ->
    kz_json:get_ne_binary_value(<<"Response-Code">>, JObj).

-spec account_id(doc()) -> kz_term:api_ne_binary().
account_id(JObj) ->
    account_id(JObj, 'undefined').

-spec account_id(doc(), Default) -> kz_term:ne_binary() | Default.
account_id(JObj, Default) ->
    custom_channel_var(JObj, <<"Account-ID">>, Default).

-spec owner_id(doc()) -> kz_term:api_ne_binary().
owner_id(JObj) ->
    custom_channel_var(JObj, <<"Owner-ID">>).

-spec timestamp(doc()) -> kz_term:api_integer().
timestamp(JObj) ->
    kz_json:get_integer_value(<<"Timestamp">>, JObj).

-spec ringing_seconds(doc()) -> kz_term:api_integer().
ringing_seconds(JObj) ->
    kz_json:get_integer_value(<<"Ringing-Seconds">>, JObj).

-spec billing_seconds(doc()) -> kz_term:api_integer().
billing_seconds(JObj) ->
    kz_json:get_integer_value(<<"Billing-Seconds">>, JObj).

-spec duration_seconds(doc()) -> kz_term:api_integer().
duration_seconds(JObj) ->
    kz_json:get_integer_value(<<"Duration-Seconds">>, JObj).

-spec is_call_forwarded(doc()) -> kz_term:api_boolean().
is_call_forwarded(JObj) ->
    is_call_forwarded(JObj, 'undefined').

-spec is_call_forwarded(doc(), Default) -> boolean() | Default.
is_call_forwarded(JObj, Default) ->
    case custom_channel_var(JObj, <<"Call-Forward">>, Default) of
        'undefined' -> Default;
        Default -> Default;
        IsForwarded -> kz_term:is_true(IsForwarded)
    end.

-spec error_message(doc()) -> kz_term:api_ne_binary().
error_message(JObj) ->
    error_message(JObj, 'undefined').

-spec error_message(doc(), Default) -> kz_term:ne_binary() | Default.
error_message(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Error-Message">>, JObj, Default).

-spec switch_nodename(doc()) -> kz_term:api_ne_binary().
switch_nodename(JObj) ->
    kz_json:get_ne_binary_value(<<"Switch-Nodename">>, JObj).

-spec caller_id_name(doc()) -> kz_term:api_ne_binary().
caller_id_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, JObj).

-spec caller_id_number(doc()) -> kz_term:api_ne_binary().
caller_id_number(JObj) ->
    kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, JObj).

-spec callee_id_name(doc()) -> kz_term:api_ne_binary().
callee_id_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Callee-ID-Name">>, JObj).

-spec callee_id_number(doc()) -> kz_term:api_ne_binary().
callee_id_number(JObj) ->
    kz_json:get_ne_binary_value(<<"Callee-ID-Number">>, JObj).

-spec caller_id(doc()) -> {kz_term:api_ne_binary(), kz_term:api_ne_binary()}.
caller_id(JObj) ->
    {caller_id_number(JObj), caller_id_name(JObj)}.

-spec callee_id(doc()) -> {kz_term:api_ne_binary(), kz_term:api_ne_binary()}.
callee_id(JObj) ->
    {callee_id_number(JObj), callee_id_name(JObj)}.

-spec recording_length(doc()) -> kz_term:api_integer().
recording_length(JObj) ->
    kz_json:get_integer_value(<<"Length">>, JObj).

-spec fetch_id(doc()) -> kz_term:api_ne_binary().
fetch_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-ID">>, JObj, custom_channel_var(JObj, <<"Fetch-ID">>)).
