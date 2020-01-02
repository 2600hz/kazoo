%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2020, 2600Hz
%%% @doc FreeSWITCH json
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_evt_freeswitch).

-export([account_id/1, account_billing/1, account_trunk_usage/1
        ,application_name/1, raw_application_name/1
        ,authorizing_id/1
        ,authorizing_type/1
        ,call_direction/1, original_call_direction/1
        ,call_id/1
        ,callee_id_name/1, callee_id_name/2
        ,callee_id_number/1, callee_id_number/2
        ,caller_id_name/1, caller_id_name/2
        ,caller_id_number/1, caller_id_number/2
        ,ccvs/1, ccv/2, ccv/3
        ,set_ccv/3, set_ccvs/2
        ,cavs/1
        ,channel_authorized/1
        ,conference_name/1, conference_profile_name/1, conference_uuid/1
        ,dialed_number/1
        ,disposition/1
        ,event_name/1
        ,from_network_ip/1, from_network_port/1
        ,from_tag/1, to_tag/1
        ,hangup_code/1, hangup_cause/1
        ,hostname/1, hostname/2
        ,hunt_destination_number/1
        ,is_channel_recovering/1, is_channel_recovering/2
        ,is_consuming_global_resource/1, is_consuming_global_resource/2
        ,is_loopback/1, loopback_other_leg/1, loopback_leg_name/1
        ,join_time/1, join_time/2
        ,media_recorder/1
        ,origination_call_id/1
        ,other_leg_call_id/1
        ,outbound_flags/1
        ,presence_id/1, presence_direction/1
        ,reseller_id/1, reseller_billing/1, reseller_trunk_usage/1
        ,resource_id/1
        ,resource_type/1, resource_type/2
        ,to_did/1
        ,user_agent/1
        ,is_call_setup/1

        ,core_uuid/1, core_uuid_atom/1
        ,fetch_uuid/1
        ,fetch_section/1
        ,fetch_winning_pid/1
        ,switch_url/1, switch_uri/1
        ]).

-include("kz_documents.hrl").

-type data() :: kz_json:object().
-export_type([data/0]).

-define(CCVs, <<"Custom-Channel-Vars">>).

-spec caller_id_name(data()) -> kz_term:api_binary().
caller_id_name(JObj) ->
    caller_id_name(JObj, 'undefined').

-spec caller_id_name(data(), Default) -> kz_term:ne_binary() | Default.
caller_id_name(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, JObj, Default).


-spec caller_id_number(data()) -> kz_term:api_binary().
caller_id_number(JObj) ->
    caller_id_number(JObj, 'undefined').

-spec caller_id_number(data(), Default) -> kz_term:ne_binary() | Default.
caller_id_number(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, JObj, Default).

-spec callee_id_name(data()) -> kz_term:api_binary().
callee_id_name(JObj) ->
    callee_id_name(JObj, 'undefined').

-spec callee_id_name(data(), Default) -> kz_term:ne_binary() | Default.
callee_id_name(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Callee-ID-Name">>, JObj, Default).

-spec callee_id_number(data()) -> kz_term:api_binary().
callee_id_number(JObj) ->
    callee_id_number(JObj, 'undefined').

-spec callee_id_number(data(), Default) -> kz_term:ne_binary() | Default.
callee_id_number(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Callee-ID-Number">>, JObj, Default).

-spec dialed_number(data()) -> kz_term:api_binary().
dialed_number(JObj) ->
    kz_json:get_ne_binary_value(<<"Dialed-Number">>, JObj).

-spec call_id(data()) -> kz_term:api_binary().
call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Call-ID">>, JObj).

-spec other_leg_call_id(data()) -> kz_term:api_binary().
other_leg_call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Other-Leg-Unique-ID">>, JObj).

-spec original_call_direction(data()) -> kz_term:api_binary().
original_call_direction(JObj) ->
    kz_json:get_binary_value(<<"Call-Direction">>, JObj).

-spec call_direction(data()) -> kz_term:api_ne_binary().
call_direction(JObj) ->
    kz_json:get_ne_binary_value(<<"Call-Direction">>, JObj).

-spec resource_type(data()) -> kz_term:api_binary().
resource_type(JObj) ->
    resource_type(JObj, 'undefined').

-spec resource_type(data(), Default) -> kz_term:ne_binary() | Default.
resource_type(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"Resource-Type">>, JObj, Default).

-spec channel_authorized(data()) -> kz_term:api_binary().
channel_authorized(JObj) ->
    ccv(JObj, <<"Channel-Authorized">>).

-spec outbound_flags(data()) -> kz_term:api_binary() | kz_term:ne_binaries().
outbound_flags(JObj) ->
    ccv(JObj, <<"Outbound-Flags">>).

-spec hunt_destination_number(data()) -> kz_term:api_binary().
hunt_destination_number(JObj) ->
    kz_json:get_ne_binary_value(<<"Destination-Number">>, JObj).

-spec is_channel_recovering(data()) -> boolean().
is_channel_recovering(JObj) ->
    is_channel_recovering(JObj, 'false').

-spec is_channel_recovering(data(), boolean()) -> boolean().
is_channel_recovering(JObj, Default) ->
    kz_json:is_true(<<"Channel-Recovered">>, JObj, Default).

-spec is_consuming_global_resource(data()) -> kz_term:api_boolean().
is_consuming_global_resource(JObj) ->
    is_consuming_global_resource(JObj, 'undefined').

-spec is_consuming_global_resource(data(), kz_term:api_boolean()) -> kz_term:api_boolean().
is_consuming_global_resource(JObj, Default) ->
    kz_term:is_true(ccv(JObj, <<"Global-Resource">>, Default)).

-spec resource_id(data()) -> kz_term:api_binary().
resource_id(JObj) ->
    ccv(JObj, <<"Resource-ID">>).

-spec authorizing_id(data()) -> kz_term:api_binary().
authorizing_id(JObj) ->
    ccv(JObj, <<"Authorizing-ID">>).

-spec authorizing_type(data()) -> kz_term:api_binary().
authorizing_type(JObj) ->
    ccv(JObj, <<"Authorizing-Type">>).

-spec account_id(data()) -> kz_term:api_binary().
account_id(JObj) ->
    ccv(JObj, <<"Account-ID">>).

-spec account_billing(data()) -> kz_term:api_binary().
account_billing(JObj) ->
    ccv(JObj, <<"Account-Billing">>).

-spec account_trunk_usage(data()) -> kz_term:api_binary().
account_trunk_usage(JObj) ->
    ccv(JObj, <<"Account-Trunk-Usage">>).

-spec reseller_id(data()) -> kz_term:api_binary().
reseller_id(JObj) ->
    ccv(JObj, <<"Reseller-ID">>).

-spec reseller_billing(data()) -> kz_term:api_binary().
reseller_billing(JObj) ->
    ccv(JObj, <<"Reseller-Billing">>).

-spec reseller_trunk_usage(data()) -> kz_term:api_binary().
reseller_trunk_usage(JObj) ->
    ccv(JObj, <<"Reseller-Trunk-Usage">>).

-spec to_did(data()) -> kz_term:api_binary().
to_did(JObj) ->
    kz_json:get_ne_binary_value(<<"To-DID">>, JObj).

-spec hangup_code(data()) -> kz_term:api_binary().
hangup_code(JObj) ->
    kz_json:get_ne_binary_value(<<"Hangup-Code">>, JObj).

-spec disposition(data()) -> kz_term:api_binary().
disposition(JObj) ->
    kz_json:get_ne_binary_value(<<"Disposition">>, JObj).

-spec hangup_cause(data()) -> kz_term:api_binary().
hangup_cause(JObj) ->
    kz_json:get_ne_binary_value(<<"Hangup-Cause">>, JObj).

-spec raw_application_name(data()) -> kz_term:api_binary().
raw_application_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Raw-Application-Name">>, JObj).

-spec application_name(data()) -> kz_term:api_binary().
application_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Application-Name">>, JObj).

-spec event_name(data()) -> kz_term:api_binary().
event_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Event-Name">>, JObj).

-spec from_network_ip(data()) -> kz_term:api_binary().
from_network_ip(JObj) ->
    kz_json:get_ne_binary_value(<<"Network-IP">>, JObj).

-spec from_network_port(data()) -> kz_term:api_binary().
from_network_port(JObj) ->
    kz_json:get_ne_binary_value(<<"Network-Port">>, JObj).

-spec user_agent(data()) -> kz_term:api_binary().
user_agent(JObj) ->
    kz_json:get_ne_binary_value(<<"User-Agent">>, JObj).

-spec loopback_leg_name(data()) -> kz_term:api_binary().
loopback_leg_name(JObj) ->
    kz_json:get_value(<<"Channel-Loopback-Leg">>, JObj).

-spec is_loopback(data()) -> boolean().
is_loopback(JObj) ->
    kz_json:is_true(<<"Channel-Is-Loopback">>, JObj).

-spec loopback_other_leg(data()) -> kz_term:api_binary().
loopback_other_leg(JObj) ->
    kz_json:get_value(<<"Channel-Loopback-Other-Leg-ID">>, JObj).

-spec media_recorder(data()) -> kz_term:api_binary().
media_recorder(JObj) -> ccv(JObj, <<"Media-Recorder">>).

-spec presence_id(data()) -> kz_term:api_binary().
presence_id(JObj) ->
    kz_json:get_binary_value(<<"Channel-Presence-ID">>, JObj).

-spec presence_direction(data()) -> kz_term:api_binary().
presence_direction(JObj) ->
    kz_json:get_value(<<"Presence-Call-Direction">>, JObj).

-spec ccv(data(), kz_term:ne_binary()) -> kz_term:api_binary() | kz_term:ne_binaries().
ccv(JObj, Key) ->
    ccv(JObj, Key, 'undefined').

-spec ccv(data(), kz_term:ne_binary(), Default) -> kz_term:ne_binary() | kz_term:ne_binaries() | Default.
ccv(JObj, Key, Default) ->
    kz_json:get_ne_binary_value(Key, ccvs(JObj), Default).

-spec set_ccv(data(), kz_term:ne_binary(), term()) -> data().
set_ccv(JObj, Key, Value) ->
    kz_json:set_value([?CCVs, Key], JObj, Value).

-spec set_ccvs(data(), [tuple()]) -> data().
set_ccvs(JObj, Values) ->
    kz_json:set_values([{[?CCVs, Key], Value} || {Key, Value} <- Values], JObj).

%% Extract custom channel variables to include in the event
-spec ccvs(data()) -> data().
ccvs(JObj) ->
    kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()).

-spec cavs(data()) -> data().
cavs(JObj) ->
    kz_json:get_json_value(<<"Custom-Application-Vars">>, JObj, kz_json:new()).

-spec from_tag(data()) -> kz_term:api_binary().
from_tag(JObj) ->
    kz_json:get_ne_binary_value(<<"From-Tag">>, JObj).

-spec to_tag(data()) -> kz_term:api_binary().
to_tag(JObj) ->
    kz_json:get_ne_binary_value(<<"To-Tag">>, JObj).

-spec origination_call_id(data()) -> kz_term:api_binary().
origination_call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Origination-Call-ID">>, JObj).

-spec conference_name(data()) -> kz_term:api_ne_binary().
conference_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-Name">>, JObj).

-spec conference_profile_name(data()) -> kz_term:api_ne_binary().
conference_profile_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-Profile-Name">>, JObj).

-spec conference_uuid(data()) -> kz_term:api_ne_binary().
conference_uuid(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-Unique-ID">>, JObj).

-spec join_time(data()) -> kz_time:gregorian_seconds().
join_time(JObj) ->
    join_time(JObj, kz_time:now_s()).

-spec join_time(data(), Default) -> kz_time:gregorian_seconds() | Default.
join_time(JObj, Default) ->
    kz_json:get_integer_value(<<"Join-Time">>, JObj, Default).

-spec core_uuid(data()) -> kz_term:api_binary().
core_uuid(JObj) ->
    kz_json:get_ne_binary_value(<<"Core-UUID">>, JObj).

-spec core_uuid_atom(data()) -> atom().
core_uuid_atom(JObj) ->
    kz_json:get_atom_value(<<"Core-UUID">>, JObj).

-spec fetch_uuid(data()) -> kz_term:api_binary().
fetch_uuid(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-UUID">>, JObj).

-spec fetch_section(data()) -> kz_term:api_binary().
fetch_section(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Section">>, JObj).

-spec fetch_winning_pid(data()) -> kz_term:api_binary().
fetch_winning_pid(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Winning-PID">>, JObj).

-spec switch_url(data()) -> kz_term:api_binary().
switch_url(JObj) ->
    kz_json:get_ne_binary_value(<<"Switch-URL">>, JObj).

-spec switch_uri(data()) -> kz_term:api_binary().
switch_uri(JObj) ->
    kz_json:get_ne_binary_value(<<"Switch-URI">>, JObj).

-spec hostname(data()) -> kz_term:api_ne_binary().
hostname(JObj) ->
    hostname(JObj, 'undefined').

-spec hostname(data(), Default) -> kz_term:ne_binary() | Default.
hostname(JObj, Default) ->
    kz_json:get_ne_binary_value(<<"FreeSWITCH-Hostname">>, JObj, Default).

-spec is_call_setup(data()) -> boolean().
is_call_setup(JObj) ->
    kz_json:is_true(<<"Call-Setup">>, JObj, 'false').
