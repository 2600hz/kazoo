%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc FreeSWITCH proplists
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_conference_event).

-export([conference_id/1
        ,participant_id/1
        ,profile/1
        ,instance_id/1
        ,event/1
        ,call_id/1
        ,ccv/2, ccv/3
        ,custom_channel_vars/1
        ,conference_channel_vars/1
        ,custom_application_vars/1
        ,join_time/1, join_time/2
        ,core_uuid/1
        ,switch_url/1, switch_uri/1, switch_hostname/1
        ,conference_node/1
        ,account_id/1
        ,caller_id_number/1, caller_id_name/1
        ]).

-include("kz_documents.hrl").

-type data() :: kz_json:object().
-export_type([data/0]).


-spec call_id(data()) -> kz_term:api_binary().
call_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Call-ID">>, JObj).

-spec ccv(data(), kz_term:ne_binary()) -> kz_term:api_binary() | kz_term:ne_binaries().
ccv(JObj, Key) ->
    ccv(JObj, Key, 'undefined').

-spec ccv(data(), kz_term:ne_binary(), Default) -> kz_term:ne_binary() | kz_term:ne_binaries() | Default.
ccv(JObj, Key, Default) ->
    kz_json:get_value(Key, custom_channel_vars(JObj), Default).

-spec custom_channel_vars(data()) -> data().
custom_channel_vars(JObj) -> kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj).

-spec conference_channel_vars(data()) -> data().
conference_channel_vars(JObj) -> kz_json:get_json_value(<<"Conference-Channel-Vars">>, JObj).

-spec custom_application_vars(data()) -> data().
custom_application_vars(JObj) -> kz_json:get_json_value(<<"Custom-Application-Vars">>, JObj).

-spec event(data()) -> kz_term:api_binary().
event(JObj) ->
    kz_json:get_ne_binary_value(<<"Event">>, JObj).

-spec conference_id(data()) -> kz_term:api_ne_binary().
conference_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-ID">>, JObj).

-spec participant_id(data()) -> kz_term:api_ne_binary().
participant_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Participant-ID">>, JObj).

-spec profile(data()) -> kz_term:api_ne_binary().
profile(JObj) ->
    kz_json:get_ne_binary_value(<<"Profile">>, JObj).

-spec instance_id(data()) -> kz_term:api_ne_binary().
instance_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Instance-ID">>, JObj).

-spec join_time(data()) -> kz_time:gregorian_seconds().
join_time(JObj) ->
    join_time(JObj, kz_time:current_tstamp()).

-spec join_time(data(), Default) -> kz_time:gregorian_seconds() | Default.
join_time(JObj, Default) ->
    kz_json:get_integer_value(<<"Join-Time">>, JObj, Default).

-spec core_uuid(data()) -> kz_term:api_binary().
core_uuid(JObj) ->
    kz_json:get_value(<<"Core-UUID">>, JObj).

-spec switch_url(data()) -> kz_term:api_binary().
switch_url(JObj) ->
    kz_json:get_ne_binary_value(<<"Switch-URL">>, JObj).

-spec switch_uri(data()) -> kz_term:api_binary().
switch_uri(JObj) ->
    kz_json:get_ne_binary_value(<<"Switch-URI">>, JObj).

-spec switch_hostname(data()) -> kz_term:api_binary().
switch_hostname(JObj) ->
    kz_json:get_ne_binary_value(<<"Switch-Hostname">>, JObj).

-spec conference_node(data()) -> kz_term:api_binary().
conference_node(JObj) ->
    kz_json:get_ne_binary_value(<<"Conference-Node">>, JObj, ccv(JObj, <<"Ecallmgr-Node">>)).

-spec account_id(data()) -> kz_term:api_ne_binary().
account_id(JObj) ->
    kz_json:get_ne_binary_value(<<"Account-ID">>, JObj).

-spec caller_id_name(data()) -> kz_term:api_ne_binary().
caller_id_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, JObj).

-spec caller_id_number(data()) -> kz_term:api_ne_binary().
caller_id_number(JObj) ->
    kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, JObj).
