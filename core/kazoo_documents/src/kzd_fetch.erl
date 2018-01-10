%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% FreeSWITCH proplists
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_fetch).

-export([call_id/1
        ,ccvs/1, ccv/2, ccv/3
        ,fetch_user/1
        ,fetch_auth_endpoint/1
        ,fetch_action/1, fetch_action/2
        ,fetch_node/1
        ,core_uuid/1
        ,fetch_uuid/1
        ,fetch_key_name/1
        ,fetch_key_value/1
        ,fetch_tag/1
        ,fetch_section/1
        ,fetch_version/1
        ,fetch_winning_pid/1
        ,controller_queue/1, controller_pid/1
        ,node/1

        ,auth_nonce/1
        ,auth_response/1
        ,auth_expires/1
        ,from_network_ip/1
        ,from_network_port/1
        ,auth_from/1
        ,auth_to/1
        ,user_agent/1
        ,cshs/1

        ]).

-include("kz_documents.hrl").

-type data() :: kz_json:object().
-export_type([data/0]).


-spec call_id(data()) -> kz_term:api_binary().
call_id(JObj) ->
    kz_json:get_binary_value(<<"Call-ID">>, JObj).

-spec ccv(data(), kz_term:ne_binary()) -> kz_term:api_binary() | kz_term:ne_binaries().
ccv(JObj, Key) ->
    ccv(JObj, Key, 'undefined').

-spec ccv(data(), kz_term:ne_binary(), Default) -> kz_term:ne_binary() | kz_term:ne_binaries() | Default.
ccv(JObj, Key, Default) ->
    kz_json:get_value(Key, ccvs(JObj), Default).

-spec ccvs(data()) -> data().
ccvs(JObj) ->
    kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()).

-spec core_uuid(data()) -> kz_term:api_binary().
core_uuid(JObj) ->
    kz_json:get_value(<<"Core-UUID">>, JObj).

-spec fetch_uuid(data()) -> kz_term:api_binary().
fetch_uuid(JObj) ->
    kz_json:get_binary_value(<<"Fetch-UUID">>, JObj).

-spec fetch_action(data()) -> kz_term:api_binary().
fetch_action(JObj) ->
    fetch_action(JObj, 'undefined').

-spec fetch_action(data(), Default) -> kz_term:api_binary() | Default.
fetch_action(JObj, Default) ->
    kz_json:get_binary_value(<<"Action">>, JObj, Default).

-spec fetch_key_name(data()) -> kz_term:api_ne_binary().
fetch_key_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Key-Name">>, JObj).

-spec fetch_key_value(data()) -> kz_term:api_ne_binary().
fetch_key_value(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Key-Value">>, JObj).

-spec fetch_node(data()) -> kz_term:api_binary().
fetch_node(JObj) ->
    kz_json:get_value(<<"Node">>, JObj).

-spec fetch_section(data()) -> kz_term:api_ne_binary().
fetch_section(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Section">>, JObj).

-spec fetch_tag(data()) -> kz_term:api_ne_binary().
fetch_tag(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Tag">>, JObj).

-spec fetch_user(data()) -> kz_term:api_ne_binary().
fetch_user(JObj) ->
    kz_json:get_ne_binary_value(<<"user">>, JObj).

-spec fetch_auth_endpoint(data()) -> kz_term:api_binary().
fetch_auth_endpoint(JObj) ->
    list_to_binary([fetch_user(JObj), "@", fetch_key_value(JObj)]).

-spec fetch_winning_pid(data()) -> kz_term:api_binary().
fetch_winning_pid(JObj) ->
    kz_json:get_binary_value(<<"Fetch-Winning-PID">>, JObj).

-spec fetch_version(data()) -> kz_term:api_ne_binary().
fetch_version(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Version">>, JObj).

-spec controller_queue(data()) -> kz_term:api_binary().
controller_queue(JObj) ->
    kz_json:get_ne_binary_value(<<"Controller-Queue">>, JObj).

-spec controller_pid(data()) -> kz_term:api_binary().
controller_pid(JObj) ->
    kz_json:get_ne_binary_value(<<"Controller-PID">>, JObj).

-spec node(kz_json:object()) -> kz_term:api_binary().
node(JObj) ->
    kz_json:get_atom_value(<<"Node">>, JObj).

-spec from_network_ip(kz_json:object()) -> kz_term:api_binary().
from_network_ip(JObj) ->
    kz_json:get_ne_binary_value(<<"Network-IP">>, JObj).

-spec from_network_port(kz_json:object()) -> kz_term:api_binary().
from_network_port(JObj) ->
    kz_json:get_ne_binary_value(<<"Network-Port">>, JObj).

-spec user_agent(kz_json:object()) -> kz_term:api_binary().
user_agent(JObj) ->
    kz_json:get_ne_binary_value(<<"User-Agent">>, JObj).

-spec auth_expires(kz_json:object()) -> kz_term:api_binary().
auth_expires(JObj) ->
    kz_json:get_ne_binary_value(<<"Expires">>, JObj).

-spec auth_nonce(kz_json:object()) -> kz_term:api_binary().
auth_nonce(JObj) ->
    kz_json:get_ne_binary_value(<<"Auth-Nonce">>, JObj).

-spec auth_response(kz_json:object()) -> kz_term:api_binary().
auth_response(JObj) ->
    kz_json:get_ne_binary_value(<<"Auth-Response">>, JObj).

-spec cshs(kz_json:object()) -> kz_term:api_binary().
cshs(JObj) ->
    kz_json:get_json_value(<<"Custom-SIP-Headers">>, JObj, kz_json:new()).

-spec auth_from(kz_json:object()) -> kz_term:api_binary().
auth_from(JObj) ->
    kz_json:get_ne_binary_value(<<"Auth-From">>, JObj).

-spec auth_to(kz_json:object()) -> kz_term:api_binary().
auth_to(JObj) ->
    kz_json:get_ne_binary_value(<<"Auth-To">>, JObj).
