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
        ]).

-include("kz_documents.hrl").

-type data() :: kz_json:object().
-export_type([data/0]).


-spec call_id(data()) -> api_binary().
call_id(JObj) ->
    kz_json:get_binary_value(<<"Call-ID">>, JObj).

-spec ccv(data(), ne_binary()) -> api_binary() | ne_binaries().
ccv(JObj, Key) ->
    ccv(JObj, Key, 'undefined').

-spec ccv(data(), ne_binary(), Default) -> ne_binary() | ne_binaries() | Default.
ccv(JObj, Key, Default) ->
    kz_json:get_value(Key, ccvs(JObj), Default).

-spec ccvs(data()) -> data().
ccvs(JObj) ->
    kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()).

-spec core_uuid(data()) -> api_binary().
core_uuid(JObj) ->
    kz_json:get_value(<<"Core-UUID">>, JObj).

-spec fetch_uuid(data()) -> api_binary().
fetch_uuid(JObj) ->
    kz_json:get_binary_value(<<"Fetch-UUID">>, JObj).

-spec fetch_action(data()) -> api_binary().
fetch_action(JObj) ->
    fetch_action(JObj, 'undefined').

-spec fetch_action(data(), Default) -> api_binary() | Default.
fetch_action(JObj, Default) ->
    kz_json:get_binary_value(<<"Action">>, JObj, Default).

-spec fetch_key_name(data()) -> api_ne_binary().
fetch_key_name(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Key-Name">>, JObj).

-spec fetch_key_value(data()) -> api_ne_binary().
fetch_key_value(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Key-Value">>, JObj).

-spec fetch_node(data()) -> api_binary().
fetch_node(JObj) ->
    kz_json:get_value(<<"Node">>, JObj).

-spec fetch_section(data()) -> api_ne_binary().
fetch_section(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Section">>, JObj).

-spec fetch_tag(data()) -> api_ne_binary().
fetch_tag(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Tag">>, JObj).

-spec fetch_user(data()) -> api_ne_binary().
fetch_user(JObj) ->
    kz_json:get_ne_binary_value(<<"user">>, JObj).

-spec fetch_auth_endpoint(data()) -> api_binary().
fetch_auth_endpoint(JObj) ->
    list_to_binary([fetch_user(JObj), "@", fetch_key_value(JObj)]).

-spec fetch_winning_pid(data()) -> api_binary().
fetch_winning_pid(JObj) ->
    kz_json:get_binary_value(<<"Fetch-Winning-PID">>, JObj).

-spec fetch_version(data()) -> api_ne_binary().
fetch_version(JObj) ->
    kz_json:get_ne_binary_value(<<"Fetch-Version">>, JObj).

-spec controller_queue(data()) -> api_binary().
controller_queue(JObj) ->
    kz_json:get_ne_binary_value(<<"Controller-Queue">>, JObj).

-spec controller_pid(data()) -> api_binary().
controller_pid(JObj) ->
    kz_json:get_ne_binary_value(<<"Controller-PID">>, JObj).

-spec node(kz_json:object()) -> api_binary().
node(JObj) ->
    kz_json:get_atom_value(<<"Node">>, JObj).
