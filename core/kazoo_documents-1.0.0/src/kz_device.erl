%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_device).

-export([sip_username/1, sip_username/2, set_sip_username/2
         ,sip_password/1, sip_password/2, set_sip_password/2
         ,sip_method/1, sip_method/2, set_sip_method/2
         ,sip_realm/1, sip_realm/2, set_sip_realm/2

         ,sip_settings/1, sip_settings/2, set_sip_settings/2

         ,presence_id/1, presence_id/2, set_presence_id/2

         ,name/1, name/2, set_name/2
        ]).

-include("kz_documents.hrl").

-define(SIP, <<"sip">>).
-define(USERNAME, [?SIP, <<"username">>]).
-define(PASSWORD, [?SIP, <<"password">>]).
-define(METHOD, [?SIP, <<"method">>]).
-define(REALM, [?SIP, <<"realm">>]).

-define(PRESENCE_ID, <<"presence_id">>).
-define(NAME, <<"name">>).

-spec sip_username(wh_json:object()) -> api_binary().
-spec sip_username(wh_json:object(), Default) -> ne_binary() | Default.
sip_username(DeviceJObj) ->
    sip_username(DeviceJObj, 'undefined').

sip_username(DeviceJObj, Default) ->
    wh_json:get_value(?USERNAME, DeviceJObj, Default).

-spec sip_password(wh_json:object()) -> api_binary().
-spec sip_password(wh_json:object(), Default) -> ne_binary() | Default.
sip_password(DeviceJObj) ->
    sip_password(DeviceJObj, 'undefined').

sip_password(DeviceJObj, Default) ->
    wh_json:get_value(?PASSWORD, DeviceJObj, Default).

-spec sip_method(wh_json:object()) -> api_binary().
-spec sip_method(wh_json:object(), Default) -> ne_binary() | Default.
sip_method(DeviceJObj) ->
    sip_method(DeviceJObj, 'undefined').

sip_method(DeviceJObj, Default) ->
    wh_json:get_value(?METHOD, DeviceJObj, Default).

-spec sip_realm(wh_json:object()) -> api_binary().
-spec sip_realm(wh_json:object(), Default) -> ne_binary() | Default.
sip_realm(DeviceJObj) ->
    sip_realm(DeviceJObj, 'undefined').

sip_realm(DeviceJObj, Default) ->
    wh_json:get_value(?REALM, DeviceJObj, Default).

-spec sip_settings(wh_json:object()) -> api_object().
-spec sip_settings(wh_json:object(), Default) -> wh_json:object() | Default.
sip_settings(DeviceJObj) ->
    sip_settings(DeviceJObj, 'undefined').

sip_settings(DeviceJObj, Default) ->
    wh_json:get_value(?SIP, DeviceJObj, Default).

-spec set_sip_username(wh_json:object(), ne_binary()) -> wh_json:object().
set_sip_username(DeviceJObj, Username) ->
    wh_json:set_value(?USERNAME, Username, DeviceJObj).

-spec set_sip_password(wh_json:object(), ne_binary()) -> wh_json:object().
set_sip_password(DeviceJObj, Password) ->
    wh_json:set_value(?PASSWORD, Password, DeviceJObj).

-spec set_sip_method(wh_json:object(), ne_binary()) -> wh_json:object().
set_sip_method(DeviceJObj, Method) ->
    wh_json:set_value(?METHOD, Method, DeviceJObj).

-spec set_sip_realm(wh_json:object(), ne_binary()) -> wh_json:object().
set_sip_realm(DeviceJObj, Realm) ->
    wh_json:set_value(?REALM, Realm, DeviceJObj).

-spec set_sip_settings(wh_json:object(), wh_json:object()) -> wh_json:object().
set_sip_settings(DeviceJObj, SipJObj) ->
    wh_json:set_value(?SIP, SipJObj, DeviceJObj).

-spec presence_id(wh_json:object()) -> api_binary().
-spec presence_id(wh_json:object(), Default) -> ne_binary() | Default.
presence_id(DeviceJObj) ->
    presence_id(DeviceJObj, 'undefined').
presence_id(DeviceJObj, Default) ->
    wh_json:get_value(?PRESENCE_ID, DeviceJObj, Default).

-spec set_presence_id(wh_json:object(), ne_binary()) -> wh_json:object().
set_presence_id(DeviceJObj, Id) ->
    wh_json:set_value(?PRESENCE_ID, Id, DeviceJObj).

-spec name(wh_json:object()) -> api_binary().
-spec name(wh_json:object(), Default) -> ne_binary() | Default.
name(DeviceJObj) ->
    name(DeviceJObj, 'undefined').
name(DeviceJObj, Default) ->
    wh_json:get_value(?NAME, DeviceJObj, Default).

-spec set_name(wh_json:object(), ne_binary()) -> wh_json:object().
set_name(DeviceJObj, Name) ->
    wh_json:set_value(?NAME, Name, DeviceJObj).
