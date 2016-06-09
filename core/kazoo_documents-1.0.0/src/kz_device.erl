%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
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
         ,sip_ip/1, sip_ip/2, set_sip_ip/2
         ,sip_invite_format/1, sip_invite_format/2, set_sip_invite_format/2
         ,sip_route/1, sip_route/2, set_sip_route/2
         ,custom_sip_headers_inbound/1, custom_sip_headers_inbound/2, set_custom_sip_headers_inbound/2
         ,custom_sip_headers_outbound/1, custom_sip_headers_outbound/2, set_custom_sip_headers_outbound/2

         ,sip_settings/1, sip_settings/2, set_sip_settings/2

         ,presence_id/1, presence_id/2, set_presence_id/2
         ,name/1, name/2, set_name/2
         ,mac_address/1, mac_address/2, set_mac_address/2
         ,language/1, language/2, set_language/2
         ,device_type/1, device_type/2, set_device_type/2
         ,owner_id/1, owner_id/2, set_owner_id/2
         ,enabled/1, enabled/2, set_enabled/2
         ,timezone/1, timezone/2
         ,unsolicitated_mwi_updates/1, set_unsolicitated_mwi_updates/2

         ,new/0
         ,type/0
         ,is_device/1
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-type docs() :: [doc()].
-export_type([doc/0
              ,docs/0
             ]).

-define(SIP, <<"sip">>).
-define(USERNAME, [?SIP, <<"username">>]).
-define(PASSWORD, [?SIP, <<"password">>]).
-define(METHOD, [?SIP, <<"method">>]).
-define(REALM, [?SIP, <<"realm">>]).
-define(IP, [?SIP, <<"ip">>]).
-define(INVITE_FORMAT, [?SIP, <<"invite_format">>]).
-define(ROUTE, [?SIP, <<"route">>]).
-define(CUSTOM_SIP_HEADERS, <<"custom_sip_headers">>).
-define(CUSTOM_SIP_HEADERS_KV_ONLY, [?SIP, ?CUSTOM_SIP_HEADERS]).
-define(CUSTOM_SIP_HEADERS_IN, [?SIP, ?CUSTOM_SIP_HEADERS, <<"in">>]).
-define(CUSTOM_SIP_HEADERS_OUT, [?SIP, ?CUSTOM_SIP_HEADERS, <<"out">>]).

-define(PRESENCE_ID, <<"presence_id">>).
-define(NAME, <<"name">>).
-define(MAC_ADDRESS, <<"mac_address">>).
-define(LANGUAGE, <<"language">>).
-define(DEVICE_TYPE, <<"device_type">>).
-define(KEY_OWNER_ID, <<"owner_id">>).
-define(ENABLED, <<"enabled">>).
-define(PVT_TYPE, <<"device">>).
-define(KEY_TIMEZONE, <<"timezone">>).
-define(KEY_UNSOLICITATED_MWI_UPDATES, <<"mwi_unsolicitated_updates">>).

-spec new() -> doc().
new() ->
    wh_json:from_list([{<<"pvt_type">>, type()}]).

-spec is_device(doc() | wh_json:object()) -> boolean().
is_device(Doc) ->
    wh_doc:type(Doc) =:= ?PVT_TYPE.

-spec sip_username(doc()) -> api_binary().
-spec sip_username(doc(), Default) -> ne_binary() | Default.
sip_username(DeviceJObj) ->
    sip_username(DeviceJObj, 'undefined').

sip_username(DeviceJObj, Default) ->
    wh_json:get_value(?USERNAME, DeviceJObj, Default).

-spec sip_password(doc()) -> api_binary().
-spec sip_password(doc(), Default) -> ne_binary() | Default.
sip_password(DeviceJObj) ->
    sip_password(DeviceJObj, 'undefined').

sip_password(DeviceJObj, Default) ->
    wh_json:get_value(?PASSWORD, DeviceJObj, Default).

-spec sip_method(doc()) -> api_binary().
-spec sip_method(doc(), Default) -> ne_binary() | Default.
sip_method(DeviceJObj) ->
    sip_method(DeviceJObj, 'undefined').

sip_method(DeviceJObj, Default) ->
    wh_json:get_value(?METHOD, DeviceJObj, Default).

-spec sip_realm(doc()) -> api_binary().
-spec sip_realm(doc(), Default) -> ne_binary() | Default.
sip_realm(DeviceJObj) ->
    sip_realm(DeviceJObj, 'undefined').

sip_realm(DeviceJObj, Default) ->
    wh_json:get_value(?REALM, DeviceJObj, Default).

-spec sip_ip(doc()) -> api_binary().
-spec sip_ip(doc(), Default) -> ne_binary() | Default.
sip_ip(DeviceJObj) ->
    sip_ip(DeviceJObj, 'undefined').

sip_ip(DeviceJObj, Default) ->
    wh_json:get_value(?IP, DeviceJObj, Default).

-spec sip_invite_format(doc()) -> api_binary().
-spec sip_invite_format(doc(), Default) -> ne_binary() | Default.
sip_invite_format(DeviceJObj) ->
    sip_invite_format(DeviceJObj, 'undefined').

sip_invite_format(DeviceJObj, Default) ->
    wh_json:get_value(?INVITE_FORMAT, DeviceJObj, Default).

-spec sip_route(doc()) -> api_binary().
-spec sip_route(doc(), Default) -> ne_binary() | Default.
sip_route(DeviceJObj) ->
    sip_route(DeviceJObj, 'undefined').

sip_route(DeviceJObj, Default) ->
    wh_json:get_value(?ROUTE, DeviceJObj, Default).

-spec custom_sip_headers_inbound(doc()) -> api_object().
-spec custom_sip_headers_inbound(doc(), Default) -> wh_json:object() | Default.
custom_sip_headers_inbound(DeviceJObj) ->
    custom_sip_headers_inbound(DeviceJObj, 'undefined').

custom_sip_headers_inbound(DeviceJObj, Default) ->
    LegacyCSH = wh_json:filter(fun filter_custom_sip_headers/1
                               ,wh_json:get_value(?CUSTOM_SIP_HEADERS_KV_ONLY, DeviceJObj, wh_json:new())),
    InCSH = wh_json:get_value(?CUSTOM_SIP_HEADERS_IN, DeviceJObj, wh_json:new()),
    CustomHeaders = wh_json:merge_jobjs(InCSH, LegacyCSH),
    case wh_json:is_empty(CustomHeaders) of
        'false' -> CustomHeaders;
        'true' -> Default
    end.

-spec filter_custom_sip_headers({ne_binary(), any()}) -> boolean().
filter_custom_sip_headers({<<"in">>, _}) -> 'false';
filter_custom_sip_headers({<<"out">>, _}) -> 'false';
filter_custom_sip_headers(_) -> 'true'.

-spec custom_sip_headers_outbound(doc()) -> api_object().
-spec custom_sip_headers_outbound(doc(), Default) -> wh_json:object() | Default.
custom_sip_headers_outbound(DeviceJObj) ->
    custom_sip_headers_outbound(DeviceJObj, 'undefined').

custom_sip_headers_outbound(DeviceJObj, Default) ->
    wh_json:get_value(?CUSTOM_SIP_HEADERS_OUT, DeviceJObj, Default).

-spec sip_settings(doc()) -> api_object().
-spec sip_settings(doc(), Default) -> wh_json:object() | Default.
sip_settings(DeviceJObj) ->
    sip_settings(DeviceJObj, 'undefined').

sip_settings(DeviceJObj, Default) ->
    wh_json:get_value(?SIP, DeviceJObj, Default).

-spec set_sip_username(doc(), ne_binary()) -> doc().
set_sip_username(DeviceJObj, Username) ->
    wh_json:set_value(?USERNAME, Username, DeviceJObj).

-spec set_sip_password(doc(), ne_binary()) -> doc().
set_sip_password(DeviceJObj, Password) ->
    wh_json:set_value(?PASSWORD, Password, DeviceJObj).

-spec set_sip_method(doc(), ne_binary()) -> doc().
set_sip_method(DeviceJObj, Method) ->
    wh_json:set_value(?METHOD, Method, DeviceJObj).

-spec set_sip_realm(doc(), ne_binary()) -> doc().
set_sip_realm(DeviceJObj, Realm) ->
    wh_json:set_value(?REALM, Realm, DeviceJObj).

-spec set_sip_ip(doc(), ne_binary()) -> doc().
set_sip_ip(DeviceJObj, Ip) ->
    wh_json:set_value(?IP, Ip, DeviceJObj).

-spec set_sip_invite_format(doc(), ne_binary()) -> doc().
set_sip_invite_format(DeviceJObj, InviteFormat) ->
    wh_json:set_value(?INVITE_FORMAT, InviteFormat, DeviceJObj).

-spec set_sip_route(doc(), ne_binary()) -> doc().
set_sip_route(DeviceJObj, Route) ->
    wh_json:set_value(?ROUTE, Route, DeviceJObj).

-spec set_custom_sip_headers_inbound(doc(), wh_json:object()) -> doc().
set_custom_sip_headers_inbound(Device, Headers) ->
    wh_json:set_value(?CUSTOM_SIP_HEADERS_IN, Headers, Device).

-spec set_custom_sip_headers_outbound(doc(), wh_json:object()) -> doc().
set_custom_sip_headers_outbound(Device, Headers) ->
    wh_json:set_value(?CUSTOM_SIP_HEADERS_OUT, Headers, Device).

-spec set_sip_settings(doc(), wh_json:object()) -> doc().
set_sip_settings(DeviceJObj, SipJObj) ->
    wh_json:set_value(?SIP, SipJObj, DeviceJObj).

-spec presence_id(doc()) -> api_binary().
-spec presence_id(doc(), Default) -> ne_binary() | Default.
presence_id(DeviceJObj) ->
    presence_id(DeviceJObj, sip_username(DeviceJObj)).
presence_id(DeviceJObj, Default) ->
    wh_json:get_binary_value(?PRESENCE_ID, DeviceJObj, Default).

-spec set_presence_id(doc(), ne_binary()) -> doc().
set_presence_id(DeviceJObj, Id) ->
    wh_json:set_value(
      ?PRESENCE_ID
      ,wh_util:to_binary(Id)
      ,DeviceJObj
     ).

-spec name(doc()) -> api_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(DeviceJObj) ->
    name(DeviceJObj, 'undefined').
name(DeviceJObj, Default) ->
    wh_json:get_value(?NAME, DeviceJObj, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(DeviceJObj, Name) ->
    wh_json:set_value(?NAME, Name, DeviceJObj).

-spec mac_address(doc()) -> api_binary().
-spec mac_address(doc(), Default) -> ne_binary() | Default.
mac_address(DeviceJObj) ->
    mac_address(DeviceJObj, 'undefined').
mac_address(DeviceJObj, Default) ->
    wh_json:get_value(?MAC_ADDRESS, DeviceJObj, Default).

-spec set_mac_address(doc(), ne_binary()) -> doc().
set_mac_address(DeviceJObj, MacAddress) ->
    wh_json:set_value(?MAC_ADDRESS, MacAddress, DeviceJObj).

-spec language(doc()) -> api_binary().
-spec language(doc(), Default) -> ne_binary() | Default.
language(DeviceJObj) ->
    language(DeviceJObj, 'undefined').
language(DeviceJObj, Default) ->
    wh_json:get_ne_value(?LANGUAGE, DeviceJObj, Default).

-spec set_language(doc(), ne_binary()) -> doc().
set_language(DeviceJObj, Language) ->
    wh_json:set_value(?LANGUAGE, Language, DeviceJObj).

-spec device_type(doc()) -> api_binary().
-spec device_type(doc(), Default) -> ne_binary() | Default.
device_type(DeviceJObj) ->
    device_type(DeviceJObj, 'undefined').
device_type(DeviceJObj, Default) ->
    wh_json:get_value(?DEVICE_TYPE, DeviceJObj, Default).

-spec set_device_type(doc(), ne_binary()) -> doc().
set_device_type(DeviceJObj, MacAddress) ->
    wh_json:set_value(?DEVICE_TYPE, MacAddress, DeviceJObj).

-spec type() -> ne_binary().
type() -> ?PVT_TYPE.

-spec owner_id(doc()) -> api_binary().
-spec owner_id(doc(), Default) -> ne_binary() | Default.
owner_id(DeviceJObj) ->
    owner_id(DeviceJObj, 'undefined').
owner_id(DeviceJObj, Default) ->
    wh_json:get_value(?KEY_OWNER_ID, DeviceJObj, Default).

-spec set_owner_id(doc(), ne_binary()) -> doc().
set_owner_id(DeviceJObj, OwnerId) ->
    wh_json:set_value(?KEY_OWNER_ID, OwnerId, DeviceJObj).


-spec enabled(doc()) -> boolean().
-spec enabled(doc(), boolean()) -> boolean().
enabled(DeviceJObj) ->
    enabled(DeviceJObj, 'true').
enabled(DeviceJObj, Default) ->
    wh_json:get_value(?ENABLED, DeviceJObj, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(DeviceJObj, Enabled) ->
    wh_json:set_value(?ENABLED, Enabled, DeviceJObj).

-spec timezone(doc()) -> api_binary().
-spec timezone(doc(), Default) -> ne_binary() | Default.
timezone(Box) ->
    timezone(Box, 'undefined').
timezone(Box, Default) ->
    case wh_json:get_value(?KEY_TIMEZONE, Box) of
        'undefined'   -> owner_timezone(Box, Default);
        <<"inherit">> -> owner_timezone(Box, Default);  %% UI-1808
        TZ -> TZ
    end.

-spec owner_timezone(doc(), Default) -> ne_binary() | Default.
-spec owner_timezone(doc(), Default, kzd_user:doc()) -> ne_binary() | Default.
owner_timezone(Box, Default) ->
    case owner(Box) of
        'undefined'   -> account_timezone(Box, Default);
        <<"inherit">> -> account_timezone(Box, Default);  %% UI-1808
        OwnerJObj -> owner_timezone(Box, Default, OwnerJObj)
    end.

-spec owner(doc()) -> kzd_user:doc() | 'undefined'.
-spec owner(doc(), ne_binary()) -> kzd_user:doc() | 'undefined'.
owner(Box) ->
    case owner_id(Box) of
        'undefined' -> 'undefined';
        OwnerId -> owner(Box, OwnerId)
    end.

owner(Box, OwnerId) ->
    case couch_mgr:open_cache_doc(wh_doc:account_db(Box)
                                  ,OwnerId
                                 )
    of
        {'ok', OwnerJObj} -> OwnerJObj;
        {'error', 'not_found'} -> 'undefined'
    end.

owner_timezone(Box, Default, OwnerJObj) ->
    case kzd_user:timezone(OwnerJObj, 'undefined') of
        'undefined'   -> account_timezone(Box, Default);
        <<"inherit">> -> account_timezone(Box, Default);  %% UI-1808
        TZ -> TZ
    end.

-spec account_timezone(doc(), Default) -> ne_binary() | Default.
account_timezone(Box, Default) ->
    {'ok', AccountJObj} = kz_account:fetch(wh_doc:account_id(Box)),
    kz_account:timezone(AccountJObj, Default).

-spec unsolicitated_mwi_updates(doc()) -> boolean().
unsolicitated_mwi_updates(DeviceJObj) ->
    wh_json:get_value(?KEY_UNSOLICITATED_MWI_UPDATES, DeviceJObj, 'true').

-spec set_unsolicitated_mwi_updates(doc(), boolean()) -> doc().
set_unsolicitated_mwi_updates(DeviceJObj, Enabled) ->
    wh_json:set_value(?KEY_UNSOLICITATED_MWI_UPDATES, Enabled, DeviceJObj).

