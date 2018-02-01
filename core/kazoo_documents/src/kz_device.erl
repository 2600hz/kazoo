%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2018, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_device).

-export([fetch/2]).
-export([sip_username/1, sip_username/2, set_sip_username/2
        ,sip_password/1, sip_password/2, set_sip_password/2
        ,sip_method/1, sip_method/2, set_sip_method/2
        ,sip_realm/1, sip_realm/2, set_sip_realm/2
        ,sip_ip/1, sip_ip/2, set_sip_ip/2
        ,sip_invite_format/1, sip_invite_format/2, set_sip_invite_format/2
        ,sip_route/1, sip_route/2, set_sip_route/2
        ,set_custom_sip_headers/2
        ,custom_sip_headers_inbound/1, custom_sip_headers_inbound/2, set_custom_sip_headers_inbound/2
        ,custom_sip_headers_outbound/1, custom_sip_headers_outbound/2, set_custom_sip_headers_outbound/2
        ,custom_sip_header_inbound/2, custom_sip_header_inbound/3
        ,custom_sip_header_outbound/2, custom_sip_header_outbound/3

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
-export([outbound_flags/1
        ,set_outbound_flags/2
        ,set_outbound_flags/3
        ]).
-export([outbound_static_flags/1
        ,set_outbound_static_flags/2
        ]).
-export([outbound_dynamic_flags/1
        ,set_outbound_dynamic_flags/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
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
-define(CUSTOM_SIP_HEADERS, [?SIP, <<"custom_sip_headers">>]).

-define(PRESENCE_ID, <<"presence_id">>).
-define(NAME, <<"name">>).
-define(MAC_ADDRESS, <<"mac_address">>).
-define(LANGUAGE, <<"language">>).
-define(DEVICE_TYPE, <<"device_type">>).
-define(KEY_OWNER_ID, <<"owner_id">>).
-define(ENABLED, <<"enabled">>).
-define(KEY_TIMEZONE, <<"timezone">>).
-define(KEY_UNSOLICITATED_MWI_UPDATES, <<"mwi_unsolicitated_updates">>).
-define(OUTBOUND_FLAGS, <<"outbound_flags">>).
-define(STATIC_FLAGS, <<"static">>).
-define(DYNAMIC_FLAGS, <<"dynamic">>).

-spec fetch(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> {'ok', doc()} |
                                                                 {'error', any()}.
fetch(Account=?NE_BINARY, DeviceId=?NE_BINARY) ->
    AccountDb = kz_util:format_account_db(Account),
    kz_datamgr:open_cache_doc(AccountDb, DeviceId, [{cache_failures,false}]);
fetch(_, _) ->
    {'error', 'invalid_parameters'}.

-spec new() -> doc().
new() ->
    kz_json:from_list([{<<"pvt_type">>, type()}]).

-spec is_device(doc()) -> boolean().
is_device(Doc) ->
    kz_doc:type(Doc) =:= type().

-spec sip_username(doc()) -> kz_term:api_binary().
sip_username(DeviceJObj) ->
    sip_username(DeviceJObj, 'undefined').

-spec sip_username(doc(), Default) -> kz_term:ne_binary() | Default.
sip_username(DeviceJObj, Default) ->
    kz_json:get_value(?USERNAME, DeviceJObj, Default).

-spec sip_password(doc()) -> kz_term:api_binary().
sip_password(DeviceJObj) ->
    sip_password(DeviceJObj, 'undefined').

-spec sip_password(doc(), Default) -> kz_term:ne_binary() | Default.
sip_password(DeviceJObj, Default) ->
    kz_json:get_value(?PASSWORD, DeviceJObj, Default).

-spec sip_method(doc()) -> kz_term:api_binary().
sip_method(DeviceJObj) ->
    sip_method(DeviceJObj, 'undefined').

-spec sip_method(doc(), Default) -> kz_term:ne_binary() | Default.
sip_method(DeviceJObj, Default) ->
    kz_json:get_value(?METHOD, DeviceJObj, Default).

-spec sip_realm(doc()) -> kz_term:api_binary().
sip_realm(DeviceJObj) ->
    sip_realm(DeviceJObj, 'undefined').

-spec sip_realm(doc(), Default) -> kz_term:ne_binary() | Default.
sip_realm(DeviceJObj, Default) ->
    kz_json:get_value(?REALM, DeviceJObj, Default).

-spec sip_ip(doc()) -> kz_term:api_binary().
sip_ip(DeviceJObj) ->
    sip_ip(DeviceJObj, 'undefined').

-spec sip_ip(doc(), Default) -> kz_term:ne_binary() | Default.
sip_ip(DeviceJObj, Default) ->
    kz_json:get_value(?IP, DeviceJObj, Default).

-spec sip_invite_format(doc()) -> kz_term:api_binary().
sip_invite_format(DeviceJObj) ->
    sip_invite_format(DeviceJObj, 'undefined').

-spec sip_invite_format(doc(), Default) -> kz_term:ne_binary() | Default.
sip_invite_format(DeviceJObj, Default) ->
    kz_json:get_value(?INVITE_FORMAT, DeviceJObj, Default).

-spec sip_route(doc()) -> kz_term:api_binary().
sip_route(DeviceJObj) ->
    sip_route(DeviceJObj, 'undefined').

-spec sip_route(doc(), Default) -> kz_term:ne_binary() | Default.
sip_route(DeviceJObj, Default) ->
    kz_json:get_value(?ROUTE, DeviceJObj, Default).

-spec custom_sip_headers(doc()) -> kz_json:object().
custom_sip_headers(DeviceJObj) ->
    kz_json:get_json_value(?CUSTOM_SIP_HEADERS, DeviceJObj, kz_json:new()).

-spec set_custom_sip_headers(doc(), kz_json:object()) -> doc().
set_custom_sip_headers(DeviceJObj, CSH) ->
    kz_json:set_value(?CUSTOM_SIP_HEADERS, CSH, DeviceJObj).

-spec custom_sip_headers_inbound(doc()) -> kz_term:api_object().
custom_sip_headers_inbound(DeviceJObj) ->
    custom_sip_headers_inbound(DeviceJObj, 'undefined').

-spec custom_sip_headers_inbound(doc(), Default) -> kz_json:object() | Default.
custom_sip_headers_inbound(DeviceJObj, Default) ->
    CSH = custom_sip_headers(DeviceJObj),
    kz_custom_sip_headers:inbound(CSH, Default).

-spec custom_sip_header_inbound(doc(), kz_json:key()) -> kz_json:json_term() | 'undefined'.
custom_sip_header_inbound(DeviceJObj, Name) ->
    custom_sip_header_inbound(DeviceJObj, Name, 'undefined').

-spec custom_sip_header_inbound(doc(), kz_json:key(), Default) -> kz_json:json_term() | Default.
custom_sip_header_inbound(DeviceJObj, Name, Default) ->
    CSH = custom_sip_headers(DeviceJObj),
    kz_custom_sip_headers:inbound_header(CSH, Name, Default).

-spec custom_sip_header_outbound(doc(), kz_json:key()) -> kz_json:json_term() | 'undefined'.
custom_sip_header_outbound(DeviceJObj, Name) ->
    custom_sip_header_outbound(DeviceJObj, Name, 'undefined').

-spec custom_sip_header_outbound(doc(), kz_json:key(), Default) -> kz_json:json_term() | Default.
custom_sip_header_outbound(DeviceJObj, Name, Default) ->
    CSH = custom_sip_headers(DeviceJObj),
    kz_custom_sip_headers:outbound_header(CSH, Name, Default).

-spec custom_sip_headers_outbound(doc()) -> kz_term:api_object().
custom_sip_headers_outbound(DeviceJObj) ->
    custom_sip_headers_outbound(DeviceJObj, 'undefined').

-spec custom_sip_headers_outbound(doc(), Default) -> kz_json:object() | Default.
custom_sip_headers_outbound(DeviceJObj, Default) ->
    CSH = custom_sip_headers(DeviceJObj),
    kz_custom_sip_headers:outbound(CSH, Default).

-spec sip_settings(doc()) -> kz_term:api_object().
sip_settings(DeviceJObj) ->
    sip_settings(DeviceJObj, 'undefined').

-spec sip_settings(doc(), Default) -> kz_json:object() | Default.
sip_settings(DeviceJObj, Default) ->
    kz_json:get_value(?SIP, DeviceJObj, Default).

-spec set_sip_username(doc(), kz_term:ne_binary()) -> doc().
set_sip_username(DeviceJObj, Username) ->
    kz_json:set_value(?USERNAME, Username, DeviceJObj).

-spec set_sip_password(doc(), kz_term:ne_binary()) -> doc().
set_sip_password(DeviceJObj, Password) ->
    kz_json:set_value(?PASSWORD, Password, DeviceJObj).

-spec set_sip_method(doc(), kz_term:ne_binary()) -> doc().
set_sip_method(DeviceJObj, Method) ->
    kz_json:set_value(?METHOD, Method, DeviceJObj).

-spec set_sip_realm(doc(), kz_term:ne_binary()) -> doc().
set_sip_realm(DeviceJObj, Realm) ->
    kz_json:set_value(?REALM, Realm, DeviceJObj).

-spec set_sip_ip(doc(), kz_term:ne_binary()) -> doc().
set_sip_ip(DeviceJObj, Ip) ->
    kz_json:set_value(?IP, Ip, DeviceJObj).

-spec set_sip_invite_format(doc(), kz_term:ne_binary()) -> doc().
set_sip_invite_format(DeviceJObj, InviteFormat) ->
    kz_json:set_value(?INVITE_FORMAT, InviteFormat, DeviceJObj).

-spec set_sip_route(doc(), kz_term:ne_binary()) -> doc().
set_sip_route(DeviceJObj, Route) ->
    kz_json:set_value(?ROUTE, Route, DeviceJObj).

-spec set_custom_sip_headers_inbound(doc(), kz_json:object()) -> doc().
set_custom_sip_headers_inbound(Device, Headers) ->
    CSH = custom_sip_headers(Device),
    InboundCSH = kz_custom_sip_headers:set_inbound(CSH, Headers),
    set_custom_sip_headers(Device, InboundCSH).

-spec set_custom_sip_headers_outbound(doc(), kz_json:object()) -> doc().
set_custom_sip_headers_outbound(Device, Headers) ->
    CSH = custom_sip_headers(Device),
    OutboundCSH = kz_custom_sip_headers:set_outbound(CSH, Headers),
    set_custom_sip_headers(Device, OutboundCSH).

-spec set_sip_settings(doc(), kz_json:object()) -> doc().
set_sip_settings(DeviceJObj, SipJObj) ->
    kz_json:set_value(?SIP, SipJObj, DeviceJObj).

-spec presence_id(doc()) -> kz_term:api_binary().
presence_id(DeviceJObj) ->
    presence_id(DeviceJObj, sip_username(DeviceJObj)).

-spec presence_id(doc(), Default) -> kz_term:ne_binary() | Default.
presence_id(DeviceJObj, Default) ->
    kz_json:get_binary_value(?PRESENCE_ID, DeviceJObj, Default).

-spec set_presence_id(doc(), kz_term:ne_binary()) -> doc().
set_presence_id(DeviceJObj, Id) ->
    kz_json:set_value(?PRESENCE_ID
                     ,kz_term:to_binary(Id)
                     ,DeviceJObj
                     ).

-spec name(doc()) -> kz_term:api_binary().
name(DeviceJObj) ->
    name(DeviceJObj, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(DeviceJObj, Default) ->
    kz_json:get_value(?NAME, DeviceJObj, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(DeviceJObj, Name) ->
    kz_json:set_value(?NAME, Name, DeviceJObj).

-spec mac_address(doc()) -> kz_term:api_binary().
mac_address(DeviceJObj) ->
    mac_address(DeviceJObj, 'undefined').

-spec mac_address(doc(), Default) -> kz_term:ne_binary() | Default.
mac_address(DeviceJObj, Default) ->
    kz_json:get_value(?MAC_ADDRESS, DeviceJObj, Default).

-spec set_mac_address(doc(), kz_term:ne_binary()) -> doc().
set_mac_address(DeviceJObj, MacAddress) ->
    kz_json:set_value(?MAC_ADDRESS, MacAddress, DeviceJObj).

-spec language(doc()) -> kz_term:api_binary().
language(DeviceJObj) ->
    language(DeviceJObj, 'undefined').

-spec language(doc(), Default) -> kz_term:ne_binary() | Default.
language(DeviceJObj, Default) ->
    kz_json:get_ne_value(?LANGUAGE, DeviceJObj, Default).

-spec set_language(doc(), kz_term:ne_binary()) -> doc().
set_language(DeviceJObj, Language) ->
    kz_json:set_value(?LANGUAGE, Language, DeviceJObj).

-spec device_type(doc()) -> kz_term:api_binary().
device_type(DeviceJObj) ->
    device_type(DeviceJObj, 'undefined').

-spec device_type(doc(), Default) -> kz_term:ne_binary() | Default.
device_type(DeviceJObj, Default) ->
    kz_json:get_value(?DEVICE_TYPE, DeviceJObj, Default).

-spec set_device_type(doc(), kz_term:ne_binary()) -> doc().
set_device_type(DeviceJObj, MacAddress) ->
    kz_json:set_value(?DEVICE_TYPE, MacAddress, DeviceJObj).

-spec type() -> kz_term:ne_binary().
type() -> <<"device">>.

-spec owner_id(doc()) -> kz_term:api_binary().
owner_id(DeviceJObj) ->
    owner_id(DeviceJObj, 'undefined').

-spec owner_id(doc(), Default) -> kz_term:ne_binary() | Default.
owner_id(DeviceJObj, Default) ->
    kz_json:get_value(?KEY_OWNER_ID, DeviceJObj, Default).

-spec set_owner_id(doc(), kz_term:ne_binary()) -> doc().
set_owner_id(DeviceJObj, OwnerId) ->
    kz_json:set_value(?KEY_OWNER_ID, OwnerId, DeviceJObj).


-spec enabled(doc()) -> boolean().
enabled(DeviceJObj) ->
    enabled(DeviceJObj, 'true').

-spec enabled(doc(), boolean()) -> boolean().
enabled(DeviceJObj, Default) ->
    kz_json:get_value(?ENABLED, DeviceJObj, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(DeviceJObj, Enabled) ->
    kz_json:set_value(?ENABLED, Enabled, DeviceJObj).

-spec timezone(doc()) -> kz_term:ne_binary().
timezone(Box) ->
    timezone(Box, 'undefined').

-spec timezone(doc(), Default) -> kz_term:ne_binary() | Default.
timezone(Box, Default) ->
    case kz_json:get_value(?KEY_TIMEZONE, Box) of
        'undefined'   -> owner_timezone(Box, Default);
        <<"inherit">> -> owner_timezone(Box, Default);  %% UI-1808
        TZ -> TZ
    end.

-spec owner_timezone(doc(), Default) -> kz_term:ne_binary() | Default.
owner_timezone(Box, Default) ->
    case kzd_user:fetch(kz_doc:account_db(Box), owner_id(Box)) of
        {'ok', OwnerJObj} -> kzd_user:timezone(OwnerJObj, Default);
        {'error', _} -> kz_account:timezone(kz_doc:account_id(Box), Default)
    end.

-spec unsolicitated_mwi_updates(doc()) -> boolean().
unsolicitated_mwi_updates(DeviceJObj) ->
    kz_json:get_value(?KEY_UNSOLICITATED_MWI_UPDATES, DeviceJObj, 'true').

-spec set_unsolicitated_mwi_updates(doc(), boolean()) -> doc().
set_unsolicitated_mwi_updates(DeviceJObj, Enabled) ->
    kz_json:set_value(?KEY_UNSOLICITATED_MWI_UPDATES, Enabled, DeviceJObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec outbound_flags(kz_json:object()) -> kz_json:object().
outbound_flags(JObj) ->
    OutboundFlags = kz_json:get_ne_value(?OUTBOUND_FLAGS, JObj, kz_json:new()),
    %% Backward compatibilty with an array of static flags
    case kz_json:is_json_object(OutboundFlags) of
        'false' -> kz_json:from_list([{?STATIC_FLAGS, OutboundFlags}]);
        'true' -> OutboundFlags
    end.

-spec set_outbound_flags(kz_json:object(), kz_json:object() | kz_term:ne_binaries()) -> kz_json:object().
set_outbound_flags(JObj, Flags) when is_list(Flags) ->
    OutboundFlags = outbound_flags(JObj),
    UpdatedFlags = kz_json:set_value(?STATIC_FLAGS, Flags, OutboundFlags),
    kz_json:set_value(?OUTBOUND_FLAGS, UpdatedFlags, JObj);
set_outbound_flags(JObj, Flags) ->
    kz_json:set_value(?OUTBOUND_FLAGS, Flags, JObj).

-spec set_outbound_flags(kz_json:object(), kz_term:ne_binaries()|undefined, kz_term:ne_binaries()|undefined) -> kz_json:object().
set_outbound_flags(JObj, 'undefined', DynamicFlags) ->
    set_outbound_flags(JObj, [], DynamicFlags);
set_outbound_flags(JObj, StaticFlags, 'undefined') ->
    set_outbound_flags(JObj, StaticFlags, []);
set_outbound_flags(JObj, StaticFlags, DynamicFlags) when is_list(StaticFlags), is_list(DynamicFlags) ->
    Flags = kz_json:from_list([{?DYNAMIC_FLAGS, DynamicFlags}
                              ,{?STATIC_FLAGS, StaticFlags}
                              ]),
    kz_json:set_value(?OUTBOUND_FLAGS, Flags, JObj).

-spec outbound_static_flags(kz_json:object()) -> kz_term:ne_binaries().
outbound_static_flags(JObj) ->
    OutboundFlags = outbound_flags(JObj),
    kz_json:get_list_value(?STATIC_FLAGS, OutboundFlags, []).

-spec set_outbound_static_flags(kz_json:object(), kz_term:ne_binaries()) -> kz_json:object().
set_outbound_static_flags(JObj, Flags) when is_list(Flags) ->
    OutboundFlags = kz_json:get_ne_value(?OUTBOUND_FLAGS, JObj, []),
    %% Backward compatibilty with an array of static flags
    case kz_json:is_json_object(OutboundFlags) of
        'true' -> kz_json:set_value([?OUTBOUND_FLAGS, ?STATIC_FLAGS], Flags, JObj);
        'false' ->
            Updates = kz_json:from_list([{?STATIC_FLAGS, Flags}]),
            kz_json:set_value(?OUTBOUND_FLAGS, Updates, JObj)
    end.

-spec outbound_dynamic_flags(kz_json:object()) -> kz_term:ne_binaries().
outbound_dynamic_flags(JObj) ->
    OutboundFlags = outbound_flags(JObj),
    kz_json:get_list_value(?DYNAMIC_FLAGS, OutboundFlags, []).

-spec set_outbound_dynamic_flags(kz_json:object(), kz_term:ne_binaries()) -> kz_json:object().
set_outbound_dynamic_flags(JObj, Flags) when is_list(Flags) ->
    OutboundFlags = kz_json:get_ne_value(?OUTBOUND_FLAGS, JObj, []),
    %% Backward compatibilty with an array of static flags
    case kz_json:is_json_object(OutboundFlags) of
        'true' -> kz_json:set_value([?OUTBOUND_FLAGS, ?DYNAMIC_FLAGS], Flags, JObj);
        'false' ->
            Updates = kz_json:from_list([{?STATIC_FLAGS, OutboundFlags}
                                        ,{?DYNAMIC_FLAGS, Flags}
                                        ]
                                       ),
            kz_json:set_value(?OUTBOUND_FLAGS, Updates, JObj)
    end.
