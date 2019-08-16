%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_devices).

-export([new/0]).
-export([call_forward/1, call_forward/2, set_call_forward/2]).
-export([call_forward_direct_calls_only/1, call_forward_direct_calls_only/2, set_call_forward_direct_calls_only/2]).
-export([call_forward_enabled/1, call_forward_enabled/2, set_call_forward_enabled/2]).
-export([call_forward_failover/1, call_forward_failover/2, set_call_forward_failover/2]).
-export([call_forward_ignore_early_media/1, call_forward_ignore_early_media/2, set_call_forward_ignore_early_media/2]).
-export([call_forward_keep_caller_id/1, call_forward_keep_caller_id/2, set_call_forward_keep_caller_id/2]).
-export([call_forward_number/1, call_forward_number/2, set_call_forward_number/2]).
-export([call_forward_require_keypress/1, call_forward_require_keypress/2, set_call_forward_require_keypress/2]).
-export([call_forward_substitute/1, call_forward_substitute/2, set_call_forward_substitute/2]).
-export([call_recording/1, call_recording/2, set_call_recording/2]).
-export([call_restriction/1, call_restriction/2, set_call_restriction/2]).
-export([call_waiting/1, call_waiting/2, set_call_waiting/2]).
-export([caller_id/1, caller_id/2, set_caller_id/2]).
-export([contact_list/1, contact_list/2, set_contact_list/2]).
-export([contact_list_exclude/1, contact_list_exclude/2, set_contact_list_exclude/2]).
-export([device_type/1, device_type/2, set_device_type/2]).
-export([dial_plan/1, dial_plan/2, set_dial_plan/2]).
-export([do_not_disturb/1, do_not_disturb/2, set_do_not_disturb/2]).
-export([do_not_disturb_enabled/1, do_not_disturb_enabled/2, set_do_not_disturb_enabled/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([exclude_from_queues/1, exclude_from_queues/2, set_exclude_from_queues/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([formatters/1, formatters/2, set_formatters/2]).
-export([hotdesk/1, hotdesk/2, set_hotdesk/2]).
-export([language/1, language/2, set_language/2]).
-export([mac_address/1, mac_address/2, set_mac_address/2]).
-export([media/1, media/2, set_media/2]).
-export([metaflows/1, metaflows/2, set_metaflows/2]).
-export([music_on_hold/1, music_on_hold/2, set_music_on_hold/2]).
-export([music_on_hold_media_id/1, music_on_hold_media_id/2, set_music_on_hold_media_id/2]).
-export([mwi_unsolicited_updates/1, mwi_unsolicited_updates/2, set_mwi_unsolicited_updates/2]).
-export([name/1, name/2, set_name/2]).
-export([outbound_flags/1, outbound_flags/2, set_outbound_flags/2]).
-export([owner_id/1, owner_id/2, set_owner_id/2]).
-export([presence_id/1, presence_id/2, set_presence_id/2]).
-export([provision/1, provision/2, set_provision/2]).
-export([provision_combo_keys/1, provision_combo_keys/2, set_provision_combo_keys/2]).
-export([provision_combo_key/2, provision_combo_key/3, set_provision_combo_key/3]).
-export([provision_endpoint_brand/1, provision_endpoint_brand/2, set_provision_endpoint_brand/2]).
-export([provision_endpoint_family/1, provision_endpoint_family/2, set_provision_endpoint_family/2]).
-export([provision_endpoint_model/1, provision_endpoint_model/2, set_provision_endpoint_model/2]).
-export([provision_feature_keys/1, provision_feature_keys/2, set_provision_feature_keys/2]).
-export([provision_feature_key/2, provision_feature_key/3, set_provision_feature_key/3]).
-export([provision_id/1, provision_id/2, set_provision_id/2]).
-export([register_overwrite_notify/1, register_overwrite_notify/2, set_register_overwrite_notify/2]).
-export([ringtones/1, ringtones/2, set_ringtones/2]).
-export([ringtones_external/1, ringtones_external/2, set_ringtones_external/2]).
-export([ringtones_internal/1, ringtones_internal/2, set_ringtones_internal/2]).
-export([sip/1, sip/2, set_sip/2]).
-export([sip_custom_sip_headers/1, sip_custom_sip_headers/2, set_sip_custom_sip_headers/2]).
-export([sip_expire_seconds/1, sip_expire_seconds/2, set_sip_expire_seconds/2]).
-export([sip_ignore_completed_elsewhere/1, sip_ignore_completed_elsewhere/2, set_sip_ignore_completed_elsewhere/2]).
-export([sip_invite_format/1, sip_invite_format/2, set_sip_invite_format/2]).
-export([sip_ip/1, sip_ip/2, set_sip_ip/2]).
-export([sip_method/1, sip_method/2, set_sip_method/2]).
-export([sip_number/1, sip_number/2, set_sip_number/2]).
-export([sip_password/1, sip_password/2, set_sip_password/2]).
-export([sip_realm/1, sip_realm/2, set_sip_realm/2]).
-export([sip_route/1, sip_route/2, set_sip_route/2]).
-export([sip_static_route/1, sip_static_route/2, set_sip_static_route/2]).
-export([sip_username/1, sip_username/2, set_sip_username/2]).
-export([suppress_unregister_notifications/1, suppress_unregister_notifications/2, set_suppress_unregister_notifications/2]).
-export([timezone/1, timezone/2, set_timezone/2]).
-export([is_hotdesked/1, hotdesk_ids/1, hotdesk_ids/2]).

-export([fetch/2
        ,type/0
        ,is_device/1

        ,custom_sip_headers_inbound/1, custom_sip_headers_inbound/2, set_custom_sip_headers_inbound/2
        ,custom_sip_headers_outbound/1, custom_sip_headers_outbound/2, set_custom_sip_headers_outbound/2
        ,custom_sip_header_inbound/2, custom_sip_header_inbound/3
        ,custom_sip_header_outbound/2, custom_sip_header_outbound/3

        ,set_outbound_flags/3
        ,outbound_static_flags/1, set_outbound_static_flags/2
        ,outbound_dynamic_flags/1, set_outbound_dynamic_flags/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"devices">>).
-define(STATIC_FLAGS, <<"static">>).
-define(DYNAMIC_FLAGS, <<"dynamic">>).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json_schema:default_object(?SCHEMA), type()).

-spec call_forward(doc()) -> kz_term:api_object().
call_forward(Doc) ->
    call_forward(Doc, 'undefined').

-spec call_forward(doc(), Default) -> kz_json:object() | Default.
call_forward(Doc, Default) ->
    kz_json:get_json_value([<<"call_forward">>], Doc, Default).

-spec set_call_forward(doc(), kz_json:object()) -> doc().
set_call_forward(Doc, CallForward) ->
    kz_json:set_value([<<"call_forward">>], CallForward, Doc).

-spec call_forward_direct_calls_only(doc()) -> boolean().
call_forward_direct_calls_only(Doc) ->
    call_forward_direct_calls_only(Doc, false).

-spec call_forward_direct_calls_only(doc(), Default) -> boolean() | Default.
call_forward_direct_calls_only(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"direct_calls_only">>], Doc, Default).

-spec set_call_forward_direct_calls_only(doc(), boolean()) -> doc().
set_call_forward_direct_calls_only(Doc, CallForwardDirectCallsOnly) ->
    kz_json:set_value([<<"call_forward">>, <<"direct_calls_only">>], CallForwardDirectCallsOnly, Doc).

-spec call_forward_enabled(doc()) -> boolean().
call_forward_enabled(Doc) ->
    call_forward_enabled(Doc, false).

-spec call_forward_enabled(doc(), Default) -> boolean() | Default.
call_forward_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"enabled">>], Doc, Default).

-spec set_call_forward_enabled(doc(), boolean()) -> doc().
set_call_forward_enabled(Doc, CallForwardEnabled) ->
    kz_json:set_value([<<"call_forward">>, <<"enabled">>], CallForwardEnabled, Doc).

-spec call_forward_failover(doc()) -> boolean().
call_forward_failover(Doc) ->
    call_forward_failover(Doc, false).

-spec call_forward_failover(doc(), Default) -> boolean() | Default.
call_forward_failover(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"failover">>], Doc, Default).

-spec set_call_forward_failover(doc(), boolean()) -> doc().
set_call_forward_failover(Doc, CallForwardFailover) ->
    kz_json:set_value([<<"call_forward">>, <<"failover">>], CallForwardFailover, Doc).

-spec call_forward_ignore_early_media(doc()) -> boolean().
call_forward_ignore_early_media(Doc) ->
    call_forward_ignore_early_media(Doc, true).

-spec call_forward_ignore_early_media(doc(), Default) -> boolean() | Default.
call_forward_ignore_early_media(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"ignore_early_media">>], Doc, Default).

-spec set_call_forward_ignore_early_media(doc(), boolean()) -> doc().
set_call_forward_ignore_early_media(Doc, CallForwardIgnoreEarlyMedia) ->
    kz_json:set_value([<<"call_forward">>, <<"ignore_early_media">>], CallForwardIgnoreEarlyMedia, Doc).

-spec call_forward_keep_caller_id(doc()) -> boolean().
call_forward_keep_caller_id(Doc) ->
    call_forward_keep_caller_id(Doc, true).

-spec call_forward_keep_caller_id(doc(), Default) -> boolean() | Default.
call_forward_keep_caller_id(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"keep_caller_id">>], Doc, Default).

-spec set_call_forward_keep_caller_id(doc(), boolean()) -> doc().
set_call_forward_keep_caller_id(Doc, CallForwardKeepCallerId) ->
    kz_json:set_value([<<"call_forward">>, <<"keep_caller_id">>], CallForwardKeepCallerId, Doc).

-spec call_forward_number(doc()) -> kz_term:api_binary().
call_forward_number(Doc) ->
    call_forward_number(Doc, 'undefined').

-spec call_forward_number(doc(), Default) -> binary() | Default.
call_forward_number(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"call_forward">>, <<"number">>], Doc, Default).

-spec set_call_forward_number(doc(), binary()) -> doc().
set_call_forward_number(Doc, CallForwardNumber) ->
    kz_json:set_value([<<"call_forward">>, <<"number">>], CallForwardNumber, Doc).

-spec call_forward_require_keypress(doc()) -> boolean().
call_forward_require_keypress(Doc) ->
    call_forward_require_keypress(Doc, true).

-spec call_forward_require_keypress(doc(), Default) -> boolean() | Default.
call_forward_require_keypress(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"require_keypress">>], Doc, Default).

-spec set_call_forward_require_keypress(doc(), boolean()) -> doc().
set_call_forward_require_keypress(Doc, CallForwardRequireKeypress) ->
    kz_json:set_value([<<"call_forward">>, <<"require_keypress">>], CallForwardRequireKeypress, Doc).

-spec call_forward_substitute(doc()) -> boolean().
call_forward_substitute(Doc) ->
    call_forward_substitute(Doc, true).

-spec call_forward_substitute(doc(), Default) -> boolean() | Default.
call_forward_substitute(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"substitute">>], Doc, Default).

-spec set_call_forward_substitute(doc(), boolean()) -> doc().
set_call_forward_substitute(Doc, CallForwardSubstitute) ->
    kz_json:set_value([<<"call_forward">>, <<"substitute">>], CallForwardSubstitute, Doc).

-spec call_recording(doc()) -> kz_term:api_object().
call_recording(Doc) ->
    call_recording(Doc, 'undefined').

-spec call_recording(doc(), Default) -> kz_json:object() | Default.
call_recording(Doc, Default) ->
    kz_json:get_json_value([<<"call_recording">>], Doc, Default).

-spec set_call_recording(doc(), kz_json:object()) -> doc().
set_call_recording(Doc, CallRecording) ->
    kz_json:set_value([<<"call_recording">>], CallRecording, Doc).

-spec call_restriction(doc()) -> kz_json:object().
call_restriction(Doc) ->
    call_restriction(Doc, kz_json:new()).

-spec call_restriction(doc(), Default) -> kz_json:object() | Default.
call_restriction(Doc, Default) ->
    kz_json:get_json_value([<<"call_restriction">>], Doc, Default).

-spec set_call_restriction(doc(), kz_json:object()) -> doc().
set_call_restriction(Doc, CallRestriction) ->
    kz_json:set_value([<<"call_restriction">>], CallRestriction, Doc).

-spec call_waiting(doc()) -> kz_term:api_object().
call_waiting(Doc) ->
    call_waiting(Doc, 'undefined').

-spec call_waiting(doc(), Default) -> kz_json:object() | Default.
call_waiting(Doc, Default) ->
    kz_json:get_json_value([<<"call_waiting">>], Doc, Default).

-spec set_call_waiting(doc(), kz_json:object()) -> doc().
set_call_waiting(Doc, CallWaiting) ->
    kz_json:set_value([<<"call_waiting">>], CallWaiting, Doc).

-spec caller_id(doc()) -> kz_term:api_object().
caller_id(Doc) ->
    caller_id(Doc, 'undefined').

-spec caller_id(doc(), Default) -> kz_json:object() | Default.
caller_id(Doc, Default) ->
    kz_json:get_json_value([<<"caller_id">>], Doc, Default).

-spec set_caller_id(doc(), kz_json:object()) -> doc().
set_caller_id(Doc, CallerId) ->
    kz_json:set_value([<<"caller_id">>], CallerId, Doc).

-spec contact_list(doc()) -> kz_json:object().
contact_list(Doc) ->
    contact_list(Doc, kz_json:new()).

-spec contact_list(doc(), Default) -> kz_json:object() | Default.
contact_list(Doc, Default) ->
    kz_json:get_json_value([<<"contact_list">>], Doc, Default).

-spec set_contact_list(doc(), kz_json:object()) -> doc().
set_contact_list(Doc, ContactList) ->
    kz_json:set_value([<<"contact_list">>], ContactList, Doc).

-spec contact_list_exclude(doc()) -> kz_term:api_boolean().
contact_list_exclude(Doc) ->
    contact_list_exclude(Doc, 'undefined').

-spec contact_list_exclude(doc(), Default) -> boolean() | Default.
contact_list_exclude(Doc, Default) ->
    kz_json:get_boolean_value([<<"contact_list">>, <<"exclude">>], Doc, Default).

-spec set_contact_list_exclude(doc(), boolean()) -> doc().
set_contact_list_exclude(Doc, ContactListExclude) ->
    kz_json:set_value([<<"contact_list">>, <<"exclude">>], ContactListExclude, Doc).

-spec device_type(doc()) -> kz_term:api_binary().
device_type(Doc) ->
    device_type(Doc, 'undefined').

-spec device_type(doc(), Default) -> binary() | Default.
device_type(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"device_type">>], Doc, Default).

-spec set_device_type(doc(), binary()) -> doc().
set_device_type(Doc, DeviceType) ->
    kz_json:set_value([<<"device_type">>], DeviceType, Doc).

-spec dial_plan(doc()) -> kz_term:api_object().
dial_plan(Doc) ->
    dial_plan(Doc, 'undefined').

-spec dial_plan(doc(), Default) -> kz_json:object() | Default.
dial_plan(Doc, Default) ->
    kz_json:get_json_value([<<"dial_plan">>], Doc, Default).

-spec set_dial_plan(doc(), kz_json:object()) -> doc().
set_dial_plan(Doc, DialPlan) ->
    kz_json:set_value([<<"dial_plan">>], DialPlan, Doc).

-spec do_not_disturb(doc()) -> kz_term:api_object().
do_not_disturb(Doc) ->
    do_not_disturb(Doc, 'undefined').

-spec do_not_disturb(doc(), Default) -> kz_json:object() | Default.
do_not_disturb(Doc, Default) ->
    kz_json:get_json_value([<<"do_not_disturb">>], Doc, Default).

-spec set_do_not_disturb(doc(), kz_json:object()) -> doc().
set_do_not_disturb(Doc, DoNotDisturb) ->
    kz_json:set_value([<<"do_not_disturb">>], DoNotDisturb, Doc).

-spec do_not_disturb_enabled(doc()) -> kz_term:api_boolean().
do_not_disturb_enabled(Doc) ->
    do_not_disturb_enabled(Doc, 'undefined').

-spec do_not_disturb_enabled(doc(), Default) -> boolean() | Default.
do_not_disturb_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"do_not_disturb">>, <<"enabled">>], Doc, Default).

-spec set_do_not_disturb_enabled(doc(), boolean()) -> doc().
set_do_not_disturb_enabled(Doc, DoNotDisturbEnabled) ->
    kz_json:set_value([<<"do_not_disturb">>, <<"enabled">>], DoNotDisturbEnabled, Doc).

-spec enabled(doc()) -> boolean().
enabled(Doc) ->
    enabled(Doc, true).

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec exclude_from_queues(doc()) -> boolean().
exclude_from_queues(Doc) ->
    exclude_from_queues(Doc, false).

-spec exclude_from_queues(doc(), Default) -> boolean() | Default.
exclude_from_queues(Doc, Default) ->
    kz_json:get_boolean_value([<<"exclude_from_queues">>], Doc, Default).

-spec set_exclude_from_queues(doc(), boolean()) -> doc().
set_exclude_from_queues(Doc, ExcludeFromQueues) ->
    kz_json:set_value([<<"exclude_from_queues">>], ExcludeFromQueues, Doc).

-spec flags(doc()) -> kz_term:api_ne_binaries().
flags(Doc) ->
    flags(Doc, 'undefined').

-spec flags(doc(), Default) -> kz_term:ne_binaries() | Default.
flags(Doc, Default) ->
    kz_json:get_list_value([<<"flags">>], Doc, Default).

-spec set_flags(doc(), kz_term:ne_binaries()) -> doc().
set_flags(Doc, Flags) ->
    kz_json:set_value([<<"flags">>], Flags, Doc).

-spec formatters(doc()) -> kz_term:api_object().
formatters(Doc) ->
    formatters(Doc, 'undefined').

-spec formatters(doc(), Default) -> kz_json:object() | Default.
formatters(Doc, Default) ->
    kz_json:get_json_value([<<"formatters">>], Doc, Default).

-spec set_formatters(doc(), kz_json:object()) -> doc().
set_formatters(Doc, Formatters) ->
    kz_json:set_value([<<"formatters">>], Formatters, Doc).

-spec hotdesk(doc()) -> kz_term:api_object().
hotdesk(Doc) ->
    hotdesk(Doc, 'undefined').

-spec hotdesk(doc(), Default) -> kz_json:object() | Default.
hotdesk(Doc, Default) ->
    kz_json:get_json_value([<<"hotdesk">>], Doc, Default).

-spec set_hotdesk(doc(), kz_json:object()) -> doc().
set_hotdesk(Doc, Hotdesk) ->
    kz_json:set_value([<<"hotdesk">>], Hotdesk, Doc).

-spec language(doc()) -> kz_term:api_binary().
language(Doc) ->
    language(Doc, 'undefined').

-spec language(doc(), Default) -> binary() | Default.
language(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"language">>], Doc, Default).

-spec set_language(doc(), binary()) -> doc().
set_language(Doc, Language) ->
    kz_json:set_value([<<"language">>], Language, Doc).

-spec mac_address(doc()) -> kz_term:api_binary().
mac_address(Doc) ->
    mac_address(Doc, 'undefined').

-spec mac_address(doc(), Default) -> binary() | Default.
mac_address(Doc, Default) ->
    provisioner_util:cleanse_mac_address(
      kz_json:get_ne_binary_value([<<"mac_address">>], Doc, Default)
     ).

-spec set_mac_address(doc(), binary()) -> doc().
set_mac_address(Doc, MacAddress) ->
    CleansedMacAddress =
        provisioner_util:cleanse_mac_address(MacAddress),
    kz_json:set_value([<<"mac_address">>], CleansedMacAddress, Doc).

-spec media(doc()) -> kz_term:api_object().
media(Doc) ->
    media(Doc, 'undefined').

-spec media(doc(), Default) -> kz_json:object() | Default.
media(Doc, Default) ->
    kz_json:get_json_value([<<"media">>], Doc, Default).

-spec set_media(doc(), kz_json:object()) -> doc().
set_media(Doc, Media) ->
    kz_json:set_value([<<"media">>], Media, Doc).

-spec metaflows(doc()) -> kz_term:api_object().
metaflows(Doc) ->
    metaflows(Doc, 'undefined').

-spec metaflows(doc(), Default) -> kz_json:object() | Default.
metaflows(Doc, Default) ->
    kz_json:get_json_value([<<"metaflows">>], Doc, Default).

-spec set_metaflows(doc(), kz_json:object()) -> doc().
set_metaflows(Doc, Metaflows) ->
    kz_json:set_value([<<"metaflows">>], Metaflows, Doc).

-spec music_on_hold(doc()) -> kz_json:object().
music_on_hold(Doc) ->
    music_on_hold(Doc, kz_json:new()).

-spec music_on_hold(doc(), Default) -> kz_json:object() | Default.
music_on_hold(Doc, Default) ->
    kz_json:get_json_value([<<"music_on_hold">>], Doc, Default).

-spec set_music_on_hold(doc(), kz_json:object()) -> doc().
set_music_on_hold(Doc, MusicOnHold) ->
    kz_json:set_value([<<"music_on_hold">>], MusicOnHold, Doc).

-spec music_on_hold_media_id(doc()) -> kz_term:api_binary().
music_on_hold_media_id(Doc) ->
    music_on_hold_media_id(Doc, 'undefined').

-spec music_on_hold_media_id(doc(), Default) -> binary() | Default.
music_on_hold_media_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"music_on_hold">>, <<"media_id">>], Doc, Default).

-spec set_music_on_hold_media_id(doc(), binary()) -> doc().
set_music_on_hold_media_id(Doc, MusicOnHoldMediaId) ->
    kz_json:set_value([<<"music_on_hold">>, <<"media_id">>], MusicOnHoldMediaId, Doc).

-spec mwi_unsolicited_updates(doc()) -> boolean().
mwi_unsolicited_updates(Doc) ->
    mwi_unsolicited_updates(Doc, true).

-spec mwi_unsolicited_updates(doc(), Default) -> boolean() | Default.
mwi_unsolicited_updates(Doc, Default) ->
    case kz_json:get_first_defined([<<"mwi_unsolicited_updates">>, <<"mwi_unsolicitated_updates">>], Doc) of
        'undefined' -> Default;
        Bool -> kz_term:safe_cast(Bool, Default, fun kz_term:to_boolean/1)
    end.

-spec set_mwi_unsolicited_updates(doc(), boolean()) -> doc().
set_mwi_unsolicited_updates(Doc, MwiUnsolicitedUpdates) ->
    kz_json:set_values([{<<"mwi_unsolicited_updates">>, MwiUnsolicitedUpdates}
                       ,{<<"mwi_unsoliciated_updates">>, 'null'}
                       ]
                      ,Doc
                      ).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec outbound_flags(doc()) -> kz_json:object().
outbound_flags(Doc) ->
    outbound_flags(Doc, kz_json:new()).

-spec outbound_flags(doc(), Default) -> kz_json:object() | Default.
outbound_flags(Doc, Default) ->
    OutboundFlags = kz_json:get_ne_value([<<"outbound_flags">>], Doc, Default),
    %% Backward compatibility with an array of static flags
    case kz_json:is_json_object(OutboundFlags) of
        'false' -> kz_json:from_list([{?STATIC_FLAGS, OutboundFlags}]);
        'true' -> OutboundFlags
    end.

-spec set_outbound_flags(doc(), kz_json:object() | kz_term:ne_binaries()) -> doc().
set_outbound_flags(JObj, Flags) when is_list(Flags) ->
    OutboundFlags = outbound_flags(JObj),
    UpdatedFlags = kz_json:set_value(?STATIC_FLAGS, Flags, OutboundFlags),
    kz_json:set_value([<<"outbound_flags">>], UpdatedFlags, JObj);
set_outbound_flags(JObj, Flags) ->
    kz_json:set_value([<<"outbound_flags">>], Flags, JObj).

-spec owner_id(doc()) -> kz_term:api_ne_binary().
owner_id(Doc) ->
    owner_id(Doc, 'undefined').

-spec owner_id(doc(), Default) -> kz_term:ne_binary() | Default.
owner_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"owner_id">>], Doc, Default).

-spec set_owner_id(doc(), kz_term:ne_binary()) -> doc().
set_owner_id(Doc, OwnerId) ->
    kz_json:set_value([<<"owner_id">>], OwnerId, Doc).

-spec is_hotdesked(doc()) -> boolean().
is_hotdesked(Doc) ->
    not kz_json:is_empty(kz_json:get_value([<<"hotdesk">>, <<"users">>], Doc, kz_json:new())).

-spec hotdesk_ids(doc()) -> kz_term:api_ne_binaries().
hotdesk_ids(Doc) ->
    hotdesk_ids(Doc, 'undefined').

-spec hotdesk_ids(doc(), Default) -> kz_term:ne_binaries() | Default.
hotdesk_ids(Doc, Default) ->
    case kz_json:get_json_value([<<"hotdesk">>, <<"users">>], Doc) of
        'undefined' ->
            Default;
        Value ->
            kz_json:get_keys(Value)
    end.

-spec presence_id(doc()) -> kz_term:api_binary().
presence_id(Doc) ->
    presence_id(Doc, 'undefined').

-spec presence_id(doc(), Default) -> binary() | Default.
presence_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"presence_id">>], Doc, Default).

-spec set_presence_id(doc(), binary()) -> doc().
set_presence_id(Doc, PresenceId) ->
    kz_json:set_value([<<"presence_id">>], PresenceId, Doc).

-spec provision(doc()) -> kz_term:api_object().
provision(Doc) ->
    provision(Doc, 'undefined').

-spec provision(doc(), Default) -> kz_json:object() | Default.
provision(Doc, Default) ->
    kz_json:get_json_value([<<"provision">>], Doc, Default).

-spec set_provision(doc(), kz_json:object()) -> doc().
set_provision(Doc, Provision) ->
    kz_json:set_value([<<"provision">>], Provision, Doc).

-spec provision_combo_keys(doc()) -> kz_term:api_object().
provision_combo_keys(Doc) ->
    provision_combo_keys(Doc, 'undefined').

-spec provision_combo_keys(doc(), Default) -> kz_json:object() | Default.
provision_combo_keys(Doc, Default) ->
    kz_json:get_json_value([<<"provision">>, <<"combo_keys">>], Doc, Default).

-spec set_provision_combo_keys(doc(), kz_json:object()) -> doc().
set_provision_combo_keys(Doc, ProvisionComboKeys) ->
    kz_json:set_value([<<"provision">>, <<"combo_keys">>], ProvisionComboKeys, Doc).

-spec provision_combo_key(doc(), kz_json:key()) -> any().
provision_combo_key(Doc, ComboKey) ->
    provision_combo_key(Doc, ComboKey, 'undefined').

-spec provision_combo_key(doc(), kz_json:key(), Default) -> any() | Default.
provision_combo_key(Doc, ComboKey, Default) ->
    kz_json:get_value([<<"provision">>, <<"combo_keys">>, ComboKey], Doc, Default).

-spec set_provision_combo_key(doc(), kz_json:key(), any()) -> doc().
set_provision_combo_key(Doc, ComboKey, Value) ->
    kz_json:set_value([<<"provision">>, <<"combo_keys">>, ComboKey], Value, Doc).

-spec provision_endpoint_brand(doc()) -> kz_term:api_binary().
provision_endpoint_brand(Doc) ->
    provision_endpoint_brand(Doc, 'undefined').

-spec provision_endpoint_brand(doc(), Default) -> binary() | Default.
provision_endpoint_brand(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"provision">>, <<"endpoint_brand">>], Doc, Default).

-spec set_provision_endpoint_brand(doc(), binary()) -> doc().
set_provision_endpoint_brand(Doc, ProvisionEndpointBrand) ->
    kz_json:set_value([<<"provision">>, <<"endpoint_brand">>], ProvisionEndpointBrand, Doc).

-spec provision_endpoint_family(doc()) -> kz_term:api_binary().
provision_endpoint_family(Doc) ->
    provision_endpoint_family(Doc, 'undefined').

-spec provision_endpoint_family(doc(), Default) -> binary() | Default.
provision_endpoint_family(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"provision">>, <<"endpoint_family">>], Doc, Default).

-spec set_provision_endpoint_family(doc(), binary()) -> doc().
set_provision_endpoint_family(Doc, ProvisionEndpointFamily) ->
    kz_json:set_value([<<"provision">>, <<"endpoint_family">>], ProvisionEndpointFamily, Doc).

-spec provision_endpoint_model(doc()) -> any().
provision_endpoint_model(Doc) ->
    provision_endpoint_model(Doc, 'undefined').

-spec provision_endpoint_model(doc(), Default) -> any() | Default.
provision_endpoint_model(Doc, Default) ->
    kz_json:get_value([<<"provision">>, <<"endpoint_model">>], Doc, Default).

-spec set_provision_endpoint_model(doc(), any()) -> doc().
set_provision_endpoint_model(Doc, ProvisionEndpointModel) ->
    kz_json:set_value([<<"provision">>, <<"endpoint_model">>], ProvisionEndpointModel, Doc).

-spec provision_feature_keys(doc()) -> kz_term:api_object().
provision_feature_keys(Doc) ->
    provision_feature_keys(Doc, 'undefined').

-spec provision_feature_keys(doc(), Default) -> kz_json:object() | Default.
provision_feature_keys(Doc, Default) ->
    kz_json:get_json_value([<<"provision">>, <<"feature_keys">>], Doc, Default).

-spec set_provision_feature_keys(doc(), kz_json:object()) -> doc().
set_provision_feature_keys(Doc, ProvisionFeatureKeys) ->
    kz_json:set_value([<<"provision">>, <<"feature_keys">>], ProvisionFeatureKeys, Doc).

-spec provision_feature_key(doc(), kz_json:key()) -> any().
provision_feature_key(Doc, FeatureKey) ->
    provision_feature_key(Doc, FeatureKey, 'undefined').

-spec provision_feature_key(doc(), kz_json:key(), Default) -> any() | Default.
provision_feature_key(Doc, FeatureKey, Default) ->
    kz_json:get_value([<<"provision">>, <<"feature_keys">>, FeatureKey], Doc, Default).

-spec set_provision_feature_key(doc(), kz_json:key(), any()) -> doc().
set_provision_feature_key(Doc, FeatureKey, Value) ->
    kz_json:set_value([<<"provision">>, <<"feature_keys">>, FeatureKey], Value, Doc).

-spec provision_id(doc()) -> kz_term:api_binary().
provision_id(Doc) ->
    provision_id(Doc, 'undefined').

-spec provision_id(doc(), Default) -> binary() | Default.
provision_id(Doc, Default) ->
    kz_json:get_binary_value([<<"provision">>, <<"id">>], Doc, Default).

-spec set_provision_id(doc(), binary()) -> doc().
set_provision_id(Doc, ProvisionId) ->
    kz_json:set_value([<<"provision">>, <<"id">>], ProvisionId, Doc).

-spec register_overwrite_notify(doc()) -> boolean().
register_overwrite_notify(Doc) ->
    register_overwrite_notify(Doc, false).

-spec register_overwrite_notify(doc(), Default) -> boolean() | Default.
register_overwrite_notify(Doc, Default) ->
    kz_json:get_boolean_value([<<"register_overwrite_notify">>], Doc, Default).

-spec set_register_overwrite_notify(doc(), boolean()) -> doc().
set_register_overwrite_notify(Doc, RegisterOverwriteNotify) ->
    kz_json:set_value([<<"register_overwrite_notify">>], RegisterOverwriteNotify, Doc).

-spec ringtones(doc()) -> kz_json:object().
ringtones(Doc) ->
    ringtones(Doc, kz_json:new()).

-spec ringtones(doc(), Default) -> kz_json:object() | Default.
ringtones(Doc, Default) ->
    kz_json:get_json_value([<<"ringtones">>], Doc, Default).

-spec set_ringtones(doc(), kz_json:object()) -> doc().
set_ringtones(Doc, Ringtones) ->
    kz_json:set_value([<<"ringtones">>], Ringtones, Doc).

-spec ringtones_external(doc()) -> kz_term:api_binary().
ringtones_external(Doc) ->
    ringtones_external(Doc, 'undefined').

-spec ringtones_external(doc(), Default) -> binary() | Default.
ringtones_external(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"ringtones">>, <<"external">>], Doc, Default).

-spec set_ringtones_external(doc(), binary()) -> doc().
set_ringtones_external(Doc, RingtonesExternal) ->
    kz_json:set_value([<<"ringtones">>, <<"external">>], RingtonesExternal, Doc).

-spec ringtones_internal(doc()) -> kz_term:api_binary().
ringtones_internal(Doc) ->
    ringtones_internal(Doc, 'undefined').

-spec ringtones_internal(doc(), Default) -> binary() | Default.
ringtones_internal(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"ringtones">>, <<"internal">>], Doc, Default).

-spec set_ringtones_internal(doc(), binary()) -> doc().
set_ringtones_internal(Doc, RingtonesInternal) ->
    kz_json:set_value([<<"ringtones">>, <<"internal">>], RingtonesInternal, Doc).

-spec sip(doc()) -> kz_json:object().
sip(Doc) ->
    sip(Doc, kz_json:new()).

-spec sip(doc(), Default) -> kz_json:object() | Default.
sip(Doc, Default) ->
    kz_json:get_json_value([<<"sip">>], Doc, Default).

-spec set_sip(doc(), kz_json:object()) -> doc().
set_sip(Doc, Sip) ->
    kz_json:set_value([<<"sip">>], Sip, Doc).

-spec sip_custom_sip_headers(doc()) -> kz_json:object().
sip_custom_sip_headers(Doc) ->
    sip_custom_sip_headers(Doc, kz_json:new()).

-spec sip_custom_sip_headers(doc(), Default) -> kz_json:object() | Default.
sip_custom_sip_headers(Doc, Default) ->
    kz_json:get_json_value([<<"sip">>, <<"custom_sip_headers">>], Doc, Default).

-spec set_sip_custom_sip_headers(doc(), kz_json:object()) -> doc().
set_sip_custom_sip_headers(Doc, SipCustomSipHeaders) ->
    kz_json:set_value([<<"sip">>, <<"custom_sip_headers">>], SipCustomSipHeaders, Doc).

-spec sip_expire_seconds(doc()) -> integer().
sip_expire_seconds(Doc) ->
    sip_expire_seconds(Doc, 300).

-spec sip_expire_seconds(doc(), Default) -> integer() | Default.
sip_expire_seconds(Doc, Default) ->
    kz_json:get_integer_value([<<"sip">>, <<"expire_seconds">>], Doc, Default).

-spec set_sip_expire_seconds(doc(), integer()) -> doc().
set_sip_expire_seconds(Doc, SipExpireSeconds) ->
    kz_json:set_value([<<"sip">>, <<"expire_seconds">>], SipExpireSeconds, Doc).

-spec sip_ignore_completed_elsewhere(doc()) -> kz_term:api_boolean().
sip_ignore_completed_elsewhere(Doc) ->
    sip_ignore_completed_elsewhere(Doc, 'undefined').

-spec sip_ignore_completed_elsewhere(doc(), Default) -> boolean() | Default.
sip_ignore_completed_elsewhere(Doc, Default) ->
    kz_json:get_boolean_value([<<"sip">>, <<"ignore_completed_elsewhere">>], Doc, Default).

-spec set_sip_ignore_completed_elsewhere(doc(), boolean()) -> doc().
set_sip_ignore_completed_elsewhere(Doc, SipIgnoreCompletedElsewhere) ->
    kz_json:set_value([<<"sip">>, <<"ignore_completed_elsewhere">>], SipIgnoreCompletedElsewhere, Doc).

-spec sip_invite_format(doc()) -> binary().
sip_invite_format(Doc) ->
    sip_invite_format(Doc, <<"contact">>).

-spec sip_invite_format(doc(), Default) -> binary() | Default.
sip_invite_format(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"invite_format">>], Doc, Default).

-spec set_sip_invite_format(doc(), binary()) -> doc().
set_sip_invite_format(Doc, SipInviteFormat) ->
    kz_json:set_value([<<"sip">>, <<"invite_format">>], SipInviteFormat, Doc).

-spec sip_ip(doc()) -> kz_term:api_binary().
sip_ip(Doc) ->
    sip_ip(Doc, 'undefined').

-spec sip_ip(doc(), Default) -> binary() | Default.
sip_ip(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"ip">>], Doc, Default).

-spec set_sip_ip(doc(), binary()) -> doc().
set_sip_ip(Doc, SipIP) ->
    kz_json:set_value([<<"sip">>, <<"ip">>], SipIP, Doc).

-spec sip_method(doc()) -> binary().
sip_method(Doc) ->
    sip_method(Doc, <<"password">>).

-spec sip_method(doc(), Default) -> binary() | Default.
sip_method(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"method">>], Doc, Default).

-spec set_sip_method(doc(), binary()) -> doc().
set_sip_method(Doc, SipMethod) ->
    kz_json:set_value([<<"sip">>, <<"method">>], SipMethod, Doc).

-spec sip_number(doc()) -> kz_term:api_binary().
sip_number(Doc) ->
    sip_number(Doc, 'undefined').

-spec sip_number(doc(), Default) -> binary() | Default.
sip_number(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"number">>], Doc, Default).

-spec set_sip_number(doc(), binary()) -> doc().
set_sip_number(Doc, SipNumber) ->
    kz_json:set_value([<<"sip">>, <<"number">>], SipNumber, Doc).

-spec sip_password(doc()) -> kz_term:api_ne_binary().
sip_password(Doc) ->
    sip_password(Doc, 'undefined').

-spec sip_password(doc(), Default) -> kz_term:ne_binary() | Default.
sip_password(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"password">>], Doc, Default).

-spec set_sip_password(doc(), kz_term:ne_binary()) -> doc().
set_sip_password(Doc, SipPassword) ->
    kz_json:set_value([<<"sip">>, <<"password">>], SipPassword, Doc).

-spec sip_realm(doc()) -> kz_term:api_ne_binary().
sip_realm(Doc) ->
    sip_realm(Doc, 'undefined').

-spec sip_realm(doc(), Default) -> kz_term:ne_binary() | Default.
sip_realm(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"realm">>], Doc, Default).

-spec set_sip_realm(doc(), kz_term:ne_binary()) -> doc().
set_sip_realm(Doc, SipRealm) ->
    kz_json:set_value([<<"sip">>, <<"realm">>], SipRealm, Doc).

-spec sip_route(doc()) -> kz_term:api_binary().
sip_route(Doc) ->
    sip_route(Doc, 'undefined').

-spec sip_route(doc(), Default) -> binary() | Default.
sip_route(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"route">>], Doc, Default).

-spec set_sip_route(doc(), binary()) -> doc().
set_sip_route(Doc, SipRoute) ->
    kz_json:set_value([<<"sip">>, <<"route">>], SipRoute, Doc).

-spec sip_static_route(doc()) -> kz_term:api_binary().
sip_static_route(Doc) ->
    sip_static_route(Doc, 'undefined').

-spec sip_static_route(doc(), Default) -> binary() | Default.
sip_static_route(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"static_route">>], Doc, Default).

-spec set_sip_static_route(doc(), binary()) -> doc().
set_sip_static_route(Doc, SipStaticRoute) ->
    kz_json:set_value([<<"sip">>, <<"static_route">>], SipStaticRoute, Doc).

-spec sip_username(doc()) -> kz_term:api_ne_binary().
sip_username(Doc) ->
    sip_username(Doc, 'undefined').

-spec sip_username(doc(), Default) -> kz_term:ne_binary() | Default.
sip_username(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"username">>], Doc, Default).

-spec set_sip_username(doc(), kz_term:ne_binary()) -> doc().
set_sip_username(Doc, SipUsername) ->
    kz_json:set_value([<<"sip">>, <<"username">>], SipUsername, Doc).

-spec suppress_unregister_notifications(doc()) -> boolean().
suppress_unregister_notifications(Doc) ->
    suppress_unregister_notifications(Doc, false).

-spec suppress_unregister_notifications(doc(), Default) -> boolean() | Default.
suppress_unregister_notifications(Doc, Default) ->
    kz_json:get_boolean_value([<<"suppress_unregister_notifications">>], Doc, Default).

-spec set_suppress_unregister_notifications(doc(), boolean()) -> doc().
set_suppress_unregister_notifications(Doc, SuppressUnregisterNotifications) ->
    kz_json:set_value([<<"suppress_unregister_notifications">>], SuppressUnregisterNotifications, Doc).

-spec timezone(doc()) -> kz_term:api_binary().
timezone(Doc) ->
    timezone(Doc, 'undefined').

-spec timezone(doc(), Default) -> binary() | Default.
timezone(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"timezone">>], Doc, Default).

-spec set_timezone(doc(), binary()) -> doc().
set_timezone(Doc, Timezone) ->
    kz_json:set_value([<<"timezone">>], Timezone, Doc).

-spec fetch(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> {'ok', doc()} |
                                                                 {'error', any()}.
fetch(Account=?NE_BINARY, DeviceId=?NE_BINARY) ->
    AccountDb = kz_util:format_account_db(Account),
    kz_datamgr:open_cache_doc(AccountDb, DeviceId, [{'cache_failures', 'false'}]);
fetch(_, _) ->
    {'error', 'invalid_parameters'}.

-spec type() -> kz_term:ne_binary().
type() -> <<"device">>.

-spec is_device(doc()) -> boolean().
is_device(Doc) ->
    kz_doc:type(Doc) =:= type().

-spec custom_sip_headers_inbound(doc()) -> kz_term:api_object().
custom_sip_headers_inbound(DeviceJObj) ->
    custom_sip_headers_inbound(DeviceJObj, 'undefined').

-spec custom_sip_headers_inbound(doc(), Default) -> kz_json:object() | Default.
custom_sip_headers_inbound(DeviceJObj, Default) ->
    CSH = sip_custom_sip_headers(DeviceJObj),
    kz_custom_sip_headers:inbound(CSH, Default).

-spec custom_sip_header_inbound(doc(), kz_json:key()) -> kz_json:json_term() | 'undefined'.
custom_sip_header_inbound(DeviceJObj, Name) ->
    custom_sip_header_inbound(DeviceJObj, Name, 'undefined').

-spec custom_sip_header_inbound(doc(), kz_json:key(), Default) -> kz_json:json_term() | Default.
custom_sip_header_inbound(DeviceJObj, Name, Default) ->
    CSH = sip_custom_sip_headers(DeviceJObj),
    kz_custom_sip_headers:inbound_header(CSH, Name, Default).

-spec custom_sip_header_outbound(doc(), kz_json:key()) -> kz_json:json_term() | 'undefined'.
custom_sip_header_outbound(DeviceJObj, Name) ->
    custom_sip_header_outbound(DeviceJObj, Name, 'undefined').

-spec custom_sip_header_outbound(doc(), kz_json:key(), Default) -> kz_json:json_term() | Default.
custom_sip_header_outbound(DeviceJObj, Name, Default) ->
    CSH = sip_custom_sip_headers(DeviceJObj),
    kz_custom_sip_headers:outbound_header(CSH, Name, Default).

-spec custom_sip_headers_outbound(doc()) -> kz_term:api_object().
custom_sip_headers_outbound(DeviceJObj) ->
    custom_sip_headers_outbound(DeviceJObj, 'undefined').

-spec custom_sip_headers_outbound(doc(), Default) -> kz_json:object() | Default.
custom_sip_headers_outbound(DeviceJObj, Default) ->
    CSH = sip_custom_sip_headers(DeviceJObj),
    kz_custom_sip_headers:outbound(CSH, Default).

-spec set_custom_sip_headers_inbound(doc(), kz_json:object()) -> doc().
set_custom_sip_headers_inbound(Device, Headers) ->
    CSH = sip_custom_sip_headers(Device),
    InboundCSH = kz_custom_sip_headers:set_inbound(CSH, Headers),
    set_sip_custom_sip_headers(Device, InboundCSH).

-spec set_custom_sip_headers_outbound(doc(), kz_json:object()) -> doc().
set_custom_sip_headers_outbound(Device, Headers) ->
    CSH = sip_custom_sip_headers(Device),
    OutboundCSH = kz_custom_sip_headers:set_outbound(CSH, Headers),
    set_sip_custom_sip_headers(Device, OutboundCSH).

-spec set_outbound_flags(kz_json:object(), kz_term:api_ne_binaries(), kz_term:api_ne_binaries()) ->
                                kz_json:object().
set_outbound_flags(JObj, 'undefined', DynamicFlags) ->
    set_outbound_flags(JObj, [], DynamicFlags);
set_outbound_flags(JObj, StaticFlags, 'undefined') ->
    set_outbound_flags(JObj, StaticFlags, []);
set_outbound_flags(JObj, StaticFlags, DynamicFlags) when is_list(StaticFlags),
                                                         is_list(DynamicFlags) ->
    Flags = kz_json:from_list([{?DYNAMIC_FLAGS, DynamicFlags}
                              ,{?STATIC_FLAGS, StaticFlags}
                              ]),
    set_outbound_flags(JObj, Flags).

-spec outbound_static_flags(kz_json:object()) -> kz_term:ne_binaries().
outbound_static_flags(JObj) ->
    OutboundFlags = outbound_flags(JObj),
    kz_json:get_list_value(?STATIC_FLAGS, OutboundFlags, []).

-spec set_outbound_static_flags(kz_json:object(), kz_term:ne_binaries()) -> kz_json:object().
set_outbound_static_flags(JObj, Flags) when is_list(Flags) ->
    set_outbound_flags(JObj, Flags).

-spec outbound_dynamic_flags(kz_json:object()) -> kz_term:ne_binaries().
outbound_dynamic_flags(JObj) ->
    OutboundFlags = outbound_flags(JObj),
    kz_json:get_list_value(?DYNAMIC_FLAGS, OutboundFlags, []).

-spec set_outbound_dynamic_flags(doc(), kz_term:ne_binaries()) -> doc().
set_outbound_dynamic_flags(JObj, Flags) when is_list(Flags) ->
    OutboundFlags = outbound_flags(JObj),
    set_outbound_flags(JObj, kz_json:set_value(?DYNAMIC_FLAGS, Flags, OutboundFlags)).
