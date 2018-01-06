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
-export([formatters/1, formatters/2, set_formatters/2]).
-export([language/1, language/2, set_language/2]).
-export([media/1, media/2, set_media/2]).
-export([metaflows/1, metaflows/2, set_metaflows/2]).
-export([music_on_hold/1, music_on_hold/2, set_music_on_hold/2]).
-export([music_on_hold_media_id/1, music_on_hold_media_id/2, set_music_on_hold_media_id/2]).
-export([mwi_unsolicitated_updates/1, mwi_unsolicitated_updates/2, set_mwi_unsolicitated_updates/2]).
-export([name/1, name/2, set_name/2]).
-export([outbound_flags/1, outbound_flags/2, set_outbound_flags/2]).
-export([owner_id/1, owner_id/2, set_owner_id/2]).
-export([presence_id/1, presence_id/2, set_presence_id/2]).
-export([provision/1, provision/2, set_provision/2]).
-export([provision_combo_keys/1, provision_combo_keys/2, set_provision_combo_keys/2]).
-export([provision_endpoint_brand/1, provision_endpoint_brand/2, set_provision_endpoint_brand/2]).
-export([provision_endpoint_family/1, provision_endpoint_family/2, set_provision_endpoint_family/2]).
-export([provision_endpoint_model/1, provision_endpoint_model/2, set_provision_endpoint_model/2]).
-export([provision_feature_keys/1, provision_feature_keys/2, set_provision_feature_keys/2]).
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


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec call_forward(doc()) -> api_object().
-spec call_forward(doc(), Default) -> kz_json:object() | Default.
call_forward(Doc) ->
    call_forward(Doc, 'undefined').
call_forward(Doc, Default) ->
    kz_json:get_json_value(<<"call_forward">>, Doc, Default).

-spec set_call_forward(doc(), kz_json:object()) -> doc().
set_call_forward(Doc, CallForward) ->
    kz_json:set_value(<<"call_forward">>, CallForward, Doc).

-spec call_forward_direct_calls_only(doc()) -> boolean().
-spec call_forward_direct_calls_only(doc(), Default) -> boolean() | Default.
call_forward_direct_calls_only(Doc) ->
    call_forward_direct_calls_only(Doc, false).
call_forward_direct_calls_only(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"direct_calls_only">>], Doc, Default).

-spec set_call_forward_direct_calls_only(doc(), boolean()) -> doc().
set_call_forward_direct_calls_only(Doc, CallForwardDirectCallsOnly) ->
    kz_json:set_value([<<"call_forward">>, <<"direct_calls_only">>], CallForwardDirectCallsOnly, Doc).

-spec call_forward_enabled(doc()) -> boolean().
-spec call_forward_enabled(doc(), Default) -> boolean() | Default.
call_forward_enabled(Doc) ->
    call_forward_enabled(Doc, false).
call_forward_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"enabled">>], Doc, Default).

-spec set_call_forward_enabled(doc(), boolean()) -> doc().
set_call_forward_enabled(Doc, CallForwardEnabled) ->
    kz_json:set_value([<<"call_forward">>, <<"enabled">>], CallForwardEnabled, Doc).

-spec call_forward_failover(doc()) -> boolean().
-spec call_forward_failover(doc(), Default) -> boolean() | Default.
call_forward_failover(Doc) ->
    call_forward_failover(Doc, false).
call_forward_failover(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"failover">>], Doc, Default).

-spec set_call_forward_failover(doc(), boolean()) -> doc().
set_call_forward_failover(Doc, CallForwardFailover) ->
    kz_json:set_value([<<"call_forward">>, <<"failover">>], CallForwardFailover, Doc).

-spec call_forward_ignore_early_media(doc()) -> boolean().
-spec call_forward_ignore_early_media(doc(), Default) -> boolean() | Default.
call_forward_ignore_early_media(Doc) ->
    call_forward_ignore_early_media(Doc, true).
call_forward_ignore_early_media(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"ignore_early_media">>], Doc, Default).

-spec set_call_forward_ignore_early_media(doc(), boolean()) -> doc().
set_call_forward_ignore_early_media(Doc, CallForwardIgnoreEarlyMedia) ->
    kz_json:set_value([<<"call_forward">>, <<"ignore_early_media">>], CallForwardIgnoreEarlyMedia, Doc).

-spec call_forward_keep_caller_id(doc()) -> boolean().
-spec call_forward_keep_caller_id(doc(), Default) -> boolean() | Default.
call_forward_keep_caller_id(Doc) ->
    call_forward_keep_caller_id(Doc, true).
call_forward_keep_caller_id(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"keep_caller_id">>], Doc, Default).

-spec set_call_forward_keep_caller_id(doc(), boolean()) -> doc().
set_call_forward_keep_caller_id(Doc, CallForwardKeepCallerId) ->
    kz_json:set_value([<<"call_forward">>, <<"keep_caller_id">>], CallForwardKeepCallerId, Doc).

-spec call_forward_number(doc()) -> api_binary().
-spec call_forward_number(doc(), Default) -> binary() | Default.
call_forward_number(Doc) ->
    call_forward_number(Doc, 'undefined').
call_forward_number(Doc, Default) ->
    kz_json:get_binary_value([<<"call_forward">>, <<"number">>], Doc, Default).

-spec set_call_forward_number(doc(), binary()) -> doc().
set_call_forward_number(Doc, CallForwardNumber) ->
    kz_json:set_value([<<"call_forward">>, <<"number">>], CallForwardNumber, Doc).

-spec call_forward_require_keypress(doc()) -> boolean().
-spec call_forward_require_keypress(doc(), Default) -> boolean() | Default.
call_forward_require_keypress(Doc) ->
    call_forward_require_keypress(Doc, true).
call_forward_require_keypress(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"require_keypress">>], Doc, Default).

-spec set_call_forward_require_keypress(doc(), boolean()) -> doc().
set_call_forward_require_keypress(Doc, CallForwardRequireKeypress) ->
    kz_json:set_value([<<"call_forward">>, <<"require_keypress">>], CallForwardRequireKeypress, Doc).

-spec call_forward_substitute(doc()) -> boolean().
-spec call_forward_substitute(doc(), Default) -> boolean() | Default.
call_forward_substitute(Doc) ->
    call_forward_substitute(Doc, true).
call_forward_substitute(Doc, Default) ->
    kz_json:get_boolean_value([<<"call_forward">>, <<"substitute">>], Doc, Default).

-spec set_call_forward_substitute(doc(), boolean()) -> doc().
set_call_forward_substitute(Doc, CallForwardSubstitute) ->
    kz_json:set_value([<<"call_forward">>, <<"substitute">>], CallForwardSubstitute, Doc).

-spec call_recording(doc()) -> api_object().
-spec call_recording(doc(), Default) -> kz_json:object() | Default.
call_recording(Doc) ->
    call_recording(Doc, 'undefined').
call_recording(Doc, Default) ->
    kz_json:get_json_value(<<"call_recording">>, Doc, Default).

-spec set_call_recording(doc(), kz_json:object()) -> doc().
set_call_recording(Doc, CallRecording) ->
    kz_json:set_value(<<"call_recording">>, CallRecording, Doc).

-spec call_restriction(doc()) -> kz_json:object().
-spec call_restriction(doc(), Default) -> kz_json:object() | Default.
call_restriction(Doc) ->
    call_restriction(Doc, kz_json:new()).
call_restriction(Doc, Default) ->
    kz_json:get_json_value(<<"call_restriction">>, Doc, Default).

-spec set_call_restriction(doc(), kz_json:object()) -> doc().
set_call_restriction(Doc, CallRestriction) ->
    kz_json:set_value(<<"call_restriction">>, CallRestriction, Doc).

-spec call_waiting(doc()) -> api_object().
-spec call_waiting(doc(), Default) -> kz_json:object() | Default.
call_waiting(Doc) ->
    call_waiting(Doc, 'undefined').
call_waiting(Doc, Default) ->
    kz_json:get_json_value(<<"call_waiting">>, Doc, Default).

-spec set_call_waiting(doc(), kz_json:object()) -> doc().
set_call_waiting(Doc, CallWaiting) ->
    kz_json:set_value(<<"call_waiting">>, CallWaiting, Doc).

-spec caller_id(doc()) -> api_object().
-spec caller_id(doc(), Default) -> kz_json:object() | Default.
caller_id(Doc) ->
    caller_id(Doc, 'undefined').
caller_id(Doc, Default) ->
    kz_json:get_json_value(<<"caller_id">>, Doc, Default).

-spec set_caller_id(doc(), kz_json:object()) -> doc().
set_caller_id(Doc, CallerId) ->
    kz_json:set_value(<<"caller_id">>, CallerId, Doc).

-spec contact_list(doc()) -> kz_json:object().
-spec contact_list(doc(), Default) -> kz_json:object() | Default.
contact_list(Doc) ->
    contact_list(Doc, kz_json:new()).
contact_list(Doc, Default) ->
    kz_json:get_json_value(<<"contact_list">>, Doc, Default).

-spec set_contact_list(doc(), kz_json:object()) -> doc().
set_contact_list(Doc, ContactList) ->
    kz_json:set_value(<<"contact_list">>, ContactList, Doc).

-spec contact_list_exclude(doc()) -> api_boolean().
-spec contact_list_exclude(doc(), Default) -> boolean() | Default.
contact_list_exclude(Doc) ->
    contact_list_exclude(Doc, 'undefined').
contact_list_exclude(Doc, Default) ->
    kz_json:get_boolean_value([<<"contact_list">>, <<"exclude">>], Doc, Default).

-spec set_contact_list_exclude(doc(), boolean()) -> doc().
set_contact_list_exclude(Doc, ContactListExclude) ->
    kz_json:set_value([<<"contact_list">>, <<"exclude">>], ContactListExclude, Doc).

-spec device_type(doc()) -> api_binary().
-spec device_type(doc(), Default) -> binary() | Default.
device_type(Doc) ->
    device_type(Doc, 'undefined').
device_type(Doc, Default) ->
    kz_json:get_binary_value(<<"device_type">>, Doc, Default).

-spec set_device_type(doc(), binary()) -> doc().
set_device_type(Doc, DeviceType) ->
    kz_json:set_value(<<"device_type">>, DeviceType, Doc).

-spec dial_plan(doc()) -> api_object().
-spec dial_plan(doc(), Default) -> kz_json:object() | Default.
dial_plan(Doc) ->
    dial_plan(Doc, 'undefined').
dial_plan(Doc, Default) ->
    kz_json:get_json_value(<<"dial_plan">>, Doc, Default).

-spec set_dial_plan(doc(), kz_json:object()) -> doc().
set_dial_plan(Doc, DialPlan) ->
    kz_json:set_value(<<"dial_plan">>, DialPlan, Doc).

-spec do_not_disturb(doc()) -> api_object().
-spec do_not_disturb(doc(), Default) -> kz_json:object() | Default.
do_not_disturb(Doc) ->
    do_not_disturb(Doc, 'undefined').
do_not_disturb(Doc, Default) ->
    kz_json:get_json_value(<<"do_not_disturb">>, Doc, Default).

-spec set_do_not_disturb(doc(), kz_json:object()) -> doc().
set_do_not_disturb(Doc, DoNotDisturb) ->
    kz_json:set_value(<<"do_not_disturb">>, DoNotDisturb, Doc).

-spec do_not_disturb_enabled(doc()) -> api_boolean().
-spec do_not_disturb_enabled(doc(), Default) -> boolean() | Default.
do_not_disturb_enabled(Doc) ->
    do_not_disturb_enabled(Doc, 'undefined').
do_not_disturb_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"do_not_disturb">>, <<"enabled">>], Doc, Default).

-spec set_do_not_disturb_enabled(doc(), boolean()) -> doc().
set_do_not_disturb_enabled(Doc, DoNotDisturbEnabled) ->
    kz_json:set_value([<<"do_not_disturb">>, <<"enabled">>], DoNotDisturbEnabled, Doc).

-spec enabled(doc()) -> boolean().
-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc) ->
    enabled(Doc, true).
enabled(Doc, Default) ->
    kz_json:get_boolean_value(<<"enabled">>, Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value(<<"enabled">>, Enabled, Doc).

-spec exclude_from_queues(doc()) -> boolean().
-spec exclude_from_queues(doc(), Default) -> boolean() | Default.
exclude_from_queues(Doc) ->
    exclude_from_queues(Doc, false).
exclude_from_queues(Doc, Default) ->
    kz_json:get_boolean_value(<<"exclude_from_queues">>, Doc, Default).

-spec set_exclude_from_queues(doc(), boolean()) -> doc().
set_exclude_from_queues(Doc, ExcludeFromQueues) ->
    kz_json:set_value(<<"exclude_from_queues">>, ExcludeFromQueues, Doc).

-spec formatters(doc()) -> api_object().
-spec formatters(doc(), Default) -> kz_json:object() | Default.
formatters(Doc) ->
    formatters(Doc, 'undefined').
formatters(Doc, Default) ->
    kz_json:get_json_value(<<"formatters">>, Doc, Default).

-spec set_formatters(doc(), kz_json:object()) -> doc().
set_formatters(Doc, Formatters) ->
    kz_json:set_value(<<"formatters">>, Formatters, Doc).

-spec language(doc()) -> api_binary().
-spec language(doc(), Default) -> binary() | Default.
language(Doc) ->
    language(Doc, 'undefined').
language(Doc, Default) ->
    kz_json:get_binary_value(<<"language">>, Doc, Default).

-spec set_language(doc(), binary()) -> doc().
set_language(Doc, Language) ->
    kz_json:set_value(<<"language">>, Language, Doc).

-spec media(doc()) -> api_object().
-spec media(doc(), Default) -> kz_json:object() | Default.
media(Doc) ->
    media(Doc, 'undefined').
media(Doc, Default) ->
    kz_json:get_json_value(<<"media">>, Doc, Default).

-spec set_media(doc(), kz_json:object()) -> doc().
set_media(Doc, Media) ->
    kz_json:set_value(<<"media">>, Media, Doc).

-spec metaflows(doc()) -> api_object().
-spec metaflows(doc(), Default) -> kz_json:object() | Default.
metaflows(Doc) ->
    metaflows(Doc, 'undefined').
metaflows(Doc, Default) ->
    kz_json:get_json_value(<<"metaflows">>, Doc, Default).

-spec set_metaflows(doc(), kz_json:object()) -> doc().
set_metaflows(Doc, Metaflows) ->
    kz_json:set_value(<<"metaflows">>, Metaflows, Doc).

-spec music_on_hold(doc()) -> kz_json:object().
-spec music_on_hold(doc(), Default) -> kz_json:object() | Default.
music_on_hold(Doc) ->
    music_on_hold(Doc, kz_json:new()).
music_on_hold(Doc, Default) ->
    kz_json:get_json_value(<<"music_on_hold">>, Doc, Default).

-spec set_music_on_hold(doc(), kz_json:object()) -> doc().
set_music_on_hold(Doc, MusicOnHold) ->
    kz_json:set_value(<<"music_on_hold">>, MusicOnHold, Doc).

-spec music_on_hold_media_id(doc()) -> api_binary().
-spec music_on_hold_media_id(doc(), Default) -> binary() | Default.
music_on_hold_media_id(Doc) ->
    music_on_hold_media_id(Doc, 'undefined').
music_on_hold_media_id(Doc, Default) ->
    kz_json:get_binary_value([<<"music_on_hold">>, <<"media_id">>], Doc, Default).

-spec set_music_on_hold_media_id(doc(), binary()) -> doc().
set_music_on_hold_media_id(Doc, MusicOnHoldMediaId) ->
    kz_json:set_value([<<"music_on_hold">>, <<"media_id">>], MusicOnHoldMediaId, Doc).

-spec mwi_unsolicitated_updates(doc()) -> boolean().
-spec mwi_unsolicitated_updates(doc(), Default) -> boolean() | Default.
mwi_unsolicitated_updates(Doc) ->
    mwi_unsolicitated_updates(Doc, true).
mwi_unsolicitated_updates(Doc, Default) ->
    kz_json:get_boolean_value(<<"mwi_unsolicitated_updates">>, Doc, Default).

-spec set_mwi_unsolicitated_updates(doc(), boolean()) -> doc().
set_mwi_unsolicitated_updates(Doc, MwiUnsolicitatedUpdates) ->
    kz_json:set_value(<<"mwi_unsolicitated_updates">>, MwiUnsolicitatedUpdates, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec outbound_flags(doc()) -> api_ne_binaries().
-spec outbound_flags(doc(), Default) -> ne_binaries() | Default.
outbound_flags(Doc) ->
    outbound_flags(Doc, 'undefined').
outbound_flags(Doc, Default) ->
    kz_json:get_list_value(<<"outbound_flags">>, Doc, Default).

-spec set_outbound_flags(doc(), ne_binaries()) -> doc().
set_outbound_flags(Doc, OutboundFlags) ->
    kz_json:set_value(<<"outbound_flags">>, OutboundFlags, Doc).

-spec owner_id(doc()) -> api_ne_binary().
-spec owner_id(doc(), Default) -> ne_binary() | Default.
owner_id(Doc) ->
    owner_id(Doc, 'undefined').
owner_id(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"owner_id">>, Doc, Default).

-spec set_owner_id(doc(), ne_binary()) -> doc().
set_owner_id(Doc, OwnerId) ->
    kz_json:set_value(<<"owner_id">>, OwnerId, Doc).

-spec presence_id(doc()) -> api_binary().
-spec presence_id(doc(), Default) -> binary() | Default.
presence_id(Doc) ->
    presence_id(Doc, 'undefined').
presence_id(Doc, Default) ->
    kz_json:get_binary_value(<<"presence_id">>, Doc, Default).

-spec set_presence_id(doc(), binary()) -> doc().
set_presence_id(Doc, PresenceId) ->
    kz_json:set_value(<<"presence_id">>, PresenceId, Doc).

-spec provision(doc()) -> api_object().
-spec provision(doc(), Default) -> kz_json:object() | Default.
provision(Doc) ->
    provision(Doc, 'undefined').
provision(Doc, Default) ->
    kz_json:get_json_value(<<"provision">>, Doc, Default).

-spec set_provision(doc(), kz_json:object()) -> doc().
set_provision(Doc, Provision) ->
    kz_json:set_value(<<"provision">>, Provision, Doc).

-spec provision_combo_keys(doc()) -> api_object().
-spec provision_combo_keys(doc(), Default) -> kz_json:object() | Default.
provision_combo_keys(Doc) ->
    provision_combo_keys(Doc, 'undefined').
provision_combo_keys(Doc, Default) ->
    kz_json:get_json_value([<<"provision">>, <<"combo_keys">>], Doc, Default).

-spec set_provision_combo_keys(doc(), kz_json:object()) -> doc().
set_provision_combo_keys(Doc, ProvisionComboKeys) ->
    kz_json:set_value([<<"provision">>, <<"combo_keys">>], ProvisionComboKeys, Doc).

-spec provision_endpoint_brand(doc()) -> api_binary().
-spec provision_endpoint_brand(doc(), Default) -> binary() | Default.
provision_endpoint_brand(Doc) ->
    provision_endpoint_brand(Doc, 'undefined').
provision_endpoint_brand(Doc, Default) ->
    kz_json:get_binary_value([<<"provision">>, <<"endpoint_brand">>], Doc, Default).

-spec set_provision_endpoint_brand(doc(), binary()) -> doc().
set_provision_endpoint_brand(Doc, ProvisionEndpointBrand) ->
    kz_json:set_value([<<"provision">>, <<"endpoint_brand">>], ProvisionEndpointBrand, Doc).

-spec provision_endpoint_family(doc()) -> api_binary().
-spec provision_endpoint_family(doc(), Default) -> binary() | Default.
provision_endpoint_family(Doc) ->
    provision_endpoint_family(Doc, 'undefined').
provision_endpoint_family(Doc, Default) ->
    kz_json:get_binary_value([<<"provision">>, <<"endpoint_family">>], Doc, Default).

-spec set_provision_endpoint_family(doc(), binary()) -> doc().
set_provision_endpoint_family(Doc, ProvisionEndpointFamily) ->
    kz_json:set_value([<<"provision">>, <<"endpoint_family">>], ProvisionEndpointFamily, Doc).

-spec provision_endpoint_model(doc()) -> any().
-spec provision_endpoint_model(doc(), Default) -> any() | Default.
provision_endpoint_model(Doc) ->
    provision_endpoint_model(Doc, 'undefined').
provision_endpoint_model(Doc, Default) ->
    kz_json:get_value([<<"provision">>, <<"endpoint_model">>], Doc, Default).

-spec set_provision_endpoint_model(doc(), any()) -> doc().
set_provision_endpoint_model(Doc, ProvisionEndpointModel) ->
    kz_json:set_value([<<"provision">>, <<"endpoint_model">>], ProvisionEndpointModel, Doc).

-spec provision_feature_keys(doc()) -> api_object().
-spec provision_feature_keys(doc(), Default) -> kz_json:object() | Default.
provision_feature_keys(Doc) ->
    provision_feature_keys(Doc, 'undefined').
provision_feature_keys(Doc, Default) ->
    kz_json:get_json_value([<<"provision">>, <<"feature_keys">>], Doc, Default).

-spec set_provision_feature_keys(doc(), kz_json:object()) -> doc().
set_provision_feature_keys(Doc, ProvisionFeatureKeys) ->
    kz_json:set_value([<<"provision">>, <<"feature_keys">>], ProvisionFeatureKeys, Doc).

-spec register_overwrite_notify(doc()) -> boolean().
-spec register_overwrite_notify(doc(), Default) -> boolean() | Default.
register_overwrite_notify(Doc) ->
    register_overwrite_notify(Doc, false).
register_overwrite_notify(Doc, Default) ->
    kz_json:get_boolean_value(<<"register_overwrite_notify">>, Doc, Default).

-spec set_register_overwrite_notify(doc(), boolean()) -> doc().
set_register_overwrite_notify(Doc, RegisterOverwriteNotify) ->
    kz_json:set_value(<<"register_overwrite_notify">>, RegisterOverwriteNotify, Doc).

-spec ringtones(doc()) -> kz_json:object().
-spec ringtones(doc(), Default) -> kz_json:object() | Default.
ringtones(Doc) ->
    ringtones(Doc, kz_json:new()).
ringtones(Doc, Default) ->
    kz_json:get_json_value(<<"ringtones">>, Doc, Default).

-spec set_ringtones(doc(), kz_json:object()) -> doc().
set_ringtones(Doc, Ringtones) ->
    kz_json:set_value(<<"ringtones">>, Ringtones, Doc).

-spec ringtones_external(doc()) -> api_binary().
-spec ringtones_external(doc(), Default) -> binary() | Default.
ringtones_external(Doc) ->
    ringtones_external(Doc, 'undefined').
ringtones_external(Doc, Default) ->
    kz_json:get_binary_value([<<"ringtones">>, <<"external">>], Doc, Default).

-spec set_ringtones_external(doc(), binary()) -> doc().
set_ringtones_external(Doc, RingtonesExternal) ->
    kz_json:set_value([<<"ringtones">>, <<"external">>], RingtonesExternal, Doc).

-spec ringtones_internal(doc()) -> api_binary().
-spec ringtones_internal(doc(), Default) -> binary() | Default.
ringtones_internal(Doc) ->
    ringtones_internal(Doc, 'undefined').
ringtones_internal(Doc, Default) ->
    kz_json:get_binary_value([<<"ringtones">>, <<"internal">>], Doc, Default).

-spec set_ringtones_internal(doc(), binary()) -> doc().
set_ringtones_internal(Doc, RingtonesInternal) ->
    kz_json:set_value([<<"ringtones">>, <<"internal">>], RingtonesInternal, Doc).

-spec sip(doc()) -> kz_json:object().
-spec sip(doc(), Default) -> kz_json:object() | Default.
sip(Doc) ->
    sip(Doc, kz_json:new()).
sip(Doc, Default) ->
    kz_json:get_json_value(<<"sip">>, Doc, Default).

-spec set_sip(doc(), kz_json:object()) -> doc().
set_sip(Doc, Sip) ->
    kz_json:set_value(<<"sip">>, Sip, Doc).

-spec sip_custom_sip_headers(doc()) -> api_object().
-spec sip_custom_sip_headers(doc(), Default) -> kz_json:object() | Default.
sip_custom_sip_headers(Doc) ->
    sip_custom_sip_headers(Doc, 'undefined').
sip_custom_sip_headers(Doc, Default) ->
    kz_json:get_json_value([<<"sip">>, <<"custom_sip_headers">>], Doc, Default).

-spec set_sip_custom_sip_headers(doc(), kz_json:object()) -> doc().
set_sip_custom_sip_headers(Doc, SipCustomSipHeaders) ->
    kz_json:set_value([<<"sip">>, <<"custom_sip_headers">>], SipCustomSipHeaders, Doc).

-spec sip_expire_seconds(doc()) -> integer().
-spec sip_expire_seconds(doc(), Default) -> integer() | Default.
sip_expire_seconds(Doc) ->
    sip_expire_seconds(Doc, 300).
sip_expire_seconds(Doc, Default) ->
    kz_json:get_integer_value([<<"sip">>, <<"expire_seconds">>], Doc, Default).

-spec set_sip_expire_seconds(doc(), integer()) -> doc().
set_sip_expire_seconds(Doc, SipExpireSeconds) ->
    kz_json:set_value([<<"sip">>, <<"expire_seconds">>], SipExpireSeconds, Doc).

-spec sip_ignore_completed_elsewhere(doc()) -> api_boolean().
-spec sip_ignore_completed_elsewhere(doc(), Default) -> boolean() | Default.
sip_ignore_completed_elsewhere(Doc) ->
    sip_ignore_completed_elsewhere(Doc, 'undefined').
sip_ignore_completed_elsewhere(Doc, Default) ->
    kz_json:get_boolean_value([<<"sip">>, <<"ignore_completed_elsewhere">>], Doc, Default).

-spec set_sip_ignore_completed_elsewhere(doc(), boolean()) -> doc().
set_sip_ignore_completed_elsewhere(Doc, SipIgnoreCompletedElsewhere) ->
    kz_json:set_value([<<"sip">>, <<"ignore_completed_elsewhere">>], SipIgnoreCompletedElsewhere, Doc).

-spec sip_invite_format(doc()) -> binary().
-spec sip_invite_format(doc(), Default) -> binary() | Default.
sip_invite_format(Doc) ->
    sip_invite_format(Doc, <<"contact">>).
sip_invite_format(Doc, Default) ->
    kz_json:get_binary_value([<<"sip">>, <<"invite_format">>], Doc, Default).

-spec set_sip_invite_format(doc(), binary()) -> doc().
set_sip_invite_format(Doc, SipInviteFormat) ->
    kz_json:set_value([<<"sip">>, <<"invite_format">>], SipInviteFormat, Doc).

-spec sip_ip(doc()) -> api_binary().
-spec sip_ip(doc(), Default) -> binary() | Default.
sip_ip(Doc) ->
    sip_ip(Doc, 'undefined').
sip_ip(Doc, Default) ->
    kz_json:get_binary_value([<<"sip">>, <<"ip">>], Doc, Default).

-spec set_sip_ip(doc(), binary()) -> doc().
set_sip_ip(Doc, SipIP) ->
    kz_json:set_value([<<"sip">>, <<"ip">>], SipIP, Doc).

-spec sip_method(doc()) -> binary().
-spec sip_method(doc(), Default) -> binary() | Default.
sip_method(Doc) ->
    sip_method(Doc, <<"password">>).
sip_method(Doc, Default) ->
    kz_json:get_binary_value([<<"sip">>, <<"method">>], Doc, Default).

-spec set_sip_method(doc(), binary()) -> doc().
set_sip_method(Doc, SipMethod) ->
    kz_json:set_value([<<"sip">>, <<"method">>], SipMethod, Doc).

-spec sip_number(doc()) -> api_binary().
-spec sip_number(doc(), Default) -> binary() | Default.
sip_number(Doc) ->
    sip_number(Doc, 'undefined').
sip_number(Doc, Default) ->
    kz_json:get_binary_value([<<"sip">>, <<"number">>], Doc, Default).

-spec set_sip_number(doc(), binary()) -> doc().
set_sip_number(Doc, SipNumber) ->
    kz_json:set_value([<<"sip">>, <<"number">>], SipNumber, Doc).

-spec sip_password(doc()) -> api_ne_binary().
-spec sip_password(doc(), Default) -> ne_binary() | Default.
sip_password(Doc) ->
    sip_password(Doc, 'undefined').
sip_password(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"password">>], Doc, Default).

-spec set_sip_password(doc(), ne_binary()) -> doc().
set_sip_password(Doc, SipPassword) ->
    kz_json:set_value([<<"sip">>, <<"password">>], SipPassword, Doc).

-spec sip_realm(doc()) -> api_ne_binary().
-spec sip_realm(doc(), Default) -> ne_binary() | Default.
sip_realm(Doc) ->
    sip_realm(Doc, 'undefined').
sip_realm(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"realm">>], Doc, Default).

-spec set_sip_realm(doc(), ne_binary()) -> doc().
set_sip_realm(Doc, SipRealm) ->
    kz_json:set_value([<<"sip">>, <<"realm">>], SipRealm, Doc).

-spec sip_route(doc()) -> api_binary().
-spec sip_route(doc(), Default) -> binary() | Default.
sip_route(Doc) ->
    sip_route(Doc, 'undefined').
sip_route(Doc, Default) ->
    kz_json:get_binary_value([<<"sip">>, <<"route">>], Doc, Default).

-spec set_sip_route(doc(), binary()) -> doc().
set_sip_route(Doc, SipRoute) ->
    kz_json:set_value([<<"sip">>, <<"route">>], SipRoute, Doc).

-spec sip_static_route(doc()) -> api_binary().
-spec sip_static_route(doc(), Default) -> binary() | Default.
sip_static_route(Doc) ->
    sip_static_route(Doc, 'undefined').
sip_static_route(Doc, Default) ->
    kz_json:get_binary_value([<<"sip">>, <<"static_route">>], Doc, Default).

-spec set_sip_static_route(doc(), binary()) -> doc().
set_sip_static_route(Doc, SipStaticRoute) ->
    kz_json:set_value([<<"sip">>, <<"static_route">>], SipStaticRoute, Doc).

-spec sip_username(doc()) -> api_ne_binary().
-spec sip_username(doc(), Default) -> ne_binary() | Default.
sip_username(Doc) ->
    sip_username(Doc, 'undefined').
sip_username(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"sip">>, <<"username">>], Doc, Default).

-spec set_sip_username(doc(), ne_binary()) -> doc().
set_sip_username(Doc, SipUsername) ->
    kz_json:set_value([<<"sip">>, <<"username">>], SipUsername, Doc).

-spec suppress_unregister_notifications(doc()) -> boolean().
-spec suppress_unregister_notifications(doc(), Default) -> boolean() | Default.
suppress_unregister_notifications(Doc) ->
    suppress_unregister_notifications(Doc, false).
suppress_unregister_notifications(Doc, Default) ->
    kz_json:get_boolean_value(<<"suppress_unregister_notifications">>, Doc, Default).

-spec set_suppress_unregister_notifications(doc(), boolean()) -> doc().
set_suppress_unregister_notifications(Doc, SuppressUnregisterNotifications) ->
    kz_json:set_value(<<"suppress_unregister_notifications">>, SuppressUnregisterNotifications, Doc).

-spec timezone(doc()) -> api_binary().
-spec timezone(doc(), Default) -> binary() | Default.
timezone(Doc) ->
    timezone(Doc, 'undefined').
timezone(Doc, Default) ->
    kz_json:get_binary_value(<<"timezone">>, Doc, Default).

-spec set_timezone(doc(), binary()) -> doc().
set_timezone(Doc, Timezone) ->
    kz_json:set_value(<<"timezone">>, Timezone, Doc).
