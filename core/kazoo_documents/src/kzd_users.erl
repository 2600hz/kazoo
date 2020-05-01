%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_users).

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
-export([dial_plan/1, dial_plan/2, set_dial_plan/2]).
-export([directories/1, directories/2, set_directories/2]).
-export([do_not_disturb/1, do_not_disturb/2, set_do_not_disturb/2]).
-export([do_not_disturb_enabled/1, do_not_disturb_enabled/2, set_do_not_disturb_enabled/2]).
-export([email/1, email/2, set_email/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([feature_level/1, feature_level/2, set_feature_level/2]).
-export([first_name/1, first_name/2, set_first_name/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([formatters/1, formatters/2, set_formatters/2]).
-export([hotdesk/1, hotdesk/2, set_hotdesk/2]).
-export([hotdesk_enabled/1, hotdesk_enabled/2, set_hotdesk_enabled/2]).
-export([hotdesk_id/1, hotdesk_id/2, set_hotdesk_id/2]).
-export([hotdesk_keep_logged_in_elsewhere/1, hotdesk_keep_logged_in_elsewhere/2, set_hotdesk_keep_logged_in_elsewhere/2]).
-export([hotdesk_pin/1, hotdesk_pin/2, set_hotdesk_pin/2]).
-export([hotdesk_require_pin/1, hotdesk_require_pin/2, set_hotdesk_require_pin/2]).
-export([language/1, language/2, set_language/2]).
-export([last_name/1, last_name/2, set_last_name/2]).
-export([media/1, media/2, set_media/2]).
-export([metaflows/1, metaflows/2, set_metaflows/2]).
-export([music_on_hold/1, music_on_hold/2, set_music_on_hold/2]).
-export([music_on_hold_media_id/1, music_on_hold_media_id/2, set_music_on_hold_media_id/2]).
-export([password/1, password/2, set_password/2]).
-export([presence_id/1, presence_id/2, set_presence_id/2]).
-export([priv_level/1, priv_level/2, set_priv_level/2]).
-export([profile/1, profile/2, set_profile/2]).
-export([pronounced_name/1, pronounced_name/2, set_pronounced_name/2]).
-export([pronounced_name_media_id/1, pronounced_name_media_id/2, set_pronounced_name_media_id/2]).
-export([require_password_update/1, require_password_update/2, set_require_password_update/2]).
-export([ringtones/1, ringtones/2, set_ringtones/2]).
-export([ringtones_external/1, ringtones_external/2, set_ringtones_external/2]).
-export([ringtones_internal/1, ringtones_internal/2, set_ringtones_internal/2]).
-export([timezone/1, timezone/2, set_timezone/2]).
-export([username/1, username/2, set_username/2]).
-export([verified/1, verified/2, set_verified/2]).
-export([vm_to_email_enabled/1, vm_to_email_enabled/2, set_vm_to_email_enabled/2]).
-export([voicemail/1, voicemail/2, set_voicemail/2]).
-export([voicemail_notify/1, voicemail_notify/2, set_voicemail_notify/2]).
-export([voicemail_notify_callback/1, voicemail_notify_callback/2, set_voicemail_notify_callback/2]).
-export([md5_auth/1, sha1_auth/1, signature_secret/1]).


-export([fetch/2]).
-export([to_vcard/1]).
-export([enable/1, disable/1]).
-export([type/0]).
-export([fax_settings/1]).
-export([name/1]).
-export([is_account_admin/1, is_account_admin/2]).
-export([classifier_restriction/2, classifier_restriction/3, set_classifier_restriction/3]).
-export([full_name/1, full_name/2]).

-export([full_name/3]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-type docs() :: [doc()].
-export_type([doc/0, docs/0]).

-define(SCHEMA, <<"users">>).

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

-spec dial_plan(doc()) -> kz_term:api_object().
dial_plan(Doc) ->
    dial_plan(Doc, 'undefined').

-spec dial_plan(doc(), Default) -> kz_json:object() | Default.
dial_plan(Doc, Default) ->
    kz_json:get_json_value([<<"dial_plan">>], Doc, Default).

-spec set_dial_plan(doc(), kz_json:object()) -> doc().
set_dial_plan(Doc, DialPlan) ->
    kz_json:set_value([<<"dial_plan">>], DialPlan, Doc).

-spec directories(doc()) -> kz_term:api_object().
directories(Doc) ->
    directories(Doc, 'undefined').

-spec directories(doc(), Default) -> kz_json:object() | Default.
directories(Doc, Default) ->
    kz_json:get_json_value([<<"directories">>], Doc, Default).

-spec set_directories(doc(), kz_json:object()) -> doc().
set_directories(Doc, Directories) ->
    kz_json:set_value([<<"directories">>], Directories, Doc).

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

-spec email(doc()) -> kz_term:api_ne_binary().
email(Doc) ->
    email(Doc, 'undefined').

-spec email(doc(), Default) -> kz_term:ne_binary() | Default.
email(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"email">>], Doc, Default).

-spec set_email(doc(), kz_term:ne_binary()) -> doc().
set_email(Doc, Email) ->
    kz_json:set_value([<<"email">>], Email, Doc).

-spec enabled(doc()) -> boolean().
enabled(Doc) ->
    enabled(Doc, true).

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec feature_level(doc()) -> kz_term:api_binary().
feature_level(Doc) ->
    feature_level(Doc, 'undefined').

-spec feature_level(doc(), Default) -> binary() | Default.
feature_level(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"feature_level">>], Doc, Default).

-spec set_feature_level(doc(), binary()) -> doc().
set_feature_level(Doc, FeatureLevel) ->
    kz_json:set_value([<<"feature_level">>], FeatureLevel, Doc).

-spec first_name(doc()) -> kz_term:api_ne_binary().
first_name(Doc) ->
    first_name(Doc, 'undefined').

-spec first_name(doc(), Default) -> kz_term:ne_binary() | Default.
first_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"first_name">>], Doc, Default).

-spec set_first_name(doc(), kz_term:ne_binary()) -> doc().
set_first_name(Doc, FirstName) ->
    kz_json:set_value([<<"first_name">>], FirstName, Doc).

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

-spec hotdesk(doc()) -> kz_json:object().
hotdesk(Doc) ->
    hotdesk(Doc, kz_json:new()).

-spec hotdesk(doc(), Default) -> kz_json:object() | Default.
hotdesk(Doc, Default) ->
    kz_json:get_json_value([<<"hotdesk">>], Doc, Default).

-spec set_hotdesk(doc(), kz_json:object()) -> doc().
set_hotdesk(Doc, Hotdesk) ->
    kz_json:set_value([<<"hotdesk">>], Hotdesk, Doc).

-spec hotdesk_enabled(doc()) -> boolean().
hotdesk_enabled(Doc) ->
    hotdesk_enabled(Doc, false).

-spec hotdesk_enabled(doc(), Default) -> boolean() | Default.
hotdesk_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"hotdesk">>, <<"enabled">>], Doc, Default).

-spec set_hotdesk_enabled(doc(), boolean()) -> doc().
set_hotdesk_enabled(Doc, HotdeskEnabled) ->
    kz_json:set_value([<<"hotdesk">>, <<"enabled">>], HotdeskEnabled, Doc).

-spec hotdesk_id(doc()) -> kz_term:api_binary().
hotdesk_id(Doc) ->
    hotdesk_id(Doc, 'undefined').

-spec hotdesk_id(doc(), Default) -> binary() | Default.
hotdesk_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"hotdesk">>, <<"id">>], Doc, Default).

-spec set_hotdesk_id(doc(), binary()) -> doc().
set_hotdesk_id(Doc, HotdeskId) ->
    kz_json:set_value([<<"hotdesk">>, <<"id">>], HotdeskId, Doc).

-spec hotdesk_keep_logged_in_elsewhere(doc()) -> boolean().
hotdesk_keep_logged_in_elsewhere(Doc) ->
    hotdesk_keep_logged_in_elsewhere(Doc, false).

-spec hotdesk_keep_logged_in_elsewhere(doc(), Default) -> boolean() | Default.
hotdesk_keep_logged_in_elsewhere(Doc, Default) ->
    kz_json:get_boolean_value([<<"hotdesk">>, <<"keep_logged_in_elsewhere">>], Doc, Default).

-spec set_hotdesk_keep_logged_in_elsewhere(doc(), boolean()) -> doc().
set_hotdesk_keep_logged_in_elsewhere(Doc, HotdeskKeepLoggedInElsewhere) ->
    kz_json:set_value([<<"hotdesk">>, <<"keep_logged_in_elsewhere">>], HotdeskKeepLoggedInElsewhere, Doc).

-spec hotdesk_pin(doc()) -> kz_term:api_ne_binary().
hotdesk_pin(Doc) ->
    hotdesk_pin(Doc, 'undefined').

-spec hotdesk_pin(doc(), Default) -> kz_term:ne_binary() | Default.
hotdesk_pin(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"hotdesk">>, <<"pin">>], Doc, Default).

-spec set_hotdesk_pin(doc(), kz_term:ne_binary()) -> doc().
set_hotdesk_pin(Doc, HotdeskPin) ->
    kz_json:set_value([<<"hotdesk">>, <<"pin">>], HotdeskPin, Doc).

-spec hotdesk_require_pin(doc()) -> boolean().
hotdesk_require_pin(Doc) ->
    hotdesk_require_pin(Doc, false).

-spec hotdesk_require_pin(doc(), Default) -> boolean() | Default.
hotdesk_require_pin(Doc, Default) ->
    kz_json:get_boolean_value([<<"hotdesk">>, <<"require_pin">>], Doc, Default).

-spec set_hotdesk_require_pin(doc(), boolean()) -> doc().
set_hotdesk_require_pin(Doc, HotdeskRequirePin) ->
    kz_json:set_value([<<"hotdesk">>, <<"require_pin">>], HotdeskRequirePin, Doc).

-spec language(doc()) -> kz_term:api_binary().
language(Doc) ->
    language(Doc, 'undefined').

-spec language(doc(), Default) -> binary() | Default.
language(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"language">>], Doc, Default).

-spec set_language(doc(), binary()) -> doc().
set_language(Doc, Language) ->
    kz_json:set_value([<<"language">>], Language, Doc).

-spec last_name(doc()) -> kz_term:api_ne_binary().
last_name(Doc) ->
    last_name(Doc, 'undefined').

-spec last_name(doc(), Default) -> kz_term:ne_binary() | Default.
last_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"last_name">>], Doc, Default).

-spec set_last_name(doc(), kz_term:ne_binary()) -> doc().
set_last_name(Doc, LastName) ->
    kz_json:set_value([<<"last_name">>], LastName, Doc).

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

-spec password(doc()) -> kz_term:api_binary().
password(Doc) ->
    password(Doc, 'undefined').

-spec password(doc(), Default) -> binary() | Default.
password(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"password">>], Doc, Default).

-spec set_password(doc(), binary()) -> doc().
set_password(Doc, Password) ->
    kz_json:set_value([<<"password">>], Password, Doc).

-spec presence_id(doc()) -> kz_term:api_binary().
presence_id(Doc) ->
    presence_id(Doc, 'undefined').

-spec presence_id(doc(), Default) -> binary() | Default.
presence_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"presence_id">>], Doc, Default).

-spec set_presence_id(doc(), binary()) -> doc().
set_presence_id(Doc, PresenceId) ->
    kz_json:set_value([<<"presence_id">>], PresenceId, Doc).

-spec priv_level(doc()) -> binary().
priv_level(Doc) ->
    priv_level(Doc, <<"user">>).

-spec priv_level(doc(), Default) -> binary() | Default.
priv_level(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"priv_level">>], Doc, Default).

-spec set_priv_level(doc(), binary()) -> doc().
set_priv_level(Doc, PrivLevel) ->
    kz_json:set_value([<<"priv_level">>], PrivLevel, Doc).

-spec profile(doc()) -> kz_term:api_object().
profile(Doc) ->
    profile(Doc, 'undefined').

-spec profile(doc(), Default) -> kz_json:object() | Default.
profile(Doc, Default) ->
    kz_json:get_json_value([<<"profile">>], Doc, Default).

-spec set_profile(doc(), kz_json:object()) -> doc().
set_profile(Doc, Profile) ->
    kz_json:set_value([<<"profile">>], Profile, Doc).

-spec pronounced_name(doc()) -> kz_term:api_object().
pronounced_name(Doc) ->
    pronounced_name(Doc, 'undefined').

-spec pronounced_name(doc(), Default) -> kz_json:object() | Default.
pronounced_name(Doc, Default) ->
    kz_json:get_json_value([<<"pronounced_name">>], Doc, Default).

-spec set_pronounced_name(doc(), kz_json:object()) -> doc().
set_pronounced_name(Doc, PronouncedName) ->
    kz_json:set_value([<<"pronounced_name">>], PronouncedName, Doc).

-spec pronounced_name_media_id(doc()) -> kz_term:api_binary().
pronounced_name_media_id(Doc) ->
    pronounced_name_media_id(Doc, 'undefined').

-spec pronounced_name_media_id(doc(), Default) -> binary() | Default.
pronounced_name_media_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"pronounced_name">>, <<"media_id">>], Doc, Default).

-spec set_pronounced_name_media_id(doc(), binary()) -> doc().
set_pronounced_name_media_id(Doc, PronouncedNameMediaId) ->
    kz_json:set_value([<<"pronounced_name">>, <<"media_id">>], PronouncedNameMediaId, Doc).

-spec require_password_update(doc()) -> boolean().
require_password_update(Doc) ->
    require_password_update(Doc, false).

-spec require_password_update(doc(), Default) -> boolean() | Default.
require_password_update(Doc, Default) ->
    kz_json:get_boolean_value([<<"require_password_update">>], Doc, Default).

-spec set_require_password_update(doc(), boolean()) -> doc().
set_require_password_update(Doc, RequirePasswordUpdate) ->
    kz_json:set_value([<<"require_password_update">>], RequirePasswordUpdate, Doc).

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

-spec timezone(doc()) -> kz_term:api_binary().
timezone(Doc) ->
    timezone(Doc, 'undefined').

-spec timezone(doc(), Default) -> binary() | Default.
-ifndef(TEST).
timezone(Doc, Default) ->
    case kz_json:get_ne_binary_value([<<"timezone">>], Doc, Default) of
        'undefined' -> kzd_accounts:timezone(kz_doc:account_id(Doc));
        <<"inherit">> -> kzd_accounts:timezone(kz_doc:account_id(Doc)); %% UI-1808
        TZ -> TZ
    end.
-else.
timezone(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"timezone">>], Doc, Default).
-endif.

-spec set_timezone(doc(), binary()) -> doc().
set_timezone(Doc, Timezone) ->
    kz_json:set_value([<<"timezone">>], Timezone, Doc).

-spec username(doc()) -> kz_term:api_ne_binary().
username(Doc) ->
    username(Doc, 'undefined').

-spec username(doc(), Default) -> kz_term:ne_binary() | Default.
username(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"username">>], Doc, Default).

-spec set_username(doc(), kz_term:ne_binary()) -> doc().
set_username(Doc, Username) ->
    kz_json:set_value([<<"username">>], Username, Doc).

-spec verified(doc()) -> boolean().
verified(Doc) ->
    verified(Doc, false).

-spec verified(doc(), Default) -> boolean() | Default.
verified(Doc, Default) ->
    kz_json:get_boolean_value([<<"verified">>], Doc, Default).

-spec set_verified(doc(), boolean()) -> doc().
set_verified(Doc, Verified) ->
    kz_json:set_value([<<"verified">>], Verified, Doc).

-spec vm_to_email_enabled(doc()) -> boolean().
vm_to_email_enabled(Doc) ->
    vm_to_email_enabled(Doc, true).

-spec vm_to_email_enabled(doc(), Default) -> boolean() | Default.
vm_to_email_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"vm_to_email_enabled">>], Doc, Default).

-spec set_vm_to_email_enabled(doc(), boolean()) -> doc().
set_vm_to_email_enabled(Doc, VmToEmailEnabled) ->
    kz_json:set_value([<<"vm_to_email_enabled">>], VmToEmailEnabled, Doc).

-spec voicemail(doc()) -> kz_term:api_object().
voicemail(Doc) ->
    voicemail(Doc, 'undefined').

-spec voicemail(doc(), Default) -> kz_json:object() | Default.
voicemail(Doc, Default) ->
    kz_json:get_json_value([<<"voicemail">>], Doc, Default).

-spec set_voicemail(doc(), kz_json:object()) -> doc().
set_voicemail(Doc, Voicemail) ->
    kz_json:set_value([<<"voicemail">>], Voicemail, Doc).

-spec voicemail_notify(doc()) -> kz_term:api_object().
voicemail_notify(Doc) ->
    voicemail_notify(Doc, 'undefined').

-spec voicemail_notify(doc(), Default) -> kz_json:object() | Default.
voicemail_notify(Doc, Default) ->
    kz_json:get_json_value([<<"voicemail">>, <<"notify">>], Doc, Default).

-spec set_voicemail_notify(doc(), kz_json:object()) -> doc().
set_voicemail_notify(Doc, VoicemailNotify) ->
    kz_json:set_value([<<"voicemail">>, <<"notify">>], VoicemailNotify, Doc).

-spec voicemail_notify_callback(doc()) -> kz_term:api_object().
voicemail_notify_callback(Doc) ->
    voicemail_notify_callback(Doc, 'undefined').

-spec voicemail_notify_callback(doc(), Default) -> kz_json:object() | Default.
voicemail_notify_callback(Doc, Default) ->
    kz_json:get_json_value([<<"voicemail">>, <<"notify">>, <<"callback">>], Doc, Default).

-spec set_voicemail_notify_callback(doc(), kz_json:object()) -> doc().
set_voicemail_notify_callback(Doc, VoicemailNotifyCallback) ->
    kz_json:set_value([<<"voicemail">>, <<"notify">>, <<"callback">>], VoicemailNotifyCallback, Doc).

-spec fetch(kz_term:api_ne_binary(), kz_term:api_ne_binary()) ->
          {'ok', doc()} |
          kz_datamgr:data_error().
fetch('undefined', _UserId) ->
    {'error', 'invalid_db_name'};
fetch(_Account, 'undefined') ->
    {'error', 'not_found'};
fetch(Account, UserId=?NE_BINARY) ->
    AccountDb = kzs_util:format_account_db(Account),
    kz_datamgr:open_cache_doc(AccountDb, UserId).

-spec to_vcard(doc()) -> binary().
to_vcard(User) ->
    %% TODO add SOUND, AGENT (X-ASSISTANT), X-MANAGER
    Fields = [<<"BEGIN">>
             ,<<"VERSION">>
             ,<<"FN">>
             ,<<"N">>
             ,<<"ORG">>
             ,<<"PHOTO">>
             ,<<"EMAIL">>
             ,<<"BDAY">>
             ,<<"NOTE">>
             ,<<"TITLE">>
             ,<<"ROLE">>
             ,<<"TZ">>
             ,<<"NICKNAME">>
             ,<<"TEL">>
             ,<<"ADR">>
             ,<<"END">>
             ],
    NotEmptyFields = lists:foldl(fun vcard_fields_acc/2
                                ,[]
                                ,[card_field(Key, User) || Key <- Fields]
                                ),
    PackedFields = lists:reverse(
                     [kz_binary:join([X, Y], <<":">>) ||
                         {X, Y} <- NotEmptyFields
                     ]
                    ),
    DividedFields = lists:reverse(
                      lists:foldl(fun vcard_field_divide_by_length/2, [], PackedFields)
                     ),
    kz_binary:join(DividedFields, <<"\n">>).

-spec vcard_escape_chars(binary()) -> binary().
vcard_escape_chars(Val) ->
    Opts = ['global', {'return', 'binary'}],
    Val1 = re:replace(Val, "(:|;|,)", "\\\\&", Opts),
    re:replace(Val1, "\n", "\\\\n", Opts).

-spec vcard_fields_acc(vcard_field(), [{kz_term:ne_binary(), binary()}]) -> [{kz_term:ne_binary(), binary()}].
vcard_fields_acc({_, Val}, Acc)
  when Val =:= 'undefined'; Val =:= []; Val =:= <<>> ->
    Acc;
vcard_fields_acc({Type, Val}, Acc) ->
    case vcard_normalize_val(Val) of
        <<>> -> Acc;
        ValN ->
            TypeN = vcard_normalize_type(Type),
            [{TypeN, ValN} | Acc]
    end;
vcard_fields_acc([X | Rest], Acc) ->
    vcard_fields_acc(Rest, vcard_fields_acc(X, Acc));
vcard_fields_acc([], Acc) ->
    Acc.

-spec vcard_normalize_val(binary() | {char(), kz_term:binaries()}) -> binary().
vcard_normalize_val({Separator, Vals}) when is_list(Vals) ->
    kz_binary:join([vcard_escape_chars(X) || X <- Vals, not kz_term:is_empty(X)], Separator);
vcard_normalize_val(Val) when is_binary(Val) ->
    vcard_escape_chars(Val).

-spec vcard_normalize_type(list() | {kz_term:ne_binary(), kz_term:ne_binary()} | kz_term:ne_binary()) -> kz_term:ne_binary().
vcard_normalize_type(T) when is_list(T) -> kz_binary:join([vcard_normalize_type(X) || X <- T], <<";">>);
vcard_normalize_type({T, V}) -> kz_binary:join([T, V], <<"=">>);
vcard_normalize_type(T) -> T.

-type vcard_val() :: binary() | {char(), kz_term:binaries()} | 'undefined'.
-type vcard_type_token() :: kz_term:ne_binary() | {kz_term:ne_binary(), kz_term:ne_binary()}.
-type vcard_type() :: [vcard_type_token()] | vcard_type_token().
-type vcard_field_token() :: {vcard_type(), vcard_val()}.
-type vcard_field() :: vcard_field_token() | [vcard_field_token()].

-spec card_field(kz_term:ne_binary(), doc()) -> vcard_field().
card_field(Key = <<"BEGIN">>, _User) ->
    {Key, <<"VCARD">>};
card_field(Key = <<"VERSION">>, _User) ->
    {Key, <<"3.0">>};
card_field(Key = <<"END">>, _User) ->
    {Key, <<"VCARD">>};
card_field(Key = <<"FN">>, User) ->
    FirstName = first_name(User),
    LastName = last_name(User),
    MiddleName = kz_json:get_ne_binary_value(<<"middle_name">>, User),
    {Key
    ,kz_binary:join([X || X <- [FirstName, MiddleName, LastName],
                          not kz_term:is_empty(X)
                    ]
                   ,<<" ">>
                   )
    };
card_field(Key = <<"N">>, User) ->
    FirstName = first_name(User),
    LastName = last_name(User),
    MiddleName = kz_json:get_ne_binary_value(<<"middle_name">>, User),
    {Key, {$;, [LastName, FirstName, MiddleName]}};
card_field(Key = <<"ORG">>, User) ->
    {Key, kz_json:get_value(<<"org">>, User)};
card_field(Key = <<"PHOTO">>, User) ->
    case kz_json:get_json_value(<<"photo">>, User) of
        'undefined' -> {Key, 'undefined'};
        PhotoJObj ->
            [{CT, PhotoBin}] = kz_json:to_proplist(PhotoJObj),
            TypeType = content_type_to_type(CT),
            Data = base64:encode(PhotoBin),
            {[Key, {<<"ENCODING">>, <<"B">>}, {<<"TYPE">>, TypeType}], Data}
    end;
card_field(Key = <<"ADR">>, User) ->
    Addresses = [normalize_address(A) || A <- kz_json:get_list_value(<<"addresses">>, User, [])],
    [{[Key, {<<"TYPE">>, Type}], Address} || {Type, Address} <- Addresses];
card_field(Key = <<"TEL">>, User) ->
    CallerId = caller_id(User, kz_json:new()),
    Internal = kz_json:get_ne_binary_value(<<"internal">>, CallerId),
    External = kz_json:get_ne_binary_value(<<"external">>, CallerId),

    [{Key, Internal}
    ,{Key, External}
    ];
card_field(Key = <<"EMAIL">>, User) ->
    {Key, email(User)};
card_field(Key = <<"BDAY">>, User) ->
    {Key, kz_json:get_value(<<"birthday">>, User)};
card_field(Key = <<"NOTE">>, User) ->
    {Key, kz_json:get_value(<<"note">>, User)};
card_field(Key = <<"TITLE">>, User) ->
    {Key, kz_json:get_value(<<"title">>, User)};
card_field(Key = <<"ROLE">>, User) ->
    {Key, kz_json:get_value(<<"role">>, User)};
card_field(Key = <<"TZ">>, User) ->
    {Key, timezone(User)};
card_field(Key = <<"NICKNAME">>, User) ->
    {Key, {$,, kz_json:get_list_value(<<"nicknames">>, User, [])}}.

-spec content_type_to_type(kz_term:ne_binary()) -> kz_term:ne_binary().
content_type_to_type(<<"image/jpeg">>) -> <<"JPEG">>.

-spec vcard_field_divide_by_length(binary(), kz_term:binaries()) -> kz_term:binaries().
vcard_field_divide_by_length(<<Row:75/binary>>, Acc) ->
    [Row | Acc];
vcard_field_divide_by_length(<<Row:75/binary, Rest/binary>>, Acc) ->
    vcard_field_divide_by_length(Rest, [Row | Acc]);
vcard_field_divide_by_length(Row, Acc) ->
    [Row | Acc].

-spec normalize_address(kz_json:object()) -> {kz_term:ne_binary(), binary()}.
normalize_address(AddressJObj) ->
    Types = case kz_json:get_list_value(<<"types">>, AddressJObj) of
                'undefined' -> [<<"intl">>, <<"postal">>, <<"parcel">>, <<"work">>];
                T -> T
            end,
    Address = kz_json:get_ne_binary_value(<<"address">>, AddressJObj),
    {kz_binary:join(Types, <<",">>), Address}.

-spec enable(doc()) -> doc().
enable(JObj) ->
    kz_json:set_value([<<"enabled">>], 'true', JObj).

-spec disable(doc()) -> doc().
disable(JObj) ->
    kz_json:set_value([<<"enabled">>], 'false', JObj).

-spec type() -> kz_term:ne_binary().
type() -> <<"user">>.

-spec fax_settings(doc()) -> doc().
fax_settings(Doc) ->
    FaxSettings = kz_json:get_json_value(?FAX_SETTINGS_KEY, Doc, kz_json:new()),
    UserFaxSettings =
        case kz_json:get_ne_binary_value(?FAX_TIMEZONE_KEY, FaxSettings) of
            'undefined' -> kz_json:set_value(?FAX_TIMEZONE_KEY, timezone(Doc), FaxSettings);
            _TZ -> FaxSettings
        end,
    AccountFaxSettings = kzd_accounts:fax_settings(kz_doc:account_id(Doc)),
    kz_json:merge_jobjs(UserFaxSettings, AccountFaxSettings).

-spec name(doc()) -> kz_term:ne_binary().
name(Doc) ->
    list_to_binary([first_name(Doc, <<>>), " ", last_name(Doc, <<>>)]).

-spec is_account_admin(kz_term:api_object()) -> boolean().
is_account_admin('undefined') -> 'false';
is_account_admin(Doc) ->
    priv_level(Doc) =:= <<"admin">>.

-spec is_account_admin(kz_term:api_binary(), kz_term:api_binary()) -> boolean().
is_account_admin('undefined', _) -> 'false';
is_account_admin(_, 'undefined') -> 'false';
is_account_admin(Account, UserId) ->
    case fetch(Account, UserId) of
        {'ok', JObj} -> is_account_admin(JObj);
        {'error', _R} ->
            lager:debug("unable to open user ~s definition in account ~s: ~p", [UserId, Account, _R]),
            'false'
    end.

-spec classifier_restriction(doc(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
classifier_restriction(Doc, Classifier) ->
    classifier_restriction(Doc, Classifier, 'undefined').

-spec classifier_restriction(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binary() | Default.
classifier_restriction(Doc, Classifier, Default) ->
    Restrictions = call_restriction(Doc),
    kz_json:get_ne_binary_value([Classifier, <<"action">>], Restrictions, Default).

-spec set_classifier_restriction(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> doc().
set_classifier_restriction(Doc, Classifier, Action) ->
    Restrictions = call_restriction(Doc),
    set_call_restriction(Doc
                        ,kz_json:set_value([Classifier, <<"action">>], Action, Restrictions)
                        ).

-spec full_name(kz_json:object()) -> kz_term:api_ne_binary().
full_name(Doc) ->
    full_name(Doc, 'undefined').

-spec full_name(kz_json:object(), Default) -> kz_term:api_ne_binary() | Default.
full_name(Doc, Default) ->
    full_name(first_name(Doc), last_name(Doc), Default).

-spec full_name(kz_term:api_binary(), kz_term:api_binary(), Default) -> kz_term:ne_binary() | Default.
full_name(?NE_BINARY = First, ?NE_BINARY = Last, _) ->
    <<First/binary, " ", Last/binary>>;
full_name(_, ?NE_BINARY = Last, _) ->
    <<Last/binary>>;
full_name(?NE_BINARY = First, _, _) ->
    <<First/binary>>;
full_name(_, _, Default) ->
    Default.

-spec md5_auth(doc()) -> kz_term:api_object().
md5_auth(Doc) ->
    kz_json:get_ne_binary_value(<<"pvt_md5_auth">>, Doc).

-spec sha1_auth(doc()) -> kz_term:api_object().
sha1_auth(Doc) ->
    kz_json:get_ne_binary_value(<<"pvt_sha1_auth">>, Doc).

-spec signature_secret(doc()) -> kz_term:api_object().
signature_secret(Doc) ->
    kz_json:get_ne_binary_value(<<"pvt_signature_secret">>, Doc).
