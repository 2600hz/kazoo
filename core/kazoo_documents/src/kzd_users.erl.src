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
-export([voicemailnotify_callback/1, voicemailnotify_callback/2, set_voicemailnotify_callback/2]).


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

-spec dial_plan(doc()) -> api_object().
-spec dial_plan(doc(), Default) -> kz_json:object() | Default.
dial_plan(Doc) ->
    dial_plan(Doc, 'undefined').
dial_plan(Doc, Default) ->
    kz_json:get_json_value(<<"dial_plan">>, Doc, Default).

-spec set_dial_plan(doc(), kz_json:object()) -> doc().
set_dial_plan(Doc, DialPlan) ->
    kz_json:set_value(<<"dial_plan">>, DialPlan, Doc).

-spec directories(doc()) -> api_object().
-spec directories(doc(), Default) -> kz_json:object() | Default.
directories(Doc) ->
    directories(Doc, 'undefined').
directories(Doc, Default) ->
    kz_json:get_json_value(<<"directories">>, Doc, Default).

-spec set_directories(doc(), kz_json:object()) -> doc().
set_directories(Doc, Directories) ->
    kz_json:set_value(<<"directories">>, Directories, Doc).

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

-spec email(doc()) -> api_ne_binary().
-spec email(doc(), Default) -> ne_binary() | Default.
email(Doc) ->
    email(Doc, 'undefined').
email(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"email">>, Doc, Default).

-spec set_email(doc(), ne_binary()) -> doc().
set_email(Doc, Email) ->
    kz_json:set_value(<<"email">>, Email, Doc).

-spec enabled(doc()) -> boolean().
-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc) ->
    enabled(Doc, true).
enabled(Doc, Default) ->
    kz_json:get_boolean_value(<<"enabled">>, Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value(<<"enabled">>, Enabled, Doc).

-spec feature_level(doc()) -> api_binary().
-spec feature_level(doc(), Default) -> binary() | Default.
feature_level(Doc) ->
    feature_level(Doc, 'undefined').
feature_level(Doc, Default) ->
    kz_json:get_binary_value(<<"feature_level">>, Doc, Default).

-spec set_feature_level(doc(), binary()) -> doc().
set_feature_level(Doc, FeatureLevel) ->
    kz_json:set_value(<<"feature_level">>, FeatureLevel, Doc).

-spec first_name(doc()) -> api_ne_binary().
-spec first_name(doc(), Default) -> ne_binary() | Default.
first_name(Doc) ->
    first_name(Doc, 'undefined').
first_name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"first_name">>, Doc, Default).

-spec set_first_name(doc(), ne_binary()) -> doc().
set_first_name(Doc, FirstName) ->
    kz_json:set_value(<<"first_name">>, FirstName, Doc).

-spec formatters(doc()) -> api_object().
-spec formatters(doc(), Default) -> kz_json:object() | Default.
formatters(Doc) ->
    formatters(Doc, 'undefined').
formatters(Doc, Default) ->
    kz_json:get_json_value(<<"formatters">>, Doc, Default).

-spec set_formatters(doc(), kz_json:object()) -> doc().
set_formatters(Doc, Formatters) ->
    kz_json:set_value(<<"formatters">>, Formatters, Doc).

-spec hotdesk(doc()) -> kz_json:object().
-spec hotdesk(doc(), Default) -> kz_json:object() | Default.
hotdesk(Doc) ->
    hotdesk(Doc, kz_json:new()).
hotdesk(Doc, Default) ->
    kz_json:get_json_value(<<"hotdesk">>, Doc, Default).

-spec set_hotdesk(doc(), kz_json:object()) -> doc().
set_hotdesk(Doc, Hotdesk) ->
    kz_json:set_value(<<"hotdesk">>, Hotdesk, Doc).

-spec hotdesk_enabled(doc()) -> boolean().
-spec hotdesk_enabled(doc(), Default) -> boolean() | Default.
hotdesk_enabled(Doc) ->
    hotdesk_enabled(Doc, false).
hotdesk_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"hotdesk">>, <<"enabled">>], Doc, Default).

-spec set_hotdesk_enabled(doc(), boolean()) -> doc().
set_hotdesk_enabled(Doc, HotdeskEnabled) ->
    kz_json:set_value([<<"hotdesk">>, <<"enabled">>], HotdeskEnabled, Doc).

-spec hotdesk_id(doc()) -> api_binary().
-spec hotdesk_id(doc(), Default) -> binary() | Default.
hotdesk_id(Doc) ->
    hotdesk_id(Doc, 'undefined').
hotdesk_id(Doc, Default) ->
    kz_json:get_binary_value([<<"hotdesk">>, <<"id">>], Doc, Default).

-spec set_hotdesk_id(doc(), binary()) -> doc().
set_hotdesk_id(Doc, HotdeskId) ->
    kz_json:set_value([<<"hotdesk">>, <<"id">>], HotdeskId, Doc).

-spec hotdesk_keep_logged_in_elsewhere(doc()) -> boolean().
-spec hotdesk_keep_logged_in_elsewhere(doc(), Default) -> boolean() | Default.
hotdesk_keep_logged_in_elsewhere(Doc) ->
    hotdesk_keep_logged_in_elsewhere(Doc, false).
hotdesk_keep_logged_in_elsewhere(Doc, Default) ->
    kz_json:get_boolean_value([<<"hotdesk">>, <<"keep_logged_in_elsewhere">>], Doc, Default).

-spec set_hotdesk_keep_logged_in_elsewhere(doc(), boolean()) -> doc().
set_hotdesk_keep_logged_in_elsewhere(Doc, HotdeskKeepLoggedInElsewhere) ->
    kz_json:set_value([<<"hotdesk">>, <<"keep_logged_in_elsewhere">>], HotdeskKeepLoggedInElsewhere, Doc).

-spec hotdesk_pin(doc()) -> api_ne_binary().
-spec hotdesk_pin(doc(), Default) -> ne_binary() | Default.
hotdesk_pin(Doc) ->
    hotdesk_pin(Doc, 'undefined').
hotdesk_pin(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"hotdesk">>, <<"pin">>], Doc, Default).

-spec set_hotdesk_pin(doc(), ne_binary()) -> doc().
set_hotdesk_pin(Doc, HotdeskPin) ->
    kz_json:set_value([<<"hotdesk">>, <<"pin">>], HotdeskPin, Doc).

-spec hotdesk_require_pin(doc()) -> boolean().
-spec hotdesk_require_pin(doc(), Default) -> boolean() | Default.
hotdesk_require_pin(Doc) ->
    hotdesk_require_pin(Doc, false).
hotdesk_require_pin(Doc, Default) ->
    kz_json:get_boolean_value([<<"hotdesk">>, <<"require_pin">>], Doc, Default).

-spec set_hotdesk_require_pin(doc(), boolean()) -> doc().
set_hotdesk_require_pin(Doc, HotdeskRequirePin) ->
    kz_json:set_value([<<"hotdesk">>, <<"require_pin">>], HotdeskRequirePin, Doc).

-spec language(doc()) -> api_binary().
-spec language(doc(), Default) -> binary() | Default.
language(Doc) ->
    language(Doc, 'undefined').
language(Doc, Default) ->
    kz_json:get_binary_value(<<"language">>, Doc, Default).

-spec set_language(doc(), binary()) -> doc().
set_language(Doc, Language) ->
    kz_json:set_value(<<"language">>, Language, Doc).

-spec last_name(doc()) -> api_ne_binary().
-spec last_name(doc(), Default) -> ne_binary() | Default.
last_name(Doc) ->
    last_name(Doc, 'undefined').
last_name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"last_name">>, Doc, Default).

-spec set_last_name(doc(), ne_binary()) -> doc().
set_last_name(Doc, LastName) ->
    kz_json:set_value(<<"last_name">>, LastName, Doc).

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

-spec password(doc()) -> api_binary().
-spec password(doc(), Default) -> binary() | Default.
password(Doc) ->
    password(Doc, 'undefined').
password(Doc, Default) ->
    kz_json:get_binary_value(<<"password">>, Doc, Default).

-spec set_password(doc(), binary()) -> doc().
set_password(Doc, Password) ->
    kz_json:set_value(<<"password">>, Password, Doc).

-spec presence_id(doc()) -> api_binary().
-spec presence_id(doc(), Default) -> binary() | Default.
presence_id(Doc) ->
    presence_id(Doc, 'undefined').
presence_id(Doc, Default) ->
    kz_json:get_binary_value(<<"presence_id">>, Doc, Default).

-spec set_presence_id(doc(), binary()) -> doc().
set_presence_id(Doc, PresenceId) ->
    kz_json:set_value(<<"presence_id">>, PresenceId, Doc).

-spec priv_level(doc()) -> binary().
-spec priv_level(doc(), Default) -> binary() | Default.
priv_level(Doc) ->
    priv_level(Doc, <<"user">>).
priv_level(Doc, Default) ->
    kz_json:get_binary_value(<<"priv_level">>, Doc, Default).

-spec set_priv_level(doc(), binary()) -> doc().
set_priv_level(Doc, PrivLevel) ->
    kz_json:set_value(<<"priv_level">>, PrivLevel, Doc).

-spec profile(doc()) -> api_object().
-spec profile(doc(), Default) -> kz_json:object() | Default.
profile(Doc) ->
    profile(Doc, 'undefined').
profile(Doc, Default) ->
    kz_json:get_json_value(<<"profile">>, Doc, Default).

-spec set_profile(doc(), kz_json:object()) -> doc().
set_profile(Doc, Profile) ->
    kz_json:set_value(<<"profile">>, Profile, Doc).

-spec pronounced_name(doc()) -> api_object().
-spec pronounced_name(doc(), Default) -> kz_json:object() | Default.
pronounced_name(Doc) ->
    pronounced_name(Doc, 'undefined').
pronounced_name(Doc, Default) ->
    kz_json:get_json_value(<<"pronounced_name">>, Doc, Default).

-spec set_pronounced_name(doc(), kz_json:object()) -> doc().
set_pronounced_name(Doc, PronouncedName) ->
    kz_json:set_value(<<"pronounced_name">>, PronouncedName, Doc).

-spec pronounced_name_media_id(doc()) -> api_binary().
-spec pronounced_name_media_id(doc(), Default) -> binary() | Default.
pronounced_name_media_id(Doc) ->
    pronounced_name_media_id(Doc, 'undefined').
pronounced_name_media_id(Doc, Default) ->
    kz_json:get_binary_value([<<"pronounced_name">>, <<"media_id">>], Doc, Default).

-spec set_pronounced_name_media_id(doc(), binary()) -> doc().
set_pronounced_name_media_id(Doc, PronouncedNameMediaId) ->
    kz_json:set_value([<<"pronounced_name">>, <<"media_id">>], PronouncedNameMediaId, Doc).

-spec require_password_update(doc()) -> boolean().
-spec require_password_update(doc(), Default) -> boolean() | Default.
require_password_update(Doc) ->
    require_password_update(Doc, false).
require_password_update(Doc, Default) ->
    kz_json:get_boolean_value(<<"require_password_update">>, Doc, Default).

-spec set_require_password_update(doc(), boolean()) -> doc().
set_require_password_update(Doc, RequirePasswordUpdate) ->
    kz_json:set_value(<<"require_password_update">>, RequirePasswordUpdate, Doc).

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

-spec timezone(doc()) -> api_binary().
-spec timezone(doc(), Default) -> binary() | Default.
timezone(Doc) ->
    timezone(Doc, 'undefined').
timezone(Doc, Default) ->
    kz_json:get_binary_value(<<"timezone">>, Doc, Default).

-spec set_timezone(doc(), binary()) -> doc().
set_timezone(Doc, Timezone) ->
    kz_json:set_value(<<"timezone">>, Timezone, Doc).

-spec username(doc()) -> api_ne_binary().
-spec username(doc(), Default) -> ne_binary() | Default.
username(Doc) ->
    username(Doc, 'undefined').
username(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"username">>, Doc, Default).

-spec set_username(doc(), ne_binary()) -> doc().
set_username(Doc, Username) ->
    kz_json:set_value(<<"username">>, Username, Doc).

-spec verified(doc()) -> boolean().
-spec verified(doc(), Default) -> boolean() | Default.
verified(Doc) ->
    verified(Doc, false).
verified(Doc, Default) ->
    kz_json:get_boolean_value(<<"verified">>, Doc, Default).

-spec set_verified(doc(), boolean()) -> doc().
set_verified(Doc, Verified) ->
    kz_json:set_value(<<"verified">>, Verified, Doc).

-spec vm_to_email_enabled(doc()) -> boolean().
-spec vm_to_email_enabled(doc(), Default) -> boolean() | Default.
vm_to_email_enabled(Doc) ->
    vm_to_email_enabled(Doc, true).
vm_to_email_enabled(Doc, Default) ->
    kz_json:get_boolean_value(<<"vm_to_email_enabled">>, Doc, Default).

-spec set_vm_to_email_enabled(doc(), boolean()) -> doc().
set_vm_to_email_enabled(Doc, VmToEmailEnabled) ->
    kz_json:set_value(<<"vm_to_email_enabled">>, VmToEmailEnabled, Doc).

-spec voicemail(doc()) -> api_object().
-spec voicemail(doc(), Default) -> kz_json:object() | Default.
voicemail(Doc) ->
    voicemail(Doc, 'undefined').
voicemail(Doc, Default) ->
    kz_json:get_json_value(<<"voicemail">>, Doc, Default).

-spec set_voicemail(doc(), kz_json:object()) -> doc().
set_voicemail(Doc, Voicemail) ->
    kz_json:set_value(<<"voicemail">>, Voicemail, Doc).

-spec voicemail_notify(doc()) -> api_object().
-spec voicemail_notify(doc(), Default) -> kz_json:object() | Default.
voicemail_notify(Doc) ->
    voicemail_notify(Doc, 'undefined').
voicemail_notify(Doc, Default) ->
    kz_json:get_json_value([<<"voicemail">>, <<"notify">>], Doc, Default).

-spec set_voicemail_notify(doc(), kz_json:object()) -> doc().
set_voicemail_notify(Doc, VoicemailNotify) ->
    kz_json:set_value([<<"voicemail">>, <<"notify">>], VoicemailNotify, Doc).

-spec voicemailnotify_callback(doc()) -> api_object().
-spec voicemailnotify_callback(doc(), Default) -> kz_json:object() | Default.
voicemailnotify_callback(Doc) ->
    voicemailnotify_callback(Doc, 'undefined').
voicemailnotify_callback(Doc, Default) ->
    kz_json:get_json_value([[<<"voicemail">>, <<"notify">>], <<"callback">>], Doc, Default).

-spec set_voicemailnotify_callback(doc(), kz_json:object()) -> doc().
set_voicemailnotify_callback(Doc, VoicemailnotifyCallback) ->
    kz_json:set_value([[<<"voicemail">>, <<"notify">>], <<"callback">>], VoicemailnotifyCallback, Doc).
