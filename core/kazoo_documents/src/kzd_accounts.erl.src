-module(kzd_accounts).

-export([new/0]).
-export([call_recording/1, call_recording/2, set_call_recording/2]).
-export([call_recording_account/1, call_recording_account/2, set_call_recording_account/2]).
-export([call_recording_endpoint/1, call_recording_endpoint/2, set_call_recording_endpoint/2]).
-export([call_restriction/1, call_restriction/2, set_call_restriction/2]).
-export([call_waiting/1, call_waiting/2, set_call_waiting/2]).
-export([caller_id/1, caller_id/2, set_caller_id/2]).
-export([dial_plan/1, dial_plan/2, set_dial_plan/2]).
-export([do_not_disturb/1, do_not_disturb/2, set_do_not_disturb/2]).
-export([do_not_disturb_enabled/1, do_not_disturb_enabled/2, set_do_not_disturb_enabled/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([formatters/1, formatters/2, set_formatters/2]).
-export([language/1, language/2, set_language/2]).
-export([metaflows/1, metaflows/2, set_metaflows/2]).
-export([music_on_hold/1, music_on_hold/2, set_music_on_hold/2]).
-export([music_on_hold_media_id/1, music_on_hold_media_id/2, set_music_on_hold_media_id/2]).
-export([name/1, name/2, set_name/2]).
-export([org/1, org/2, set_org/2]).
-export([preflow/1, preflow/2, set_preflow/2]).
-export([preflow_always/1, preflow_always/2, set_preflow_always/2]).
-export([realm/1, realm/2, set_realm/2]).
-export([ringtones/1, ringtones/2, set_ringtones/2]).
-export([ringtones_external/1, ringtones_external/2, set_ringtones_external/2]).
-export([ringtones_internal/1, ringtones_internal/2, set_ringtones_internal/2]).
-export([timezone/1, timezone/2, set_timezone/2]).
-export([voicemail/1, voicemail/2, set_voicemail/2]).
-export([voicemail_notify/1, voicemail_notify/2, set_voicemail_notify/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec call_recording(doc()) -> api_object().
-spec call_recording(doc(), Default) -> kz_json:object() | Default.
call_recording(Doc) ->
    call_recording(Doc, 'undefined').
call_recording(Doc, Default) ->
    kz_json:get_json_value(<<"call_recording">>, Doc, Default).

-spec set_call_recording(doc(), kz_json:object()) -> doc().
set_call_recording(Doc, CallRecording) ->
    kz_json:set_value(<<"call_recording">>, CallRecording, Doc).

-spec call_recording_account(doc()) -> api_object().
-spec call_recording_account(doc(), Default) -> kz_json:object() | Default.
call_recording_account(Doc) ->
    call_recording_account(Doc, 'undefined').
call_recording_account(Doc, Default) ->
    kz_json:get_json_value([<<"call_recording">>, <<"account">>], Doc, Default).

-spec set_call_recording_account(doc(), kz_json:object()) -> doc().
set_call_recording_account(Doc, CallRecordingAccount) ->
    kz_json:set_value([<<"call_recording">>, <<"account">>], CallRecordingAccount, Doc).

-spec call_recording_endpoint(doc()) -> api_object().
-spec call_recording_endpoint(doc(), Default) -> kz_json:object() | Default.
call_recording_endpoint(Doc) ->
    call_recording_endpoint(Doc, 'undefined').
call_recording_endpoint(Doc, Default) ->
    kz_json:get_json_value([<<"call_recording">>, <<"endpoint">>], Doc, Default).

-spec set_call_recording_endpoint(doc(), kz_json:object()) -> doc().
set_call_recording_endpoint(Doc, CallRecordingEndpoint) ->
    kz_json:set_value([<<"call_recording">>, <<"endpoint">>], CallRecordingEndpoint, Doc).

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

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec org(doc()) -> api_binary().
-spec org(doc(), Default) -> binary() | Default.
org(Doc) ->
    org(Doc, 'undefined').
org(Doc, Default) ->
    kz_json:get_binary_value(<<"org">>, Doc, Default).

-spec set_org(doc(), binary()) -> doc().
set_org(Doc, Org) ->
    kz_json:set_value(<<"org">>, Org, Doc).

-spec preflow(doc()) -> kz_json:object().
-spec preflow(doc(), Default) -> kz_json:object() | Default.
preflow(Doc) ->
    preflow(Doc, kz_json:new()).
preflow(Doc, Default) ->
    kz_json:get_json_value(<<"preflow">>, Doc, Default).

-spec set_preflow(doc(), kz_json:object()) -> doc().
set_preflow(Doc, Preflow) ->
    kz_json:set_value(<<"preflow">>, Preflow, Doc).

-spec preflow_always(doc()) -> api_binary().
-spec preflow_always(doc(), Default) -> binary() | Default.
preflow_always(Doc) ->
    preflow_always(Doc, 'undefined').
preflow_always(Doc, Default) ->
    kz_json:get_binary_value([<<"preflow">>, <<"always">>], Doc, Default).

-spec set_preflow_always(doc(), binary()) -> doc().
set_preflow_always(Doc, PreflowAlways) ->
    kz_json:set_value([<<"preflow">>, <<"always">>], PreflowAlways, Doc).

-spec realm(doc()) -> api_ne_binary().
-spec realm(doc(), Default) -> ne_binary() | Default.
realm(Doc) ->
    realm(Doc, 'undefined').
realm(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"realm">>, Doc, Default).

-spec set_realm(doc(), ne_binary()) -> doc().
set_realm(Doc, Realm) ->
    kz_json:set_value(<<"realm">>, Realm, Doc).

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

-spec timezone(doc()) -> api_ne_binary().
-spec timezone(doc(), Default) -> ne_binary() | Default.
timezone(Doc) ->
    timezone(Doc, 'undefined').
timezone(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"timezone">>, Doc, Default).

-spec set_timezone(doc(), ne_binary()) -> doc().
set_timezone(Doc, Timezone) ->
    kz_json:set_value(<<"timezone">>, Timezone, Doc).

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
