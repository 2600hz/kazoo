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
-export([notifications/1, notifications/2, set_notifications/2]).
-export([notification/2, notification/3, set_notification/3]).
-export([org/1, org/2, set_org/2]).
-export([preflow/1, preflow/2, set_preflow/2]).
-export([preflow_always/1, preflow_always/2, set_preflow_always/2]).
-export([realm/1, realm/2, set_realm/2]).
-export([ringtones/1, ringtones/2, set_ringtones/2]).
-export([ringtones_external/1, ringtones_external/2, set_ringtones_external/2]).
-export([ringtones_internal/1, ringtones_internal/2, set_ringtones_internal/2]).
-export([timezone/1, timezone/2, set_timezone/2]).
-export([topup/1, topup/2, set_topup/2]).
-export([topup_threshold/1, topup_threshold/2, set_topup_threshold/2]).
-export([voicemail/1, voicemail/2, set_voicemail/2]).
-export([voicemail_notify/1, voicemail_notify/2, set_voicemail_notify/2]).
-export([voicemail_notify_callback/1, voicemail_notify_callback/2, set_voicemail_notify_callback/2]).
-export([zones/1, zones/2, set_zones/2]).
-export([zones_home/1, zones_home/2, set_zones_home/2]).


-export([type/0
        ,fetch/1, fetch/2
        ,fetch_name/1, fetch_realm/1

        ,api_key/1, set_api_key/2
        ,is_enabled/1, enable/1, disable/1
        ,is_expired/1

        ,tree/1, tree/2, set_tree/2
        ,default_timezone/0
        ,notification_preference/1, set_notification_preference/2
        ,allow_number_additions/1, set_allow_number_additions/2
        ,is_superduper_admin/1, set_superduper_admin/2

        ,trial_expiration/1, trial_expiration/2, set_trial_expiration/2
        ,trial_time_left/1, trial_time_left/2
        ,trial_has_expired/2

        ,demote/1, promote/1

        ,fax_settings/1
        ,get_inherited_value/3
        ,get_parent_account_id/1

        ,reseller_id/1, set_reseller_id/2, is_reseller/1
        ,is_trial_account/1
        ,low_balance_enabled/1, low_balance_enabled_exists/1, set_low_balance_enabled/1, reset_low_balance_enabled/1

        ,low_balance_sent/1, reset_low_balance_sent/1


        ,low_balance_threshold/1, set_low_balance_threshold/2
        ,low_balance_tstamp/1, remove_low_balance_tstamp/1
        ,parent_account_id/1
        ,preflow_id/1

        ,home_zone/1, home_zone/2, set_home_zone/2

        ,sent_initial_call/1
        ,sent_initial_registration/1
        ,set_initial_call_sent/2
        ,set_initial_registration_sent/2
        ,set_low_balance_sent/1
        ,set_low_balance_tstamp/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json_schema:default_object(?MODULE_STRING), type()).

-spec call_recording(doc()) -> kz_term:api_object().
call_recording(Doc) ->
    call_recording(Doc, 'undefined').

-spec call_recording(doc(), Default) -> kz_json:object() | Default.
call_recording(Doc, Default) ->
    kz_json:get_json_value([<<"call_recording">>], Doc, Default).

-spec set_call_recording(doc(), kz_json:object()) -> doc().
set_call_recording(Doc, CallRecording) ->
    kz_json:set_value([<<"call_recording">>], CallRecording, Doc).

-spec call_recording_account(doc()) -> kz_term:api_object().
call_recording_account(Doc) ->
    call_recording_account(Doc, 'undefined').

-spec call_recording_account(doc(), Default) -> kz_json:object() | Default.
call_recording_account(Doc, Default) ->
    kz_json:get_json_value([<<"call_recording">>, <<"account">>], Doc, Default).

-spec set_call_recording_account(doc(), kz_json:object()) -> doc().
set_call_recording_account(Doc, CallRecordingAccount) ->
    kz_json:set_value([<<"call_recording">>, <<"account">>], CallRecordingAccount, Doc).

-spec call_recording_endpoint(doc()) -> kz_term:api_object().
call_recording_endpoint(Doc) ->
    call_recording_endpoint(Doc, 'undefined').

-spec call_recording_endpoint(doc(), Default) -> kz_json:object() | Default.
call_recording_endpoint(Doc, Default) ->
    kz_json:get_json_value([<<"call_recording">>, <<"endpoint">>], Doc, Default).

-spec set_call_recording_endpoint(doc(), kz_json:object()) -> doc().
set_call_recording_endpoint(Doc, CallRecordingEndpoint) ->
    kz_json:set_value([<<"call_recording">>, <<"endpoint">>], CallRecordingEndpoint, Doc).

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

-spec formatters(doc()) -> kz_term:api_object().
formatters(Doc) ->
    formatters(Doc, 'undefined').

-spec formatters(doc(), Default) -> kz_json:object() | Default.
formatters(Doc, Default) ->
    kz_json:get_json_value([<<"formatters">>], Doc, Default).

-spec set_formatters(doc(), kz_json:object()) -> doc().
set_formatters(Doc, Formatters) ->
    kz_json:set_value([<<"formatters">>], Formatters, Doc).

-spec language(doc()) -> kz_term:api_binary().
language(Doc) ->
    language(Doc, 'undefined').

-spec language(doc(), Default) -> binary() | Default.
language(Doc, Default) ->
    kz_json:get_binary_value([<<"language">>], Doc, Default).

-spec set_language(doc(), binary()) -> doc().
set_language(Doc, Language) ->
    kz_json:set_value([<<"language">>], Language, Doc).

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
    kz_json:get_binary_value([<<"music_on_hold">>, <<"media_id">>], Doc, Default).

-spec set_music_on_hold_media_id(doc(), binary()) -> doc().
set_music_on_hold_media_id(Doc, MusicOnHoldMediaId) ->
    kz_json:set_value([<<"music_on_hold">>, <<"media_id">>], MusicOnHoldMediaId, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec notifications(doc()) -> kz_term:api_object().
notifications(Doc) ->
    notifications(Doc, 'undefined').

-spec notifications(doc(), Default) -> kz_json:object() | Default.
notifications(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>], Doc, Default).

-spec set_notifications(doc(), kz_json:object()) -> doc().
set_notifications(Doc, Notifications) ->
    kz_json:set_value([<<"notifications">>], Notifications, Doc).

-spec notification(doc(), kz_json:key()) -> kz_term:api_object().
notification(Doc, Notification) ->
    notification(Doc, Notification, 'undefined').

-spec notification(doc(), kz_json:key(), Default) -> kz_json:object() | Default.
notification(Doc, Notification, Default) ->
    kz_json:get_json_value([<<"notifications">>, Notification], Doc, Default).

-spec set_notification(doc(), kz_json:key(), kz_json:object()) -> doc().
set_notification(Doc, Notification, Value) ->
    kz_json:set_value([<<"notifications">>, Notification], Value, Doc).

-spec org(doc()) -> kz_term:api_binary().
org(Doc) ->
    org(Doc, 'undefined').

-spec org(doc(), Default) -> binary() | Default.
org(Doc, Default) ->
    kz_json:get_binary_value([<<"org">>], Doc, Default).

-spec set_org(doc(), binary()) -> doc().
set_org(Doc, Org) ->
    kz_json:set_value([<<"org">>], Org, Doc).

-spec preflow(doc()) -> kz_json:object().
preflow(Doc) ->
    preflow(Doc, kz_json:new()).

-spec preflow(doc(), Default) -> kz_json:object() | Default.
preflow(Doc, Default) ->
    kz_json:get_json_value([<<"preflow">>], Doc, Default).

-spec set_preflow(doc(), kz_json:object()) -> doc().
set_preflow(Doc, Preflow) ->
    kz_json:set_value([<<"preflow">>], Preflow, Doc).

-spec preflow_always(doc()) -> kz_term:api_binary().
preflow_always(Doc) ->
    preflow_always(Doc, 'undefined').

-spec preflow_always(doc(), Default) -> binary() | Default.
preflow_always(Doc, Default) ->
    kz_json:get_binary_value([<<"preflow">>, <<"always">>], Doc, Default).

-spec set_preflow_always(doc(), binary()) -> doc().
set_preflow_always(Doc, PreflowAlways) ->
    kz_json:set_value([<<"preflow">>, <<"always">>], PreflowAlways, Doc).

-spec realm(doc()) -> kz_term:api_ne_binary().
realm(Doc) ->
    realm(Doc, 'undefined').

-spec realm(doc(), Default) -> kz_term:ne_binary() | Default.
realm(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"realm">>], Doc, Default).

-spec set_realm(doc(), kz_term:ne_binary()) -> doc().
set_realm(Doc, Realm) ->
    kz_json:set_value([<<"realm">>], Realm, Doc).

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
    kz_json:get_binary_value([<<"ringtones">>, <<"external">>], Doc, Default).

-spec set_ringtones_external(doc(), binary()) -> doc().
set_ringtones_external(Doc, RingtonesExternal) ->
    kz_json:set_value([<<"ringtones">>, <<"external">>], RingtonesExternal, Doc).

-spec ringtones_internal(doc()) -> kz_term:api_binary().
ringtones_internal(Doc) ->
    ringtones_internal(Doc, 'undefined').

-spec ringtones_internal(doc(), Default) -> binary() | Default.
ringtones_internal(Doc, Default) ->
    kz_json:get_binary_value([<<"ringtones">>, <<"internal">>], Doc, Default).

-spec set_ringtones_internal(doc(), binary()) -> doc().
set_ringtones_internal(Doc, RingtonesInternal) ->
    kz_json:set_value([<<"ringtones">>, <<"internal">>], RingtonesInternal, Doc).

-spec timezone(kz_term:api_ne_binary() | doc()) -> kz_term:ne_binary().
timezone('undefined') -> default_timezone();
timezone(AccountId) when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'ok', JObj} -> timezone(JObj);
        {'error', _R} ->
            lager:debug("failed to open account ~s definition, returning system's default timezone"),
            default_timezone()
    end;
timezone(JObj) ->
    timezone(JObj, 'undefined').

-spec timezone(kz_term:api_ne_binary() | doc(), Default) -> kz_term:ne_binary() | Default.
timezone('undefined', 'undefined') ->
    default_timezone();
timezone('undefined', <<"inherit">>) -> %% UI-1808
    default_timezone();
timezone('undefined', Default) ->
    Default;
timezone(AccountId, Default) when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'ok', JObj} -> timezone(JObj, Default);
        {'error', _R} when Default =:= 'undefined';
                           Default =:= <<"inherit">> -> %% UI-1808
            lager:debug("failed to open account ~s definition, returning system's default timezone"),
            default_timezone();
        {'error', _} ->
            Default
    end;
timezone(JObj, Default) ->
    case kz_json:get_value([<<"timezone">>], JObj, Default) of
        <<"inherit">> -> parent_timezone(kz_doc:account_id(JObj), parent_account_id(JObj)); %% UI-1808
        'undefined' -> parent_timezone(kz_doc:account_id(JObj), parent_account_id(JObj));
        TZ -> TZ
    end.

-spec parent_timezone(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
parent_timezone(AccountId, AccountId) -> default_timezone();
parent_timezone(_AccountId, 'undefined') -> default_timezone();
parent_timezone(_AccountId, ParentId) -> timezone(ParentId).

-spec default_timezone() -> kz_term:ne_binary().
default_timezone() ->
    kapps_config:get_ne_binary(<<"accounts">>, <<"default_timezone">>, <<"America/Los_Angeles">>).

-spec set_timezone(doc(), kz_term:ne_binary()) -> doc().
set_timezone(JObj, Timezone) ->
    kz_json:set_value([<<"timezone">>], Timezone, JObj).

-spec parent_account_id(doc()) -> kz_term:api_binary().
parent_account_id(JObj) ->
    case tree(JObj) of
        [] -> 'undefined';
        Ancestors -> lists:last(Ancestors)
    end.

-spec topup(doc()) -> kz_term:api_object().
topup(Doc) ->
    topup(Doc, 'undefined').

-spec topup(doc(), Default) -> kz_json:object() | Default.
topup(Doc, Default) ->
    kz_json:get_json_value([<<"topup">>], Doc, Default).

-spec set_topup(doc(), kz_json:object()) -> doc().
set_topup(Doc, Topup) ->
    kz_json:set_value([<<"topup">>], Topup, Doc).

-spec topup_threshold(doc()) -> kz_term:api_number().
topup_threshold(Doc) ->
    topup_threshold(Doc, 'undefined').

-spec topup_threshold(doc(), Default) -> number() | Default.
topup_threshold(Doc, Default) ->
    kz_json:get_float_value([<<"topup">>, <<"threshold">>], Doc, Default).

-spec set_topup_threshold(doc(), number()) -> doc().
set_topup_threshold(Doc, TopupThreshold) ->
    kz_json:set_value([<<"topup">>, <<"threshold">>], TopupThreshold, Doc).

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

-spec zones(doc()) -> kz_term:api_object().
zones(Doc) ->
    zones(Doc, 'undefined').

-spec zones(doc(), Default) -> kz_json:object() | Default.
zones(Doc, Default) ->
    kz_json:get_json_value([<<"zones">>], Doc, Default).

-spec set_zones(doc(), kz_json:object()) -> doc().
set_zones(Doc, Zones) ->
    kz_json:set_value([<<"zones">>], Zones, Doc).

-spec zones_home(doc()) -> kz_term:api_binary().
zones_home(Doc) ->
    zones_home(Doc, 'undefined').

-spec zones_home(doc(), Default) -> binary() | Default.
zones_home(Doc, Default) ->
    kz_json:get_binary_value([<<"zones">>, <<"home">>], Doc, Default).

-spec set_zones_home(doc(), binary()) -> doc().
set_zones_home(Doc, ZonesHome) ->
    kz_json:set_value([<<"zones">>, <<"home">>], ZonesHome, Doc).


-spec type() -> kz_term:ne_binary().
type() -> <<"account">>.

-spec fetch(kz_term:api_ne_binary()) ->
                   {'ok', doc()} |
                   kz_datamgr:data_error().
fetch('undefined') ->
    {'error', 'invalid_db_name'};
fetch(Account=?NE_BINARY) ->
    fetch(Account, 'account').

-spec fetch(kz_term:api_ne_binary(), 'account' | 'accounts') ->
                   {'ok', doc()} |
                   kz_datamgr:data_error().
fetch('undefined', _) ->
    {'error', 'invalid_db_name'};
fetch(Account, 'account') ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    open_cache_doc(AccountDb, AccountId);
fetch(AccountId, 'accounts') ->
    open_cache_doc(?KZ_ACCOUNTS_DB, AccountId).

-spec open_cache_doc(kz_term:ne_binary(), kz_term:ne_binary()) ->
                            {'ok', doc()} |
                            kz_datamgr:data_error().
open_cache_doc(Db, AccountId) ->
    kz_datamgr:open_cache_doc(Db, AccountId, [{'cache_failures','false'}]).

-spec fetch_name(kz_term:ne_binary()) -> kz_term:api_ne_binary().
fetch_name(Account) ->
    fetch_value(Account, fun name/1).

-spec fetch_realm(kz_term:ne_binary()) -> kz_term:api_ne_binary().
fetch_realm(Account) ->
    fetch_value(Account, fun realm/1).

-spec fetch_value(kz_term:ne_binary(), fun((doc()) -> kz_json:json_term())) ->
                         kz_json:api_json_term().
fetch_value(Account, Getter) ->
    case fetch(Account) of
        {'ok', Doc} -> Getter(Doc);
        {'error', _R} ->
            lager:error("error opening account doc ~p", [Account]),
            'undefined'
    end.

-spec api_key(doc()) -> kz_term:api_binary().
api_key(JObj) ->
    kz_json:get_value([<<"pvt_api_key">>], JObj).

-spec set_api_key(doc(), kz_term:ne_binary()) -> doc().
set_api_key(JObj, ApiKey) ->
    kz_json:set_value([<<"pvt_api_key">>], ApiKey, JObj).

-spec is_enabled(doc()) -> boolean().
is_enabled(JObj) ->
    kz_json:is_true([<<"pvt_enabled">>], JObj, 'true').

-spec enable(doc()) -> doc().
enable(JObj) ->
    kz_json:set_value([<<"pvt_enabled">>], 'true', JObj).

-spec disable(doc()) -> doc().
disable(JObj) ->
    kz_json:set_value([<<"pvt_enabled">>], 'false', JObj).


-spec tree(doc()) -> kz_term:ne_binaries().
tree(JObj) ->
    tree(JObj, []).

-spec tree(doc(), Default) -> kz_term:ne_binaries() | Default.
tree(JObj, Default) ->
    kz_json:get_list_value([<<"pvt_tree">>], JObj, Default).

-spec set_tree(doc(), kz_term:ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    kz_json:set_value([<<"pvt_tree">>], Tree, JObj).

-spec notification_preference(doc()) -> kz_term:api_binary().
notification_preference(JObj) ->
    Pref = notification_preference(JObj, [[<<"pvt_notification_preference">>]
                                         ,[<<"notifications">>, <<"voicemail_to_email">>]
                                         ,[<<"notifications">>, <<"fax_to_email">>]
                                         ]),

    case Pref of
        'undefined'    -> 'undefined';
        <<"teletype">> -> <<"teletype">>;
        _Default       -> <<"notify">>
    end.

-spec notification_preference(doc(), list()) -> kz_term:api_binary().
notification_preference(_JObj, []) ->
    'undefined';
notification_preference(JObj, [H|T]) ->
    case kz_json:get_ne_value(H, JObj) of
        'undefined' -> notification_preference(JObj, T);
        Value -> Value
    end.

-spec set_notification_preference(doc(), kz_term:ne_binary()) -> doc().
set_notification_preference(JObj, Pref) ->
    kz_json:set_value([<<"pvt_notification_preference">>], Pref, JObj).

-spec allow_number_additions(doc()) -> boolean().
allow_number_additions(JObj) ->
    kz_json:is_true([<<"pvt_wnm_allow_additions">>], JObj).

-spec set_allow_number_additions(doc(), boolean()) -> doc().
set_allow_number_additions(JObj, IsAllowed) ->
    kz_json:set_value([<<"pvt_wnm_allow_additions">>], kz_term:is_true(IsAllowed), JObj).

-spec is_superduper_admin(doc()) -> boolean().
is_superduper_admin(JObj) ->
    kz_json:is_true([<<"pvt_superduper_admin">>], JObj).

-spec set_superduper_admin(doc(), boolean()) -> doc().
set_superduper_admin(JObj, IsAdmin) ->
    kz_json:set_value([<<"pvt_superduper_admin">>], IsAdmin, JObj).

-spec trial_expiration(doc()) -> kz_term:api_integer().
trial_expiration(JObj) ->
    trial_expiration(JObj, 'undefined').

-spec trial_expiration(doc(), Default) -> integer() | Default.
trial_expiration(JObj, Default) ->
    kz_json:get_integer_value([<<"pvt_trial_expires">>], JObj, Default).

-spec set_trial_expiration(doc(), kz_time:gregorian_seconds()) -> doc().
set_trial_expiration(JObj, Expiration) ->
    JObj1 = kz_json:delete_key([<<"is_trial_account">>], JObj),
    kz_json:set_value([<<"pvt_trial_expires">>], Expiration, JObj1).

-spec trial_time_left(doc()) -> integer().
trial_time_left(JObj) ->
    trial_time_left(JObj, kz_time:now_s()).

-spec trial_time_left(doc(), kz_time:gregorian_seconds()) -> integer().
trial_time_left(JObj, Now) ->
    case trial_expiration(JObj) of
        'undefined' -> 0;
        Expiration -> kz_time:elapsed_s(Now, Expiration)
    end.

-spec trial_has_expired(doc()) -> boolean().
trial_has_expired(JObj) ->
    trial_has_expired(JObj, kz_time:now_s()).

-spec trial_has_expired(doc(), kz_time:gregorian_seconds()) -> boolean().
trial_has_expired(JObj, Now) ->
    trial_expiration(JObj) =/= 'undefined'
        andalso trial_time_left(JObj, Now) =< 0.

-spec is_expired(doc()) -> 'false' | {'true', kz_time:gregorian_seconds()}.
is_expired(JObj) ->
    case trial_has_expired(JObj) of
        'false' -> 'false';
        'true' -> {'true', trial_expiration(JObj)}
    end.

-spec is_trial_account(doc()) -> boolean().
is_trial_account(JObj) ->
    kz_json:is_true([<<"is_trial_account">>], JObj, 'false').

-spec is_reseller(doc()) -> boolean().
is_reseller(JObj) ->
    kz_json:is_true([<<"pvt_reseller">>], JObj)
        orelse is_superduper_admin(JObj).

-spec promote(doc()) -> doc().
promote(JObj) ->
    kz_json:set_value([<<"pvt_reseller">>], 'true', JObj).

-spec demote(doc()) -> doc().
demote(JObj) ->
    kz_json:set_value([<<"pvt_reseller">>], 'false', JObj).

-spec reseller_id(doc()) -> doc().
reseller_id(JObj) ->
    kz_json:get_value([<<"pvt_reseller_id">>], JObj).

-spec set_reseller_id(doc(), kz_term:ne_binary()) -> doc().
set_reseller_id(JObj, ResellerId) ->
    kz_json:set_value([<<"pvt_reseller_id">>], ResellerId, JObj).

-spec fax_settings(doc() | kz_term:ne_binary()) -> doc().
fax_settings(AccountId)
  when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'ok', JObj} -> fax_settings(JObj);
        {'error', _} -> ?SYSTEM_FAX_SETTINGS
    end;
fax_settings(JObj) ->
    FaxSettings = kz_json:get_json_value(?FAX_SETTINGS_KEY, JObj, ?DEFAULT_FAX_SETTINGS),
    case kz_json:get_value(?FAX_TIMEZONE_KEY, FaxSettings) of
        'undefined' -> kz_json:set_value(?FAX_TIMEZONE_KEY, timezone(JObj), FaxSettings);
        _ -> FaxSettings
    end.

-spec preflow_id(doc()) -> kz_term:api_ne_binary().
preflow_id(Doc) ->
    preflow_id(Doc, 'undefined').

-spec preflow_id(doc(), Default) -> kz_term:ne_binary() | Default.
preflow_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"preflow">>, <<"always">>], Doc, Default).

-spec get_inherited_value(kz_term:api_binary(), fun(), any()) -> any().
get_inherited_value('undefined', _ValueFun, Default) ->
    Default;
get_inherited_value(Account, ValueFun, Default) ->
    case check_account(Account, ValueFun) of
        'undefined' ->
            check_reseller(Account, ValueFun, Default);
        Value -> Value
    end.

-spec check_account(kz_term:api_binary(), fun()) -> any().
check_account(Account, ValueFun) ->
    case fetch(Account) of
        {'error', _Err} -> 'undefined';
        {'ok', JObj} -> ValueFun(JObj)
    end.

-spec check_reseller(kz_term:api_binary(), fun(), any()) -> any().
check_reseller(Account, ValueFun, Default) ->
    Reseller = kz_services:find_reseller_id(Account),
    case check_account(Reseller, ValueFun) of
        'undefined' -> Default;
        Value -> Value
    end.

-spec get_parent_account_id(kz_term:ne_binary()) -> kz_term:api_binary().
get_parent_account_id(AccountId) ->
    case fetch(AccountId) of
        {'ok', JObj} -> parent_account_id(JObj);
        {'error', _R} ->
            lager:debug("failed to open account's ~s parent: ~p", [AccountId, _R]),
            'undefined'
    end.

-spec low_balance_threshold(kz_term:ne_binary() | doc()) -> kz_term:api_float().
low_balance_threshold(Thing) ->
    Default = kapps_config:get_float(<<"notify.low_balance">>, <<"threshold">>, 5.00),
    low_balance_threshold(Thing, Default).

-spec low_balance_threshold(kz_term:ne_binary() | doc(), Default) -> float() | Default.
low_balance_threshold(AccountId, Default) when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'error', _R} -> low_balance_threshold(kz_json:new(), Default);
        {'ok', JObj} -> low_balance_threshold(JObj, Default)
    end;
low_balance_threshold(Doc, Default) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    case kz_json:get_float_value(<<"threshold">>, LowBalance) of
        'undefined' -> topup_threshold(Doc, Default);
        Threshold -> Threshold
    end.

-spec set_low_balance_threshold(doc(), float()) -> doc().
set_low_balance_threshold(Doc, Threshold) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    set_notification(Doc, <<"low_balance">>, kz_json:set_value(<<"threshold">>, Threshold, LowBalance)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_sent(doc()) -> boolean().
low_balance_sent(Doc) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    kz_json:is_true(<<"sent_low_balance">>, LowBalance).

-spec set_low_balance_sent(doc()) -> doc().
set_low_balance_sent(Doc) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    set_notification(Doc, <<"low_balance">>, kz_json:set_value(<<"sent_low_balance">>, 'true', LowBalance)).


-spec reset_low_balance_sent(doc()) -> doc().
reset_low_balance_sent(Doc) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    set_notification(Doc, <<"low_balance">>, kz_json:set_value(<<"sent_low_balance">>, 'false', LowBalance)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_enabled(doc()) -> boolean().
low_balance_enabled(Doc) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    kzd_notifications:enabled(LowBalance).

-spec set_low_balance_enabled(doc()) -> doc().
set_low_balance_enabled(Doc) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    set_notification(Doc, <<"low_balance">>, kzd_notifications:set_enabled(LowBalance, 'true')).

-spec reset_low_balance_enabled(doc()) -> doc().
reset_low_balance_enabled(Doc) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    set_notification(Doc, <<"low_balance">>, kzd_notifications:set_enabled(LowBalance, 'true')).

-spec low_balance_enabled_exists(doc()) -> boolean().
low_balance_enabled_exists(Doc) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    'undefined' =/= kzd_notifications:enabled(LowBalance, 'undefined').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_tstamp(doc()) -> kz_term:api_number().
low_balance_tstamp(Doc) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    kz_json:get_integer_value([<<"last_notification">>], LowBalance).

-spec set_low_balance_tstamp(doc()) -> doc().
set_low_balance_tstamp(Doc) ->
    set_low_balance_tstamp(Doc, kz_time:now_s()).

-spec set_low_balance_tstamp(doc(), kz_term:gregorian_seconds()) -> doc().
set_low_balance_tstamp(Doc, TStamp) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    set_notification(Doc, <<"low_balance">>, kz_json:set_value([<<"last_notification">>], TStamp, LowBalance)).

-spec remove_low_balance_tstamp(doc()) -> doc().
remove_low_balance_tstamp(Doc) ->
    LowBalance = notification(Doc, <<"low_balance">>, kz_json:new()),
    set_notification(Doc, <<"low_balance">>, kz_json:delete_key([<<"last_notification">>], LowBalance)).

-spec home_zone(kz_term:ne_binary() | doc()) -> kz_term:api_binary().
home_zone(AccountId) when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'error', _R} -> 'undefined';
        {'ok', JObj}  -> home_zone(JObj, 'undefined')
    end;

home_zone(JObj) ->
    home_zone(JObj, 'undefined').

-spec home_zone(kz_term:ne_binary() | doc(), kz_term:api_binary()) -> kz_term:api_binary().
home_zone(AccountId, Default) when is_binary(AccountId) ->
    case fetch(AccountId) of
        {'error', _R} -> Default;
        {'ok', JObj}  -> home_zone(JObj, Default)
    end;

home_zone(JObj, Default) ->
    zones_home(JObj, Default).

-spec set_home_zone(doc(), kz_term:api_binary()) -> doc().
set_home_zone(JObj, Zone) ->
    set_zones_home(JObj, Zone).

-spec sent_initial_registration(doc()) -> boolean().
sent_initial_registration(Doc) ->
    FirstOccurrence = notification(Doc, <<"first_occurrence">>, kz_json:new()),
    kz_json:is_true(<<"sent_initial_registration">>, FirstOccurrence).

-spec set_initial_registration_sent(doc(), boolean()) -> doc().
set_initial_registration_sent(Doc, Sent) ->
    FirstOccurrence = notification(Doc, <<"first_occurrence">>, kz_json:new()),
    set_notification(Doc, <<"first_occurrence">>, kz_json:set_value(<<"sent_initial_registration">>, Sent, FirstOccurrence)).

-spec sent_initial_call(doc()) -> boolean().
sent_initial_call(Doc) ->
    FirstOccurrence = notification(Doc, <<"first_occurrence">>, kz_json:new()),
    kz_json:is_true(<<"sent_initial_call">>, FirstOccurrence).

-spec set_initial_call_sent(doc(), boolean()) -> doc().
set_initial_call_sent(Doc, Sent) ->
    FirstOccurrence = notification(Doc, <<"first_occurrence">>, kz_json:new()),
    set_notification(Doc, <<"first_occurrence">>, kz_json:set_value(<<"sent_initial_call">>, Sent, FirstOccurrence)).
