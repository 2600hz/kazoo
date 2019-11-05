%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_accounts).

-export([new/0]).
-export([call_recording/1, call_recording/2, set_call_recording/2]).
-export([call_recording_account/1, call_recording_account/2, set_call_recording_account/2]).
-export([call_recording_endpoint/1, call_recording_endpoint/2, set_call_recording_endpoint/2]).
-export([call_restriction/1, call_restriction/2, set_call_restriction/2]).
-export([call_waiting/1, call_waiting/2, set_call_waiting/2]).
-export([caller_id/1, caller_id/2, set_caller_id/2]).
-export([caller_id_options/1, caller_id_options/2, set_caller_id_options/2]).
-export([caller_id_options_outbound_privacy/1, caller_id_options_outbound_privacy/2, set_caller_id_options_outbound_privacy/2]).
-export([caller_id_options_show_rate/1, caller_id_options_show_rate/2, set_caller_id_options_show_rate/2]).
-export([dial_plan/1, dial_plan/2, set_dial_plan/2]).
-export([do_not_disturb/1, do_not_disturb/2, set_do_not_disturb/2]).
-export([do_not_disturb_enabled/1, do_not_disturb_enabled/2, set_do_not_disturb_enabled/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([formatters/1, formatters/2, set_formatters/2]).
-export([language/1, language/2, set_language/2]).
-export([metaflows/1, metaflows/2, set_metaflows/2]).
-export([music_on_hold/1, music_on_hold/2, set_music_on_hold/2]).
-export([music_on_hold_media_id/1, music_on_hold_media_id/2, set_music_on_hold_media_id/2]).
-export([id/1]).
-export([name/1, name/2, set_name/2]).
-export([notifications/1, notifications/2, set_notifications/2]).
-export([notifications_first_occurrence/1, notifications_first_occurrence/2, set_notifications_first_occurrence/2]).
-export([notifications_first_occurrence_sent_initial_call/1, notifications_first_occurrence_sent_initial_call/2, set_notifications_first_occurrence_sent_initial_call/2]).
-export([notifications_first_occurrence_sent_initial_registration/1, notifications_first_occurrence_sent_initial_registration/2, set_notifications_first_occurrence_sent_initial_registration/2]).
-export([notifications_low_balance/1, notifications_low_balance/2, set_notifications_low_balance/2, path_notifications_low_balance/0]).
-export([notifications_low_balance_enabled/1, notifications_low_balance_enabled/2, set_notifications_low_balance_enabled/2]).
-export([notifications_low_balance_last_notification/1, notifications_low_balance_last_notification/2, set_notifications_low_balance_last_notification/2]).
-export([notifications_low_balance_sent_low_balance/1, notifications_low_balance_sent_low_balance/2, set_notifications_low_balance_sent_low_balance/2, path_notifications_low_balance_sent_low_balance/0]).
-export([notifications_low_balance_threshold/1, notifications_low_balance_threshold/2, set_notifications_low_balance_threshold/2]).
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
-export([topup_amount/1, topup_amount/2, set_topup_amount/2]).
-export([voicemail/1, voicemail/2, set_voicemail/2]).
-export([voicemail_notify/1, voicemail_notify/2, set_voicemail_notify/2]).
-export([voicemail_notify_callback/1, voicemail_notify_callback/2, set_voicemail_notify_callback/2]).
-export([zones/1, zones/2, set_zones/2]).
-export([zones_home/1, zones_home/2, set_zones_home/2]).


-export([type/0
        ,fetch/1, fetch/2
        ,fetch_name/1, fetch_realm/1
        ,save/1, save_accounts_doc/1
        ,update/2

        ,api_key/1, set_api_key/2
        ,is_enabled/1, enable/1, disable/1
        ,path_enabled/0
        ,is_expired/1

        ,tree/1, tree/2, set_tree/2, path_tree/0
        ,default_timezone/0
        ,notification_preference/1, set_notification_preference/2, path_notification_preference/0
        ,allow_number_additions/1, set_allow_number_additions/2, path_allow_number_additions/0
        ,is_superduper_admin/1, set_superduper_admin/2, path_superduper_admin/0

        ,trial_expiration/1, trial_expiration/2, set_trial_expiration/2
        ,trial_time_left/1, trial_time_left/2
        ,trial_has_expired/2

        ,demote/1, promote/1, path_reseller/0

        ,fax_settings/1
        ,get_inherited_value/3
        ,get_parent_account_id/1
        ,get_authoritative_parent_id/1, get_authoritative_parent_id/2

        ,reseller_id/1, set_reseller_id/2, is_reseller/1, path_reseller_id/0
        ,is_trial_account/1
        ,low_balance_enabled/1, low_balance_enabled_exists/1, set_low_balance_enabled/1, reset_low_balance_enabled/1

        ,low_balance_sent/1, reset_low_balance_sent/1, path_low_balance_sent/0


        ,low_balance_threshold/1, low_balance_threshold/2, set_low_balance_threshold/2
        ,low_balance_tstamp/1, remove_low_balance_tstamp/1, path_low_balance_tstamp/0
        ,parent_account_id/1
        ,preflow_id/1

        ,bill_early_task_timestamp/1, set_bill_early_task_timestamp/2, path_bill_early_task_timestamp/0

        ,home_zone/1, home_zone/2, set_home_zone/2

        ,sent_initial_call/1
        ,sent_initial_registration/1
        ,set_initial_call_sent/2, path_initial_call_sent/0
        ,set_initial_registration_sent/2, path_initial_registration_sent/0
        ,set_low_balance_sent/1
        ,set_low_balance_tstamp/1

        ,is_in_account_hierarchy/2, is_in_account_hierarchy/3
        ,normalize_name/1

        ,validate/3
        ,add_pvt_api_key/1

        ,cpaas_token/1
        ,path_cpaas_token/0
        ,set_cpaas_token/1, set_cpaas_token/2, maybe_set_cpaas_token/1

        ,is_unique_realm/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"accounts">>).
-define(AGG_VIEW_REALM, <<"accounts/listing_by_realm">>).
-define(AGG_VIEW_NAME, <<"accounts/listing_by_name">>).

-define(ACCOUNTS_CONFIG_CAT, <<"crossbar.accounts">>).
-define(CONFIG_CAT, <<"crossbar">>).

-define(SHOULD_ENSURE_SCHEMA_IS_VALID
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"ensure_valid_schema">>, 'true')
       ).

-define(SHOULD_FAIL_ON_INVALID_DATA
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"schema_strict_validation">>, 'false')
       ).

-define(ACCOUNT_REALM_SUFFIX
       ,kapps_config:get_binary(?ACCOUNTS_CONFIG_CAT, <<"account_realm_suffix">>, <<"sip.2600hz.com">>)
       ).
-define(RANDOM_REALM_STRENGTH
       ,kapps_config:get_integer(?ACCOUNTS_CONFIG_CAT, <<"random_realm_strength">>, 3)
       ).

-define(REMOVE_SPACES, [<<"realm">>]).

-spec new() -> doc().
new() ->
    kz_doc:update_pvt_parameters(kz_json_schema:default_object(?SCHEMA)
                                ,'undefined'
                                ,[{'type', type()}
                                 ,{'now', kz_time:now_s()}
                                 ]
                                ).

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

-spec caller_id_options(doc()) -> kz_term:api_object().
caller_id_options(Doc) ->
    caller_id_options(Doc, 'undefined').

-spec caller_id_options(doc(), Default) -> kz_json:object() | Default.
caller_id_options(Doc, Default) ->
    kz_json:get_json_value([<<"caller_id_options">>], Doc, Default).

-spec set_caller_id_options(doc(), kz_json:object()) -> doc().
set_caller_id_options(Doc, CallerIdOptions) ->
    kz_json:set_value([<<"caller_id_options">>], CallerIdOptions, Doc).

-spec caller_id_options_outbound_privacy(doc()) -> kz_term:api_binary().
caller_id_options_outbound_privacy(Doc) ->
    caller_id_options_outbound_privacy(Doc, 'undefined').

-spec caller_id_options_outbound_privacy(doc(), Default) -> binary() | Default.
caller_id_options_outbound_privacy(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"caller_id_options">>, <<"outbound_privacy">>], Doc, Default).

-spec set_caller_id_options_outbound_privacy(doc(), binary()) -> doc().
set_caller_id_options_outbound_privacy(Doc, CallerIdOptionsOutboundPrivacy) ->
    kz_json:set_value([<<"caller_id_options">>, <<"outbound_privacy">>], CallerIdOptionsOutboundPrivacy, Doc).

-spec caller_id_options_show_rate(doc()) -> boolean().
caller_id_options_show_rate(Doc) ->
    caller_id_options_show_rate(Doc, 'false').

-spec caller_id_options_show_rate(doc(), Default) -> boolean() | Default.
caller_id_options_show_rate(Doc, Default) ->
    kz_json:is_true([<<"caller_id_options">>, <<"show_rate">>], Doc, Default).

-spec set_caller_id_options_show_rate(doc(), boolean()) -> doc().
set_caller_id_options_show_rate(Doc, ShowRate) ->
    kz_json:set_value([<<"caller_id_options">>, <<"show_rate">>], ShowRate, Doc).

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
    enabled(Doc, 'true').

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

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

-spec language(doc()) -> kz_term:api_binary().
language(Doc) ->
    language(Doc, 'undefined').

-spec language(doc(), Default) -> binary() | Default.
language(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"language">>], Doc, Default).

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
    kz_json:get_ne_binary_value([<<"music_on_hold">>, <<"media_id">>], Doc, Default).

-spec set_music_on_hold_media_id(doc(), binary()) -> doc().
set_music_on_hold_media_id(Doc, MusicOnHoldMediaId) ->
    kz_json:set_value([<<"music_on_hold">>, <<"media_id">>], MusicOnHoldMediaId, Doc).

-spec id(doc()) -> kz_term:api_binary().
id(Doc) ->
    kz_doc:id(Doc).

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

-spec notifications_first_occurrence(doc()) -> kz_term:api_object().
notifications_first_occurrence(Doc) ->
    notifications_first_occurrence(Doc, 'undefined').

-spec notifications_first_occurrence(doc(), Default) -> kz_json:object() | Default.
notifications_first_occurrence(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"first_occurrence">>], Doc, Default).

-spec set_notifications_first_occurrence(doc(), kz_json:object()) -> doc().
set_notifications_first_occurrence(Doc, NotificationsFirstOccurrence) ->
    kz_json:set_value([<<"notifications">>, <<"first_occurrence">>], NotificationsFirstOccurrence, Doc).

-spec notifications_first_occurrence_sent_initial_call(doc()) -> boolean().
notifications_first_occurrence_sent_initial_call(Doc) ->
    notifications_first_occurrence_sent_initial_call(Doc, false).

-spec notifications_first_occurrence_sent_initial_call(doc(), Default) -> boolean() | Default.
notifications_first_occurrence_sent_initial_call(Doc, Default) ->
    kz_json:get_boolean_value([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>], Doc, Default).

-spec set_notifications_first_occurrence_sent_initial_call(doc(), boolean()) -> doc().
set_notifications_first_occurrence_sent_initial_call(Doc, NotificationsFirstOccurrenceSentInitialCall) ->
    kz_json:set_value([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>], NotificationsFirstOccurrenceSentInitialCall, Doc).

-spec notifications_first_occurrence_sent_initial_registration(doc()) -> boolean().
notifications_first_occurrence_sent_initial_registration(Doc) ->
    notifications_first_occurrence_sent_initial_registration(Doc, false).

-spec notifications_first_occurrence_sent_initial_registration(doc(), Default) -> boolean() | Default.
notifications_first_occurrence_sent_initial_registration(Doc, Default) ->
    kz_json:get_boolean_value([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>], Doc, Default).

-spec set_notifications_first_occurrence_sent_initial_registration(doc(), boolean()) -> doc().
set_notifications_first_occurrence_sent_initial_registration(Doc, NotificationsFirstOccurrenceSentInitialRegistration) ->
    kz_json:set_value([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>], NotificationsFirstOccurrenceSentInitialRegistration, Doc).

-spec notifications_low_balance(doc()) -> kz_term:api_object().
notifications_low_balance(Doc) ->
    notifications_low_balance(Doc, 'undefined').

-spec notifications_low_balance(doc(), Default) -> kz_json:object() | Default.
notifications_low_balance(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"low_balance">>], Doc, Default).

-spec path_notifications_low_balance() -> kz_json:path().
path_notifications_low_balance() ->
    [<<"notifications">>, <<"low_balance">>].

-spec set_notifications_low_balance(doc(), kz_json:object()) -> doc().
set_notifications_low_balance(Doc, NotificationsLowBalance) ->
    kz_json:set_value([<<"notifications">>, <<"low_balance">>], NotificationsLowBalance, Doc).

-spec notifications_low_balance_enabled(doc()) -> boolean().
notifications_low_balance_enabled(Doc) ->
    notifications_low_balance_enabled(Doc, 'undefined').

-spec notifications_low_balance_enabled(doc(), Default) -> boolean() | Default.
notifications_low_balance_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"notifications">>, <<"low_balance">>, <<"enabled">>], Doc, Default).

-spec set_notifications_low_balance_enabled(doc(), boolean()) -> doc().
set_notifications_low_balance_enabled(Doc, NotificationsLowBalanceEnabled) ->
    kz_json:set_value([<<"notifications">>, <<"low_balance">>, <<"enabled">>], NotificationsLowBalanceEnabled, Doc).

-spec notifications_low_balance_last_notification(doc()) -> kz_term:api_integer().
notifications_low_balance_last_notification(Doc) ->
    notifications_low_balance_last_notification(Doc, 'undefined').

-spec notifications_low_balance_last_notification(doc(), Default) -> integer() | Default.
notifications_low_balance_last_notification(Doc, Default) ->
    kz_json:get_integer_value([<<"notifications">>, <<"low_balance">>, <<"last_notification">>], Doc, Default).

-spec set_notifications_low_balance_last_notification(doc(), integer()) -> doc().
set_notifications_low_balance_last_notification(Doc, NotificationsLowBalanceLastNotification) ->
    kz_json:set_value([<<"notifications">>, <<"low_balance">>, <<"last_notification">>], NotificationsLowBalanceLastNotification, Doc).

-spec notifications_low_balance_sent_low_balance(doc()) -> kz_term:api_boolean().
notifications_low_balance_sent_low_balance(Doc) ->
    notifications_low_balance_sent_low_balance(Doc, 'undefined').

-spec notifications_low_balance_sent_low_balance(doc(), Default) -> boolean() | Default.
notifications_low_balance_sent_low_balance(Doc, Default) ->
    kz_json:get_boolean_value([<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>], Doc, Default).

-spec set_notifications_low_balance_sent_low_balance(doc(), boolean()) -> doc().
set_notifications_low_balance_sent_low_balance(Doc, NotificationsLowBalanceSentLowBalance) ->
    kz_json:set_value([<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>], NotificationsLowBalanceSentLowBalance, Doc).

-spec path_notifications_low_balance_sent_low_balance() -> kz_json:path().
path_notifications_low_balance_sent_low_balance() ->
    [<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>].

-spec notifications_low_balance_threshold(doc()) -> kz_term:api_number().
notifications_low_balance_threshold(Doc) ->
    notifications_low_balance_threshold(Doc, 'undefined').

-spec notifications_low_balance_threshold(doc(), Default) -> number() | Default.
notifications_low_balance_threshold(Doc, Default) ->
    kz_json:get_float_value([<<"notifications">>, <<"low_balance">>, <<"threshold">>], Doc, Default).

-spec set_notifications_low_balance_threshold(doc(), number()) -> doc().
set_notifications_low_balance_threshold(Doc, NotificationsLowBalanceThreshold) ->
    kz_json:set_value([<<"notifications">>, <<"low_balance">>, <<"threshold">>], NotificationsLowBalanceThreshold, Doc).

-spec org(doc()) -> kz_term:api_binary().
org(Doc) ->
    org(Doc, 'undefined').

-spec org(doc(), Default) -> binary() | Default.
org(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"org">>], Doc, Default).

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
    kz_json:get_ne_binary_value([<<"preflow">>, <<"always">>], Doc, Default).

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

-spec topup_threshold(doc()) -> kz_term:api_float().
topup_threshold(Doc) ->
    topup_threshold(Doc, 'undefined').

-spec topup_threshold(doc(), Default) -> float() | Default.
topup_threshold(Doc, Default) ->
    kz_json:get_float_value([<<"topup">>, <<"threshold">>], Doc, Default).

-spec set_topup_threshold(doc(), float()) -> doc().
set_topup_threshold(Doc, TopupThreshold) ->
    kz_json:set_value([<<"topup">>, <<"threshold">>], TopupThreshold, Doc).

-spec topup_amount(doc()) -> kz_term:api_float().
topup_amount(Doc) ->
    topup_amount(Doc, 'undefined').

-spec topup_amount(doc(), Default) -> float() | Default.
topup_amount(Doc, Default) ->
    kz_json:get_float_value([<<"topup">>, <<"amount">>], Doc, Default).

-spec set_topup_amount(doc(), float()) -> doc().
set_topup_amount(Doc, TopupAmount) ->
    kz_json:set_value([<<"topup">>, <<"amount">>], TopupAmount, Doc).

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
    kz_json:get_ne_binary_value([<<"zones">>, <<"home">>], Doc, Default).

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
    Options = [{'cache_failures', 'false'}
              ,{'deleted', 'true'}
              ],
    kz_datamgr:open_cache_doc(Db, AccountId, Options).

-spec fetch_name(kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
fetch_name(Account) ->
    fetch_value(Account, fun name/1).

-spec fetch_realm(kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
fetch_realm(Account) ->
    fetch_value(Account, fun realm/1).

-spec fetch_value(kz_term:api_ne_binary(), fun((doc()) -> kz_json:json_term())) ->
                         kz_json:api_json_term().
fetch_value('undefined', _Getter) -> 'undefined';
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

-spec is_enabled(doc() | kz_term:api_ne_binary()) -> boolean().
is_enabled('undefined') -> 'false';
is_enabled(?NE_BINARY = Id) ->
    case fetch(Id) of
        {'ok', JObj} -> is_enabled(JObj);
        {'error', _} -> 'false'
    end;
is_enabled(JObj) ->
    kz_json:is_true([<<"pvt_enabled">>], JObj, 'true').

-spec enable(doc()) -> doc().
enable(JObj) ->
    kz_json:set_value([<<"pvt_enabled">>], 'true', JObj).

-spec disable(doc()) -> doc().
disable(JObj) ->
    kz_json:set_value([<<"pvt_enabled">>], 'false', JObj).

-spec path_enabled() -> kz_json:path().
path_enabled() ->
    [<<"pvt_enabled">>].

-spec tree(kz_term:ne_binary() | doc()) -> kz_term:ne_binaries().
tree(JObj) ->
    tree(JObj, []).

-spec tree(kz_term:ne_binary() | doc(), Default) -> kz_term:ne_binaries() | Default.
tree(?NE_BINARY=AccountId, Default) ->
    {'ok', Doc} = fetch(AccountId),
    tree(Doc, Default);
tree(JObj, Default) ->
    kz_json:get_list_value([<<"pvt_tree">>], JObj, Default).

-spec path_tree() -> kz_json:path().
path_tree() ->
    [<<"pvt_tree">>].

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

-spec path_notification_preference() -> kz_json:path().
path_notification_preference() ->
    [<<"pvt_notification_preference">>].

-spec allow_number_additions(doc()) -> boolean().
allow_number_additions(JObj) ->
    kz_json:is_true([<<"pvt_wnm_allow_additions">>], JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_allow_number_additions(doc(), boolean()) -> doc().
set_allow_number_additions(JObj, IsAllowed) ->
    kz_json:set_value([<<"pvt_wnm_allow_additions">>], kz_term:is_true(IsAllowed), JObj).

-spec path_allow_number_additions() -> kz_json:path().
path_allow_number_additions() ->
    [<<"pvt_wnm_allow_additions">>].

-spec is_superduper_admin(kz_term:api_ne_binary() | doc()) -> boolean().
is_superduper_admin('undefined') -> 'false';
is_superduper_admin(?NE_BINARY = Id) ->
    case fetch(Id) of
        {'ok', JObj} -> is_superduper_admin(JObj);
        {'error', _} -> 'false'
    end;
is_superduper_admin(JObj) ->
    kz_json:is_true([<<"pvt_superduper_admin">>], JObj).

-spec set_superduper_admin(doc(), boolean()) -> doc().
set_superduper_admin(JObj, IsAdmin) ->
    kz_json:set_value([<<"pvt_superduper_admin">>], IsAdmin, JObj).

-spec path_superduper_admin() -> kz_json:path().
path_superduper_admin() ->
    [<<"pvt_superduper_admin">>].

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

-spec is_expired(doc() | kz_term:api_ne_binary()) -> 'false' | {'true', kz_time:gregorian_seconds()}.
is_expired('undefined') -> 'false';
is_expired(?NE_BINARY = Id) ->
    case fetch(Id) of
        {'ok', JObj} -> is_expired(JObj);
        {'error', _} -> 'false'
    end;
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

-spec path_reseller() -> kz_json:path().
path_reseller() ->
    [<<"pvt_reseller">>].

-spec reseller_id(kz_term:ne_binary() | doc()) -> kz_term:api_ne_binary().
reseller_id(<<AccountId/binary>>) ->
    {'ok', Doc} = fetch(AccountId),
    reseller_id(Doc);
reseller_id(Doc) ->
    kz_json:get_ne_binary_value([<<"pvt_reseller_id">>], Doc).

-spec set_reseller_id(doc(), kz_term:ne_binary()) -> doc().
set_reseller_id(JObj, ResellerId) ->
    kz_json:set_value([<<"pvt_reseller_id">>], ResellerId, JObj).

-spec path_reseller_id() -> kz_json:path().
path_reseller_id() ->
    [<<"pvt_reseller_id">>].

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
    Reseller = kz_services_reseller:get_id(Account),
    case check_account(Reseller, ValueFun) of
        'undefined' -> Default;
        Value -> Value
    end.

-spec get_parent_account_id(kz_term:api_ne_binary()) -> kz_term:api_binary().
get_parent_account_id(AccountId) ->
    case fetch(AccountId) of
        {'ok', JObj} -> parent_account_id(JObj);
        {'error', _R} ->
            lager:debug("failed to open account's ~s parent: ~p", [AccountId, _R]),
            'undefined'
    end.

%% @equiv get_parent_account_id(AccountId, kapps_util:get_master_account_id())
-spec get_authoritative_parent_id(kz_term:api_ne_binary()) -> kz_term:api_binary().
get_authoritative_parent_id(AccountId) ->
    get_authoritative_parent_id(AccountId, kapps_util:get_master_account_id()).

%%------------------------------------------------------------------------------
%% @doc Get authoritative parent account's ID (teletype style).
%%
%% If the account id is not a reseller returns parent account id and if the account
%% is a reseller returns master account.
%% In case that it can't get master account id, it returns `undefined'.
%%
%% The idea is that you want to write code to walk the account's heirarchy and
%% stop at the first reseller, and then jump to master account.
%%
%% This is same as teletype bahaviour to get templates.
%%
%% Please note that you have to write the actual code to walk the account's
%% heirarchy and then use this function just to get the parent id!
%% @end
%%------------------------------------------------------------------------------
-spec get_authoritative_parent_id(kz_term:api_ne_binary(), {'ok', kz_term:ne_binary()} | {'error', any()} | kz_term:ne_binary()) ->
                                         kz_term:api_ne_binary().
get_authoritative_parent_id(AccountId, {'ok', MasterAccountId}) ->
    get_authoritative_parent_id(AccountId, MasterAccountId);
get_authoritative_parent_id(_AccountId, {'error', _}) ->
    'undefined';
get_authoritative_parent_id(MasterAccountId, MasterAccountId) ->
    MasterAccountId;
get_authoritative_parent_id('undefined', _MasterAccountId) ->
    'undefined';
get_authoritative_parent_id(AccountId, MasterAccountId) ->
    case kz_services_reseller:is_reseller(AccountId) of
        'true' -> MasterAccountId;
        'false' ->
            get_parent_account_id(AccountId)
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
    LowBalance = notifications_low_balance(Doc, kz_json:new()),
    case kz_json:get_float_value(<<"threshold">>, LowBalance) of
        'undefined' -> topup_threshold(Doc, Default);
        Threshold -> Threshold
    end.

-spec set_low_balance_threshold(doc(), float()) -> doc().
set_low_balance_threshold(Doc, Threshold) ->
    set_notifications_low_balance_threshold(Doc, Threshold).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec low_balance_sent(doc()) -> boolean().
low_balance_sent(Doc) ->
    LowBalance = notifications_low_balance(Doc, kz_json:new()),
    kz_json:is_true(<<"sent_low_balance">>, LowBalance).

-spec set_low_balance_sent(doc()) -> doc().
set_low_balance_sent(Doc) ->
    set_notifications_low_balance_sent_low_balance(Doc, 'true').

-spec reset_low_balance_sent(doc()) -> doc().
reset_low_balance_sent(Doc) ->
    set_notifications_low_balance_sent_low_balance(Doc, 'false').

-spec path_low_balance_sent() -> kz_json:path().
path_low_balance_sent() ->
    [<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bill_early_task_timestamp(doc()) -> kz_term:api_integer().
bill_early_task_timestamp(Doc) ->
    kz_json:get_integer_value(path_bill_early_task_timestamp(), Doc).

-spec set_bill_early_task_timestamp(doc(), non_neg_integer()) -> doc().
set_bill_early_task_timestamp(Doc, Timestamp) ->
    kz_json:set_value(path_bill_early_task_timestamp(), Timestamp, Doc).

-spec path_bill_early_task_timestamp() -> kz_json:path().
path_bill_early_task_timestamp() ->
    [<<"notifications">>, <<"low_balance">>, <<"bill_early_task_timestamp">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec low_balance_enabled(doc()) -> boolean().
low_balance_enabled(Doc) ->
    LowBalance = notifications_low_balance(Doc, kz_json:new()),
    kz_json:is_true(<<"enabled">>, LowBalance).

-spec set_low_balance_enabled(doc()) -> doc().
set_low_balance_enabled(Doc) ->
    set_notifications_low_balance_enabled(Doc, 'true').

-spec reset_low_balance_enabled(doc()) -> doc().
reset_low_balance_enabled(Doc) ->
    set_notifications_low_balance_enabled(Doc, 'true').

-spec low_balance_enabled_exists(doc()) -> boolean().
low_balance_enabled_exists(Doc) ->
    LowBalance = notifications_low_balance(Doc, kz_json:new()),
    'undefined' =/= kz_json:get_value(<<"enabled">>, LowBalance).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec low_balance_tstamp(doc()) -> kz_term:api_number().
low_balance_tstamp(Doc) ->
    LowBalance = notifications_low_balance(Doc, kz_json:new()),
    kz_json:get_integer_value([<<"last_notification">>], LowBalance).

-spec set_low_balance_tstamp(doc()) -> doc().
set_low_balance_tstamp(Doc) ->
    set_low_balance_tstamp(Doc, kz_time:now_s()).

-spec set_low_balance_tstamp(doc(), kz_time:gregorian_seconds()) -> doc().
set_low_balance_tstamp(Doc, TStamp) ->
    set_notifications_low_balance_last_notification(Doc, TStamp).

-spec remove_low_balance_tstamp(doc()) -> doc().
remove_low_balance_tstamp(Doc) ->
    LowBalance = notifications_low_balance(Doc, kz_json:new()),
    set_notifications_low_balance(Doc, kz_json:delete_key([<<"last_notification">>], LowBalance)).

-spec path_low_balance_tstamp() -> kz_json:path().
path_low_balance_tstamp() ->
    path_notifications_low_balance() ++ [<<"last_notification">>].

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
    FirstOccurrence = notifications_first_occurrence(Doc, kz_json:new()),
    kz_json:is_true(<<"sent_initial_registration">>, FirstOccurrence).

-spec set_initial_registration_sent(doc(), boolean()) -> doc().
set_initial_registration_sent(Doc, Sent) ->
    set_notifications_first_occurrence_sent_initial_registration(Doc, Sent).

-spec path_initial_registration_sent() -> kz_json:path().
path_initial_registration_sent() ->
    [<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>].

-spec sent_initial_call(doc()) -> boolean().
sent_initial_call(Doc) ->
    FirstOccurrence = notifications_first_occurrence(Doc, kz_json:new()),
    kz_json:is_true(<<"sent_initial_call">>, FirstOccurrence).

-spec set_initial_call_sent(doc(), boolean()) -> doc().
set_initial_call_sent(Doc, Sent) ->
    set_notifications_first_occurrence_sent_initial_call(Doc, Sent).

-spec path_initial_call_sent() -> kz_json:path().
path_initial_call_sent() ->
    [<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save(doc()) ->
                  {'ok', doc()} |
                  kz_datamgr:data_error().
save(AccountJObj) ->
    AccountDb = kz_doc:account_db(AccountJObj),
    case kz_datamgr:save_doc(AccountDb, AccountJObj) of
        {'error', _R}=E ->
            lager:info("failed to save account doc: ~p", [_R]),
            E;
        {'ok', SavedJObj} ->
            lager:info("saved account doc ~s(~s)", [kz_doc:id(SavedJObj), kz_doc:revision(SavedJObj)]),
            save_accounts_doc(SavedJObj)
    end.

-spec save_accounts_doc(doc()) ->
                               {'ok', doc()} |
                               kz_datamgr:data_error().
save_accounts_doc(AccountDoc) ->
    case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, kz_doc:id(AccountDoc)) of
        {'error', 'not_found'} ->
            handle_saved_accounts_doc(AccountDoc, kz_datamgr:save_doc(?KZ_ACCOUNTS_DB, kz_doc:delete_revision(AccountDoc)));
        {'error', _R}=E ->
            lager:info("failed to save account doc to accounts: ~p", [_R]),
            E;
        {'ok', AccountsDoc} ->
            NewAccountDoc = kz_json:set_value(kz_doc:path_revision()
                                             ,kz_doc:revision(AccountsDoc)
                                             ,AccountDoc
                                             ),
            handle_saved_accounts_doc(AccountDoc
                                     ,kz_datamgr:save_doc(?KZ_ACCOUNTS_DB, NewAccountDoc)
                                     )
    end.

-spec handle_saved_accounts_doc(doc(), kz_datamgr:data_error() | {'ok', doc()}) ->
                                       kz_datamgr:data_error() | {'ok', doc()}.
handle_saved_accounts_doc(AccountDoc, {'ok', _AccountsDoc}) ->
    lager:info("saved accounts doc ~s(~s)", [kz_doc:id(_AccountsDoc), kz_doc:revision(_AccountsDoc)]),
    {'ok', AccountDoc};
handle_saved_accounts_doc(_AccountDoc, Error) ->
    lager:debug("failed to save 'accounts' doc ~s: ~p", [kz_doc:id(_AccountDoc), Error]),
    Error.

-spec update(kz_term:ne_binary(), kz_json:flat_proplist()) ->
                    {'ok', doc()} |
                    kz_datamgr:data_error().
update(?NE_BINARY = Account, UpdateProps) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    AccountDb = kz_util:format_account_db(AccountId),

    UpdateOptions = [{'update', UpdateProps}
                    ,{'ensure_saved', 'true'}
                    ],

    case kz_datamgr:update_doc(AccountDb, AccountId, UpdateOptions) of
        {'error', _}=E -> E;
        {'ok', AccountDoc} ->
            handle_saved_accounts_doc(AccountDoc
                                     ,kz_datamgr:update_doc(?KZ_ACCOUNTS_DB, AccountId, UpdateOptions)
                                     )
    end.

%% @equiv is_in_account_hierarchy(CheckFor, InAccount, false)

-spec is_in_account_hierarchy(kz_term:api_binary(), kz_term:api_binary()) -> boolean().
is_in_account_hierarchy(CheckFor, InAccount) ->
    is_in_account_hierarchy(CheckFor, InAccount, 'false').

%%------------------------------------------------------------------------------
%% @doc Determine if the given account ID/DB exists in the hierarchy of
%% the provided account ID/DB. Optionally consider the account in
%% its own hierarchy if third argument is `true'.
%% @end
%%------------------------------------------------------------------------------

-spec is_in_account_hierarchy(kz_term:api_binary(), kz_term:api_binary(), boolean()) -> boolean().
is_in_account_hierarchy('undefined', _, _) -> 'false';
is_in_account_hierarchy(_, 'undefined', _) -> 'false';
is_in_account_hierarchy(CheckFor, InAccount, IncludeSelf) ->
    CheckId = kz_util:format_account_id(CheckFor),
    AccountId = kz_util:format_account_id(InAccount),
    case (IncludeSelf
          andalso AccountId =:= CheckId
         )
        orelse fetch(AccountId)
    of
        'true' ->
            lager:debug("account ~s is the same as the account to fetch the hierarchy from", [CheckId]),
            'true';
        {'ok', JObj} ->
            Tree = tree(JObj),
            case lists:member(CheckId, Tree) of
                'true' ->
                    lager:debug("account ~s is in the account hierarchy of ~s", [CheckId, AccountId]),
                    'true';
                'false' ->
                    lager:debug("account ~s was not found in the account hierarchy of ~s", [CheckId, AccountId]),
                    'false'
            end;
        {'error', _R} ->
            lager:debug("failed to get the ancestry of the account ~s: ~p", [AccountId, _R]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Normalize the account name by converting the name to lower case
%% and then removing all non-alphanumeric characters.
%%
%% This can possibly return an empty binary.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_name(kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
normalize_name('undefined') -> 'undefined';
normalize_name(AccountName) ->
    << <<Char>>
       || <<Char>> <= kz_term:to_lower_binary(AccountName),
          is_alphanumeric(Char)
    >>.

is_alphanumeric(Char)
  when Char >= $a,
       Char =< $z ->
    'true';
is_alphanumeric(Char)
  when Char >= $0,
       Char =< $9 ->
    'true';
is_alphanumeric(_) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc Validate a requested account can be created
%%
%% Returns the updated account doc (with relevant defaults)
%% or returns the validation error {Path, ErrorType, ErrorMessage}
%% @end
%%------------------------------------------------------------------------------
-type validation_error() :: {kz_json:path(), kz_term:ne_binary(), kz_json:object()}.
-type validation_errors() :: [validation_error()].
-spec validate(kz_term:api_ne_binary(), kz_term:api_ne_binary(), doc()) ->
                      {'true', doc()} |
                      {'validation_errors', validation_errors()} |
                      {'system_error', atom()}.
validate(ParentId, AccountId, ReqJObj) ->
    ValidateFuns = [fun ensure_account_has_realm/2
                   ,fun ensure_account_has_timezone/2
                   ,fun remove_spaces/2
                   ,fun cleanup_leaky_keys/2
                   ,fun validate_realm_is_unique/2
                   ,fun validate_account_name_is_unique/2
                   ,fun(AID, Acc) ->  validate_schema(ParentId, AID, Acc) end
                   ,fun normalize_alphanum_name/2
                   ],
    try do_validation(AccountId, ReqJObj, ValidateFuns) of
        {AccountDoc, []} -> {'true', AccountDoc};
        {_AccountDoc, ValidationErrors} -> {'validation_errors', ValidationErrors}
    catch
        'throw':SystemError -> SystemError
    end.

-type validate_acc() :: {doc(), validation_errors()}.
-type validate_fun() :: fun((kz_term:api_ne_binary(), validate_acc()) -> validate_acc()).

-spec do_validation(kz_term:api_ne_binary(), doc(), [validate_fun()]) ->
                           {'true', doc()} |
                           {'validation_errors', validation_errors()}.
do_validation(AccountId, ReqJObj, ValidateFuns) ->
    lists:foldl(fun(F, Acc) -> F(AccountId, Acc) end
               ,{ReqJObj, []}
               ,ValidateFuns
               ).

-spec ensure_account_has_realm(kz_term:api_ne_binary(), validate_acc()) -> validate_acc().
ensure_account_has_realm(_AccountId, {Doc, Errors}) ->
    case realm(Doc) of
        'undefined' ->
            Realm = random_realm(),
            lager:debug("doc has no realm, creating random realm '~s'", [Realm]),
            {set_realm(Doc, Realm), Errors};
        _Realm ->
            lager:debug("doc has realm '~s'", [_Realm]),
            {Doc, Errors}
    end.

-spec ensure_account_has_timezone(kz_term:api_ne_binary(), validate_acc()) -> validate_acc().
ensure_account_has_timezone(_AccountId, {Doc, Errors}) ->
    Timezone = timezone(Doc),
    lager:debug("selected timezone: ~s", [Timezone]),
    {set_timezone(Doc, Timezone), Errors}.

-spec random_realm() -> kz_term:ne_binary().
random_realm() ->
    <<(kz_binary:rand_hex(?RANDOM_REALM_STRENGTH))/binary, ".", (?ACCOUNT_REALM_SUFFIX)/binary>>.

-spec remove_spaces(kz_term:api_ne_binary(), validate_acc()) -> validate_acc().
remove_spaces(_AccountId, {Doc, Errors}) ->
    {lists:foldl(fun remove_spaces_fold/2, Doc, ?REMOVE_SPACES)
    ,Errors
    }.

-spec remove_spaces_fold(kz_json:path(), doc()) -> doc().
remove_spaces_fold(Key, Doc) ->
    case kz_json:get_ne_binary_value(Key, Doc) of
        'undefined' -> Doc;
        Value ->
            NoSpaces = binary:replace(Value, <<" ">>, <<>>, ['global']),
            kz_json:set_value(Key, NoSpaces, Doc)
    end.

-spec cleanup_leaky_keys(kz_term:api_ne_binary(), validate_acc()) -> validate_acc().
cleanup_leaky_keys(_AccountId, {Doc, Errors}) ->
    RemoveKeys = [<<"wnm_allow_additions">>
                 ,<<"superduper_admin">>
                 ,<<"billing_mode">>
                 ],
    {kz_json:delete_keys(RemoveKeys, Doc)
    ,Errors
    }.

-spec validate_realm_is_unique(kz_term:api_ne_binary(), validate_acc()) -> validate_acc().
validate_realm_is_unique(AccountId, {Doc, Errors}) ->
    Realm = realm(Doc),
    case is_unique_realm(AccountId, Realm) of
        'true' ->
            lager:debug("realm ~s is indeed unique", [Realm]),
            {Doc, Errors};
        'false' ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"Account realm already in use">>}
                    ,{<<"cause">>, Realm}
                    ]),
            {Doc, [{[<<"realm">>], <<"unique">>, Msg} | Errors]}
    end.


%%------------------------------------------------------------------------------
%% @doc This function will determine if the realm in the request is
%% unique or belongs to the request being made
%% @end
%%------------------------------------------------------------------------------
-spec is_unique_realm(kz_term:api_binary(), kz_term:ne_binary()) -> boolean().
is_unique_realm(AccountId, Realm) ->
    ViewOptions = [{'key', kz_term:to_lower_binary(Realm)}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?AGG_VIEW_REALM, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> kz_doc:id(JObj) =:= AccountId;
        {'error', 'not_found'} -> 'true';
        _Else -> 'false'
    end.

-spec validate_account_name_is_unique(kz_term:api_ne_binary(), validate_acc()) -> validate_acc().
validate_account_name_is_unique(AccountId, {Doc, Errors}) ->
    Name = name(Doc),
    case maybe_is_unique_account_name(AccountId, Name) of
        'true' ->
            lager:debug("name ~s is indeed unique", [Name]),
            {Doc, Errors};
        'false' ->
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"Account name already in use">>}
                    ,{<<"cause">>, Name}
                    ]),
            {Doc, [{[<<"name">>], <<"unique">>, Msg} | Errors]}
    end.

%%------------------------------------------------------------------------------
%% @doc This function will determine if the account name is unique
%% @end
%%------------------------------------------------------------------------------
-spec maybe_is_unique_account_name(kz_term:api_binary(), kz_term:ne_binary()) -> boolean().
maybe_is_unique_account_name(AccountId, Name) ->
    case kapps_config:get_is_true(?ACCOUNTS_CONFIG_CAT, <<"ensure_unique_name">>, 'true') of
        'true' -> is_unique_account_name(AccountId, Name);
        'false' -> 'true'
    end.

-spec is_unique_account_name(kz_term:api_ne_binary(), kz_term:ne_binary()) -> boolean().
is_unique_account_name(AccountId, Name) ->
    AccountName = normalize_name(Name),
    ViewOptions = [{'key', AccountName}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?AGG_VIEW_NAME, ViewOptions) of
        {'ok', []} -> 'true';
        {'error', 'not_found'} -> 'true';
        {'ok', [JObj|_]} -> kz_doc:id(JObj) =:= AccountId;
        _Else ->
            lager:error("error ~p checking view ~p in ~p", [_Else, ?AGG_VIEW_NAME, ?KZ_ACCOUNTS_DB]),
            'false'
    end.

-spec validate_schema(kz_term:api_ne_binary(), kz_term:api_ne_binary(), validate_acc()) -> validate_acc().
validate_schema(ParentId, AccountId, {Doc, Errors}) ->
    lager:debug("validating payload against schema"),
    SchemaRequired = ?SHOULD_ENSURE_SCHEMA_IS_VALID,

    case kz_json_schema:load(<<"accounts">>) of
        {'ok', SchemaJObj} -> validate_account_schema(ParentId, AccountId, Doc, Errors, SchemaJObj);
        {'error', 'not_found'} when SchemaRequired ->
            lager:error("accounts schema not found and is required"),
            throw({'system_error', <<"schema accounts not found.">>});
        {'error', 'not_found'} ->
            lager:error("accounts schema not found, continuing anyway"),
            validate_passed(ParentId, AccountId, {Doc, Errors})
    end.

-spec validate_passed(kz_term:api_ne_binary(), kz_term:api_ne_binary(), validate_acc()) -> validate_acc().
validate_passed(ParentId, 'undefined', {Doc, Errors}) ->
    lager:info("validation passed for new account: ~s", [kz_json:encode(Doc)]),
    {set_private_properties(ParentId, Doc), Errors};
validate_passed(_ParentId, _AccountId, {Doc, Errors}) ->
    {Doc, Errors}.

-spec validate_account_schema(kz_term:api_ne_binary(), kz_term:api_ne_binary(), doc(), validation_errors(), kz_json:object()) ->
                                     validate_acc().
validate_account_schema(ParentId, AccountId, Doc, ValidationErrors, SchemaJObj) ->
    Strict = ?SHOULD_FAIL_ON_INVALID_DATA,
    SystemSL = kapps_config:get_binary(<<"crossbar">>, <<"stability_level">>),
    Options = [{'extra_validator_options', [{'stability_level', SystemSL}]}],

    try kz_json_schema:validate(SchemaJObj, kz_doc:public_fields(Doc), Options) of
        {'ok', JObj} ->
            lager:debug("account payload is valid"),
            validate_passed(ParentId, AccountId, {JObj, ValidationErrors});
        {'error', SchemaErrors} when Strict ->
            lager:error("validation errors when strictly validating"),
            validate_failed(Doc, ValidationErrors, SchemaErrors);
        {'error', SchemaErrors} ->
            lager:error("validation errors but not strictly validating, trying to fix request"),
            maybe_fix_js_types(ParentId, AccountId, Doc, ValidationErrors, SchemaErrors, SchemaJObj)
    catch
        ?STACKTRACE('error', 'function_clause', ST)
        lager:error("function clause failure"),
        kz_log:log_stacktrace(ST),
        throw({'system_error', <<"validation failed to run on the server">>})
        end.

-spec maybe_fix_js_types(kz_term:api_ne_binary(), kz_term:api_ne_binary(), doc(), validation_errors(), [jesse_error:error_reason()], kz_json:object()) ->
                                validate_acc().
maybe_fix_js_types(ParentId, AccountId, Doc, ValidationErrors, SchemaErrors, SchemaJObj) ->
    case kz_json_schema:fix_js_types(Doc, SchemaErrors) of
        'false' -> validate_failed(Doc, ValidationErrors, SchemaErrors);
        {'true', NewDoc} ->
            validate_account_schema(ParentId, AccountId, NewDoc, ValidationErrors, SchemaJObj)
    end.

-spec validate_failed(doc(), validation_errors(), [jesse_error:error_reason()]) -> validate_acc().
validate_failed(Doc, ValidationErrors, SchemaErrors) ->
    {Doc
    ,[validation_error(Error) || Error <- SchemaErrors] ++ ValidationErrors
    }.

-spec validation_error(jesse_error:error_reason()) -> validation_error().
validation_error(Error) ->
    {_ErrorCode, ErrorMessage, ErrorJObj} = kz_json_schema:error_to_jobj(Error),
    [Key] = kz_json:get_keys(ErrorJObj),
    {[JObj], [_Code]} = kz_json:get_values(Key, ErrorJObj),
    lager:info("adding error prop ~s ~s: ~p", [Key, ErrorMessage, JObj]),
    {Key, ErrorMessage, JObj}.

-spec normalize_alphanum_name(kz_term:api_ne_binary(), validate_acc()) -> validate_acc().
normalize_alphanum_name(_AccountId, {Doc, Errors}) ->
    Normalized = normalize_name(name(Doc)),
    {kz_json:set_value(<<"pvt_alphanum_name">>, Normalized, Doc)
    ,Errors
    }.

%%------------------------------------------------------------------------------
%% @doc This function returns the private fields to be added to a new account
%% document
%% @end
%%------------------------------------------------------------------------------
-spec set_private_properties(kz_term:api_ne_binary(), doc()) -> doc().
set_private_properties(ParentId, Doc) ->
    PvtFuns = [fun add_pvt_type/1
              ,fun add_pvt_vsn/1
              ,fun maybe_add_pvt_api_key/1
              ,fun(D) ->  maybe_add_pvt_tree(ParentId, D) end
              ,fun maybe_set_cpaas_token/1
              ,fun add_pvt_enabled/1
              ],
    lists:foldl(fun(F, D) -> F(D) end, Doc, PvtFuns).

-spec add_pvt_type(doc()) -> doc().
add_pvt_type(Doc) ->
    kz_doc:set_type(Doc, type()).

-spec add_pvt_vsn(doc()) -> doc().
add_pvt_vsn(Doc) ->
    kz_doc:set_vsn(Doc, <<"1">>).

-spec add_pvt_enabled(doc()) -> doc().
add_pvt_enabled(Doc) ->
    case lists:reverse(tree(Doc)) of
        [] -> Doc;
        [ParentId | _] ->
            add_pvt_enabled(Doc, ParentId, (not kz_term:is_empty(ParentId)))
    end.

-spec add_pvt_enabled(doc(), kz_term:api_ne_binary(), boolean()) -> doc().
add_pvt_enabled(Doc, _ParentId, 'false') -> Doc;
add_pvt_enabled(Doc, ParentId, 'true') ->
    case fetch(ParentId) of
        {'ok', Parent} ->
            case is_enabled(Parent) of
                'true'  -> enable(Doc);
                'false' -> disable(Doc)
            end;
        _Else -> Doc
    end.

-spec maybe_add_pvt_api_key(doc()) -> doc().
maybe_add_pvt_api_key(Doc) ->
    case api_key(Doc) of
        'undefined' -> add_pvt_api_key(Doc);
        _Else -> Doc
    end.

-spec add_pvt_api_key(doc()) -> doc().
add_pvt_api_key(Doc) ->
    APIKey = kz_term:to_hex_binary(crypto:strong_rand_bytes(32)),
    set_api_key(Doc, APIKey).

-spec maybe_set_cpaas_token(doc()) -> doc().
maybe_set_cpaas_token(AccountDoc) ->
    case cpaas_token(AccountDoc) of
        'undefined' -> set_cpaas_token(AccountDoc);
        _Token -> AccountDoc
    end.

-spec cpaas_token(doc()) -> kz_term:api_ne_binary().
cpaas_token(AccountDoc) ->
    kz_json:get_ne_binary_value(path_cpaas_token(), AccountDoc).

-spec path_cpaas_token() -> kz_json:path().
path_cpaas_token() ->
    [<<"pvt_cpaas_token">>].

-spec set_cpaas_token(doc()) -> doc().
set_cpaas_token(AccountDoc) ->
    APIKey = kz_term:to_hex_binary(crypto:strong_rand_bytes(32)),
    set_cpaas_token(AccountDoc, APIKey).

-spec set_cpaas_token(doc(), kz_term:ne_binary()) -> doc().
set_cpaas_token(AccountDoc, Token) ->
    kz_json:set_value(path_cpaas_token(), Token, AccountDoc).

-spec maybe_add_pvt_tree(kz_term:api_ne_binary(), doc()) -> doc().
maybe_add_pvt_tree(ParentId, Doc) ->
    case tree(Doc) of
        [_|_]=_Tree ->
            lager:info("tree already defined: ~p", [_Tree]),
            Doc;
        _Else ->
            add_pvt_tree(ParentId, Doc)
    end.

-spec add_pvt_tree(kz_term:api_ne_binary(), doc()) -> doc().
add_pvt_tree(ParentId, Doc) ->
    case create_new_tree(ParentId, Doc) of
        'error' -> throw({'system_error', 'empty_tree_accounts_exist'});
        Tree -> set_tree(Doc, Tree)
    end.

-spec create_new_tree(kz_term:api_ne_binary(), doc()) -> kz_term:ne_binaries() | 'error'.
create_new_tree('undefined', Doc) ->
    case kz_json:get_ne_binary_value(<<"pvt_parent_id">>, Doc) of
        'undefined' -> create_tree_from_master();
        ParentId -> create_tree_from_parent(ParentId)
    end;
create_new_tree(ParentId, _Doc) ->
    create_tree_from_parent(ParentId).

create_tree_from_master() ->
    case kapps_util:get_master_account_id() of
        {'ok', MasterAccountId} ->
            lager:info("using master account ~s as tree", [MasterAccountId]),
            [MasterAccountId];
        {'error', _} ->
            case kapps_util:get_all_accounts() of
                [] -> [];
                _Else -> 'error'
            end
    end.

create_tree_from_parent(ParentId) ->
    case fetch(ParentId) of
        {'error', _E} ->
            lager:info("failed to create tree from parent ~s: ~p", [ParentId, _E]),
            create_tree_from_master();
        {'ok', ParentJObj} ->
            lager:info("appending parent to tree"),
            tree(ParentJObj) ++ [ParentId]
    end.
