%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_account).

-export([new/0
         ,type/0
         ,id/1
         ,fetch/1

         ,name/1, name/2, set_name/2
         ,realm/1, realm/2, set_realm/2
         ,language/1, set_language/2
         ,timezone/1, timezone/2, set_timezone/2
         ,parent_account_id/1
         ,tree/1, tree/2 ,set_tree/2
         ,notification_preference/1, set_notification_preference/2
         ,is_enabled/1, enable/1, disable/1
         ,api_key/1, set_api_key/2
         ,is_superduper_admin/1, set_superduper_admin/2
         ,allow_number_additions/1, set_allow_number_additions/2
         ,trial_expiration/1, trial_expiration/2, set_trial_expiration/2
         ,trial_time_left/1, trial_time_left/2
         ,trial_has_expired/1, trial_has_expired/2
         ,is_expired/1
         ,is_trial_account/1
         ,is_reseller/1, promote/1, demote/1
         ,reseller_id/1, set_reseller_id/2

         ,dial_plan/1, dial_plan/2
         ,fax_settings/1
         ,low_balance_threshold/1, low_balance_threshold/2, set_low_balance_threshold/2
         ,low_balance_sent/1, set_low_balance_sent/1, reset_low_balance_sent/1
         ,low_balance_enabled/1, set_low_balance_enabled/1, reset_low_balance_enabled/1, low_balance_enabled_exists/1
         ,low_balance_tstamp/1, set_low_balance_tstamp/1, set_low_balance_tstamp/2, remove_low_balance_tstamp/1
         ,topup_threshold/1, topup_threshold/2, set_topup_threshold/2
        ]).

-include("kz_documents.hrl").

-define(ID, <<"_id">>).
-define(NAME, <<"name">>).
-define(REALM, <<"realm">>).
-define(LANGUAGE, <<"language">>).
-define(TIMEZONE, <<"timezone">>).
-define(THRESHOLD, <<"threshold">>).
-define(TREE, <<"pvt_tree">>).
-define(IS_ENABLED, <<"pvt_enabled">>).
-define(API_KEY, <<"pvt_api_key">>).
-define(IS_SUPERDUPER_ADMIN, <<"pvt_superduper_admin">>).
-define(ALLOW_NUMBER_ADDITIONS, <<"pvt_wnm_allow_additions">>).
-define(NOTIFY_PREF, <<"pvt_notification_preference">>).
-define(KEY_TRIAL_EXPIRATION, <<"pvt_trial_expires">>).
-define(KEY_TRIAL_ACCOUNT, <<"is_trial_account">>).
-define(RESELLER, <<"pvt_reseller">>).
-define(RESELLER_ID, <<"pvt_reseller_id">>).
-define(KEY_DIAL_PLAN, <<"dial_plan">>).
-define(LOW_BALANCE_SENT, [<<"notifications">>, <<"low_balance">>, <<"sent_low_balance">>]).
-define(LOW_BALANCE_ENABLED, [<<"notifications">>, <<"low_balance">>, <<"enabled">>]).
-define(LOW_BALANCE_THRESHOLD, [<<"notifications">>, <<"low_balance">>, <<"threshold">>]).
-define(LOW_BALANCE_TSTAMP, [<<"notifications">>, <<"low_balance">>, <<"last_notification">>]).
-define(TOPUP_THRESHOLD, [<<"topup">>, <<"threshold">>]).

-define(PVT_TYPE, <<"account">>).

-type doc() :: kz_json:object().
-export_type([doc/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json:new(), ?PVT_TYPE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec type() -> ne_binary().
type() -> ?PVT_TYPE.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec id(doc()) -> api_binary().
id(JObj) ->
    kz_doc:id(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(api_binary()) -> {'ok', doc()} |
                             {'error', any()}.
fetch('undefined') ->
    {'error', 'invalid_db_name'};
fetch(<<_/binary>> = Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    kz_datamgr:open_cache_doc(AccountDb, AccountId, ['cache_failures']).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec name(doc()) -> api_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(JObj) ->
    name(JObj, 'undefined').
name(JObj, Default) ->
    kz_json:get_value(?NAME, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_name(doc(), ne_binary()) -> doc().
set_name(JObj, Name) ->
    kz_json:set_value(?NAME, Name, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec realm(doc()) -> api_binary().
-spec realm(doc(), Default) -> ne_binary() | Default.
realm(JObj) ->
    realm(JObj, 'undefined').
realm(JObj, Default) ->
    kz_json:get_ne_value(?REALM, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_realm(doc(), ne_binary()) -> doc().
set_realm(JObj, Realm) ->
    kz_json:set_value(?REALM, Realm, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec language(doc()) -> api_binary().
language(JObj) ->
    kz_json:get_value(?LANGUAGE, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_language(doc(), ne_binary()) -> doc().
set_language(JObj, Language) ->
    kz_json:set_value(?LANGUAGE, Language, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec timezone(ne_binary() | doc()) -> api_binary().
-spec timezone(ne_binary() | doc(), Default) -> ne_binary() | Default.
timezone(AccountId)
  when is_binary(AccountId) ->
    {'ok', JObj} = fetch(AccountId),
    timezone(JObj);
timezone(JObj) ->
    timezone(JObj, 'undefined').
timezone(AccountId, Default)
  when is_binary(AccountId) ->
    {'ok', JObj} = fetch(AccountId),
    timezone(JObj, Default);
timezone(JObj, Default) ->
    case kz_json:get_value(?TIMEZONE, JObj, Default) of
        <<"inherit">> -> parent_timezone(kz_doc:account_id(JObj), parent_account_id(JObj));
        'undefined' -> parent_timezone(kz_doc:account_id(JObj), parent_account_id(JObj));
        TZ -> TZ
    end.

-spec parent_timezone(ne_binary(), api_binary()) -> ne_binary().
parent_timezone(AccountId, AccountId) -> ?DEFAULT_TIMEZONE;
parent_timezone(_AccountId, 'undefined') -> ?DEFAULT_TIMEZONE;
parent_timezone(_AccountId, ParentId) -> timezone(ParentId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_timezone(doc(), ne_binary()) -> doc().
set_timezone(JObj, Timezone) ->
    kz_json:set_value(?TIMEZONE, Timezone, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_threshold(doc()) -> api_float().
low_balance_threshold(JObj) ->
    low_balance_threshold(JObj, 'undefined').

-spec low_balance_threshold(doc(), Default) -> float() | Default.
low_balance_threshold(JObj, Default) ->
    case kz_json:get_float_value(?LOW_BALANCE_THRESHOLD, JObj) of
        'undefined' -> topup_threshold(JObj, Default);
        Threshold -> Threshold
    end.

-spec set_low_balance_threshold(doc(), float()) -> doc().
set_low_balance_threshold(JObj, Threshold) ->
    kz_json:set_value(?LOW_BALANCE_THRESHOLD, Threshold, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_sent(doc()) -> boolean().
low_balance_sent(JObj) ->
    kz_json:is_true(?LOW_BALANCE_SENT, JObj).

-spec set_low_balance_sent(doc()) -> doc().
set_low_balance_sent(JObj) ->
    kz_json:set_value(?LOW_BALANCE_SENT, 'true', JObj).

-spec reset_low_balance_sent(doc()) -> doc().
reset_low_balance_sent(JObj) ->
    kz_json:set_value(?LOW_BALANCE_SENT, 'false', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_enabled(doc()) -> boolean().
low_balance_enabled(JObj) ->
    kz_json:is_true(?LOW_BALANCE_ENABLED, JObj).

-spec set_low_balance_enabled(doc()) -> doc().
set_low_balance_enabled(JObj) ->
    kz_json:set_value(?LOW_BALANCE_ENABLED, 'true', JObj).

-spec reset_low_balance_enabled(doc()) -> doc().
reset_low_balance_enabled(JObj) ->
    kz_json:set_value(?LOW_BALANCE_ENABLED, 'false', JObj).

-spec low_balance_enabled_exists(doc()) -> boolean().
low_balance_enabled_exists(JObj) ->
    kz_json:get_ne_value(?LOW_BALANCE_ENABLED, JObj) =/= 'undefined'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec low_balance_tstamp(doc()) -> api_number().
low_balance_tstamp(JObj) ->
    kz_json:get_integer_value(?LOW_BALANCE_TSTAMP, JObj).

-spec set_low_balance_tstamp(doc()) -> doc().
set_low_balance_tstamp(JObj) ->
    TStamp = kz_util:current_tstamp(),
    set_low_balance_tstamp(JObj, TStamp).

-spec set_low_balance_tstamp(doc(), number()) -> doc().
set_low_balance_tstamp(JObj, TStamp) ->
    kz_json:set_value(?LOW_BALANCE_TSTAMP, TStamp, JObj).

-spec remove_low_balance_tstamp(doc()) -> doc().
remove_low_balance_tstamp(JObj) ->
    kz_json:delete_key(?LOW_BALANCE_TSTAMP, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec topup_threshold(doc()) -> api_float().
topup_threshold(JObj) ->
    topup_threshold(JObj, 'undefined').

-spec topup_threshold(doc(), Default) -> float() | Default.
topup_threshold(JObj, Default) ->
    kz_json:get_float_value(?TOPUP_THRESHOLD, JObj, Default).

-spec set_topup_threshold(doc(), float()) -> doc().
set_topup_threshold(JObj, Threshold) ->
    kz_json:set_value(?TOPUP_THRESHOLD, Threshold, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parent_account_id(doc()) -> api_binary().
parent_account_id(JObj) ->
    case tree(JObj) of
        [] -> 'undefined';
        Ancestors -> lists:last(Ancestors)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tree(doc()) -> ne_binaries().
-spec tree(doc(), Default) -> ne_binaries() | Default.
tree(JObj) ->
    tree(JObj, []).
tree(JObj, Default) ->
    kz_json:get_list_value(?TREE, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_tree(doc(), ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    kz_json:set_value(?TREE, Tree, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec notification_preference(doc()) -> api_binary().
notification_preference(JObj) ->
    kz_json:get_value(?NOTIFY_PREF, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_notification_preference(doc(), ne_binary()) -> doc().
set_notification_preference(JObj, Pref) ->
    kz_json:set_value(?NOTIFY_PREF, Pref, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(doc()) -> boolean().
is_enabled(JObj) ->
    kz_json:is_true(?IS_ENABLED, JObj, 'true').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec enable(doc()) -> doc().
enable(JObj) ->
    kz_json:set_value(?IS_ENABLED, 'true', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec disable(doc()) -> doc().
disable(JObj) ->
    kz_json:set_value(?IS_ENABLED, 'false', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec api_key(doc()) -> api_binary().
api_key(JObj) ->
    kz_json:get_value(?API_KEY, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_api_key(doc(), ne_binary()) -> doc().
set_api_key(JObj, ApiKey) ->
    kz_json:set_value(?API_KEY, ApiKey, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_superduper_admin(doc()) -> boolean().
is_superduper_admin(JObj) ->
    kz_json:is_true(?IS_SUPERDUPER_ADMIN, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_superduper_admin(doc(), boolean()) -> doc().
set_superduper_admin(JObj, IsAdmin) ->
    kz_json:set_value(?IS_SUPERDUPER_ADMIN, IsAdmin, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec allow_number_additions(doc()) -> boolean().
allow_number_additions(JObj) ->
    kz_json:is_true(?ALLOW_NUMBER_ADDITIONS, JObj).

-spec set_allow_number_additions(doc(), boolean()) -> doc().
set_allow_number_additions(JObj, IsAllowed) ->
    kz_json:set_value(?ALLOW_NUMBER_ADDITIONS, kz_util:is_true(IsAllowed), JObj).

-spec trial_expiration(doc()) -> api_integer().
-spec trial_expiration(doc(), Default) -> integer() | Default.
trial_expiration(JObj) ->
    trial_expiration(JObj, 'undefined').

trial_expiration(JObj, Default) ->
    kz_json:get_integer_value(?KEY_TRIAL_EXPIRATION, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_trial_expiration(doc(), gregorian_seconds()) -> doc().
set_trial_expiration(JObj, Expiration) ->
    JObj1 = kz_json:delete_key(?KEY_TRIAL_ACCOUNT, JObj),
    kz_json:set_value(?KEY_TRIAL_EXPIRATION, Expiration, JObj1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec trial_time_left(doc()) -> integer().
-spec trial_time_left(doc(), gregorian_seconds()) -> integer().
trial_time_left(JObj) ->
    trial_time_left(JObj, kz_util:current_tstamp()).
trial_time_left(JObj, Now) ->
    case trial_expiration(JObj) of
        'undefined' -> 0;
        Expiration -> kz_util:elapsed_s(Now, Expiration)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec trial_has_expired(doc()) -> boolean().
trial_has_expired(JObj) ->
    trial_has_expired(JObj, kz_util:current_tstamp()).
trial_has_expired(JObj, Now) ->
    trial_expiration(JObj) =/= 'undefined' andalso
        trial_time_left(JObj, Now) =< 0.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_expired(doc()) -> 'false' | {'true', gregorian_seconds()}.
is_expired(JObj) ->
    case trial_has_expired(JObj) of
        'false' -> 'false';
        'true' ->
            {'true', kz_account:trial_expiration(JObj)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_trial_account(doc()) -> boolean().
is_trial_account(JObj) ->
    kz_json:is_true(?KEY_TRIAL_ACCOUNT, JObj, 'false').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_reseller(doc()) -> boolean().
is_reseller(JObj) ->
    kz_json:is_true(?RESELLER, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec promote(doc()) -> doc().
promote(JObj) ->
    kz_json:set_value(?RESELLER, 'true', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec demote(doc()) -> doc().
demote(JObj) ->
    io:format("demote~n", []),
    kz_json:set_value(?RESELLER, 'false', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reseller_id(doc()) -> doc().
reseller_id(JObj) ->
    kz_json:get_value(?RESELLER_ID, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_reseller_id(doc(), ne_binary()) -> doc().
set_reseller_id(JObj, ResellerId) ->
    kz_json:set_value(?RESELLER_ID, ResellerId, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dial_plan(doc()) -> api_object().
-spec dial_plan(doc(), Default) -> kz_json:object() | Default.
dial_plan(JObj) ->
    dial_plan(JObj, 'undefined').
dial_plan(JObj, Default) ->
    kz_json:get_json_value(?KEY_DIAL_PLAN, JObj, Default).

-spec fax_settings(doc() | ne_binary()) -> doc().
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
