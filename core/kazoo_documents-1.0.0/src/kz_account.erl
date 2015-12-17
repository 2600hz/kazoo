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
         ,threshold/2 ,set_threshold/2
         ,parent_account_id/1
         ,tree/1, tree/2 ,set_tree/2
         ,notification_preference/1, set_notification_preference/2
         ,is_enabled/1, enable/1, disable/1
         ,api_key/1, set_api_key/2
         ,is_superduper_admin/1, set_superduper_admin/2
         ,allow_number_additions/1, set_allow_number_additions/2
         ,trial_expiration/1, trial_expiration/2, set_trial_expiration/2
         ,trial_time_left/1, trial_time_left/2
         ,trial_has_expired/1, is_expired/1
         ,is_trial_account/1
        ]).

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

-define(PVT_TYPE, <<"account">>).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> doc().
new() ->
    wh_doc:set_type(wh_json:new(), ?PVT_TYPE).

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
    wh_doc:id(JObj).

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
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    couch_mgr:open_cache_doc(AccountDb, AccountId).

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
    wh_json:get_value(?NAME, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_name(doc(), ne_binary()) -> doc().
set_name(JObj, Name) ->
    wh_json:set_value(?NAME, Name, JObj).

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
    wh_json:get_ne_value(?REALM, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_realm(doc(), ne_binary()) -> doc().
set_realm(JObj, Realm) ->
    wh_json:set_value(?REALM, Realm, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec language(doc()) -> api_binary().
language(JObj) ->
    wh_json:get_value(?LANGUAGE, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_language(doc(), ne_binary()) -> doc().
set_language(JObj, Language) ->
    wh_json:set_value(?LANGUAGE, Language, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec timezone(doc()) -> api_binary().
-spec timezone(doc(), Default) -> ne_binary() | Default.
timezone(JObj) ->
    timezone(JObj, 'undefined').
timezone(JObj, Default) ->
    case wh_json:get_value(?TIMEZONE, JObj, Default) of
        <<"inherit">> -> Default;  %% UI-1808
        TZ -> TZ
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_timezone(doc(), ne_binary()) -> doc().
set_timezone(JObj, Timezone) ->
    wh_json:set_value(?TIMEZONE, Timezone, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec threshold(doc(), Default) -> float() | Default.
threshold(JObj, Default) ->
    wh_json:get_float_value(<<"threshold">>, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_threshold(doc(), float()) -> doc().
set_threshold(JObj, Threshold) ->
    wh_json:set_value(?THRESHOLD, Threshold, JObj).

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
    wh_json:get_list_value(?TREE, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_tree(doc(), ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    wh_json:set_value(?TREE, Tree, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec notification_preference(doc()) -> api_binary().
notification_preference(JObj) ->
    wh_json:get_value(?NOTIFY_PREF, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_notification_preference(doc(), ne_binary()) -> doc().
set_notification_preference(JObj, Pref) ->
    wh_json:set_value(?NOTIFY_PREF, Pref, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(doc()) -> boolean().
is_enabled(JObj) ->
    wh_json:is_true(?IS_ENABLED, JObj, 'true').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec enable(doc()) -> doc().
enable(JObj) ->
    wh_json:set_value(?IS_ENABLED, 'true', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec disable(doc()) -> doc().
disable(JObj) ->
    wh_json:set_value(?IS_ENABLED, 'false', JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec api_key(doc()) -> api_binary().
api_key(JObj) ->
    wh_json:get_value(?API_KEY, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_api_key(doc(), ne_binary()) -> doc().
set_api_key(JObj, ApiKey) ->
    wh_json:set_value(?API_KEY, ApiKey, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_superduper_admin(doc()) -> boolean().
is_superduper_admin(JObj) ->
    wh_json:is_true(?IS_SUPERDUPER_ADMIN, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_superduper_admin(doc(), boolean()) -> doc().
set_superduper_admin(JObj, IsAdmin) ->
    wh_json:set_value(?IS_SUPERDUPER_ADMIN, IsAdmin, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec allow_number_additions(doc()) -> boolean().
allow_number_additions(JObj) ->
    wh_json:is_true(?ALLOW_NUMBER_ADDITIONS, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_allow_number_additions(doc(), boolean()) -> doc().
set_allow_number_additions(JObj, IsAllowed) ->
    wh_json:set_value(?ALLOW_NUMBER_ADDITIONS, IsAllowed, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec trial_expiration(doc()) -> api_integer().
-spec trial_expiration(doc(), Default) -> integer() | Default.
trial_expiration(JObj) ->
    trial_expiration(JObj, 'undefined').

trial_expiration(JObj, Default) ->
    wh_json:get_integer_value(?KEY_TRIAL_EXPIRATION, JObj, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_trial_expiration(doc(), gregorian_seconds()) -> doc().
set_trial_expiration(JObj, Expiration) ->
    JObj1 = wh_json:delete_key(?KEY_TRIAL_ACCOUNT, JObj),
    wh_json:set_value(?KEY_TRIAL_EXPIRATION, Expiration, JObj1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec trial_time_left(doc()) -> integer().
-spec trial_time_left(doc(), gregorian_seconds()) -> integer().
trial_time_left(JObj) ->
    trial_time_left(JObj, wh_util:current_tstamp()).
trial_time_left(JObj, Now) ->
    case trial_expiration(JObj) of
        'undefined' -> 0;
        Expiration -> wh_util:elapsed_s(Now, Expiration)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec trial_has_expired(doc()) -> boolean().
trial_has_expired(JObj) ->
    trial_expiration(JObj) =/= 'undefined' andalso
        trial_time_left(JObj) =< 0.

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
    wh_json:is_true(?KEY_TRIAL_ACCOUNT, JObj, 'false').

%%%===================================================================
%%% Internal functions
%%%===================================================================
