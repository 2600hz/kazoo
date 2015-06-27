%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_account).

-export([name/1, name/2, set_name/2
         ,realm/1, realm/2, set_realm/2
         ,language/1, set_language/2
         ,timezone/1, timezone/2, set_timezone/2
         ,id/1
         ,parent_account_id/1
         ,set_tree/2, tree/1, tree/2
         ,notification_preference/1, set_notification_preference/2
         ,is_enabled/1, enable/1, disable/1
         ,api_key/1, set_api_key/2
         ,is_superduper_admin/1
         ,allow_number_additions/1

         ,fetch/1
        ]).

-define(ID, <<"_id">>).
-define(NAME, <<"name">>).
-define(REALM, <<"realm">>).
-define(LANGUAGE, <<"language">>).
-define(TIMEZONE, <<"timezone">>).
-define(TREE, <<"pvt_tree">>).
-define(IS_ENABLED, <<"pvt_enabled">>).
-define(API_KEY, <<"pvt_api_key">>).
-define(IS_SUPERDUPER_ADMIN, <<"pvt_superduper_admin">>).
-define(ALLOW_NUMBER_ADDITIONS, <<"pvt_wnm_allow_additions">>).
-define(NOTIFY_PREF, <<"pvt_notification_preference">>).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-spec id(doc()) -> api_binary().
id(JObj) ->
    wh_doc:id(JObj).

-spec name(doc()) -> api_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(JObj) ->
    name(JObj, 'undefined').
name(JObj, Default) ->
    wh_json:get_value(?NAME, JObj, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(JObj, Name) ->
    wh_json:set_value(?NAME, Name, JObj).

-spec realm(doc()) -> api_binary().
-spec realm(doc(), Default) -> ne_binary() | Default.
realm(JObj) ->
    realm(JObj, 'undefined').
realm(JObj, Default) ->
    wh_json:get_ne_value(?REALM, JObj, Default).

-spec set_realm(doc(), ne_binary()) -> doc().
set_realm(JObj, Realm) ->
    wh_json:set_value(?REALM, Realm, JObj).

-spec language(doc()) -> api_binary().
language(JObj) ->
    wh_json:get_value(?LANGUAGE, JObj).

-spec set_language(doc(), ne_binary()) -> doc().
set_language(JObj, Language) ->
    wh_json:set_value(?LANGUAGE, Language, JObj).

-spec timezone(doc()) -> api_binary().
-spec timezone(doc(), Default) -> ne_binary() | Default.
timezone(JObj) ->
    timezone(JObj, 'undefined').
timezone(JObj, Default) ->
    wh_json:get_value(?TIMEZONE, JObj, Default).

-spec set_timezone(doc(), ne_binary()) -> doc().
set_timezone(JObj, Timezone) ->
    wh_json:set_value(?TIMEZONE, Timezone, JObj).

-spec parent_account_id(doc()) -> api_binary().
parent_account_id(JObj) ->
    case tree(JObj) of
        [] -> 'undefined';
        Ancestors -> lists:last(Ancestors)
    end.

-spec tree(doc()) -> ne_binaries().
-spec tree(doc(), Default) -> ne_binaries() | Default.
tree(JObj) ->
    tree(JObj, []).
tree(JObj, Default) ->
    wh_json:get_list_value(?TREE, JObj, Default).

-spec set_tree(doc(), ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    wh_json:set_value(?TREE, Tree, JObj).

-spec notification_preference(doc()) -> api_binary().
notification_preference(JObj) ->
    wh_json:get_value(?NOTIFY_PREF, JObj).

-spec set_notification_preference(doc(), ne_binary()) -> doc().
set_notification_preference(JObj, Pref) ->
    wh_json:set_value(?NOTIFY_PREF, Pref, JObj).

-spec is_enabled(doc()) -> boolean().
is_enabled(JObj) ->
    wh_json:is_true(?IS_ENABLED, JObj, 'true').

-spec enable(doc()) -> doc().
enable(JObj) ->
    wh_json:set_value(?IS_ENABLED, 'true', JObj).

-spec disable(doc()) -> doc().
disable(JObj) ->
    wh_json:set_value(?IS_ENABLED, 'false', JObj).

-spec api_key(doc()) -> api_binary().
api_key(JObj) ->
    wh_json:get_value(?API_KEY, JObj).

-spec set_api_key(doc(), ne_binary()) -> doc().
set_api_key(JObj, ApiKey) ->
    wh_json:set_value(?API_KEY, ApiKey, JObj).

-spec is_superduper_admin(doc()) -> boolean().
is_superduper_admin(JObj) ->
    wh_json:is_true(?IS_SUPERDUPER_ADMIN, JObj).

-spec allow_number_additions(doc()) -> boolean().
allow_number_additions(JObj) ->
    wh_json:is_true(?ALLOW_NUMBER_ADDITIONS, JObj).

-spec fetch(ne_binary()) -> {'ok', doc()} |
                            {'error', _}.
fetch(<<_/binary>> = Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),

    couch_mgr:open_cache_doc(AccountDb, AccountId).
