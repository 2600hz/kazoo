%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_account).

-export([name/1, set_name/2
         ,realm/1, set_realm/2
         ,language/1, set_language/2
         ,timezone/1, set_timezone/2
         ,id/1
         ,parent_account_id/1
         ,set_tree/2, tree/1
         ,notification_preference/1, set_notification_preference/2
         ,is_enabled/1, enable/1, disable/1
         ,set_api_key/2, api_key/1
         ,is_superduper_admin/1
         ,allow_number_additions/1
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

id(JObj) ->
    wh_doc:id(JObj).

-spec name(wh_json:object()) -> api_binary().
name(JObj) ->
    wh_json:get_value(?NAME, JObj).

-spec set_name(wh_json:object(), ne_binary()) -> wh_json:object().
set_name(JObj, Name) ->
    wh_json:set_value(?NAME, Name, JObj).

-spec realm(wh_json:object()) -> api_binary().
realm(JObj) ->
    wh_json:get_ne_value(?REALM, JObj).

-spec set_realm(wh_json:object(), ne_binary()) -> wh_json:object().
set_realm(JObj, Realm) ->
    wh_json:set_value(?REALM, Realm, JObj).

-spec language(wh_json:object()) -> api_binary().
language(JObj) ->
    wh_json:get_value(?LANGUAGE, JObj).

-spec set_language(wh_json:object(), ne_binary()) -> wh_json:object().
set_language(JObj, Language) ->
    wh_json:set_value(?LANGUAGE, Language, JObj).

-spec timezone(wh_json:object()) -> api_binary().
timezone(JObj) ->
    wh_json:get_value(?TIMEZONE, JObj).

-spec set_timezone(wh_json:object(), ne_binary()) -> wh_json:object().
set_timezone(JObj, Timezone) ->
    wh_json:set_value(?TIMEZONE, Timezone, JObj).

-spec parent_account_id(wh_json:object()) -> api_binary().
parent_account_id(JObj) ->
    case tree(JObj) of
        [] -> 'undefined';
        Ancestors -> lists:last(Ancestors)
    end.

-spec tree(wh_json:object()) -> ne_binaries().
tree(JObj) ->
    wh_json:get_value(?TREE, JObj, []).

-spec set_tree(wh_json:object(), ne_binaries()) -> wh_json:object().
set_tree(JObj, Tree) ->
    wh_json:set_value(?TREE, Tree, JObj).

-spec notification_preference(wh_json:object()) -> api_binary().
notification_preference(JObj) ->
    wh_json:get_value(?NOTIFY_PREF, JObj).

-spec set_notification_preference(wh_json:object(), ne_binary()) -> wh_json:object().
set_notification_preference(JObj, Pref) ->
    wh_json:set_value(?NOTIFY_PREF, Pref, JObj).

-spec is_enabled(wh_json:object()) -> boolean().
is_enabled(JObj) ->
    wh_json:is_true(?IS_ENABLED, JObj, 'true').

-spec enable(wh_json:object()) -> wh_json:object().
enable(JObj) ->
    wh_json:set_value(?IS_ENABLED, 'true', JObj).

-spec disable(wh_json:object()) -> wh_json:object().
disable(JObj) ->
    wh_json:set_value(?IS_ENABLED, 'false', JObj).

-spec api_key(wh_json:object()) -> api_binary().
api_key(JObj) ->
    wh_json:get_value(?API_KEY, JObj).

-spec set_api_key(wh_json:object(), ne_binary()) -> wh_json:object().
set_api_key(JObj, ApiKey) ->
    wh_json:set_value(?API_KEY, ApiKey, JObj).

-spec is_superduper_admin(wh_json:object()) -> boolean().
is_superduper_admin(JObj) ->
    wh_json:is_true(?IS_SUPERDUPER_ADMIN, JObj).

-spec allow_number_additions(wh_json:object()) -> boolean().
allow_number_additions(JObj) ->
    wh_json:is_true(?ALLOW_NUMBER_ADDITIONS, JObj).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(MASTER_ACCOUNT_ID, <<"1">>).
-define(MASTER_ACCOUNT, wh_json:from_list([{?TREE, []}
                                           ,{?ID, ?MASTER_ACCOUNT_ID}
                                          ])).

-define(SUB_ACCOUNT_ID, <<"2">>).
-define(SUB_ACCOUNT, wh_json:from_list([{?TREE, [?MASTER_ACCOUNT_ID]}
                                        ,{?ID, ?SUB_ACCOUNT_ID}
                                       ])).

-define(SUB_SUB_ACCOUNT_ID, <<"2">>).
-define(SUB_SUB_ACCOUNT, wh_json:from_list([{?TREE, [?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID]}
                                            ,{?ID, ?SUB_SUB_ACCOUNT_ID}
                                           ])).

parent_account_id_test() ->
    ?assertEqual('undefined', parent_account_id(?MASTER_ACCOUNT)),
    ?assertEqual(?MASTER_ACCOUNT_ID, parent_account_id(?SUB_ACCOUNT)),
    ?assertEqual(?SUB_ACCOUNT_ID, parent_account_id(?SUB_SUB_ACCOUNT)).

tree_test() ->
    ?assertEqual([], tree(?MASTER_ACCOUNT)),
    ?assertEqual([?MASTER_ACCOUNT_ID], tree(?SUB_ACCOUNT)),
    ?assertEqual([?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID], tree(?SUB_SUB_ACCOUNT)).

-endif.
