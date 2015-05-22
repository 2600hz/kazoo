%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Audit Log document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_audit_log).

-export([audit_account_ids/1
         ,audit_account_id/2, audit_account_id/3
         ,audit_account_quantities/2
         ,audit_cascase_quantities/2
         ,audit_account_name/2
         ,tree/1
         ,authenticating_user/1
         ,authenticating_user_first_name/1
         ,authenticating_user_last_name/1
         ,authenticating_user_account_id/1
         ,authenticating_user_account_name/1
         ,type/0, type/1

         ,new/0
         ,set_tree/2
         ,set_authenticating_user/2
         ,set_audit_account/3
        ]).

-include("kz_documents.hrl").

-define(KEY_AUDIT, <<"audit">>).
-define(KEY_CASCADE_QUANTITIES, <<"cascade_quantities">>).
-define(KEY_ACCOUNT_QUANTITIES, <<"account_quantities">>).
-define(KEY_ACCOUNT_ID, <<"account_id">>).
-define(KEY_ACCOUNT_NAME, <<"account_name">>).
-define(KEY_TREE, <<"tree">>).
-define(KEY_AUTHENTICATING_USER, <<"authenticating_user">>).
-define(KEY_USER_ID, <<"user_id">>).
-define(KEY_FIRST_NAME, <<"first_name">>).
-define(KEY_LAST_NAME, <<"last_name">>).

-define(PVT_TYPE, <<"audit_log">>).

-type doc() :: wh_json:object().
-export_type([doc/0]).

-spec audit_account_ids(doc()) -> wh_json:keys().
audit_account_ids(JObj) ->
    wh_json:get_keys(?KEY_AUDIT, JObj).

-spec audit_account_id(doc(), ne_binary()) -> api_object().
-spec audit_account_id(doc(), ne_binary(), Default) -> wh_json:object() | Default.
audit_account_id(JObj, AccountId) ->
    audit_account_id(JObj, AccountId, 'undefined').
audit_account_id(JObj, AccountId, Default) ->
    wh_json:get_json_value([?KEY_AUDIT, AccountId], JObj, Default).

-spec audit_account_quantities(doc(), ne_binary()) -> api_object().
audit_account_quantities(JObj, AccountId) ->
    wh_json:get_json_value([?KEY_AUDIT, AccountId, ?KEY_ACCOUNT_QUANTITIES], JObj).

-spec audit_cascase_quantities(doc(), ne_binary()) -> api_object().
audit_cascase_quantities(JObj, AccountId) ->
    wh_json:get_json_value([?KEY_AUDIT, AccountId, ?KEY_CASCADE_QUANTITIES], JObj).

-spec audit_account_name(doc(), ne_binary()) -> api_binary().
audit_account_name(JObj, AccountId) ->
    wh_json:get_value([?KEY_AUDIT, AccountId, ?KEY_ACCOUNT_NAME], JObj).

-spec tree(doc()) -> ne_binaries().
tree(JObj) ->
    wh_json:get_value(?KEY_TREE, JObj).

-spec authenticating_user(doc()) -> api_object().
authenticating_user(JObj) ->
    wh_json:get_json_value(?KEY_AUTHENTICATING_USER, JObj).

-spec authenticating_user_first_name(doc()) -> api_binary().
authenticating_user_first_name(JObj) ->
    wh_json:get_value([?KEY_AUTHENTICATING_USER, ?KEY_FIRST_NAME], JObj).

-spec authenticating_user_last_name(doc()) -> api_binary().
authenticating_user_last_name(JObj) ->
    wh_json:get_value([?KEY_AUTHENTICATING_USER, ?KEY_LAST_NAME], JObj).

-spec authenticating_user_account_id(doc()) -> api_binary().
authenticating_user_account_id(JObj) ->
    wh_json:get_value([?KEY_AUTHENTICATING_USER, ?KEY_ACCOUNT_ID], JObj).

-spec authenticating_user_account_name(doc()) -> api_binary().
authenticating_user_account_name(JObj) ->
    wh_json:get_value([?KEY_AUTHENTICATING_USER, ?KEY_ACCOUNT_NAME], JObj).

-spec type() -> ne_binary().
-spec type(doc()) -> api_binary().
type() -> ?PVT_TYPE.

type(JObj) -> wh_doc:pvt_type(JObj).

-spec new() -> doc().
new() ->
    wh_doc:update_pvt_parameters(wh_json:new(), 'undefined', [{'type', ?PVT_TYPE}]).

-spec set_tree(doc(), ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    wh_json:set_value(?KEY_TREE, Tree, JObj).

-spec set_authenticating_user(doc(), wh_json:object()) -> doc().
set_authenticating_user(JObj, User) ->
    wh_json:set_value(?KEY_AUTHENTICATING_USER, User, JObj).

-spec set_audit_account(doc(), ne_binary(), wh_json:object()) -> doc().
set_audit_account(JObj, AccountId, AuditJObj) ->
    OldAudit = audit_account_id(JObj, AccountId, wh_json:new()),
    NewAudit = wh_json:merge_recursive(OldAudit, AuditJObj),
    wh_json:set_value([?KEY_AUDIT, AccountId], NewAudit, JObj).
