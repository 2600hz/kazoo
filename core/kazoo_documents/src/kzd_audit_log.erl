%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Audit Log document manipulation
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
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

        ,save/2
        ]).

-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include("kz_documents.hrl").

-define(KEY_AUDIT, <<"audit">>).
-define(KEY_CASCADE_QUANTITIES, <<"cascade_quantities">>).
-define(KEY_ACCOUNT_QUANTITIES, <<"account_quantities">>).
-define(KEY_DIFF_QUANTITIES, <<"diff_quantities">>).
-define(KEY_ACCOUNT_ID, <<"account_id">>).
-define(KEY_ACCOUNT_NAME, <<"account_name">>).
-define(KEY_TREE, <<"tree">>).
-define(KEY_AUTHENTICATING_USER, <<"authenticating_user">>).
-define(KEY_USER_ID, <<"user_id">>).
-define(KEY_FIRST_NAME, <<"first_name">>).
-define(KEY_LAST_NAME, <<"last_name">>).

-define(PVT_TYPE, <<"audit_log">>).

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec audit_account_ids(doc()) -> kz_json:keys() | [].
audit_account_ids(JObj) ->
    kz_json:get_keys(?KEY_AUDIT, JObj).

-spec audit_account_id(doc(), kz_term:ne_binary()) -> kz_term:api_object().
audit_account_id(JObj, AccountId) ->
    audit_account_id(JObj, AccountId, 'undefined').

-spec audit_account_id(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
audit_account_id(JObj, AccountId, Default) ->
    kz_json:get_json_value([?KEY_AUDIT, AccountId], JObj, Default).

-spec audit_account_quantities(doc(), kz_term:ne_binary()) -> kz_term:api_object().
audit_account_quantities(JObj, AccountId) ->
    kz_json:get_json_value([?KEY_AUDIT, AccountId, ?KEY_ACCOUNT_QUANTITIES], JObj).

-spec audit_cascase_quantities(doc(), kz_term:ne_binary()) -> kz_term:api_object().
audit_cascase_quantities(JObj, AccountId) ->
    kz_json:get_json_value([?KEY_AUDIT, AccountId, ?KEY_CASCADE_QUANTITIES], JObj).

-spec audit_account_name(doc(), kz_term:ne_binary()) -> kz_term:api_binary().
audit_account_name(JObj, AccountId) ->
    kz_json:get_value([?KEY_AUDIT, AccountId, ?KEY_ACCOUNT_NAME], JObj).

-spec tree(doc()) -> kz_term:ne_binaries().
tree(JObj) ->
    kz_json:get_value(?KEY_TREE, JObj).

-spec authenticating_user(doc()) -> kz_term:api_object().
authenticating_user(JObj) ->
    kz_json:get_json_value(?KEY_AUTHENTICATING_USER, JObj).

-spec authenticating_user_first_name(doc()) -> kz_term:api_binary().
authenticating_user_first_name(JObj) ->
    kz_json:get_value([?KEY_AUTHENTICATING_USER, ?KEY_FIRST_NAME], JObj).

-spec authenticating_user_last_name(doc()) -> kz_term:api_binary().
authenticating_user_last_name(JObj) ->
    kz_json:get_value([?KEY_AUTHENTICATING_USER, ?KEY_LAST_NAME], JObj).

-spec authenticating_user_account_id(doc()) -> kz_term:api_binary().
authenticating_user_account_id(JObj) ->
    kz_json:get_value([?KEY_AUTHENTICATING_USER, ?KEY_ACCOUNT_ID], JObj).

-spec authenticating_user_account_name(doc()) -> kz_term:api_binary().
authenticating_user_account_name(JObj) ->
    kz_json:get_value([?KEY_AUTHENTICATING_USER, ?KEY_ACCOUNT_NAME], JObj).

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec type(doc()) -> kz_term:api_binary().
type(JObj) -> kz_doc:type(JObj).

-spec new() -> doc().
new() ->
    kz_doc:update_pvt_parameters(kz_json:new(), 'undefined', [{'type', ?PVT_TYPE}]).

-spec set_tree(doc(), kz_term:ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    kz_json:set_value(?KEY_TREE, Tree, JObj).

-spec set_authenticating_user(doc(), kz_json:object()) -> doc().
set_authenticating_user(JObj, User) ->
    kz_json:set_value(?KEY_AUTHENTICATING_USER, User, JObj).

-spec set_audit_account(doc(), kz_term:ne_binary(), kz_json:object()) -> doc().
set_audit_account(JObj, AccountId, AuditJObj) ->
    OldAudit = audit_account_id(JObj, AccountId, kz_json:new()),
    NewAudit = kz_json:merge(OldAudit, AuditJObj),
    kz_json:set_value([?KEY_AUDIT, AccountId], NewAudit, JObj).

-spec save(kz_services:services(), doc()) -> 'ok'.
save(Services, AuditLog) ->
    case kz_services:have_quantities_changed(Services) of
        'true' ->
            {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
            lager:debug("maybe save the base audit log ~s", [kz_json:encode(AuditLog)]),
            save(Services, AuditLog, MasterAccountId);
        'false' ->
            lager:debug("nothing has changed for service account ~s, ignoring audit log", [kz_services:account_id(Services)])
    end.

-spec save(kz_services:services(), doc(), kz_term:ne_binary()) -> 'ok'.
save(Services, AuditLog, MasterAccountId) ->
    save(Services, AuditLog, MasterAccountId, kz_services:account_id(Services)).

-spec save(kz_services:services(), doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
save(Services, AuditLog, MasterAccountId, MasterAccountId) ->
    lager:debug("maybe saving master audit log for ~s", [kz_services:account_id(Services)]),
    maybe_save_master_audit_log(Services, AuditLog, MasterAccountId);
save(Services, AuditLog, MasterAccountId, AccountId) ->
    JObj = kz_services:services_json(Services),
    UpdatedLog = save_audit_log(Services, AuditLog, AccountId),
    maybe_save_audit_log_to_reseller(Services, UpdatedLog, MasterAccountId, AccountId, JObj).

-spec maybe_save_audit_log_to_reseller(kz_services:services(), doc(), kz_term:ne_binary(), kz_term:ne_binary(), kzd_services:doc()) -> 'ok'.
maybe_save_audit_log_to_reseller(Services, AuditLog, MasterAccountId, AccountId, JObj) ->
    case kzd_services:reseller_id(JObj) of
        MasterAccountId ->
            lager:debug("reseller account of ~s is the master account", [AccountId]),
            maybe_save_master_audit_log(Services, AuditLog, MasterAccountId);
        ResellerId ->
            lager:debug("saving audit log for account ~s's reseller ~s", [AccountId, ResellerId]),
            ResellerServices = kz_services:fetch(ResellerId),
            AuditLog1 = save_audit_log(Services, AuditLog, ResellerId),
            save(ResellerServices, AuditLog1, MasterAccountId)
    end.

-spec maybe_save_master_audit_log(kz_services:services(), doc(), kz_term:ne_binary()) -> 'ok'.
maybe_save_master_audit_log(Services, AuditLog, MasterAccountId) ->
    maybe_save_master_audit_log(Services, AuditLog, MasterAccountId
                               ,kapps_config:get_is_true(<<"services">>, <<"should_save_master_audit_logs">>, 'false')
                               ).

-spec maybe_save_master_audit_log(kz_services:services(), doc(), kz_term:ne_binary(), boolean()) -> 'ok'.
maybe_save_master_audit_log(_Services, _AuditLog, _MasterAccountId, 'false') ->
    lager:debug("reached master account, not saving audit log");
maybe_save_master_audit_log(Services, AuditLog, MasterAccountId, 'true') ->
    save_audit_log(Services, AuditLog, MasterAccountId),
    lager:debug("reached master account, saved audit log").

-spec save_audit_log(kz_services:services(), doc(), kz_term:ne_binary()) -> doc().
save_audit_log(Services, AuditLog, ResellerId) ->
    AuditLog1 = update_audit_log(Services, AuditLog),
    case audit_account_ids(AuditLog1) of
        [] ->
            lager:debug("no audit log to save");
        _Ids ->
            MODb = kazoo_modb:get_modb(ResellerId),
            ResellerAuditLog = kz_doc:update_pvt_parameters(AuditLog1, MODb, [{'account_id', ResellerId}]),
            {'ok', _Saved} = kazoo_modb:save_doc(MODb, ResellerAuditLog),
            lager:debug("saved audit log to ~s: ~s", [ResellerId, kz_json:encode(_Saved)])
    end,
    AuditLog1.

-spec update_audit_log(kz_services:services(), doc()) -> doc().
update_audit_log(Services, AuditLog) ->
    AccountId = kz_services:account_id(Services),
    JObj = kz_services:services_json(Services),
    AccountAudit = kz_json:from_list(
                     props:filter_empty(
                       [{?KEY_ACCOUNT_QUANTITIES, kzd_services:quantities(JObj)}
                       ,{?KEY_DIFF_QUANTITIES, kz_services:diff_quantities(Services)}
                       ,{?KEY_CASCADE_QUANTITIES, kz_services:cascade_quantities(Services)}
                       ,{<<"account_name">>, kzd_accounts:fetch_name(AccountId)}
                       ])),
    set_audit_account(AuditLog, AccountId, AccountAudit).
