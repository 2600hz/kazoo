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

         ,save/2
        ]).

-include_lib("whistle/include/wh_log.hrl").
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

type(JObj) -> wh_doc:type(JObj).

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

-spec save(wh_services:services(), doc()) -> 'ok'.
-spec save(wh_services:services(), doc(), ne_binary()) -> 'ok'.
-spec save(wh_services:services(), doc(), ne_binary(), ne_binary()) -> 'ok'.
save(Services, AuditLog) ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    lager:debug("maybe save the base audit log ~s", [wh_json:encode(AuditLog)]),
    save(Services, AuditLog, MasterAccountId).

save(Services, AuditLog, MasterAccountId) ->
    save(Services, AuditLog, MasterAccountId, wh_services:account_id(Services)).

save(_Services, _AuditLog, MasterAccountId, MasterAccountId) ->
    lager:debug("reached master account");
save(Services, AuditLog, MasterAccountId, AccountId) ->
    JObj = wh_services:services_json(Services),
    case kzd_services:is_reseller(JObj) of
        'false' ->
            maybe_save_audit_log_to_reseller(Services, AuditLog, MasterAccountId, AccountId, JObj);
        'true' ->
            maybe_save_audit_log(Services, AuditLog, AccountId),
            maybe_save_audit_log_to_reseller(Services, AuditLog, MasterAccountId, AccountId, JObj)
    end.

-spec maybe_save_audit_log_to_reseller(wh_services:services()
                                       ,doc()
                                       ,ne_binary()
                                       ,ne_binary()
                                       ,kzd_services:doc()
                                      ) ->
                                              'ok'.
maybe_save_audit_log_to_reseller(Services
                                 ,AuditLog
                                 ,MasterAccountId
                                 ,AccountId
                                 ,JObj
                                ) ->
    case kzd_services:reseller_id(JObj) of
        MasterAccountId ->
            lager:debug("account ~s' reseller is the master account", [AccountId]),
            maybe_save_master_audit_log(Services, AuditLog, MasterAccountId);
        ResellerId ->
            lager:debug("saving audit log for account ~s's reseller ~s", [AccountId, ResellerId]),
            ResellerServices = wh_services:fetch(ResellerId),
            AuditLog1 = maybe_save_audit_log(Services, AuditLog, ResellerId),
            save(ResellerServices, AuditLog1, MasterAccountId)
    end.

-spec maybe_save_audit_log(wh_services:services(), wh_json:object(), ne_binary()) ->
                                  kzd_audit_log:doc().
maybe_save_audit_log(Services, AuditLog, ResellerId) ->
    case wh_services:have_quantities_changed(Services) of
        'true' ->
            save_audit_log(Services, AuditLog, ResellerId);
        'false' ->
            lager:debug("nothing has changed for account ~s(reseller ~s), ignoring audit log"
                        ,[wh_services:account_id(Services), ResellerId]
                       ),
            AuditLog
    end.

-spec maybe_save_master_audit_log(wh_services:services(), kzd_audit_log:doc(), ne_binary()) -> 'ok'.
-spec maybe_save_master_audit_log(wh_services:services(), kzd_audit_log:doc(), ne_binary(), boolean()) -> 'ok'.
maybe_save_master_audit_log(Services, AuditLog, MasterAccountId) ->
    maybe_save_master_audit_log(Services, AuditLog, MasterAccountId
                                ,whapps_config:get_is_true(<<"services">>, <<"should_save_master_audit_logs">>, 'false')
                               ).

maybe_save_master_audit_log(_Services, _AuditLog, _MasterAccountId, 'false') ->
    lager:debug("reached master account, not saving audit log");
maybe_save_master_audit_log(Services, AuditLog, MasterAccountId, 'true') ->
    save_audit_log(Services, AuditLog, MasterAccountId),
    lager:debug("reached master account, saved audit log").

-spec save_audit_log(wh_services:services(), kzd_audit_log:doc(), ne_binary()) -> kzd_audit_log:doc().
save_audit_log(Services, AuditLog, ResellerId) ->
    AuditLog1 = update_audit_log(Services, AuditLog),
    case kzd_audit_log:audit_account_ids(AuditLog1) of
        [] ->
            lager:debug("no audit log to save");
        _Ids ->
            MODb = kazoo_modb:get_modb(ResellerId),
            ResellerAuditLog = wh_doc:update_pvt_parameters(AuditLog1, MODb, [{'account_id', ResellerId}]),
            {'ok', _Saved} = kazoo_modb:save_doc(MODb, ResellerAuditLog),
            lager:debug("saved audit log to ~s: ~s", [ResellerId, wh_json:encode(_Saved)])
    end,
    AuditLog1.

-spec update_audit_log(wh_services:services(), kzd_audit_log:doc()) -> kzd_audit_log:doc().
update_audit_log(Services
                 ,AuditLog
                ) ->
    AccountId = wh_services:account_id(Services),
    JObj = wh_services:services_json(Services),

    AccountAudit = wh_json:from_list(
                     props:filter_empty(
                       [{?KEY_ACCOUNT_QUANTITIES, kzd_services:quantities(JObj)}
                        ,{?KEY_DIFF_QUANTITIES, wh_services:diff_quantities(Services)}
                        ,{<<"account_name">>, wh_services:account_name(AccountId)}
                       ])
                    ),
    kzd_audit_log:set_audit_account(AuditLog, AccountId, AccountAudit).
