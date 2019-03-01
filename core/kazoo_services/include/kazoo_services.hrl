-ifndef(KZ_SERVICE_HRL).

-define(SERVICES_BOM, <<"services_bom">>).
-define(SERVICES_EOM, <<"services_eom">>).

-define(SERVICES_PVT_IS_DELETED, <<"pvt_deleted">>).
-define(SERVICES_PVT_IS_DIRTY, <<"pvt_dirty">>).
-define(SERVICES_PVT_IS_RESELLER, <<"pvt_reseller">>).
-define(SERVICES_PVT_MODIFIED, <<"pvt_modified">>).
-define(SERVICES_PVT_PLANS, <<"pvt_service_plans">>).
-define(SERVICES_PVT_RESELLER_ID, <<"pvt_reseller_id">>).
-define(SERVICES_PVT_REV, <<"_rev">>).
-define(SERVICES_PVT_STATUS, <<"pvt_status">>).
-define(SERVICES_PVT_TREE, <<"pvt_tree">>).
-define(SERVICES_PVT_TREE_PREVIOUSLY, <<"pvt_previous_tree">>).

-define(KZ_SERVICE_MASTER_ACCOUNT_BOOKKEEPER
       ,kapps_config:get_atom(<<"services">>, <<"master_account_bookkeeper">>, 'kz_bookkeeper_local')
       ).

-define(KZ_SERVICE_STORE_MASTER_AUDIT
       ,kapps_config:get_is_true(<<"services">>, <<"should_save_master_audit_logs">>, 'false')
       ).

-define(KZ_SERVICE_ENFORCE_GOOD_STANDING
       ,kapps_config:get_is_true(<<"services">>, <<"enforce_good_standing">>, 'false')
       ).

-define(TOPUP_CONFIG, <<"topup">>).

-define(KZ_SERVICE_HRL, 'true').
-endif.
