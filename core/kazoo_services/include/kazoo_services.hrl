-ifndef(KZ_SERVICE_HRL).

-define(SERVICES_BOM, <<"services_bom">>).
-define(SERVICES_EOM, <<"services_eom">>).

-define(SERVICES_PVT_IS_DELETED, <<"pvt_deleted">>).
-define(SERVICES_PVT_IS_DIRTY, <<"pvt_dirty">>).
-define(SERVICES_PVT_IS_RESELLER, <<"pvt_reseller">>).
-define(SERVICES_PVT_MODIFIED, <<"pvt_modified">>).
-define(SERVICES_PVT_PLANS, <<"pvt_service_plans">>).
-define(SERVICES_PVT_RESELLER_ID, <<"pvt_reseller_id">>).
-define(SERVICES_PVT_TREE, <<"pvt_tree">>).
-define(SERVICES_PVT_TREE_PREVIOUSLY, <<"pvt_previous_tree">>).
-define(SERVICES_PVT_REV, <<"_rev">>).
-define(SERVICES_PVT_STATUS, <<"pvt_status">>).

-define(KZ_SERVICE_MASTER_ACCOUNT_BOOKKEEPER,
        kapps_config:get_atom(<<"services">>, <<"master_account_bookkeeper">>, 'kz_bookkeeper_local')).

-define(MAYBE_RESELLER_BOOKKEEPER_LOOKUP,
        kapps_config:get_is_true(<<"services">> ,<<"reseller_bookkeeper_lookup">> ,'false')).

-define(KZ_LOOKUP_BOOKKEEPER(ResellerId),
        kz_term:to_atom(kapps_account_config:get_global(ResellerId
                                                       ,<<"services">>
                                                       ,<<"master_account_bookkeeper">>
                                                       ,'kz_bookkeeper_local'))).
-define(KZ_SERVICE_HRL, 'true').
-endif.
