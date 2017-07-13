-ifndef(KZ_SERVICE_HRL).

-define(KZ_SERVICE_PLANS_FIELD, <<"pvt_service_plans">>).

-define(SERVICES_BOM, <<"services_bom">>).
-define(SERVICES_EOM, <<"services_eom">>).

-define(KZ_SERVICE_MASTER_ACCOUNT_BOOKKEEPER,
        kapps_config:get_atom(<<"services">>, <<"master_account_bookkeeper">>, 'kz_bookkeeper_local')).

-define(MAYBE_RESELLER_BOOKKEEPER_LOOKUP,
        kapps_config:get_atom(<<"services">> ,<<"reseller_bookkeeper_lookup">> ,'false')).

-define(KZ_LOOKUP_BOOKKEEPER(ResellerId),
        kz_term:to_atom(kapps_account_config:get_global(ResellerId
                                                       ,<<"services">>
                                                       ,<<"master_account_bookkeeper">>
                                                       ,'kz_bookkeeper_local'))).
-define(KZ_SERVICE_HRL, 'true').
-endif.
