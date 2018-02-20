-ifndef(KAZOO_SERVICES_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").
-include_lib("kazoo_services/include/kazoo_services.hrl").

-define(APP, 'kazoo_services').
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, <<"services">>).

-define(CACHE_NAME, 'kazoo_services_cache').

-type bookkeeper_sync_result() :: 'ok' | 'delinquent' | 'retry'.

-define(MAYBE_RESELLER_BOOKKEEPER_LOOKUP
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"reseller_bookkeeper_lookup">>, 'false')
       ).

-define(KZ_LOOKUP_BOOKKEEPER(ResellerId)
       ,kz_term:to_atom(kapps_account_config:get_global(ResellerId
                                                       ,?CONFIG_CAT
                                                       ,<<"master_account_bookkeeper">>
                                                       ,'kz_bookkeeper_local'
                                                       ))
       ).

-ifdef(TEST).
-define(A_MASTER_ACCOUNT_ID, <<"master_3dd0df9f3b3940b8a972c0e43">>).
-define(A_MASTER_ACCOUNT_DB, <<"account%2Fma%2Fst%2Fer_3dd0df9f3b3940b8a972c0e43">>).
-define(A_MASTER_USER_ID, <<"master_user_36bff4d09b5b32fc14be">>).
-define(A_MASTER_PLAN_ID, <<"plan_saas_ss_l3">>).

-define(A_RESELLER_ACCOUNT_ID, <<"reseller_c0f0a3fe3ab10b6f6234d07">>).
-define(A_RESELLER_ACCOUNT_DB, <<"account%2Fre%2Fse%2Fller_c0f0a3fe3ab10b6f6234d07">>).
-define(A_RESELLER_USER_ID, <<"reseller_user_dfcec203fb5717efb8">>).

-define(A_SUB_ACCOUNT_ID, <<"sub________152f639c4118f8c21d4bb">>).
-define(A_SUB_ACCOUNT_DB, <<"account%2Fsu%2Fb_%2F_______152f639c4118f8c21d4bb">>).
-define(A_SUB_USER_ID, <<"sub_user_b26d94e175cb6bf60624769">>).

-define(B_SUB_ACCOUNT_ID, <<"sub_b______152f639c4118f8c21d4bb">>).

-define(UNRELATED_ACCOUNT_ID, <<"unrelated_29cae05bd2c7779bc706d5">>).
-define(UNRELATED_ACCOUNT_DB, <<"account%2Fun%2Fre%2Flated_29cae05bd2c7779bc706d5">>).

-define(WRONG_ACCOUNT_ID, <<"non_existing_ae05bd2c7779bc706da">>).
-endif.

-define(KAZOO_SERVICES_HRL, 'true').
-endif.
