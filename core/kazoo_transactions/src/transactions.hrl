-ifndef(TRANSACTIONS_HRL).

-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").
-include_lib("kazoo_services/include/kazoo_services.hrl").

-define(APP, 'kazoo_transactions').
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).

-ifdef(TEST).
-define(A_MASTER_ACCOUNT_ID, <<"master_3dd0df9f3b3940b8a972c0e43">>).
-define(A_MASTER_ACCOUNT_DB, <<"account%2Fma%2Fst%2Fer_3dd0df9f3b3940b8a972c0e43">>).
-define(A_MASTER_USER_ID, <<"master_user_36bff4d09b5b32fc14be">>).
-define(A_RESELLER_ACCOUNT_ID, <<"reseller_c0f0a3fe3ab10b6f6234d07">>).
-define(A_RESELLER_ACCOUNT_DB, <<"account%2Fre%2Fse%2Fller_c0f0a3fe3ab10b6f6234d07">>).
-define(A_RESELLER_USER_ID, <<"reseller_user_dfcec203fb5717efb8">>).
-define(A_SUB_ACCOUNT_ID, <<"sub________152f639c4118f8c21d4bb">>).
-define(A_SUB_ACCOUNT_DB, <<"account%2Fsu%2Fb_%2F_______152f639c4118f8c21d4bb">>).
-define(A_SUB_USER_ID, <<"sub_user_b26d94e175cb6bf60624769">>).
-endif.

-ifdef(TEST).
-define(LOG_ERROR(F,A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_WARN(F,A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_DEBUG(F,A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-define(LOG_DEBUG(F), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE,?LINE])).
-else.
-define(LOG_ERROR(F,A), lager:error(F,A)).
-define(LOG_WARN(F,A), lager:warning(F,A)).
-define(LOG_DEBUG(F,A), lager:debug(F,A)).
-define(LOG_DEBUG(F), lager:debug(F)).
-endif.

-define(TRANSACTIONS_HRL, 'true').
-endif.
