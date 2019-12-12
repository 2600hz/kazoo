-ifndef(KNM_HRL).
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_numbers/include/knm_phone_number.hrl").

-define(APP, 'kazoo_numbers').
-define(APP_VERSION, <<"4.0.0">>).
-define(APP_NAME, atom_to_binary(?APP, 'utf8')).

-define(CACHE_NAME, 'knm_cache').
-define(KNM_CONFIG_CAT, <<"number_manager">>).

-define(KZ_MANAGED_DB, <<"numbers%2Fmanaged">>).
-define(KZ_INUM_DB,<<"numbers%2Finum">>).

-define(KNM_USER_AGENT, "Kazoo Number Manager " ++ binary_to_list(?APP_VERSION)).

-define(IS_US_TOLLFREE(Prefix)
       ,Prefix == <<"800">>
            orelse Prefix == <<"822">>
            orelse Prefix == <<"833">>
            orelse Prefix == <<"844">>
            orelse Prefix == <<"855">>
            orelse Prefix == <<"866">>
            orelse Prefix == <<"877">>
            orelse Prefix == <<"880">>
            orelse Prefix == <<"881">>
            orelse Prefix == <<"882">>
            orelse Prefix == <<"883">>
            orelse Prefix == <<"884">>
            orelse Prefix == <<"885">>
            orelse Prefix == <<"886">>
            orelse Prefix == <<"887">>
            orelse Prefix == <<"888">>
            orelse Prefix == <<"889">>
       ).

-define(IS_US_TOLLFREE_WILDCARD(Prefix)
       ,Prefix == <<"80*">>
            orelse Prefix == <<"84*">>
            orelse Prefix == <<"85*">>
            orelse Prefix == <<"86*">>
            orelse Prefix == <<"87*">>
            orelse Prefix == <<"88*">>
       ).

-define(IS_UIFN_TOLLFREE(Prefix)
       ,Prefix == <<"800">>
       ).

-define(KEY_FEATURES_ALLOW, [<<"features">>, <<"allow">>]).
-define(KEY_FEATURES_DENY, [<<"features">>, <<"deny">>]).

-define(DEFAULT_FEATURES_ALLOWED_SYSTEM, ?ALL_KNM_FEATURES).

-define(PORT_IN_MODULE_NAME
       ,kapps_config:get_ne_binary(?KNM_CONFIG_CAT, <<"port_in_module_name">>, ?CARRIER_LOCAL)
       ).
-define(FEATURES_ALLOWED_RESELLER(AccountId)
       ,kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, ?KEY_FEATURES_ALLOW)
       ).
-define(FEATURES_DENIED_RESELLER(AccountId)
       ,kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, ?KEY_FEATURES_DENY)
       ).
-define(FEATURES_ALLOWED_SYSTEM(Default)
       ,kapps_config:get_ne_binaries(?KNM_CONFIG_CAT, ?KEY_FEATURES_ALLOW, Default)
       ).
-define(LOCAL_FEATURE_OVERRIDE
       ,kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"local_feature_override">>, 'false')
       ).

-ifdef(TEST).
-include_lib("kazoo_numbers/test/knm_test.hrl").
-endif.

-define(KNM_HRL, 'true').
-endif.
