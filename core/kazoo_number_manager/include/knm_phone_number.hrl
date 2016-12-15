-ifndef(KNM_NUMBER_MANAGER_HRL).

-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_types.hrl").

-define(KNM_DEFAULT_AUTH_BY, <<"system">>).

-define(KNM_DEFAULT_COUNTRY, <<"US">>).

-type knm_phone_number_return() ::
        {'ok', knm_phone_number:knm_phone_number()} |
        {'error', any()}.

-type dry_run_return() ::
        {'dry_run', kz_services:services(), non_neg_integer()}.

-type knm_number_return() ::
        {'ok', knm_number:knm_number()} |
        dry_run_return() |
        {'error', atom()} |
        {'error', knm_errors:error()}.

-define(PVT_ASSIGNED_TO, <<"pvt_assigned_to">>).
-define(PVT_AUTH_BY, <<"pvt_authorizing_account">>).
-define(PVT_CARRIER_DATA, <<"pvt_carrier_data">>).
-define(PVT_CREATED, <<"pvt_created">>).
-define(PVT_DB_NAME, <<"pvt_db_name">>).
-define(PVT_FEATURES, <<"pvt_features">>).
-define(PVT_FEATURES_AVAILABLE, <<"pvt_features_available">>).
-define(PVT_IS_BILLABLE, <<"pvt_is_billable">>).
-define(PVT_MODIFIED, <<"pvt_modified">>).
-define(PVT_MODULE_NAME, <<"pvt_module_name">>).
-define(PVT_PORTED_IN, <<"pvt_ported_in">>).
-define(PVT_PREVIOUSLY_ASSIGNED_TO, <<"pvt_previously_assigned_to">>).
-define(PVT_REGION, <<"pvt_region">>).
-define(PVT_RESERVE_HISTORY, <<"pvt_reserve_history">>).
-define(PVT_STATE, <<"pvt_state">>).
-define(PVT_STATE_LEGACY, <<"pvt_number_state">>).
-define(PVT_TYPE, <<"pvt_type">>).
-define(PVT_USED_BY, <<"pvt_used_by">>).

-define(KNM_DB_PREFIX, "numbers/+").
-define(KNM_DB_PREFIX_ENCODED, "numbers%2F%2B").
-define(KNM_DB_PREFIX_encoded, "numbers%2f%2b").

-define(NUMBER_STATE_AGING, <<"aging">>).
-define(NUMBER_STATE_AVAILABLE, <<"available">>).
-define(NUMBER_STATE_DELETED, <<"deleted">>).
-define(NUMBER_STATE_DISCONNECTED, <<"disconnected">>).
-define(NUMBER_STATE_DISCOVERY, <<"discovery">>).
-define(NUMBER_STATE_IN_SERVICE, <<"in_service">>).
-define(NUMBER_STATE_PORT_IN, <<"port_in">>).
-define(NUMBER_STATE_PORT_OUT, <<"port_out">>).
-define(NUMBER_STATE_RELEASED, <<"released">>).
-define(NUMBER_STATE_RESERVED, <<"reserved">>).

-define(KNM_AVAILABLE_STATES, [?NUMBER_STATE_DISCOVERY, ?NUMBER_STATE_AVAILABLE]).

-define(CARRIER_INUM, <<"knm_inum">>).
-define(CARRIER_LOCAL, <<"knm_local">>).
-define(CARRIER_MANAGED, <<"knm_managed">>).
-define(CARRIER_MDN, <<"knm_mdn">>).
-define(CARRIER_OTHER, <<"knm_other">>).
-define(CARRIER_RESERVED, <<"knm_reserved">>).
-define(CARRIER_RESERVED_RESELLER, <<"knm_reserved_reseller">>).

-define(FEATURE_CNAM, <<"cnam">>).
-define(FEATURE_CNAM_INBOUND, <<"inbound_cnam">>).
-define(FEATURE_CNAM_OUTBOUND, <<"outbound_cnam">>).
-define(FEATURE_E911, <<"e911">>).
-define(FEATURE_FAILOVER, <<"failover">>).
-define(FEATURE_FORCE_OUTBOUND, <<"force_outbound">>).
-define(FEATURE_LOCAL, <<"local">>).
-define(FEATURE_PORT, <<"port">>).
-define(FEATURE_PREPEND, <<"prepend">>).
-define(FEATURE_RINGBACK, <<"ringback">>).

-define(DEFAULT_LEGACY_PROVIDERS, [?FEATURE_CNAM
                                  ,?FEATURE_FAILOVER
                                  ,?FEATURE_LOCAL
                                  ,?FEATURE_PORT
                                  ,?FEATURE_PREPEND
                                  ]).

-define(DEFAULT_MASTER_PROVIDERS, [?FEATURE_CNAM
                                  ,?FEATURE_E911
                                  ,?FEATURE_FAILOVER
                                  ,?FEATURE_FORCE_OUTBOUND
                                  ,?FEATURE_LOCAL
                                  ,?FEATURE_PORT
                                  ,?FEATURE_PREPEND
                                  ,?FEATURE_RINGBACK
                                  ]).

-define(CNAM_DISPLAY_NAME, <<"display_name">>).
-define(CNAM_INBOUND_LOOKUP, <<"inbound_lookup">>).

-define(E911_CITY, <<"locality">>).
-define(E911_NAME, <<"caller_name">>).
-define(E911_NAME_DEFAULT, <<"Valued Customer">>).
-define(E911_STATE, <<"region">>).
-define(E911_STREET1, <<"street_address">>).
-define(E911_STREET2, <<"extended_address">>).
-define(E911_ZIP, <<"postal_code">>).

-define(KNM_NUMBER_MANAGER_HRL, 'true').
-endif.
