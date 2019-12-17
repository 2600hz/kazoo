-ifndef(KNM_NUMBER_MANAGER_HRL).

-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(KNM_DEFAULT_AUTH_BY, <<"system">>).

-define(KNM_DEFAULT_COUNTRY, <<"US">>).

-define(PVT_STATE_LEGACY, <<"pvt_number_state">>).

-define(KNM_DB_PREFIX, "numbers/").
-define(KNM_DB_PREFIX_ENCODED, "numbers%2F").
-define(KNM_DB_PREFIX_encoded, "numbers%2f").

-define(NUMBER_STATE_AGING, <<"aging">>).
-define(NUMBER_STATE_AVAILABLE, <<"available">>).
-define(NUMBER_STATE_DELETED, <<"deleted">>).
-define(NUMBER_STATE_DISCOVERY, <<"discovery">>).
-define(NUMBER_STATE_IN_SERVICE, <<"in_service">>).
-define(NUMBER_STATE_PORT_IN, <<"port_in">>).
-define(NUMBER_STATE_PORT_OUT, <<"port_out">>).
-define(NUMBER_STATE_RELEASED, <<"released">>).
-define(NUMBER_STATE_RESERVED, <<"reserved">>).

-define(KNM_AVAILABLE_STATES, [?NUMBER_STATE_DISCOVERY, ?NUMBER_STATE_AVAILABLE]).

-define(CARRIER_INFO_MAX_PREFIX, <<"maximal_prefix_length">>).
-define(CARRIER_INFO_USABLE_CARRIERS, <<"usable_carriers">>).
-define(CARRIER_INFO_USABLE_CREATION_STATES, <<"usable_creation_states">>).

-define(CARRIER_INUM, <<"knm_inum">>).
-define(CARRIER_LOCAL, <<"knm_local">>).
-define(CARRIER_INVENTORY, <<"knm_inventory">>).
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
-define(FEATURE_RENAME_CARRIER, <<"carrier_name">>).
-define(FEATURE_RINGBACK, <<"ringback">>).
-define(FEATURE_IM, <<"im">>).

-define(PROVIDER_RENAME_CARRIER, <<"knm_rename_carrier">>).
-define(PROVIDER_FORCE_OUTBOUND, <<"knm_", (?FEATURE_FORCE_OUTBOUND)/binary>>).

-define(LEGACY_DASH_E911, <<"dash_e911">>).
-define(LEGACY_TELNYX_E911, <<"telnyx_e911">>).
-define(LEGACY_VITELITY_E911, <<"vitelity_e911">>).

-define(KAZOO_NUMBER_FEATURES, [?FEATURE_FAILOVER
                               ,?FEATURE_FORCE_OUTBOUND
                               ,?FEATURE_PREPEND
                               ,?FEATURE_RINGBACK
                               ]).

-define(EXTERNAL_NUMBER_FEATURES, [?FEATURE_CNAM
                                  ,?FEATURE_CNAM_INBOUND
                                  ,?FEATURE_CNAM_OUTBOUND
                                  ,?FEATURE_E911
                                  ,?FEATURE_PORT
                                  ,?FEATURE_IM
                                  ]).

-define(ADMIN_ONLY_FEATURES, [?FEATURE_RENAME_CARRIER
                             ]).

-define(ALL_KNM_FEATURES, ?KAZOO_NUMBER_FEATURES ++ ?EXTERNAL_NUMBER_FEATURES ++ ?ADMIN_ONLY_FEATURES).

%% Keys on number document's root reserved to update features
-define(FEATURES_ROOT_KEYS, [?FEATURE_CNAM
                            ,?FEATURE_E911
                            ,?FEATURE_FAILOVER
                            ,?FEATURE_FORCE_OUTBOUND
                            ,?FEATURE_LOCAL
                            ,?FEATURE_PORT
                            ,?FEATURE_PREPEND
                            ,?FEATURE_RENAME_CARRIER
                            ,?FEATURE_RINGBACK
                            ,?FEATURE_IM
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

-define(PREPEND_ENABLED, <<"enabled">>).
-define(PREPEND_NAME, <<"name">>).
-define(PREPEND_NUMBER, <<"number">>).

-define(RINGBACK_EARLY, <<"early">>).
-define(RINGBACK_TRANSFER, <<"transfer">>).

-define(FAILOVER_E164, <<"e164">>).
-define(FAILOVER_SIP, <<"sip">>).

-define(FEATURE_ENABLED, <<"enabled">>).

-define(KNM_NUMBER_MANAGER_HRL, 'true').
-endif.
