-ifndef(WH_NUMBER_MANAGER_HRL).

-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").

-define(NUMBER_STATE_PORT_IN, <<"port_in">>).
-define(NUMBER_STATE_PORT_OUT, <<"port_out">>).
-define(NUMBER_STATE_DISCOVERY, <<"discovery">>).
-define(NUMBER_STATE_IN_SERVICE, <<"in_service">>).
-define(NUMBER_STATE_RELEASED, <<"released">>).
-define(NUMBER_STATE_RESERVED, <<"reserved">>).
-define(NUMBER_STATE_AVAILABLE, <<"available">>).
-define(NUMBER_STATE_DISCONNECTED, <<"disconnected">>).
-define(NUMBER_STATE_DELETED, <<"deleted">>).

-define(PVT_NUMBER_STATE, <<"pvt_number_state">>).

-define(WNM_NUMBER_STATUS, [?NUMBER_STATE_AVAILABLE
                            ,?NUMBER_STATE_DELETED, ?NUMBER_STATE_DISCONNECTED, ?NUMBER_STATE_DISCOVERY
                            ,?NUMBER_STATE_IN_SERVICE
                            ,?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_PORT_OUT
                            ,?NUMBER_STATE_RELEASED, ?NUMBER_STATE_RESERVED
                           ]).
-define(WNM_AVALIABLE_STATES, [?NUMBER_STATE_DISCOVERY, ?NUMBER_STATE_AVAILABLE]).
-define(WNM_UNAVAILABLE_STATES, [?NUMBER_STATE_RESERVED, ?NUMBER_STATE_IN_SERVICE
                                 ,?NUMBER_STATE_PORT_IN, ?NUMBER_STATE_PORT_OUT
                                ]).

-record(number, {number :: api_binary()
                 ,number_db :: api_binary()
                 ,state = ?NUMBER_STATE_DISCOVERY :: ne_binary()
                 ,current_state = ?NUMBER_STATE_DISCOVERY :: ne_binary()
                 ,reserve_history = ordsets:new() :: ordsets:ordset(ne_binary())
                 ,assign_to :: api_binary()
                 ,assigned_to :: api_binary()
                 ,prev_assigned_to :: api_binary()
                 ,auth_by :: 'system' | api_binary()
                 ,module_name :: atom()
                 ,module_data = wh_json:new() :: wh_json:object()
                 ,features = sets:new() :: sets:set()
                 ,feature_activation_charges = 0 :: number()
                 ,phone_number_activation_charges = 0 :: number()
                 ,current_features = sets:new() :: sets:set()
                 ,number_doc = wh_json:new() :: wh_json:object()
                 ,current_number_doc = wh_json:new() :: wh_json:object()
                 ,phone_number_docs :: 'undefined' | dict:dict()
                 ,hard_delete = 'false' :: boolean()
                 ,error_jobj = wh_json:new() :: wh_json:object()
                 ,activations = [] :: wh_json:objects()
                 ,services :: wh_services:services()
                 ,current_balance :: 'undefined' | number()
                 ,billing_id :: api_binary()
                 ,used_by = <<>> :: binary()
                 ,dry_run = 'false' :: boolean()
                 ,is_new = 'false' :: boolean()
                }).

-type wnm_number() :: #number{}.

-type number_property() :: {'force_outbound', boolean()} |
                           {'pending_port', boolean()} |
                           {'local', boolean()} |
                           {'inbound_cnam', boolean()} |
                           {'ringback_media', api_binary()} |
                           {'transfer_media', api_binary()} |
                           {'number', api_binary()} |
                           {'account_id', api_binary()} |
                           {'prepend', 'false' | api_binary()}.
-type number_properties() :: [number_property(),...] | [].

-define(WNM_DEFAULT_CARRIER_MODULES, [<<"wnm_local">>]).
-define(WNM_DEFAULT_PROVIDER_MODULES, [<<"cnam_notifier">>, <<"port_notifier">>
                                       ,<<"failover">> ,<<"prepend">>
                                      ]).

-define(WNM_DB_PREFIX_L, "numbers/").
-define(WNM_DB_PREFIX, <<?WNM_DB_PREFIX_L>>).
-define(WNM_DOC_VSN, <<"1">>).

-define(WNM_USER_AGENT, "Whistle Number Manager 1.0.0").

-define(WNM_DEAFULT_TOLLFREE_RE, <<"^\\+1(800|888|877|866|855)\\d{7}$">>).

-define(WNM_PHONE_NUMBER_DOC, <<"phone_numbers">>).

-type wnm_failures() :: 'invalid_state_transition' |
                        'unauthorized' |
                        'number_exists' |
                        'not_found' |
                        'no_change_required' |
                        'not_reconcilable' |
                        'database_error' |
                        'unknown_carrier' |
                        'service_restriction' |
                        'provider_fault' |
                        'carrier_fault' |
                        'not_in_service' |
                        'account_disabled' |
                        api_binary().

-type operation_return() :: {'ok', wh_json:object()} |
                            {'dry_run', wh_proplist()} |
                            {wnm_failures(), api_object()}.

%%% NUMBER STATES
%%% discovery    - The number was discovered via a carrier lookup but has not been reserved or purchased.
%%%                Numbers in this state should be cleared out on a cleanup period (every 2 hours or so)
%%% avaliable    - The number belongs to the system admins and is routed to the cluster but is not assigned
%%%                to any account.
%%% reserved     - The number has been added to the routing table for a specific account and only it can
%%%                claim the number.
%%% in_service   - The number currently routes to an account
%%% released     - The number was mapped to an account but they have released it, after a period of time
%%%                it will be moved to avaliable or cancled with the carrier.
%%% disconnected - Number is being ported or cancelled
%%% cancelled    - Number has been cancelled with the carrier and will be removed from the system
%%% deleted      - Number has been permanently deleted (and will be removed from
%%%                the system after the number has been aged properly

-define(DASH_KEY, <<"dash_e911">>).
-define(VITELITY_KEY, <<"vitelity_e911">>).
-define(EMERGENCY_SERVICES_KEY, <<"e911">>).

-define(WH_NUMBER_MANAGER_HRL, 'true').
-endif.
