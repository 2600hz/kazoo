-ifndef(WH_NUMBER_MANAGER_HRL).

-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").

-record(number, {number :: wh_json:json_string()
                 ,number_db :: api_binary()
                 ,state = <<"discovery">> :: ne_binary()
                 ,current_state = <<"discovery">> :: ne_binary()
                 ,reserve_history = ordsets:new() :: ordsets:ordset(ne_binary())
                 ,assign_to :: api_binary()
                 ,assigned_to :: api_binary()
                 ,prev_assigned_to :: api_binary()
                 ,auth_by :: 'system' | api_binary()
                 ,module_name :: atom()
                 ,module_data = wh_json:new() :: wh_json:object()
                 ,features = sets:new() :: set()
                 ,current_features = sets:new() :: set()
                 ,number_doc = wh_json:new() :: wh_json:object()
                 ,current_number_doc = wh_json:new() :: wh_json:object()
                 ,phone_number_docs :: dict()
                 ,hard_delete = false :: boolean()
                 ,error_jobj = wh_json:new() :: wh_json:object()
                 ,activations = [] :: wh_json:objects()
                 ,services :: wh_services:services()
                 ,current_balance :: float() | integer()
                 ,billing_id :: api_binary()
                 ,used_by = <<"">> :: binary()
                }).

-type wnm_number() :: #number{}.

-define(WNM_NUMBER_STATUS, [<<"discovery">>, <<"available">>, <<"reserved">>, <<"released">>
                                ,<<"port_in">> ,<<"in_service">>, <<"disconnected">>, <<"port_out">>
                           ]).
-define(WNM_AVALIABLE_STATES, [<<"discovery">>, <<"available">>]).
-define(WNM_UNAVAILABLE_STATES, [<<"reserved">>, <<"in_service">>
                                     ,<<"port_in">>, <<"port_out">>
                                ]).

-define(WNM_DEAFULT_CARRIER_MODULES, [<<"wnm_local">>]).
-define(WNM_DEAFULT_PROVIDER_MODULES, [<<"cnam_notifier">>, <<"port_notifier">>
                                           ,<<"failover">>
                                      ]).

-define(WNM_DB_PREFIX, <<"numbers/">>).
-define(WNM_DOC_VSN, <<"1">>).

-define(WNM_USER_AGENT, "Whistle Number Manager 1.0.0").

-define(WNM_DEAFULT_TOLLFREE_RE, <<"^\\+1(800|888|877|866|855)\\d{7}$">>).

-define(WNM_PHONE_NUMBER_DOC, <<"phone_numbers">>).

-type wnm_failures() :: invalid_state_transition |  
                        unauthorized |
                        number_exists |
                        not_found |
                        no_change_required |
                        not_reconcilable |
                        database_error |
                        unknown_carrier |
                        service_restriction |
                        provider_fault |
                        carrier_fault.

-type operation_return() :: {'ok', wh_json:object()} |
                            {wnm_failures(), wh_json:object()}.

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

-define(WH_NUMBER_MANAGER_HRL, 'true').
-endif.
