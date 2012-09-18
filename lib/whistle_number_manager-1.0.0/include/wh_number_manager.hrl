-define(WNM_NUMBER_STATUS, [<<"discovery">>, <<"available">>, <<"reserved">>, <<"released">>
                                ,<<"port_in">> ,<<"in_service">>, <<"disconnected">>, <<"port_out">>
                           ]).
-define(WNM_AVALIABLE_STATES, [<<"discovery">>, <<"available">>]).
-define(WNM_UNAVAILABLE_STATES, [<<"reserved">>, <<"in_service">>
                                     ,<<"port_in">>, <<"port_out">>
                                ]).

-define(WNM_DEAFULT_CARRIER_MODULES, [<<"wnm_local">>]).

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

-type operation_return() :: {'ok', wh_json:json_object()} |
                            {wnm_failures(), wh_json:json_object()}.

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
