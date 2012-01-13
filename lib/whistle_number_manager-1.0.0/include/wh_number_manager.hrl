-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").

-define(WNM_CONFIG_CAT, <<"number_manager">>).

-define(WNM_NUMBER_STATUS, [<<"discovery">>, <<"avaliable">>, <<"reserved">>, <<"released">>
                                ,<<"in_service">>, <<"disconnected">>, <<"cancelled">>]).
-define(WNM_AVALIABLE_STATES, [<<"discovery">>, <<"avaliable">>]).

-define(WNM_DEAFULT_CARRIER_MODULES, [<<"wnm_bandwidth">>]).

-define(WNM_DB_PREFIX, <<"numbers/">>).
-define(WNM_DOC_VSN, <<"1">>).

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
