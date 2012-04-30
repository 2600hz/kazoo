-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").

-define(WNM_CONFIG_CAT, <<"number_manager">>).

-define(WNM_NUMBER_STATUS, [<<"discovery">>, <<"available">>, <<"reserved">>, <<"released">>
                                ,<<"porting">> ,<<"in_service">>, <<"disconnected">>, <<"cancelled">>]).
-define(WNM_AVALIABLE_STATES, [<<"discovery">>, <<"available">>]).

-define(WNM_DEAFULT_CARRIER_MODULES, [<<"wnm_local">>]).

-define(APP_VERSION, <<"1.0.0">>).
-define(APP_NAME, <<"whistle_number_manager">>).

-define(WNM_DB_PREFIX, <<"numbers/">>).
-define(WNM_DOC_VSN, <<"1">>).

-define(WNM_USER_AGENT, "Whistle Number Manager 1.0.0").

-define(WNM_DEAFULT_TOLLFREE_RE, "^(\\+?1)?(8[1-4,9]\\d{8}|80[1-9]\\d{7}|85[1-4,6-9]\\d{7}|86[1-5,7-9]\\d{7}|87[1-6,8-9]\\d{7}|88[1-7,9]\\d{7}|([1-7,9]\\d{9}))$").

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
