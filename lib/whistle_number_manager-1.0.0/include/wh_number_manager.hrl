-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").

-define(WNM_CONFIG_CAT, <<"number_manager">>).

-define(WNM_NUMBER_STATUS, [<<"discovery">>, <<"avaliable">>, <<"reserved">>
                                ,<<"in_service">>, <<"disconnected">>, <<"cancelled">>]).
-define(WNM_AVALIABLE_STATES, [<<"discovery">>, <<"avaliable">>]).

-define(WNM_DEAFULT_CARRIER_MODULES, [<<"wnm_bandwidth">>]).

-define(WNM_DB_PREFIX, <<"numbers/">>).
-define(WNM_DOC_VSN, <<"1">>).

