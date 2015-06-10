-ifndef(KNM_NUMBER_MANAGER_HRL).

-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").

-record(number, {number :: ne_binary()
                 ,number_db :: ne_binary()
                 ,assigned_to :: api_binary()
                 ,prev_assigned_to :: api_binary()
                 ,used_by :: api_binary()
                 ,features = wh_json:new() :: wh_json:object()
                 ,state :: ne_binary()
                 ,reserve_history = [] :: ne_binaries()
                 ,ported_in = 'false' :: boolean()
                 ,module_name :: ne_binary()
                 ,region :: ne_binary()
                 ,doc :: wh_json:object()
                }).

-define(PVT_DB_NAME, <<"pvt_db_name">>).
-define(PVT_ASSIGNED_TO, <<"pvt_assigned_to">>).
-define(PVT_PREVIOUSLY_ASSIGNED_TO, <<"pvt_previously_assigned_to">>).
-define(PVT_USED_BY, <<"pvt_used_by">>).
-define(PVT_FEATURES, <<"pvt_features">>).
-define(PVT_STATE, <<"pvt_state">>).
-define(PVT_RESERVE_HISTORY, <<"pvt_reserve_history">>).
-define(PVT_PORTED_IN, <<"pvt_ported_in">>).
-define(PVT_MODULE_NAME, <<"pvt_module_name">>).
-define(PVT_REGION, <<"pvt_region">>).
-define(PVT_MODIFIED, <<"pvt_modified">>).
-define(PVT_CREATED, <<"pvt_created">>).
-define(PVT_TYPE, <<"pvt_type">>).

-define(NUMBER_STATE_PORT_IN, <<"port_in">>).
-define(NUMBER_STATE_PORT_OUT, <<"port_out">>).
-define(NUMBER_STATE_DISCOVERY, <<"discovery">>).
-define(NUMBER_STATE_IN_SERVICE, <<"in_service">>).
-define(NUMBER_STATE_RELEASED, <<"released">>).
-define(NUMBER_STATE_RESERVED, <<"reserved">>).
-define(NUMBER_STATE_AVAILABLE, <<"available">>).
-define(NUMBER_STATE_DISCONNECTED, <<"disconnected">>).
-define(NUMBER_STATE_DELETED, <<"deleted">>).


-define(KNM_NUMBER_MANAGER_HRL, 'true').
-endif.
