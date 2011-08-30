-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("detergent/include/detergent.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-include("../include/dthsoap.hrl").
-include("../include/dth_amqp.hrl").

-define(APP_NAME, <<"dth">>).
-define(APP_VERSION, <<"0.2.0">>).

-define(DTH_CALL_TYPE_INTERSTATE, "Interstate").
-define(DTH_CALL_TYPE_INTRASTATE, "Intrastate").
-define(DTH_CALL_TYPE_OTHER, "Other").
-define(DTH_CALL_TYPE_LOCAL, "Local").
-define(DTH_CALL_TYPE_TIERED, "TieredOrigination").
