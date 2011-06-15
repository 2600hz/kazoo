-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("whistle/include/whistle_amqp.hrl").
-include_lib("whistle/include/whistle_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-include_lib("detergent/include/detergent.hrl").

-define(DTH_URL, "http://173.203.64.57/dthsoapapi/dthsoap.asmx").

-type dth_ct_interstate() :: 10.
-type dth_ct_intrastate() :: 5.
-type dth_ct_other() :: 0.

-define(DTH_CT_INTERSTATE, 10).
-define(DTH_CT_INTRASTATE, 5).
-define(DTH_CT_OTHER, 0).

-type dth_call_type() :: dth_ct_interstate() | dth_ct_intrastate() | dth_ct_other().
