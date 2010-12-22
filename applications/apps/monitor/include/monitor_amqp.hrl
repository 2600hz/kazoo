%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% AMQP-specific things for Monitor
%%% @end
%%% Created :  12 Nov 2010 by Karl Monitor <karl@2600hz.org>

-include("./amqp_client/include/amqp_client.hrl").
-include("../../../utils/src/whistle_amqp.hrl").

%-define(AMQP_HOST, "whistle-apps001-fmt.2600hz.org").
-define(AMQP_HOST, "localhost").

%% routing keys to use in the monitormgr exchange
-define(KEY_MONITOR_MASTER_REQ, <<"monitor.master.req">>).

-define(KEY_AGENT_NET_REQ, <<"monitor.net.req">>).
-define(KEY_AGENT_SIP_REQ, <<"monitor.sip.req">>).
-define(KEY_AGENT_CALL_REQ, <<"monitor.call.req">>).

-type proplist() :: list(tuple(binary(), (binary() | list() | fun()) )).
