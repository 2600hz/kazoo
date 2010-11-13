%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% AMQP-specific things for Monitor
%%% @end
%%% Created :  12 Nov 2010 by Karl Monitor <karl@2600hz.org>

%% routing keys to use in the monitormgr exchange
-define(KEY_PING_REQ, <<"ping.req">>).
-define(KEY_SIP_OPTION_REQ, <<"sip.option.req">>).
-define(KEY_CALL_BASIC_REQ, <<"call.basic">>).
