%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, VoIP INC
%%% @doc
%%% AMQP-specific things for Whistle
%%% @end
%%% Created :  3 Nov 2010 by James Aimonetti <james@2600hz.org>

%% routing keys to use in the callmgr exchange
-define(KEY_AUTHN_REQ, <<"authn.req">>). %% corresponds to the authn_req/1 api call
-define(KEY_AUTHZ_REQ, <<"authz.req">>). %% corresponds to the authz_req/1 api call
-define(KEY_ROUTE_REQ, <<"route.req">>). %% corresponds to the route_req/1 api call

-define(KEY_RESOURCE_REQ, <<"originate.resource.req">>). %% corresponds to resource_req/1 api call
-define(KEY_ORGN_RESOURCE_REQ, <<"orginate.resource.req">>). %% corresponds to originate_resource_req/1 api call
-define(KEY_OFFNET_RESOURCE_REQ, <<"offnet.resource.req">>). %% corresponds to offnet_resource_req/1 api call
-define(RESOURCE_QUEUE_NAME, <<"resource.provider">>).

-define(KEY_CALL_MEDIA_REQ, <<"call.media">>). %% corresponds to media_req/1
-define(KEY_CALL_EVENT, <<"call.event.">>). %% corresponds to the call_event/1 api call
-define(KEY_CALL_STATUS_REQ, <<"call.status.">>). % corresponds to the call_status_req/1 api call
-define(KEY_CALL_CDR, <<"call.cdr.">>). %% corresponds to the call_cdr/1 api call

-define(KEY_REG_SUCCESS, <<"registration.success">>).
-define(KEY_REG_QUERY, <<"registration.query">>).

-define(KEY_SIP_NOTIFY, <<"sip.notify">>).

-define(KEY_CONF_DISCOVERY_REQ, <<"conference.discovery">>).
-define(KEY_CONF_SERVICE_REQ, <<"conference.service.">>).
-define(KEY_CONF_EVENTS, <<"conference.events.">>).
-define(CONF_DISCOVERY_QUEUE_NAME, <<"conference_discovery">>).

%% To listen for auth requests, bind your queue in the CallMgr Exchange with the <<"auth.req">> routing key.
%% To listen for route requests, bind your queue in the CallMgr Exchange with the <<"route.req">> routing key.

%% For a specific call event stream, bind to <<"call.event.CALLID">> on the CallEvent Exchange
%% For all call events, bind to <<"call.event.*">>

%% For a specific call cdr, bind to <<"call.cdr.CALLID">> on the CallEvent Exchange
%% For all call cdrs, bind to <<"call.cdr.*">>

-define(AMQP_RECONNECT_INIT_TIMEOUT, 500).
-define(AMQP_RECONNECT_MAX_TIMEOUT, 5000).
