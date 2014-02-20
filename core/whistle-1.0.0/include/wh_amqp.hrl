%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%% AMQP-specific things for Whistle
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------

-ifndef(WH_AMQP_HRL).

-define(DEFAULT_CONTENT_TYPE, <<"application/json">>).

-define(KEY_ORGN_RESOURCE_REQ, <<"orginate.resource.req">>). %% corresponds to originate_resource_req/1 api call
-define(KEY_OFFNET_RESOURCE_REQ, <<"offnet.resource.req">>). %% corresponds to offnet_resource_req/1 api call
-define(RESOURCE_QUEUE_NAME, <<"resource.provider">>).

-define(KEY_CALL_MEDIA_REQ, <<"call.media">>). %% corresponds to media_req/1
-define(KEY_CALL_EVENT, <<"call.event.">>). %% corresponds to the call_event/1 api call
-define(KEY_CALL_CDR, <<"call.cdr.">>). %% corresponds to the call_cdr/1 api call
-define(KEY_PUBLISHER_USURP, <<"publisher.usurp.">>).

-define(KEY_REG_SUCCESS, <<"registration.success">>).
-define(KEY_REG_QUERY, <<"registration.query">>).

-define(KEY_ASR_REQ, <<"asr.req">>).

-define(KEY_CONFERENCE_DISCOVERY, <<"conference.discovery">>).
-define(KEY_CONFERENCE_COMMAND, <<"conference.command.">>).
-define(KEY_CONFERENCE_EVENT, <<"conference.event.">>).
-define(KEY_CONFERENCE_CONFIG, <<"conference.config.">>).

%% To listen for auth requests, bind your queue in the CallMgr Exchange with the <<"auth.req">> routing key.
%% To listen for route requests, bind your queue in the CallMgr Exchange with the <<"route.req">> routing key.

%% For a specific call event stream, bind to <<"call.event.CALLID">> on the CallEvent Exchange
%% For all call events, bind to <<"call.event.*">>

%% For a specific call cdr, bind to <<"call.cdr.CALLID">> on the CallEvent Exchange
%% For all call cdrs, bind to <<"call.cdr.*">>

-define(AMQP_RECONNECT_INIT_TIMEOUT, 500).
-define(AMQP_RECONNECT_MAX_TIMEOUT, 5000).

-define(WH_AMQP_HRL, 'true').
-endif.
