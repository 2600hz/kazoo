%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% AMQP-specific things for Whistle
%%% @end
%%% Created :  3 Nov 2010 by James Aimonetti <james@2600hz.org>

%% routing keys to use in the callmgr exchange
-define(KEY_AUTH_REQ, <<"auth.req">>). %% corresponds to the auth_req/1 api call
-define(KEY_ROUTE_REQ, <<"route.req">>). %% corresponds to the route_req/1 api call
-define(KEY_RESOURCE_REQ, <<"resource.req">>). %% corresponds to resource_req/1 api call

-define(KEY_CALL_MEDIA_REQ, <<"call.media">>). %% corresponds to media_req/1
-define(KEY_CALL_EVENT, <<"call.event.">>). %% corresponds to the call_event/1 api call
-define(KEY_CALL_STATUS_REQ, <<"call.status.">>). % corresponds to the call_status_req/1 api call
-define(KEY_CALL_CDR, <<"call.cdr.">>). %% corresponds to the call_cdr/1 api call

%% To listen for auth requests, bind your queue in the CallMgr Exchange with the <<"auth.req">> routing key.
%% To listen for route requests, bind your queue in the CallMgr Exchange with the <<"route.req">> routing key.

%% For a specific call event stream, bind to <<"call.event.CALLID">> on the CallEvent Exchange
%% For all call events, bind to <<"call.event.*">>

%% For a specific call cdr, bind to <<"call.cdr.CALLID">> on the CallEvent Exchange
%% For all call cdrs, bind to <<"call.cdr.*">>
