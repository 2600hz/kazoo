%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Dialplan API commands
%%% @end
%%% Created : 19 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_dialplan).

-export([bridge/1, bridge_v/1, bridge_endpoint/1, bridge_endpoint_v/1]).

-export([publish_action/2, publish_action/3]).

-include("wapi_dialplan.hrl").


%%--------------------------------------------------------------------
%% @doc Bridge a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec bridge/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
bridge(Prop) when is_list(Prop) ->
    EPs = [begin
	       {ok, EPProps} = bridge_endpoint_headers(EP),
	       wh_json:from_list(EPProps)
	   end
	   || EP <- props:get_value(<<"Endpoints">>, Prop, []),
	      bridge_endpoint_v(EP)
	  ],
    Prop1 = [ {<<"Endpoints">>, EPs} | props:delete(<<"Endpoints">>, Prop)],
    case bridge_v(Prop1) of
	true -> wh_api:build_message(Prop1, ?BRIDGE_REQ_HEADERS, ?OPTIONAL_BRIDGE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for bridge_req"}
    end;
bridge(JObj) ->
    bridge(wh_json:to_proplist(JObj)).

-spec bridge_v/1 :: (proplist() | json_object()) -> boolean().
bridge_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?BRIDGE_REQ_HEADERS, ?BRIDGE_REQ_VALUES, ?BRIDGE_REQ_TYPES);
bridge_v(JObj) ->
    bridge_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Endpoints for bridging a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec bridge_endpoint/1 :: (proplist() | json_object()) -> {'ok', proplist()} | {'error', string()}.
bridge_endpoint(Prop) when is_list(Prop) ->
    case bridge_endpoint_v(Prop) of
	true -> wh_api:build_message_specific(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS);
	false -> {error, "Proplist failed validation for bridge_req_endpoint"}
    end;
bridge_endpoint(JObj) ->
    bridge_endpoint(wh_json:to_proplist(JObj)).

-spec bridge_endpoint_headers/1 :: (proplist() | json_object()) -> {'ok', proplist()} | {'error', string()}.
bridge_endpoint_headers(Prop) when is_list(Prop) ->
    wh_api:build_message_specific_headers(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS);
bridge_endpoint_headers(JObj) ->
    bridge_endpoint_headers(wh_json:to_proplist(JObj)).

-spec bridge_endpoint_v/1 :: (proplist() | json_object()) -> boolean().
bridge_endpoint_v(Prop) when is_list(Prop) ->
    wh_api:validate_message(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?BRIDGE_REQ_ENDPOINT_VALUES, ?BRIDGE_REQ_ENDPOINT_TYPES);
bridge_endpoint_v(JObj) ->
    bridge_endpoint_v(wh_json:to_proplist(JObj)).

publish_action(Queue, JSON) ->
    publish_action(Queue, JSON, ?DEFAULT_CONTENT_TYPE).
publish_action(Queue, Payload, ContentType) ->
    amqp_util:callctl_publish(Queue, Payload, ContentType).
