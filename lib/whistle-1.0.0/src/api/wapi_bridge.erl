%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Bridge Dialplan API
%%% @end
%%% Created : 18 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_bridge).

-export([req/1, req_v/1, req_endpoint/1, req_endpoint_v/1, publish_req/2, publish_req/3]).

-include("../wh_api.hrl").

-define(BRIDGE_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Endpoints">>]).
-define(OPTIONAL_BRIDGE_REQ_HEADERS, [<<"Timeout">>, <<"Continue-On-Fail">>, <<"Ignore-Early-Media">>
					  ,<<"Outgoing-Caller-ID-Name">>, <<"Outgoing-Caller-ID-Number">>
					  ,<<"Outgoing-Callee-ID-Name">>, <<"Outgoing-Callee-ID-Number">>
					  ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
					  ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
					  ,<<"Ringback">>, <<"Dial-Endpoint-Method">>, <<"Insert-At">>
					  ,<<"Media">>, <<"SIP-Headers">>, <<"Custom-Channel-Vars">>
				     ]).
-define(BRIDGE_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
			    ,{<<"Event-Name">>, <<"command">>}
			    ,{<<"Application-Name">>, <<"bridge">>}
			    ,{<<"Dial-Endpoint-Method">>, [<<"single">>, <<"simultaneous">>]}
                            ,{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
			    ,{<<"Continue-On-Fail">>, [<<"true">>, <<"false">>]}
			    ,?INSERT_AT_TUPLE
			   ]).
-define(BRIDGE_REQ_TYPES, [{<<"Endpoints">>, fun is_list/1}
			   ,{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
                           ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
			  ]).

%% Bridge Endpoints
-define(BRIDGE_REQ_ENDPOINT_HEADERS, [<<"Invite-Format">>]).
-define(OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS, [ <<"Route">>, <<"To-User">>, <<"To-Realm">>, <<"To-DID">>
						    ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
						    ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                                    ,<<"Ignore-Early-Media">>, <<"Bypass-Media">>
                                                    ,<<"Endpoint-Timeout">>, <<"Endpoint-Progress-Timeout">>
                                                    ,<<"Endpoint-Delay">>, <<"Codecs">>, <<"SIP-Headers">>
                                                    ,<<"Custom-Channel-Vars">>, <<"Auth-User">>, <<"Auth-Password">>
					      ]).
-define(BRIDGE_REQ_ENDPOINT_VALUES, [?INVITE_FORMAT_TUPLE
                                     ,{<<"Ignore-Early-Media">>, [<<"true">>, <<"false">>]}
                                     ,{<<"Bypass-Media">>, [<<"true">>, <<"false">>]}
                                    ]).
-define(BRIDGE_REQ_ENDPOINT_TYPES, [{<<"SIP-Headers">>, ?IS_JSON_OBJECT}
                                    ,{<<"Custom-Channel-Vars">>, ?IS_JSON_OBJECT}
                                   ]).

%%--------------------------------------------------------------------
%% @doc Bridge a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req/1 :: (proplist() | json_object()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    EPs = [begin
	       {ok, EPProps} = req_endpoint_headers(EP),
	       wh_json:from_list(EPProps)
	   end
	   || EP <- props:get_value(<<"Endpoints">>, Prop, []),
	      req_endpoint_v(EP)
	  ],
    Prop1 = [ {<<"Endpoints">>, EPs} | props:delete(<<"Endpoints">>, Prop)],
    case req_v(Prop1) of
	true -> wh_api:build_message(Prop1, ?BRIDGE_REQ_HEADERS, ?OPTIONAL_BRIDGE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for bridge_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v/1 :: (proplist() | json_object()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?BRIDGE_REQ_HEADERS, ?BRIDGE_REQ_VALUES, ?BRIDGE_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Endpoints for bridging a call - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec req_endpoint/1 :: (proplist() | json_object()) -> {'ok', proplist()} | {'error', string()}.
req_endpoint(Prop) when is_list(Prop) ->
    case req_endpoint_v(Prop) of
	true -> wh_api:build_message_specific(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS);
	false -> {error, "Proplist failed validation for bridge_req_endpoint"}
    end;
req_endpoint(JObj) ->
    req_endpoint(wh_json:to_proplist(JObj)).

-spec req_endpoint_headers/1 :: (proplist() | json_object()) -> {'ok', proplist()} | {'error', string()}.
req_endpoint_headers(Prop) when is_list(Prop) ->
    wh_api:build_message_specific_headers(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?OPTIONAL_BRIDGE_REQ_ENDPOINT_HEADERS);
req_endpoint_headers(JObj) ->
    req_endpoint_headers(wh_json:to_proplist(JObj)).

-spec req_endpoint_v/1 :: (proplist() | json_object()) -> boolean().
req_endpoint_v(Prop) when is_list(Prop) ->
    wh_api:validate_message(Prop, ?BRIDGE_REQ_ENDPOINT_HEADERS, ?BRIDGE_REQ_ENDPOINT_VALUES, ?BRIDGE_REQ_ENDPOINT_TYPES);
req_endpoint_v(JObj) ->
    req_endpoint_v(wh_json:to_proplist(JObj)).

publish_req(Queue, JSON) ->
    publish_req(Queue, JSON, ?DEFAULT_CONTENT_TYPE).
publish_req(Queue, Payload, ContentType) ->
    amqp_util:targeted_publish(Queue, Payload, ContentType).
