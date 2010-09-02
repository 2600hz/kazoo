%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Whistle API request and response helpers
%%% Most API functions take a proplist, filter it against required headers
%%% and optional headers, and return either the JSON string if all
%%% required headers (default AND API-call-specific) are present, or an
%%% error if some headers are missing.
%%%
%%% To check a received message's validity, pass true to only check
%%% validity and not create the JSON message.
%%% 
%%%
%%% See http://corp.switchfreedom.com/mediawiki/index.php/API_Definition
%%% @end
%%% Created : 19 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(whistle_api).

%% API
-export([default_headers/5, extract_defaults/1]).

%% Directory-related APIs
-export([auth_req/1, auth_req_v/1, auth_resp/1, auth_resp_v/1
	 ,route_req/1, route_req_v/1, route_resp/1, route_resp_v/1
	 ,route_resp_route/1, route_resp_route_v/1]).

%% Dialplan-related APIs
-export([call_event/1, call_event_v/1, error_resp/1, error_resp_v/1]).
-export([store_req/1, store_req_v/1, store_amqp_resp/1, store_amqp_resp_v/1
	 ,store_http_resp/1, store_http_resp_v/1, tones_req/1, tones_req_v/1
	 ,tones_req_tone/1, tones_req_tone_v/1, queue_req/1, queue_req_v/1
	]).

-import(proplists, [get_value/2, get_value/3, delete/2, is_defined/2]).

-include("whistle_api.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Default Headers in all messages - see wiki
%% Creates the seed proplist for the eventual message to be sent
%% All fields are required general headers.
%% @end
%%--------------------------------------------------------------------
-spec(default_headers/5 :: (ServerID :: binary(), EvtCat :: binary(), AppName :: binary(), AppVsn :: binary(), MsgId :: binary()) -> proplist()).
default_headers(ServerID, EvtCat, AppName, AppVsn, MsgID) ->
    [{<<"Server-ID">>, ServerID}
     ,{<<"Event-Category">>, EvtCat}
     ,{<<"App-Name">>, AppName}
     ,{<<"App-Version">>, AppVsn}
     ,{<<"Msg-ID">>, MsgID}].

%%--------------------------------------------------------------------
%% @doc Extract just the default headers from a message
%% @end
%%--------------------------------------------------------------------
-spec(extract_defaults/1 :: (Prop :: proplist()) -> proplist()).
extract_defaults(Prop) ->
    lists:foldl(fun(H, Acc) -> [{H, get_value(H, Prop)} | Acc] end, [], ?DEFAULT_HEADERS).

%%--------------------------------------------------------------------
%% @doc Authentication Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(auth_req/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
auth_req(Prop) ->
    Prop0 = [{<<"To">>, get_sip_to(Prop)}
	     ,{<<"From">>, get_sip_from(Prop)}
	     ,{<<"Orig-IP">>, get_orig_ip(Prop)}
	     ,{<<"Auth-User">>, get_value(<<"user">>, Prop, get_value(<<"Auth-User">>, Prop))}
             ,{<<"Auth-Domain">>, get_value(<<"domain">>, Prop, get_value(<<"Auth-Domain">>, Prop))}
	     ,{<<"Auth-Pass">>, get_value(<<"password">>, Prop, get_value(<<"Auth-Pass">>, Prop))}
	     | Prop],
    case defaults(Prop0) of
	{error, _Reason}=Error ->
	    io:format("AuthReq Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers, Prop1} ->
	    case update_required_headers(Prop1, ?AUTH_REQ_HEADERS, Headers) of
		{error, _Reason} = Error ->
		    io:format("AuthReq Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?AUTH_REQ_HEADERS, Prop1]),
		    Error;
		{Headers1, _Prop2} ->
		    {ok, mochijson2:encode({struct, Headers1})}
	    end
    end.

-spec(auth_req_v/1 :: (Prop :: proplist()) -> boolean()).
auth_req_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?AUTH_REQ_HEADERS).

%%--------------------------------------------------------------------
%% @doc Authentication Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(auth_resp/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
auth_resp(Prop) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("AuthResp DefError: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers, Prop1} ->
	    case update_required_headers(Prop1, ?AUTH_RESP_HEADERS, Headers) of
		{error, _Reason} = Error ->
		    io:format("AuthResp Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?AUTH_RESP_HEADERS, Prop]),
		    Error;
		{Headers1, _Prop2} ->
		    {ok, mochijson2:encode({struct, Headers1})}
	    end
    end.

-spec(auth_resp_v/1 :: (Prop :: proplist()) -> boolean()).
auth_resp_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?AUTH_RESP_HEADERS).

%%--------------------------------------------------------------------
%% @doc Dialplan Route Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_req/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
route_req(Prop) ->
    Prop0 = [{<<"To">>, get_sip_to(Prop)}
	     ,{<<"From">>, get_sip_from(Prop)}
	     | Prop],
    case defaults(Prop0) of
	{error, _Reason}=Error ->
	    io:format("RouteReq Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers, Prop1} ->
	    case update_required_headers(Prop1, ?ROUTE_REQ_HEADERS, Headers) of
		{error, _Reason} = Error ->
		    io:format("RouteReq Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?ROUTE_REQ_HEADERS, Prop1]),
		    Error;
		{Headers1, Prop2} ->
		    {Headers2, _Prop3} = update_optional_headers(Prop2, ?OPTIONAL_ROUTE_REQ_HEADERS, Headers1),
		    {ok, mochijson2:encode({struct, Headers2})}
	    end
    end.

-spec(route_req_v/1 :: (Prop :: proplist()) -> boolean()).
route_req_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?ROUTE_REQ_HEADERS).

%%--------------------------------------------------------------------
%% @doc Dialplan Route Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_resp/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
route_resp(Prop) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("RouteResp Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers, Prop1} ->
	    case update_required_headers(Prop1, ?ROUTE_RESP_HEADERS, Headers) of
		{error, _Reason} = Error ->
		    io:format("RouteResp Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?ROUTE_RESP_HEADERS, Prop1]),
		    Error;
		{Headers1, _Prop2} ->
		    {ok, mochijson2:encode({struct, Headers1})}
	    end
    end.

-spec(route_resp_v/1 :: (Prop :: proplist()) -> boolean()).
route_resp_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?ROUTE_RESP_HEADERS).

%%--------------------------------------------------------------------
%% @doc Route within a Dialplan Route Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_resp_route/1 :: (Prop :: proplist()) -> {ok, proplist()} | {error, string()}).
route_resp_route(Prop) ->
    case update_required_headers(Prop, ?ROUTE_RESP_ROUTE_HEADERS, []) of
	{error, _Reason} = Error ->
	    io:format("RouteRespRoute Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?ROUTE_RESP_ROUTE_HEADERS, Prop]),
	    Error;
	{Headers0, Prop0} ->
	    {Headers1, _Prop1} = update_optional_headers(Prop0, ?OPTIONAL_ROUTE_RESP_ROUTE_HEADERS, Headers0),
	    {ok, Headers1}
    end.

-spec(route_resp_route_v/1 :: (Prop :: proplist()) -> boolean()).
route_resp_route_v(Prop) ->
    has_all(Prop, ?ROUTE_RESP_ROUTE_HEADERS).

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(call_event/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
call_event(Prop) ->
    EvtName = get_value(<<"Event-Name">>, Prop),
    EvtProp = [{<<"Event-Name">>, EvtName}
	       ,{<<"Event-Category">>, get_value(<<"Event-Category">>, Prop)}
	       ,{<<"Event-Timestamp">>, get_value(<<"Event-Timestamp">>, Prop)}
	       ,{<<"Call-ID">>, get_value(<<"Unique-ID">>, Prop)}
	       ,{<<"Channel-Call-State">>, get_value(<<"Channel-Call-State">>, Prop)}
	       ,{<<"Server-ID">>, get_value(<<"Server-ID">>, Prop)}
	       ,{<<"App-Name">>, get_value(<<"App-Name">>, Prop)}
	       ,{<<"App-Version">>, get_value(<<"App-Name">>, Prop)}
	       ,{<<"Msg-ID">>, get_value(<<"Msg-ID">>, Prop)}
	       | event_specific(EvtName, Prop)
	      ],

    case defaults(EvtProp) of
	{error, _Reason}=Error ->
	    io:format("CallEvt Defaults Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, EvtProp]),
	    Error;
	{Headers0, Prop0} ->
	    case update_required_headers(Prop0, ?CALL_EVENT_HEADERS, Headers0) of
		{error, _Reason} = Error ->
		    io:format("CallEvt Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?CALL_EVENT_HEADERS, Prop0]),
		    Error;
		{Headers1, Prop1} ->
		    {Headers2, _Prop2} = update_optional_headers(Prop1, ?OPTIONAL_CALL_EVENT_HEADERS, Headers1),
		    {ok, mochijson2:encode({struct, Headers2})}
	    end
    end.

-spec(call_event_v/1 :: (Prop :: proplist()) -> boolean()).
call_event_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?CALL_EVENT_HEADERS).

%%--------------------------------------------------------------------
%% @doc Format an error event
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(error_resp/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
error_resp(Prop) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("ErrorResp Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers, Prop1} ->
	    case update_required_headers(Prop1, ?ERROR_RESP_HEADERS, Headers) of
		{error, _Reason} = Error ->
		    io:format("ErrorResp Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?ERROR_RESP_HEADERS, Prop1]),
		    Error;
		{Headers1, _Prop2} ->
		    {ok, mochijson2:encode({struct, Headers1})}
	    end
    end.

-spec(error_resp_v/1 :: (Prop :: proplist()) -> boolean()).
error_resp_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?ERROR_RESP_HEADERS).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:store API call
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(store_req/1 :: ( Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
store_req(Prop) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("StoreReq Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers, Prop1} ->
	    case update_required_headers(Prop1, ?STORE_REQ_HEADERS, Headers) of
		{error, _Reason} = Error ->
		    io:format("StoreReq Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?STORE_REQ_HEADERS, Prop1]),
		    Error;
		{Headers1, Prop2} ->
		    {Headers2, _Prop3} = update_optional_headers(Prop2, ?OPTIONAL_STORE_REQ_HEADERS, Headers1),
		    {ok, mochijson2:encode({struct, Headers2})}
	    end
    end.

-spec(store_req_v/1 :: (Prop :: proplist()) -> boolean()).
store_req_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?STORE_REQ_HEADERS).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:store response for amqp storage method
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(store_amqp_resp/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
store_amqp_resp(Prop) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("StoreAmqpResp Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers, Prop1} ->
	    case update_required_headers(Prop1, ?STORE_AMQP_RESP_HEADERS, Headers) of
		{error, _Reason} = Error ->
		    io:format("StoreAmqpResp Error: ~p~nReqHeaders: ~p~nPassed: ~p~n"
			      ,[Error, ?STORE_AMQP_RESP_HEADERS, Prop1]),
		    Error;
		{Headers1, Prop2} ->
		    {Headers2, _Prop3} = update_optional_headers(Prop2, ?OPTIONAL_STORE_AMQP_RESP_HEADERS, Headers1),
		    {ok, mochijson2:encode({struct, Headers2})}
	    end
    end.

-spec(store_amqp_resp_v/1 :: (Prop :: proplist()) -> boolean()).
store_amqp_resp_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?STORE_AMQP_RESP_HEADERS).

%%--------------------------------------------------------------------
%% @doc Format a Dialplan:store response for http storage method
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(store_http_resp/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
store_http_resp(Prop) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("StoreHttpResp Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers, Prop1} ->
	    case update_required_headers(Prop1, ?STORE_HTTP_RESP_HEADERS, Headers) of
		{error, _Reason} = Error ->
		    io:format("StoreHttpResp Error: ~p~nReqHeaders: ~p~nPassed: ~p~n"
			      ,[Error, ?STORE_HTTP_RESP_HEADERS, Prop1]),
		    Error;
		{Headers1, Prop2} ->
		    {Headers2, _Prop3} = update_optional_headers(Prop2, ?OPTIONAL_STORE_HTTP_RESP_HEADERS, Headers1),
		    {ok, mochijson2:encode({struct, Headers2})}
	    end
    end.

-spec(store_http_resp_v/1 :: (Prop :: proplist()) -> boolean()).
store_http_resp_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?STORE_HTTP_RESP_HEADERS).

%%--------------------------------------------------------------------
%% @doc Create a tone on the channel - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(tones_req/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
tones_req(Prop) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("TonesReq Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers, Prop1} ->
	    case update_required_headers(Prop1, ?TONES_REQ_HEADERS, Headers) of
		{error, _Reason} = Error ->
		    io:format("TonesReq Error: ~p~nReqHeaders: ~p~nPassed: ~p~n"
			      ,[Error, ?TONES_REQ_HEADERS, Prop1]),
		    Error;
		{Headers1, Prop2} ->
		    {Headers2, _Prop3} = update_optional_headers(Prop2, ?OPTIONAL_TONES_REQ_HEADERS, Headers1),
		    {ok, mochijson2:encode({struct, Headers2})}
	    end
    end.

-spec(tones_req_v/1 :: (Prop :: proplist()) -> boolean()).
tones_req_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?TONES_REQ_HEADERS).

%%--------------------------------------------------------------------
%% @doc A Tone within a Tones request - see wiki
%% Takes proplist and returns a proplist
%% @end
%%--------------------------------------------------------------------
-spec(tones_req_tone/1 :: (Prop :: proplist()) -> {ok, proplist()} | {error, string()}).
tones_req_tone(Prop) ->
    case update_required_headers(Prop, ?TONES_REQ_TONE_HEADERS, []) of
	{error, _Reason} = Error ->
	    io:format("TonesReqTone Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?TONES_REQ_TONE_HEADERS, Prop]),
	    Error;
	{Headers0, Prop0} ->
	    {Headers1, _Prop1} = update_optional_headers(Prop0, ?OPTIONAL_TONES_REQ_TONE_HEADERS, Headers0),
	    {ok, Headers1}
    end.

-spec(tones_req_tone_v/1 :: (Prop :: proplist()) -> boolean()).
tones_req_tone_v(Prop) ->
    has_all(Prop, ?TONES_REQ_TONE_HEADERS).

%%--------------------------------------------------------------------
%% @doc Create a tone on the channel - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(queue_req/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
queue_req(Prop) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("QueueReq Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers, Prop1} ->
	    case update_required_headers(Prop1, ?QUEUE_REQ_HEADERS, Headers) of
		{error, _Reason} = Error ->
		    io:format("QueueReq Error: ~p~nReqHeaders: ~p~nPassed: ~p~n"
			      ,[Error, ?QUEUE_REQ_HEADERS, Prop1]),
		    Error;
		{Headers1, Prop2} ->
		    {Headers2, _Prop3} = update_optional_headers(Prop2, ?OPTIONAL_QUEUE_REQ_HEADERS, Headers1),
		    {ok, mochijson2:encode({struct, Headers2})}
	    end
    end.

-spec(queue_req_v/1 :: (Prop :: proplist()) -> boolean()).
queue_req_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?QUEUE_REQ_HEADERS).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(event_specific/2 :: (EventName :: binary(), Prop :: proplist()) -> proplist()).			       
event_specific(<<"CHANNEL_EXECUTE_COMPLETE">>, Prop) ->
    Application = get_value(<<"Application">>, Prop),
    case get_value(Application, ?SUPPORTED_APPLICATIONS) of
	undefined ->
	    io:format("WHISTLE_API: Didn't find ~p in supported~n", [Application]),
	    [{<<"Application-Name">>, <<"">>}, {<<"Application-Response">>, <<"">>}];
	AppName ->
	    [{<<"Application-Name">>, AppName}
	     ,{<<"Application-Response">>, get_value(<<"Application-Response">>, Prop, <<"">>)}
	    ]
    end;
event_specific(_Evt, _Prop) ->
    [].

%% Checks Prop for all default headers, throws error if one is missing
%% defaults(PassedProps) -> { Headers, NewPropList } | {error, Reason}
-spec(defaults/1 :: (Prop :: proplist()) -> {proplist(), proplist()} | {error, string()}).
defaults(Prop) ->
    defaults(Prop, []).
defaults(Prop, Headers) ->
    case update_required_headers(Prop, ?DEFAULT_HEADERS, Headers) of
	{error, _Reason} = Error ->
	    Error;
	{Headers1, Prop1} ->
	    update_optional_headers(Prop1, ?OPTIONAL_DEFAULT_HEADERS, Headers1)
    end.

-spec(update_required_headers/3 :: (Prop :: proplist(), Fields :: list(binary()), Headers :: proplist()) -> {proplist(), proplist()} | {error, string()}).
update_required_headers(Prop, Fields, Headers) ->
    case has_all(Prop, Fields) of 
	true ->
	    add_headers(Prop, Fields, Headers);
	false ->
	    {error, "All required headers not defined"}
    end.

-spec(update_optional_headers/3 :: (Prop :: proplist(), Fields :: list(binary()), Headers :: proplist()) -> {proplist(), proplist()}).
update_optional_headers(Prop, Fields, Headers) ->
    case has_any(Prop, Fields) of
	true ->
	    add_optional_headers(Prop, Fields, Headers);
	false ->
	    {Headers, Prop}
    end.

%% add [Header] from Prop to HeadProp
-spec(add_headers/3 :: (Prop :: proplist(), Fields :: list(binary()), Headers :: proplist()) -> {proplist(), proplist()}).
add_headers(Prop, Fields, Headers) ->
    lists:foldl(fun(K, {Headers1, KVs}) ->
			{[{K, get_value(K, KVs)} | Headers1], delete(K, KVs)}
		end, {Headers, Prop}, Fields).

-spec(add_optional_headers/3 :: (Prop :: proplist(), Fields :: list(binary()), Headers :: proplist()) -> {proplist(), proplist()}).
add_optional_headers(Prop, Fields, Headers) ->
    lists:foldl(fun(K, {Headers1, KVs}) ->
			case get_value(K, KVs) of
			    undefined ->
				{Headers1, KVs};
			    V ->
				{[{K, V} | Headers1], delete(K, KVs)}
			end
		end, {Headers, Prop}, Fields).

%% Checks Prop against a list of required headers, returns true | false
-spec(has_all/2 :: (Prop :: proplist(), Headers :: list(binary())) -> boolean()).
has_all(Prop, Headers) ->
    lists:all(fun(Header) ->
		      case is_defined(Header, Prop) of
			  true -> true;
			  false ->
			      io:format("has_all: Failed to find ~p~nProp: ~p~n", [Header, Prop]),
			      false
		      end
	      end, Headers).

%% Checks Prop against a list of optional headers, returns true | false if at least one if found
-spec(has_any/2 :: (Prop :: proplist(), Headers :: list(binary())) -> boolean()).
has_any(Prop, Headers) ->
    lists:any(fun(Header) -> is_defined(Header, Prop) end, Headers).

%% retrieves the sip address for the 'to' field
-spec(get_sip_to/1 :: (Prop :: proplist()) -> binary()).
get_sip_to(Prop) ->
    list_to_binary([get_value(<<"sip_to_user">>, Prop, get_value(<<"variable_sip_to_user">>, Prop, ""))
		    , "@"
		    , get_value(<<"sip_to_host">>, Prop, get_value(<<"variable_sip_to_host">>, Prop, ""))
		   ]).

%% retrieves the sip address for the 'from' field
-spec(get_sip_from/1 :: (Prop :: proplist()) -> binary()).
get_sip_from(Prop) ->
    list_to_binary([
		    get_value(<<"sip_from_user">>, Prop, get_value(<<"variable_sip_from_user">>, Prop, ""))
		    ,"@"
		    , get_value(<<"sip_from_host">>, Prop, get_value(<<"variable_sip_from_host">>, Prop, ""))
		   ]).

get_orig_ip(Prop) ->
    get_value(<<"ip">>, Prop).
