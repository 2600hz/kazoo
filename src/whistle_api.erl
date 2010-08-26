%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Whistle API request and response helpers
%%% See http://corp.switchfreedom.com/mediawiki/index.php/API_Definition
%%% @end
%%% Created : 19 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(whistle_api).

%% API
-export([default_headers/5]).
-export([auth_req/1, auth_resp/1, route_req/1, route_resp/1, route_resp_route/1]).

-import(proplists, [get_value/2, get_value/3, delete/2, is_defined/2]).

-define(DEFAULT_HEADERS, [<<"Server-ID">>, <<"Event-Category">>, <<"Event-Name">>
			      , <<"App-Name">>, <<"App-Version">>]).
-define(OPTIONAL_DEFAULT_HEADERS, [<<"Raw-Headers">>, <<"Destination-Server">>
				  , <<"Geo-Location">>, <<"Access-Group">>
				  , <<"Tenant-ID">>]).

-define(AUTH_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Orig-IP">>
			       , <<"Auth-User">>, <<"Auth-Domain">>]).

-define(AUTH_RESP_HEADERS, [<<"Msg-ID">>, <<"Auth-Method">>, <<"Auth-Pass">>]).

-define(ROUTE_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Call-ID">>]).

-define(OPTIONAL_ROUTE_REQ_HEADERS, [<<"Min-Setup-Cost">>, <<"Max-Setup-Cost">>, <<"Geo-Location">>
					 ,<<"Orig-IP">>, <<"Max-Call-Length">>, <<"Media">> %%process | proxy | bypass
					 , <<"Transcode">>, <<"Codecs">>, <<"Tenant-ID">>
					 ,<<"Resource-Type">> %% MMS | SMS | audio | video | chat
					 ,<<"Min-Increment-Cost">>, <<"Max-Incremental-Cost">>]).

-define(ROUTE_RESP_HEADERS, [<<"Msg-ID">>, <<"Routes">>, <<"Method">>]).

-define(ROUTE_RESP_ROUTE_HEADERS, [<<"Route">>, <<"Weight-Cost">>, <<"Weight-Location">>]).

-define(OPTIONAL_ROUTE_RESP_ROUTE_HEADERS, [<<"Proxy-Via">>, <<"Media">>, <<"Auth-User">>
						,<<"Auth-Password">>, <<"Codec">>]).

-type proplist() :: list(tuple(binary(), binary())). % just want to deal with binary K/V pairs

%%%===================================================================
%%% API
%%%===================================================================

%% create the default header list
-spec(default_headers/5 :: (ServerID :: binary(), EvtCat :: binary(), AppName :: binary(), AppVsn :: binary(), MsgId :: binary()) -> proplist()).
default_headers(ServerID, EvtCat, AppName, AppVsn, MsgID) ->
    [{<<"Server-ID">>, ServerID}
     ,{<<"Event-Category">>, EvtCat}
     ,{<<"App-Name">>, AppName}
     ,{<<"App-Version">>, AppVsn}
     ,{<<"Msg-ID">>, MsgID}].

%%--------------------------------------------------------------------
%% @doc Authentication Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(auth_req/1 :: (Prop :: proplist()) -> {ok, string()} | {error, string()}).
auth_req(Prop) ->
    Prop0 = [{<<"To">>, get_sip_to(Prop)}
	     ,{<<"From">>, get_sip_from(Prop)}
	     ,{<<"Orig-IP">>, get_orig_ip(Prop)}
	     ,{<<"Auth-User">>, get_value(<<"user">>, Prop)}
             ,{<<"Auth-Domain">>, get_value(<<"domain">>, Prop)}
	     ,{<<"Auth-Pass">>, get_value(<<"password">>, Prop, <<"">>)}
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

%%--------------------------------------------------------------------
%% @doc Authentication Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(auth_resp/1 :: (Prop :: proplist()) -> {ok, string()} | {error, string()}).
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

%%--------------------------------------------------------------------
%% @doc Dialplan Route Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_req/1 :: (Prop :: proplist()) -> {ok, string()} | {error, string()}).
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

%%--------------------------------------------------------------------
%% @doc Dialplan Route Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_resp/1 :: (Prop :: proplist()) -> {ok, string()} | {error, string()}).
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

%%--------------------------------------------------------------------
%% @doc Route within a Dialplan Route Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_resp_route/1 :: (Prop :: proplist()) -> {ok, string()} | {error, string()}).
route_resp_route(Prop) ->
    case update_required_headers(Prop, ?ROUTE_RESP_ROUTE_HEADERS, []) of
	{error, _Reason} = Error ->
	    io:format("RouteRespRoute Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?ROUTE_RESP_ROUTE_HEADERS, Prop]),
	    Error;
	{Headers0, Prop0} ->
	    {Headers1, _Prop1} = update_optional_headers(Prop0, ?OPTIONAL_ROUTE_RESP_ROUTE_HEADERS, Headers0),
	    {ok, mochijson2:encode({struct, Headers1})}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Checks Prop for all default headers, throws error if one is missing
%% defaults(PassedProps) -> { Headers, NewPropList } | {error, Reason}
-spec(defaults/1 :: (Prop :: proplist()) -> {proplist(), proplist()} | {error, list()}).
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
-spec(add_headers/3 :: (Prop :: proplist(), Fields :: list(binary()), Headers :: proplist) -> {proplist(), proplist()}).
add_headers(Prop, Fields, Headers) ->
    lists:foldl(fun(K, {Headers1, KVs}) ->
			{[{K, get_value(K, KVs)} | Headers1], delete(K, KVs)}
		end, {Headers, Prop}, Fields).

-spec(add_optional_headers/3 :: (Prop :: proplist(), Fields :: list(binary()), Headers :: proplist) -> {proplist(), proplist()}).
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
