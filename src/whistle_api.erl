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
-export([auth_req/1, auth_resp/1]).

-import(proplists, [get_value/2, get_value/3, delete/2, is_defined/2]).

-define(DEFAULT_HEADERS, [<<"Server-ID">>, <<"Event-Category">>, <<"Event-Name">>
			      , <<"App-Name">>, <<"App-Version">>]).
-define(OPTIONAL_DEFAULT_HEADERS, [<<"Raw-Headers">>, <<"Destination-Server">>
				  , <<"Geo-Location">>, <<"Access-Group">>
				  , <<"Tenant-ID">>]).

-define(AUTH_REQ_HEADERS, [<<"Msg-ID">>, <<"To">>, <<"From">>, <<"Orig-IP">>
			       , <<"Auth-User">>, <<"Auth-Domain">>]).

-define(AUTH_RESP_HEADERS, [<<"Msg-ID">>, <<"Auth-Method">>, <<"Auth-Pass">>]).

-type proplist() :: list(tuple(binary(), binary())). % just want to deal with binary K/V pairs

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Authentication Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(auth_req/1 :: (Prop :: proplist()) -> {ok, string()} | {error, string()}).
auth_req(Prop) ->
    case defaults(Prop) of
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
