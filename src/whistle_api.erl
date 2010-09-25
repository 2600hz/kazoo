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

%% Authentication and Routing
-export([auth_req/1, auth_req_v/1, auth_resp/1, auth_resp_v/1
	 ,route_req/1, route_req_v/1, route_resp/1, route_resp_v/1
	 ,route_resp_route/1, route_resp_route_v/1, route_win/1, route_win_v/1]).

%% In-Call
-export([call_event/1, call_event_v/1, error_resp/1, error_resp_v/1]).
-export([store_req/1, store_req_v/1, store_amqp_resp/1, store_amqp_resp_v/1
	 ,store_http_resp/1, store_http_resp_v/1, tones_req/1, tones_req_v/1
	 ,tones_req_tone/1, tones_req_tone_v/1, queue_req/1, queue_req_v/1
	]).

%% FS-specific routines
-export([convert_fs_evt_name/1, convert_whistle_app_name/1]).

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
-spec(default_headers/5 :: (ServerID :: binary(), EvtCat :: binary(), EvtName :: binary(), AppName :: binary(), AppVsn :: binary()) -> proplist()).
default_headers(ServerID, EvtCat, EvtName, AppName, AppVsn) ->
    [{<<"Server-ID">>, ServerID}
     ,{<<"Event-Category">>, EvtCat}
     ,{<<"Event-Name">>, EvtName}
     ,{<<"App-Name">>, AppName}
     ,{<<"App-Version">>, AppVsn}
    ].

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
    case auth_req_v(Prop) of
	true -> build_message(Prop, ?AUTH_REQ_HEADERS, ?OPTIONAL_AUTH_REQ_HEADERS);
	false -> {error, "Proplist failed validation for auth_req"}
    end.

-spec(auth_req_v/1 :: (Prop :: proplist()) -> boolean()).
auth_req_v(Prop) ->
    validate(Prop, ?AUTH_REQ_HEADERS, ?AUTH_REQ_VALUES, ?AUTH_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Authentication Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(auth_resp/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
auth_resp(Prop) ->
    case auth_resp_v(Prop) of
	true -> build_message(Prop, ?AUTH_RESP_HEADERS, ?OPTIONAL_AUTH_RESP_HEADERS);
	false -> {error, "Proplist failed validation for auth_resp"}
    end.

-spec(auth_resp_v/1 :: (Prop :: proplist()) -> boolean()).
auth_resp_v(Prop) ->
    validate(Prop, ?AUTH_RESP_HEADERS, ?AUTH_RESP_VALUES, ?AUTH_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Dialplan Route Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_req/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
route_req(Prop) ->
    case route_req_v(Prop) of
	true -> build_message(Prop, ?ROUTE_REQ_HEADERS, ?OPTIONAL_ROUTE_REQ_HEADERS);
	false -> {error, "Proplist failed validation for route_req"}
    end.

-spec(route_req_v/1 :: (Prop :: proplist()) -> boolean()).
route_req_v(Prop) ->
    validate(Prop, ?ROUTE_REQ_HEADERS, ?ROUTE_REQ_VALUES, ?ROUTE_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Dialplan Route Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_resp/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
route_resp(Prop) ->
    case route_resp_v(Prop) of
	true -> build_message(Prop, ?ROUTE_RESP_HEADERS, ?OPTIONAL_ROUTE_RESP_HEADERS);
	false -> {error, "Proplist failed validation for route_resp"}
    end.

-spec(route_resp_v/1 :: (Prop :: proplist()) -> boolean()).
route_resp_v(Prop) ->
    validate(Prop, ?ROUTE_RESP_HEADERS, ?ROUTE_RESP_VALUES, ?ROUTE_RESP_TYPES).

%%--------------------------------------------------------------------
%% @doc Route within a Dialplan Route Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_resp_route/1 :: (Prop :: proplist()) -> {ok, proplist()} | {error, string()}).
route_resp_route(Prop) ->
    case route_resp_route_v(Prop) of
	true -> build_message_specific(Prop, ?ROUTE_RESP_ROUTE_HEADERS, ?OPTIONAL_ROUTE_RESP_ROUTE_HEADERS);
	false -> {error, "Proplist failed validation for route_resp_route"}
    end.

-spec(route_resp_route_v/1 :: (Prop :: proplist()) -> boolean()).
route_resp_route_v(Prop) ->
    validate_message(Prop, ?ROUTE_RESP_ROUTE_HEADERS, ?ROUTE_RESP_ROUTE_VALUES, ?ROUTE_RESP_ROUTE_TYPES).

%%--------------------------------------------------------------------
%% @doc Winning Responder Message - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(route_win/1 :: (Prop :: proplist()) -> {ok, proplist()} | {error, string()}).
route_win(Prop) ->
    case route_win_v(Prop) of
	true -> build_message(Prop, ?ROUTE_WIN_HEADERS, ?OPTIONAL_ROUTE_WIN_HEADERS);
	false -> {error, "Proplist failed validation for route_win"}
    end.

-spec(route_win_v/1 :: (Prop :: proplist()) -> boolean()).
route_win_v(Prop) ->
    validate(Prop, ?ROUTE_WIN_HEADERS, ?ROUTE_WIN_VALUES, ?ROUTE_WIN_TYPES).

%%--------------------------------------------------------------------
%% @doc Format a call event from the switch for the listener
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(call_event/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
call_event(Prop) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("CallEvt Defaults Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	{Headers0, Prop0} ->
	    case update_required_headers(Prop0, ?CALL_EVENT_HEADERS, Headers0) of
		{error, _Reason} = Error ->
		    io:format("CallEvt Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ?CALL_EVENT_HEADERS, Prop0]),
		    Error;
		{Headers1, Prop1} ->
		    {Headers2, _Prop2} = update_optional_headers(Prop1, ?OPTIONAL_CALL_EVENT_HEADERS, Headers1),
		    headers_to_json(Headers2)
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
		    headers_to_json(Headers1)
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
		    headers_to_json(Headers2)
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
		    headers_to_json(Headers2)
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
		    headers_to_json(Headers2)
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
		    headers_to_json(Headers2)
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
		    headers_to_json(Headers2)
	    end
    end.

-spec(queue_req_v/1 :: (Prop :: proplist()) -> boolean()).
queue_req_v(Prop) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso has_all(Prop, ?QUEUE_REQ_HEADERS).

%% given a proplist of a FS event, return the Whistle-equivalent app name
-spec(convert_fs_evt_name/1 :: (EvtName :: binary()) -> undefined | binary()).
convert_fs_evt_name(EvtName) ->
    case lists:keyfind(EvtName, 1, ?SUPPORTED_APPLICATIONS) of
	false -> undefined;
	{EvtName, AppName} -> AppName
    end.

%% given a Whistle Dialplan Application name, return the FS-equivalent event name
-spec(convert_whistle_app_name/1 :: (AppName :: binary()) -> undefined | binary()).
convert_whistle_app_name(AppName) ->
    case lists:keyfind(AppName, 2, ?SUPPORTED_APPLICATIONS) of
	false -> undefined;
	{EvtName, AppName} -> EvtName
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(validate/4 :: (Prop :: proplist(), ReqH :: list(binary()), Vals :: proplist(), Types :: proplist()) -> boolean()).
validate(Prop, ReqH, Vals, Types) ->
    has_all(Prop, ?DEFAULT_HEADERS) andalso
	validate_message(Prop, ReqH, Vals, Types).

-spec(validate_message/4 :: (Prop :: proplist(), ReqH :: list(binary()), Vals :: proplist(), Types :: proplist()) -> boolean()).
validate_message(Prop, ReqH, Vals, Types) ->
    has_all(Prop, ReqH) andalso
	values_check(Prop, Vals) andalso
	type_check(Prop, Types).

-spec(build_message/3 :: (Prop :: proplist(), ReqH :: list(binary()), OptH :: list(binary())) -> {ok, iolist()} | {error, string()}).
build_message(Prop, ReqH, OptH) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("Build Error: ~p~nDefHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	HeadAndProp ->
	    build_message_specific(HeadAndProp, ReqH, OptH)
    end.

-spec(build_message_specific/3 :: (proplist() | tuple(), list(binary()), list(binary())) -> {ok, iolist()} | {error, string()}).
build_message_specific({Headers, Prop}, ReqH, OptH) ->
    case update_required_headers(Prop, ReqH, Headers) of
	{error, _Reason} = Error ->
	    io:format("Build Error: ~p~nReqHeaders: ~p~nPassed: ~p~n", [Error, ReqH, Prop]),
	    Error;
	{Headers1, Prop1} ->
	    {Headers2, _Prop2} = update_optional_headers(Prop1, OptH, Headers1),
	    headers_to_json(Headers2)
    end;
build_message_specific(Prop, ReqH, OptH) ->
    build_message_specific({[], Prop}, ReqH, OptH).


-spec(headers_to_json/1 :: (HeadersProp :: proplist()) -> {ok, iolist()} | {error, string()}).
headers_to_json(HeadersProp) ->
    try
	JSON = mochijson2:encode({struct, HeadersProp}),
	{ok, JSON}
    catch
	throw:E -> {error, io_lib:format("WHISTLE TO_JSON THROW ERROR: ~s~n~p", [E, HeadersProp])};
	error:E -> {error, io_lib:format("WHISTLE TO_JSON ERROR: ~s~n~p", [E, HeadersProp])};
	exit:E -> {error, io_lib:format("WHISTLE TO_JSON EXIT ERROR: ~s~n~p", [E, HeadersProp])}
    end.

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
			      io:format("WHISTLE_API.has_all: Failed to find ~p~nProp: ~p~n", [Header, Prop]),
			      false
		      end
	      end, Headers).

%% Checks Prop against a list of optional headers, returns true | false if at least one if found
-spec(has_any/2 :: (Prop :: proplist(), Headers :: list(binary())) -> boolean()).
has_any(Prop, Headers) ->
    lists:any(fun(Header) -> is_defined(Header, Prop) end, Headers).

%% checks Prop against a list of values to ensure known key/value pairs are correct (like Event-Category
%% and Event-Name). We don't care if a key is defined in Values and not in Prop; that is handled by has_all/1
values_check(Prop, Values) ->
    lists:all(fun({Key, Vs}) when is_list(Vs) ->
		      case get_value(Key, Prop) of
			  undefined -> true; % isn't defined in Prop, has_all will error if req'd
			  V -> case lists:member(V, Vs) of
				   true -> true;
				   false ->
				       io:format("WHISTLE_API.values_check: K: ~p V: ~p not in ~p~n", [Key, V, Vs]),
				       false
			       end
		      end;
		 ({Key, V}) ->
		      case get_value(Key, Prop) of
			  undefined -> true; % isn't defined in Prop, has_all will error if req'd
			  V -> true;
			  _Val ->
			      io:format("WHISTLE_API.values_check: Key: ~p Set: ~p Expected: ~p~n", [Key, _Val, V]),
			      false
		      end
	      end, Values).

%% checks Prop against a list of {Key, Fun}, running the value of Key through Fun, which returns a
%% boolean.
type_check(Prop, Types) ->
    lists:all(fun({Key, Fun}) ->
		      case get_value(Key, Prop) of
			  undefined -> true; % isn't defined in Prop, has_all will error if req'd
			  Value -> case Fun(Value) of % returns boolean
				       true -> true;
				       false ->
					   io:format("WHISTLE_API.type_check: K: ~p V: ~p failed fun~n", [Key, Value]),
					   false
				   end
		      end
	      end, Types).
