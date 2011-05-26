%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Monitor API Helpers
%%%
%%% Most API functions take a proplist, filter it against required headers
%%% and optional headers, and return either the JSON string if all
%%% required headers (default AND API-call-specific) are present, or an
%%% error if some headers are missing.
%%%
%%% To only check the validity, use the api call's corresponding *_v/1 function.
%%% This will parse the proplist and return a boolean if the proplist is valid
%%% for creating a JSON message.
%%%
%%% See http://corp.switchfreedom.com/mediawiki/
%%% @end
%%% Created : 12 Aug 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_api).

%% API
-export([default_headers/4, default_headers/3, optional_default_headers/3]).
-export([prepare_amqp_prop/1, extract_defaults/1, extract_nondefault/1]).

%% Monitor Agent Ping
-export([ping_net_req/1, ping_net_resp/1]).
-export([ping_net_req_v/1, ping_net_resp_v/1]).

%% Monitor Agent Basic Call
-export([basic_call_req/1, basic_call_resp/1]).
-export([basic_call_req_v/1, basic_call_resp_v/1]).

-import(proplists, [get_value/2, get_value/3, delete/2, is_defined/2]).

-include("monitor_api.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Default Headers in all messages - see wiki
%% Creates the seed proplist for the eventual message to be sent
%% All fields are required general headers.
%% @end
%%--------------------------------------------------------------------
%% -spec(default_headers/4 :: (ServerID :: binary(), EvtCat :: binary(), EvtName :: binary()), MsgID :: binary() -> proplist()).
default_headers(ServerID, EvtCat, EvtName, MsgID) ->
    [
      {<<"Msg-ID">>, MsgID}
     ,{<<"Server-ID">>, ServerID}
     ,{<<"Event-Category">>, EvtCat}
     ,{<<"Event-Name">>, EvtName}
     ,{<<"App-Name">>, <<"monitor">>}
     ,{<<"App-Version">>, <<"0.1.0">>}
    ].

-spec(default_headers/3 :: (ServerID :: binary(), EvtCat :: binary(), EvtName :: binary()) -> proplist()).
default_headers(ServerID, EvtCat, EvtName) ->
    [
      {<<"Msg-ID">>, monitor_util:to_binary(monitor_util:uuid())}
     ,{<<"Server-ID">>, ServerID}
     ,{<<"Event-Category">>, EvtCat}
     ,{<<"Event-Name">>, EvtName}
     ,{<<"App-Name">>, <<"monitor">>}
     ,{<<"App-Version">>, <<"0.1.0">>}
    ].

optional_default_headers(Job_ID, Name, Iteration) ->
    [
      {"Task-Name", Name}
     ,{"Job-ID", Job_ID}
     ,{"Task-Iteration", Iteration}
    ].

%%--------------------------------------------------------------------
%% @doc Extract just the default headers from a message
%% @end
%%--------------------------------------------------------------------
-spec(extract_defaults/1 :: (Prop :: proplist()) -> proplist()).
extract_defaults(Prop) ->
    lists:foldl(fun(H, Acc) -> [{H, get_value(H, Prop)} | Acc] end, [], ?DEFAULT_HEADERS).

extract_nondefault(Prop) ->
    lists:foldl(fun(H, Acc) -> proplists:delete(H, Acc) end, Prop, ?DEFAULT_HEADERS).

prepare_amqp_prop([{_, _}|_]=Prop) ->
    lists:map(fun({K,V}) -> {monitor_util:to_binary(K), monitor_util:to_binary(V)} end, Prop);

prepare_amqp_prop(ListOfPropLists) ->
    prepare_amqp_prop(lists:append(ListOfPropLists)).

%%--------------------------------------------------------------------
%% @doc Monitor Agent Network Ping Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(ping_net_req/1 :: (Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
ping_net_req(Prop) ->
    case ping_net_req_v(Prop) of
	true -> build_message(Prop, ?PING_NET_REQ_HEADERS, ?OPTIONAL_PING_NET_REQ_HEADERS);
	false -> {error, "Proplist failed validation for ping_net_req"}
    end.

-spec(ping_net_req_v/1 :: (Prop :: proplist()) -> boolean()).
ping_net_req_v(Prop) ->
    validate(Prop, ?PING_NET_REQ_HEADERS, ?PING_NET_REQ_VALUES, ?PING_NET_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Monitor Agent Network Ping Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(ping_net_resp/1 :: (Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
ping_net_resp(Prop) ->
    case ping_net_resp_v(Prop) of
	true -> build_message(Prop, ?PING_NET_RESP_HEADERS, ?OPTIONAL_PING_NET_RESP_HEADERS);
	false -> {error, "Proplist failed validation for ping_net_resp"}
    end.

-spec(ping_net_resp_v/1 :: (Prop :: proplist()) -> boolean()).
ping_net_resp_v(Prop) ->
    validate(Prop, ?PING_NET_RESP_HEADERS, ?PING_NET_RESP_VALUES, ?PING_NET_RESP_TYPES).


%%--------------------------------------------------------------------
%% @doc Monitor Agent Basic Call Request - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(basic_call_req/1 :: (Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
basic_call_req(Prop) ->
    case basic_call_req_v(Prop) of
    true -> build_message(Prop, ?BASIC_CALL_REQ_HEADERS, ?OPTIONAL_BASIC_CALL_REQ_HEADERS);
    false -> {error, "Proplist failed validation for basic_call_req"}
    end.

-spec(basic_call_req_v/1 :: (Prop :: proplist()) -> boolean()).
basic_call_req_v(Prop) ->
    validate(Prop, ?BASIC_CALL_REQ_HEADERS, ?BASIC_CALL_REQ_VALUES, ?BASIC_CALL_REQ_TYPES).

%%--------------------------------------------------------------------
%% @doc Monitor Agent Basic Call Response - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec(basic_call_resp/1 :: (Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
basic_call_resp(Prop) ->
    case basic_call_resp_v(Prop) of
    true -> build_message(Prop, ?BASIC_CALL_RESP_HEADERS, ?OPTIONAL_BASIC_CALL_RESP_HEADERS);
    false -> {error, "Proplist failed validation for ping_net_resp"}
    end.

-spec(basic_call_resp_v/1 :: (Prop :: proplist()) -> boolean()).
basic_call_resp_v(Prop) ->
    validate(Prop, ?BASIC_CALL_RESP_HEADERS, ?BASIC_CALL_RESP_VALUES, ?BASIC_CALL_RESP_TYPES).

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

-spec(build_message/3 :: (Prop :: proplist(), ReqH :: list(binary()), OptH :: list(binary())) -> tuple(ok, iolist()) | tuple(error, string())).
build_message(Prop, ReqH, OptH) ->
    case defaults(Prop) of
	{error, _Reason}=Error ->
	    io:format("Build Error: ~p~nDefHeaders: ~p~nPassed: ~p~n", [Error, ?DEFAULT_HEADERS, Prop]),
	    Error;
	HeadAndProp ->
	    build_message_specific(HeadAndProp, ReqH, OptH)
    end.

-spec(build_message_specific/3 :: (proplist() | tuple(), list(binary()), list(binary())) -> tuple(ok, iolist()) | tuple(error, string())).
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
io:format("MARKER: ~p~n", [Prop]),
    build_message_specific({[], Prop}, ReqH, OptH).


-spec(headers_to_json/1 :: (HeadersProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
headers_to_json(HeadersProp) ->
    try
	JSON = mochijson2:encode({struct, HeadersProp}),
	{ok, JSON}
    catch
	throw:E -> {error, io_lib:format("MONITOR TO_JSON THROW ERROR: ~p~n~p", [E, HeadersProp])};
	error:E -> {error, io_lib:format("MONITOR TO_JSON ERROR: ~p~n~p", [E, HeadersProp])};
	exit:E -> {error, io_lib:format("MONITOR TO_JSON EXIT ERROR: ~p~n~p", [E, HeadersProp])}
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
			      io:format("MONITOR_API.has_all: Failed to find ~p~nProp: ~p~n", [Header, Prop]),
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
				       io:format("MONITOR_API.values_check: K: ~p V: ~p not in ~p~n", [Key, V, Vs]),
				       false
			       end
		      end;
		 ({Key, V}) ->
		      case get_value(Key, Prop) of
			  undefined -> true; % isn't defined in Prop, has_all will error if req'd
			  V -> true;
			  _Val ->
			      io:format("MONITOR_API.values_check: Key: ~p Set: ~p Expected: ~p~n", [Key, _Val, V]),
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
					   io:format("MONITOR_API.type_check: K: ~p V: ~p failed fun~n", [Key, Value]),
					   false
				   end
		      end
	      end, Types).
