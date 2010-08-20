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
-export([auth_req/1]).

-import(proplists, [get_value/2, get_value/3, delete/2, is_defined/2]).

-define(DEFAULT_HEADERS, ['Server-ID', 'Event-Category', 'Event-Name'
			  , 'AppName', 'App-Version']).

-define(AUTH_REQ_HEADERS, ['Msg-ID', 'To', 'From', 'Orig-IP'
			   , 'Auth-User', 'Auth-Domain', 'Auth-Pass']).

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
	{error, _Reason}=Error -> Error;
	{Headers, Prop1} ->
	    case has_all(Prop, ?AUTH_REQ_HEADERS) of
		false -> {error, "Missing autentication request headers"};
		true ->
		    {Headers1, Prop2} = add_headers(Prop1, ?AUTH_REQ_HEADERS, Headers),
		    {ok, mochijson2:encode({struct, Headers1})}
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Checks Prop for all default headers, throws error if one is missing
%% defaults(PassedProps) -> { Headers, NewPropList } | {error, Reason}
-spec(defaults/1 :: (Prop :: proplist()) -> {proplist(), proplist()} | {error, list()).
defaults(Prop) ->
    defaults(Prop, []).
defaults(Prop, Headers) ->
    update_required_headers(Prop, ?DEFAULT_HEADERS, Headers).

update_required_headers(Prop, Fields, Headers) ->
    case has_all(Prop, Fields) of 
	true ->
	    add_headers(Prop, Fields, Headers);
	false ->
	    {error, "All required headers not defined"}
    end.

update_optional_headers(Prop, Fields, Headers) ->
    case has_any(Prop, Fields) of
	true ->
	    add_optional_headers(Prop, Fields, Headers);
	false ->
	    Headers
    end.

%% add [Header] from Prop to HeadProp
-spec(add_headers/3 :: (Prop :: proplist(), Headers :: list(string()), HeadProp :: proplist) -> {proplist(), proplist()}).
add_headers(Prop, Headers, HeadProp) ->
    lists:foldl(fun(K, {HeadProp1, KVs}) ->
			{[{K, get_value(K, KVs)} | HeadProp1], delete(K, KVs)}
		end, {HeadProp, Prop}, Headers).

%% Checks Prop against a list of required headers, returns true | false
-spec(has_all/2 :: (Prop :: proplist(), Headers :: list(string())) -> bool()).
has_all(Prop, Headers) ->
    lists:all(fun(Header) -> is_defined(Header, Prop) end, Headers).
