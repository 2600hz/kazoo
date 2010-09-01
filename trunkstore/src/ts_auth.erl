%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Respond to Authentication requests
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(ts_auth).

%% API
-export([handle_req/1]).

-define(APP_NAME, <<"ts_responder.auth">>).
-define(APP_VERSION, <<"0.1">>).

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-type proplist() :: list(tuple(binary(), binary())). % just want to deal with binary K/V pairs

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Give Prop, the Auth API request, create the API response JSON
%% @end
%%--------------------------------------------------------------------
-spec(handle_req/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
handle_req(Prop) ->
    User = get_value(<<"Auth-User">>, Prop),
    Domain = get_value(<<"Auth-Domain">>, Prop),
    From = get_value(<<"From">>, Prop),
    To = get_value(<<"To">>, Prop),

    %% lookup carrier
    case is_inbound(From) of
	{true, CarrierInfo} ->
	    %% tell ts_route to create a handler for From/To combo as an inbound call
	    %% return auth response
	    response(CarrierInfo, Prop);
	false ->
	    case is_outbound(From) of
		{true, CustomerInfo} ->
		    %% tell ts_route to create a handler for From/To combo as an outbound call
		    %% return auth response
		    response(CustomerInfo, Prop);
		false ->
		    response(403, Prop)
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(is_inbound/1 :: (From :: binary()) -> {true, proplist()} | false).
is_inbound(From) ->
    %% lookup in couch the From address to see if its a known carrier
    %% return {true, CarrierInfo} or false
    false.

-spec(is_outbound/1 :: (From :: binary()) -> {true, proplist()} | false).
is_outbound(From) ->
    %% lookup in couch the From address to see if its a known customer
    %% return {true, CustomerInfo} or false
    false.

-spec(response/2 :: (RespData :: proplist(), Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
response(RespData, Prop) ->
    Data0 = [{<<"App-Name">>, ?APP_NAME}
	     ,{<<"App-Version">>, ?APP_VERSION}
	     | Prop],
    Data1 = lists:umerge(specific_response(RespData), Data0),
    whistle_api:auth_resp(Data1).

-spec(specific_response/1 :: (Info :: proplist()) -> proplist();
			     (integer()) -> proplist()).
specific_response(Info) when is_list(Info) ->
    [{<<"Auth-Pass">>, get_value(<<"password">>, Info)}
     ,{<<"Auth-Method">>, get_value(<<"method">>, Info)}
     ,{<<"Access-Group">>, get_value(<<"access-group">>, Info, <<"ignore">>)}
     ,{<<"Tenant-ID">>, get_value(<<"tenant-id">>, Info)}];
specific_response(403) ->
    [{<<"Auth-Method">>, <<"error">>}
     ,{<<"Auth-Pass">>, <<"403 Forbidden">>}
     ,{<<"Access-Group">>, <<"ignore">>}
     ,{<<"Tenant-ID">>, <<"ignore">>}].
