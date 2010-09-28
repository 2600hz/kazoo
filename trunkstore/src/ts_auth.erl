%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Respond to Authentication requests
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_auth).

%% API
-export([handle_req/1]).

-define(APP_NAME, <<"ts_responder.auth">>).
-define(APP_VERSION, <<"0.1">>).

-include_lib("kernel/include/inet.hrl"). %% for hostent record, used in find_ip/1
-include("ts.hrl").

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Give Prop, the Auth API request, create the API response JSON
%% @end
%%--------------------------------------------------------------------
-spec(handle_req/1 :: (Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
handle_req(Prop) ->
    [_FromUser, FromDomain] = binary:split(get_value(<<"From">>, Prop), <<"@">>),
    {AuthU, AuthD} = {get_value(<<"Auth-User">>, Prop), get_value(<<"Auth-Domain">>, Prop)},

    ViewInfo = case is_inbound(FromDomain) of
		   true ->
		       Direction = <<"inbound">>,
		       lookup_user(AuthU, AuthD);
		   false ->
		       Direction = <<"outbound">>,
		       lookup_user(AuthU, AuthD)
	       end,

    Defaults = [{<<"Msg-ID">>, get_value(<<"Msg-ID">>, Prop)}
		,{<<"Custom-Channel-Vars">>, {struct, [{<<"Direction">>, Direction}]}} | 
		whistle_api:default_headers(<<>>
					    ,get_value(<<"Event-Category">>, Prop)
					    ,get_value(<<"Event-Name">>, Prop)
					    ,?APP_NAME
					    ,?APP_VERSION)],
    Info = case ViewInfo of
	       {error, Reason} ->
		   format_log(error, "TS_AUTH(~p): Sending a 500 for error: ~p~n", [self(), Reason]),
		   500;
	       [] ->
		   case Direction of
		       <<"inbound">> ->
			   %% send urgent email or alert for Phantom Number
			   format_log(error, "TS_AUTH(~p): Alert! ~p@~p was routed to us~n", [self(), AuthU, AuthD]);
		       <<"outbound">> ->
			   %% unauthed user trying to make calls, alert
			   format_log(error, "TS_AUTH(~p): Sending a 403 ~p~n", [self()])
		   end,
		   403;
	       _ ->
		   format_log(error, "TS_AUTH(~p): Sending a 200~n", [self()]),
		   ViewInfo
	   end,
    response(Info, Defaults).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(is_inbound/1 :: (Domain :: binary()) -> boolean()).
is_inbound(Domain) ->
    IP = find_ip(Domain),
    Options = [{"key", IP}],
    format_log(info, "TS_AUTH(~p): lookup_carrier using ~p(~p) in ~p~n", [self(), Domain, IP, ?TS_VIEW_CARRIERIP]),
    case ts_couch:has_view(?TS_DB, ?TS_VIEW_CARRIERIP) andalso
	ts_couch:get_results(?TS_DB, ?TS_VIEW_CARRIERIP, Options) of
	false ->
	    format_log(error, "TS_AUTH(~p): No ~p view found while looking up ~p(~p)~n"
		       ,[self(), ?TS_VIEW_CARRIERIP, Domain, IP]),
	    false;
	[] ->
	    format_log(info, "TS_AUTH(~p): No Carrier matching ~p(~p)~n", [self(), Domain, IP]),
	    false;
	[{ViewProp} | _Rest] ->
	    format_log(info, "TS_AUTH(~p): Carrier found for ~p(~p)~n~p~n", [self(), Domain, IP, ViewProp]),
	    true;
	_Else ->
	    format_log(error, "TS_AUTH(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    false
    end.

-spec(lookup_user/2 :: (Name :: binary(), Domain :: binary()) -> proplist() | {error, string()}).
lookup_user(Name, Domain) ->
    IP = find_ip(Domain),
    Options = [{"key", IP}],
    format_log(info, "TS_AUTH(~p): lookup_user with ~p and ~p(~p) in ~p.~p~n"
	       ,[self(), Name, Domain, IP, ?TS_DB, ?TS_VIEW_IPAUTH]),
    case ts_couch:has_view(?TS_DB, ?TS_VIEW_IPAUTH) andalso
	ts_couch:get_results(?TS_DB, ?TS_VIEW_IPAUTH, Options) of
	false ->
	    format_log(error, "TS_AUTH(~p): No ~p view found while looking up ~p(~p)~n"
		       ,[self(), ?TS_VIEW_IPAUTH, Domain, IP]),
	    {error, "No view found."};
	[] ->
	    format_log(info, "TS_AUTH(~p): No Domain matching ~p(~p)~n", [self(), Domain, IP]),
	    lookup_user(Name);
	[{ViewProp} | _Rest] ->
	    format_log(info, "TS_AUTH(~p): Using ~p(~p), retrieved~n~p~n", [self(), Domain, IP, ViewProp]),
	    ViewProp;
	_Else ->
	    format_log(error, "TS_AUTH(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    {error, "Unexpected error in lookup_user/2"}
    end.

-spec(lookup_user/1 :: (Name :: binary()) -> proplist() | {error, string()}).
lookup_user(Name) ->
    Options = [{"key", Name}],
    case ts_couch:has_view(?TS_DB, ?TS_VIEW_USERAUTH) andalso
	ts_couch:get_results(?TS_DB, ?TS_VIEW_USERAUTH, Options) of
	false ->
	    format_log(error, "TS_AUTH(~p): No ~p view found while looking up ~p~n", [self(), ?TS_VIEW_USERAUTH, Name]),
	    {error, "No view found."};
	[] ->
	    format_log(info, "TS_AUTH(~p): No Name matching ~p~n", [self(), Name]),
	    [];
	[{ViewProp} | _Rest] ->
	    format_log(info, "TS_AUTH(~p): Using ~p, retrieved~n~p~n", [self(), Name, ViewProp]),
	    ViewProp;
	_Else ->
	    format_log(error, "TS_AUTH(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    {error, "Unexpeced error in lookup_user/1"}
    end.

-spec(response/2 :: (RespData :: proplist() | integer(), Prop :: proplist()) -> {ok, iolist()} | {error, string()}).
response(ViewInfo, Prop) ->
    Data = lists:umerge(specific_response(ViewInfo), Prop),
    whistle_api:auth_resp(Data).

-spec(specific_response/1 :: (ViewInfo :: proplist() | integer()) -> proplist()).
specific_response(ViewInfo) when is_list(ViewInfo) ->
    {AuthProp} = get_value(<<"value">>, ViewInfo),
    {Info} = get_value(<<"auth">>, AuthProp),
    [{<<"Auth-Password">>, get_value(<<"AuthPassword">>, Info)}
     ,{<<"Auth-Method">>, get_value(<<"AuthMethod">>, Info)}
     ,{<<"Event-Name">>, <<"auth_resp">>}
     ,{<<"Access-Group">>, get_value(<<"Access-Group">>, Info, <<"ignore">>)}
     ,{<<"Tenant-ID">>, get_value(<<"Tenant-ID">>, Info, <<"ignore">>)}
     ,{<<"Custom-Channel-Vars">>, {struct, [{<<"Custom1Key">>, <<"Custom1Value">>}]}}
    ];
specific_response(500) ->
    [{<<"Auth-Method">>, <<"error">>}
     ,{<<"Auth-Pass">>, <<"500 Internal Error">>}
     ,{<<"Access-Group">>, <<"ignore">>}
     ,{<<"Tenant-ID">>, <<"ignore">>}];
specific_response(403) ->
    [{<<"Auth-Method">>, <<"error">>}
     ,{<<"Auth-Pass">>, <<"403 Forbidden">>}
     ,{<<"Access-Group">>, <<"ignore">>}
     ,{<<"Tenant-ID">>, <<"ignore">>}].

find_ip(Domain) when is_binary(Domain) ->
    find_ip(binary_to_list(Domain));
find_ip(Domain) when is_list(Domain) ->
    case inet:gethostbyname(Domain, inet) of %% eventually we'll want to support both IPv4 and IPv6
	{error, Err} ->
	    format_log(error, "TS_AUTH(~p): Failed to find ip for ~p: ~p~n", [self(), Domain, Err]),
	    Domain;
	{ok, Hostent} when is_record(Hostent, hostent) ->
	    case Hostent#hostent.h_addr_list of
		[] ->
		    format_log(error, "TS_AUTH(~p): Failed to find addrs for ~p: ~p~n", [self(), Domain, Hostent]),
		    Domain;
		[Addr | _Rest] ->
		    format_log(info, "TS_AUTH(~p): Found ~p addresses, using the first ~p~n", [self(), _Rest, Addr]),
		    inet_parse:ntoa(Addr)
	    end
    end.
	    
	    
