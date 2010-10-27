%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Respond to Route requests
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_route).

%% API
-export([handle_req/2]).

-import(proplists, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("ts.hrl").

-define(APP_NAME, <<"ts_responder.route">>).
-define(APP_VERSION, <<"0.4.0">>).

%%%===================================================================
%%% API
%%%===================================================================
-spec(handle_req/2 :: (ApiProp :: proplist(), ServerID :: binary()) -> {ok, iolist()} | {error, string()}).
handle_req(ApiProp, ServerID) ->
    format_log(info, "TS_ROUTE(~p): Handling Route Request~n", [self()]),
    case get_value(<<"Custom-Channel-Vars">>, ApiProp) of
	undefined ->
	    {error, "No Custom Vars"};
	{struct, []} -> %% assuming call authed via ACL, meaning carrier IP was known, hence an inbound call
	    inbound_handler([{<<"Direction">>, <<"inbound">>} | ApiProp], ServerID);
	{struct, CCVs} ->
	    case get_value(<<"Direction">>, CCVs) of
		<<"outbound">>=D ->
		    outbound_handler([{<<"Direction">>, D} | ApiProp], ServerID);
		<<"inbound">>=D ->
		    inbound_handler([{<<"Direction">>, D} | ApiProp], ServerID)
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(inbound_handler/2 :: (ApiProp :: list(), ServerID :: binary()) -> {ok, iolist()} | {error, string()}).
inbound_handler(ApiProp, ServerID) ->
    format_log(info, "TS_ROUTE(~p): Inbound handler started...~n", [self()]),
    [ToUser, _ToDomain] = binary:split(get_value(<<"To">>, ApiProp), <<"@">>),
    Did = ts_util:to_e164(ToUser),
    Options = [{"key", Did}],
    case ts_couch:get_results(?TS_DB, ?TS_VIEW_DIDLOOKUP, Options) of
	false ->
	    format_log(error, "TS_ROUTE(~p): No ~p view found while looking up ~p(~p)~n"
		       ,[self(), ?TS_VIEW_DIDLOOKUP, Did, _ToDomain]),
	    {error, "No DIDLOOKUP view"};
	[] ->
	    format_log(info, "TS_ROUTE(~p): No DID matching ~p~n", [self(), ToUser]),
	    {error, "No matching DID"};
	[{ViewProp} | _Rest] ->
	    OurDid = get_value(<<"key">>, ViewProp),
	    format_log(info, "TS_ROUTE(~p): DID found for ~p~n~p~n", [self(), OurDid, ViewProp]),
	    {Value} = get_value(<<"value">>, ViewProp),
	    process_routing(inbound_features(set_flags(Value, ApiProp)), ApiProp, ServerID);
	_Else ->
	    format_log(error, "TS_ROUTE(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    {error, "Unexpected error in inbound_handler"}
    end.

-spec(outbound_handler/2 :: (ApiProp :: list(), ServerID :: binary()) -> {ok, iolist()} | {error, string()}).
outbound_handler(Prop, ServerID) ->
    format_log(info, "TS_ROUTE(~p): Outbound handler starting...~n", [self()]),
    Did = ts_util:to_e164(get_value(<<"Caller-ID-Number">>, Prop, <<>>)),
    Options = [{"keys", [Did]}],
    case ts_couch:get_results(?TS_DB, ?TS_VIEW_DIDLOOKUP, Options) of
	false ->
	    format_log(error, "TS_ROUTE(~p): No ~p view found while looking up ~p~n"
		       ,[self(), ?TS_VIEW_DIDLOOKUP, Did]),
	    {error, "No DIDLOOKUP view"};
	[] ->
	    format_log(info, "TS_ROUTE(~p): No DID(s) matching ~p~n", [self(), Options]),
	    {error, "No matching DID"};
	[{ViewProp} | _Rest] ->
	    OurDid = get_value(<<"key">>, ViewProp),
	    format_log(info, "TS_ROUTE(~p): DID found for ~p~n~p~n", [self(), OurDid, ViewProp]),
	    {Value} = get_value(<<"value">>, ViewProp),
	    process_routing(outbound_features(set_flags(Value, Prop)), Prop, ServerID);
	_Else ->
	    format_log(error, "TS_ROUTE(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    {error, "Unexpected error in outbound_handler"}
    end.

-spec(process_routing/3 :: (Flags :: tuple(), ApiProp :: proplist(), ServerID :: binary()) -> {ok, iolist()} | {error, string()}).
process_routing(Flags, ApiProp, ServerID) ->
    case ts_credit:check(Flags) of
	{ok, Flags1} ->
	    %% call may proceed
	    find_route(Flags1, ApiProp, ServerID);
	{error, Error} ->
	    format_log(error, "TS_ROUTE(~p): Credit Error ~p~n", [self(), Error]),
	    response(503, ApiProp, ServerID)
    end.

-spec(find_route/3 :: (Flags :: tuple(), ApiProp :: proplist(), ServerID :: binary()) -> {ok, iolist()} | {error, string()}).
find_route(Flags, ApiProp, ServerID) ->
    case Flags#route_flags.direction =:= <<"outbound">> of
	false ->
	    %% handle inbound routing
	    case inbound_route(Flags) of
		{ok, Routes} ->
		    format_log(info, "TS_ROUTE(~p): Generated Inbound Routes~n~p~n", [self(), Routes]),
		    response(Routes, ApiProp, ServerID);
		{error, Error} ->
		    format_log(error, "TS_ROUTE(~p): Inbound Routing Error ~p~n", [self(), Error]),
		    response(404, ApiProp, ServerID)
	    end;
	true ->
	    case ts_carrier:route(Flags) of
		{ok, Routes} ->
		    format_log(info, "TS_ROUTE(~p): Generated Outbound Routes~n~p~n", [self(), Routes]),
		    response(Routes, ApiProp, ServerID);
		{error, Error} ->
		    format_log(error, "TS_ROUTE(~p): Outbound Routing Error ~p~n", [self(), Error]),
		    response(404, ApiProp, ServerID)
	    end
    end.

-spec(inbound_route/1 :: (Flags :: tuple()) -> {ok, proplist()} | {error, string()}).
inbound_route(Flags) ->
    User = case Flags#route_flags.inbound_format of
	       <<"E.164">> -> ts_util:to_e164(Flags#route_flags.to_user);
	       <<"1NPANXXXXXX">> -> ts_util:to_1npanxxxxxx(Flags#route_flags.to_user);
	       _NPANXXXXXX -> ts_util:to_npanxxxxxx(Flags#route_flags.to_user)
	   end,
    Dialstring = list_to_binary([User, $@, get_value(<<"auth_ip">>, Flags#route_flags.auth_options, Flags#route_flags.to_domain)]),
    Route = [{<<"Route">>, Dialstring}
	     ,{<<"Weight-Cost">>, 1}
	     ,{<<"Weight-Location">>, 1}
	     ,{<<"Media">>, <<"bypass">>}
	    ],
    case whistle_api:route_resp_route_v(Route) of
	true -> {ok, [{struct, Route}]};
	false ->
	    format_log(error, "TS_ROUTE(~p): Failed to validate Route ~p~n", [self(), Route]),
	    {error, "Inbound route validation failed"}
    end.
		  
-spec(inbound_features/1 :: (Flags :: tuple()) -> tuple()).
inbound_features(Flags) ->
    Features = [],
    fold_features(Features, Flags).

-spec(outbound_features/1 :: (Flags :: tuple()) -> tuple()).
outbound_features(Flags) ->
    Features = [ts_e911, ts_t38],
    fold_features(Features, Flags).
    
-spec(fold_features/2 :: (Features :: list(atom()), Flags :: tuple()) -> tuple()).
fold_features(Features, Flags) ->
    lists:foldl(fun(Mod, Flags0) ->
			Mod:process_flags(Flags0)
		end, Flags, Features).

-spec(set_flags/2 :: (DidProp :: proplist(), ApiProp :: proplist()) -> tuple()).
set_flags(DidProp, ApiProp) ->
    {DidOptions} = get_value(<<"DID_Opts">>, DidProp, {[]}),
    {AuthOptions} = get_value(<<"auth">>, DidProp, {[]}),
    {AccountOptions} = get_value(<<"account">>, DidProp, {[]}),

    [ToUser, ToDomain] = binary:split(get_value(<<"To">>, ApiProp), <<"@">>),
    [FromUser, FromDomain] = binary:split(get_value(<<"From">>, ApiProp), <<"@">>),

    {FailOpts} = get_value(<<"failover">>, DidOptions, {[]}),
    {CallerIDOpts} = get_value(<<"caller_id">>, DidOptions, {[]}),
    Opts = get_value(<<"options">>, DidProp, {[]}),

    Trunks = to_integer(get_value(<<"trunks">>, AccountOptions, 0)),

    #route_flags{
	     to_user = ts_util:to_e164(ToUser)
	     ,to_domain = ToDomain
	     ,from_user = ts_util:to_e164(FromUser)
	     ,from_domain = FromDomain
	     ,direction = get_value(<<"Direction">>, ApiProp)
	     ,failover = FailOpts
	     ,callerid = {get_value(<<"cid_name">>, CallerIDOpts, get_value(<<"Caller-ID-Name">>, ApiProp))
			  ,get_value(<<"cid_num">>, CallerIDOpts, get_value(<<"Caller-ID-Number">>, ApiProp))}
	     ,route_options = Opts
	     ,credit_available = get_value(<<"account_credit">>, DidProp)
	     ,flat_rate_enabled = (Trunks > 0)
	     ,auth_options = AuthOptions
	     ,inbound_format = get_value(<<"inbound_format">>, DidProp, <<"NPANXXXXXX">>)
	     %,fax = []
	     %,callerid_default
	     %,flat_rate_enabled -> number of flat rate trunks available
	     %,codecs = [] :: list()
	    }.

-spec(response/3 :: (Routes :: proplist() | integer(), Prop :: proplist(), ServerID :: binary()) -> {ok, iolist()} | {error, string()}).
response(Routes, Prop, ServerID) ->
    Prop1 = [ {<<"Msg-ID">>, get_value(<<"Msg-ID">>, Prop)}
	      | whistle_api:default_headers(ServerID, <<"dialplan">>, <<"route_resp">>, <<"ts_route">>, <<"0.1">>) ],
    Data = specific_response(Routes) ++ Prop1,
    whistle_api:route_resp(Data).

-spec(specific_response/1 :: (CodeOrRoutes :: integer() | proplist()) -> proplist()).
specific_response(404) ->
    [{<<"Routes">>, []}
     ,{<<"Method">>, <<"error">>}
     ,{<<"Route-Error-Code">>, <<"404">>}
     ,{<<"Route-Error-Message">>, <<"Not Found">>}
    ];
specific_response(503) ->
    [{<<"Routes">>, []}
     ,{<<"Method">>, <<"error">>}
     ,{<<"Route-Error-Code">>, <<"503">>}
     ,{<<"Route-Error-Message">>, <<"Insufficient Credit">>}
    ];
specific_response(Routes) when is_list(Routes) ->
    [{<<"Routes">>, Routes}
     ,{<<"Method">>, <<"bridge">>}
    ].

to_integer(X) when is_binary(X) ->
    list_to_integer(binary_to_list(X));
to_integer(X) when is_list(X) ->
    list_to_integer(X);
to_integer(X) when is_integer(X) ->
    X.
