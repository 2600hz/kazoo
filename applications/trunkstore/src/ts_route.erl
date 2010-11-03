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
-spec(handle_req/2 :: (ApiProp :: proplist(), ServerID :: binary()) -> tuple(ok, iolist(), tuple()) | tuple(error, string())).
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
-spec(inbound_handler/2 :: (ApiProp :: list(), ServerID :: binary()) -> tuple(ok, iolist(), tuple()) | tuple(error, string())).
inbound_handler(ApiProp, ServerID) ->
    format_log(info, "TS_ROUTE(~p): Inbound handler starting...~n", [self()]),
    [ToUser, _ToDomain] = binary:split(get_value(<<"To">>, ApiProp), <<"@">>),
    Did = ts_util:to_e164(ToUser),
    Flags = create_flags(Did, ApiProp),
    process_routing(inbound_features(Flags), ApiProp, ServerID).

-spec(outbound_handler/2 :: (ApiProp :: list(), ServerID :: binary()) -> tuple(ok, iolist(), tuple()) | tuple(error, string())).
outbound_handler(ApiProp, ServerID) ->
    format_log(info, "TS_ROUTE(~p): Outbound handler starting...~n", [self()]),
    Did = ts_util:to_e164(get_value(<<"Caller-ID-Number">>, ApiProp, <<>>)),
    Flags = create_flags(Did, ApiProp),
    process_routing(outbound_features(Flags), ApiProp, ServerID).

-spec(lookup_user/1 :: (Name :: binary()) -> tuple(ok | error, proplist() | string())).
lookup_user(Name) ->
    Options = [{"keys", [Name]}],
    case ts_couch:get_results(?TS_DB, ?TS_VIEW_USERAUTH, Options) of
	false ->
	    format_log(error, "TS_ROUTE(~p): No ~p view found while looking up ~p~n", [self(), ?TS_VIEW_USERAUTH, Name]),
	    {error, "No view found."};
	[] ->
	    format_log(info, "TS_ROUTE(~p): No Name matching ~p~n", [self(), Name]),
	    [];
	[{ViewProp} | _Rest] ->
	    format_log(info, "TS_ROUTE(~p): Using user ~p, retrieved~n~p~n", [self(), Name, ViewProp]),
	    DocID = get_value(<<"id">>, ViewProp),
	    case ts_couch:open_doc(?TS_DB, DocID) of
		{error, E}=Error ->
		    format_log(error, "TS_ROUTE(~p): Failed to retrieve doc ~p: ~p~n", [self(), DocID, E]),
		    Error;
		Doc ->
		    {ok, Doc}
	    end;
	_Else ->
	    format_log(error, "TS_ROUTE(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    {error, "Unexpeced error in lookup_user/1"}
    end.

-spec(lookup_did/1 :: (Did :: binary()) -> tuple(ok | error, proplist() | string())).
lookup_did(Did) ->
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
	    {ok, Value};
	_Else ->
	    format_log(error, "TS_ROUTE(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    {error, "Unexpected error in outbound_handler"}
    end.

-spec(process_routing/3 :: (Flags :: tuple(), ApiProp :: proplist(), ServerID :: binary()) -> tuple(ok, iolist(), tuple()) | tuple(error, string())).
process_routing(Flags, ApiProp, ServerID) ->
    format_log(info, "TS_ROUTE(~p): Flags for routing...~n~p~n", [self(), Flags]),
    case ts_credit:check(Flags) of
	{ok, Flags1} ->
	    %% call may proceed
	    find_route(Flags1, ApiProp, ServerID);
	{error, Error} ->
	    format_log(error, "TS_ROUTE(~p): Credit Error ~p~n", [self(), Error]),
	    response(503, ApiProp, Flags, ServerID)
    end.

-spec(find_route/3 :: (Flags :: tuple(), ApiProp :: proplist(), ServerID :: binary()) -> tuple(ok, iolist(), tuple()) | tuple(error, string())).
find_route(Flags, ApiProp, ServerID) ->
    case Flags#route_flags.direction =:= <<"outbound">> of
	false ->
	    %% handle inbound routing
	    case inbound_route(Flags) of
		{ok, Routes} ->
		    format_log(info, "TS_ROUTE(~p): Generated Inbound Routes~n~p~n", [self(), Routes]),
		    response(Routes, ApiProp, Flags, ServerID);
		{error, Error} ->
		    format_log(error, "TS_ROUTE(~p): Inbound Routing Error ~p~n", [self(), Error]),
		    response(404, ApiProp, Flags, ServerID)
	    end;
	true ->
	    case ts_carrier:route(Flags) of
		{ok, Routes} ->
		    format_log(info, "TS_ROUTE(~p): Generated Outbound Routes~n~p~n", [self(), Routes]),
		    response(Routes, ApiProp, Flags, ServerID);
		{error, Error} ->
		    format_log(error, "TS_ROUTE(~p): Outbound Routing Error ~p~n", [self(), Error]),
		    response(404, ApiProp, Flags, ServerID)
	    end
    end.

-spec(inbound_route/1 :: (Flags :: tuple()) -> tuple(ok | error, proplist() | string())).
inbound_route(Flags) ->
    format_log(info, "TS_ROUTE(~p): Inbound route flags: ~p~n", [self(), Flags]),
    DID = case Flags#route_flags.inbound_format of
	      <<"E.164">> -> ts_util:to_e164(Flags#route_flags.to_user);
	      <<"1NPANXXXXXX">> -> ts_util:to_1npanxxxxxx(Flags#route_flags.to_user);
	      _ -> ts_util:to_npanxxxxxx(Flags#route_flags.to_user)
	  end,
    Dialstring = [list_to_binary(["user:", Flags#route_flags.auth_user]), DID],
    Route = [{<<"Route">>, Dialstring}
	     ,{<<"Weight-Cost">>, <<"1">>}
	     ,{<<"Weight-Location">>, <<"1">>}
	     ,{<<"Media">>, <<"bypass">>}
	    ],
    case whistle_api:route_resp_route_v(Route) of
	true -> {ok, add_failover_route(Flags#route_flags.failover, Flags, [{struct, Route}])};
	false ->
	    format_log(error, "TS_ROUTE(~p): Failed to validate Route ~p~n", [self(), Route]),
	    {error, "Inbound route validation failed"}
    end.

add_failover_route({}, _Flags, Rs) -> Rs;
%% route to a SIP URI
add_failover_route({<<"SIP-URI">>, URI}, Flags, Rs) ->
    Rs;
%% route to a E.164 number - need to setup outbound for this sucker
add_failover_route({<<"E.164">>, DID}, Flags, Rs) ->
    Rs.

    

-spec(inbound_features/1 :: (Flags :: tuple()) -> tuple()).
inbound_features(Flags) ->
    Features = [],
    fold_features(Features, Flags).

-spec(outbound_features/1 :: (Flags :: tuple()) -> tuple()).
outbound_features(Flags) ->
    Features = [ts_e911],
    fold_features(Features, Flags).

-spec(fold_features/2 :: (Features :: list(atom()), Flags :: tuple()) -> tuple()).
fold_features(Features, Flags) ->
    lists:foldl(fun(Mod, Flags0) ->
			Mod:process_flags(Flags0)
		end, Flags, Features).

-spec(create_flags/2 :: (Did :: binary(), ApiProp :: proplist()) -> tuple()).
create_flags(Did, ApiProp) ->
    {struct, ChannelVars} = get_value(<<"Custom-Channel-Vars">>, ApiProp, {struct, []}),
    AuthUser = get_value(<<"Auth-User">>, ChannelVars, <<>>),
    Doc = lookup_user(AuthUser),

    F1 = case lookup_did(Did) of
	     {ok, DidProp} ->
		 flags_from_did(DidProp, #route_flags{});
	     {error, _E} ->
		 #route_flags{}
	 end,
    F3 = case Doc of
	     {error, _E1} -> F1;
	     D when is_list(D) ->
		 F2 = flags_from_srv(AuthUser, D, F1),
		 flags_from_account(D, F2);
	     {ok, D} ->
		 F2 = flags_from_srv(AuthUser, D, F1),
		 flags_from_account(D, F2)
	 end,
    flags_from_api(ApiProp, ChannelVars, F3).

-spec(flags_from_api/3 :: (ApiProp :: proplist(), ChannelVars :: proplist(), Flags :: tuple()) -> tuple()).
flags_from_api(ApiProp, ChannelVars, Flags) ->
    [ToUser, ToDomain] = binary:split(get_value(<<"To">>, ApiProp), <<"@">>),
    [FromUser, FromDomain] = binary:split(get_value(<<"From">>, ApiProp), <<"@">>),

    F0 = case Flags#route_flags.caller_id of
	     {} -> Flags#route_flags{caller_id = {get_value(<<"Caller-ID-Name">>, ApiProp, <<>>)
						  ,get_value(<<"Caller-ID-Number">>, ApiProp, <<>>)
						 }};
	     _CID -> Flags
	 end,
    F0#route_flags{
      to_user = ts_util:to_e164(ToUser)
      ,to_domain = ToDomain
      ,from_user = ts_util:to_e164(FromUser)
      ,from_domain = FromDomain
      ,auth_user = get_value(<<"Auth-User">>, ChannelVars, Flags#route_flags.auth_user)
      ,direction = get_value(<<"Direction">>, ChannelVars, <<"inbound">>)
     }.

%% Flags from the DID
%% - Failover
%% - Caller ID
%% - Auth User
%% - Trunks
%% - Credit Available
-spec(flags_from_did/2 :: (DidProp :: proplist(), Flags :: tuple()) -> tuple()).
flags_from_did(DidProp, Flags) ->
    {DidOptions} = get_value(<<"DID_Opts">>, DidProp, {[]}),
    {AcctOpts} = get_value(<<"account">>, DidProp, {[]}),
    {AuthOpts} = get_value(<<"auth">>, DidProp, {[]}),

    Trunks = ts_util:to_integer(get_value(<<"trunks">>, AcctOpts, 0)),
    format_log(info, "Got ~p trunks from ~p~n", [Trunks, AcctOpts]),

    Opts = get_value(<<"options">>, DidProp, {[]}),

    F0 = add_failover(Flags, get_value(<<"failover">>, DidOptions, {[]})),
    F1 = add_caller_id(F0, get_value(<<"caller_id">>, DidOptions, {[]})),
    F1#route_flags{route_options = Opts
		   ,auth_user = get_value(<<"auth_user">>, AuthOpts, <<>>)
		   ,trunks = Trunks
		   ,credit_available = ts_util:to_float(get_value(<<"account_credit">>, DidProp, 0.0))
		  }.

%% Flags from the Server
%% - Inbound Format <- what format does the server expect the inbound caller-id in?
%% - Codecs <- list of codecs supported by the server
%% - Caller Id <- only if it hasn't been set on the DID level
%% - Failover <- only if it hasn't been set on the DID level
%% - Trunks <- Max trunks allowed on the server
%% - 
-spec(flags_from_srv/3 :: ( AuthUser :: binary(), Doc :: proplist(), Flags :: tuple()) -> tuple()).
flags_from_srv(AuthUser, Doc, Flags) ->
    Srv = lookup_server(AuthUser, Doc),

    F0 = Flags#route_flags{inbound_format=get_value(<<"inbound_format">>, Srv, <<"NPANXXXXXX">>)
			   ,codecs=get_value(<<"codecs">>, Srv, [])
			   %,trunks = Trunks
			   %,credit_available = ts_util:to_float(get_value(<<"account_credit">>, DidProp, 0.0))
			  },
    F1 = add_caller_id(F0, get_value(<<"caller_id">>, Srv, {[]})),
    add_failover(F1, get_value(<<"failover">>, Srv, {[]})).

%% Flags from the Account
%% - Credit available
%% - Trunks purchased <- eventually need to look at the server under the account to see how many are allocatable to the server
%% - Trunks in use
%% - Caller ID <- only if it hasn't been set at the server or DID level
%% - Failover <- only if it hasn't been set at the server or DID level
-spec(flags_from_account(Doc :: proplist(), Flags :: tuple()) -> tuple()).
flags_from_account(Doc, Flags) ->
    {Acct} = get_value(<<"account">>, Doc, {[]}),
    {Credit} = get_value(<<"credit">>, Acct, {[]}),
    Trunks = ts_util:to_integer(get_value(<<"trunks">>, Acct, Flags#route_flags.trunks)),
    TrunksInUse = ts_util:to_integer(get_value(<<"trunks_in_use">>, Acct, Flags#route_flags.trunks_in_use)),

    F0 = Flags#route_flags{credit_available = ts_util:to_float(get_value(<<"prepay">>, Credit, 0.0))
			   ,trunks = Trunks
			   ,trunks_in_use = TrunksInUse
			  },
    F1 = add_caller_id(F0, get_value(<<"caller_id">>, Acct, {[]})),
    add_failover(F1, get_value(<<"failover">>, Acct, {[]})).

-spec(add_failover/2 :: (F0 :: tuple(), FOver :: tuple(proplist())) -> tuple()).
add_failover(#route_flags{failover={}}=F0, {[]}) -> F0;
add_failover(#route_flags{failover={}}=F0, {[{_K, _V}=FOver]}) ->
    F0#route_flags{failover=FOver};
add_failover(F, _) -> F.

-spec(add_caller_id/2 :: (F0 :: tuple(), CID :: tuple(proplist())) -> tuple()).
add_caller_id(#route_flags{caller_id={}}=F0, {[]}) -> F0;
add_caller_id(#route_flags{caller_id={}}=F0, {CID}) ->
    F0#route_flags{caller_id = {get_value(<<"cid_name">>, CID, <<>>)
				,get_value(<<"cid_number">>, CID, <<>>)}};
add_caller_id(F, _) -> F.

-spec(lookup_server/2 :: (AuthUser :: binary(), Doc :: proplist()) -> proplist()).
lookup_server(AuthUser, Doc) ->
    case lists:filter(fun({S}) ->
			      {Auth} = get_value(<<"auth">>, S, {[]}),
			      get_value(<<"auth_user">>, Auth, <<>>) =:= AuthUser
		      end, get_value(<<"servers">>, Doc, [])) of
	[{Srv}] -> Srv;
	_ -> []
    end.

-spec(response/4 :: (Routes :: proplist() | integer(), Prop :: proplist(), Flags :: tuple(), ServerID :: binary()) -> tuple(ok, iolist(), tuple()) | tuple(error, string())).
response(Routes, Prop, Flags, ServerID) ->
    Prop1 = [ {<<"Msg-ID">>, get_value(<<"Msg-ID">>, Prop)}
	      | whistle_api:default_headers(ServerID, <<"dialplan">>, <<"route_resp">>, <<"ts_route">>, <<"0.1">>) ],
    Data = specific_response(Routes) ++ Prop1,
    case whistle_api:route_resp(Data) of
	{ok, JSON} ->
	    {ok, JSON, Flags};
	{error, _E}=Error ->
	    Error
    end.

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
