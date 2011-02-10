%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Respond to Route requests
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_route).

%% API
-export([handle_req/1]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [log/2, format_log/3]).

-include("ts.hrl").

-define(APP_NAME, <<"ts_responder.route">>).
-define(APP_VERSION, <<"0.4.5">>).

%%%===================================================================
%%% API
%%%===================================================================
-spec(handle_req/1 :: (ApiProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
handle_req(ApiProp) ->
    format_log(info, "TS_ROUTE(~p): Handling Route Request~n", [self()]),
    case get_value(<<"Custom-Channel-Vars">>, ApiProp) of
	undefined ->
	    {error, "No Custom Vars"};
	{struct, []} -> %% assuming call authed via ACL, meaning carrier IP was known, hence an inbound call
	    inbound_handler([{<<"Direction">>, <<"inbound">>} | ApiProp]);
	{struct, CCVs} ->
	    case get_value(<<"Direction">>, CCVs) of
		<<"outbound">>=D ->
		    outbound_handler([{<<"Direction">>, D} | ApiProp]);
		<<"inbound">>=D ->
		    inbound_handler([{<<"Direction">>, D} | ApiProp])
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(inbound_handler/1 :: (ApiProp :: list()) -> tuple(ok, iolist()) | tuple(error, string())).
inbound_handler(ApiProp) ->
    format_log(info, "TS_ROUTE(~p): Inbound handler starting...~n", [self()]),
    [ToUser, _ToDomain] = binary:split(get_value(<<"To">>, ApiProp), <<"@">>),
    Did = whistle_util:to_e164(ToUser),
    Flags = create_flags(Did, ApiProp),
    case Flags#route_flags.account_doc_id of
	<<>> -> response(404, ApiProp, Flags);
	_ -> process_routing(inbound_features(Flags), ApiProp)
    end.

-spec(outbound_handler/1 :: (ApiProp :: list()) -> tuple(ok, iolist()) | tuple(error, string())).
outbound_handler(ApiProp) ->
    format_log(info, "TS_ROUTE(~p): Outbound handler starting...~n", [self()]),
    Did = whistle_util:to_e164(get_value(<<"Caller-ID-Number">>, ApiProp, <<>>)),
    Flags = create_flags(Did, ApiProp),
    process_routing(outbound_features(Flags), ApiProp).

-spec(lookup_users_account/2 :: (Name :: binary(), Realm :: binary()) -> tuple(ok, proplist()) | tuple(error, string())).
lookup_users_account(Name, Realm) ->
    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_USERAUTHREALM, [{<<"key">>, [Realm, Name]}]) of
	{error, _}=E -> E;
	{ok, []} -> {error, "No user/realm found"};
	{ok, [{struct, User}|_]} ->
	    %{struct, Auth} = props:get_value(<<"value">>, User),
	    %{ok, Auth}
	    couch_mgr:open_doc(?TS_DB, props:get_value(<<"id">>, User))
    end.

-spec(lookup_did/1 :: (Did :: binary()) -> tuple(ok, proplist()) | tuple(error, string())).
lookup_did(Did) ->
    Options = [{"keys", [Did]}],
    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_DIDLOOKUP, Options) of
	{error, _} ->
	    format_log(error, "TS_ROUTE(~p): No ~p view found while looking up ~p~n"
		       ,[self(), ?TS_VIEW_DIDLOOKUP, Did]),
	    {error, "No DIDLOOKUP view"};
	{ok, []} ->
	    format_log(info, "TS_ROUTE(~p): No DID(s) matching ~p~n", [self(), Options]),
	    {error, "No matching DID"};
	{ok, [{struct, ViewProp} | _Rest]} ->
	    OurDid = get_value(<<"key">>, ViewProp),
	    format_log(info, "TS_ROUTE(~p): DID doc found for ~p~n", [self(), OurDid]),
	    {struct, Value} = get_value(<<"value">>, ViewProp),
	    {ok, [{<<"id">>, get_value(<<"id">>, ViewProp)} | Value]};
	_Else ->
	    format_log(error, "TS_ROUTE(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    {error, "Unexpected error in outbound_handler"}
    end.

-spec(process_routing/2 :: (Flags :: #route_flags{}, ApiProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
process_routing(Flags, ApiProp) ->
    case ts_credit:check(Flags) of
	{ok, Flags1} ->
	    %% call may proceed
	    find_route(Flags1, ApiProp);
	{error, entry_exists} ->
	    format_log(error, "TS_ROUTE(~p): Call-ID ~p has a trunk reserved already, aborting~n", [self(), Flags#route_flags.callid]),
	    {error, "Call-ID exists"};
	{error, no_route_found} ->
	    format_log(error, "TS_ROUTE(~p): No rating information found to handle routing to ~s~n", [self(), Flags#route_flags.to_user]),
	    {error, "No rating information found"};
	{error, no_funds} ->
	    format_log(error, "TS_ROUTE(~p): No funds/flat rate trunks to route call~p~n", [self()]),
	    response(503, ApiProp, Flags)
    end.

-spec(find_route/2 :: (Flags :: #route_flags{}, ApiProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
find_route(Flags, ApiProp) ->
    case Flags#route_flags.direction =:= <<"outbound">> of
	false ->
	    %% handle inbound routing
	    case inbound_route(Flags) of
		{ok, Routes} ->
		    response(Routes, ApiProp, Flags#route_flags{routes_generated=Routes});
		{error, Error} ->
		    format_log(error, "TS_ROUTE(~p): Inbound Routing Error ~p~n", [self(), Error]),
		    response(404, ApiProp, Flags)
	    end;
	true ->
	    [ToUser, _ToDomain] = binary:split(get_value(<<"To">>, ApiProp), <<"@">>),
	    Did = whistle_util:to_e164(ToUser),
	    case lookup_did(Did) of
		{error, _} ->
		    route_over_carriers(Flags, ApiProp);
		{ok, DidProp} ->
		    FlagsIn = flags_from_did(DidProp, Flags#route_flags{to_user=Did}),
		    case inbound_route(FlagsIn) of
			{ok, Routes} ->
			    response(Routes, ApiProp, FlagsIn#route_flags{routes_generated=Routes});
			{error, _} ->
			    route_over_carriers(Flags, ApiProp)
		    end
	    end
    end.

-spec(route_over_carriers/2 :: (Flags :: #route_flags{}, ApiProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
route_over_carriers(Flags, ApiProp) ->
    case ts_carrier:route(Flags) of
	{ok, Routes} ->
	    response(Routes, ApiProp, Flags#route_flags{routes_generated=Routes});
	{error, Error} ->
	    format_log(error, "TS_ROUTE(~p): Outbound Routing Error ~p~n", [self(), Error]),
	    {error, "We don't handle this route"}
    end.

-spec(inbound_route/1 :: (Flags :: #route_flags{}) -> tuple(ok, proplist()) | tuple(error, string())).
inbound_route(Flags) ->
    Invite = case Flags#route_flags.inbound_format of
		 <<"E.164">> ->
		     [{<<"Invite-Format">>, <<"e164">>}
		      ,{<<"To-User">>, Flags#route_flags.auth_user}
		      ,{<<"To-Realm">>, Flags#route_flags.auth_realm}
		      ,{<<"To-DID">>, whistle_util:to_e164(Flags#route_flags.to_user)}
		     ];
		 <<"1NPANXXXXXX">> ->
		     [{<<"Invite-Format">>, <<"1npan">>}
		      ,{<<"To-User">>, Flags#route_flags.auth_user}
		      ,{<<"To-Realm">>, Flags#route_flags.auth_realm}
		      ,{<<"To-DID">>, whistle_util:to_1npanxxxxxx(Flags#route_flags.to_user)}
		     ];
		 <<"NPANXXXXXX">> ->
		     [{<<"Invite-Format">>, <<"npan">>}
		      ,{<<"To-User">>, Flags#route_flags.auth_user}
		      ,{<<"To-Realm">>, Flags#route_flags.auth_realm}
		      ,{<<"To-DID">>, whistle_util:to_npanxxxxxx(Flags#route_flags.to_user)}
		     ];
		 _ ->
		     [{<<"Invite-Format">>, <<"username">>}
		      ,{<<"To-User">>, Flags#route_flags.auth_user}
		      ,{<<"To-Realm">>, Flags#route_flags.auth_realm}
		     ]
	     end,
    Route = [{<<"Weight-Cost">>, <<"1">>}
	     ,{<<"Weight-Location">>, <<"1">>}
	     ,{<<"Media">>, <<"bypass">>}
	     | Invite
	    ],
    case whistle_api:route_resp_route_v(Route) of
	true -> {ok, add_failover_route(Flags#route_flags.failover, Flags, {struct, Route})};
	false ->
	    format_log(error, "TS_ROUTE(~p): Failed to validate Route ~p~n", [self(), Route]),
	    {error, "Inbound route validation failed"}
    end.

-spec(add_failover_route/3 :: (tuple() | tuple(binary(), binary()), Flags :: #route_flags{}, tuple(struct, proplist())) -> proplist()).
add_failover_route({}, _Flags, InboundRoute) -> [InboundRoute];
%% route to a SIP URI
add_failover_route({<<"sip">>, URI}, _Flags, InboundRoute) ->
    [InboundRoute, {struct, [{<<"Route">>, URI}
			     ,{<<"Invite-Format">>, <<"route">>}
			     ,{<<"Weight-Cost">>, <<"1">>}
			     ,{<<"Weight-Location">>, <<"1">>}
			     ,{<<"Media">>, <<"bypass">>}
			    ]}];
%% route to a E.164 number - need to setup outbound for this sucker
add_failover_route({<<"e164">>, DID}, Flags, InboundRoute) ->
    OutBFlags = Flags#route_flags{to_user=DID
				  ,direction = <<"outbound">>
				 },
    case ts_credit:check(OutBFlags) of
	{ok, OutBFlags1} ->
	    case ts_carrier:route(OutBFlags1) of
		{ok, Routes} ->
		    %%format_log(info, "TS_ROUTE(~p): Generated Outbound Routes For Failover~n~p~n", [self(), Routes]),
		    [InboundRoute | Routes];
		{error, Error} ->
		    format_log(error, "TS_ROUTE(~p): Outbound Routing Error For Failover ~p~n", [self(), Error]),
		    [InboundRoute]
	    end;
	{error, Error} ->
	    format_log(error, "TS_ROUTE(~p): Failed to secure credit for failover DID(~p): ~p~n", [self(), DID, Error]),
	    [InboundRoute]
    end.

-spec(inbound_features/1 :: (Flags :: #route_flags{}) -> #route_flags{}).
inbound_features(Flags) ->
    Features = [],
    fold_features(Features, Flags).

-spec(outbound_features/1 :: (Flags :: #route_flags{}) -> #route_flags{}).
outbound_features(Flags) ->
    Features = [ts_e911],
    fold_features(Features, Flags).

-spec(fold_features/2 :: (Features :: list(atom()), Flags :: #route_flags{}) -> #route_flags{}).
fold_features(Features, Flags) ->
    lists:foldl(fun(Mod, Flags0) ->
			Mod:process_flags(Flags0)
		end, Flags, Features).

-spec(create_flags/2 :: (Did :: binary(), ApiProp :: proplist()) -> #route_flags{}).
create_flags(Did, ApiProp) ->
    {struct, ChannelVars} = get_value(<<"Custom-Channel-Vars">>, ApiProp, {struct, []}),

    F1 = case lookup_did(Did) of
	     {ok, DidProp} ->
		 flags_from_did(DidProp, #route_flags{});
	     {error, _E} ->
		 #route_flags{auth_user=get_value(<<"Username">>, ChannelVars, <<>>), auth_realm=get_value(<<"Realm">>, ChannelVars, <<>>)}
	 end,

    AuthUser = F1#route_flags.auth_user,
    Realm = F1#route_flags.auth_realm,

    F3 = case lookup_users_account(AuthUser, Realm) of
	     {error, _E1} ->
		 format_log(error, "TS_ROUTE(~p).create_flags: Failed to lookup doc for ~p@~p: ~p", [self(), AuthUser, Realm, _E1]),
		 case couch_mgr:open_doc(?TS_DB, F1#route_flags.account_doc_id) of
		     {error, _E2} -> F1;
		     {ok, {struct, Doc1}} ->
			 F2 = flags_from_srv(AuthUser, Doc1, F1),
			 flags_from_account(Doc1, F2)
		 end;
	     {ok, {struct, D}} ->
		 F2 = flags_from_srv(AuthUser, D, F1),
		 flags_from_account(D, F2)
	 end,
    flags_from_api(ApiProp, ChannelVars, F3).

-spec(flags_from_api/3 :: (ApiProp :: proplist(), ChannelVars :: proplist(), Flags :: #route_flags{}) -> #route_flags{}).
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
      callid = get_value(<<"Call-ID">>, ApiProp)
      ,to_user = whistle_util:to_e164(ToUser)
      ,to_domain = ToDomain
      ,from_user = whistle_util:to_e164(FromUser)
      ,from_domain = FromDomain
      ,auth_user = get_value(<<"Auth-User">>, ChannelVars, Flags#route_flags.auth_user)
      ,direction = get_value(<<"Direction">>, ChannelVars, <<"inbound">>)
     }.

%% Flags from the DID
%% - Failover
%% - Caller ID
%% - Auth User
%% - Auth Realm
-spec(flags_from_did/2 :: (DidProp :: proplist(), Flags :: #route_flags{}) -> #route_flags{}).
flags_from_did(DidProp, Flags) ->
    {struct, DidOptions} = get_value(<<"DID_Opts">>, DidProp, {struct, []}),
    {struct, AuthOpts} = get_value(<<"auth">>, DidProp, {struct, []}),

    {struct, Opts} = get_value(<<"options">>, DidProp, {struct, []}),

    F0 = add_failover(Flags, get_value(<<"failover">>, DidOptions, {struct, []})),
    F1 = add_caller_id(F0, get_value(<<"caller_id">>, DidOptions, {struct, []})),
    F1#route_flags{route_options = Opts
		   ,auth_user = get_value(<<"auth_user">>, AuthOpts, <<>>)
		   ,account_doc_id = get_value(<<"id">>, DidProp)
		  }.

%% Flags from the Server
%% - Inbound Format <- what format does the server expect the inbound caller-id in?
%% - Codecs <- list of codecs supported by the server
%% - Caller Id <- only if it hasn't been set on the DID level
%% - Failover <- only if it hasn't been set on the DID level
%% - Trunks <- Max trunks allowed on the server
%% - Auth Realm <- just in case it wasn't set from the DID
%% - 
-spec(flags_from_srv/3 :: ( AuthUser :: binary(), Doc :: proplist(), Flags :: #route_flags{}) -> #route_flags{}).
flags_from_srv(AuthUser, Doc, Flags) ->
    Srv = lookup_server(AuthUser, Doc),

    {struct, Options} = get_value(<<"options">>, Srv, {struct, []}),

    F0 = Flags#route_flags{inbound_format=get_value(<<"inbound_format">>, Options, <<>>)
			   ,codecs=get_value(<<"codecs">>, Srv, [])
			   ,account_doc_id = get_value(<<"_id">>, Doc, Flags#route_flags.account_doc_id)
			  },
    F1 = add_caller_id(F0, get_value(<<"caller_id">>, Srv, {struct, []})),
    add_failover(F1, get_value(<<"failover">>, Srv, {struct, []})).

%% Flags from the Account
%% - Credit available
%% - Trunks purchased <- eventually need to look at the server under the account to see how many are allocatable to the server
%% - Trunks in use
%% - Caller ID <- only if it hasn't been set at the server or DID level
%% - Failover <- only if it hasn't been set at the server or DID level
-spec(flags_from_account(Doc :: proplist(), Flags :: #route_flags{}) -> #route_flags{}).
flags_from_account(Doc, Flags) ->
    {struct, Acct} = get_value(<<"account">>, Doc, {struct, []}),

    F1 = add_caller_id(Flags#route_flags{account_doc_id = get_value(<<"_id">>, Doc, Flags#route_flags.account_doc_id)}
		       ,get_value(<<"caller_id">>, Acct, {struct, []})),
    F2 = add_failover(F1, get_value(<<"failover">>, Acct, {struct, []})),
    F2#route_flags{auth_realm = get_value(<<"auth_realm">>, Acct, Flags#route_flags.auth_realm)}.

-spec(add_failover/2 :: (F0 :: #route_flags{}, FOver :: tuple(proplist())) -> #route_flags{}).
add_failover(#route_flags{failover={}}=F0, {struct, []}) -> F0;
add_failover(#route_flags{failover={}}=F0, {struct, [{_K, _V}=FOver]}) ->
    F0#route_flags{failover=FOver};
add_failover(F, _) -> F.

-spec(add_caller_id/2 :: (F0 :: #route_flags{}, CID :: tuple(proplist())) -> #route_flags{}).
add_caller_id(#route_flags{caller_id={}}=F0, {struct, []}) -> F0;
add_caller_id(#route_flags{caller_id={}}=F0, {struct, CID}) ->
    F0#route_flags{caller_id = {get_value(<<"cid_name">>, CID, <<>>)
				,get_value(<<"cid_number">>, CID, <<>>)}};
add_caller_id(F, _) -> F.

-spec(lookup_server/2 :: (AuthUser :: binary(), Doc :: proplist()) -> proplist()).
lookup_server(AuthUser, Doc) ->
    case lists:filter(fun({struct, S}) ->
			      {struct, Auth} = get_value(<<"auth">>, S, {struct, []}),
			      get_value(<<"auth_user">>, Auth, <<>>) =:= AuthUser
		      end, get_value(<<"servers">>, Doc, [])) of
	[{struct, Srv}] -> Srv;
	_ -> []
    end.

-spec(response/3 :: (Routes :: proplist() | integer(), Prop :: proplist(), Flags :: #route_flags{}) -> tuple(ok, iolist()) | tuple(error, string())).
response(Routes, Prop, Flags) ->
    ServerID = case is_list(Routes) of
		   true ->
		       {ok, Pid} = ts_call_sup:start_proc([Flags#route_flags.callid, whapps_controller:get_amqp_host(), Flags]),
		       {ok, Q} = ts_call_handler:get_queue(Pid),
		       Q;
		   false -> <<>>
	       end,

    Prop1 = [ {<<"Msg-ID">>, get_value(<<"Msg-ID">>, Prop)}
	      | whistle_api:default_headers(ServerID, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION) ],
    Data = specific_response(Routes, Prop1),
    case whistle_api:route_resp(Data) of
	{ok, JSON} ->
	    {ok, JSON};
	{error, _E}=Error ->
	    Error
    end.

-spec(specific_response/2 :: (CodeOrRoutes :: integer() | proplist(), Prop :: proplist()) -> proplist()).
specific_response(404, Prop) ->
    [{<<"Routes">>, []}
     ,{<<"Method">>, <<"error">>}
     ,{<<"Route-Error-Code">>, <<"404">>}
     ,{<<"Route-Error-Message">>, <<"Not Found">>}
     | Prop
    ];
specific_response(503, Prop) ->
    [{<<"Routes">>, []}
     ,{<<"Method">>, <<"error">>}
     ,{<<"Route-Error-Code">>, <<"503">>}
     ,{<<"Route-Error-Message">>, <<"Insufficient Credit">>}
     | Prop
    ];
specific_response(Routes, Prop) when is_list(Routes) ->
    [{<<"Routes">>, Routes}
     ,{<<"Method">>, <<"bridge">>}
     | Prop
    ].
