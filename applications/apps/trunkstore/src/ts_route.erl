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
-import(logger, [format_log/3]).

-include("ts.hrl").

-define(APP_NAME, <<"ts_responder.route">>).
-define(APP_VERSION, <<"0.5.3">>).

%%%===================================================================
%%% API
%%%===================================================================
-spec(handle_req/1 :: (ApiProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
handle_req(ApiProp) ->
    %% format_log(info, "TS_ROUTE(~p): Handling Route Request~n", [self()]),
    wh_timer:start("ts_route"),
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
    wh_timer:tick("inbound_handler/1"),
    %% format_log(info, "TS_ROUTE(~p): Inbound handler starting...~n", [self()]),
    [ToUser, _ToDomain] = binary:split(get_value(<<"To">>, ApiProp), <<"@">>),
    Flags = create_flags(whistle_util:to_e164(ToUser), ApiProp),
    case Flags#route_flags.account_doc_id of
	<<>> -> response(404, ApiProp, Flags);
	_ -> process_routing(inbound_features(Flags), ApiProp)
    end.

-spec(outbound_handler/1 :: (ApiProp :: list()) -> tuple(ok, iolist()) | tuple(error, string())).
outbound_handler(ApiProp) ->
    wh_timer:tick("outbound_handler/1"),
    %% format_log(info, "TS_ROUTE(~p): Outbound handler starting...~n", [self()]),
    Did = whistle_util:to_e164(get_value(<<"Caller-ID-Number">>, ApiProp, <<>>)),
    Flags = create_flags(Did, ApiProp),
    %% format_log(info, "TS_ROUTE(~p): Flags acctid: ~p~n", [self(), Flags#route_flags.account_doc_id]),
    process_routing(outbound_features(Flags), ApiProp).

lookup_user_flags(Name, Realm) ->
    wh_timer:tick("lookup_user_flags/2"),
    case wh_cache:fetch({lookup_user_flags, Realm, Name}) of
	{ok, _}=Result -> Result;
	{error, not_found} ->
	    case couch_mgr:get_results(?TS_DB, "LookUpUser/LookUpUserFlags", [{<<"key">>, [Realm, Name]}]) of
		{error, _}=E -> E;
		{ok, []} -> {error, "No user@realm found"};
		{ok, [User|_]} ->
		    {struct, Prop} = whapps_json:get_value(<<"value">>, User),
		    JObj = {struct, [{<<"id">>, whapps_json:get_value(<<"id">>, User)} | Prop]},
		    wh_cache:store({lookup_user_flags, Realm, Name}, JObj),
		    {ok, JObj}
	    end
    end.

-spec(lookup_did/1 :: (Did :: binary()) -> tuple(ok, proplist()) | tuple(error, string())).
lookup_did(Did) ->
    wh_timer:tick("lookup_did/1"),
    Options = [{"keys", [Did]}],
    case wh_cache:fetch({lookup_did, Did}) of
	{ok, _}=Resp -> wh_timer:tick("lookup_did/1 cache hit"), Resp;
	{error, not_found} ->
	    wh_timer:tick("lookup_did/1 cache miss"),
	    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_DIDLOOKUP, Options) of
		{error, _} ->
		    %% format_log(error, "TS_ROUTE(~p): No ~p view found while looking up ~p~n", [self(), ?TS_VIEW_DIDLOOKUP, Did]),
		    {error, "No DIDLOOKUP view"};
		{ok, []} ->
		    %% format_log(info, "TS_ROUTE(~p): No DID(s) matching ~p~n", [self(), Options]),
		    {error, "No matching DID"};
		{ok, [{struct, ViewProp} | _Rest]} ->
		    {struct, Value} = get_value(<<"value">>, ViewProp),
		    Resp = [{<<"id">>, get_value(<<"id">>, ViewProp)} | Value],
		    wh_cache:store({lookup_did, Did}, Resp),
		    {ok, Resp};
		_Else ->
		    %% format_log(error, "TS_ROUTE(~p): Got something unexpected~n~p~n", [self(), _Else]),
		    {error, "Unexpected error in outbound_handler"}
	    end
    end.

-spec(process_routing/2 :: (Flags :: #route_flags{}, ApiProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
process_routing(Flags, ApiProp) ->
    wh_timer:tick("process_routing/2"),
    case ts_credit:check(Flags) of
	{ok, Flags1} ->
	    %% call may proceed
	    wh_timer:tick("process_routing post credit"),
	    find_route(Flags1, ApiProp);
	{error, entry_exists} ->
	    %% format_log(error, "TS_ROUTE(~p): Call-ID ~p has a trunk reserved already, aborting~n", [self(), Flags#route_flags.callid]),
	    {error, "Call-ID exists"};
	{error, no_route_found} ->
	    %% format_log(error, "TS_ROUTE(~p): No rating information found to handle routing to ~s~n", [self(), Flags#route_flags.to_user]),
	    {error, "No rating information found"};
	{error, no_funds} ->
	    %% format_log(error, "TS_ROUTE(~p): No funds/flat rate trunks to route call~n", [self()]),
	    response(503, ApiProp, Flags)
    end.
	

-spec(find_route/2 :: (Flags :: #route_flags{}, ApiProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
find_route(Flags, ApiProp) ->
    wh_timer:tick("find_route/2"),
    case Flags#route_flags.direction =:= <<"outbound">> of
	false ->
	    %% handle inbound routing
	    case inbound_route(Flags) of
		{ok, Routes, InboundFlags} ->
		    response(Routes, ApiProp, InboundFlags#route_flags{routes_generated=Routes});
		{error, _Error}=E ->
		    %% format_log(error, "TS_ROUTE(~p): Inbound Routing Error ~p~n", [self(), _Error]),
		    E
	    end;
	true ->
	    find_outbound_route(Flags, ApiProp)
    end.

-spec(find_outbound_route/2 :: (Flags :: #route_flags{}, ApiProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
find_outbound_route(Flags, ApiProp) ->
    wh_timer:tick("find_outbound_route/2"),
    try
	[ToUser, _ToDomain] = binary:split(get_value(<<"To">>, ApiProp), <<"@">>),
	Did = whistle_util:to_e164(ToUser),

	case lookup_did(Did) of
	    {error, _} -> % if lookup_did(Did) failed
		route_over_carriers(Flags#route_flags{scenario=outbound}, ApiProp);
	    {ok, DidProp} -> % out-in scenario
		OrigAcctId = Flags#route_flags.account_doc_id,
		FlagsIn0 = create_flags(Did, ApiProp, DidProp),
		FlagsIn1 = FlagsIn0#route_flags{direction = <<"inbound">>},

		wh_timer:tick("pre force_outbound check"),
		case (not FlagsIn1#route_flags.force_outbound) andalso ts_credit:check(FlagsIn1) of
		    false -> % if force_outbound == true
			wh_timer:tick("post force_outbound check - false"),
			route_over_carriers(Flags#route_flags{scenario=outbound}, ApiProp);
		    {ok, FlagsIn} ->
			wh_timer:tick("post force_outbound check - ok, flags"),
			%% we'll do the actual trunk reservation on CHANNEL_BRIDGE in ts_call_handler
			%% format_log(info, "TS_ROUTE(~p): Rerouting ~p back to known user ~s@~s~n", [self(), Did, FlagsIn#route_flags.auth_user, FlagsIn#route_flags.auth_realm]),
			case inbound_route(FlagsIn) of
			    {ok, Routes, FlagsIn2} ->
				wh_timer:tick("found inbound route to route over instead"),
				case FlagsIn1#route_flags.scenario of
				    inbound ->
					response(Routes, ApiProp, FlagsIn2#route_flags{routes_generated=Routes
										       ,account_doc_id=OrigAcctId
										       ,diverted_account_doc_id=FlagsIn#route_flags.account_doc_id
										       ,scenario=outbound_inbound
										      });
				    inbound_failover ->
					response(Routes, ApiProp, FlagsIn2#route_flags{routes_generated=Routes
										       ,account_doc_id=OrigAcctId
										       ,diverted_account_doc_id=FlagsIn#route_flags.account_doc_id
										       ,scenario=outbound_inbound_failover
										      })
				end;
			    {error, _} ->
				wh_timer:tick("routing over carrier anyway"),
				route_over_carriers(Flags#route_flags{scenario=outbound}, ApiProp)
			end;

		    %% someone on the account is calling someone else on the same account; don't allocate a trunk
		    {error, entry_exists} ->
			case inbound_route(FlagsIn1) of
			    {ok, Routes, _} ->
				response(Routes, ApiProp, Flags#route_flags{direction = <<"inbound">>});
			    {error, _} ->
				route_over_carriers(Flags#route_flags{scenario=outbound}, ApiProp)
			end;

		    {error, _}  ->
			%% format_log(error, "TS_ROUTE(~p): Unable to route back to ~p, no credits or flat rate trunks.~n", [self(), FlagsIn1#route_flags.account_doc_id]),
			ts_acctmgr:release_trunk(FlagsIn1#route_flags.account_doc_id, FlagsIn1#route_flags.callid),
			response(503, ApiProp, Flags)
		end
	end
    catch
	_A:_B ->
	    %% format_log(error, "TS_ROUTE(~p): Exception when going outbound: ~p: ~p~n", [self(), _A, _B]),
	    %% format_log(error, "TS_ROUTE(~p): Stacktrace: ~p~n", [self(), erlang:get_stacktrace()]),
	    ts_acctmgr:release_trunk(Flags#route_flags.account_doc_id, Flags#route_flags.callid),
	    response(503, ApiProp, Flags)
    end.

-spec(route_over_carriers/2 :: (Flags :: #route_flags{}, ApiProp :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
route_over_carriers(Flags, ApiProp) ->
    wh_timer:tick("route_over_carriers/2"),
    case ts_carrier:route(Flags) of
	{ok, Routes} ->
	    response(Routes, ApiProp, Flags#route_flags{routes_generated=Routes});
	{error, _Error} ->
	    %% format_log(error, "TS_ROUTE(~p): Outbound Routing Error ~p~n", [self(), _Error]),
	    {error, "We don't handle this route"}
    end.

-spec(inbound_route/1 :: (Flags :: #route_flags{}) -> tuple(ok, proplist(), #route_flags{}) | tuple(error, string())).
inbound_route(Flags) ->
    wh_timer:tick("inbound_route/1"),
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
	     ,{<<"Custom-Channel-Vars">>, {struct, [
						    {<<"Auth-User">>, Flags#route_flags.auth_user}
						    ,{<<"Auth-Realm">>, Flags#route_flags.auth_realm}
						    ,{<<"Direction">>, <<"inbound">>}
						    | ts_util:get_base_channel_vars(Flags)
						   ]}
	      }
	     ,{<<"Media">>, ts_util:get_media_handling(Flags#route_flags.media_handling)}
	     | Invite ],

    Route1 = case Flags#route_flags.progress_timeout of
		 none -> Route;
		 Secs -> [{<<"Progress-Timeout">>, whistle_util:to_integer(Secs)} | Route]
	     end,

    case whistle_api:route_resp_route_v(Route1) of
	true ->
	    {Routes, Flags1} = add_failover_route(Flags#route_flags.failover, Flags, {struct, Route1}),
	    {ok, Routes, Flags1};
	false ->
	    %% format_log(error, "TS_ROUTE(~p): Failed to validate Route ~p~n", [self(), Route1]),
	    {error, "Inbound route validation failed"}
    end.

-spec(add_failover_route/3 :: (tuple() | tuple(binary(), binary()), Flags :: #route_flags{}, tuple(struct, proplist())) -> tuple(proplist(), #route_flags{})).
add_failover_route({}, Flags, InboundRoute) -> {[InboundRoute], Flags#route_flags{scenario=inbound}};
%% route to a SIP URI
add_failover_route({<<"sip">>, URI}, Flags, InboundRoute) ->
    { [InboundRoute, {struct, [{<<"Route">>, URI}
			       ,{<<"Invite-Format">>, <<"route">>}
			       ,{<<"Weight-Cost">>, <<"1">>}
			       ,{<<"Weight-Location">>, <<"1">>}
			       ,{<<"Failover-Route">>, <<"true">>}
			       ,{<<"Media">>, ts_util:get_media_handling(Flags#route_flags.media_handling)}
			      ]}]
      ,Flags#route_flags{scenario=inbound_failover}};
%% route to a E.164 number - need to setup outbound for this sucker
add_failover_route({<<"e164">>, DID}, #route_flags{callid=CallID}=Flags, InboundRoute) ->
    OutBFlags = Flags#route_flags{to_user=DID
				  ,callid = <<CallID/binary, "-failover">>
				  ,direction = <<"outbound">>
				 },
    case ts_credit:check(OutBFlags) of
	{ok, OutBFlags1} ->
	    case ts_carrier:route(OutBFlags1) of
		{ok, Routes} ->
		    %% format_log(info, "TS_ROUTE(~p): Generated Outbound Routes For Failover~n~p~n", [self(), Routes]),
		    { [InboundRoute | Routes], Flags#route_flags{scenario=inbound_failover}};
		{error, _Error} ->
		    %% format_log(error, "TS_ROUTE(~p): Outbound Routing Error For Failover ~p~n", [self(), _Error]),
		    ts_acctmgr:release_trunk(OutBFlags1#route_flags.account_doc_id, OutBFlags1#route_flags.callid, 0),
		    { [InboundRoute], Flags#route_flags{scenario=inbound}}
	    end;
	{error, _Error} ->
	    %% format_log(error, "TS_ROUTE(~p): Failed to secure credit for failover DID(~p): ~p~n", [self(), DID, _Error]),
	    {[InboundRoute], Flags#route_flags{scenario=inbound}}
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
    wh_timer:tick("create_flags/2"),
    case lookup_did(Did) of
	{ok, DidProp} ->
	    create_flags(Did, ApiProp, DidProp);
	{error, _E} ->
	    create_flags(Did, ApiProp, [])
    end.

create_flags(_, ApiProp, DidProp) ->
    {struct, ChannelVars} = get_value(<<"Custom-Channel-Vars">>, ApiProp, {struct, []}),

    F1 = case DidProp of
	     [] -> add_auth_user(add_auth_realm(#route_flags{}, get_value(<<"Realm">>, ChannelVars)), get_value(<<"Username">>, ChannelVars));
	     _ -> add_auth_realm(flags_from_did(DidProp, #route_flags{}), get_value(<<"Realm">>, ChannelVars))
	 end,

    AuthUser = F1#route_flags.auth_user,
    Realm = F1#route_flags.auth_realm,

    F3 = case lookup_user_flags(AuthUser, Realm) of
	     {ok, D} ->
		 Id = whapps_json:get_value(<<"id">>, D),
		 F2 = flags_from_srv(whapps_json:get_value(<<"server">>, D, {struct, []}), F1#route_flags{account_doc_id = Id}),
		 flags_from_account(whapps_json:get_value(<<"account">>, D, {struct, []}), F2)
	 end,
    flags_from_api(ApiProp, ChannelVars, F3).

-spec(flags_from_api/3 :: (ApiProp :: proplist(), ChannelVars :: proplist(), Flags :: #route_flags{}) -> #route_flags{}).
flags_from_api(ApiProp, ChannelVars, Flags) ->
    wh_timer:tick("flags_from_api/3"),
    [ToUser, ToDomain] = binary:split(get_value(<<"To">>, ApiProp), <<"@">>),
    [FromUser, FromDomain] = binary:split(get_value(<<"From">>, ApiProp), <<"@">>),

    F0 = case Flags#route_flags.caller_id of
	     {} -> Flags#route_flags{caller_id = {get_value(<<"Caller-ID-Name">>, ApiProp, <<>>)
						  ,get_value(<<"Caller-ID-Number">>, ApiProp, <<>>)
						 }};
	     _CID -> Flags
	 end,
    F1 = F0#route_flags{
	   callid = get_value(<<"Call-ID">>, ApiProp)
	   ,to_user = whistle_util:to_e164(ToUser)
	   ,to_domain = ToDomain
	   ,from_user = whistle_util:to_e164(FromUser)
	   ,from_domain = FromDomain
	   ,direction = get_value(<<"Direction">>, ChannelVars, <<"inbound">>)
	  },
    add_auth_user(F1, get_value(<<"Auth-User">>, ChannelVars)).

%% Flags from the DID
%% - Failover
%% - Caller ID
%% - Auth User
%% - Auth Realm
-spec(flags_from_did/2 :: (DidProp :: proplist(), Flags :: #route_flags{}) -> #route_flags{}).
flags_from_did(DidProp, Flags) ->
    wh_timer:tick("flags_from_did/2"),
    {struct, DidOptions} = get_value(<<"DID_Opts">>, DidProp, {struct, []}),
    {struct, AuthOpts} = get_value(<<"auth">>, DidProp, {struct, []}),

    {struct, Opts} = get_value(<<"options">>, DidProp, {struct, []}),
    {struct, Acct} = get_value(<<"account">>, DidProp, {struct, []}),

    F0 = add_failover(Flags, get_value(<<"failover">>, DidOptions, {struct, []})),
    F1 = add_caller_id(F0, get_value(<<"caller_id">>, DidOptions, {struct, []})),
    F2 = F1#route_flags{route_options = Opts
			,account_doc_id = get_value(<<"id">>, DidProp)
		       },
    F3 = add_auth_user(F2, get_value(<<"auth_user">>, AuthOpts)),
    F4 = add_auth_realm(F3, get_value(<<"auth_realm">>, AuthOpts, get_value(<<"auth_realm">>, Acct))),
    add_force_outbound(F4, get_value(<<"force_outbound">>, DidOptions, false)).

%% Flags from the Server
%% - Inbound Format <- what format does the server expect the inbound caller-id in?
%% - Codecs <- list of codecs supported by the server
%% - Caller Id <- only if it hasn't been set on the DID level
%% - Failover <- only if it hasn't been set on the DID level
%% - Trunks <- Max trunks allowed on the server
%% - Auth Realm <- just in case it wasn't set from the DID
%% - 
-spec(flags_from_srv/2 :: (Srv :: json_object(), Flags :: #route_flags{}) -> #route_flags{}).
flags_from_srv(Srv, Flags) ->
    wh_timer:tick("flags_from_srv/2"),
    {struct, Options} = whapps_json:get_value(<<"options">>, Srv, {struct, []}),

    F0 = Flags#route_flags{inbound_format = whapps_json:get_value(<<"inbound_format">>, Options, <<>>)
			   ,codecs = whapps_json:get_value(<<"codecs">>, Srv, [])
			   ,media_handling = whapps_json:get_value(<<"media_handling">>, Options)
			   ,progress_timeout = whapps_json:get_value(<<"progress_timeout">>, Options, none)
			  },
    F1 = add_caller_id(F0, get_value(<<"caller_id">>, Srv, {struct, []})),
    F2 = add_failover(F1, get_value(<<"failover">>, Srv, {struct, []})),
    add_force_outbound(F2, get_value(<<"force_outbound">>, Options, false)).

%% Flags from the Account
%% - Credit available
%% - Trunks purchased <- eventually need to look at the server under the account to see how many are allocatable to the server
%% - Trunks in use
%% - Caller ID <- only if it hasn't been set at the server or DID level
%% - Failover <- only if it hasn't been set at the server or DID level
-spec(flags_from_account(Doc :: proplist(), Flags :: #route_flags{}) -> #route_flags{}).
flags_from_account(Acct, Flags) ->
    wh_timer:tick("flags_from_acct/2"),
    F1 = add_caller_id(Flags, whapps_json:get_value(<<"caller_id">>, Acct, {struct, []})),
    F2 = add_failover(F1, whapps_json:get_value(<<"failover">>, Acct, {struct, []})),
    add_auth_realm(F2, whapps_json:get_value(<<"auth_realm">>, Acct)).

-spec(add_force_outbound/2 :: (F :: #route_flags{}, Force :: boolean()) -> #route_flags{}).
add_force_outbound(#route_flags{force_outbound=undefined}=F, Force) ->
    F#route_flags{force_outbound=whistle_util:to_boolean(Force)};
add_force_outbound(F, _) -> F.

-spec(add_failover/2 :: (F0 :: #route_flags{}, FOver :: tuple(proplist())) -> #route_flags{}).
add_failover(#route_flags{failover={}}=F0, {struct, []}) -> F0;
add_failover(#route_flags{failover={}}=F0, {struct, [{_K, _V}=FOver]}) ->
    F0#route_flags{failover=FOver};
add_failover(F, _) -> F.

-spec(add_auth_user/2 :: (F :: #route_flags{}, User :: binary() | undefined) -> #route_flags{}).
add_auth_user(F, <<>>) -> F;
add_auth_user(F, undefined) -> F;
add_auth_user(#route_flags{auth_user = <<>>}=F, User) ->
    F#route_flags{auth_user=User};
add_auth_user(#route_flags{auth_user=undefined}=F, User) ->
    F#route_flags{auth_user=User};
add_auth_user(F, _User) ->
    F.

-spec(add_auth_realm/2 :: (F :: #route_flags{}, Realm :: binary() | undefined) -> #route_flags{}).
add_auth_realm(F, <<>>) -> F;
add_auth_realm(F, undefined) -> F;
add_auth_realm(#route_flags{auth_realm = <<>>}=F, Realm) ->
    F#route_flags{auth_realm=Realm};
add_auth_realm(#route_flags{auth_realm=undefined}=F, Realm) ->
    F#route_flags{auth_realm=Realm};
add_auth_realm(F, _Realm) ->
    F.

-spec(add_caller_id/2 :: (F0 :: #route_flags{}, CID :: tuple(proplist())) -> #route_flags{}).
add_caller_id(#route_flags{caller_id={}}=F0, {struct, []}) -> F0;
add_caller_id(#route_flags{caller_id={}}=F0, {struct, CID}) ->
    F0#route_flags{caller_id = {get_value(<<"cid_name">>, CID, <<>>)
				,get_value(<<"cid_number">>, CID, <<>>)}};
add_caller_id(F, _) -> F.

-spec(response/3 :: (Routes :: proplist() | integer(), Prop :: proplist(), Flags :: #route_flags{}) -> tuple(ok, iolist()) | tuple(error, string())).
response(Routes, Prop, Flags) ->
    wh_timer:tick("response/3"),
    ServerID = case is_list(Routes) of
		   true ->
		       {ok, Pid} = ts_call_sup:start_proc([Flags#route_flags.callid, Flags]),
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
