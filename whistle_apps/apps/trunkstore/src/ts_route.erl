%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Respond to Route requests
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_route).

%% API
-export([handle_req/1]).

-include("ts.hrl").

-define(APP_NAME, <<"ts_responder.route">>).
-define(APP_VERSION, <<"0.7.4">>).

%%%===================================================================
%%% API
%%%===================================================================
-spec(handle_req/1 :: (ApiJObj :: json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
handle_req(ApiJObj) ->
    %% wh_timer:start("ts_route"),
    case wh_json:get_value(<<"Custom-Channel-Vars">>, ApiJObj) of
	undefined ->
	    {error, "No Custom Vars"};
	?EMPTY_JSON_OBJECT -> %% assuming call authed via ACL, meaning carrier IP was known, hence an inbound call
	    inbound_handler(wh_json:set_value([<<"Direction">>], <<"inbound">>, ApiJObj));
	{struct, _}=CCVs ->
	    case wh_json:get_value(<<"Direction">>, CCVs) of
		<<"outbound">>=D ->
		    outbound_handler(wh_json:set_value([<<"Direction">>], D, ApiJObj));
		<<"inbound">>=D ->
		    inbound_handler(wh_json:set_value([<<"Direction">>], D, ApiJObj))
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(inbound_handler/1 :: (ApiJObj :: json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
inbound_handler(ApiJObj) ->
    %% wh_timer:tick("inbound_handler/1"),
    ?LOG("Inbound handler started"),
    [ToUser, _ToDomain] = binary:split(wh_json:get_value(<<"To">>, ApiJObj), <<"@">>),
    Flags = create_flags(whistle_util:to_e164(ToUser), ApiJObj),
    case Flags#route_flags.account_doc_id of
	<<>> -> response(404, ApiJObj, Flags);
	_ -> process_routing(inbound_features(Flags), ApiJObj)
    end.

-spec(outbound_handler/1 :: (ApiJObj :: json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
outbound_handler(ApiJObj) ->
    %% wh_timer:tick("outbound_handler/1"),
    ?LOG("Outbound handler started"),
    Did = whistle_util:to_e164(wh_json:get_value(<<"Caller-ID-Number">>, ApiJObj, <<>>)),
    Flags = create_flags(Did, ApiJObj),
    process_routing(outbound_features(Flags), ApiJObj).

-spec(lookup_user_flags/2 :: (Name :: binary(), Realm :: binary()) -> tuple(ok, json_object()) | tuple(error, term())).
lookup_user_flags(Name, Realm) ->
    %% wh_timer:tick("lookup_user_flags/2"),
    case wh_cache:fetch({lookup_user_flags, Realm, Name}) of
	{ok, _}=Result -> Result;
	{error, not_found} ->
	    case couch_mgr:get_results(?TS_DB, <<"LookUpUser/LookUpUserFlags">>, [{<<"key">>, [Realm, Name]}]) of
		{error, _}=E -> E;
		{ok, []} -> {error, <<"No user@realm found">>};
		{ok, [User|_]} ->
		    ValJObj = wh_json:get_value(<<"value">>, User),
		    JObj = wh_json:set_value(<<"id">>, wh_json:get_value(<<"id">>, User), ValJObj),
		    wh_cache:store({lookup_user_flags, Realm, Name}, JObj),
		    {ok, JObj}
	    end
    end.

-spec(lookup_did/1 :: (DID :: binary()) -> tuple(ok, json_object()) | tuple(error, binary())).
lookup_did(DID) ->
    %% wh_timer:tick("lookup_did/1"),
    Options = [{<<"key">>, DID}],
    case wh_cache:fetch({lookup_did, DID}) of
	{ok, _}=Resp ->
	    %% wh_timer:tick("lookup_did/1 cache hit"),
	    Resp;
	{error, not_found} ->
	    %% wh_timer:tick("lookup_did/1 cache miss"),
	    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_DIDLOOKUP, Options) of
		{error, _} ->
		    {error, <<"No DIDLOOKUP view">>};
		{ok, []} ->
		    {error, <<"No matching DID">>};
		{ok, [{struct, _}=ViewJObj]} ->
		    ValueJObj = wh_json:get_value(<<"value">>, ViewJObj),
		    Resp = wh_json:set_value(<<"id">>, wh_json:get_value(<<"id">>, ViewJObj), ValueJObj),
		    wh_cache:store({lookup_did, DID}, Resp),
		    {ok, Resp};
		{ok, [{struct, _}=ViewJObj | _Rest]} ->
		    ?LOG("Looking up DID ~s resulted in more than one result", [DID]),
		    ValueJObj = wh_json:get_value(<<"value">>, ViewJObj),
		    Resp = wh_json:set_value(<<"id">>, wh_json:get_value(<<"id">>, ViewJObj), ValueJObj),
		    wh_cache:store({lookup_did, DID}, Resp),
		    {ok, Resp}
	    end
    end.

-spec(process_routing/2 :: (Flags :: #route_flags{}, ApiJObj :: json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
process_routing(Flags, ApiJObj) ->
    %% wh_timer:tick("process_routing/2"),
    case ts_credit:check(Flags) of
	{ok, Flags1} ->
	    %% call may proceed
	    %% wh_timer:tick("process_routing post credit"),
	    find_route(Flags1, ApiJObj);
	{error, entry_exists} ->
	    {error, "Call-ID exists"};
	{error, no_route_found} ->
	    {error, "No rating information found"};
	{error, no_funds} ->
	    response(503, ApiJObj, Flags)
    end.

-spec(find_route/2 :: (Flags :: #route_flags{}, ApiJObj :: json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
find_route(Flags, ApiJObj) ->
    %% wh_timer:tick("find_route/2"),
    case Flags#route_flags.direction =:= <<"outbound">> of
	false ->
	    %% handle inbound routing
	    case inbound_route(Flags) of
		{ok, Routes, InboundFlags} ->
		    response(Routes, ApiJObj, InboundFlags#route_flags{routes_generated=Routes});
		{error, _Error}=E ->
		    ?LOG("Inbound routing error: ~p", [_Error]),
		    E
	    end;
	true ->
	    find_outbound_route(Flags, ApiJObj)
    end.

-spec(find_outbound_route/2 :: (Flags :: #route_flags{}, ApiJObj :: json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
find_outbound_route(#route_flags{callid=CallID, account_doc_id=AccountDocId}=Flags, ApiJObj) ->
    %% wh_timer:tick("find_outbound_route/2"),
    try
	[ToUser, _ToDomain] = binary:split(wh_json:get_value(<<"To">>, ApiJObj), <<"@">>),
	DID = whistle_util:to_e164(ToUser),

	case lookup_did(DID) of
	    {error, _} -> % if lookup_did(Did) failed
		?LOG("We don't know ~s, routing over carriers", [DID]),
		route_over_carriers(Flags#route_flags{scenario=outbound}, ApiJObj);
	    {ok, DidJObj} -> % out-in scenario
		OrigAcctId = AccountDocId,
		FlagsIn0 = create_flags(DID, ApiJObj, DidJObj),
		FlagsIn1 = FlagsIn0#route_flags{direction = <<"inbound">>},

		case (not FlagsIn1#route_flags.force_outbound) andalso ts_credit:check(FlagsIn1) of
		    false -> % if force_outbound == true
			?LOG("We know ~s, but force_outbound is true, routing over carriers", [DID]),
			route_over_carriers(Flags#route_flags{scenario=outbound}, ApiJObj);
		    {ok, FlagsIn} ->
			?LOG("We know ~s, and credit checked, routing in", [DID]),
			%% we'll do the actual trunk reservation on CHANNEL_BRIDGE in ts_call_handler
			case inbound_route(FlagsIn) of
			    {ok, Routes, FlagsIn2} ->
				%% wh_timer:tick("found inbound route to route over instead"),
				?LOG("We know ~s, generated inbound route"),
				case FlagsIn#route_flags.scenario of
				    inbound ->
					response(Routes, ApiJObj, FlagsIn2#route_flags{routes_generated=Routes
										       ,account_doc_id=OrigAcctId
										       ,diverted_account_doc_id=FlagsIn#route_flags.account_doc_id
										       ,scenario=outbound_inbound
										      });
				    inbound_failover ->
					response(Routes, ApiJObj, FlagsIn2#route_flags{routes_generated=Routes
										       ,account_doc_id=OrigAcctId
										       ,diverted_account_doc_id=FlagsIn#route_flags.account_doc_id
										       ,scenario=outbound_inbound_failover
										      })
				end;
			    {error, _ErrCredit} ->
				?LOG("We know ~s, but errors in credit check with ~p, routing over carriers", DID, _ErrCredit),
				route_over_carriers(Flags#route_flags{scenario=outbound}, ApiJObj)
			end;

		    %% someone on the account is calling someone else on the same account; don't allocate a trunk
		    {error, entry_exists} ->
			case inbound_route(FlagsIn1) of
			    {ok, Routes, _} ->
				?LOG("~s is in the same account as caller, no new trunk", [DID]),
				response(Routes, ApiJObj, Flags#route_flags{direction = <<"inbound">>});
			    {error, _} ->
				route_over_carriers(Flags#route_flags{scenario=outbound}, ApiJObj)
			end;

		    {error, _}  ->
			?LOG("Unable to route back to account ~s (credit fail)", [FlagsIn1#route_flags.account_doc_id]),
			_ = ts_acctmgr:release_trunk(FlagsIn1#route_flags.account_doc_id, FlagsIn1#route_flags.callid, 0),
			response(503, ApiJObj, Flags)
		end
	end
    catch
	_A:_B ->
	    ?LOG_SYS("Exception occurred: ~p:~p, releasing trunk", [_A, _B]),
	    ?LOG_SYS("Stacktrace: ~p", [erlang:get_stacktrace()]),
	    _ = ts_acctmgr:release_trunk(AccountDocId, CallID, 0),
	    response(500, ApiJObj, Flags)
    end.

-spec(route_over_carriers/2 :: (Flags :: #route_flags{}, ApiJObj :: json_object()) -> tuple(ok, iolist()) | tuple(error, string())).
route_over_carriers(Flags, ApiJObj) ->
    %% wh_timer:tick("route_over_carriers/2"),
    case ts_carrier:route(Flags) of
	{ok, Routes} ->
	    %% wh_timer:tick("routes found, response time"),
	    response(Routes, ApiJObj, Flags#route_flags{routes_generated=Routes});
	{error, _Error} ->
	    ?LOG("Outbound routing error: ~p", [_Error]),
	    {error, "We don't handle this route"}
    end.

-spec(inbound_route/1 :: (Flags :: #route_flags{}) -> tuple(ok, json_objects(), #route_flags{}) | tuple(error, string())).
inbound_route(#route_flags{auth_user=U, auth_realm=R, to_user=To, inbound_format=InFormat, failover=Failover
			   ,media_handling=MediaHandling, progress_timeout=ProgressTimeout}=Flags) ->
    %% wh_timer:tick("inbound_route/1"),

    InviteBase = [{<<"To-User">>, U}, {<<"To-Realm">>, R}],

    Invite = invite_format(whistle_util:binary_to_lower(InFormat), To) ++ InviteBase,

    Route = [{<<"Weight-Cost">>, <<"1">>}
	     ,{<<"Weight-Location">>, <<"1">>}
	     ,{<<"Custom-Channel-Vars">>, {struct, [
						    {<<"Auth-User">>, U}
						    ,{<<"Auth-Realm">>, R}
						    ,{<<"Direction">>, <<"inbound">>}
						    | ts_util:get_base_channel_vars(Flags)
						   ]}
	      }
	     ,{<<"Media">>, ts_util:get_media_handling(MediaHandling)}
	     | Invite ],

    Route1 = case ProgressTimeout of
		 none -> Route;
		 Secs -> [{<<"Progress-Timeout">>, whistle_util:to_integer(Secs)} | Route]
	     end,

    case whistle_api:route_resp_route_v(Route1) of
	true ->
	    add_failover_route(Failover, Flags, {struct, Route1});
	false ->
	    {error, "Inbound route validation failed"}
    end.

-spec(invite_format/2 :: (Format :: binary(), To :: binary()) -> proplist()).
invite_format(<<"e.164">>, To) ->
    [{<<"Invite-Format">>, <<"e164">>}, {<<"To-DID">>, whistle_util:to_e164(To)}];
invite_format(<<"e164">>, To) ->
    [{<<"Invite-Format">>, <<"e164">>}, {<<"To-DID">>, whistle_util:to_e164(To)}];
invite_format(<<"1npanxxxxxx">>, To) ->
    [{<<"Invite-Format">>, <<"1npan">>}, {<<"To-DID">>, whistle_util:to_1npan(To)}];
invite_format(<<"1npan">>, To) ->
    [{<<"Invite-Format">>, <<"1npan">>}, {<<"To-DID">>, whistle_util:to_1npan(To)}];
invite_format(<<"npanxxxxxx">>, To) ->
    [{<<"Invite-Format">>, <<"npan">>}, {<<"To-DID">>, whistle_util:to_npan(To)}];
invite_format(<<"npan">>, To) ->
    [{<<"Invite-Format">>, <<"npan">>}, {<<"To-DID">>, whistle_util:to_npan(To)}];
invite_format(_, _) ->
    [{<<"Invite-Format">>, <<"username">>} ].

-spec(add_failover_route/3 :: (tuple() | tuple(binary(), binary()), Flags :: #route_flags{}, InboundRoute :: json_object()) ->
				   tuple(ok, json_objects(), #route_flags{})).
add_failover_route({}, Flags, InboundRoute) -> {ok, [InboundRoute], Flags#route_flags{scenario=inbound}};
%% route to a SIP URI
add_failover_route({<<"sip">>, URI}, #route_flags{media_handling=MediaHandling}=Flags, InboundRoute) ->
    ?LOG("Adding SIP failover ~p", [URI]),

    {ok, [InboundRoute, {struct, [{<<"Route">>, URI}
				  ,{<<"Invite-Format">>, <<"route">>}
				  ,{<<"Weight-Cost">>, <<"1">>}
				  ,{<<"Weight-Location">>, <<"1">>}
				  ,{<<"Failover-Route">>, <<"true">>}
				  ,{<<"Media">>, ts_util:get_media_handling(MediaHandling)}
				 ]}]
     ,Flags#route_flags{scenario=inbound_failover}
    };
%% route to a E.164 number - need to setup outbound for this sucker
add_failover_route({<<"e164">>, DID}, #route_flags{callid=CallID}=Flags, InboundRoute) ->
    ?LOG("Trying to add DID failover ~s", [DID]),
    OutBFlags = Flags#route_flags{to_user=DID
				  ,callid = <<CallID/binary, "-failover">>
				  ,direction = <<"outbound">>
				 },
    case ts_credit:check(OutBFlags) of
	{ok, OutBFlags1} ->
	    case ts_carrier:route(OutBFlags1) of
		{ok, Routes} ->
		    ?LOG("Adding DID failover ~s", [DID]),
		    { ok, [InboundRoute | Routes], Flags#route_flags{scenario=inbound_failover}};
		{error, _Error} ->
		    ?LOG("Error adding DID failover ~p", [_Error]),
		    _ = ts_acctmgr:release_trunk(OutBFlags1#route_flags.account_doc_id, OutBFlags1#route_flags.callid, 0),
		    { ok, [InboundRoute], Flags#route_flags{scenario=inbound}}
	    end;
	{error, _Error} ->
	    ?LOG("Failed to secure failover trunk for ~s: ~p", [DID, _Error]),
	    {ok, [InboundRoute], Flags#route_flags{scenario=inbound}}
    end.

-spec(inbound_features/1 :: (Flags :: #route_flags{}) -> #route_flags{}).
inbound_features(Flags) ->
    Features = [ts_tollfree],
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

-spec(create_flags/2 :: (Did :: binary(), ApiJObj :: json_object()) -> #route_flags{}).
create_flags(Did, ApiJObj) ->
    %% wh_timer:tick("create_flags/2"),
    case lookup_did(Did) of
	{ok, DidJObj} ->
	    create_flags(Did, ApiJObj, DidJObj);
	{error, _E} ->
	    create_flags(Did, ApiJObj, ?EMPTY_JSON_OBJECT)
    end.

create_flags(_, ApiJObj, DidJObj) ->
    ChannelVars = wh_json:get_value(<<"Custom-Channel-Vars">>, ApiJObj, ?EMPTY_JSON_OBJECT),

    F1 = case DidJObj of
	     ?EMPTY_JSON_OBJECT -> add_auth_user(add_auth_realm(#route_flags{}, wh_json:get_value(<<"Realm">>, ChannelVars)), wh_json:get_value(<<"Username">>, ChannelVars));
	     _ -> add_auth_realm(flags_from_did(DidJObj, #route_flags{}), wh_json:get_value(<<"Realm">>, ChannelVars))
	 end,

    AuthUser = F1#route_flags.auth_user,
    Realm = F1#route_flags.auth_realm,

    {ok, D} = lookup_user_flags(AuthUser, Realm),
    Id = wh_json:get_value(<<"id">>, D),

    F2 = flags_from_srv(wh_json:get_value(<<"server">>, D, ?EMPTY_JSON_OBJECT), F1#route_flags{account_doc_id = Id}),
    F3 = flags_from_account(wh_json:get_value(<<"account">>, D, ?EMPTY_JSON_OBJECT), F2),
    flags_from_api(ApiJObj, ChannelVars, F3).

-spec(flags_from_api/3 :: (ApiJObj :: json_object(), ChannelVarsJObj :: json_object(), Flags :: #route_flags{}) -> #route_flags{}).
flags_from_api(ApiJObj, ChannelVarsJObj, Flags) ->
    %% wh_timer:tick("flags_from_api/3"),
    [ToUser, ToDomain] = binary:split(wh_json:get_value(<<"To">>, ApiJObj), <<"@">>),
    [FromUser, FromDomain] = binary:split(wh_json:get_value(<<"From">>, ApiJObj), <<"@">>),

    F0 = add_caller_id(Flags, {struct, [ {<<"cid_name">>, wh_json:get_value(<<"Caller-ID-Name">>, ApiJObj, <<>>)}
					 ,{<<"cid_number">>, wh_json:get_value(<<"Caller-ID-Number">>, ApiJObj, <<>>)}
				       ]
			      }),
    F1 = F0#route_flags{
	   callid = wh_json:get_value(<<"Call-ID">>, ApiJObj)
	   ,to_user = whistle_util:to_e164(ToUser)
	   ,to_domain = ToDomain
	   ,from_user = whistle_util:to_e164(FromUser)
	   ,from_domain = FromDomain
	   ,direction = wh_json:get_value(<<"Direction">>, ChannelVarsJObj, <<"inbound">>)
	  },
    add_auth_user(F1, wh_json:get_value(<<"Auth-User">>, ChannelVarsJObj)).

%% Flags from the DID
%% - Failover
%% - Caller ID
%% - Auth User
%% - Auth Realm
-spec(flags_from_did/2 :: (DidJObj :: json_object(), Flags :: #route_flags{}) -> #route_flags{}).
flags_from_did(DidJObj, Flags) ->
    %% wh_timer:tick("flags_from_did/2"),
    DidOptions = wh_json:get_value(<<"DID_Opts">>, DidJObj, ?EMPTY_JSON_OBJECT),
    AuthOpts = wh_json:get_value(<<"auth">>, DidJObj, ?EMPTY_JSON_OBJECT),

    Opts = wh_json:get_value(<<"options">>, DidJObj, ?EMPTY_JSON_OBJECT),
    Acct = wh_json:get_value(<<"account">>, DidJObj, ?EMPTY_JSON_OBJECT),

    F0 = add_failover(Flags, wh_json:get_value(<<"failover">>, DidOptions, ?EMPTY_JSON_OBJECT)),
    F1 = add_caller_id(F0, wh_json:get_value(<<"caller_id">>, DidOptions, ?EMPTY_JSON_OBJECT)),
    F2 = F1#route_flags{route_options = Opts
			,account_doc_id = wh_json:get_value(<<"id">>, DidJObj)
		       },
    F3 = add_auth_user(F2, wh_json:get_value(<<"auth_user">>, AuthOpts)),
    F4 = add_auth_realm(F3, wh_json:get_value(<<"auth_realm">>, AuthOpts, wh_json:get_value(<<"auth_realm">>, Acct))),
    add_force_outbound(F4, wh_json:get_value(<<"force_outbound">>, DidOptions, false)).

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
    %% wh_timer:tick("flags_from_srv/2"),
    Options = wh_json:get_value(<<"options">>, Srv, ?EMPTY_JSON_OBJECT),

    F0 = Flags#route_flags{inbound_format = wh_json:get_value(<<"inbound_format">>, Options, <<>>)
			   ,codecs = wh_json:get_value(<<"codecs">>, Srv, [])
			   ,media_handling = wh_json:get_value(<<"media_handling">>, Options)
			   ,progress_timeout = wh_json:get_value(<<"progress_timeout">>, Options, none)
			  },
    F1 = add_caller_id(F0, wh_json:get_value(<<"caller_id">>, Srv, ?EMPTY_JSON_OBJECT)),
    F2 = add_failover(F1, wh_json:get_value(<<"failover">>, Srv, ?EMPTY_JSON_OBJECT)),
    add_force_outbound(F2, wh_json:get_value(<<"force_outbound">>, Options, false)).

%% Flags from the Account
%% - Credit available
%% - Trunks purchased <- eventually need to look at the server under the account to see how many are allocatable to the server
%% - Trunks in use
%% - Caller ID <- only if it hasn't been set at the server or DID level
%% - Failover <- only if it hasn't been set at the server or DID level
-spec(flags_from_account(Acct :: json_object(), Flags :: #route_flags{}) -> #route_flags{}).
flags_from_account(Acct, Flags) ->
    %% wh_timer:tick("flags_from_acct/2"),
    F1 = add_caller_id(Flags, wh_json:get_value(<<"caller_id">>, Acct, ?EMPTY_JSON_OBJECT)),
    F2 = add_failover(F1, wh_json:get_value(<<"failover">>, Acct, ?EMPTY_JSON_OBJECT)),
    add_auth_realm(F2, wh_json:get_value(<<"auth_realm">>, Acct)).

-spec(add_force_outbound/2 :: (F :: #route_flags{}, Force :: boolean()) -> #route_flags{}).
add_force_outbound(#route_flags{force_outbound=undefined}=F, Force) ->
    F#route_flags{force_outbound=whistle_util:to_boolean(Force)};
add_force_outbound(F, _) -> F.

-spec(add_failover/2 :: (F0 :: #route_flags{}, FOver :: json_object()) -> #route_flags{}).
add_failover(#route_flags{failover={}}=F0, ?EMPTY_JSON_OBJECT) -> F0;
add_failover(#route_flags{failover={}}=F0, Failover) ->
    case wh_json:get_value(<<"e164">>, Failover) of
	undefined ->
	    case wh_json:get_value(<<"sip">>, Failover) of
		undefined -> F0;
		SipFail -> F0#route_flags{failover={<<"sip">>, SipFail}}
	    end;
	E164Fail ->
	    F0#route_flags{failover={<<"e164">>, E164Fail}}
    end;
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

-spec(add_caller_id/2 :: (F0 :: #route_flags{}, CID :: json_object()) -> #route_flags{}).
add_caller_id(#route_flags{caller_id={}}=F0, ?EMPTY_JSON_OBJECT) -> F0;
add_caller_id(#route_flags{caller_id={}}=F0, {struct, _}=CID) ->
    F0#route_flags{caller_id = {wh_json:get_value(<<"cid_name">>, CID, <<>>)
				,wh_json:get_value(<<"cid_number">>, CID, <<>>)}};
add_caller_id(F, _) -> F.

-spec(response/3 :: (Routes :: json_objects() | integer(), JObj :: json_object(), Flags :: #route_flags{}) -> tuple(ok, iolist()) | tuple(error, string())).
response(ErrCode, JObj, _Flags) when is_integer(ErrCode) ->
    %% wh_timer:tick("response/3 err code"),
    JObj1 = {struct, [ {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
	      | whistle_api:default_headers(<<>>, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION) ]
	     },
    response(ErrCode, JObj1);
response(Routes, JObj, Flags) ->
    %% wh_timer:tick("response/3 routes here"),

    {ok, Pid} = ts_call_sup:start_proc([Flags#route_flags.callid, Flags]),
    %% wh_timer:tick("response/3 routes start_call_handler"),
    {ok, Q} = ts_call_handler:get_queue(Pid),
    %% wh_timer:tick("response/3 routes got queue of call_handler"),
    JObj1 = {struct, [ {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
		       | whistle_api:default_headers(Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION) ]
	    },
    %% wh_timer:tick("response/3 new jobj"),
    response(Routes, JObj1).

response(Routes, JObj) ->
    %% wh_timer:tick("response/2"),
    whistle_api:route_resp(specific_response(Routes, JObj)).

-spec(specific_response/2 :: (CodeOrRoutes :: integer() | json_objects(), Prop :: json_object()) -> json_object()).
specific_response(404, {struct, Prop}) ->
    %% wh_timer:tick("specific_response 404"),
    {struct, [{<<"Routes">>, []}
	      ,{<<"Method">>, <<"error">>}
	      ,{<<"Route-Error-Code">>, <<"404">>}
	      ,{<<"Route-Error-Message">>, <<"Not Found">>}
	      | Prop ]};
specific_response(503, {struct, Prop}) ->
    %% wh_timer:tick("specific_response 503"),
    {struct, [{<<"Routes">>, []}
	      ,{<<"Method">>, <<"error">>}
	      ,{<<"Route-Error-Code">>, <<"503">>}
	      ,{<<"Route-Error-Message">>, <<"Insufficient Credit">>}
	      | Prop]};
specific_response(500, {struct, Prop}) ->
    %% wh_timer:tick("specific_response 500"),
    {struct, [{<<"Routes">>, []}
	      ,{<<"Method">>, <<"error">>}
	      ,{<<"Route-Error-Code">>, <<"500">>}
	      ,{<<"Route-Error-Message">>, <<"Internal Server Error">>}
	      | Prop]};
specific_response(Routes, {struct, Prop}) ->
    %% wh_timer:tick("specific_response routes"),
    {struct, [{<<"Routes">>, Routes}
	      ,{<<"Method">>, <<"bridge">>}
	      | Prop]}.
