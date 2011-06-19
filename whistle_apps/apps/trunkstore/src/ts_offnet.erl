%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle calls coming from off-net (carriers), destined for on-net endpoints.
%%% Also handle if the user has configured failover, sending the call back
%%% off-net (either via SIP or PSTN).
%%% @end
%%% Created : 18 Jun 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_offnet).

-behaviour(gen_server).

%% API
-export([start_link/1, inbound_endpoint/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ts.hrl").

-define(SERVER, ?MODULE).
-define(APP_NAME, <<"ts_offnet">>).
-define(APP_VERSION, <<"0.0.1">>).

-record(state, {
         route_req_jobj = ?EMPTY_JSON_OBJECT :: json_object()
         ,callctl_q = <<>> :: binary()
         ,endpoint = ?EMPTY_JSON_OBJECT :: json_object()
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(JObj) ->
    gen_server:start_link(?MODULE, [JObj], []).

-spec(inbound_endpoint/2 :: (Srv :: pid(), Endpoint :: json_object()) -> ok).
inbound_endpoint(Srv, Endpoint) ->
    gen_server:cast(Srv, {inbound_endpoint, Endpoint}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([JObj]) ->
    put(callid, wh_json:get_value(<<"Call-ID">>, JObj)),
    {ok, #state{route_req_jobj=JObj}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({inbound_endpoint, Endpoint}, State) ->
    {noreply, State#state{endpoint=Endpoint}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{route_req_jobj=JObj}=S) ->
    {ok, CQ} = start_amqp(get(callid)),
    ?LOG_SYS("Starting up ts_offnet with AMQP Queue: ~s", [CQ]),

    send_park(CQ, JObj),

    Self = self(),
    spawn(fun() -> inbound_route(Self, JObj) end),

    {noreply, S};
handle_info({_, #amqp_msg{payload=Payload}}, #state{route_req_jobj=RouteJObj, callctl_q = <<>>}=S) ->
    WinJObj = mochijson2:decode(Payload),
    true = whistle_api:route_win_v(WinJObj),
    {noreply, S#state{callctl_q=wh_json:get_value(<<"Control-Queue">>, WinJObj)}};
handle_info({_, #amqp_msg{payload=Payload}}, S) ->
    {noreply, S}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _) ->
    ?LOG_SYS("Terminating: ~p~n", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    ?LOG_SYS("Code Change from ~p~n", [_OldVsn]),
    {ok, State}.

-spec(start_amqp/1 :: (CallID :: binary()) -> tuple(ok, binary()) | tuple(error, amqp_error)).
start_amqp(CallID) ->
    try
        Q = amqp_util:new_queue(),

        %% Bind the queue to an exchange
        _ = amqp_util:bind_q_to_targeted(Q),
        _ = amqp_util:bind_q_to_callevt(Q, CallID),

        amqp_util:basic_consume(Q, [{exclusive, false}]),

        {ok, Q}
    catch
	_:_ -> {error, amqp_error}
    end.

send_park(Q, JObj) ->
    JObj1 = {struct, [ {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                       ,{<<"Routes">>, []}
                       ,{<<"Method">>, <<"park">>}
		       | whistle_api:default_headers(Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION) ]
	    },
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    JSON = whistle_api:route_resp(JObj1),
    ?LOG("Sending to ~s: ~s", [RespQ, JSON]),
    amqp_util:targeted_publish(RespQ, JSON, <<"application/json">>).

-spec(inbound_route/2 :: (ParentSrv :: pid(), JObj :: json_object()) -> ok).
inbound_route(ParentSrv, JObj) ->
    %% wh_timer:tick("inbound_route/1"),
    AcctID = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),

    [ToUser, _ToDomain] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
    ToDID = whistle_util:to_e164(ToUser),

    {AuthUser, AuthRealm, RouteOpts, InFormat} = routing_data(ToDID),

    {ok, RateData} = ts_credit:reserve(ToDID, get(callid), AcctID, inbound, RouteOpts),

    InviteBase = [{<<"To-User">>, AuthUser}, {<<"To-Realm">>, AuthRealm}],

    Invite = invite_format(whistle_util:binary_to_lower(InFormat), ToDID) ++ InviteBase,

    Route = [{<<"Weight-Cost">>, <<"1">>}
	     ,{<<"Weight-Location">>, <<"1">>}
	     ,{<<"Custom-Channel-Vars">>, {struct, [
						    {<<"Auth-User">>, AuthUser}
						    ,{<<"Auth-Realm">>, AuthRealm}
						    ,{<<"Direction">>, <<"inbound">>}
						    | RateData
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

-spec(routing_data/1 :: (ToDID :: binary()) -> proplist()).
routing_data(ToDID) ->
    {ok, Settings} = lookup_did(ToDID),

    AuthOpts = wh_json:get_value(<<"auth">>, Settings, ?EMPTY_JSON_OBJECT),
    Acct = wh_json:get_value(<<"account">>, Settings, ?EMPTY_JSON_OBJECT),
    DidOptions = wh_json:get_value(<<"DID_Opts">>, Settings, ?EMPTY_JSON_OBJECT),
    RouteOpts = wh_json:get_value(<<"options">>, DidOptions, []),

    AuthU = wh_json:get_value(<<"auth_user">>, AuthOpts),
    AuthR = wh_json:get_value(<<"auth_realm">>, AuthOpts, wh_json:get_value(<<"auth_realm">>, Acct)),

    {Srv, Acct} = try
                      {ok, AccountSettings} = lookup_user_flags(AuthU, AuthR),
                      {
                        wh_json:get_value(<<"server">>, AccountSettings, ?EMPTY_JSON_OBJECT)
                        ,wh_json:get_value(<<"account">>, AccountSettings, ?EMPTY_JSON_OBJECT)
                      }
                  catch
                      _:_ -> {?EMPTY_JSON_OBJECT, ?EMPTY_JSON_OBJECT}
                  end,

    SrvOptions = wh_json:get_value(<<"options">>, Srv, ?EMPTY_JSON_OBJECT)

    [ {<<"Invite-Format">>, wh_json:get_value(<<"inbound_format">>, SrvOptions, <<"npan">>)}
      ,{<<"Codecs">>, wh_json:get_value(<<"codecs">>, Srv, [])}
      ,{<<"Media-Handling">>, wh_json:get_value(<<"media_handling">>, Options, <<"bypass">>)}
      ,{<<"Progress-Timeout">>, wh_json:get_value(<<"progress_timeout">>, Options, 8)}
      ,{<<"Auth-User">>, AuthU}
      ,{<<"Auth-Realm">>, AuthR}
      ,{<<"Route-Options">>, RouteOpts}
    ].

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

-spec(lookup_did/1 :: (DID :: binary()) -> tuple(ok, json_object())).
lookup_did(DID) ->
    Options = [{<<"key">>, DID}],
    case wh_cache:fetch({lookup_did, DID}) of
	{ok, _}=Resp ->
	    %% wh_timer:tick("lookup_did/1 cache hit"),
	    {ok, Resp};
	{error, not_found} ->
	    %% wh_timer:tick("lookup_did/1 cache miss"),
	    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_DIDLOOKUP, Options) of
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
