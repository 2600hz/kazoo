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
         ,call_id = <<>> :: binary()
         ,callctl_q = <<>> :: binary()
         ,endpoint = ?EMPTY_JSON_OBJECT :: json_object()
         ,win_fail_ref = undefined :: undefined | reference()
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
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallID),
    {ok, #state{route_req_jobj=JObj, call_id=CallID}, 0}.

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
handle_cast({inbound_endpoint, Endpoint}, #state{callctl_q = <<>>}=State) ->
    TRef = erlang:start_timer(5000, self(), win_fail),
    {noreply, State#state{endpoint=Endpoint, win_fail_ref=TRef}};
handle_cast({inbound_endpoint, Endpoint}, #state{callctl_q=CtlQ, call_id=CallID}=State) ->
    spawn(fun() -> bridge_to_endpoint(CtlQ, CallID, Endpoint) end),
    {noreply, #state{endpoint=Endpoint}=State}.

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

handle_info(win_fail_ref, S) ->
    ?LOG_END("Failed to win route, timed out"),
    {stop, normal, S};

handle_info({_, #amqp_msg{payload=Payload}}, #state{callctl_q = <<>>, endpoint = ?EMPTY_JSON_OBJECT, win_fail_ref=TRef}=S) ->
    WinJObj = mochijson2:decode(Payload),

    try
	true = whistle_api:route_win_v(WinJObj),
	erlang:cancel_timer(TRef),

	{noreply, S#state{callctl_q=wh_json:get_value(<<"Control-Queue">>, WinJObj)}};
    catch
	_:_ -> {noreply, S}
    end;

handle_info({_, #amqp_msg{payload=Payload}}, #state{callctl_q = <<>>, endpoint=EP, call_id=CallID, win_fail_ref=TRef}=S) ->
    WinJObj = mochijson2:decode(Payload),

    try
	true = whistle_api:route_win_v(WinJObj),
	erlang:cancel_timer(TRef),

	CtlQ = wh_json:get_value(<<"Control-Queue">>, WinJObj),
	spawn(fun() -> bridge_to_endpoint(CtlQ, CallID, EP) end),
	{noreply, S#state{callctl_q=CtlQ}}
    catch
	_:_ -> {noreply, S}
    end;

handle_info({_, #amqp_msg{payload=Payload}}, #state{callctl_q=CtlQ}=S) ->
    JObj = mochijson2:decode(Payload),
    case { wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Event-Category">>, JObj) } of
	{ _, <<"CHANNEL_BRIDGE">>, <<"call_event">> } ->
	    {ok, JObj};
	{ <<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
	    case wh_json:get_value(<<"Application-Response">>, JObj) of
		<<"SUCCESS">> -> {ok, JObj};
		Cause -> {error, {bridge_failed, Cause}}
	    end;
	{ _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
	    {error, channel_hungup};
	{ _, _, <<"error">> } ->
	    {error, execution_failed};
	_ ->
	    DiffMicro = timer:now_diff(erlang:now(), Start),
	    wait_for_bridge(Timeout - (DiffMicro div 1000))
    end;

handle_info(_Message, S) ->
    ?LOG("Unhandled message: ~p", [_Message]),
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

    RoutingData = routing_data(ToDID),

    AuthUser = props:get_value(<<"Auth-User">>, RoutingData),
    AuthRealm = props:get_value(<<"Auth-Realm">>, RoutingData),

    {ok, RateData} = ts_credit:reserve(ToDID, get(callid), AcctID, inbound, props:get_value(<<"Route-Options">>, RoutingData)),

    InviteBase = [{<<"To-User">>, AuthUser}, {<<"To-Realm">>, AuthRealm} | RoutingData],

    InFormat = props:get_value(<<"Invite-Format">>, RoutingData),
    Invite = invite_format(whistle_util:binary_to_lower(InFormat), ToDID) ++ InviteBase,

    
    ?MODULE:inbound_endpoint(ParentSrv, {struct, [{<<"Custom-Channel-Vars">>, {struct, [
											{<<"Auth-User">>, AuthUser}
											,{<<"Auth-Realm">>, AuthRealm}
											,{<<"Direction">>, <<"inbound">>}
											| RateData
										       ]}
						  }
						  | Invite ]
					}).

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

    SrvOptions = wh_json:get_value(<<"options">>, Srv, ?EMPTY_JSON_OBJECT),

    RD0 = [ {<<"Invite-Format">>, wh_json:get_value(<<"inbound_format">>, SrvOptions, <<"npan">>)}
	    ,{<<"Codecs">>, wh_json:get_value(<<"codecs">>, Srv, [])}
	    ,{<<"Bypass-Media">>, wh_json:get_value(<<"media_handling">>, SrvOptions, <<"bypass">>)}
	    ,{<<"Progress-Timeout">>, wh_json:get_value(<<"progress_timeout">>, SrvOptions, 8)}
	    ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, SrvOptions, <<"true">>)}
	    ,{<<"Auth-User">>, AuthU}
	    ,{<<"Auth-Realm">>, AuthR}
	    ,{<<"To-User">>, AuthU}
	    ,{<<"To-Realm">>, AuthR}
	    ,{<<"Route-Options">>, RouteOpts}
	    ,{<<"To-DID">>, ToDID}
	  ],
    case wh_json:get_value(<<"failover">>, DidOptions
			   ,wh_json:get_value(<<"failover">>, Srv
					      ,wh_json:get_value(<<"failover">>, Acct))
			  ) of
	undefined -> RD0;
	F -> [{<<"Failover">>, F} | RD0]
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

bridge_to_endpoint(CtlQ, CallID, Endpoint) ->
    Command = [
	       {<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, [Endpoint]}
               ,{<<"Timeout">>, 26}
               ,{<<"Ignore-Early-Media">>, <<"false">>}
               ,{<<"Ringback">>, <<"us-ring">>}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Call-ID">>, CallID}
               | whistle_api:default_headers(<<>>, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
	      ],
    {ok, Payload} = whistle_api:bridge_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    ?LOG(CallID, "Sending bridge command: ~s", [Payload]),
    amqp_util:callctl_publish(CtlQ, Payload).
