%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_from_offnet).

-export([start_link/1, init/2]).

-include("ts.hrl").

-record(state, {
	  callid = <<>> :: binary()
          ,acctid = <<>> :: binary()
	  ,route_req_jobj = ?EMPTY_JSON_OBJECT :: json_object()
          ,endpoint = ?EMPTY_JSON_OBJECT :: json_object()
          ,my_q = <<>> :: binary()
          ,callctl_q = <<>> :: binary()
          ,call_start = erlang:now() :: tuple()
          ,failover = ?EMPTY_JSON_OBJECT :: json_object()
	 }).

-define(APP_NAME, <<"ts_from_onnet">>).
-define(APP_VERSION, <<"0.0.5">>).
-define(WAIT_FOR_WIN_TIMEOUT, 5000).
-define(WAIT_FOR_BRIDGE_TIMEOUT, 10000).
-define(WAIT_FOR_HANGUP_TIMEOUT, 1000 * 60 * 60 * 2). %% 2 hours

start_link(RouteReqJObj) ->
    proc_lib:start_link(?MODULE, init, [self(), RouteReqJObj]).

init(Parent, RouteReqJObj) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    CallID = wh_json:get_value(<<"Call-ID">>, RouteReqJObj),
    put(callid, CallID),
    start_amqp(#state{callid=CallID, route_req_jobj=RouteReqJObj}).

start_amqp(#state{route_req_jobj=JObj}=State) ->
    Q = amqp_util:new_queue(),

    %% Bind the queue to an exchange
    _ = amqp_util:bind_q_to_targeted(Q),
    amqp_util:basic_consume(Q, [{exclusive, false}]),

    endpoint_data(State#state{my_q=Q}, JObj).

endpoint_data(State, JObj) ->
    {endpoint, EP} = endpoint_data(JObj),
    send_park(State#state{endpoint=EP}).

send_park(#state{route_req_jobj=JObj, my_q=Q}=State) ->
    JObj1 = {struct, [ {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                       ,{<<"Routes">>, []}
                       ,{<<"Method">>, <<"park">>}
		       | whistle_api:default_headers(Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION) ]
	    },
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    JSON = whistle_api:route_resp(JObj1),
    ?LOG("Sending to ~s: ~s", [RespQ, JSON]),
    amqp_util:targeted_publish(RespQ, JSON, <<"application/json">>),

    wait_for_win(State, ?WAIT_FOR_WIN_TIMEOUT).

wait_for_win(#state{callid=CallID, my_q=Q}=State, Timeout) ->
    receive
	{_, #amqp_msg{payload=Payload}} ->
	    WinJObj = mochijson2:decode(Payload),
	    true = whistle_api:route_win_v(WinJObj),
	    CallID = wh_json:get_value(<<"Call-ID">>, WinJObj),

	    _ = amqp_util:bind_q_to_callevt(Q, CallID),
	    _ = amqp_util:bind_q_to_callevt(Q, CallID, cdr),

	    CallctlQ = wh_json:get_value(<<"Control-Queue">>, WinJObj),

	    bridge_to_endpoint(State#state{callctl_q=CallctlQ})
    after Timeout ->
	    ?LOG("Timed out(~b) waiting for route_win", [Timeout])
    end.

bridge_to_endpoint(#state{callctl_q=CtlQ, callid=CallID, endpoint=EP}=State) ->
    Command = [
	       {<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, [{struct, whistle_api:bridge_req_endpoint(EP)}]}
               ,{<<"Timeout">>, 26}
               ,{<<"Ignore-Early-Media">>, <<"false">>}
               ,{<<"Ringback">>, <<"us-ring">>}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Call-ID">>, CallID}
               | whistle_api:default_headers(<<>>, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
	      ],
    {ok, Payload} = whistle_api:bridge_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    ?LOG(CallID, "Sending bridge command: ~s", [Payload]),
    amqp_util:callctl_publish(CtlQ, Payload),
    wait_for_bridge(State#state{failover=wh_json:get_value(<<"Failover">>, EP, ?EMPTY_JSON_OBJECT)
				,acctid = wh_json:get_value(<<"Auth-User">>, EP)
			       }, ?WAIT_FOR_BRIDGE_TIMEOUT).

wait_for_bridge(State, Timeout) ->
    Start = erlang:now(),
    receive
	{_, #amqp_msg{payload=Payload}} ->
	    JObj = mochijson2:decode(Payload),
	    case { wh_json:get_value(<<"Application-Name">>, JObj)
		   ,wh_json:get_value(<<"Event-Name">>, JObj)
		   ,wh_json:get_value(<<"Event-Category">>, JObj) } of
		{ _, <<"CHANNEL_BRIDGE">>, <<"call_event">> } ->
		    ?LOG("Bridge successful"),
		    wait_for_hangup(State#state{call_start=erlang:now()});
		{ <<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
		    case wh_json:get_value(<<"Application-Response">>, JObj) of
			<<"SUCCESS">> ->
			    ?LOG("Bridge successful"),
			    wait_for_hangup(State#state{call_start=erlang:now()});
			Cause ->
			    ?LOG("Failed to bridge: ~s", [Cause]),
			    try_failover(State)
		    end;
		{ _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
		    ?LOG("Channel hungup");
		{ _, _, <<"error">> } ->
		    ?LOG("Execution failed");
		_Other ->
		    ?LOG("Received other: ~p~n", [_Other]),
		    Diff = Timeout - (timer:now_diff(erlang:now(), Start) div 1000),
		    ?LOG("~b left to timeout", [Diff]),
		    wait_for_bridge(State, Diff)
	    end
    after Timeout ->
	    ?LOG("Timed out(~b) waiting for bridge success", [Timeout])
    end.

wait_for_cdr(State) ->
    wait_for_hangup(State, ?WAIT_FOR_HANGUP_TIMEOUT).
wait_for_cdr(#state{callid=CallID, acctid=AcctID, call_start=Start}=State, Timeout) ->
    receive
	{_, #amqp_msg{payload=Payload}} ->
	    JObj = mochijson2:decode(Payload),
            case { wh_json:get_value(<<"Event-Category">>, JObj)
		   ,wh_json:get_value(<<"Event-Name">>, JObj) } of
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
                    {ok, channel_hungup};
                { <<"error">>, _ } ->
                    {error, execution_failure};
                _ ->
                    wait_for_hangup(?WAIT_FOR_HANGUP_TIMEOUT)
            end
    after Timeout ->
	    ?LOG("Timed out(~b) waiting for hangup"),
	    Duration = (timer:now_diff(erlang:now(), Start) div 1000) - Timeout,
	    ts_acctmgr:release_trunk(AcctID, CallID, Duration)
    end.

try_failover(State) ->
    ok.

%%--------------------------------------------------------------------
%% Out-of-band functions
%%--------------------------------------------------------------------
endpoint_data(JObj) ->
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

    
    {endpoint, {struct, [{<<"Custom-Channel-Vars">>, {struct, [
							       {<<"Auth-User">>, AuthUser}
							       ,{<<"Auth-Realm">>, AuthRealm}
							       ,{<<"Direction">>, <<"inbound">>}
							       | RateData
							      ]}
			 }
			 | Invite ]
	       }}.

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
