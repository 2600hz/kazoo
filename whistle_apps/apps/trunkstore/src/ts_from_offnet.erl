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
	  aleg_callid = <<>> :: binary()
	  ,bleg_callid = <<>> :: binary()
          ,acctid = <<>> :: binary()
	  ,route_req_jobj = ?EMPTY_JSON_OBJECT :: json_object()
          ,endpoint = ?EMPTY_JSON_OBJECT :: json_object()
          ,my_q = <<>> :: binary()
          ,callctl_q = <<>> :: binary()
          ,failover = ?EMPTY_JSON_OBJECT :: json_object()
	 }).

-define(APP_NAME, <<"ts_from_offnet">>).
-define(APP_VERSION, <<"0.1.0">>).
-define(WAIT_FOR_WIN_TIMEOUT, 5000).
-define(WAIT_FOR_BRIDGE_TIMEOUT, 10000).
-define(WAIT_FOR_HANGUP_TIMEOUT, 1000 * 60 * 60 * 2). %% 2 hours
-define(WAIT_FOR_CDR_TIMEOUT, 5000).
-define(WAIT_FOR_OFFNET_BRIDGE, 60000). %% 1 minute

start_link(RouteReqJObj) ->
    proc_lib:start_link(?MODULE, init, [self(), RouteReqJObj]).

init(Parent, RouteReqJObj) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    CallID = wh_json:get_value(<<"Call-ID">>, RouteReqJObj),
    put(callid, CallID),
    start_amqp(#state{aleg_callid=CallID, route_req_jobj=RouteReqJObj}).

start_amqp(#state{route_req_jobj=JObj}=State) ->
    Q = amqp_util:new_queue(),

    %% Bind the queue to an exchange
    _ = amqp_util:bind_q_to_targeted(Q),
    amqp_util:basic_consume(Q, [{exclusive, false}]),

    endpoint_data(State#state{my_q=Q}, JObj).

endpoint_data(State, JObj) ->
    {endpoint, EP} = endpoint_data(JObj),
    send_park(State#state{endpoint=EP, acctid = wh_json:get_value(<<"Auth-User">>, EP)}).

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

wait_for_win(#state{aleg_callid=CallID, my_q=Q}=State, Timeout) ->
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
	    ?LOG("Timed out(~b) waiting for route_win", [Timeout]),
	    _ = amqp_util:bind_q_to_callevt(Q, CallID),
	    _ = amqp_util:bind_q_to_callevt(Q, CallID, cdr),
	    wait_for_bridge(State, ?WAIT_FOR_BRIDGE_TIMEOUT)
    end.

bridge_to_endpoint(#state{callctl_q=CtlQ, aleg_callid=CallID, endpoint=EP}=State) ->
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
    wait_for_bridge(State#state{failover=wh_json:get_value(<<"Failover">>, EP, ?EMPTY_JSON_OBJECT)}, ?WAIT_FOR_BRIDGE_TIMEOUT).

wait_for_bridge(#state{aleg_callid=ALeg, acctid=AcctID, my_q=Q}=State, Timeout) ->
    Start = erlang:now(),
    receive
	{_, #amqp_msg{payload=Payload}} ->
	    JObj = mochijson2:decode(Payload),
	    true = whistle_api:call_event_v(JObj),

	    case { wh_json:get_value(<<"Application-Name">>, JObj)
		   ,wh_json:get_value(<<"Event-Name">>, JObj)
		   ,wh_json:get_value(<<"Event-Category">>, JObj) } of
		{ _, <<"CHANNEL_BRIDGE">>, <<"call_event">> } ->
		    BLeg = wh_json:get_value(<<"Other-Leg-Call-Id">>, JObj),
		    _ = amqp_util:bind_q_to_callevt(Q, BLeg, cdr),
		    ?LOG("Bridge to ~s successful", [BLeg]),
		    wait_for_cdr(State#state{bleg_callid=BLeg});
		{ <<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
		    case wh_json:get_value(<<"Application-Response">>, JObj) of
			<<"SUCCESS">> ->
			    BLeg = wh_json:get_value(<<"Other-Leg-Call-Id">>, JObj),
			    _ = amqp_util:bind_q_to_callevt(Q, BLeg, cdr),
			    ?LOG("Bridge to ~s successful", [BLeg]),
			    wait_for_cdr(State#state{bleg_callid=BLeg});
			Cause ->
			    ?LOG("Failed to bridge: ~s", [Cause]),
			    try_failover(State)
		    end;
		{ _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
		    ts_acctmgr:release_trunk(AcctID, ALeg, 0),
		    ?LOG("Channel hungup");
		{ _, _, <<"error">> } ->
		    ts_acctmgr:release_trunk(AcctID, ALeg, 0),
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
    wait_for_cdr(State, ?WAIT_FOR_HANGUP_TIMEOUT).
wait_for_cdr(#state{aleg_callid=ALeg, acctid=AcctID}=State, Timeout) ->
    receive
	{_, #amqp_msg{payload=Payload}} ->
	    JObj = mochijson2:decode(Payload),
            case { wh_json:get_value(<<"Event-Category">>, JObj)
		   ,wh_json:get_value(<<"Event-Name">>, JObj) } of
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
		    ?LOG("Hangup received, waiting on CDR"),
		    wait_for_cdr(State, ?WAIT_FOR_CDR_TIMEOUT);
                { <<"error">>, _ } ->
		    ?LOG("Received error in event stream, waiting for CDR"),
		    wait_for_cdr(State, ?WAIT_FOR_CDR_TIMEOUT);
		{ <<"cdr">>, <<"call_detail">> } ->
		    true = whistle_api:call_cdr_v(JObj),
		    Leg = wh_json:get_value(<<"Call-ID">>, JObj),
		    Duration = ts_util:get_call_duration(JObj),

		    {R, RI, RM, S} = ts_util:get_rate_factors(JObj),
		    Cost = ts_util:calculate_cost(R, RI, RM, S, Duration),

		    ?LOG("CDR received for leg ~s", [Leg]),
		    ?LOG("Leg to be billed for ~b seconds", [Duration]),
		    ?LOG("Acct ~s to be charged ~p if per_min", [AcctID, Cost]),

		    ts_acctmgr:release_trunk(AcctID, Leg, Cost),

		    wait_for_cdr(State, ?WAIT_FOR_CDR_TIMEOUT);
                _ ->
                    wait_for_cdr(State, ?WAIT_FOR_HANGUP_TIMEOUT)
            end
    after Timeout ->
	    ?LOG("Timed out(~b) waiting for CDR"),
	    %% will fail if already released
	    ts_acctmgr:release_trunk(AcctID, ALeg, 0)
    end.

try_failover(#state{failover=?EMPTY_JSON_OBJECT, aleg_callid=CallID, acctid=AcctID}) ->
    ?LOG_END("No failover configured, ending"),
    ts_acctmgr:release_trunk(AcctID, CallID, 0);
try_failover(#state{failover=FailJObj}=State) ->
    case wh_json:get_value(<<"e164">>, FailJObj) of
	undefined -> try_failover_sip(State, wh_json:get_value(<<"sip">>, FailJObj));
	DID -> try_failover_e164(State, DID)
    end.

try_failover_sip(#state{callctl_q = <<>>}, _) ->
    ?LOG("No control queue to try SIP failover");
try_failover_sip(#state{endpoint=EP, aleg_callid=CallID, callctl_q=CtlQ}=State, SIPUri) ->
    EndPoint = {struct, [
			 {<<"Invite-Format">>, <<"route">>}
			 ,{<<"Route">>, SIPUri}
			]},

    Command = [
	       {<<"Call-ID">>, CallID}
	       ,{<<"Application-Name">>, <<"bridge">>}
	       ,{<<"Endpoints">>, [EndPoint]}
	       ,{<<"Timeout">>, 6}
	       ,{<<"Bypass-Media">>, true}
	       ,{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, EP)}
	       ,{<<"Outgoing-Caller-ID-Number">>, wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, EP)}
	       ,{<<"Outgoing-Callee-ID-Name">>, wh_json:get_value(<<"Outgoing-Callee-ID-Name">>, EP)}
	       ,{<<"Outgoing-Callee-ID-Number">>, wh_json:get_value(<<"Outgoing-Callee-ID-Number">>, EP)}
	      ],

    {ok, Payload} = whistle_api:bridge_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),

    ?LOG("Sending SIP failover for ~s: ~s", [SIPUri, Payload]),

    amqp_util:targeted_publish(CtlQ, Payload),
    wait_for_bridge(State#state{failover=?EMPTY_JSON_OBJECT}, ?WAIT_FOR_BRIDGE_TIMEOUT).

try_failover_e164(#state{callctl_q = <<>>}, _) ->
    ?LOG("No control queue to try E.164 failover");
try_failover_e164(#state{acctid=AcctID, aleg_callid=CallID, callctl_q=CallctlQ, my_q=Q, endpoint=EP}=State, ToDID) ->
    FailCallID = <<CallID/binary, "-failover">>,
    {ok, RateData} = ts_credit:reserve(ToDID, FailCallID, AcctID, outbound, wh_json:get_value(<<"Route-Options">>, EP)),
    Command = [
	       {<<"Call-ID">>, CallID}
	       ,{<<"Resource-Type">>, <<"audio">>}
	       ,{<<"To-DID">>, ToDID}
	       ,{<<"Account-ID">>, AcctID}
	       ,{<<"Control-Queue">>, CallctlQ}
	       ,{<<"Application-Name">>, <<"bridge">>}
	       ,{<<"Custom-Channel-Vars">>, {struct, RateData}}
	       ,{<<"Flags">>, wh_json:get_value(<<"flags">>, EP)}
	       ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, EP)}
	       ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, EP)}
	       ,{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, EP)}
	       ,{<<"Outgoing-Caller-ID-Number">>, wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, EP)}
	       ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, EP)}
	       | whistle_api:default_headers(Q, <<"resource">>, <<"offnet_req">>, ?APP_NAME, ?APP_VERSION)
	      ],
    {ok, Payload} = whistle_api:offnet_resource_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    amqp_util:offnet_resource_publish(Payload),
    wait_for_offnet_bridge(State#state{aleg_callid=FailCallID}, ?WAIT_FOR_OFFNET_BRIDGE).

wait_for_offnet_bridge(#state{aleg_callid=CallID, acctid=AcctID, my_q=Q}=State, Timeout) ->
    Start = erlang:now(),
    receive
	{_, #amqp_msg{payload=Payload}} ->
	    JObj = mochijson2:decode(Payload),
	    case { wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Event-Category">>, JObj) } of
                { <<"offnet_resp">>, <<"resource">> } ->
		    BLegCallID = wh_json:get_value(<<"Call-ID">>, JObj),
		    amqp_util:bind_q_to_callevt(Q, BLegCallID, cdr),
		    wait_for_offnet_cdr(State#state{bleg_callid=BLegCallID}, ?WAIT_FOR_HANGUP_TIMEOUT);
                { <<"resource_error">>, <<"resource">> } ->
		    ?LOG("Failed to failover to e164"),
		    ?LOG("Failure message: ~s", [wh_json:get_value(<<"Failure-Message">>, JObj)]),
		    ?LOG("Failure code: ~s", [wh_json:get_value(<<"Failure-Code">>, JObj)]),

		    %% TODO: Send Commands to CtlQ to play media depending on failure code

		    ts_acctmgr:release_trunk(AcctID, CallID, 0);
                { <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
		    ?LOG("Hangup received"),
		    ts_acctmgr:release_trunk(AcctID, CallID, 0);
                { _, <<"error">> } ->
		    ?LOG("Error received"),
		    ts_acctmgr:release_trunk(AcctID, CallID, 0);
                _ ->
		    Diff = Timeout - (timer:now_diff(erlang:now(), Start) div 1000),
                    wait_for_offnet_bridge(State, Diff)
            end;
        _ ->
            Diff = Timeout - (timer:now_diff(erlang:now(), Start) div 1000),
            wait_for_offnet_bridge(State, Diff)
    after Timeout ->
	    ?LOG("Offnet bridge timed out(~b)", [Timeout]),
	    ts_acctmgr:release_trunk(AcctID, CallID, 0)
    end.

wait_for_offnet_cdr(#state{aleg_callid=ALeg, bleg_callid=BLeg, acctid=AcctID}=State, Timeout) ->
    receive
	{_, #amqp_msg{payload=Payload}} ->
	    JObj = mochijson2:decode(Payload),
            case { wh_json:get_value(<<"Event-Category">>, JObj)
		   ,wh_json:get_value(<<"Event-Name">>, JObj) } of
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
		    ?LOG("Hangup received, waiting on CDR"),
		    wait_for_cdr(State, ?WAIT_FOR_CDR_TIMEOUT);
                { <<"error">>, _ } ->
		    ?LOG("Received error in event stream, waiting for CDR"),
		    wait_for_cdr(State, ?WAIT_FOR_CDR_TIMEOUT);
		{ <<"cdr">>, <<"call_detail">> } ->
		    true = whistle_api:call_cdr_v(JObj),

		    Leg = wh_json:get_value(<<"Call-ID">>, JObj),
		    Duration = ts_util:get_call_duration(JObj),

		    {R, RI, RM, S} = ts_util:get_rate_factors(JObj),
		    Cost = ts_util:calculate_cost(R, RI, RM, S, Duration),

		    ?LOG("CDR received for leg ~s", [Leg]),
		    ?LOG("Leg to be billed for ~b seconds", [Duration]),
		    ?LOG("Acct ~s to be charged ~p if per_min", [AcctID, Cost]),

		    case Leg =:= BLeg of
			true -> ts_acctmgr:release_trunk(AcctID, Leg, Cost);
			false -> ts_acctmgr:release_trunk(AcctID, ALeg, Cost)
		    end,

		    wait_for_cdr(State, ?WAIT_FOR_CDR_TIMEOUT);
                _ ->
                    wait_for_cdr(State, ?WAIT_FOR_HANGUP_TIMEOUT)
            end
    after Timeout ->
	    ?LOG("Timed out(~b) waiting for CDR"),
	    %% will fail if already released
	    ts_acctmgr:release_trunk(AcctID, ALeg, 0)
    end.

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
    Invite = ts_util:invite_format(whistle_util:binary_to_lower(InFormat), ToDID) ++ InviteBase,

    
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
    {ok, Settings} = ts_util:lookup_did(ToDID),

    AuthOpts = wh_json:get_value(<<"auth">>, Settings, ?EMPTY_JSON_OBJECT),
    Acct = wh_json:get_value(<<"account">>, Settings, ?EMPTY_JSON_OBJECT),
    DidOptions = wh_json:get_value(<<"DID_Opts">>, Settings, ?EMPTY_JSON_OBJECT),
    RouteOpts = wh_json:get_value(<<"options">>, DidOptions, []),

    AuthU = wh_json:get_value(<<"auth_user">>, AuthOpts),
    AuthR = wh_json:get_value(<<"auth_realm">>, AuthOpts, wh_json:get_value(<<"auth_realm">>, Acct)),

    {Srv, Acct} = try
                      {ok, AccountSettings} = ts_util:lookup_user_flags(AuthU, AuthR),
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
