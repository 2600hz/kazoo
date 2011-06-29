%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_from_onnet).

-export([start_link/1, init/2]).

-include("ts.hrl").

-record(state, {
	  aleg_callid = <<>> :: binary()
	  ,bleg_callid = <<>> :: binary()
          ,acctid = <<>> :: binary()
	  ,route_req_jobj = ?EMPTY_JSON_OBJECT :: json_object()
          ,onnet = ?EMPTY_JSON_OBJECT :: json_object()
          ,my_q = <<>> :: binary()
          ,callctl_q = <<>> :: binary()
          ,failover = ?EMPTY_JSON_OBJECT :: json_object()
	 }).

-define(APP_NAME, <<"ts_from_onnet">>).
-define(APP_VERSION, <<"0.0.5">>).
-define(WAIT_FOR_WIN_TIMEOUT, 5000).
-define(WAIT_FOR_OFFNET_RESPONSE_TIMEOUT, 60000).
-define(WAIT_FOR_BRIDGE_TIMEOUT, 10000).
-define(WAIT_FOR_HANGUP_TIMEOUT, 1000 * 60 * 60 * 2). %% 2 hours
-define(WAIT_FOR_CDR_TIMEOUT, 5000).

start_link(RouteReqJObj) ->
    proc_lib:start_link(?MODULE, init, [self(), RouteReqJObj]).

init(Parent, RouteReqJObj) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    CallID = wh_json:get_value(<<"Call-ID">>, RouteReqJObj),
    put(callid, CallID),
    ?LOG("Init done"),
    start_amqp(#state{aleg_callid=CallID, route_req_jobj=RouteReqJObj, acctid=wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], RouteReqJObj)}).

start_amqp(#state{route_req_jobj=JObj}=State) ->
    Q = amqp_util:new_queue(),

    %% Bind the queue to an exchange
    _ = amqp_util:bind_q_to_targeted(Q),
    amqp_util:basic_consume(Q, [{exclusive, false}]),

    ?LOG("Started AMQP with queue ~s", [Q]),
    onnet_data(State#state{my_q=Q}, JObj).

onnet_data(#state{aleg_callid=CallID, my_q=Q, acctid=AcctID}=State, JObj) ->
    [ToUser, _ToDomain] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
    ToDID = whistle_util:to_e164(ToUser),

    FromUser = wh_json:get_value(<<"Caller-ID-Name">>, JObj),

    ?LOG("From ~s(~s) to ~s", [FromUser, AcctID, ToDID]),

    DIDJObj = case ts_util:lookup_did(FromUser) of
		  {ok, DIDFlags} -> DIDFlags;
		  _ -> ?EMPTY_JSON_OBJECT
	      end,

    RouteOptions = wh_json:get_value(<<"options">>, DIDJObj, []),

    case ts_credit:reserve(ToDID, CallID, AcctID, outbound, RouteOptions) of
        {error, _}=E -> ?LOG("release ~s for ~s", [CallID, AcctID]), ok = ts_acctmgr:release_trunk(AcctID, CallID, 0), E;
        {ok, RateData} ->
            Command = [
                       {<<"Call-ID">>, CallID}
                       ,{<<"Resource-Type">>, <<"audio">>}
                       ,{<<"To-DID">>, ToDID}
                       ,{<<"Account-ID">>, AcctID}
                       ,{<<"Application-Name">>, <<"bridge">>}
                       ,{<<"Flags">>, RouteOptions}
                       ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, DIDJObj)}
                       ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, DIDJObj)}
                       %% ,{<<"Outgoing-Caller-ID-Name">>, CallerIDName}
                       %% ,{<<"Outgoing-Caller-ID-Number">>, CallerIDNum}
                       ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, DIDJObj)}
                       ,{<<"Custom-Channel-Vars">>, {struct, RateData}}
                       | whistle_api:default_headers(Q, <<"resource">>, <<"offnet_req">>, ?APP_NAME, ?APP_VERSION)
                      ],
            try
                send_park(State#state{acctid=AcctID}, Command)
            catch
                _A:_B ->
                    ?LOG("Exception connecting from onnet"),
                    ?LOG("~p:~p", [_A, _B]),
                    ?LOG("Stacktrace: ~p", [erlang:get_stacktrace()]),
                    ?LOG("Release ~s for ~s", [CallID, AcctID]),
                    ts_acctmgr:release_trunk(CallID, AcctID, 0)
            end
    end.

send_park(#state{route_req_jobj=JObj, my_q=Q}=State, Command) ->
    JObj1 = {struct, [ {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                       ,{<<"Routes">>, []}
                       ,{<<"Method">>, <<"park">>}
		       | whistle_api:default_headers(Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION) ]
	    },
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    {ok, JSON} = whistle_api:route_resp(JObj1),
    ?LOG("Sending park to ~s: ~s", [RespQ, JSON]),
    amqp_util:targeted_publish(RespQ, JSON, <<"application/json">>),

    wait_for_win(State, Command, ?WAIT_FOR_WIN_TIMEOUT).

wait_for_win(#state{aleg_callid=CallID, my_q=Q}=State, Command, Timeout) ->
    receive
        #'basic.consume_ok'{} -> wait_for_win(State, Command, Timeout);
	{_, #amqp_msg{payload=Payload}} ->
	    WinJObj = mochijson2:decode(Payload),
	    true = whistle_api:route_win_v(WinJObj),
	    CallID = wh_json:get_value(<<"Call-ID">>, WinJObj),

	    _ = amqp_util:bind_q_to_callevt(Q, CallID),
	    _ = amqp_util:bind_q_to_callevt(Q, CallID, cdr),

	    CallctlQ = wh_json:get_value(<<"Control-Queue">>, WinJObj),

	    send_offnet(State#state{callctl_q=CallctlQ}, [{<<"Control-Queue">>, CallctlQ} | Command])
    after Timeout ->
	    ?LOG("Timed out(~b) waiting for route_win", [Timeout]),
	    _ = amqp_util:bind_q_to_callevt(Q, CallID),
	    _ = amqp_util:bind_q_to_callevt(Q, CallID, cdr),
	    wait_for_bridge(State, ?WAIT_FOR_BRIDGE_TIMEOUT)
    end.

send_offnet(State, Command) ->
    {ok, Payload} = whistle_api:offnet_resource_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    ?LOG("Sending offnet: ~s", [Payload]),
    amqp_util:offnet_resource_publish(Payload),
    wait_for_offnet_bridge(State, ?WAIT_FOR_OFFNET_RESPONSE_TIMEOUT).

wait_for_offnet_bridge(#state{aleg_callid=CallID, acctid=AcctID, my_q=Q, callctl_q=CtlQ}=State, Timeout) ->
    Start = erlang:now(),
    receive
	{_, #amqp_msg{payload=Payload}} ->
	    JObj = mochijson2:decode(Payload),
	    case { wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Event-Category">>, JObj) } of
                { <<"offnet_resp">>, <<"resource">> } ->
		    BLegCallID = wh_json:get_value(<<"Call-ID">>, JObj),
		    _ = amqp_util:bind_q_to_callevt(Q, BLegCallID, cdr),
		    ?LOG("Bridging to offnet callid ~s", [BLegCallID]),
		    ?LOG(BLegCallID, "Bridged to aleg ~s", [CallID]),
		    wait_for_cdr(State#state{bleg_callid=BLegCallID}, ?WAIT_FOR_HANGUP_TIMEOUT);
                { <<"resource_error">>, <<"resource">> } ->
		    Code = wh_json:get_value(<<"Failure-Code">>, JObj, <<"486">>),
		    Message = wh_json:get_value(<<"Failure-Message">>, JObj),

		    ?LOG("Failed to bridge to offnet"),
		    ?LOG("Failure message: ~s", [Message]),
		    ?LOG("Failure code: ~s", [Code]),

		    %% send failure code to Call
		    whistle_util:call_response(CallID, CtlQ, Code, Message),

                    ?LOG("release ~s for ~s", [CallID, AcctID]),
		    ts_acctmgr:release_trunk(AcctID, CallID, 0);
                { <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
		    ?LOG("Hangup received"),
                    ?LOG("release ~s for ~s", [CallID, AcctID]),
		    ts_acctmgr:release_trunk(AcctID, CallID, 0);
                { _, <<"error">> } ->
		    ?LOG("Error received"),
                    ?LOG("release ~s for ~s", [CallID, AcctID]),
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
            ?LOG("release ~s for ~s", [CallID, AcctID]),
	    ts_acctmgr:release_trunk(AcctID, CallID, 0)
    end.

wait_for_cdr(#state{aleg_callid=ALeg, bleg_callid=BLeg, acctid=AcctID}=State, Timeout) ->
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

		    CDR0 = wh_json:set_value(<<"Call-Cost">>, Cost, JObj),
		    _ = case Leg =:= BLeg of
			    true ->
				?LOG(ALeg, "releasing b-leg ~s for ~s", [Leg, AcctID]),
				?LOG(BLeg, "release ~s for ~s", [Leg, AcctID]),
				ok = ts_acctmgr:release_trunk(AcctID, Leg, Cost),
				ts_cdr:store(wh_json:set_value(<<"B-Leg">>, BLeg, CDR0));
			    false ->
				?LOG(BLeg, "releasing a-leg ~s for ~s", [ALeg, AcctID]),
				?LOG(ALeg, "release ~s for ~s", [ALeg, AcctID]),
				ok = ts_acctmgr:release_trunk(AcctID, ALeg, Cost),
				ts_cdr:store(wh_json:set_value(<<"A-Leg">>, ALeg, CDR0))
			end,

		    wait_for_cdr(State, ?WAIT_FOR_CDR_TIMEOUT);
                _ ->
                    wait_for_cdr(State, ?WAIT_FOR_HANGUP_TIMEOUT)
            end
    after Timeout ->
	    ?LOG("Timed out(~b) waiting for CDR"),
	    %% will fail if already released
            ?LOG("release ~s for ~s", [ALeg, AcctID]),
	    ts_acctmgr:release_trunk(AcctID, ALeg, 0),
            ?LOG("release ~s for ~s", [BLeg, AcctID]),
	    ts_acctmgr:release_trunk(AcctID, BLeg, 0)
    end.

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
		    wait_for_cdr(State#state{bleg_callid=BLeg}, ?WAIT_FOR_HANGUP_TIMEOUT);
		{ <<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
		    case wh_json:get_value(<<"Application-Response">>, JObj) of
			<<"SUCCESS">> ->
			    BLeg = wh_json:get_value(<<"Other-Leg-Call-Id">>, JObj),
			    _ = amqp_util:bind_q_to_callevt(Q, BLeg, cdr),
			    ?LOG("Bridge to ~s successful", [BLeg]),
			    wait_for_cdr(State#state{bleg_callid=BLeg}, ?WAIT_FOR_HANGUP_TIMEOUT);
			Cause ->
			    ?LOG("Failed to bridge: ~s", [Cause]),
                            ?LOG("release ~s for ~s", [ALeg, AcctID]),
			    ok = ts_acctmgr:release_trunk(AcctID, ALeg, 0)
		    end;
		{ _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
                    ?LOG("release ~s for ~s", [ALeg, AcctID]),
		    ok = ts_acctmgr:release_trunk(AcctID, ALeg, 0),
		    ?LOG("Channel hungup");
		{ _, _, <<"error">> } ->
                    ?LOG("release ~s for ~s", [ALeg, AcctID]),
		    ok = ts_acctmgr:release_trunk(AcctID, ALeg, 0),
		    ?LOG("Execution failed");
		{_App, _, _Evt} ->
		    ?LOG("Received other: ~s: ~s", [_Evt, _App]),
		    Diff = Timeout - (timer:now_diff(erlang:now(), Start) div 1000),
		    ?LOG("~b left to timeout", [Diff]),
		    wait_for_bridge(State, Diff)
	    end
    after Timeout ->
	    ?LOG("Timed out(~b) waiting for bridge success", [Timeout]),
            ?LOG("release ~s for ~s", [ALeg, AcctID]),
            ts_acctmgr:release_trunk(AcctID, ALeg, 0)
    end.
