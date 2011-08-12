%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Common functionality for onnet and offnet call handling
%%% @end
%%% Created : 30 Jun 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_callflow).

-export([init/1, start_amqp/1, send_park/1, wait_for_win/1
	 ,wait_for_bridge/1, wait_for_cdr/1, send_hangup/1
	 ,finish_leg/2
	]).

%% data access functions
-export([get_request_data/1, get_my_queue/1, get_control_queue/1, set_endpoint_data/2
	 ,set_account_id/2, get_aleg_id/1, get_bleg_id/1, get_call_cost/1
	 ,set_failover/2, get_failover/1, get_endpoint_data/1, get_account_id/1
	]).

-include("ts.hrl").

-define(WAIT_FOR_WIN_TIMEOUT, 5000).
-define(WAIT_FOR_BRIDGE_TIMEOUT, 10000).
-define(WAIT_FOR_HANGUP_TIMEOUT, 1000 * 60 * 60 * 1). %% 1 hour
-define(WAIT_FOR_CDR_TIMEOUT, 5000).

-record(state, {
	  aleg_callid = <<>> :: binary()
	  ,bleg_callid = <<>> :: binary()
          ,acctid = <<>> :: binary()
	  ,route_req_jobj = ?EMPTY_JSON_OBJECT :: json_object()
          ,ep_data = ?EMPTY_JSON_OBJECT :: json_object() %% data for the endpoint, either an actual endpoint or an offnet request
          ,my_q = <<>> :: binary()
          ,callctl_q = <<>> :: binary()
	  ,call_cost = 0.0 :: float()
          ,failover = ?EMPTY_JSON_OBJECT :: json_object()
	 }).

-spec init/1 :: (RouteReqJObj) -> #state{} when
      RouteReqJObj :: json_object().
init(RouteReqJObj) ->
    CallID = wh_json:get_value(<<"Call-ID">>, RouteReqJObj),
    put(callid, CallID),
    ?LOG("Init done"),
    #state{aleg_callid=CallID, route_req_jobj=RouteReqJObj, acctid=wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], RouteReqJObj)}.

-spec start_amqp/1 :: (State) -> #state{} when
      State :: #state{}.
start_amqp(#state{}=State) ->
    Q = amqp_util:new_queue(),

    %% Bind the queue to an exchange
    _ = amqp_util:bind_q_to_targeted(Q),
    amqp_util:basic_consume(Q, [{exclusive, false}]),

    ?LOG("Started AMQP with queue ~s", [Q]),
    State#state{my_q=Q}.

-spec send_park/1 :: (State) -> #state{} when
      State :: #state{}.
send_park(#state{aleg_callid=CallID, my_q=Q, route_req_jobj=JObj}=State) ->
    JObj1 = {struct, [ {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                       ,{<<"Routes">>, []}
                       ,{<<"Method">>, <<"park">>}
		       | wh_api:default_headers(Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION) ]
	    },
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    {ok, JSON} = wh_api:route_resp(JObj1),
    ?LOG("Sending park to ~s: ~s", [RespQ, JSON]),
    amqp_util:targeted_publish(RespQ, JSON, <<"application/json">>),

    _ = amqp_util:bind_q_to_callevt(Q, CallID),
    _ = amqp_util:bind_q_to_callevt(Q, CallID, cdr),
    amqp_util:basic_consume(Q), %% need to verify if this step is needed
    State.

-spec wait_for_win/1 :: (State) -> tuple(won | lost, #state{}) when
      State :: #state{}.
wait_for_win(#state{aleg_callid=CallID}=State) ->
    receive
	#'basic.consume_ok'{} -> wait_for_win(State);

	%% call events come from callevt exchange, ignore for now
	{#'basic.deliver'{exchange = <<"targeted">>}, #amqp_msg{payload=Payload}} ->
	    WinJObj = mochijson2:decode(Payload),
	    true = wh_api:route_win_v(WinJObj),
	    CallID = wh_json:get_value(<<"Call-ID">>, WinJObj),

	    CallctlQ = wh_json:get_value(<<"Control-Queue">>, WinJObj),

	    {won, State#state{callctl_q=CallctlQ}}
    after ?WAIT_FOR_WIN_TIMEOUT ->
	    ?LOG("Timed out(~b) waiting for route_win", [?WAIT_FOR_WIN_TIMEOUT]),
	    {lost, State}
    end.

-spec wait_for_bridge/1 :: (State) -> tuple(bridged | error | hangup | timeout, #state{}) when
      State :: #state{}.
-spec wait_for_bridge/2 :: (State, Timeout) -> tuple(bridged | error | hangup | timeout, #state{}) when
      State :: #state{},
      Timeout :: integer().
wait_for_bridge(State) ->
    wait_for_bridge(State, ?WAIT_FOR_BRIDGE_TIMEOUT).
wait_for_bridge(State, Timeout) ->
    Start = erlang:now(),
    receive
	{_, #amqp_msg{payload=Payload}} ->
	    JObj = mochijson2:decode(Payload),
	    case process_event_for_bridge(State, JObj) of
		ignore ->
		    wait_for_bridge(State, Timeout - (timer:now_diff(erlang:now(), Start) div 1000));
		{bridged, _}=Success -> Success;
		{error, _}=Error -> Error;
		{hangup, _}=Hangup -> Hangup
	    end;
	_E ->
	    ?LOG("Unexpected msg: ~p", [_E]),
	    wait_for_bridge(State, Timeout - (timer:now_diff(erlang:now(), Start) div 1000))
    after Timeout ->
	    ?LOG("Timeout waiting for bridge"),
	    {timeout, State}
    end.

-spec process_event_for_bridge/2 :: (State, JObj) -> ignore | tuple(bridged | error | hangup, #state{}) when
      State :: #state{},
      JObj :: json_object().
process_event_for_bridge(#state{aleg_callid=ALeg, my_q=Q, callctl_q=CtlQ}=State, JObj) ->
    case { wh_json:get_value(<<"Application-Name">>, JObj)
	   ,wh_json:get_value(<<"Event-Name">>, JObj)
	   ,wh_json:get_value(<<"Event-Category">>, JObj) } of

	{_, <<"offnet_resp">>, <<"resource">>} ->
	    BLeg = wh_json:get_value(<<"Call-ID">>, JObj),
	    ?LOG("Bridged to ~s successful", [BLeg]),
	    ?LOG(BLeg, "Bridged from ~s successful", [ALeg]),

	    _ = amqp_util:bind_q_to_callevt(Q, BLeg, cdr),
	    amqp_util:basic_consume(Q),
	    {bridged, State#state{bleg_callid=BLeg}};

	{ _, <<"CHANNEL_BRIDGE">>, <<"call_event">> } ->
	    BLeg = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
	    ?LOG("Bridged to ~s successful", [BLeg]),
	    ?LOG(BLeg, "Bridged from ~s successful", [ALeg]),

	    _ = amqp_util:bind_q_to_callevt(Q, BLeg, cdr),
	    amqp_util:basic_consume(Q),
	    {bridged, State#state{bleg_callid=BLeg}};

	{ <<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
	    ?LOG("Bridge event received"),
	    case wh_json:get_value(<<"Application-Response">>, JObj) of
		<<"SUCCESS">> ->
		    ?LOG("Bridge event successful, waiting for CHANNEL_BRIDGE"),
		    ignore;
		Cause ->
		    ?LOG("Failed to bridge: ~s", [Cause]),
		    {error, State}
	    end;

	{ _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
	    ?LOG("Channel hungup before bridge"),
	    {hangup, State};

	{ _, _, <<"error">> } ->
	    ?LOG("Execution failed"),
	    {error, State};

	{ _, <<"resource_error">>, <<"resource">> } ->
	    Code = wh_json:get_value(<<"Failure-Code">>, JObj, <<"486">>),
	    Message = wh_json:get_value(<<"Failure-Message">>, JObj),

	    ?LOG("Failed to bridge to offnet"),
	    ?LOG("Failure message: ~s", [Message]),
	    ?LOG("Failure code: ~s", [Code]),

	    %% send failure code to Call
	    wh_util:call_response(ALeg, CtlQ, Code, Message),

	    {hangup, State};
	_Unhandled ->
	    ?LOG("Unhandled combo: ~p", [_Unhandled]),
	    ignore
    end.

-spec wait_for_cdr/1 :: (State) -> {timeout, #state{}} | {cdr, aleg | bleg, json_object(), #state{}} when
      State :: #state{}.
-spec wait_for_cdr/2 :: (State, Timeout) -> {timeout, #state{}} | {cdr, aleg | bleg, json_object(), #state{}} when
      State :: #state{},
      Timeout :: integer().
wait_for_cdr(State) ->
    wait_for_cdr(State, ?WAIT_FOR_HANGUP_TIMEOUT).
wait_for_cdr(State, Timeout) ->
    receive
	{_, #amqp_msg{payload=Payload}} ->
	    JObj = mochijson2:decode(Payload),
	    case process_event_for_cdr(State, JObj) of
		{cdr, _, _, _}=CDR -> CDR;
		{hangup, State1} -> wait_for_cdr(State1, ?WAIT_FOR_CDR_TIMEOUT);
		ignore -> wait_for_cdr(State, Timeout)
	    end
    after Timeout ->
	    {timeout, State}
    end.

process_event_for_cdr(#state{aleg_callid=ALeg, acctid=AcctID}=State, JObj) ->
    case { wh_json:get_value(<<"Event-Category">>, JObj)
	   ,wh_json:get_value(<<"Event-Name">>, JObj) } of
	{ <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
	    ?LOG("Hangup received, waiting on CDR"),
	    {hangup, State};

        { <<"call_event">>, <<"CHANNEL_UNBRIDGE">> } ->
            ?LOG("Unbridge received, waiting on CDR"),
            {hangup, State};

	{ <<"error">>, _ } ->
	    ?LOG("Error received, waiting on CDR"),
	    {hangup, State};

	{ <<"call_detail">>, <<"cdr">> } ->
	    true = wh_api:call_cdr_v(JObj),
	    Leg = wh_json:get_value(<<"Call-ID">>, JObj),
	    Duration = ts_util:get_call_duration(JObj),

	    {R, RI, RM, S} = ts_util:get_rate_factors(JObj),
	    Cost = ts_util:calculate_cost(R, RI, RM, S, Duration),

	    ?LOG("CDR received for leg ~s", [Leg]),
	    ?LOG("Leg to be billed for ~b seconds", [Duration]),
	    ?LOG("Acct ~s to be charged ~p if per_min", [AcctID, Cost]),

	    case Leg =:= ALeg of
		true -> {cdr, aleg, JObj, State#state{call_cost=Cost}};
		false -> {cdr, bleg, JObj, State#state{call_cost=Cost}}
	    end;
	_E ->
	    ?LOG("Ignorable event: ~p", [_E]),
	    ignore
    end.

finish_leg(State, undefined) ->
    send_hangup(State);
finish_leg(#state{acctid=AcctID, call_cost=Cost}=State, Leg) ->
    ok = ts_acctmgr:release_trunk(AcctID, Leg, Cost),
    send_hangup(State).

-spec send_hangup/1 :: (State) -> ok when
      State :: #state{}.
send_hangup(#state{callctl_q = <<>>}) ->
    ok;
send_hangup(#state{callctl_q=CtlQ, my_q=Q, aleg_callid=CallID}) ->
    Command = [
	       {<<"Application-Name">>, <<"hangup">>}
	       ,{<<"Call-ID">>, CallID}
	       | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
	      ],
    {ok, JSON} = wh_api:hangup_req(Command),
    ?LOG("Sending hangup to ~s: ~s", [CtlQ, JSON]),
    amqp_util:targeted_publish(CtlQ, JSON, <<"application/json">>).

%%%-----------------------------------------------------------------------------
%%% Data access functions
%%%-----------------------------------------------------------------------------
-spec get_request_data/1 :: (State) -> json_object() when
      State :: #state{}.
get_request_data(#state{route_req_jobj=JObj}) ->
    JObj.

-spec set_endpoint_data/2 :: (State, Data) -> #state{} when
      State :: #state{},
      Data :: json_object().
set_endpoint_data(State, Data) ->
    State#state{ep_data=Data}.

-spec get_endpoint_data/1 :: (State) -> json_object() when
      State :: #state{}.
get_endpoint_data(#state{ep_data=EP}) ->
    EP.

-spec set_account_id/2 :: (State, ID) -> #state{} when
      State :: #state{},
      ID :: binary().
set_account_id(State, ID) ->
    State#state{acctid=ID}.

-spec get_account_id/1 :: (State) -> binary() when
      State :: #state{}.
get_account_id(#state{acctid=ID}) ->
    ID.

-spec get_my_queue/1 :: (State) -> binary() when
      State :: #state{}.
-spec get_control_queue/1 :: (State) -> binary() when
      State :: #state{}.
get_my_queue(#state{my_q=Q}) ->
    Q.
get_control_queue(#state{callctl_q=CtlQ}) ->
    CtlQ.

-spec get_aleg_id/1 :: (State) -> binary() when
      State :: #state{}.
-spec get_bleg_id/1 :: (State) -> binary() when
      State :: #state{}.
get_aleg_id(#state{aleg_callid=ALeg}) ->
    ALeg.
get_bleg_id(#state{bleg_callid=ALeg}) ->
    ALeg.

-spec get_call_cost/1 :: (State) -> float() when
      State :: #state{}.
get_call_cost(#state{call_cost=Cost}) ->
    Cost.

-spec set_failover/2 :: (State, Failover) -> #state{} when
      State :: #state{},
      Failover :: json_object().
set_failover(State, Failover) ->
    State#state{failover=Failover}.

-spec get_failover/1 :: (State) -> json_object() when
      State :: #state{}.
get_failover(#state{failover=Fail}) ->
    Fail.
