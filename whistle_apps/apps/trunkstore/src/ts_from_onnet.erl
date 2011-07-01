%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Calls coming from known clients, getting settings for caller-id and
%%% what not, and sending the calls offnet.
%%%
%%% If the call is destined for a known client, the offnet whapp (probably
%%% stepswitch) will redirect the request back in, to be picked up by
%%% ts_from_offnet.
%%% @end
%%% Created : 20 Jun 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_from_onnet).

-export([start_link/1, init/2]).

-include("ts.hrl").

start_link(RouteReqJObj) ->
    proc_lib:start_link(?MODULE, init, [self(), RouteReqJObj]).

init(Parent, RouteReqJObj) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    start_amqp(ts_callflow:init(RouteReqJObj)).

start_amqp(State) ->
    onnet_data(ts_callflow:start_amqp(State)).

onnet_data(State) ->
    JObj = ts_callflow:get_request_data(State),

    [ToUser, _ToDomain] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
    ToDID = whistle_util:to_e164(ToUser),

    CallID = ts_callflow:get_aleg_id(State),
    AcctID = ts_callflow:get_account_id(State),

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
	    Q = ts_callflow:get_my_queue(State),

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
                send_park(State, Command)
            catch
                _A:_B ->
                    ?LOG("Exception ~p:~p", [_A, _B]),
                    ?LOG_SYS("Stacktrace: ~p", [erlang:get_stacktrace()]),
                    wait_for_cdr(State)
            end
    end.

send_park(State, Command) ->
    State1 = ts_callflow:send_park(State),
    wait_for_win(State1, Command).

wait_for_win(State, Command) ->
    case ts_callflow:wait_for_win(State) of
	{won, State1} ->
	    ?LOG("Route won, sending command"),
	    send_offnet(State1, Command);
	{lost, State2} ->
	    ?LOG("Didn't win route, passive listening"),
	    wait_for_bridge(State2)
    end.

send_offnet(State, Command) ->
    CtlQ = ts_callflow:get_control_queue(State),
    {ok, Payload} = whistle_api:offnet_resource_req([ KV || {_, V}=KV <- [{<<"Control-Queue">>, CtlQ} | Command], V =/= undefined ]),
    ?LOG("Sending offnet: ~s", [Payload]),
    amqp_util:offnet_resource_publish(Payload),
    wait_for_bridge(State).

wait_for_bridge(State) ->
    case ts_callflow:wait_for_bridge(State) of
	{bridged, State1} ->
	    wait_for_cdr(State1);
	{error, State2} ->
	    wait_for_bridge(State2);
	{hangup, State3} ->
	    AcctID = ts_callflow:get_account_id(State3),
	    CallID = ts_callflow:get_aleg_id(State3),
	    ?LOG("Releasing ~s for ~s: costed 0", [AcctID, CallID]),
	    ok = ts_acctmgr:release_trunk(AcctID, CallID, 0),
	    ts_callflow:send_hangup(State3);
	{timeout, State4} ->
	    AcctID = ts_callflow:get_account_id(State4),
	    CallID = ts_callflow:get_aleg_id(State4),
	    ?LOG("Releasing ~s for ~s: costed 0", [AcctID, CallID]),
	    ok = ts_acctmgr:release_trunk(AcctID, CallID, 0),
	    ts_callflow:send_hangup(State4)
    end.

wait_for_cdr(State) ->
    case ts_callflow:wait_for_cdr(State) of
	{cdr, aleg, CDR, State1} ->
	    ALeg = ts_callflow:get_aleg_id(State1),
	    AcctID = ts_callflow:get_account_id(State1),
	    Cost = ts_callflow:get_call_cost(State1),

	    ?LOG("a-leg CDR for ~s costs ~p", [AcctID, Cost]),

	    _ = ts_cdr:store(wh_json:set_value(<<"A-Leg">>, ALeg, CDR)),
	    ok = ts_acctmgr:release_trunk(AcctID, ALeg, Cost),

	    wait_for_other_leg(State1, bleg);
	{cdr, bleg, CDR, State2} ->
	    BLeg = ts_callflow:get_bleg_id(State2),
	    AcctID = ts_callflow:get_account_id(State2),
	    Cost = ts_callflow:get_call_cost(State2),

	    ?LOG("b-leg CDR for ~s costs ~p", [AcctID, Cost]),
	    ?LOG(BLeg, "b-leg CDR for ~s costs ~p", [AcctID, Cost]),

	    _ = ts_cdr:store(wh_json:set_value(<<"B-Leg">>, BLeg, CDR)),
	    ok = ts_acctmgr:release_trunk(AcctID, BLeg, Cost),

	    wait_for_other_leg(State2, aleg);
	{timeout, State3} ->
	    ?LOG("Timed out waiting for CDRs, cleaning up"),

	    ALeg = ts_callflow:get_aleg_id(State3),
	    AcctID = ts_callflow:get_account_id(State3),
            Cost = ts_callflow:get_call_cost(State3),

	    ok = ts_acctmgr:release_trunk(AcctID, ALeg, Cost),
	    ts_callflow:send_hangup(State3)
    end.

wait_for_other_leg(State, WaitingOnLeg) ->
    wait_for_other_leg(State, WaitingOnLeg, ts_callflow:wait_for_cdr(State)).

wait_for_other_leg(_State, aleg, {cdr, aleg, CDR, State1}) ->
    ALeg = ts_callflow:get_aleg_id(State1),
    AcctID = ts_callflow:get_account_id(State1),
    Cost = ts_callflow:get_call_cost(State1),

    ?LOG("a-leg CDR for ~s costs ~p", [AcctID, Cost]),

    _ = ts_cdr:store(wh_json:set_value(<<"A-Leg">>, ALeg, CDR)),
    ok = ts_acctmgr:release_trunk(AcctID, ALeg, Cost),
    ts_callflow:send_hangup(State1);
wait_for_other_leg(_State, bleg, {cdr, bleg, CDR, State1}) ->
    BLeg = ts_callflow:get_bleg_id(State1),
    AcctID = ts_callflow:get_account_id(State1),
    Cost = ts_callflow:get_call_cost(State1),

    ?LOG("b-leg CDR for ~s costs ~p", [AcctID, Cost]),

    _ = ts_cdr:store(wh_json:set_value(<<"B-Leg">>, BLeg, CDR)),
    ok = ts_acctmgr:release_trunk(AcctID, BLeg, Cost),
    ts_callflow:send_hangup(State1);
wait_for_other_leg(_State, Leg, {timeout, State1}) ->
    ?LOG("Timed out waiting for ~s CDR, cleaning up", [Leg]),

    ALeg = ts_callflow:get_aleg_id(State1),
    AcctID = ts_callflow:get_account_id(State1),
    Cost = ts_callflow:get_call_cost(State1),

    ok = ts_acctmgr:release_trunk(AcctID, ALeg, Cost),
    ts_callflow:send_hangup(State1).
