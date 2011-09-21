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
    ToDID = wh_util:to_e164(ToUser),

    CallID = ts_callflow:get_aleg_id(State),
    AcctID = ts_callflow:get_account_id(State),

    FromUser = wh_json:get_value(<<"Caller-ID-Name">>, JObj),

    ?LOG("on-net request from ~s(~s) to ~s", [FromUser, AcctID, ToDID]),

    Options = case ts_util:lookup_did(FromUser) of
		  {ok, Opts} -> Opts;
		  _ -> ?EMPTY_JSON_OBJECT
	      end,

    DIDOptions = wh_json:get_value(<<"DID_Opts">>, Options, ?EMPTY_JSON_OBJECT),
    AcctOptions = wh_json:get_value(<<"account">>, Options, ?EMPTY_JSON_OBJECT),
    SrvOptions = wh_json:get_value([<<"server">>, <<"options">>], Options, ?EMPTY_JSON_OBJECT),

    MediaHandling = ts_util:get_media_handling([
                                                wh_json:get_value(<<"media_handling">>, DIDOptions)
                                                ,wh_json:get_value(<<"media_handling">>, SrvOptions)
                                                ,wh_json:get_value(<<"media_handling">>, AcctOptions)
                                               ]),

    SIPHeaders = ts_util:sip_headers([
				      wh_json:get_value(<<"sip_headers">>, DIDOptions)
				      ,wh_json:get_value(<<"sip_headers">>, SrvOptions)
				      ,wh_json:get_value(<<"sip_headers">>, AcctOptions)
				     ]),

    DIDFlags = wh_json:get_value(<<"options">>, DIDOptions, []),

    case ts_credit:reserve(ToDID, CallID, AcctID, outbound, DIDFlags) of
        {error, _}=E ->
            ?LOG("release ~s for ~s", [CallID, AcctID]),
            ok = ts_acctmgr:release_trunk(AcctID, CallID, 0), E;
        {ok, RateData} ->
	    Q = ts_callflow:get_my_queue(State),

            Command = [
                       {<<"Call-ID">>, CallID}
                       ,{<<"Resource-Type">>, <<"audio">>}
                       ,{<<"To-DID">>, ToDID}
                       ,{<<"Account-ID">>, AcctID}
                       ,{<<"Application-Name">>, <<"bridge">>}
                       ,{<<"Flags">>, DIDFlags}
                       ,{<<"Media">>, MediaHandling}
                       ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, DIDOptions)}
                       ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, DIDOptions)}
                       ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, DIDOptions)}
                       ,{<<"SIP-Headers">>, SIPHeaders}
                       ,{<<"Custom-Channel-Vars">>, {struct, [{<<"Inception">>, <<"on-net">>}
							      | RateData]}}
                       | wh_api:default_headers(Q, <<"resource">>, <<"offnet_req">>, ?APP_NAME, ?APP_VERSION)
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
	    ?LOG("route won, sending offnet resource request"),
	    send_offnet(State1, Command);
	{lost, State2} ->
	    ?LOG("did not win route, passive listening"),
	    wait_for_bridge(State2)
    end.

send_offnet(State, Command) ->
    CtlQ = ts_callflow:get_control_queue(State),
    {ok, Payload} = wh_api:offnet_resource_req({struct, [ KV || {_, V}=KV <- [{<<"Control-Queue">>, CtlQ} | Command], V =/= undefined ]}),
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
	    ALeg = ts_callflow:get_aleg_id(State3),
	    ts_callflow:finish_leg(State3, ALeg);
	{timeout, State4} ->
	    ALeg = ts_callflow:get_aleg_id(State4),
	    ts_callflow:finish_leg(State4, ALeg)
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
	    ?LOG("timed out waiting for CDRs, cleaning up"),

	    ALeg = ts_callflow:get_aleg_id(State3),
	    ts_callflow:finish_leg(State3, ALeg)
    end.

wait_for_other_leg(State, WaitingOnLeg) ->
    wait_for_other_leg(State, WaitingOnLeg, ts_callflow:wait_for_cdr(State)).

wait_for_other_leg(_State, aleg, {cdr, aleg, CDR, State1}) ->
    ALeg = ts_callflow:get_aleg_id(State1),
    _ = ts_cdr:store(wh_json:set_value(<<"A-Leg">>, ALeg, CDR)),
    ts_callflow:finish_leg(State1, ALeg);
wait_for_other_leg(_State, bleg, {cdr, bleg, CDR, State1}) ->
    BLeg = ts_callflow:get_bleg_id(State1),
    _ = ts_cdr:store(wh_json:set_value(<<"B-Leg">>, BLeg, CDR)),
    ts_callflow:finish_leg(State1, BLeg);
wait_for_other_leg(_State, Leg, {timeout, State1}) ->
    ?LOG("timed out waiting for ~s CDR, cleaning up", [Leg]),

    ALeg = ts_callflow:get_aleg_id(State1),
    ts_callflow:finish_leg(State1, ALeg).
