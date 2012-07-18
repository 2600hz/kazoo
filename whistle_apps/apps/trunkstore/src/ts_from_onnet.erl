%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Calls coming from known clients, getting settings for caller-id and
%%% what not, and sending the calls offnet.
%%%
%%% If the call is destined for a known client, the offnet whapp (probably
%%% stepswitch) will redirect the request back in, to be picked up by
%%% ts_from_offnet.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_from_onnet).

-export([start_link/1, init/2]).

-include("ts.hrl").

start_link(RouteReqJObj) ->
    proc_lib:start_link(?MODULE, init, [self(), RouteReqJObj]).

init(Parent, RouteReqJObj) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    start_amqp(ts_callflow:init(RouteReqJObj)).

start_amqp({error, not_ts_account}) -> ok;
start_amqp(State) ->
    onnet_data(ts_callflow:start_amqp(State)).

onnet_data(State) ->
    JObj = ts_callflow:get_request_data(State),

    {ToUser, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"outbound_user_field">>),
    ToDID = wnm_util:to_e164(ToUser),

    CallID = ts_callflow:get_aleg_id(State),
    AcctID = ts_callflow:get_account_id(State),

    FromUser = wh_json:get_value(<<"Caller-ID-Name">>, JObj),

    lager:debug("on-net request from ~s(~s) to ~s", [FromUser, AcctID, ToDID]),

    Options = case ts_util:lookup_did(FromUser, AcctID) of
                  {ok, Opts} -> Opts;
                  _ -> wh_json:new()
              end,

    DIDOptions = wh_json:get_value(<<"DID_Opts">>, Options, wh_json:new()),
    AcctOptions = wh_json:get_value(<<"account">>, Options, wh_json:new()),
    SrvOptions = wh_json:get_value([<<"server">>, <<"options">>], Options, wh_json:new()),

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

    EmergencyCallerID = case ts_util:caller_id([
                                                wh_json:get_value(<<"emergency_caller_id">>, DIDOptions)
                                                ,wh_json:get_value(<<"emergency_caller_id">>, SrvOptions)
                                                ,wh_json:get_value(<<"emergency_caller_id">>, AcctOptions)
                                               ]) of
                            {undefined, undefined} -> [];
                            {ECIDName, ECIDNum} ->
                                [{<<"Emergency-Caller-ID-Name">>, ECIDName}
                                 ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
                                 ]
                        end,
    CallerID = case ts_util:caller_id([
                                       wh_json:get_value(<<"caller_id">>, DIDOptions)
                                       ,wh_json:get_value(<<"caller_id">>, SrvOptions)
                                       ,wh_json:get_value(<<"caller_id">>, AcctOptions)
                                      ]) of
                   {undefined, undefined} -> EmergencyCallerID;
                   {CIDName, CIDNum} ->
                       [{<<"Outgoing-Caller-ID-Name">>, CIDName}
                        ,{<<"Outgoing-Caller-ID-Number">>, CIDNum}
                        | EmergencyCallerID
                       ]
               end,

    DIDFlags = ts_util:offnet_flags([wh_json:get_value(<<"DID_Opts">>, DIDOptions)
                                     ,wh_json:get_value(<<"flags">>, SrvOptions)
                                     ,wh_json:get_value(<<"flags">>, AcctOptions)
                                    ]),

    Q = ts_callflow:get_my_queue(State),

    Command = [ KV
                || {_,V}=KV <- CallerID ++ EmergencyCallerID ++
                       [
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
                        ,{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"Inception">>, <<"on-net">>}])}
                        | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                       ],
                   V =/= undefined,
                   V =/= <<>>
              ],

    try
        send_park(State, Command)
    catch
        _A:_B ->
            ST = erlang:get_stacktrace(),
            lager:debug("exception ~p:~p", [_A, _B]),
            _ = [lager:debug("st: ~p", [S]) || S <- ST],
            wait_for_cdr(State)
    end.

send_park(State, Command) ->
    State1 = ts_callflow:send_park(State),
    wait_for_win(State1, Command).

wait_for_win(State, Command) ->
    case ts_callflow:wait_for_win(State) of
        {won, State1} ->
            lager:debug("route won, sending offnet resource request"),
            send_offnet(State1, Command);
        {lost, State2} ->
            lager:debug("did not win route, passive listening"),
            wait_for_bridge(State2)
    end.

send_offnet(State, Command) ->
    lager:debug("sending offnet request"),
    CtlQ = ts_callflow:get_control_queue(State),
    wapi_offnet_resource:publish_req([{<<"Control-Queue">>, CtlQ} | Command]),
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
        {cdr, aleg, _CDR, State1} ->
            AcctID = ts_callflow:get_account_id(State1),
            Cost = ts_callflow:get_call_cost(State1),

            lager:debug("a-leg CDR for ~s costs ~p", [AcctID, Cost]),

            wait_for_other_leg(State1, bleg);
        {cdr, bleg, _CDR, State2} ->
            BLeg = ts_callflow:get_bleg_id(State2),
            AcctID = ts_callflow:get_account_id(State2),
            Cost = ts_callflow:get_call_cost(State2),

            lager:debug("b-leg ~s CDR for ~s costs ~p", [BLeg, AcctID, Cost]),

            wait_for_other_leg(State2, aleg);
        {timeout, State3} ->
            lager:debug("timed out waiting for CDRs, cleaning up"),

            ALeg = ts_callflow:get_aleg_id(State3),
            ts_callflow:finish_leg(State3, ALeg)
    end.

wait_for_other_leg(State, WaitingOnLeg) ->
    wait_for_other_leg(State, WaitingOnLeg, ts_callflow:wait_for_cdr(State)).

wait_for_other_leg(_State, aleg, {cdr, aleg, _CDR, State1}) ->
    ts_callflow:finish_leg(State1, ts_callflow:get_aleg_id(State1));
wait_for_other_leg(_State, bleg, {cdr, bleg, _CDR, State1}) ->
    ts_callflow:finish_leg(State1, ts_callflow:get_bleg_id(State1));
wait_for_other_leg(_State, Leg, {timeout, State1}) ->
    lager:debug("timed out waiting for ~s CDR, cleaning up", [Leg]),

    ALeg = ts_callflow:get_aleg_id(State1),
    ts_callflow:finish_leg(State1, ALeg).
