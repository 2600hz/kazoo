%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle offnet requests, including rating them
%%% @end
%%% Created : 15 Dec 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ss_offnet_req).

-export([init/0, handle_req/2]).

-include("stepswitch.hrl").

init() ->
    'ok'.

handle_req(JObj, Props) ->
    whapps_util:put_callid(JObj),
    true = wapi_offnet_resource:req_v(JObj),
    ?LOG_START("Valid offnet request"),

    CallID = get(callid),

    <<"audio">> = wh_json:get_value(<<"Resource-Type">>, JObj),
    ?LOG("Requested an audio resource"),

    Number = wh_json:get_value(<<"To-DID">>, JObj),

    ?LOG("off-net resource bridge request to ~s for account ~s", [Number, wh_json:get_value(<<"Account-ID">>, JObj)]),

    Q = amqp_util:new_queue(),
    amqp_util:basic_consume(Q),

    wapi_call:bind_q(Q, [{restrict_to, [events, cdr]}, {callid, CallID}]),
    wapi_self:bind_q(Q, []),

    _Pid = spawn(fun() -> request_rating(Q, JObj) end),
    ?LOG("Rate requested in ~p", [_Pid]),

    R1 = props:get_value(resources, Props),
    ?LOG("Resources available: ~p", [R1]),

    BridgeReq = case stepswitch_util:lookup_number(Number) of
                    {ok, AccountId, false} ->
			?LOG("number belongs to another on-net account, loopback back to account ~s", [AccountId]),
			stepswitch_util:build_loopback_request(JObj, Number, AccountId, Q);
		    {error, _} ->
			?LOG("failed to find number in another on-net account...to the cloud!"),
                        EPs = case wh_json:get_value(<<"Flags">>, JObj) of
                                  'undefined' -> stepswitch_util:evaluate_number(Number, R1);
                                  Flags ->
                                      _ = [?LOG("resource must have ~s flag", [F]) || F <- Flags],
                                      R2 = stepswitch_util:evaluate_flags(Flags, R1),
                                      stepswitch_util:evaluate_number(Number, R2)
                              end,
			?LOG("Found ~b endpoints", [length(EPs)]),
                        stepswitch_util:build_bridge_request(JObj, EPs, Q)
                end,

    case wh_json:get_value(<<"Endpoints">>, BridgeReq, []) of
        [] ->
            ?LOG_END("no offnet resources found for request, sending failure response"),
            whapps_util:alert(<<"alert">>, ["Source: ~s(~p)~n"
                                            ,"Alert: could not process ~s~n"
                                            ,"Fault: no offnet resources found for request~n"
                                            ,"Call-ID: ~s~n"]
                              ,[?MODULE, ?LINE, Number, CallID]),
            stepswitch_util:respond_resource_failed(wh_json:from_list([{<<"Hangup-Cause">>, <<"NO_RESOURCES">>}
								       ,{<<"Hangup-Code">>, <<"sip:404">>}
								      ]), 0, JObj);
        BridgeEPs ->
            {ok, Payload} = wapi_dialplan:bridge(BridgeReq),

	    CtlQueue = wh_json:get_value(<<"Control-Queue">>, JObj),

	    wapi_dialplan:publish_action(CtlQueue, Payload, ?DEFAULT_CONTENT_TYPE),

	    wait_for_bridge(Number, JObj, CtlQueue, length(BridgeEPs))
    end.

wait_for_bridge(Number, JObj, CtlQueue, EPLen) ->
    case stepswitch_util:wait_for_bridge(60000) of
	{ok, BridgeResp} ->
	    ?LOG_END("offnet resource request resulted in a successful bridge"),
	    stepswitch_util:respond_bridged_to_resource(BridgeResp, JObj);
	{fail, BridgeResp} ->
	    whapps_util:alert(<<"warning">>, ["Source: ~s(~p)~n"
					      ,"Alert: could not process ~s~n"
					      ,"Fault: ~p~n"
					      ,"Call-ID: ~s~n"]
			      ,[?MODULE, ?LINE, Number, BridgeResp, get(callid)]),
	    ?LOG_END("offnet resource failed, ~s:~s", [wh_json:get_value(<<"Hangup-Code">>, BridgeResp)
						       ,wh_json:get_value(<<"Application-Response">>, BridgeResp)
						      ]),
	    stepswitch_util:respond_resource_failed(BridgeResp, EPLen, JObj);
	{hungup, HangupResp} ->
	    ?LOG_END("requesting leg hungup, ~s:~s", [wh_json:get_value(<<"Hangup-Code">>, HangupResp)
						      ,wh_json:get_value(<<"Hangup-Cause">>, HangupResp)
						     ]);
	{error, timeout} ->
	    ?LOG_END("resource bridge request did not respond"),
	    whapps_util:alert(<<"error">>, ["Source: ~s(~p)~n"
					    ,"Alert: could not process ~s~n"
					    ,"Fault: timeout~n"
					    ,"Call-ID: ~s~n"]
			      ,[?MODULE, ?LINE, Number, get(callid)]),
	    stepswitch_util:respond_resource_failed(wh_json:set_value(<<"Failure-Message">>, <<"TIMEOUT">>, wh_json:new()), EPLen, JObj);
	{error, ErrorResp} ->
	    ?LOG_END("internal resource bridge error"),
	    whapps_util:alert(<<"error">>, ["Source: ~s(~p)~n"
					    ,"Alert: could not process ~s~n"
					    ,"Fault: ~p~n"
					    ,"Call-ID: ~s~n"]
			      ,[?MODULE, ?LINE, Number, ErrorResp, get(callid)]),
	    stepswitch_util:respond_erroneously(ErrorResp, JObj);
	{rate_resp, RateJObj} ->
	    case wh_json:get_value(<<"Rates">>, RateJObj) of
		[] ->
		    whapps_util:alert(<<"error">>, ["Source: ~s(~b)~n"
						    ,"Alert: rate information unavailable for ~s~n"
						    ,"Call-ID: ~s~n"]
				      ,[?MODULE, ?LINE, Number, get(callid)]),
		    ?LOG("No rates found for ~s", [Number]),
		    wait_for_bridge(Number, JObj, CtlQueue, EPLen);
		[RateInfoJObj | _] ->
		    {ok, Payload} = wapi_dialplan:set([{<<"Application-Name">>, <<"set">>}
						       ,{<<"Call-ID">>, get(callid)}
						       ,{<<"Custom-Call-Vars">>, wh_json:new()}
						       ,{<<"Custom-Channel-Vars">>, RateInfoJObj}
						      ]),
		    wapi_dialplan:publish_event(CtlQueue, Payload),
		    ?LOG("Sent rate information to call channel"),
		    wait_for_bridge(Number, JObj, CtlQueue, EPLen)
	    end
    end.

-spec request_rating/2 :: (ne_binary(), json_object()) -> 'ok'.
request_rating(Q, JObj) ->
    whapps_util:put_callid(JObj),

    ?LOG("Building rate request"),

    Req = [{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, JObj)}
	   ,{<<"Call-ID">>, get(callid)}
	   ,{<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, JObj)}
	   ,{<<"Options">>, wh_json:get_value(<<"Flags">>, JObj, [])}
	   ,{<<"Direction">>, <<"outbound">>}
	   | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
	  ],

    wapi_call:publish_rate_req(get(callid), [KV || {_,V}=KV <- Req, V =/= undefined]).
