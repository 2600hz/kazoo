%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2012, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 29 Nov 2012 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(acdc_eavesdrop).

-include("acdc.hrl").

-export([start/3]).

start(MCall, AcctId, AgentCallId) ->
    Prop = [{<<"Eavesdrop-Mode">>, <<"listen">>}
            ,{<<"Account-ID">>, AcctId}
            ,{<<"Endpoint-ID">>, <<"5381e0c5caa8d34eec06e0f75d0b4189">>}
            ,{<<"Eavesdrop-Call-ID">>, AgentCallId}
            ,{<<"Outbound-Caller-ID-Name">>, whapps_call:caller_id_name(MCall)}
            ,{<<"Outbound-Caller-ID-Number">>, whapps_call:caller_id_number(MCall)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    eavesdrop_req(Prop).

eavesdrop_req(Prop) ->
    lager:debug("Sending eavs ~p", [Prop]),
    case whapps_util:amqp_pool_request(Prop
                                       ,fun wapi_resource:publish_eavesdrop_req/1
                                       ,fun wapi_resource:eavesdrop_resp_v/1
                                       ,2000
                                      )
    of
        {ok, Resp} -> lager:debug("ok: ~p", [Resp]);
        {error, timeout} -> lager:debug("err: timeout");
        {error, E} -> lager:debug("err: ~p", [E])
    end.
