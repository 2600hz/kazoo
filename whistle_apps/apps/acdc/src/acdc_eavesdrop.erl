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

start(MemberCall, AcctId, AgentCallId) ->
    EPId = <<"5381e0c5caa8d34eec06e0f75d0b4189">>,
    {ok, EPDoc} = couch_mgr:open_cache_doc(wh_util:format_account_id(AcctId, encoded), EPId),
    {ok, [EP]} = cf_endpoint:build(EPDoc, new_call(AcctId)),

    CCVs = props:filter_undefined([{<<"Account-ID">>, AcctId}]),

    Prop = props:filter_undefined(
             [{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
              ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
              ,{<<"Timeout">>, 8}
              ,{<<"Endpoints">>, [wh_json:set_values([{<<"Endpoint-Timeout">>, 8}
                                                      ,{<<"Outgoing-Caller-ID-Name">>, <<"eaves:", (whapps_call:caller_id_name(MemberCall))/binary>>}
                                                      ,{<<"Outgoing-Caller-ID-Number">>, whapps_call:caller_id_number(MemberCall)}
                                                     ], EP)
                                 ]
               }
              ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
                                                   ,<<"Retain-CID">>
                                                   ,<<"Authorizing-ID">>
                                                   ,<<"Authorizing-Type">>
                                                  ]}
              ,{<<"Account-ID">>, AcctId}
              ,{<<"Resource-Type">>, <<"originate">>}
              ,{<<"Application-Name">>, <<"eavesdrop">>}
              ,{<<"Eavesdrop-Call-ID">>, AgentCallId}
              ,{<<"Eavesdrop-Mode">>, <<"full">>}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),

    lager:debug("sending eavesdrop request: ~p", [Prop]),

    case whapps_util:amqp_pool_request(Prop
                                       ,fun wapi_resource:publish_originate_req/1
                                       ,fun wapi_dialplan:originate_ready_v/1
                                      ) of
        {ok, JObj} ->
            lager:debug("originate is ready to execute"),
            send_originate_execute(JObj, wh_json:get_value(<<"Server-ID">>, JObj));
        {error, E} ->
            lager:debug("error originating: ~p", [E])
    end.

new_call(AcctId) ->
    whapps_call:from_json(wh_json:from_list([{<<"Account-ID">>, AcctId}
                                             ,{<<"Account-DB">>, wh_util:format_account_id(AcctId, encoded)}
                                            ])).

send_originate_execute(JObj, Q) ->
    Prop = [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_dialplan:publish_originate_execute(wh_json:get_value(<<"Server-ID">>, JObj), Prop).


