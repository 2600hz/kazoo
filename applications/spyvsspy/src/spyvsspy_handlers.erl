%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(spyvsspy_handlers).

-export([handle_eavesdrop_req/2]).

-include("spyvsspy.hrl").

-spec handle_eavesdrop_req(wh_json:object(), wh_proplist()) -> any().
handle_eavesdrop_req(JObj, _Props) ->
    'true' = wapi_resource:eavesdrop_req_v(JObj),
    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),

    case get_endpoints(AcctId
                      ,wh_json:get_value(<<"Endpoint-ID">>, JObj)
                     )
    of
        {'ok', EPs} -> send_eavesdrop(JObj, EPs, AcctId);
        {'error', E} -> respond_error(JObj, E)
    end.

-spec get_endpoints(ne_binary(), ne_binary()) ->
                           {'ok', wh_json:objects()} |
                           {'error', _}.
get_endpoints(AcctId, EndpointId) ->
    cf_endpoint:build(EndpointId, new_call(AcctId)).

-spec new_call(ne_binary()) -> whapps_call:call().
new_call(AcctId) ->
    whapps_call:from_json(wh_json:from_list(
                            [{<<"Account-ID">>, AcctId}
                             ,{<<"Account-DB">>, wh_util:format_account_id(AcctId, 'encoded')}
                            ])).

-spec get_group_and_call_id(wh_json:object()) -> {api_binary(), api_binary()}.
get_group_and_call_id(JObj) ->
    case wh_json:get_value(<<"Eavesdrop-Group-ID">>, JObj) of
        'undefined' -> {'undefined', wh_json:get_value(<<"Eavesdrop-Call-ID">>, JObj)};
        GroupId -> {GroupId, wh_json:get_value(<<"Eavesdrop-Call-ID">>, JObj, <<"all">>)}
    end.

-spec send_eavesdrop(wh_json:object(), wh_json:objects(), ne_binary()) -> 'ok'.
send_eavesdrop(JObj, EPs, AcctId) ->
    {GroupId, CallId} = get_group_and_call_id(JObj),

    CCVs = props:filter_undefined([{<<"Account-ID">>, AcctId}]),
    Timeout = wh_json:get_integer_value(<<"Endpoint-Timeout">>, JObj, 20),

    {CallerIdName, CallerIdNumber} = find_caller_id(JObj),

    Prop = wh_json:set_values(
             props:filter_undefined(
               [{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
                ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
                ,{<<"Timeout">>, Timeout}
                ,{<<"Endpoints">>, [wh_json:set_values([{<<"Endpoint-Timeout">>, Timeout}
                                                        ,{<<"Outbound-Caller-ID-Name">>, CallerIdName}
                                                        ,{<<"Outbound-Caller-ID-Number">>, CallerIdNumber}
                                                       ]
                                                       ,EP)
                                    || EP <- EPs
                                   ]}
                ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
                                                     ,<<"Retain-CID">>
                                                     ,<<"Authorizing-ID">>
                                                     ,<<"Authorizing-Type">>
                                                    ]}
                ,{<<"Account-ID">>, AcctId}
                ,{<<"Resource-Type">>, <<"originate">>}
                ,{<<"Application-Name">>, <<"eavesdrop">>}
                ,{<<"Eavesdrop-Call-ID">>, CallId}
                ,{<<"Eavesdrop-Group-ID">>, GroupId}
                ,{<<"Eavesdrop-Mode">>, wh_json:get_value(<<"Eavesdrop-Mode">>, JObj)}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]
              )
             ,wh_json:delete_key(<<"Event-Name">>, JObj)),

    lager:debug("sending eavesdrop request for ~s:~s", [CallerIdName, CallerIdNumber]),

    case whapps_util:amqp_pool_collect(Prop
                                       ,fun wapi_resource:publish_originate_req/1
                                       ,fun until_callback/1
                                       ,5000
                                      ) of
        {'ok', [OrigJObj|_]} ->
            lager:debug("originate is ready to execute"),
            send_originate_execute(OrigJObj, wh_json:get_value(<<"Server-ID">>, OrigJObj)),
            respond_success(JObj, OrigJObj);
        {'error', E} ->
            lager:debug("error originating: ~p", [E]),
            respond_error(JObj, E);
        {'timeout', _} ->
            lager:debug("error originating: timeout"),
            respond_error(JObj, 'timeout')
    end.

-spec until_callback(wh_json:objects()) -> boolean().
until_callback([JObj | _]) ->
    wapi_dialplan:originate_ready_v(JObj).

-spec respond_success(wh_json:object(), wh_json:object()) -> 'ok'.
respond_success(JObj, OrigJObj) ->
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    EavesdropCallId = wh_json:get_value(<<"Call-ID">>, OrigJObj),
    respond(ServerId, [{<<"Status">>, <<"started">>}
                       ,{<<"Eavesdropper-Call-ID">>, EavesdropCallId}
                       ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ]).

-spec respond_error(wh_json:object(), ne_binary()) -> 'ok'.
respond_error(JObj, E) ->
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    respond(ServerId, [{<<"Status">>, <<"error">>}
                       ,{<<"Error-Msg">>, E}
                       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ]).

-spec respond(ne_binary(), wh_proplist()) -> 'ok'.
respond(ServerId, Props) ->
    lager:debug("respond to ~s: ~p", [ServerId, Props]),
    wapi_resource:publish_eavesdrop_resp(ServerId, Props).

-spec send_originate_execute(wh_json:object(), ne_binary()) -> 'ok'.
send_originate_execute(JObj, Q) ->
    Prop = [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_dialplan:publish_originate_execute(wh_json:get_value(<<"Server-ID">>, JObj), Prop).

-spec find_caller_id(wh_json:object()) ->
                            {ne_binary(), api_binary()}.
-spec find_caller_id(wh_json:object(), wh_proplist()) ->
                            {ne_binary(), api_binary()}.
find_caller_id(JObj) ->
    find_caller_id(JObj, [{<<"Outbound-Caller-ID-Name">>, <<"Outbound-Caller-ID-Number">>}
                          ,{<<"Caller-ID-Name">>, <<"Caller-ID-Number">>}
                         ]).

find_caller_id(_JObj, []) -> {<<"SpyVsSpy">>, <<"01010101">>};
find_caller_id(JObj, [{KName, KNum}|Ks]) ->
    case wh_json:get_value(KName, JObj) of
        'undefined' -> find_caller_id(JObj, Ks);
        Name -> {Name, wh_json:get_value(KNum, JObj)}
    end.
