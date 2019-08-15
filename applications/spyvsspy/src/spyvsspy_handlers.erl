%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(spyvsspy_handlers).

-export([handle_eavesdrop_req/2]).

-include("spyvsspy.hrl").

-define(RESOURCE_TYPE_AUDIO, <<"audio">>).

-spec handle_eavesdrop_req(kz_json:object(), kz_term:proplist()) -> any().
handle_eavesdrop_req(JObj, _Props) ->
    'true' = kapi_resource:eavesdrop_req_v(JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),

    case get_endpoints(AccountId
                      ,kz_json:get_value(<<"Endpoint-ID">>, JObj)
                      )
    of
        {'ok', EPs} -> send_eavesdrop(JObj, EPs, AccountId);
        {'error', E} -> respond_error(JObj, kz_term:to_binary(E))
    end.

-spec get_endpoints(kz_term:ne_binary(), kz_term:ne_binary()) ->
                           {'ok', kz_json:objects()} |
                           {'error', any()}.
get_endpoints(AccountId, EndpointId) ->
    kz_endpoint:build(EndpointId, new_call(AccountId)).

-spec new_call(kz_term:ne_binary()) -> kapps_call:call().
new_call(AccountId) ->
    kapps_call:from_json(
      kz_json:from_list(
        [{<<"Account-ID">>, AccountId}
        ,{<<"Account-DB">>, kz_util:format_account_id(AccountId, 'encoded')}
        ,{<<"Resource-Type">>, ?RESOURCE_TYPE_AUDIO}
        ]
       )
     ).

-spec get_group_and_call_id(kz_json:object()) -> {kz_term:api_binary(), kz_term:api_binary()}.
get_group_and_call_id(JObj) ->
    case kz_json:get_value(<<"Eavesdrop-Group-ID">>, JObj) of
        'undefined' -> {'undefined', kz_json:get_value(<<"Eavesdrop-Call-ID">>, JObj)};
        GroupId -> {GroupId, kz_json:get_value(<<"Eavesdrop-Call-ID">>, JObj, <<"all">>)}
    end.

-spec send_eavesdrop(kz_json:object(), kz_json:objects(), kz_term:ne_binary()) -> 'ok'.
send_eavesdrop(JObj, EPs, AccountId) ->
    {GroupId, CallId} = get_group_and_call_id(JObj),

    CCVs = props:filter_undefined([{<<"Account-ID">>, AccountId}]),
    Timeout = kz_json:get_integer_value(<<"Endpoint-Timeout">>, JObj, 20),

    {CallerIdName, CallerIdNumber} = find_caller_id(JObj),

    Prop = props:filter_undefined(
             [{<<"Msg-ID">>, kz_binary:rand_hex(6)}
             ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
             ,{<<"Timeout">>, Timeout}
             ,{<<"Endpoints">>, [kz_json:set_values([{<<"Endpoint-Timeout">>, Timeout}
                                                    ,{<<"Outbound-Caller-ID-Name">>, CallerIdName}
                                                    ,{<<"Outbound-Caller-ID-Number">>, CallerIdNumber}
                                                    ]
                                                   ,EP
                                                   )
                                 || EP <- EPs
                                ]}
             ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
                                                 ,<<"Retain-CID">>
                                                 ,<<"Authorizing-ID">>
                                                 ,<<"Authorizing-Type">>
                                                 ]}
             ,{<<"Account-ID">>, AccountId}
             ,{<<"Resource-Type">>, <<"originate">>}
             ,{<<"Application-Name">>, <<"eavesdrop">>}
             ,{<<"Eavesdrop-Call-ID">>, CallId}
             ,{<<"Eavesdrop-Group-ID">>, GroupId}
             ,{<<"Eavesdrop-Mode">>, kz_json:get_value(<<"Eavesdrop-Mode">>, JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),

    lager:debug("sending eavesdrop request for ~s:~s", [CallerIdName, CallerIdNumber]),

    case kz_amqp_worker:call_collect(Prop
                                    ,fun kapi_resource:publish_originate_req/1
                                    ,fun until_callback/1
                                    ,5 * ?MILLISECONDS_IN_SECOND
                                    )
    of
        {'ok', [OrigJObj|_]} ->
            lager:debug("originate is ready to execute"),
            send_originate_execute(OrigJObj, kz_json:get_value(<<"Server-ID">>, OrigJObj)),
            respond_success(JObj, OrigJObj);
        {'error', E} ->
            lager:debug("error originating: ~p", [E]),
            respond_error(JObj, E);
        {'timeout', _} ->
            lager:debug("error originating: timeout"),
            respond_error(JObj, <<"timeout">>)
    end.

-spec until_callback(kz_json:objects()) -> boolean().
until_callback([JObj | _]) ->
    kapi_dialplan:originate_ready_v(JObj).

-spec respond_success(kz_json:object(), kz_json:object()) -> 'ok'.
respond_success(JObj, OrigJObj) ->
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
    EavesdropCallId = kz_json:get_value(<<"Call-ID">>, OrigJObj),
    respond(ServerId, [{<<"Status">>, <<"started">>}
                      ,{<<"Eavesdropper-Call-ID">>, EavesdropCallId}
                      ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ]).

-spec respond_error(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
respond_error(JObj, E) ->
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
    respond(ServerId, [{<<"Status">>, <<"error">>}
                      ,{<<"Error-Msg">>, E}
                       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ]).

-spec respond(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
respond(ServerId, Props) ->
    lager:debug("respond to ~s: ~p", [ServerId, Props]),
    kapi_resource:publish_eavesdrop_resp(ServerId, Props).

-spec send_originate_execute(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_originate_execute(JObj, Q) ->
    Prop = [{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, JObj)}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    kapi_dialplan:publish_originate_execute(kz_json:get_value(<<"Server-ID">>, JObj), Prop).

-spec find_caller_id(kz_json:object()) ->
                            {kz_term:ne_binary(), kz_term:api_binary()}.
find_caller_id(JObj) ->
    find_caller_id(JObj, [{<<"Outbound-Caller-ID-Name">>, <<"Outbound-Caller-ID-Number">>}
                         ,{<<"Caller-ID-Name">>, <<"Caller-ID-Number">>}
                         ]).

-spec find_caller_id(kz_json:object(), kz_term:proplist()) ->
                            {kz_term:ne_binary(), kz_term:api_binary()}.
find_caller_id(_JObj, []) -> {<<"SpyVsSpy">>, <<"01010101">>};
find_caller_id(JObj, [{KName, KNum}|Ks]) ->
    case kz_json:get_value(KName, JObj) of
        'undefined' -> find_caller_id(JObj, Ks);
        Name -> {Name, kz_json:get_value(KNum, JObj)}
    end.
