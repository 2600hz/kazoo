%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_sms_command).

-include("kapps_sms_command.hrl").

-export([send_sms/2, send_sms/3
        ,send_amqp_sms/1, send_amqp_sms/2
        ]).
-export([b_send_sms/2, b_send_sms/3, b_send_sms/4]).

-export([default_collect_timeout/0
        ,default_message_timeout/0
        ,default_application_timeout/0
        ]).

-define(CONFIG_CAT, <<"sms_command">>).

-define(DEFAULT_COLLECT_TIMEOUT
       ,kapps_config:get_integer(?CONFIG_CAT, <<"collect_timeout">>, 60 * ?MILLISECONDS_IN_SECOND)
       ).

-define(DEFAULT_MESSAGE_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, <<"message_timeout">>, 60 * ?MILLISECONDS_IN_SECOND)).

-define(DEFAULT_APPLICATION_TIMEOUT
       ,kapps_config:get_integer(?CONFIG_CAT, <<"application_timeout">>, 500 * ?MILLISECONDS_IN_SECOND)
       ).
-define(DEFAULT_STRATEGY, <<"single">>).

-define(ATOM(X), kz_term:to_atom(X, 'true')).
-define(SMS_POOL(A,B,C), ?ATOM(<<A/binary, "_", B/binary, "_", C/binary>>) ).

-define(SMS_DEFAULT_OUTBOUND_OPTIONS
       ,kz_json:from_list([{<<"delivery_mode">>, 2}
                          ,{<<"mandatory">>, 'true'}
                          ])
       ).
-define(SMS_OUTBOUND_OPTIONS_KEY, [<<"outbound">>, <<"options">>]).
-define(SMS_OUTBOUND_OPTIONS
       ,kapps_config:get_json(<<"sms">>, ?SMS_OUTBOUND_OPTIONS_KEY, ?SMS_DEFAULT_OUTBOUND_OPTIONS)
       ).

-spec default_collect_timeout() -> pos_integer().
default_collect_timeout() ->
    ?DEFAULT_COLLECT_TIMEOUT.

-spec default_message_timeout() -> pos_integer().
default_message_timeout() ->
    ?DEFAULT_MESSAGE_TIMEOUT.

-spec default_application_timeout() -> pos_integer().
default_application_timeout() ->
    ?DEFAULT_APPLICATION_TIMEOUT.

-type kapps_api_sms_return() :: {'error', 'timeout' | kz_json:object()} |
                                {'ok', kz_json:object()}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_sms(kz_json:objects(), kapps_call:call()) -> 'ok'.
send_sms(Endpoints, Call) -> send_sms(Endpoints, ?DEFAULT_STRATEGY, Call).

-spec send_sms(kz_json:objects(), binary(), kapps_call:call()) -> 'ok'.
send_sms(EndpointList, Strategy, Call) ->
    Endpoints = create_sms_endpoints(EndpointList, []),
    API = create_sms(Call),
    send(Strategy, API, Endpoints).

-spec b_send_sms(kz_json:objects(), kapps_call:call()) -> kapps_api_sms_return().
b_send_sms(Endpoints, Call) -> b_send_sms(Endpoints, ?DEFAULT_STRATEGY, Call).

-spec b_send_sms(kz_json:objects(), binary(), kapps_call:call()) -> kapps_api_sms_return().
b_send_sms(Endpoints, Strategy, Call) -> b_send_sms(Endpoints, Strategy, ?DEFAULT_MESSAGE_TIMEOUT, Call).

-spec b_send_sms(kz_json:objects(), binary(), integer(), kapps_call:call()) -> kapps_api_sms_return().
b_send_sms(EndpointList, Strategy, Timeout, Call) ->
    Endpoints = create_sms_endpoints(EndpointList, []),
    API = create_sms(Call),
    send_and_wait(Strategy, API, Endpoints, Timeout).

send(<<"single">>, _API, []) ->
    {'error', <<"no endpoints available">>};
send(<<"single">>, API, [Endpoint | Others]) ->
    CallId = props:get_value(<<"Call-ID">>, API),
    Payload = props:set_values(
                [{<<"Endpoints">>, [Endpoint]}
                ,{<<"Callee-ID-Name">>, kz_json:get_value(<<"Callee-ID-Name">>, Endpoint)}
                ,{<<"Callee-ID-Number">>, kz_json:get_value(<<"Callee-ID-Number">>, Endpoint)}
                ,{<<"To-DID">>, kz_json:get_value(<<"To-DID">>, Endpoint)}
                 | kz_json:get_value(<<"Endpoint-Options">>, Endpoint, [])
                ], API),
    case kz_amqp_worker:cast(Payload, fun kapi_sms:publish_message/1) of
        'ok' -> 'ok';
        {'error', _R}=Err when Others =:= [] ->
            lager:info("received error while sending msg ~s: ~-800p", [CallId, _R]),
            Err;
        {'error', _R} ->
            lager:info("received error while sending msg ~s: ~-800p", [CallId, _R]),
            lager:info("processing next endpoint."),
            send(<<"single">>, API, Others)
    end;
send(Strategy, _API, _Endpoints) ->
    lager:debug("strategy ~s not implemented", [Strategy]).

send_and_wait(<<"single">>, _API, [], _Timeout) ->
    {'error', <<"no endpoints available">>};
send_and_wait(<<"single">>, API, [Endpoint| Others], Timeout) ->
    CallId = props:get_value(<<"Call-ID">>, API),
    Type = kz_json:get_value(<<"Endpoint-Type">>, Endpoint, <<"sip">>),
    ReqResp = send(Type, API, Endpoint, Timeout),
    case ReqResp of
        {'error', _R}=Err when Others =:= [] ->
            lager:info("received error while sending msg ~s: ~-800p", [CallId, _R]),
            Err;
        {'error', _R} ->
            lager:info("received error while sending msg ~s: ~-800p", [CallId, _R]),
            lager:info("processing next endpoint."),
            send_and_wait(<<"single">>, API, Others, Timeout);
        {_, _JObjs} = Ret ->
            lager:debug("received sms delivery result for msg ~s", [CallId]),
            Ret
    end;
send_and_wait(Strategy, _API, _Endpoints, _Timeout) ->
    lager:debug("strategy ~s not implemented", [Strategy]).

send(<<"sip">>, API, Endpoint, Timeout) ->
    Options = kz_json:to_proplist(kz_json:get_value(<<"Endpoint-Options">>, Endpoint, [])),
    Payload = props:set_values( [{<<"Endpoints">>, [Endpoint]} | Options], API),
    CallId = props:get_value(<<"Call-ID">>, Payload),
    lager:debug("sending sms and waiting for response ~s", [CallId]),
    _ = kz_amqp_worker:cast(Payload, fun kapi_sms:publish_message/1),
    wait_for_correlated_message(CallId, <<"delivery">>, <<"message">>, Timeout);
send(<<"amqp">>, API, Endpoint, _Timeout) ->
    CallId = props:get_value(<<"Call-ID">>, API),
    Options = kz_json:to_proplist(kz_json:get_value(<<"Endpoint-Options">>, Endpoint, [])),
    Props = kz_json:to_proplist(Endpoint) ++ Options,
    Broker = kz_json:get_value(<<"Route">>, Endpoint),
    Exchange = kz_json:get_value([<<"Endpoint-Options">>, <<"Exchange-ID">>], Endpoint),
    ExchangeType = kz_json:get_value([<<"Endpoint-Options">>, <<"Exchange-Type">>], Endpoint, <<"topic">>),
    ExchangeOptions = amqp_exchange_options(kz_json:get_value([<<"Endpoint-Options">>, <<"Exchange-Options">>], Endpoint)),
    RouteId = kz_json:get_value([<<"Endpoint-Options">>, <<"Route-ID">>], Endpoint),
    BrokerName = kz_json:get_value([<<"Endpoint-Options">>, <<"Broker-Name">>], Endpoint, <<"noname">>),
    FailOver = kz_json:get_value(<<"Failover">>, Endpoint),
    maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName),

    Payload = props:set_values(Props, API),
    case send_amqp_sms(Payload, ?SMS_POOL(Exchange, RouteId, BrokerName)) of
        'ok' ->
            DeliveryProps = props:filter_undefined(
                              [{<<"Delivery-Result-Code">>, <<"sip:200">> }
                              ,{<<"Status">>, <<"Success">>}
                              ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, API) }
                              ,{<<"Call-ID">>, CallId }
                               | kz_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
                              ]),
            {'ok', kz_json:set_values(DeliveryProps, kz_json:new())};
        {'error', {'timeout', _Reason}} when FailOver =:= 'undefined' ->
            DeliveryProps = props:filter_undefined(
                              [{<<"Delivery-Result-Code">>, <<"sip:500">> }
                              ,{<<"Delivery-Failure">>, 'true'}
                              ,{<<"Error-Code">>, 500}
                              ,{<<"Error-Message">>, <<"timeout">>}
                              ,{<<"Status">>, <<"Failed">>}
                              ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, API) }
                              ,{<<"Call-ID">>, CallId }
                               | kz_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
                              ]),
            {'ok', kz_json:set_values(DeliveryProps, kz_json:new())};
        {'error', Reason} when FailOver =:= 'undefined' ->
            DeliveryProps = props:filter_undefined(
                              [{<<"Delivery-Result-Code">>, <<"sip:500">> }
                              ,{<<"Delivery-Failure">>, 'true'}
                              ,{<<"Error-Code">>, 500}
                              ,{<<"Error-Message">>, kz_term:to_binary(Reason)}
                              ,{<<"Status">>, <<"Failed">>}
                              ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, API) }
                              ,{<<"Call-ID">>, CallId }
                               | kz_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
                              ]),
            {'ok', kz_json:set_values(DeliveryProps, kz_json:new())};
        {'error', Reason} ->
            lager:info("received error while sending msg ~s: ~-800p", [CallId, Reason]),
            lager:info("trying failover"),
            send(<<"amqp">>, API, FailOver, _Timeout)
    end.

-spec amqp_exchange_options(kz_term:api_object()) -> kz_term:proplist().
amqp_exchange_options('undefined') -> [];
amqp_exchange_options(JObj) ->
    [{kz_term:to_atom(K, 'true'), V}
     || {K, V} <- kz_json:to_proplist(JObj)
    ].

-spec send_amqp_sms(kz_term:proplist()) -> 'ok' | {'error', any()}.
send_amqp_sms(Payload) ->
    send_amqp_sms(Payload, kz_amqp_worker:worker_pool()).

-spec send_amqp_sms(kz_term:proplist(), atom()) -> 'ok' | {'error', any()}.
send_amqp_sms(Payload, Pool) ->
    case kz_amqp_worker:cast(Payload, fun publish_outbound/1, Pool) of
        'ok' -> 'ok';
        {'error', _}=E -> E;
        {'returned', _JObj, Deliver} ->
            {'error', kz_json:get_value(<<"message">>, Deliver, <<"unknown">>)}
    end.

publish_outbound(Payload) ->
    AMQPOptions = amqp_options(),
    kapi_sms:publish_outbound(Payload, ?DEFAULT_CONTENT_TYPE, AMQPOptions).

amqp_options() ->
    amqp_options(?SMS_OUTBOUND_OPTIONS).

-spec amqp_options(kz_term:api_object()) -> kz_term:proplist().
amqp_options('undefined') -> [];
amqp_options(JObj) ->
    [{kz_term:to_atom(K, 'true'), V}
     || {K, V} <- kz_json:to_proplist(JObj)
    ].

-spec maybe_add_broker(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary()) -> 'ok'.
maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName) ->
    PoolPid = kz_amqp_sup:pool_pid(?SMS_POOL(Exchange, RouteId, BrokerName)),
    maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName, PoolPid).

-spec maybe_add_broker(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary(), kz_term:api_pid()) -> 'ok'.
maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName, 'undefined') ->
    Exchanges = [{Exchange, ExchangeType, ExchangeOptions}],
    _ = kz_amqp_sup:add_amqp_pool(?SMS_POOL(Exchange, RouteId, BrokerName), Broker, 5, 5, [], Exchanges, 'true'),
    'ok';
maybe_add_broker(_Broker, _Exchange, _RouteId, _ExchangeType, _ExchangeOptions, _BrokerName, _Pid) -> 'ok'.

-spec create_sms(kapps_call:call()) -> kz_term:proplist().
create_sms(Call) ->
    AccountId = kapps_call:account_id(Call),
    AccountRealm = kapps_call:to_realm(Call),
    CCVUpdates = props:filter_undefined(
                   [{<<"Ignore-Display-Updates">>, <<"true">>}
                   ,{<<"Account-ID">>, AccountId}
                   ,{<<"Account-Realm">>, AccountRealm}
                   ,{<<"From-User">>, kapps_call:from_user(Call)}
                   ,{<<"From-Realm">>, kapps_call:from_realm(Call)}
                   ,{<<"From-URI">>, kapps_call:from(Call)}
                   ,{<<"Reseller-ID">>, kz_services_reseller:get_id(AccountId)}
                   ]),
    [{<<"Message-ID">>, kapps_call:kvs_fetch(<<"Message-ID">>, Call)}
    ,{<<"Call-ID">>, kapps_call:call_id(Call)}
    ,{<<"Body">>, kapps_call:kvs_fetch(<<"Body">>, Call)}
    ,{<<"From">>, kapps_call:from(Call)}
    ,{<<"Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
    ,{<<"To">>, kapps_call:to(Call)}
    ,{<<"Request">>, kapps_call:request(Call) }
    ,{<<"Application-Name">>, <<"send">>}
    ,{<<"Custom-Channel-Vars">>, kz_json:set_values(CCVUpdates, kz_json:new())}
     | kz_api:default_headers(kapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)
    ].

-spec create_sms_endpoints(kz_json:objects(), kz_json:objects()) -> kz_json:objects().
create_sms_endpoints([], Endpoints) -> Endpoints;
create_sms_endpoints([Endpoint | Others], Endpoints) ->
    EndpointType = kz_json:get_value(<<"Endpoint-Type">>, Endpoint, <<"sip">>),
    case create_sms_endpoint(Endpoint, EndpointType) of
        'undefined' -> create_sms_endpoints(Others, Endpoints);
        NewEndpoint -> create_sms_endpoints(Others, [NewEndpoint | Endpoints])
    end.

-spec create_sms_endpoint(kz_json:object(), binary()) -> kz_term:api_object().
create_sms_endpoint(Endpoint, <<"amqp">>) -> Endpoint;
create_sms_endpoint(Endpoint, <<"sip">>) ->
    Realm = kz_json:get_value(<<"To-Realm">>, Endpoint),
    Username = kz_json:get_value(<<"To-User">>, Endpoint),
    case lookup_reg(Username, Realm) of
        {'ok', Node} ->
            Options = kz_json:get_value(<<"Endpoint-Options">>, Endpoint, []),
            kz_json:set_values(
              [{<<"Route-ID">>, Node}
              ,{<<"Endpoint-Options">>, kz_json:from_list([{<<"Route-ID">>, Node} | Options])}
              ], Endpoint);
        {'error', _E} -> 'undefined'
    end.

-spec lookup_reg(kz_term:ne_binary(), kz_term:ne_binary()) -> {'error', any()} |
                                                              {'ok', kz_term:ne_binary()}.
lookup_reg('undefined', _Realm) -> {'error', 'invalid_user'};
lookup_reg(_Username, 'undefined') -> {'error', 'invalid_realm'};
lookup_reg(Username, Realm) ->
    Req = [{<<"Realm">>, Realm}
          ,{<<"Username">>, Username}
          ,{<<"Fields">>, [<<"Registrar-Node">>]}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_registration:publish_query_req/1
                                    ,{'ecallmgr', 'true'}
                                    )
    of
        {'error', _E}=E ->
            lager:debug("error getting registration: ~p", [_E]),
            E;
        {_, JObjs} ->
            case extract_device_registrations(JObjs) of
                [] -> {'error', 'not_registered'};
                [FirstNode | _Others] -> {'ok', FirstNode}
            end
    end.

-spec extract_device_registrations(kz_json:objects()) -> kz_term:ne_binaries().
extract_device_registrations(JObjs) ->
    sets:to_list(extract_device_registrations(JObjs, sets:new())).

-spec extract_device_registrations(kz_json:objects(), sets:set()) -> sets:set().
extract_device_registrations([], Set) -> Set;
extract_device_registrations([JObj|JObjs], Set) ->
    Fields = kz_json:get_value(<<"Fields">>, JObj, []),
    S = lists:foldl(fun extract_device_registrar_fold/2, Set, Fields),
    extract_device_registrations(JObjs, S).

-spec extract_device_registrar_fold(kz_json:object(), sets:set()) -> sets:set().
extract_device_registrar_fold(JObj, Set) ->
    case kz_json:get_ne_value(<<"Registrar-Node">>, JObj) of
        'undefined' -> Set;
        AuthId -> sets:add_element(AuthId, Set)
    end.

-spec get_correlated_msg_type(kz_json:object()) ->
                                     {kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()}.
get_correlated_msg_type(JObj) ->
    get_correlated_msg_type(<<"Call-ID">>, JObj).

-spec get_correlated_msg_type(kz_term:ne_binary(), kz_json:object()) ->
                                     {kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()}.
get_correlated_msg_type(Key, JObj) ->
    {C, N} = kz_util:get_event_type(JObj),
    {C, N, kz_json:get_value(Key, JObj)}.

-spec wait_for_correlated_message(kz_term:ne_binary() | kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), timeout()) ->
                                         kapps_api_std_return().
wait_for_correlated_message(CallId, Event, Type, Timeout) when is_binary(CallId) ->
    Start = kz_time:start_time(),
    case kapps_call_command:receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj}=Ok ->
            case get_correlated_msg_type(JObj) of
                {<<"error">>, _, CallId} ->
                    lager:debug("channel execution error while waiting for ~s", [CallId]),
                    {'error', JObj};
                {Type, Event, CallId } ->
                    Ok;
                {_Type, _Event, _CallId} ->
                    lager:debug("received message (~s , ~s, ~s)",[_Type, _Event, _CallId]),
                    wait_for_correlated_message(CallId, Event, Type, kz_time:decr_timeout(Timeout, Start))
            end
    end;
wait_for_correlated_message(Call, Event, Type, Timeout) ->
    CallId = kapps_call:call_id(Call),
    wait_for_correlated_message(CallId, Event, Type, Timeout).
