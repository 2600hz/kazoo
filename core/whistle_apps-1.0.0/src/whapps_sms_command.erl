%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whapps_sms_command).

-include("whapps_sms_command.hrl").

-export([send_sms/2, send_sms/3]).
-export([b_send_sms/2, b_send_sms/3, b_send_sms/4]).

-export([default_collect_timeout/0
         ,default_message_timeout/0
         ,default_application_timeout/0
        ]).

-define(CONFIG_CAT, <<"sms_command">>).

-define(DEFAULT_COLLECT_TIMEOUT
        ,whapps_config:get_integer(?CONFIG_CAT, <<"collect_timeout">>, 60 * ?MILLISECONDS_IN_SECOND)
       ).

-define(DEFAULT_MESSAGE_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, <<"message_timeout">>, 60 * ?MILLISECONDS_IN_SECOND)).

-define(DEFAULT_APPLICATION_TIMEOUT
        ,whapps_config:get_integer(?CONFIG_CAT, <<"application_timeout">>, 500 * ?MILLISECONDS_IN_SECOND)
       ).
-define(DEFAULT_STRATEGY, <<"single">>).

-define(ATOM(X), wh_util:to_atom(X, 'true')).
-define(SMS_POOL(A,B,C), ?ATOM(<<A/binary, "_", B/binary, "_", C/binary>>) ).

-spec default_collect_timeout() -> pos_integer().
default_collect_timeout() ->
    ?DEFAULT_COLLECT_TIMEOUT.

-spec default_message_timeout() -> pos_integer().
default_message_timeout() ->
    ?DEFAULT_MESSAGE_TIMEOUT.

-spec default_application_timeout() -> pos_integer().
default_application_timeout() ->
    ?DEFAULT_APPLICATION_TIMEOUT.

-type whapps_api_sms_return() :: {'error', 'timeout' | wh_json:object()} |
                                 {'ok', wh_json:object()}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_sms(wh_json:objects(), whapps_call:call()) -> 'ok'.
-spec send_sms(wh_json:objects(), binary(), whapps_call:call()) -> 'ok'.

-spec b_send_sms(wh_json:objects(), whapps_call:call()) -> whapps_api_sms_return().
-spec b_send_sms(wh_json:objects(), binary(), whapps_call:call()) -> whapps_api_sms_return().
-spec b_send_sms(wh_json:objects(), binary(), integer(), whapps_call:call()) -> whapps_api_sms_return().

send_sms(Endpoints, Call) -> send_sms(Endpoints, ?DEFAULT_STRATEGY, Call).
send_sms(EndpointList, Strategy, Call) ->
    Endpoints = create_sms_endpoints(EndpointList, []),
    API = create_sms(Call),
    send(Strategy, API, Endpoints).

b_send_sms(Endpoints, Call) -> b_send_sms(Endpoints, ?DEFAULT_STRATEGY, Call).
b_send_sms(Endpoints, Strategy, Call) -> b_send_sms(Endpoints, Strategy, ?DEFAULT_MESSAGE_TIMEOUT, Call).
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
                 ,{<<"Callee-ID-Name">>, wh_json:get_value(<<"Callee-ID-Name">>, Endpoint)}
                 ,{<<"Callee-ID-Number">>, wh_json:get_value(<<"Callee-ID-Number">>, Endpoint)}
                 ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Endpoint)}
                   | wh_json:get_value(<<"Endpoint-Options">>, Endpoint, [])
                ], API),
    case whapps_util:amqp_pool_send(Payload, fun wapi_sms:publish_message/1) of
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
    lager:debug("Strategy ~s not implemented", [Strategy]).

send_and_wait(<<"single">>, _API, [], _Timeout) ->
    {'error', <<"no endpoints available">>};
send_and_wait(<<"single">>, API, [Endpoint| Others], Timeout) ->
    CallId = props:get_value(<<"Call-ID">>, API),
    Type = wh_json:get_value(<<"Endpoint-Type">>, Endpoint, <<"sip">>),
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
    lager:debug("Strategy ~s not implemented", [Strategy]).

send(<<"sip">>, API, Endpoint, Timeout) ->
    Options = wh_json:to_proplist(wh_json:get_value(<<"Endpoint-Options">>, Endpoint, [])),
    Payload = props:set_values( [{<<"Endpoints">>, [Endpoint]} | Options], API),
    CallId = props:get_value(<<"Call-ID">>, Payload),
    lager:debug("sending sms and waiting for response ~s", [CallId]),
    whapps_util:amqp_pool_send(Payload, fun wapi_sms:publish_message/1),
    wait_for_correlated_message(CallId, <<"delivery">>, <<"message">>, Timeout);
send(<<"amqp">>, API, Endpoint, _Timeout) ->
    CallId = props:get_value(<<"Call-ID">>, API),
    Options = wh_json:to_proplist(wh_json:get_value(<<"Endpoint-Options">>, Endpoint, [])),
    Props = wh_json:to_proplist(Endpoint) ++ Options,
    Broker = wh_json:get_value(<<"Route">>, Endpoint),
    Exchange = wh_json:get_value([<<"Endpoint-Options">>, <<"Exchange-ID">>], Endpoint),
    ExchangeType = wh_json:get_value([<<"Endpoint-Options">>, <<"Exchange-Type">>], Endpoint, <<"topic">>),
    ExchangeOptions = amqp_exchange_options(wh_json:get_value([<<"Endpoint-Options">>, <<"Exchange-Options">>], Endpoint)),
    RouteId = wh_json:get_value([<<"Endpoint-Options">>, <<"Route-ID">>], Endpoint),
    BrokerName = wh_json:get_value([<<"Endpoint-Options">>, <<"Broker-Name">>], Endpoint, <<"noname">>),
    FailOver = wh_json:get_value(<<"Failover">>, Endpoint),
    maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName),

    Payload = props:set_values(Props, API),
    case send_amqp_sms(Payload, ?SMS_POOL(Exchange, RouteId, BrokerName)) of
        'ok' ->
            DeliveryProps = props:filter_undefined(
                              [{<<"Delivery-Result-Code">>, <<"sip:200">> }
                               ,{<<"Status">>, <<"Success">>}
                               ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, API) }
                               ,{<<"Call-ID">>, CallId }
                               | wh_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
                              ]),
            {'ok', wh_json:set_values(DeliveryProps, wh_json:new())};
        {'error', {'timeout', _Reason}} when FailOver =:= 'undefined' ->
            DeliveryProps = props:filter_undefined(
                              [{<<"Delivery-Result-Code">>, <<"sip:500">> }
                               ,{<<"Delivery-Failure">>, 'true'}
                               ,{<<"Error-Code">>, 500}
                               ,{<<"Error-Message">>, <<"timeout">>}
                               ,{<<"Status">>, <<"Failed">>}
                               ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, API) }
                               ,{<<"Call-ID">>, CallId }
                               | wh_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
                              ]),
            {'ok', wh_json:set_values(DeliveryProps, wh_json:new())};
        {'error', Reason} when FailOver =:= 'undefined' ->
            DeliveryProps = props:filter_undefined(
                              [{<<"Delivery-Result-Code">>, <<"sip:500">> }
                               ,{<<"Delivery-Failure">>, 'true'}
                               ,{<<"Error-Code">>, 500}
                               ,{<<"Error-Message">>, wh_util:to_binary(Reason)}
                               ,{<<"Status">>, <<"Failed">>}
                               ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, API) }
                               ,{<<"Call-ID">>, CallId }
                               | wh_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
                              ]),
            {'ok', wh_json:set_values(DeliveryProps, wh_json:new())};
        {'error', Reason} ->
            lager:info("received error while sending msg ~s: ~-800p", [CallId, Reason]),
            lager:info("trying failover"),
            send(<<"amqp">>, API, FailOver, _Timeout)
    end.

-spec amqp_exchange_options(api_object()) -> wh_proplist().
amqp_exchange_options('undefined') -> [];
amqp_exchange_options(JObj) ->
    [{wh_util:to_atom(K, 'true'), V}
     || {K, V} <- wh_json:to_proplist(JObj)
    ].

-spec send_amqp_sms(wh_proplist(), atom()) -> 'ok' | {'error', term()}.
send_amqp_sms(Payload, Pool) ->
    case wh_amqp_worker:cast(Payload, fun wapi_sms:publish_outbound/1, Pool) of
        {'returned', _JObj, Deliver} ->
            {'error', wh_json:get_value(<<"message">>, Deliver, <<"unknown">>)};
        {'timeout', _} -> {'error', 'timeout'};
        Else -> Else
    end.

-spec maybe_add_broker(api_binary(), api_binary(), api_binary(), ne_binary(), wh_proplist(), ne_binary()) -> 'ok'.
-spec maybe_add_broker(api_binary(), api_binary(), api_binary(), ne_binary(), wh_proplist(), ne_binary(), api_pid()) -> 'ok'.
maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName) ->
    PoolPid = wh_amqp_sup:pool_pid(?SMS_POOL(Exchange, RouteId, BrokerName)),
    maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName, PoolPid).

maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName, 'undefined') ->
    Exchanges = [{Exchange, ExchangeType, ExchangeOptions}],
    wh_amqp_sup:add_amqp_pool(?SMS_POOL(Exchange, RouteId, BrokerName), Broker, 5, 5, [], Exchanges, 'true'),
    'ok';
maybe_add_broker(_Broker, _Exchange, _RouteId, _ExchangeType, _ExchangeOptions, _BrokerName, _Pid) -> 'ok'.

-spec create_sms(whapps_call:call()) -> wh_proplist().
create_sms(Call) ->
    AccountId = whapps_call:account_id(Call),
    AccountRealm = whapps_call:to_realm(Call),
    CCVUpdates = props:filter_undefined(
                   [{<<"Ignore-Display-Updates">>, <<"true">>}
                    ,{<<"Account-ID">>, AccountId}
                    ,{<<"Account-Realm">>, AccountRealm}
                    ,{<<"From-User">>, whapps_call:from_user(Call)}
                    ,{<<"From-Realm">>, whapps_call:from_realm(Call)}
                    ,{<<"From-URI">>, whapps_call:from(Call)}
                    ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
                   ]),
    [{<<"Message-ID">>, whapps_call:kvs_fetch(<<"Message-ID">>, Call)}
     ,{<<"Call-ID">>, whapps_call:call_id(Call)}
     ,{<<"Body">>, whapps_call:kvs_fetch(<<"Body">>, Call)}
     ,{<<"From">>, whapps_call:from(Call)}
     ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
     ,{<<"To">>, whapps_call:to(Call)}
     ,{<<"Request">>, whapps_call:request(Call) }
     ,{<<"Application-Name">>, <<"send">>}
     ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, wh_json:new())}
     | wh_api:default_headers(whapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)
    ].

-spec create_sms_endpoints(wh_json:objects(), wh_json:objects()) -> wh_json:objects().
create_sms_endpoints([], Endpoints) -> Endpoints;
create_sms_endpoints([Endpoint | Others], Endpoints) ->
    EndpointType = wh_json:get_value(<<"Endpoint-Type">>, Endpoint, <<"sip">>),
    case create_sms_endpoint(Endpoint, EndpointType) of
        'undefined' -> create_sms_endpoints(Others, Endpoints);
        NewEndpoint -> create_sms_endpoints(Others, [NewEndpoint | Endpoints])
    end.

-spec create_sms_endpoint(wh_json:object(), binary()) -> api_object().
create_sms_endpoint(Endpoint, <<"amqp">>) -> Endpoint;
create_sms_endpoint(Endpoint, <<"sip">>) ->
    Realm = wh_json:get_value(<<"To-Realm">>, Endpoint),
    Username = wh_json:get_value(<<"To-User">>, Endpoint),
    case lookup_reg(Username, Realm) of
        {'ok', Node} ->
            Options = wh_json:get_value(<<"Endpoint-Options">>, Endpoint, []),
            wh_json:set_values(
              [{<<"Route-ID">>, Node}
               ,{<<"Endpoint-Options">>, wh_json:from_list([{<<"Route-ID">>, Node} | Options])}
              ], Endpoint);
        {'error', _E} -> 'undefined'
    end.

-spec lookup_reg(ne_binary(), ne_binary()) -> {'error', _} |
                                              {'ok', ne_binary()}.
lookup_reg(Username, Realm) ->
    Req = [{<<"Realm">>, Realm}
           ,{<<"Username">>, Username}
           ,{<<"Fields">>, [<<"Registrar-Node">>]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_collect(Req
                                       ,fun wapi_registration:publish_query_req/1
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

-spec extract_device_registrations(wh_json:objects()) -> ne_binaries().
extract_device_registrations(JObjs) ->
    sets:to_list(extract_device_registrations(JObjs, sets:new())).

-spec extract_device_registrations(wh_json:objects(), sets:set()) -> sets:set().
extract_device_registrations([], Set) -> Set;
extract_device_registrations([JObj|JObjs], Set) ->
    Fields = wh_json:get_value(<<"Fields">>, JObj, []),
    S = lists:foldl(fun extract_device_registrar_fold/2, Set, Fields),
    extract_device_registrations(JObjs, S).

-spec extract_device_registrar_fold(wh_json:object(), sets:set()) -> sets:set().
extract_device_registrar_fold(JObj, Set) ->
    case wh_json:get_ne_value(<<"Registrar-Node">>, JObj) of
        'undefined' -> Set;
        AuthId -> sets:add_element(AuthId, Set)
    end.

-spec get_correlated_msg_type(wh_json:object()) ->
                                     {api_binary(), api_binary(), api_binary()}.
-spec get_correlated_msg_type(ne_binary(), wh_json:object()) ->
                                     {api_binary(), api_binary(), api_binary()}.
get_correlated_msg_type(JObj) ->
    get_correlated_msg_type(<<"Call-ID">>, JObj).
get_correlated_msg_type(Key, JObj) ->
    {C, N} = wh_util:get_event_type(JObj),
    {C, N, wh_json:get_value(Key, JObj)}.

-spec wait_for_correlated_message(ne_binary() | whapps_call:call(), ne_binary(), ne_binary(), wh_timeout()) ->
                                         whapps_api_std_return().
wait_for_correlated_message(CallId, Event, Type, Timeout) when is_binary(CallId) ->
    Start = os:timestamp(),
    case whapps_call_command:receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj} ->
            case get_correlated_msg_type(JObj) of
                {<<"error">>, _, CallId} ->
                    lager:debug("channel execution error while waiting for ~s", [CallId]),
                    {'error', JObj};
                {Type, Event, CallId } ->
                    {'ok', JObj};
                {_Type, _Event, _CallId} ->
                    lager:debug("received message (~s , ~s, ~s)",[_Type, _Event, _CallId]),
                    wait_for_correlated_message(CallId, Event, Type, wh_util:decr_timeout(Timeout, Start))
            end
    end;
wait_for_correlated_message(Call, Event, Type, Timeout) ->
    CallId = whapps_call:call_id(Call),
    wait_for_correlated_message(CallId, Event, Type, Timeout).
