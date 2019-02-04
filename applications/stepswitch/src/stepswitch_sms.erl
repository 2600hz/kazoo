%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_sms).

-behaviour(gen_listener).

-export([start_link/2]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-export([handle_message_delivery/2]).

-include("stepswitch.hrl").

-define(SERVER, ?MODULE).

-record(state, {endpoints = [] :: kz_json:objects()
               ,resource_req :: kapi_offnet_resource:req()
               ,request_handler :: pid()
               ,control_queue :: kz_term:api_binary()
               ,response_queue :: kz_term:api_binary()
               ,queue :: kz_term:api_binary()
               ,message = [] :: kz_term:proplist()
               ,messages = queue:new() :: queue:queue()
               }).
-type state() :: #state{}.

-define(RESPONDERS, [{{?MODULE, 'handle_message_delivery'}
                     ,[{<<"message">>, <<"delivery">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(ATOM(X), kz_term:to_atom(X, 'true')).
-define(SMS_POOL(A,B,C), ?ATOM(<<A/binary,"_", B/binary, "_", C/binary>>) ).

%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_json:objects(), kapi_offnet_resource:req()) -> kz_types:startlink_ret().
start_link(Endpoints, OffnetReq) ->
    Bindings = [{'self', []}],
    gen_listener:start_link(?SERVER, [{'bindings', Bindings}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Endpoints, OffnetReq]).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([kz_json:objects() | kapi_offnet_resource:req()]) -> {'ok', state()}.
init([Endpoints, OffnetReq]) ->
    _ = kapi_offnet_resource:put_callid(OffnetReq),
    CallId = kapi_offnet_resource:call_id(OffnetReq),
    case kapi_offnet_resource:control_queue(OffnetReq) of
        'undefined' ->
            lager:debug("control-Queue is undefined for Call-ID ~s, exiting.", [CallId]),
            {'stop', 'normal'};
        ControlQ ->
            {'ok', #state{endpoints=Endpoints
                         ,resource_req=OffnetReq
                         ,request_handler=self()
                         ,control_queue=ControlQ
                         ,response_queue=kapi_offnet_resource:server_id(OffnetReq)
                         }}
    end.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call: ~p", [_Request]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'kz_amqp_channel', _}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{queue=Q}};
handle_cast({'gen_listener', {'is_consuming', 'true'}}, State) ->
    {'noreply', build_sms(State)};
handle_cast({'sms_result', _Props}, #state{response_queue='undefined'}=State) ->
    {'stop', 'normal', State};
handle_cast({'sms_result', Props}, #state{response_queue=ResponseQ}=State) ->
    kapi_offnet_resource:publish_resp(ResponseQ, Props),
    {'stop', 'normal', State};
handle_cast({'sms_success', JObj}, #state{resource_req=OffnetReq}=State) ->
    gen_listener:cast(self(), {'sms_result', sms_success(JObj, OffnetReq)}),
    {'noreply', State};
handle_cast({'sms_failure', JObj}, #state{resource_req=OffnetReq}=State) ->
    gen_listener:cast(self(), {'sms_result', sms_failure(JObj, OffnetReq)}),
    {'noreply', State};
handle_cast({'sms_error', JObj}, #state{resource_req=OffnetReq}=State) ->
    gen_listener:cast(self(), {'sms_result', sms_error(JObj, OffnetReq)}),
    {'noreply', State};
handle_cast('next_message', #state{message=API
                                  ,messages=Queue
                                  ,resource_req=JObj
                                  ,response_queue=ResponseQ
                                  }=State) ->
    case queue:out(Queue) of
        {'empty', _} ->
            kapi_offnet_resource:publish_resp(ResponseQ, sms_timeout(JObj)),
            {'stop', 'normal', State};
        {{'value', Endpoint}, NewQ} ->
            send(Endpoint, API),
            {'noreply', State#state{messages=NewQ}}
    end;
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p~n", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('sms_timeout', State) ->
    gen_listener:cast(self(), 'next_message'),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled info: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_message_delivery(kz_json:object(), kz_term:proplist()) -> no_return().
handle_message_delivery(JObj, Props) ->
    _ = kz_util:put_callid(JObj),
    Server = props:get_value('server',Props),
    'true' = kapi_sms:delivery_v(JObj),
    case kz_json:is_true(<<"Delivery-Failure">>, JObj) of
        'true'  -> gen_listener:cast(Server, {'sms_failure', JObj});
        'false' -> gen_listener:cast(Server, {'sms_success', JObj})
    end.

-spec send(kz_json:object(), kz_term:proplist()) -> no_return().
send(Endpoint, API) ->
    Type = kz_json:get_value(<<"Endpoint-Type">>, Endpoint, <<"sip">>),
    send(Type, Endpoint, API).

-spec send(binary(), kz_json:object(), kz_term:proplist()) -> no_return().
send(<<"sip">>, Endpoint, API) ->
    Options = kz_json:to_proplist(kz_json:get_value(<<"Endpoint-Options">>, Endpoint, [])),
    Payload = props:set_values([{<<"Endpoints">>, [Endpoint]}
                                | Options
                               ]
                              ,API
                              ),
    CallId = props:get_value(<<"Call-ID">>, Payload),
    lager:debug("sending sms and waiting for response ~s", [CallId]),
    kz_amqp_worker:cast(Payload, fun kapi_sms:publish_message/1),
    erlang:send_after(60000, self(), 'sms_timeout');
send(<<"amqp">>, Endpoint, API) ->
    CallId = props:get_value(<<"Call-ID">>, API),
    Options = kz_json:to_proplist(kz_json:get_value(<<"Endpoint-Options">>, Endpoint, [])),
    CCVs = kz_json:merge_jobjs(kz_json:get_json_value(<<"Custom-Channel-Vars">>, Endpoint, kz_json:new())
                              ,kz_json:filter(fun filter_smpp/1, props:get_value(<<"Custom-Channel-Vars">>, API, kz_json:new()))
                              ),
    Props = kz_json:to_proplist(Endpoint) ++ Options,
    Payload = props:set_value(<<"Custom-Channel-Vars">>, CCVs, props:set_values(Props, API)),
    Broker = kz_json:get_value([<<"Endpoint-Options">>, <<"AMQP-Broker">>], Endpoint),
    BrokerName = kz_json:get_value([<<"Endpoint-Options">>, <<"Broker-Name">>], Endpoint),
    Exchange = kz_json:get_value([<<"Endpoint-Options">>, <<"Exchange-ID">>], Endpoint),
    RouteId = kz_json:get_value([<<"Endpoint-Options">>, <<"Route-ID">>], Endpoint),
    ExchangeType = kz_json:get_value([<<"Endpoint-Options">>, <<"Exchange-Type">>], Endpoint, <<"topic">>),
    ExchangeOptions = amqp_exchange_options(kz_json:get_value([<<"Endpoint-Options">>, <<"Exchange-Options">>], Endpoint)),
    maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName),

    lager:debug("sending sms and not waiting for response ~s", [CallId]),
    case send_amqp_sms(Payload, ?SMS_POOL(Exchange, RouteId, BrokerName)) of
        'ok' ->
            send_success(API, CallId);
        {'error', 'timeout'} ->
            send_timeout_error(API, CallId);
        {'error', Reason} ->
            send_error(API, CallId, Reason)
    end.

-spec filter_smpp({kz_term:ne_binary(), any()}) -> boolean().
filter_smpp({<<"SMPP-", _/binary>>, _}) -> 'true';
filter_smpp(_) -> 'false'.

-spec send_success(kz_term:proplist(), kz_term:ne_binary()) -> 'ok'.
send_success(API, CallId) ->
    DeliveryProps = [{<<"Delivery-Result-Code">>, <<"sip:200">>}
                    ,{<<"Status">>, <<"Success">>}
                    ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, API)}
                    ,{<<"Call-ID">>, CallId}
                     | kz_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
                    ],
    gen_listener:cast(self(), {'sms_success', kz_json:from_list(DeliveryProps)}).

-spec send_timeout_error(kz_term:proplist(), kz_term:ne_binary()) -> 'ok'.
send_timeout_error(API, CallId) ->
    DeliveryProps = [{<<"Delivery-Result-Code">>, <<"sip:500">>}
                    ,{<<"Delivery-Failure">>, 'true'}
                    ,{<<"Error-Code">>, 500}
                    ,{<<"Error-Message">>, <<"timeout">>}
                    ,{<<"Status">>, <<"Failed">>}
                    ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, API)}
                    ,{<<"Call-ID">>, CallId}
                     | kz_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
                    ],
    gen_listener:cast(self(), {'sms_error', kz_json:from_list(DeliveryProps)}).

-spec send_error(kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_error(API, CallId, Reason) ->
    DeliveryProps = [{<<"Delivery-Result-Code">>, <<"sip:500">>}
                    ,{<<"Delivery-Failure">>, 'true'}
                    ,{<<"Error-Code">>, 500}
                    ,{<<"Error-Message">>, kz_term:error_to_binary(Reason)}
                    ,{<<"Status">>, <<"Failed">>}
                    ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, API)}
                    ,{<<"Call-ID">>, CallId}
                     | kz_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
                    ],
    gen_listener:cast(self(), {'sms_error', kz_json:from_list(DeliveryProps)}).

-spec amqp_exchange_options(kz_term:api_object()) -> kz_term:proplist().
amqp_exchange_options('undefined') -> [];
amqp_exchange_options(JObj) ->
    [{kz_term:to_atom(K, 'true'), V}
     || {K, V} <- kz_json:to_proplist(JObj)
    ].

-spec send_amqp_sms(kz_term:proplist(), atom()) ->
                           'ok' |
                           {'error', kz_term:ne_binary() | 'timeout'}.
send_amqp_sms(Payload, Pool) ->
    kapps_sms_command:send_amqp_sms(Payload, Pool).

-spec maybe_add_broker(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(), kz_term:proplist(), kz_term:api_binary()) -> 'ok'.
maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName) ->
    PoolExists = kz_amqp_sup:pool_pid(?SMS_POOL(Exchange, RouteId, BrokerName)) =/= 'undefined',
    maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName, PoolExists).

-spec maybe_add_broker(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(), kz_term:proplist(), kz_term:api_binary(), boolean()) -> 'ok'.
maybe_add_broker(_Broker, _Exchange, _RouteId, _ExchangeType, _ExchangeOptions, _BrokerName, 'true') -> 'ok';
maybe_add_broker(Broker, Exchange, RouteId, ExchangeType, ExchangeOptions, BrokerName, 'false') ->
    Exchanges = [{Exchange, ExchangeType, ExchangeOptions}],
    kz_amqp_sup:add_amqp_pool(?SMS_POOL(Exchange, RouteId, BrokerName), Broker, 5, 5, [], Exchanges, 'true'),
    'ok'.

-spec build_sms(state()) -> state().
build_sms(#state{endpoints=Endpoints
                ,resource_req=OffnetReq
                ,queue=Q
                }=State) ->
    {CIDNum, CIDName} = bridge_caller_id(Endpoints, OffnetReq),
    gen_listener:cast(self(), 'next_message'),
    State#state{messages=queue:from_list(maybe_endpoints_format_from(Endpoints, CIDNum, OffnetReq))
               ,message=build_sms_base({CIDNum, CIDName}, OffnetReq, Q)
               }.

-spec build_sms_base({binary(), binary()}, kapi_offnet_resource:req(), binary()) -> kz_term:proplist().
build_sms_base({CIDNum, CIDName}, OffnetReq, Q) ->
    AccountId = kapi_offnet_resource:account_id(OffnetReq),
    AccountRealm = kapi_offnet_resource:account_realm(OffnetReq),
    CCVs = kapi_offnet_resource:custom_channel_vars(OffnetReq, kz_json:new()),
    CCVUpdates = props:filter_undefined(
                   [{<<"Ignore-Display-Updates">>, <<"true">>}
                   ,{<<"Account-ID">>, AccountId}
                   ,{<<"Account-Realm">>, AccountRealm}
                   ,{<<"From-URI">>, bridge_from_uri(CIDNum, OffnetReq)}
                   ,{<<"Reseller-ID">>, kz_services_reseller:get_id(AccountId)}
                   ]),
    props:filter_undefined(
      [{<<"Application-Name">>, <<"send">>}
      ,{<<"Dial-Endpoint-Method">>, <<"single">>}
      ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
      ,{<<"Outbound-Caller-ID-Name">>, CIDName}
      ,{<<"Caller-ID-Number">>, CIDNum}
      ,{<<"Caller-ID-Name">>, CIDName}
      ,{<<"Presence-ID">>, kapi_offnet_resource:presence_id(OffnetReq)}
      ,{<<"Custom-Channel-Vars">>, kz_json:set_values(CCVUpdates, CCVs)}
      ,{<<"Custom-SIP-Headers">>, stepswitch_util:get_sip_headers(OffnetReq)}
      ,{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
      ,{<<"Outbound-Callee-ID-Number">>, kapi_offnet_resource:outbound_callee_id_number(OffnetReq)}
      ,{<<"Outbound-Callee-ID-Name">>, kapi_offnet_resource:outbound_callee_id_name(OffnetReq)}
      ,{<<"Message-ID">>, kapi_offnet_resource:message_id(OffnetReq)}
      ,{<<"Body">>, kapi_offnet_resource:body(OffnetReq)}
       | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
      ]).

-spec maybe_endpoints_format_from(kz_json:objects(), kz_term:api_binary(), kapi_offnet_resource:req()) ->
                                         kz_json:objects().
maybe_endpoints_format_from([], _ , _) -> [];
maybe_endpoints_format_from(Endpoints, 'undefined', _) -> Endpoints;
maybe_endpoints_format_from(Endpoints, CIDNum, OffnetReq) ->
    DefaultRealm = stepswitch_util:default_realm(OffnetReq),
    [maybe_endpoint_format_from(Endpoint, CIDNum, DefaultRealm)
     || Endpoint <- Endpoints
    ].

-spec maybe_endpoint_format_from(kz_json:object(), kz_term:ne_binary(), kz_term:api_binary()) ->
                                        kz_json:object().
maybe_endpoint_format_from(Endpoint, CIDNum, DefaultRealm) ->
    CCVs = kz_json:get_json_value(<<"Custom-Channel-Vars">>, Endpoint, kz_json:new()),
    case kz_json:is_true(<<"Format-From-URI">>, CCVs) of
        'true' -> endpoint_format_from(Endpoint, CIDNum, DefaultRealm);
        'false' ->
            kz_json:set_value(<<"Custom-Channel-Vars">>
                             ,kz_json:delete_keys([<<"Format-From-URI">>
                                                  ,<<"From-URI-Realm">>
                                                  ]
                                                 ,CCVs
                                                 )
                             ,Endpoint
                             )
    end.

-spec endpoint_format_from(kz_json:object(), kz_term:ne_binary(), kz_term:api_binary()) -> kz_json:object().
endpoint_format_from(Endpoint, CIDNum, DefaultRealm) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, kz_json:new()),
    Realm = kz_json:get_value(<<"From-URI-Realm">>, CCVs, DefaultRealm),
    case is_binary(Realm) of
        'false' ->
            kz_json:set_value(<<"Custom-Channel-Vars">>
                             ,kz_json:delete_keys([<<"Format-From-URI">>
                                                  ,<<"From-URI-Realm">>
                                                  ]
                                                 ,CCVs
                                                 )
                             ,Endpoint
                             );
        'true' ->
            FromURI = <<"sip:", CIDNum/binary, "@", Realm/binary>>,
            lager:debug("setting resource ~s from-uri to ~s"
                       ,[kz_json:get_value(<<"Resource-ID">>, CCVs)
                        ,FromURI
                        ]),
            UpdatedCCVs = kz_json:set_value(<<"From-URI">>, FromURI, CCVs),
            kz_json:set_value(<<"Custom-Channel-Vars">>
                             ,kz_json:delete_keys([<<"Format-From-URI">>
                                                  ,<<"From-URI-Realm">>
                                                  ]
                                                 ,UpdatedCCVs
                                                 )
                             ,Endpoint
                             )
    end.

-spec bridge_caller_id(kz_json:objects(), kapi_offnet_resource:req()) ->
                              {kz_term:api_binary(), kz_term:api_binary()}.
bridge_caller_id(Endpoints, JObj) ->
    case contains_emergency_endpoints(Endpoints) of
        'true' -> bridge_emergency_caller_id(JObj);
        'false' -> bridge_caller_id(JObj)
    end.

-spec bridge_emergency_caller_id(kapi_offnet_resource:req()) ->
                                        {kz_term:api_binary(), kz_term:api_binary()}.
bridge_emergency_caller_id(OffnetReq) ->
    lager:debug("outbound call is using an emergency route, attempting to set CID accordingly"),
    {maybe_emergency_cid_number(OffnetReq)
    ,stepswitch_bridge:bridge_emergency_cid_name(OffnetReq)
    }.

-spec bridge_caller_id(kapi_offnet_resource:req()) ->
                              {kz_term:ne_binary(), kz_term:ne_binary()}.
bridge_caller_id(OffnetReq) ->
    {stepswitch_bridge:bridge_outbound_cid_number(OffnetReq)
    ,stepswitch_bridge:bridge_outbound_cid_name(OffnetReq)
    }.

-spec bridge_from_uri(kz_term:api_binary(), kapi_offnet_resource:req()) ->
                             kz_term:api_binary().
bridge_from_uri(CIDNum, OffnetReq) ->
    Realm = stepswitch_util:default_realm(OffnetReq),
    case (kapps_config:get_is_true(?APP_NAME, <<"format_from_uri">>, 'false')
          orelse kapi_offnet_resource:format_from_uri(OffnetReq)
         )
        andalso (is_binary(CIDNum)
                 andalso is_binary(Realm)
                )
    of
        'false' -> 'undefined';
        'true' ->
            FromURI = <<"sip:", CIDNum/binary, "@", Realm/binary>>,
            lager:debug("setting bridge from-uri to ~s", [FromURI]),
            FromURI
    end.

-spec maybe_emergency_cid_number(kapi_offnet_resource:req()) ->
                                        kz_term:api_binary().
maybe_emergency_cid_number(OffnetReq) ->
    %% NOTE: if this request had a hunt-account-id then we
    %%   are assuming it was for a local resource (at the
    %%   time of this commit offnet DB is still in use)
    case kapi_offnet_resource:hunt_account_id(OffnetReq) of
        'undefined' -> emergency_cid_number(OffnetReq);
        _Else ->
            stepswitch_bridge:bridge_emergency_cid_number(OffnetReq)
    end.

-spec emergency_cid_number(kapi_offnet_resource:req()) -> kz_term:ne_binary().
emergency_cid_number(OffnetReq) ->
    AccountId = kapi_offnet_resource:account_id(OffnetReq),
    Candidates = [kapi_offnet_resource:emergency_caller_id_number(OffnetReq)
                 ,kapi_offnet_resource:outbound_caller_id_number(OffnetReq)
                 ],
    Requested = stepswitch_bridge:bridge_emergency_cid_number(OffnetReq),
    lager:debug("ensuring requested CID is emergency enabled: ~s", [Requested]),
    EnabledNumbers = knm_numbers:emergency_enabled(AccountId),
    emergency_cid_number(Requested, Candidates, EnabledNumbers).

-spec emergency_cid_number(kz_term:ne_binary(), kz_term:api_binaries(), kz_term:ne_binaries()) -> kz_term:ne_binary().
%% if there are no emergency enabled numbers then either use the global system default
%% or the requested (if there isn't one)
emergency_cid_number(Requested, _, []) ->
    case ?DEFAULT_EMERGENCY_CID_NUMBER of
        'undefined' -> Requested;
        DefaultEmergencyCID -> DefaultEmergencyCID
    end;
%% If neither their emergency cid or outgoing cid is emergency enabled but their account
%% has other numbers with emergency then use the first...
emergency_cid_number(_, [], [EmergencyEnabled|_]) -> EmergencyEnabled;
%% due to the way we built the candidates list it can contain the atom 'undefined'
%% handle that condition (ignore)
emergency_cid_number(Requested, ['undefined'|Candidates], EmergencyEnabled) ->
    emergency_cid_number(Requested, Candidates, EmergencyEnabled);
%% check if the first non-atom undefined element in the list is in the list of
%% emergency enabled numbers, if so use it otherwise keep checking.
emergency_cid_number(Requested, [Candidate|Candidates], EmergencyEnabled) ->
    case lists:member(Candidate, EmergencyEnabled) of
        'true' -> Candidate;
        'false' -> emergency_cid_number(Requested, Candidates, EmergencyEnabled)
    end.

-spec contains_emergency_endpoints(kz_json:objects()) -> boolean().
contains_emergency_endpoints([]) -> 'false';
contains_emergency_endpoints([Endpoint|Endpoints]) ->
    kz_json:is_true([<<"Custom-Channel-Vars">>, <<"Emergency-Resource">>], Endpoint)
        orelse contains_emergency_endpoints(Endpoints).

-spec sms_timeout(kapi_offnet_resource:req()) -> kz_term:proplist().
sms_timeout(OffnetReq) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, <<"bridge request timed out">>}
    ,{<<"To-DID">>, kapi_offnet_resource:to_did(OffnetReq)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec sms_error(kz_json:object(), kapi_offnet_resource:req()) -> kz_term:proplist().
sms_error(JObj, OffnetReq) ->
    lager:debug("error during outbound request: ~s", [kz_term:to_binary(kz_json:encode(JObj))]),
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, kz_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
    ,{<<"To-DID">>, kapi_offnet_resource:to_did(OffnetReq)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec sms_success(kz_json:object(), kapi_offnet_resource:req()) -> kz_term:proplist().
sms_success(JObj, OffnetReq) ->
    lager:debug("outbound request successfully completed"),
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"SUCCESS">>}
    ,{<<"Response-Code">>, <<"sip:200">>}
    ,{<<"Resource-Response">>, JObj}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec sms_failure(kz_json:object(), kapi_offnet_resource:req()) -> kz_term:proplist().
sms_failure(JObj, OffnetReq) ->
    lager:debug("resources for outbound request failed: ~s"
               ,[kz_json:get_value(<<"Disposition">>, JObj)]
               ),
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, kz_json:get_first_defined([<<"Application-Response">>
                                                        ,<<"Hangup-Cause">>
                                                        ], JObj)}
    ,{<<"Response-Code">>, kz_json:get_value(<<"Hangup-Code">>, JObj)}
    ,{<<"Resource-Response">>, JObj}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
