%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_local_extension).
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

-include("stepswitch.hrl").

-define(SERVER, ?MODULE).

-record(state, {number_props = [] :: knm_number_options:extra_options()
               ,resource_req :: kapi_offnet_resource:req()
               ,request_handler :: pid()
               ,control_queue :: kz_term:api_binary()
               ,response_queue :: kz_term:api_binary()
               ,queue :: kz_term:api_binary()
               ,timeout :: kz_term:api_reference()
               ,call_id :: kz_term:api_binary()
               }).
-type state() :: #state{}.

-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(CALL_BINDING(CallId), {'call', [{'callid', CallId}
                                       ,{'restrict_to'
                                        ,[<<"CHANNEL_DESTROY">>
                                         ,<<"CHANNEL_REPLACED">>
                                         ,<<"CHANNEL_TRANSFEROR">>
                                         ,<<"CHANNEL_EXECUTE_COMPLETE">>
                                         ,<<"CHANNEL_BRIDGE">>
                                         ]
                                        }
                                       ]
                              }).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(knm_number_options:extra_options(), kapi_offnet_resource:req()) -> kz_types:startlink_ret().
start_link(NumberProps, OffnetReq) ->
    CallId = kapi_offnet_resource:call_id(OffnetReq),
    Bindings = [?CALL_BINDING(CallId)
               ,{'self', []}
               ],
    gen_listener:start_link(?SERVER, [{'bindings', Bindings}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [NumberProps, OffnetReq]).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([knm_number_options:extra_options() | kapi_offnet_resource:req()]) -> {'ok', state()}.
init([NumberProps, OffnetReq]) ->
    kz_util:put_callid(OffnetReq),
    case kapi_offnet_resource:control_queue(OffnetReq) of
        'undefined' -> {'stop', 'normal'};
        ControlQ ->
            {'ok', #state{number_props=NumberProps
                         ,resource_req=OffnetReq
                         ,request_handler=self()
                         ,control_queue=ControlQ
                         ,response_queue=kz_api:server_id(OffnetReq)
                         ,timeout=erlang:send_after(120000, self(), 'local_extension_timeout')
                         ,call_id=kapi_offnet_resource:call_id(OffnetReq)
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
handle_cast({'gen_listener', {'is_consuming', 'true'}}
           ,#state{call_id=CallId
                  ,resource_req=OffnetReq
                  ,request_handler=RequestHandler
                  ,control_queue=ControlQ
                  }=State
           ) ->
    case kapps_call_events:get_event(CallId) of
        {'ok', CallEvt} ->
            lager:info("channel died while we were initializing"),
            handle_channel_destroy(CallEvt, OffnetReq, RequestHandler);
        {'error', 'not_found'} ->
            Payload = build_local_extension(State),
            'ok' = kapi_dialplan:publish_command(ControlQ, Payload),
            lager:debug("sent local extension command to ~s", [ControlQ]),
            {'noreply', State}
    end;
handle_cast({'local_extension_result', _Props}, #state{response_queue='undefined'}=State) ->
    {'stop', 'normal', State};
handle_cast({'local_extension_result', Props}, #state{response_queue=ResponseQ}=State) ->
    kapi_offnet_resource:publish_resp(ResponseQ, Props),
    {'stop', 'normal', State};
handle_cast({'bridged', CallId}, #state{timeout='undefined'}=State) ->
    lager:debug("channel bridged to ~s", [CallId]),
    {'noreply', State};
handle_cast({'bridged', CallId}, #state{timeout=TimerRef}=State) ->
    lager:debug("channel bridged to ~s, canceling timeout", [CallId]),
    _ = erlang:cancel_timer(TimerRef),
    {'noreply', State#state{timeout='undefined'}};
handle_cast({'replaced', ReplacedBy}, #state{}=State) ->
    {'noreply', State#state{call_id=ReplacedBy}};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p~n", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('local_extension_timeout', #state{timeout='undefined'}=State) ->
    {'noreply', State};
handle_info('local_extension_timeout', #state{response_queue=ResponseQ
                                             ,resource_req=OffnetJObj
                                             }=State) ->
    kapi_offnet_resource:publish_resp(ResponseQ, local_extension_timeout(OffnetJObj)),
    {'stop', 'normal', State#state{timeout='undefined'}};
handle_info(_Info, State) ->
    lager:debug("unhandled info: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_call_event:doc(), state()) -> gen_listener:handle_event_return().
handle_event(CallEvt, #state{request_handler=RequestHandler
                            ,resource_req=OffnetReq
                            ,call_id=CallId
                            }) ->
    case get_event_type(CallEvt) of
        {<<"error">>, _EvtName, CallId} ->
            handle_error_event(CallEvt, OffnetReq, RequestHandler);
        {<<"call_event">>, <<"CHANNEL_TRANSFEROR">>, _CallId} ->
            handle_channel_transferor(CallEvt, RequestHandler);
        {<<"call_event">>, <<"CHANNEL_REPLACED">>, _CallId} ->
            handle_channel_replaced(CallEvt, RequestHandler);
        {<<"call_event">>, <<"CHANNEL_DESTROY">>, CallId} ->
            handle_channel_destroy(CallEvt, OffnetReq, RequestHandler);
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, CallId} ->
            handle_channel_execute_complete(CallEvt, OffnetReq, RequestHandler);
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>, CallId} ->
            handle_channel_bridge(CallEvt, RequestHandler);
        _ -> 'ok'
    end,
    {'reply', []}.

-spec handle_error_event(kz_call_event:doc(), kapi_offnet_resource:req(), kz_term:api_pid()) -> 'ok'.
handle_error_event(CallEvt, OffnetReq, RequestHandler) ->
    handle_error_event(CallEvt
                      ,OffnetReq
                      ,RequestHandler
                      ,kapi_dialplan:application_name(kz_call_event:request(CallEvt))
                      ).

-spec handle_error_event(kz_call_event:doc(), kapi_offnet_resource:req(), kz_term:api_pid(), kz_term:ne_binary()) -> 'ok'.
handle_error_event(CallEvt, OffnetReq, RequestHandler, <<"bridge">>) ->
    lager:debug("channel execution error while waiting for execute extension: ~s"
               ,[kz_term:to_binary(kz_json:encode(CallEvt))]
               ),
    gen_listener:cast(RequestHandler, {'local_extension_result', local_extension_error(CallEvt, OffnetReq)});
handle_error_event(_CallEvnt, _OffnetReq, _RequestHandler, _EvtName) ->
    lager:debug("ignoring execution error of ~s", [_EvtName]).

-spec handle_channel_transferor(kz_call_event:doc(), kz_term:api_pid()) -> 'ok'.
handle_channel_transferor(CallEvt, RequestHandler) ->
    Transferor = kz_call_event:other_leg_call_id(CallEvt),
    follow_call_id(RequestHandler, Transferor).

-spec handle_channel_replaced(kz_call_event:doc(), kz_term:api_pid()) -> 'ok'.
handle_channel_replaced(CallEvt, RequestHandler) ->
    ReplacedBy = kz_call_event:replaced_by(CallEvt),
    follow_call_id(RequestHandler, ReplacedBy).

-spec follow_call_id(kz_term:api_pid(), kz_term:ne_binary()) -> 'ok'.
follow_call_id(RequestHandler, CallId) ->
    gen_listener:cast(RequestHandler, {'replaced', CallId}),
    gen_listener:add_binding(RequestHandler, ?CALL_BINDING(CallId)).

-spec handle_channel_destroy(kz_call_event:doc(), kapi_offnet_resource:req(), kz_term:api_pid()) -> 'ok'.
handle_channel_destroy(CallEvt, OffnetReq, RequestHandler) ->
    handle_bridge_result(CallEvt, OffnetReq, RequestHandler).

-spec handle_bridge_result(kz_call_event:doc(), kapi_offnet_resource:req(), kz_term:api_pid()) -> 'ok'.
handle_bridge_result(CallEvt, OffnetReq, RequestHandler) ->
    lager:debug("channel was destroyed while waiting for bridge"),
    Result = case <<"SUCCESS">> =:= kz_call_event:disposition(CallEvt) of
                 'true' -> local_extension_success(OffnetReq);
                 'false' -> local_extension_failure(CallEvt, OffnetReq)
             end,
    gen_listener:cast(RequestHandler, {'local_extension_result', Result}).

-spec handle_channel_execute_complete(kz_call_event:doc(), kapi_offnet_resource:req(), kz_term:api_pid()) -> 'ok'.
handle_channel_execute_complete(CallEvt, OffnetReq, RequestHandler) ->
    handle_channel_execute_complete(CallEvt
                                   ,OffnetReq
                                   ,RequestHandler
                                   ,kz_call_event:application_name(CallEvt)
                                   ).

-spec handle_channel_execute_complete(kz_call_event:doc(), kapi_offnet_resource:req(), kz_term:api_pid(), kz_term:ne_binary()) -> 'ok'.
handle_channel_execute_complete(CallEvt, OffnetReq, RequestHandler, <<"bridge">>) ->
    lager:debug("channel execute complete for bridge"),
    handle_bridge_result(CallEvt, OffnetReq, RequestHandler);
handle_channel_execute_complete(_CallEvt, _OffnetReq, _RequestHandler, _AppName) ->
    lager:debug("ignoring channel_execute_complete for application ~s", [_AppName]).

-spec handle_channel_bridge(kz_call_event:doc(), kz_term:api_pid()) -> 'ok'.
handle_channel_bridge(CallEvt, RequestHandler) ->
    OtherLeg = kz_call_event:other_leg_call_id(CallEvt),
    gen_listener:cast(RequestHandler, {'bridged', OtherLeg}).

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
-spec outbound_flags(kapi_offnet_resource:req()) -> kz_term:api_binary().
outbound_flags(OffnetJObj) ->
    case kapi_offnet_resource:flags(OffnetJObj) of
        [] -> 'undefined';
        Flags -> kz_binary:join(Flags, <<"|">>)
    end.

-spec build_local_extension(state()) -> kz_term:proplist().
build_local_extension(#state{number_props=Props
                            ,resource_req=OffnetJObj
                            ,queue=Q
                            }) ->
    {CIDName, CIDNum} = local_extension_caller_id(OffnetJObj),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    Number = knm_number_options:number(Props),
    AccountId = knm_number_options:account_id(Props),
    OriginalAccountId = kapi_offnet_resource:account_id(OffnetJObj),
    ResellerId = kz_services_reseller:get_id(OriginalAccountId),
    {CEDNum, CEDName} = local_extension_callee_id(OffnetJObj, Number),
    Realm = get_account_realm(AccountId),
    FromRealm = get_account_realm(OriginalAccountId),
    FromURI = <<"sip:", CIDNum/binary, "@", Realm/binary>>,
    CCVsOrig = kapi_offnet_resource:custom_channel_vars(OffnetJObj, kz_json:new()),
    CAVs = kapi_offnet_resource:custom_application_vars(OffnetJObj),

    CCVs = kz_json:set_values([{<<"Ignore-Display-Updates">>, <<"true">>}
                              ,{<<"Account-ID">>, OriginalAccountId}
                              ,{<<"Reseller-ID">>, ResellerId}
                              ,{<<"Outbound-Flags">>, outbound_flags(OffnetJObj)}
                              ]
                             ,CCVsOrig
                             ),

    CCVUpdates = kz_json:from_list(
                   [{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Inception">>, <<Number/binary, "@", Realm/binary>>}
                   ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Account-ID">>, AccountId}
                   ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Retain-CID">>, kz_json:get_value(<<"Retain-CID">>, CCVsOrig)}
                   ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "From-URI">>, FromURI}
                   ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Inception-Account-ID">>, OriginalAccountId}
                   ,{<<?CHANNEL_LOOPBACK_HEADER_PREFIX, "Resource-Type">>, <<"onnet-origination">>}
                   ,{<<"Resource-ID">>, AccountId}
                   ,{<<"Loopback-Request-URI">>, <<Number/binary, "@", Realm/binary>>}
                   ,{<<"Resource-Type">>, <<"onnet-termination">>}
                   ]),

    Endpoint = kz_json:from_list(
                 [{<<"Invite-Format">>, <<"loopback">>}
                 ,{<<"Route">>, Number}
                 ,{<<"To-DID">>, Number}
                 ,{<<"To-Realm">>, FromRealm}
                 ,{<<"Custom-Channel-Vars">>, CCVUpdates}
                 ,{<<"Outbound-Caller-ID-Name">>, CIDName}
                 ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
                 ,{<<"Outbound-Callee-ID-Name">>, CEDName}
                 ,{<<"Outbound-Callee-ID-Number">>, CEDNum}
                 ,{<<"Caller-ID-Name">>, CIDName}
                 ,{<<"Caller-ID-Number">>, CIDNum}
                 ,{<<"Ignore-Early-Media">>, 'true'}
                 ,{<<"Enable-T38-Fax">>, 'false'}
                 ,{<<"Enable-T38-Fax-Request">>, 'false'}
                 ]),

    props:filter_undefined(
      [{<<"Application-Name">>, <<"bridge">>}
      ,{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetJObj)}
      ,{<<"Caller-ID-Name">>, CIDName}
      ,{<<"Caller-ID-Number">>, CIDNum}
      ,{<<"Custom-Application-Vars">>, CAVs}
      ,{<<"Custom-Channel-Vars">>, CCVs}
      ,{<<"Dial-Endpoint-Method">>, <<"single">>}
      ,{<<"Endpoints">>, [Endpoint]}
      ,{<<"Loopback-Bowout">>, <<"false">>}
      ,{<<"Media">>, <<"process">>}
      ,{<<"Outbound-Callee-ID-Name">>, CEDName}
      ,{<<"Outbound-Callee-ID-Number">>, CEDNum}
      ,{<<"Outbound-Caller-ID-Name">>, CIDName}
      ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
      ,{<<"Simplify-Loopback">>, <<"false">>}
       | kz_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
      ]).

-spec get_account_realm(kz_term:ne_binary()) -> kz_term:ne_binary().
get_account_realm(AccountId) ->
    case kzd_accounts:fetch_realm(AccountId) of
        'undefined' -> AccountId;
        Realm -> Realm
    end.

-spec local_extension_caller_id(kapi_offnet_resource:req()) ->
                                       {kz_term:api_ne_binary(), kz_term:api_ne_binary()}.
local_extension_caller_id(OffnetReq) ->
    {kz_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                               ,<<"Emergency-Caller-ID-Name">>
                               ]
                              ,OffnetReq
                              )
    ,kz_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                               ,<<"Emergency-Caller-ID-Number">>
                               ]
                              ,OffnetReq
                              )
    }.

-spec local_extension_callee_id(kapi_offnet_resource:req(), kz_term:ne_binary()) -> {kz_term:api_binary(), kz_term:api_binary()}.
local_extension_callee_id(OffnetReq, Number) ->
    {kz_json:get_ne_binary_value(<<"Outbound-Callee-ID-Number">>, OffnetReq, Number)
    ,kz_json:get_ne_binary_value(<<"Outbound-Callee-ID-Name">>, OffnetReq, Number)
    }.

-spec local_extension_timeout(kapi_offnet_resource:req()) -> kz_term:proplist().
local_extension_timeout(OffnetReq) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, kz_api:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kz_api:msg_id(OffnetReq, <<>>)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, <<"local extension request timed out">>}
    ,{<<"To-DID">>, kapi_offnet_resource:to_did(OffnetReq)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec local_extension_error(kz_call_event:doc(), kapi_offnet_resource:req()) -> kz_term:proplist().
local_extension_error(CallEvt, OffnetReq) ->
    lager:debug("error during outbound request: ~s", [kz_term:to_binary(kz_json:encode(CallEvt))]),
    [{<<"Call-ID">>, kz_api:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kz_api:msg_id(OffnetReq, <<>>)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, kz_call_event:error_message(CallEvt, <<"failed to process request">>)}
    ,{<<"To-DID">>, kapi_offnet_resource:to_did(OffnetReq)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec local_extension_success(kapi_offnet_resource:req()) -> kz_term:proplist().
local_extension_success(OffnetReq) ->
    lager:debug("local extension request successfully completed"),
    [{<<"Call-ID">>, kz_api:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kz_api:msg_id(OffnetReq, <<>>)}
    ,{<<"Response-Message">>, <<"SUCCESS">>}
    ,{<<"Response-Code">>, <<"sip:200">>}
    ,{<<"Resource-Response">>, kz_json:new()}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec local_extension_failure(kz_call_event:doc(), kapi_offnet_resource:req()) -> kz_term:proplist().
local_extension_failure(CallEvt, OffnetReq) ->
    lager:debug("resources for outbound request failed: ~s"
               ,[kz_call_event:disposition(CallEvt)]
               ),
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, response_message(CallEvt)}
    ,{<<"Response-Code">>, kz_call_event:hangup_code(CallEvt)}
    ,{<<"Resource-Response">>, CallEvt}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec response_message(kz_call_event:doc()) -> kz_term:api_ne_binary().
response_message(CallEvt) ->
    case kz_call_event:application_response(CallEvt) of
        'undefined' -> kz_call_event:hangup_cause(CallEvt);
        AppResp -> AppResp
    end.

-spec get_event_type(kz_call_event:doc()) ->
                            {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}.
get_event_type(JObj) ->
    {C, E} = kz_util:get_event_type(JObj),
    {C, E, kz_call_event:call_id(JObj)}.
