%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_bridge).
-behaviour(gen_listener).

-export([start_link/2]).

-export([bridge_emergency_cid_number/1
        ,bridge_outbound_cid_number/1
        ,bridge_emergency_cid_name/1
        ,bridge_outbound_cid_name/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("stepswitch.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(SERVER, ?MODULE).

-record(state, {endpoints = [] :: kz_json:objects()
               ,resource_req :: kapi_offnet_resource:req()
               ,request_handler :: api_pid()
               ,control_queue :: api_binary()
               ,response_queue :: api_binary()
               ,queue :: api_binary()
               ,timeout :: api_reference()
               ,call_id :: api_binary()
               }).
-type state() :: #state{}.

-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(CALL_BINDING(CallId), {'call', [{'callid', CallId}
                                       ,{'restrict_to',
                                         [<<"CHANNEL_DESTROY">>
                                         ,<<"CHANNEL_REPLACED">>
                                         ,<<"CHANNEL_TRANSFEROR">>
                                         ,<<"CHANNEL_EXECUTE_COMPLETE">>
                                         ,<<"CHANNEL_BRIDGE">>
                                         ]
                                        }
                                       ]
                              }).

-define(SHOULD_ENSURE_E911_CID_VALID
       ,kapps_config:get_is_true(?SS_CONFIG_CAT, <<"ensure_valid_emergency_cid">>, 'false')
       ).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(kz_json:objects(), kapi_offnet_resource:req()) -> startlink_ret().
start_link(Endpoints, OffnetReq) ->
    CallId = kapi_offnet_resource:call_id(OffnetReq),
    Bindings = [?CALL_BINDING(CallId)
               ,{'self', []}
               ],
    gen_listener:start_link(?SERVER, [{'bindings', Bindings}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Endpoints, OffnetReq]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([kz_json:objects() | kapi_offnet_resource:req()]) -> {'ok', state()}.
init([Endpoints, OffnetReq]) ->
    kapi_offnet_resource:put_callid(OffnetReq),
    case kapi_offnet_resource:control_queue(OffnetReq) of
        'undefined' -> {'stop', 'normal'};
        ControlQ ->
            {'ok', #state{endpoints=Endpoints
                         ,resource_req=OffnetReq
                         ,request_handler=self()
                         ,control_queue=ControlQ
                         ,response_queue=kapi_offnet_resource:server_id(OffnetReq)
                         ,timeout=erlang:send_after(30000, self(), 'bridge_timeout')
                         ,call_id=kapi_offnet_resource:call_id(OffnetReq)
                         }}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call: ~p", [_Request]),
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'kz_amqp_channel', _}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{queue=Q}};
handle_cast({'gen_listener', {'is_consuming', 'true'}}, State) ->
    _ = maybe_bridge(State),
    {'noreply', State};
handle_cast({'bridge_result', _Props}, #state{response_queue='undefined'}=State) ->
    {'stop', 'normal', State};
handle_cast({'bridge_result', Props}, #state{response_queue=ResponseQ}=State) ->
    kapi_offnet_resource:publish_resp(ResponseQ, Props),
    {'stop', 'normal', State};
handle_cast({'bridged', _CallId}, #state{timeout='undefined'}=State) ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info('bridge_timeout', #state{timeout='undefined'}=State) ->
    {'noreply', State};
handle_info('bridge_timeout', #state{response_queue=ResponseQ
                                    ,resource_req=OffnetReq
                                    }=State) ->
    kapi_offnet_resource:publish_resp(ResponseQ, bridge_timeout(OffnetReq)),
    {'stop', 'normal', State#state{timeout='undefined'}};
handle_info(_Info, State) ->
    lager:debug("unhandled info: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, #state{request_handler=RequestHandler
                         ,resource_req=OffnetReq
                         ,call_id=CallId
                         }) ->
    case get_event_type(JObj) of
        {<<"error">>, _, _} ->
            <<"bridge">> = kz_json:get_value([<<"Request">>, <<"Application-Name">>], JObj),
            lager:debug("channel execution error while waiting for bridge: ~s"
                       ,[kz_term:to_binary(kz_json:encode(JObj))]
                       ),
            gen_listener:cast(RequestHandler, {'bridge_result', bridge_error(JObj, OffnetReq)});
        {<<"call_event">>, <<"CHANNEL_TRANSFEROR">>, _} ->
            Transferor = kz_call_event:other_leg_call_id(JObj),
            gen_listener:cast(RequestHandler, {'replaced', Transferor}),
            gen_listener:add_binding(RequestHandler, ?CALL_BINDING(Transferor));
        {<<"call_event">>, <<"CHANNEL_REPLACED">>, _} ->
            ReplacedBy = kz_call_event:replaced_by(JObj),
            gen_listener:cast(RequestHandler, {'replaced', ReplacedBy}),
            gen_listener:add_binding(RequestHandler, ?CALL_BINDING(ReplacedBy));
        {<<"call_event">>, <<"CHANNEL_DESTROY">>, CallId} ->
            lager:debug("channel was destroyed while waiting for bridge"),
            Result = case <<"SUCCESS">> =:= kz_json:get_value(<<"Disposition">>, JObj) of
                         'true' -> bridge_success(JObj, OffnetReq);
                         'false' -> bridge_failure(JObj, OffnetReq)
                     end,
            gen_listener:cast(RequestHandler, {'bridge_result', Result});
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, CallId} ->
            <<"bridge">> = kz_json:get_value(<<"Application-Name">>, JObj),
            lager:debug("channel execute complete for bridge"),
            Result = case <<"SUCCESS">> =:= kz_json:get_value(<<"Disposition">>, JObj) of
                         'true' -> bridge_success(JObj, OffnetReq);
                         'false' -> bridge_failure(JObj, OffnetReq)
                     end,
            gen_listener:cast(RequestHandler, {'bridge_result', Result});
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>, CallId} ->
            OtherLeg = kz_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
            gen_listener:cast(RequestHandler, {'bridged', OtherLeg});
        _ -> 'ok'
    end,
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_bridge(state()) -> 'ok'.
maybe_bridge(#state{endpoints=Endpoints
                   ,resource_req=OffnetReq
                   ,control_queue=ControlQ
                   }=State) ->
    case contains_emergency_endpoints(Endpoints) of
        'true' -> maybe_bridge_emergency(State);
        'false' ->
            Name = bridge_outbound_cid_name(OffnetReq),
            Number = bridge_outbound_cid_number(OffnetReq),
            kapi_dialplan:publish_command(ControlQ
                                         ,build_bridge(State, Number, Name)
                                         ),
            lager:debug("sent bridge command to ~s", [ControlQ])
    end.

-spec maybe_bridge_emergency(state()) -> 'ok'.
maybe_bridge_emergency(#state{resource_req=OffnetReq
                             ,control_queue=ControlQ
                             }=State) ->
    %% NOTE: if this request had a hunt-account-id then we
    %%   are assuming it was for a local resource (at the
    %%   time of this commit offnet DB is still in use)
    Name = bridge_emergency_cid_name(OffnetReq),
    case kapi_offnet_resource:hunt_account_id(OffnetReq) of
        'undefined' ->
            Number = find_emergency_number(OffnetReq),
            maybe_deny_emergency_bridge(State, Number, Name);
        _Else ->
            Number = bridge_emergency_cid_number(OffnetReq),
            lager:debug("not enforcing emergency caller id validation when using resource from account ~s", [_Else]),
            kapi_dialplan:publish_command(ControlQ, build_bridge(State, Number, Name)),
            lager:debug("sent bridge command to ~s", [ControlQ])
    end.

-spec maybe_deny_emergency_bridge(state(), api_binary(), api_binary()) -> 'ok'.
maybe_deny_emergency_bridge(#state{resource_req=OffnetReq}=State, 'undefined', Name) ->
    AccountId = kapi_offnet_resource:account_id(OffnetReq),
    case kapps_config:get_is_true(?SS_CONFIG_CAT
                                 ,<<"deny_invalid_emergency_cid">>
                                 ,'false'
                                 )
    of
        'true' -> deny_emergency_bridge(State);
        'false' ->
            Number = default_emergency_number(kz_privacy:anonymous_caller_id_number(AccountId)),
            maybe_deny_emergency_bridge(State, Number, Name)
    end;
maybe_deny_emergency_bridge(#state{control_queue=ControlQ}=State, Number, Name) ->
    State0 = update_endpoint_emergency_cid(State, Number, Name),
    kapi_dialplan:publish_command(ControlQ, build_bridge(State0, Number, Name)),
    lager:debug("sent bridge command to ~s", [ControlQ]).

-spec update_endpoint_emergency_cid(state() | kz_json:object(), ne_binary(), api_binary()) -> state().
update_endpoint_emergency_cid(#state{endpoints=Endpoints}=State, Number, Name) ->
    State#state{endpoints=[update_endpoint_emergency_cid(Endpoint, Number, Name)
                           || Endpoint <- Endpoints
                          ]};
update_endpoint_emergency_cid(Endpoint, Number, Name) ->
    case {kz_json:get_value(<<"Outbound-Caller-ID-Number">>, Endpoint, Number)
         ,kz_json:get_value(<<"Outbound-Caller-ID-Name">>, Endpoint, Name)
         }
    of
        {Number, Name} -> Endpoint;
        {Number, _} -> kz_json:set_value(<<"Outbound-Caller-ID-Name">>, Name, Endpoint);
        {_, Name} -> kz_json:set_value(<<"Outbound-Caller-ID-Number">>, Number, Endpoint);
        {_, _} ->
            Props = [{<<"Outbound-Caller-ID-Name">>, Name}
                    ,{<<"Outbound-Caller-ID-Number">>, Number}
                    ],
            kz_json:set_values(Props, Endpoint)
    end.

-spec build_bridge(state(), api_binary(), api_binary()) -> kz_proplist().
build_bridge(#state{endpoints=Endpoints
                   ,resource_req=OffnetReq
                   ,queue=Q
                   }
            ,Number
            ,Name
            ) ->
    lager:debug("set outbound caller id to ~s '~s'", [Number, Name]),
    AccountId = kapi_offnet_resource:account_id(OffnetReq),
    CCVs =
        kz_json:set_values(props:filter_undefined([{<<"Ignore-Display-Updates">>, <<"true">>}
                                                  ,{<<"Account-ID">>, AccountId}
                                                  ,{<<"From-URI">>, bridge_from_uri(Number, OffnetReq)}
                                                  ,{<<"Realm">>, stepswitch_util:default_realm(OffnetReq)}
                                                  ,{<<"Reseller-ID">>, kz_services:find_reseller_id(AccountId)}
                                                  ])
                          ,kapi_offnet_resource:custom_channel_vars(OffnetReq, kz_json:new())
                          ),
    FmtEndpoints = stepswitch_util:format_endpoints(Endpoints, Name, Number, OffnetReq),
    IgnoreEarlyMedia = kz_json:is_true(<<"Require-Ignore-Early-Media">>, CCVs, 'false')
        orelse kapi_offnet_resource:ignore_early_media(OffnetReq, 'false'),

    props:filter_undefined(
      [{<<"Application-Name">>, <<"bridge">>}
      ,{<<"Dial-Endpoint-Method">>, <<"single">>}
      ,{?KEY_OUTBOUND_CALLER_ID_NUMBER, Number}
      ,{?KEY_OUTBOUND_CALLER_ID_NAME, Name}
      ,{<<"Caller-ID-Number">>, Number}
      ,{<<"Caller-ID-Name">>, Name}
      ,{<<"Custom-Channel-Vars">>, CCVs}
      ,{<<"Timeout">>, kapi_offnet_resource:timeout(OffnetReq)}
      ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
      ,{<<"Media">>, kapi_offnet_resource:media(OffnetReq)}
      ,{<<"Hold-Media">>, kapi_offnet_resource:hold_media(OffnetReq)}
      ,{<<"Presence-ID">>, kapi_offnet_resource:presence_id(OffnetReq)}
      ,{<<"Ringback">>, kapi_offnet_resource:ringback(OffnetReq)}
      ,{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
      ,{<<"Fax-Identity-Number">>, kapi_offnet_resource:fax_identity_number(OffnetReq, Number)}
      ,{<<"Fax-Identity-Name">>, kapi_offnet_resource:fax_identity_name(OffnetReq, Name)}
      ,{<<"Outbound-Callee-ID-Number">>, kapi_offnet_resource:outbound_callee_id_number(OffnetReq)}
      ,{<<"Outbound-Callee-ID-Name">>, kapi_offnet_resource:outbound_callee_id_name(OffnetReq)}
      ,{<<"B-Leg-Events">>, kapi_offnet_resource:b_leg_events(OffnetReq, [])}
      ,{<<"Endpoints">>, FmtEndpoints}
       | kz_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
      ]).

-spec bridge_from_uri(api_binary(), kapi_offnet_resource:req()) ->
                             api_binary().
bridge_from_uri(Number, OffnetReq) ->
    Realm = stepswitch_util:default_realm(OffnetReq),

    case (kapps_config:get_is_true(?SS_CONFIG_CAT, <<"format_from_uri">>, 'false')
          orelse kapi_offnet_resource:format_from_uri(OffnetReq)
         )
        andalso is_binary(Number)
        andalso is_binary(Realm)
    of
        'false' -> 'undefined';
        'true' ->
            FromURI = <<"sip:", Number/binary, "@", Realm/binary>>,
            lager:debug("setting bridge from-uri to ~s", [FromURI]),
            FromURI
    end.

-spec bridge_outbound_cid_name(kapi_offnet_resource:req()) -> api_binary().
bridge_outbound_cid_name(OffnetReq) ->
    case kapi_offnet_resource:outbound_caller_id_name(OffnetReq) of
        'undefined' -> kapi_offnet_resource:emergency_caller_id_name(OffnetReq);
        Name -> Name
    end.

-spec bridge_outbound_cid_number(kapi_offnet_resource:req()) -> api_binary().
bridge_outbound_cid_number(OffnetReq) ->
    case kapi_offnet_resource:outbound_caller_id_number(OffnetReq) of
        'undefined' -> kapi_offnet_resource:emergency_caller_id_number(OffnetReq);
        Number -> Number
    end.

-spec bridge_emergency_cid_name(kapi_offnet_resource:req()) -> api_binary().
bridge_emergency_cid_name(OffnetReq) ->
    case kapi_offnet_resource:emergency_caller_id_name(OffnetReq) of
        'undefined' -> kapi_offnet_resource:outbound_caller_id_name(OffnetReq);
        Name -> Name
    end.

-spec bridge_emergency_cid_number(kapi_offnet_resource:req()) -> api_ne_binary().
bridge_emergency_cid_number(OffnetReq) ->
    case kapi_offnet_resource:emergency_caller_id_number(OffnetReq) of
        'undefined' -> kapi_offnet_resource:outbound_caller_id_number(OffnetReq);
        Number -> Number
    end.

-spec find_emergency_number(kapi_offnet_resource:req()) -> api_binary().
find_emergency_number(OffnetReq) ->
    case ?SHOULD_ENSURE_E911_CID_VALID of
        'true' -> ensure_valid_emergency_number(OffnetReq);
        'false' ->
            lager:debug("using first configured unverified emergency caller id"),
            bridge_emergency_cid_number(OffnetReq)
    end.

-spec ensure_valid_emergency_number(kapi_offnet_resource:req()) -> api_binary().
ensure_valid_emergency_number(OffnetReq) ->
    AccountId = kapi_offnet_resource:account_id(OffnetReq),
    lager:debug("ensuring emergency caller is valid for account ~s", [AccountId]),
    Numbers = knm_numbers:emergency_enabled(AccountId),
    Emergency = bridge_emergency_cid_number(OffnetReq),
    Outbound = bridge_outbound_cid_number(OffnetReq),
    case {lists:member(Emergency, Numbers), lists:member(Outbound, Numbers)} of
        {'true', _} ->
            lager:info("determined emergency caller id number ~s is configured for e911", [Emergency]),
            Emergency;
        {_, 'true'} ->
            lager:info("determined outbound caller id number ~s is configured for e911", [Outbound]),
            Outbound;
        {'false', 'false'} ->
            lager:notice("emergency caller id number ~s nor outbound caller id number ~s configured for e911"
                        ,[Emergency, Outbound]
                        ),
            find_valid_emergency_number(Numbers)
    end.

-spec find_valid_emergency_number(ne_binaries()) -> api_binary().
find_valid_emergency_number([]) ->
    lager:info("no alternative e911 enabled numbers available", []),
    'undefined';
find_valid_emergency_number([Number|_]) ->
    lager:info("found alternative emergency caller id number ~s", [Number]),
    Number.

-spec default_emergency_number(ne_binary()) -> ne_binary().
default_emergency_number(Requested) ->
    case ?DEFAULT_EMERGENCY_CID_NUMBER of
        'undefined' -> Requested;
        Else -> Else
    end.

-spec contains_emergency_endpoints(kz_json:objects()) -> boolean().
contains_emergency_endpoints([]) -> 'false';
contains_emergency_endpoints([Endpoint|Endpoints]) ->
    case kz_json:is_true([<<"Custom-Channel-Vars">>, <<"Emergency-Resource">>], Endpoint) of
        'true' ->
            lager:debug("endpoints contain an emergency resource", []),
            'true';
        'false' -> contains_emergency_endpoints(Endpoints)
    end.

-spec bridge_timeout(kapi_offnet_resource:req()) -> kz_proplist().
bridge_timeout(OffnetReq) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, <<"bridge request timed out">>}
    ,{<<"To-DID">>, kapi_offnet_resource:to_did(OffnetReq)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec bridge_error(kz_json:object(), kapi_offnet_resource:req()) -> kz_proplist().
bridge_error(JObj, OffnetReq) ->
    lager:debug("error during outbound request: ~s", [kz_term:to_binary(kz_json:encode(JObj))]),
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, kz_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
    ,{<<"To-DID">>, kapi_offnet_resource:to_did(OffnetReq)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec bridge_success(kz_json:object(), kapi_offnet_resource:req()) -> kz_proplist().
bridge_success(JObj, OffnetReq) ->
    lager:debug("outbound request successfully completed"),
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"SUCCESS">>}
    ,{<<"Response-Code">>, <<"sip:200">>}
    ,{<<"Resource-Response">>, JObj}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec bridge_failure(kz_json:object(), kapi_offnet_resource:req()) -> kz_proplist().
bridge_failure(JObj, OffnetReq) ->
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

-spec bridge_not_configured(kapi_offnet_resource:req()) -> kz_proplist().
bridge_not_configured(OffnetReq) ->
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"MANDATORY_IE_MISSING">>}
    ,{<<"Response-Code">>, <<"sip:403">>}
    ,{<<"Error-Message">>, <<"services not configured">>}
    ,{<<"To-DID">>, kapi_offnet_resource:to_did(OffnetReq)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec deny_emergency_bridge(state()) -> 'ok'.
deny_emergency_bridge(#state{resource_req=OffnetReq
                            ,control_queue=ControlQ
                            }) ->
    lager:warning("terminating attempted emergency bridge from unconfigured device"),
    _ = send_deny_emergency_response(OffnetReq, ControlQ),
    send_deny_emergency_notification(OffnetReq),
    Result = bridge_not_configured(OffnetReq),
    gen_listener:cast(self(), {'bridge_result', Result}).

-spec send_deny_emergency_notification(kapi_offnet_resource:req()) -> 'ok'.
send_deny_emergency_notification(OffnetReq) ->
    Props =
        [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
        ,{<<"Account-ID">>, kapi_offnet_resource:account_id(OffnetReq)}
        ,{?KEY_E_CALLER_ID_NUMBER, kapi_offnet_resource:emergency_caller_id_number(OffnetReq)}
        ,{?KEY_E_CALLER_ID_NAME, kapi_offnet_resource:emergency_caller_id_name(OffnetReq)}
        ,{?KEY_OUTBOUND_CALLER_ID_NUMBER, kapi_offnet_resource:outbound_caller_id_number(OffnetReq)}
        ,{?KEY_OUTBOUND_CALLER_ID_NAME, kapi_offnet_resource:outbound_caller_id_name(OffnetReq)}
         | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
        ],
    kapi_notifications:publish_denied_emergency_bridge(Props).

-spec send_deny_emergency_response(kapi_offnet_resource:req(), ne_binary()) ->
                                          {'ok', ne_binary()} |
                                          {'error', 'no_response'}.
send_deny_emergency_response(OffnetReq, ControlQ) ->
    CallId = kapi_offnet_resource:call_id(OffnetReq),
    Code = kapps_config:get_integer(?SS_CONFIG_CAT, <<"deny_emergency_bridge_code">>, 486),
    Cause = kapps_config:get_ne_binary(?SS_CONFIG_CAT
                                      ,<<"deny_emergency_bridge_cause">>
                                      ,<<"Emergency service not configured">>
                                      ),
    Media = kapps_config:get_ne_binary(?SS_CONFIG_CAT
                                      ,<<"deny_emergency_bridge_media">>
                                      ,<<"prompt://system_media/stepswitch-emergency_not_configured/">>
                                      ),
    kz_call_response:send(CallId, ControlQ, Code, Cause, Media).

-spec get_event_type(kz_json:object()) -> {ne_binary(), ne_binary(), ne_binary()}.
get_event_type(JObj) ->
    {C, E} = kapps_util:get_event_type(JObj),
    {C, E, kz_call_event:call_id(JObj)}.
