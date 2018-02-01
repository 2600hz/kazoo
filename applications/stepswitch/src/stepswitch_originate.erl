%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_originate).
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

-record(state, {msg_id=kz_binary:rand_hex(12)
               ,endpoints = [] :: kz_json:objects()
               ,resource_req :: kapi_offnet_resource:req()
               ,request_handler :: pid()
               ,response_queue :: kz_term:api_binary()
               ,queue :: kz_term:api_binary()
               ,timeout :: kz_term:api_reference()
               }).
-type state() :: #state{}.

-define(RESPONDERS, []).
-define(BINDINGS, [{'resource', []}
                  ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(kz_json:objects(), kapi_offnet_resource:req()) -> kz_types:startlink_ret().
start_link(Endpoints, OffnetReq) ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS}
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
    kz_util:put_callid(OffnetReq),
    {'ok', #state{endpoints=Endpoints
                 ,resource_req=OffnetReq
                 ,request_handler=self()
                 ,response_queue=kz_api:server_id(OffnetReq)
                 ,timeout=erlang:send_after(120000, self(), 'originate_timeout')
                 }}.

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
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
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
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'kz_amqp_channel', _}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{queue=Q}};
handle_cast({'gen_listener', {'is_consuming', 'true'}}, State) ->
    'ok' = kapi_resource:publish_originate_req(build_originate(State)),
    lager:debug("sent originate command"),
    {'noreply', State};
handle_cast({'originate_result', _Props}, #state{response_queue='undefined'}=State) ->
    {'stop', 'normal', State};
handle_cast({'originate_result', Props}, #state{response_queue=ResponseQ}=State) ->
    kapi_offnet_resource:publish_resp(ResponseQ, Props),
    {'stop', 'normal', State};
handle_cast({'bridged', CallId}, #state{timeout='undefined'}=State) ->
    lager:debug("channel bridged to ~s", [CallId]),
    {'noreply', State};
handle_cast({'bridged', CallId}, #state{timeout=TimerRef}=State) ->
    lager:debug("channel bridged to ~s, canceling timeout", [CallId]),
    _ = erlang:cancel_timer(TimerRef),
    {'noreply', State#state{timeout='undefined'}};
handle_cast('answered', #state{timeout='undefined'}=State) ->
    lager:debug("channel answered"),
    {'noreply', State};
handle_cast('answered', #state{timeout=TimerRef}=State) ->
    lager:debug("channel answered, canceling timeout"),
    _ = erlang:cancel_timer(TimerRef),
    {'noreply', State#state{timeout='undefined'}};
handle_cast({'bind_to_call', 'undefined'}, #state{resource_req=OffnetReq}=State) ->
    gen_listener:cast(self(), {'originate_result', originate_failure(kz_json:new(), OffnetReq)}),
    {'stop', 'normal', State};
handle_cast({'bind_to_call', CallId}, State) ->
    kz_util:put_callid(CallId),
    Props = [{'callid', CallId}
            ,{'restrict_to', [<<"CHANNEL_DESTROY">>
                             ,<<"CHANNEL_BRIDGE">>
                             ,<<"CHANNEL_ANSWER">>
                             ]}
            ],
    gen_listener:add_binding(self(), 'call', Props),
    gen_listener:rm_binding(self(), 'resource', []),
    {'noreply', State};
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
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('originate_timeout', #state{timeout='undefined'}=State) ->
    {'noreply', State};
handle_info('originate_timeout', #state{response_queue=ResponseQ
                                       ,resource_req=OffnetReq
                                       }=State) ->
    kapi_offnet_resource:publish_resp(ResponseQ, originate_timeout(OffnetReq)),
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
                         ,msg_id=MsgId
                         }) ->
    case kz_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            lager:debug("channel was destroy while waiting for execute extension"),
            gen_listener:cast(RequestHandler, {'originate_result', originate_success(JObj, OffnetReq)});
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>} ->
            CallId = kz_call_event:other_leg_call_id(JObj),
            gen_listener:cast(RequestHandler, {'bridged', CallId});
        {<<"call_event">>, <<"CHANNEL_ANSWER">>} ->
            gen_listener:cast(RequestHandler, 'answered');
        {<<"resource">>, <<"originate_resp">>} ->
            MsgId = kz_api:msg_id(JObj),
            case kz_call_event:application_response(JObj) =:= <<"SUCCESS">> of
                'true' ->
                    gen_listener:cast(RequestHandler, {'originate_result', originate_success(JObj, OffnetReq)});
                'false' ->
                    gen_listener:cast(RequestHandler, {'originate_result', originate_failure(JObj, OffnetReq)})
            end;
        {<<"error">>, <<"originate_resp">>} ->
            MsgId = kz_api:msg_id(JObj),
            lager:debug("channel execution error while waiting for originate: ~s"
                       ,[kz_json:encode(JObj)]
                       ),
            gen_listener:cast(RequestHandler, {'originate_result', originate_error(JObj, OffnetReq)});
        {<<"dialplan">>, <<"originate_ready">>} ->
            gen_listener:cast(RequestHandler, {'originate_result', originate_ready(JObj, OffnetReq)});
        _ -> 'ok'
    end,
    'ignore'.

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
build_originate(#state{endpoints=Endpoints
                      ,resource_req=OffnetReq
                      ,queue=Q
                      ,msg_id=MsgId
                      }) ->
    {CIDNum, CIDName} = originate_caller_id(OffnetReq),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    AccountId = kz_json:get_value(<<"Account-ID">>, OffnetReq),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, OffnetReq, kz_json:new()),
    CCVUpdates = props:filter_undefined(
                   [{<<"Global-Resource">>, <<"true">>}
                   ,{<<"Account-ID">>, AccountId}
                   ,{<<"From-URI">>, originate_from_uri(CIDNum, OffnetReq)}
                   ,{<<"Realm">>, stepswitch_util:default_realm(OffnetReq)}
                   ,{<<"Reseller-ID">>, kz_services:find_reseller_id(AccountId)}
                   ]),
    Application = kz_json:get_value(<<"Application-Name">>, OffnetReq, <<"park">>),

    FmtEndpoints = stepswitch_util:format_endpoints(Endpoints, CIDName, CIDNum, OffnetReq),

    props:filter_undefined(
      [{<<"Application-Data">>, kz_json:get_value(<<"Application-Data">>, OffnetReq)}
      ,{<<"Application-Name">>, Application}
      ,{<<"Call-ID">>, kz_json:get_value(<<"Outbound-Call-ID">>, OffnetReq)}
      ,{<<"Caller-ID-Name">>, CIDName}
      ,{<<"Caller-ID-Number">>, CIDNum}
      ,{<<"Custom-Channel-Vars">>, kz_json:set_values(CCVUpdates, CCVs)}
      ,{<<"Custom-SIP-Headers">>, kz_json:get_value(<<"Custom-SIP-Headers">>, OffnetReq)}
      ,{<<"Dial-Endpoint-Method">>, <<"single">>}
      ,{<<"Endpoints">>, FmtEndpoints}
      ,{<<"Existing-Call-ID">>, kz_json:get_value(<<"Existing-Call-ID">>, OffnetReq)}
      ,{<<"Fax-Identity-Name">>, kz_json:get_value(<<"Fax-Identity-Name">>, OffnetReq, CIDName)}
      ,{<<"Fax-Identity-Number">>, kz_json:get_value(<<"Fax-Identity-Number">>, OffnetReq, CIDNum)}
      ,{<<"Fax-Timezone">>, kz_json:get_value(<<"Fax-Timezone">>, OffnetReq)}
      ,{<<"Hold-Media">>, kz_json:get_value(<<"Hold-Media">>, OffnetReq)}
      ,{<<"Ignore-Early-Media">>, kz_json:get_value(<<"Ignore-Early-Media">>, OffnetReq)}
      ,{<<"Loopback-Bowout">>, kz_json:get_value(<<"Loopback-Bowout">>, OffnetReq)}
      ,{<<"Media">>, kz_json:get_value(<<"Media">>, OffnetReq)}
      ,{<<"Msg-ID">>, MsgId}
      ,{<<"Originate-Immediate">>, kz_json:get_value(<<"Originate-Immediate">>, OffnetReq)}
      ,{<<"Origination-Call-ID">>, kz_json:get_value(<<"Origination-Call-ID">>, OffnetReq)}
      ,{<<"Outbound-Call-ID">>, kz_json:get_first_defined([<<"Origination-Call-ID">>, <<"Outbound-Call-ID">>], OffnetReq)}
      ,{<<"Outbound-Callee-ID-Name">>, kz_json:get_value(<<"Outbound-Callee-ID-Name">>, OffnetReq)}
      ,{<<"Outbound-Callee-ID-Number">>, kz_json:get_value(<<"Outbound-Callee-ID-Number">>, OffnetReq)}
      ,{<<"Outbound-Caller-ID-Name">>, CIDName}
      ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
      ,{<<"Presence-ID">>, kz_json:get_value(<<"Presence-ID">>, OffnetReq)}
      ,{<<"Ringback">>, kz_json:get_value(<<"Ringback">>, OffnetReq)}
      ,{<<"Simplify-Loopback">>, kz_json:get_value(<<"Simplify-Loopback">>, OffnetReq)}
      ,{<<"Timeout">>, kz_json:get_value(<<"Timeout">>, OffnetReq)}
       | kz_api:default_headers(Q, <<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
      ]).

-spec originate_from_uri(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
originate_from_uri(CIDNum, OffnetReq) ->
    Realm = kz_json:get_first_defined([<<"From-URI-Realm">>
                                      ,<<"Account-Realm">>
                                      ], OffnetReq),
    case (kapps_config:get_is_true(?SS_CONFIG_CAT, <<"format_from_uri">>, 'false')
          orelse kz_json:is_true(<<"Format-From-URI">>, OffnetReq))
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

-spec originate_caller_id(kz_json:object()) -> {kz_term:api_binary(), kz_term:api_binary()}.
originate_caller_id(OffnetReq) ->
    {kz_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                               ,<<"Emergency-Caller-ID-Number">>
                               ], OffnetReq)
    ,kz_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                               ,<<"Emergency-Caller-ID-Name">>
                               ], OffnetReq)
    }.

-spec originate_timeout(kz_json:object()) -> kz_term:proplist().
originate_timeout(Request) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, kz_json:get_value(<<"Outbound-Call-ID">>, Request)}
    ,{<<"Msg-ID">>, kz_api:msg_id(Request)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, <<"originate request timed out">>}
    ,{<<"To-DID">>, kz_json:get_value(<<"To-DID">>, Request)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec originate_error(kz_json:object(), kz_json:object()) -> kz_term:proplist().
originate_error(JObj, OffnetReq) ->
    lager:debug("error during originate request: ~s", [kz_term:to_binary(kz_json:encode(JObj))]),
    [{<<"Call-ID">>, kz_json:get_value(<<"Outbound-Call-ID">>, OffnetReq)}
    ,{<<"Msg-ID">>, kz_api:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, kz_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
    ,{<<"To-DID">>, kz_json:get_value(<<"To-DID">>, OffnetReq)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec originate_success(kz_json:object(), kz_json:object()) -> kz_term:proplist().
originate_success(JObj, OffnetReq) ->
    lager:debug("originate request successfully completed"),
    [{<<"Call-ID">>, kz_json:get_value(<<"Outbound-Call-ID">>, OffnetReq)}
    ,{<<"Msg-ID">>, kz_api:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"SUCCESS">>}
    ,{<<"Response-Code">>, <<"sip:200">>}
    ,{<<"Resource-Response">>, JObj}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec originate_failure(kz_json:object(), kz_json:object()) -> kz_term:proplist().
originate_failure(JObj, OffnetReq) ->
    lager:debug("originate request failed: ~s", [kz_json:get_value(<<"Application-Response">>, JObj)]),
    [{<<"Call-ID">>, kz_json:get_value(<<"Outbound-Call-ID">>, OffnetReq)}
    ,{<<"Msg-ID">>, kz_api:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, kz_json:get_first_defined([<<"Application-Response">>
                                                        ,<<"Hangup-Cause">>
                                                        ], JObj)}
    ,{<<"Response-Code">>, kz_json:get_value(<<"Hangup-Code">>, JObj)}
    ,{<<"Resource-Response">>, JObj}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec originate_ready(kz_json:object(), kz_json:object()) -> kz_term:proplist().
originate_ready(JObj, OffnetReq) ->
    lager:debug("originate is ready to execute"),
    [{<<"Call-ID">>, kz_json:get_value(<<"Outbound-Call-ID">>, JObj)}
    ,{<<"Msg-ID">>, kz_api:msg_id(OffnetReq)}
    ,{<<"Control-Queue">>, kz_json:get_value(<<"Control-Queue">>, JObj)}
    ,{<<"Response-Message">>, <<"READY">>}
    ,{<<"Resource-Response">>, JObj}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
