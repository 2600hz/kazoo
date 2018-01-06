%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(knm_number_options:extra_options(), kapi_offnet_resource:req()) -> startlink_ret().
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
handle_cast({'gen_listener', {'is_consuming', 'true'}}, #state{control_queue=ControlQ}=State) ->
    Payload = build_local_extension(State),
    'ok' = kapi_dialplan:publish_command(ControlQ, Payload),
    lager:debug("sent local extension command to ~s", [ControlQ]),
    {'noreply', State};
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
                         ,resource_req=Request
                         ,call_id=CallId
                         }) ->
    case get_event_type(JObj) of
        {<<"error">>, _, _} ->
            <<"bridge">> = kz_json:get_value([<<"Request">>, <<"Application-Name">>], JObj),
            lager:debug("channel execution error while waiting for execute extension: ~s"
                       ,[kz_term:to_binary(kz_json:encode(JObj))]),
            gen_listener:cast(RequestHandler, {'local_extension_result', local_extension_error(JObj, Request)});
        {<<"call_event">>, <<"CHANNEL_TRANSFEROR">>, _CallId} ->
            Transferor = kz_call_event:other_leg_call_id(JObj),
            gen_listener:cast(RequestHandler, {'replaced', Transferor}),
            gen_listener:add_binding(RequestHandler, ?CALL_BINDING(Transferor));
        {<<"call_event">>, <<"CHANNEL_REPLACED">>, _CallId} ->
            ReplacedBy = kz_call_event:replaced_by(JObj),
            gen_listener:cast(RequestHandler, {'replaced', ReplacedBy}),
            gen_listener:add_binding(RequestHandler, ?CALL_BINDING(ReplacedBy));
        {<<"call_event">>, <<"CHANNEL_DESTROY">>, CallId} ->
            lager:debug("channel was destroyed while waiting for bridge"),
            Result = case kz_json:get_value(<<"Disposition">>, JObj)
                         =:= <<"SUCCESS">>
                     of
                         'true' -> local_extension_success(Request);
                         'false' -> local_extension_failure(JObj, Request)
                     end,
            gen_listener:cast(RequestHandler, {'local_extension_result', Result});
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, CallId} ->
            <<"bridge">> = kz_json:get_value(<<"Application-Name">>, JObj),
            lager:debug("channel execute complete for bridge"),
            Result = case kz_json:get_value(<<"Disposition">>, JObj)
                         =:= <<"SUCCESS">>
                     of
                         'true' -> local_extension_success(Request);
                         'false' -> local_extension_failure(JObj, Request)
                     end,
            gen_listener:cast(RequestHandler, {'local_extension_result', Result});
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
-spec outbound_flags(kapi_offnet_resource:req()) -> api_binary().
outbound_flags(OffnetJObj) ->
    case kapi_offnet_resource:flags(OffnetJObj) of
        [] -> 'undefined';
        Flags -> kz_binary:join(Flags, <<"|">>)
    end.

-spec build_local_extension(state()) -> kz_proplist().
build_local_extension(#state{number_props=Props
                            ,resource_req=OffnetJObj
                            ,queue=Q
                            }) ->
    {CIDName, CIDNum} = local_extension_caller_id(OffnetJObj),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    Number = knm_number_options:number(Props),
    AccountId = knm_number_options:account_id(Props),
    OriginalAccountId = kapi_offnet_resource:account_id(OffnetJObj),
    ResellerId = kz_services:find_reseller_id(OriginalAccountId),
    {CEDNum, CEDName} = local_extension_callee_id(OffnetJObj, Number),
    Realm = get_account_realm(AccountId),
    FromRealm = get_account_realm(OriginalAccountId),
    FromURI = <<"sip:", CIDNum/binary, "@", Realm/binary>>,
    CCVsOrig = kapi_offnet_resource:custom_channel_vars(OffnetJObj, kz_json:new()),
    CAVs = kapi_offnet_resource:custom_application_vars(OffnetJObj),

    CCVs = kz_json:set_values(props:filter_undefined([{<<"Ignore-Display-Updates">>, <<"true">>}
                                                     ,{<<"Account-ID">>, OriginalAccountId}
                                                     ,{<<"Reseller-ID">>, ResellerId}
                                                     ,{<<"Outbound-Flags">>, outbound_flags(OffnetJObj)}
                                                     ])
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

-spec get_account_realm(ne_binary()) -> ne_binary().
get_account_realm(AccountId) ->
    case kz_account:fetch_realm(AccountId) of
        'undefined' -> AccountId;
        Realm -> Realm
    end.

-spec local_extension_caller_id(kz_json:object()) -> {api_binary(), api_binary()}.
local_extension_caller_id(JObj) ->
    {kz_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                               ,<<"Emergency-Caller-ID-Name">>
                               ], JObj)
    ,kz_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                               ,<<"Emergency-Caller-ID-Number">>
                               ], JObj)
    }.

-spec local_extension_callee_id(kz_json:object(), ne_binary()) -> {api_binary(), api_binary()}.
local_extension_callee_id(JObj, Number) ->
    {kz_json:get_value(<<"Outbound-Callee-ID-Number">>, JObj, Number)
    ,kz_json:get_value(<<"Outbound-Callee-ID-Name">>, JObj, Number)
    }.

-spec local_extension_timeout(kz_json:object()) -> kz_proplist().
local_extension_timeout(Request) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, Request)}
    ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, Request, <<>>)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, <<"local extension request timed out">>}
    ,{<<"To-DID">>, kz_json:get_value(<<"To-DID">>, Request)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec local_extension_error(kz_json:object(), kz_json:object()) -> kz_proplist().
local_extension_error(JObj, Request) ->
    lager:debug("error during outbound request: ~s", [kz_term:to_binary(kz_json:encode(JObj))]),
    [{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, Request)}
    ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, Request, <<>>)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, kz_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
    ,{<<"To-DID">>, kz_json:get_value(<<"To-DID">>, Request)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec local_extension_success(kz_json:object()) -> kz_proplist().
local_extension_success(Request) ->
    lager:debug("local extension request successfully completed"),
    [{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, Request)}
    ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, Request, <<>>)}
    ,{<<"Response-Message">>, <<"SUCCESS">>}
    ,{<<"Response-Code">>, <<"sip:200">>}
    ,{<<"Resource-Response">>, kz_json:new()}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec local_extension_failure(kz_json:object(), kapi_offnet_resource:req()) -> kz_proplist().
local_extension_failure(JObj, OffnetReq) ->
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

-spec get_event_type(kz_json:object()) -> {ne_binary(), ne_binary(), ne_binary()}.
get_event_type(JObj) ->
    {C, E} = kz_util:get_event_type(JObj),
    {C, E, kz_call_event:call_id(JObj)}.
