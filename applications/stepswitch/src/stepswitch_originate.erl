%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
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
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

-record(state, {msg_id=wh_util:rand_hex_binary(12)
                ,endpoints = [] :: wh_json:objects()
                ,resource_req :: wh_json:object()
                ,request_handler :: pid()
                ,response_queue :: api_binary()
                ,queue :: api_binary()
                ,timeout :: api_reference()
               }).

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
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(wh_json:objects(), wh_json:object()) -> startlink_ret().
start_link(Endpoints, JObj) ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Endpoints, JObj]).

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
init([Endpoints, JObj]) ->
    wh_util:put_callid(JObj),
    {'ok', #state{endpoints=Endpoints
                  ,resource_req=JObj
                  ,request_handler=self()
                  ,response_queue=wh_json:get_ne_value(<<"Server-ID">>, JObj)
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
handle_cast({'wh_amqp_channel', _}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{queue=Q}};
handle_cast({'gen_listener', {'is_consuming', 'true'}}, State) ->
    'ok' = wapi_resource:publish_originate_req(build_originate(State)),
    lager:debug("sent originate command"),
    {'noreply', State};
handle_cast({'originate_result', _Props}, #state{response_queue='undefined'}=State) ->
    {'stop', 'normal', State};
handle_cast({'originate_result', Props}, #state{response_queue=ResponseQ}=State) ->
    wapi_offnet_resource:publish_resp(ResponseQ, Props),
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
handle_cast({'bind_to_call', 'undefined'}, #state{resource_req=Request}=State) ->
    gen_listener:cast(self(), {'originate_result', originate_failure(wh_json:new(), Request)}),
    {'stop', 'normal', State};
handle_cast({'bind_to_call', CallId}, State) ->
    wh_util:put_callid(CallId),
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
handle_info('originate_timeout', #state{timeout='undefined'}=State) ->
    {'noreply', State};
handle_info('originate_timeout', #state{response_queue=ResponseQ
                                        ,resource_req=JObj
                                       }=State) ->
    wapi_offnet_resource:publish_resp(ResponseQ, originate_timeout(JObj)),
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
handle_event(JObj, #state{request_handler=RequestHandler
                          ,resource_req=Request
                          ,msg_id=MsgId
                         }) ->
    case whapps_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            lager:debug("channel was destroy while waiting for execute extension", []),
            gen_listener:cast(RequestHandler, {'originate_result', originate_success(JObj, Request)});
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>} ->
            CallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
            gen_listener:cast(RequestHandler, {'bridged', CallId});
        {<<"call_event">>, <<"CHANNEL_ANSWER">>} ->
            gen_listener:cast(RequestHandler, 'answered');
        {<<"resource">>, <<"originate_resp">>} ->
            MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
            case wh_json:get_value(<<"Application-Response">>, JObj) =:= <<"SUCCESS">> of
                'true' ->
                    gen_listener:cast(RequestHandler, {'originate_result', originate_success(JObj, Request)});
                'false' ->
                    gen_listener:cast(RequestHandler, {'originate_result', originate_failure(JObj, Request)})
            end;
        {<<"error">>, <<"originate_resp">>} ->
            MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
            lager:debug("channel execution error while waiting for originate: ~s"
                        ,[wh_util:to_binary(wh_json:encode(JObj))]),
            gen_listener:cast(RequestHandler, {'originate_result', originate_error(JObj, Request)});
        {<<"dialplan">>, <<"originate_ready">>} ->
            gen_listener:cast(RequestHandler, {'originate_result', originate_ready(JObj, Request)});
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
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
build_originate(#state{endpoints=Endpoints
                       ,resource_req=JObj
                       ,queue=Q
                       ,msg_id=MsgId
                      }) ->
    {CIDNum, CIDName} = originate_caller_id(JObj),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    CCVUpdates = props:filter_undefined(
                   [{<<"Global-Resource">>, <<"true">>}
                    ,{<<"Account-ID">>, AccountId}
                    ,{<<"From-URI">>, originate_from_uri(CIDNum, JObj)}
                    ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
                   ]),
    Application = wh_json:get_value(<<"Application-Name">>, JObj, <<"park">>),
    props:filter_undefined(
      [{<<"Dial-Endpoint-Method">>, <<"single">>}
       ,{<<"Application-Name">>, Application}
       ,{<<"Msg-ID">>, MsgId}
       ,{<<"Call-ID">>, wh_json:get_value(<<"Outbound-Call-ID">>, JObj)}
       ,{<<"Outbound-Call-ID">>, wh_json:get_value(<<"Outbound-Call-ID">>, JObj)}
       ,{<<"Existing-Call-ID">>, wh_json:get_value(<<"Existing-Call-ID">>, JObj)}
       ,{<<"Originate-Immediate">>, wh_json:get_value(<<"Originate-Immediate">>, JObj)}
       ,{<<"Simplify-Loopback">>, wh_json:get_value(<<"Simplify-Loopback">>, JObj)}
       ,{<<"Endpoints">>, Endpoints}
       ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
       ,{<<"Outbound-Caller-ID-Name">>, CIDName}
       ,{<<"Caller-ID-Number">>, CIDNum}
       ,{<<"Caller-ID-Name">>, CIDName}
       ,{<<"Application-Data">>, wh_json:get_value(<<"Application-Data">>, JObj)}
       ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
       ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
       ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
       ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
       ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
       ,{<<"Outbound-Callee-ID-Number">>, wh_json:get_value(<<"Outbound-Callee-ID-Number">>, JObj)}
       ,{<<"Outbound-Callee-ID-Name">>, wh_json:get_value(<<"Outbound-Callee-ID-Name">>, JObj)}
       ,{<<"Fax-Identity-Number">>, wh_json:get_value(<<"Fax-Identity-Number">>, JObj, CIDNum)}
       ,{<<"Fax-Identity-Name">>, wh_json:get_value(<<"Fax-Identity-Name">>, JObj, CIDName)}
       ,{<<"Fax-Timezone">>, wh_json:get_value(<<"Fax-Timezone">>, JObj)}
       ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
       ,{<<"Custom-SIP-Headers">>, wh_json:get_value(<<"Custom-SIP-Headers">>, JObj)}
       ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, CCVs)}
       | wh_api:default_headers(Q, <<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
      ]).

-spec originate_from_uri(ne_binary(), wh_json:object()) -> api_binary().
originate_from_uri(CIDNum, JObj) ->
    Realm = wh_json:get_first_defined([<<"From-URI-Realm">>
                                       ,<<"Account-Realm">>
                                      ], JObj),
    case (whapps_config:get_is_true(?SS_CONFIG_CAT, <<"format_from_uri">>, 'false')
          orelse wh_json:is_true(<<"Format-From-URI">>, JObj))
        andalso (is_binary(CIDNum) andalso is_binary(Realm))
    of
        'false' -> 'undefined';
        'true' ->
            FromURI = <<"sip:", CIDNum/binary, "@", Realm/binary>>,
            lager:debug("setting bridge from-uri to ~s", [FromURI]),
            FromURI
    end.

-spec originate_caller_id(wh_json:object()) -> {api_binary(), api_binary()}.
originate_caller_id(JObj) ->
    {wh_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                                ,<<"Emergency-Caller-ID-Number">>
                               ], JObj)
     ,wh_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                                 ,<<"Emergency-Caller-ID-Name">>
                                ], JObj)
    }.

-spec originate_timeout(wh_json:object()) -> wh_proplist().
originate_timeout(Request) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Outbound-Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, <<"originate request timed out">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Request)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec originate_error(wh_json:object(), wh_json:object()) -> wh_proplist().
originate_error(JObj, Request) ->
    lager:debug("error during originate request: ~s", [wh_util:to_binary(wh_json:encode(JObj))]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Outbound-Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, wh_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Request)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec originate_success(wh_json:object(), wh_json:object()) -> wh_proplist().
originate_success(JObj, Request) ->
    lager:debug("originate request successfully completed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Outbound-Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"SUCCESS">>}
     ,{<<"Response-Code">>, <<"sip:200">>}
     ,{<<"Resource-Response">>, JObj}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec originate_failure(wh_json:object(), wh_json:object()) -> wh_proplist().
originate_failure(JObj, Request) ->
    lager:debug("originate request failed: ~s", [wh_json:get_value(<<"Application-Response">>, JObj)]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Outbound-Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, wh_json:get_first_defined([<<"Application-Response">>
                                                          ,<<"Hangup-Cause">>
                                                         ], JObj)}
     ,{<<"Response-Code">>, wh_json:get_value(<<"Hangup-Code">>, JObj)}
     ,{<<"Resource-Response">>, JObj}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec originate_ready(wh_json:object(), wh_json:object()) -> wh_proplist().
originate_ready(JObj, Request) ->
    lager:debug("originate is ready to execute"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Outbound-Call-ID">>, JObj)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request)}
     ,{<<"Control-Queue">>, wh_json:get_value(<<"Control-Queue">>, JObj)}
     ,{<<"Response-Message">>, <<"READY">>}
     ,{<<"Resource-Response">>, JObj}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
