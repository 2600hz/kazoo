%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_bridge).

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

-record(state, {endpoints = [] :: wh_json:objects()
                ,resource_req :: wh_json:object()
                ,request_handler :: pid()
                ,control_queue :: api_binary()
                ,response_queue :: api_binary()
                ,queue :: api_binary()
                ,timeout :: reference()
               }).
-type state() :: #state{}.

-define(RESPONDERS, []).
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
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Bindings = [{'call', [{'callid', CallId}
                          ,{'restrict_to', [<<"CHANNEL_DESTROY">>
                                            ,<<"CHANNEL_EXECUTE_COMPLETE">>
                                            ,<<"CHANNEL_BRIDGE">>
                                           ]}
                         ]}
                ,{'self', []}
               ],
    gen_listener:start_link(?MODULE, [{'bindings', Bindings}
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
    case wh_json:get_ne_value(<<"Control-Queue">>, JObj) of
        'undefined' -> {'stop', 'normal'};
        ControlQ ->
            {'ok', #state{endpoints=Endpoints
                          ,resource_req=JObj
                          ,request_handler=self()
                          ,control_queue=ControlQ
                          ,response_queue=wh_json:get_ne_value(<<"Server-ID">>, JObj)
                          ,timeout=erlang:send_after(30000, self(), 'bridge_timeout')
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
    _ = maybe_bridge(State),
    {'noreply', State};
handle_cast({'bridge_result', _Props}, #state{response_queue='undefined'}=State) ->
    {'stop', 'normal', State};
handle_cast({'bridge_result', Props}, #state{response_queue=ResponseQ}=State) ->
    wapi_offnet_resource:publish_resp(ResponseQ, Props),
    {'stop', 'normal', State};
handle_cast({'bridged', CallId}, #state{timeout='undefined'}=State) ->
    lager:debug("channel bridged to ~s", [CallId]),
    {'noreply', State};
handle_cast({'bridged', CallId}, #state{timeout=TimerRef}=State) ->
    lager:debug("channel bridged to ~s, canceling timeout", [CallId]),
    _ = erlang:cancel_timer(TimerRef),
    {'noreply', State#state{timeout='undefined'}};
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
handle_info('bridge_timeout', #state{timeout='undefined'}=State) ->
    {'noreply', State};
handle_info('bridge_timeout', #state{response_queue=ResponseQ
                                     ,resource_req=JObj
                                    }=State) ->
    wapi_offnet_resource:publish_resp(ResponseQ, bridge_timeout(JObj)),
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
                         }) ->
    case whapps_util:get_event_type(JObj) of
        {<<"error">>, _} ->
            <<"bridge">> = wh_json:get_value([<<"Request">>, <<"Application-Name">>], JObj),
            lager:debug("channel execution error while waiting for bridge: ~s"
                        ,[wh_util:to_binary(wh_json:encode(JObj))]),
            gen_listener:cast(RequestHandler, {'bridge_result', bridge_error(JObj, Request)});
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            lager:debug("channel was destroyed while waiting for bridge"),
            Result = case wh_json:get_value(<<"Disposition">>, JObj)
                         =:= <<"SUCCESS">>
                     of
                         'true' -> bridge_success(JObj, Request);
                         'false' -> bridge_failure(JObj, Request)
                     end,
            gen_listener:cast(RequestHandler, {'bridge_result', Result});
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            <<"bridge">> = wh_json:get_value(<<"Application-Name">>, JObj),
            lager:debug("channel execute complete for bridge"),
            Result = case wh_json:get_value(<<"Disposition">>, JObj)
                         =:= <<"SUCCESS">>
                     of
                         'true' -> bridge_success(JObj, Request);
                         'false' -> bridge_failure(JObj, Request)
                     end,
            gen_listener:cast(RequestHandler, {'bridge_result', Result});
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>} ->
            CallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
            gen_listener:cast(RequestHandler, {'bridged', CallId});
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
-spec maybe_bridge(state()) -> 'ok'.
maybe_bridge(#state{endpoints=Endpoints
                    ,resource_req=JObj
                    ,control_queue=ControlQ
                   }=State) ->
    case contains_emergency_endpoints(Endpoints) of
        'true' -> maybe_bridge_emergency(State);
        'false' ->
            Name = bridge_outbound_cid_name(JObj),
            Number = bridge_outbound_cid_number(JObj),
            wapi_dialplan:publish_command(
              ControlQ
              ,build_bridge(State, Number, Name)
             ),
            lager:debug("sent bridge command to ~s", [ControlQ])
    end.

-spec maybe_bridge_emergency(state()) -> 'ok'.
maybe_bridge_emergency(#state{resource_req=JObj
                              ,control_queue=ControlQ
                             }=State) ->
    %% NOTE: if this request had a hunt-account-id then we
    %%   are assuming it was for a local resource (at the
    %%   time of this commit offnet DB is still in use)
    Name = bridge_emergency_cid_name(JObj),
    case wh_json:get_value(<<"Hunt-Account-ID">>, JObj) of
        'undefined' ->
            Number = find_emergency_number(JObj),
            maybe_deny_emergency_bridge(State, Number, Name);
        _Else ->
            Number = bridge_emergency_cid_number(JObj),
            lager:debug("not enforcing emergency caller id validation when using resource from account ~s", [_Else]),
            wapi_dialplan:publish_command(
              ControlQ
              ,build_bridge(State, Number, Name)
             ),
            lager:debug("sent bridge command to ~s", [ControlQ])
    end.

-spec maybe_deny_emergency_bridge(state(), api_binary(), api_binary()) -> 'ok'.
maybe_deny_emergency_bridge(State, 'undefined', Name) ->
    case whapps_config:get_is_true(
           ?SS_CONFIG_CAT
           ,<<"deny_invalid_emergency_cid">>
           ,'false'
          )
    of
        'false' ->
            maybe_deny_emergency_bridge(
              State
              ,default_emergency_number(wh_util:anonymous_caller_id_number())
              ,Name
             );
        'true' -> deny_emergency_bridge(State)
    end;
maybe_deny_emergency_bridge(#state{control_queue=ControlQ}=State, Number, Name) ->
    wapi_dialplan:publish_command(
      ControlQ
      ,build_bridge(State, Number, Name)
     ),
    lager:debug("sent bridge command to ~s", [ControlQ]).

-spec build_bridge(state(), api_binary(), api_binary()) -> wh_proplist().
build_bridge(#state{endpoints=Endpoints
                    ,resource_req=OffnetJObj
                    ,queue=Q
                   }
            ,Number
            ,Name
            ) ->
    lager:debug("set outbound caller id to ~s '~s'", [Number, Name]),
    AccountId = wapi_offnet_resource:account_id(OffnetJObj),
    CCVs =
        wh_json:set_values(
          props:filter_undefined(
            [{<<"Ignore-Display-Updates">>, <<"true">>}
             ,{<<"Account-ID">>, AccountId}
             ,{<<"From-URI">>, bridge_from_uri(Number, OffnetJObj)}
             ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
            ])
          ,wh_json:get_value(<<"Custom-Channel-Vars">>, OffnetJObj, wh_json:new())
         ),

    EndpointFilter = fun(Element) ->
                             case Element of
                                 {<<"Outbound-Caller-ID-Number">>, Number} -> false;
                                 {<<"Outbound-Caller-ID-Name">>, Name}     -> false;
                                 _OtherValues                              -> true
                             end
                     end,

    FmtEndpoints = format_endpoints(Endpoints, Number, OffnetJObj, EndpointFilter),

    props:filter_undefined(
      [{<<"Application-Name">>, <<"bridge">>}
       ,{<<"Dial-Endpoint-Method">>, <<"single">>}
       ,{<<"Outbound-Caller-ID-Number">>, Number}
       ,{<<"Outbound-Caller-ID-Name">>, Name}
       ,{<<"Caller-ID-Number">>, Number}
       ,{<<"Caller-ID-Name">>, Name}
       ,{<<"Custom-Channel-Vars">>, CCVs}
       ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, OffnetJObj)}
       ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, OffnetJObj, <<"false">>)}
       ,{<<"Media">>, wh_json:get_value(<<"Media">>, OffnetJObj)}
       ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, OffnetJObj)}
       ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, OffnetJObj)}
       ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, OffnetJObj)}
       ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, OffnetJObj)}
       ,{<<"Fax-Identity-Number">>, wh_json:get_value(<<"Fax-Identity-Number">>, OffnetJObj, Number)}
       ,{<<"Fax-Identity-Name">>, wh_json:get_value(<<"Fax-Identity-Name">>, OffnetJObj, Name)}
       ,{<<"Outbound-Callee-ID-Number">>, wh_json:get_value(<<"Outbound-Callee-ID-Number">>, OffnetJObj)}
       ,{<<"Outbound-Callee-ID-Name">>, wh_json:get_value(<<"Outbound-Callee-ID-Name">>, OffnetJObj)}
       ,{<<"B-Leg-Events">>, wh_json:get_list_value(<<"B-Leg-Events">>, OffnetJObj, [])}
       ,{<<"Endpoints">>, FmtEndpoints}
       | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
      ]).

-spec format_endpoints(wh_json:objects(), api_binary(), wh_json:object(), fun()) ->
                              wh_json:objects().
format_endpoints(Endpoints, Number, OffnetJObj, Filter) ->
    DefaultRealm = wh_json:get_first_defined([<<"From-URI-Realm">>
                                              ,<<"Account-Realm">>
                                             ], OffnetJObj),
    [format_endpoint(Endpoint, Number, DefaultRealm, Filter)
     || Endpoint <- Endpoints
    ].

-spec format_endpoint(wh_json:object(), api_binary(), api_binary(), fun()) -> wh_json:object().
format_endpoint(Endpoint, Number, DefaultRealm, Filter) ->
    FilteredEndpoint = wh_json:filter(Filter, Endpoint),
    maybe_endpoint_format_from(FilteredEndpoint, Number, DefaultRealm).

-spec maybe_endpoint_format_from(wh_json:object(), ne_binary(), api_binary()) ->
                                        wh_json:object().
maybe_endpoint_format_from(Endpoint, Number, DefaultRealm) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, wh_json:new()),
    case wh_json:is_true(<<"Format-From-URI">>, CCVs) of
        'true' -> endpoint_format_from(Endpoint, Number, DefaultRealm, CCVs);
        'false' ->
            wh_json:set_value(<<"Custom-Channel-Vars">>
                              ,wh_json:delete_keys([<<"Format-From-URI">>
                                                    ,<<"From-URI-Realm">>
                                                   ], CCVs)
                              ,Endpoint)
    end.

-spec endpoint_format_from(wh_json:object(), ne_binary(), api_binary(), wh_json:object()) ->
                                  wh_json:object().
endpoint_format_from(Endpoint, Number, DefaultRealm, CCVs) ->
    case wh_json:get_value(<<"From-URI-Realm">>, CCVs, DefaultRealm) of
        <<_/binary>> = Realm ->
            FromURI = <<"sip:", Number/binary, "@", Realm/binary>>,
            lager:debug("setting resource ~s from-uri to ~s"
                        ,[wh_json:get_value(<<"Resource-ID">>, CCVs)
                          ,FromURI
                         ]),
            UpdatedCCVs = wh_json:set_value(<<"From-URI">>, FromURI, CCVs),
            wh_json:set_value(<<"Custom-Channel-Vars">>
                              ,wh_json:delete_keys([<<"Format-From-URI">>
                                                    ,<<"From-URI-Realm">>
                                                   ], UpdatedCCVs)
                              ,Endpoint);
        _ ->
            wh_json:set_value(<<"Custom-Channel-Vars">>
                              ,wh_json:delete_keys([<<"Format-From-URI">>
                                                    ,<<"From-URI-Realm">>
                                                   ], CCVs)
                              ,Endpoint)
    end.

-spec bridge_from_uri(api_binary(), wh_json:object()) ->
                             api_binary().
bridge_from_uri(Number, JObj) ->
    Realm = wh_json:get_first_defined([<<"From-URI-Realm">>
                                       ,<<"Account-Realm">>
                                      ], JObj),
    case (whapps_config:get_is_true(?SS_CONFIG_CAT, <<"format_from_uri">>, 'false')
          orelse wh_json:is_true(<<"Format-From-URI">>, JObj)
         )
        andalso (is_binary(Number) andalso is_binary(Realm))
    of
        'false' -> 'undefined';
        'true' ->
            FromURI = <<"sip:", Number/binary, "@", Realm/binary>>,
            lager:debug("setting bridge from-uri to ~s", [FromURI]),
            FromURI
    end.

-spec bridge_outbound_cid_name(wh_json:object()) -> api_binary().
bridge_outbound_cid_name(JObj) ->
    wh_json:get_first_defined(
      [<<"Outbound-Caller-ID-Name">>
       ,<<"Emergency-Caller-ID-Name">>
      ], JObj
     ).

-spec bridge_outbound_cid_number(wh_json:object()) -> api_binary().
bridge_outbound_cid_number(JObj) ->
    wh_json:get_first_defined(
      [<<"Outbound-Caller-ID-Number">>
       ,<<"Emergency-Caller-ID-Number">>
      ], JObj
     ).

-spec bridge_emergency_cid_name(wh_json:object()) -> api_binary().
bridge_emergency_cid_name(JObj) ->
    wh_json:get_first_defined(
      [<<"Emergency-Caller-ID-Name">>
       ,<<"Outbound-Caller-ID-Name">>
      ], JObj
     ).

-spec bridge_emergency_cid_number(wh_json:object()) -> api_binary().
bridge_emergency_cid_number(JObj) ->
    wh_json:get_first_defined(
      [<<"Emergency-Caller-ID-Number">>
       ,<<"Outbound-Caller-ID-Number">>
      ], JObj
     ).

-spec find_emergency_number(wh_json:object()) -> api_binary().
find_emergency_number(JObj) ->
    case whapps_config:get_is_true(
           ?SS_CONFIG_CAT
           ,<<"ensure_valid_emergency_cid">>
           ,'false'
          )
    of
        'true' -> ensure_valid_emergency_number(JObj);
        'false' ->
            lager:debug("using first configured unverified emergency caller id"),
            bridge_emergency_cid_number(JObj)
    end.

-spec ensure_valid_emergency_number(wh_json:object()) -> api_binary().
ensure_valid_emergency_number(JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    lager:debug("ensuring emergency caller is valid for account ~s", [AccountId]),
    Numbers = valid_emergency_numbers(AccountId),
    Emergency = bridge_emergency_cid_number(JObj),
    Outbound = bridge_outbound_cid_number(JObj),
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

-spec valid_emergency_numbers(ne_binary()) -> ne_binaries().
valid_emergency_numbers(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {'ok', JObj} ->
            [Number
             || Number <- wh_json:get_public_keys(JObj),
                wnm_util:emergency_services_configured(Number, JObj)
            ];
        {'error', _R} ->
            []
    end.

-spec default_emergency_number(ne_binary()) -> ne_binary().
default_emergency_number(Requested) ->
    case whapps_config:get_non_empty(
           ?SS_CONFIG_CAT
           ,<<"default_emergency_cid_number">>
          )
    of
        'undefined' -> Requested;
        Else -> Else
    end.

-spec contains_emergency_endpoints(wh_json:objects()) -> boolean().
contains_emergency_endpoints([]) -> 'false';
contains_emergency_endpoints([Endpoint|Endpoints]) ->
    case wh_json:is_true([<<"Custom-Channel-Vars">>, <<"Emergency-Resource">>], Endpoint) of
        'true' ->
            lager:debug("endpoints contain an emergency resource", []),
            'true';
        'false' -> contains_emergency_endpoints(Endpoints)
    end.

-spec bridge_timeout(wh_json:object()) -> wh_proplist().
bridge_timeout(Request) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, <<"bridge request timed out">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Request)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec bridge_error(wh_json:object(), wh_json:object()) -> wh_proplist().
bridge_error(JObj, Request) ->
    lager:debug("error during outbound request: ~s", [wh_util:to_binary(wh_json:encode(JObj))]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, wh_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Request)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec bridge_success(wh_json:object(), wh_json:object()) -> wh_proplist().
bridge_success(JObj, Request) ->
    lager:debug("outbound request successfully completed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"SUCCESS">>}
     ,{<<"Response-Code">>, <<"sip:200">>}
     ,{<<"Resource-Response">>, JObj}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec bridge_failure(wh_json:object(), wh_json:object()) -> wh_proplist().
bridge_failure(JObj, Request) ->
    lager:debug("resources for outbound request failed: ~s", [wh_json:get_value(<<"Disposition">>, JObj)]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, wh_json:get_first_defined([<<"Application-Response">>
                                                          ,<<"Hangup-Cause">>
                                                         ], JObj)}
     ,{<<"Response-Code">>, wh_json:get_value(<<"Hangup-Code">>, JObj)}
     ,{<<"Resource-Response">>, JObj}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec bridge_not_configured(wh_json:object()) -> wh_proplist().
bridge_not_configured(Request) ->
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"MANDATORY_IE_MISSING">>}
     ,{<<"Response-Code">>, <<"sip:403">>}
     ,{<<"Error-Message">>, <<"services not configured">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Request)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec deny_emergency_bridge(state()) -> 'ok'.
deny_emergency_bridge(#state{resource_req=JObj, control_queue=ControlQ}) ->
    lager:warning("terminating attempted emergency bridge from unconfigured device"),
    send_deny_emergency_response(JObj, ControlQ),
    send_deny_emergency_notification(JObj),
    Result = bridge_not_configured(JObj),
    gen_listener:cast(self(), {'bridge_result', Result}).

-spec send_deny_emergency_notification(wh_json:object()) -> 'ok'.
send_deny_emergency_notification(Request) ->
    Props = [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
             ,{<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, Request)}
             ,{<<"Emergency-Caller-ID-Number">>, wh_json:get_value(<<"Emergency-Caller-ID-Number">>, Request)}
             ,{<<"Emergency-Caller-ID-Name">>, wh_json:get_value(<<"Emergency-Caller-ID-Name">>, Request)}
             ,{<<"Outbound-Caller-ID-Number">>, wh_json:get_value(<<"Outbound-Caller-ID-Number">>, Request)}
             ,{<<"Outbound-Caller-ID-Name">>, wh_json:get_value(<<"Outbound-Caller-ID-Name">>, Request)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wapi_notifications:publish_denied_emergency_bridge(Props).

-spec send_deny_emergency_response(wh_json:object(), ne_binary()) -> 'ok'.
send_deny_emergency_response(JObj, ControlQ) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Code = whapps_config:get_ne_binary(
             ?SS_CONFIG_CAT
             ,<<"deny_emergency_bridge_code">>
             ,486
            ),
    Cause = whapps_config:get_ne_binary(
              ?SS_CONFIG_CAT
              ,<<"deny_emergency_bridge_cause">>
              ,<<"Emergency service not configured">>
             ),
    Media = whapps_config:get_ne_binary(
              ?SS_CONFIG_CAT
              ,<<"deny_emergency_bridge_media">>
              ,<<"prompt://system_media/stepswitch-emergency_not_configured/">>
             ),
    wh_call_response:send(CallId, ControlQ, Code, Cause, Media).
