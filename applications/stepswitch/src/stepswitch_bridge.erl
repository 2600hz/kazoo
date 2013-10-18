%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
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

-record(state, {endpoints
                ,resource_req
                ,request_handler
                ,control_queue
                ,response_queue
                ,queue
                ,timeout}).

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
start_link(Endpoints, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Bindings = [{'call', [{'callid', CallId}
                          ,{'restrict_to', ['events'
                                            ,'destroy_channel'
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
handle_cast({'gen_listener', {'is_consuming', 'true'}}, #state{control_queue=ControlQ}=State) ->
    'ok' = wapi_dialplan:publish_command(ControlQ, build_bridge(State)),
    lager:debug("sent bridge command to ~s", [ControlQ]),
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
                                     ,resource_req=JObj}=State) ->
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
handle_event(JObj, #state{request_handler=RequestHandler, resource_req=Request}) ->
    case whapps_util:get_event_type(JObj) of
        {<<"error">>, _} ->
            <<"bridge">> = wh_json:get_value([<<"Request">>, <<"Application-Name">>], JObj),
            lager:debug("channel execution error while waiting for bridge: ~s"
                        ,[wh_util:to_binary(wh_json:encode(JObj))]),
            gen_listener:cast(RequestHandler, {'bridge_result', bridge_error(JObj, Request)});
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            lager:debug("channel was destroy while waiting for bridge", []),
            Result = case wh_json:get_value(<<"Disposition">>, JObj) 
                         =:= <<"SUCCESS">> 
                     of
                         'true' -> bridge_success(JObj, Request);
                         'false' -> bridge_failure(JObj, Request)
                     end,
            gen_listener:cast(RequestHandler, {'bridge_result', Result});
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            <<"bridge">> = wh_json:get_value(<<"Application-Name">>, JObj),
            lager:debug("channel execute complete for bridge", []),
            Result = case wh_json:get_value(<<"Disposition">>, JObj) 
                         =:= <<"SUCCESS">> 
                     of
                         'true' -> bridge_success(JObj, Request);
                         'false' -> bridge_failure(JObj, Request)
                     end,
            gen_listener:cast(RequestHandler, {'bridge_result', Result});
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>} ->
            CallId = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
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
build_bridge(#state{endpoints=Endpoints, resource_req=JObj, queue=Q}) ->
    {CIDNum, CIDName} = bridge_caller_id(Endpoints, JObj),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    CCVUpdates = props:filter_undefined([{<<"Ignore-Display-Updates">>, <<"true">>}
                                         ,{<<"Account-ID">>, AccountId}
                                         ,{<<"From-URI">>, bridge_from_uri(CIDNum, JObj)}
                                         ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
                                        ]),
    props:filter_undefined(
      [{<<"Application-Name">>, <<"bridge">>}
       ,{<<"Dial-Endpoint-Method">>, <<"single">>}
       ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
       ,{<<"Outbound-Caller-ID-Name">>, CIDName}
       ,{<<"Caller-ID-Number">>, CIDNum}
       ,{<<"Caller-ID-Name">>, CIDName}
       ,{<<"Endpoints">>, maybe_endpoints_format_from(Endpoints, CIDNum, JObj)}
       ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
       ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj, <<"false">>)}
       ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
       ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
       ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
       ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
       ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
       ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, CCVs)}
       ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
       ,{<<"Fax-Identity-Number">>, wh_json:get_value(<<"Fax-Identity-Number">>, JObj, CIDNum)}
       ,{<<"Fax-Identity-Name">>, wh_json:get_value(<<"Fax-Identity-Name">>, JObj, CIDName)}
       ,{<<"Outbound-Callee-ID-Number">>, wh_json:get_value(<<"Outbound-Callee-ID-Number">>, JObj)}
       ,{<<"Outbound-Callee-ID-Name">>, wh_json:get_value(<<"Outbound-Callee-ID-Name">>, JObj)}
       | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
      ]).

-spec maybe_endpoints_format_from(wh_proplists(), ne_binary(), api_binary()) -> wh_proplists().
maybe_endpoints_format_from(Endpoints, CIDNum, JObj) when is_binary(CIDNum) ->        
    DefaultRealm = wh_json:get_first_defined([<<"From-URI-Realm">>
                                              ,<<"Account-Realm">>
                                             ], JObj),
    [maybe_endpoint_format_from(Endpoint, CIDNum, DefaultRealm)
     || Endpoint <- Endpoints
    ];
maybe_endpoints_format_from(Endpoints, _, _) -> Endpoints.

-spec maybe_endpoint_format_from(wh_proplist(), ne_binary(), api_binary()) -> wh_proplist().
maybe_endpoint_format_from(Endpoint, CIDNum, DefaultRealm) ->
    CCVs = props:get_value(<<"Custom-Channel-Vars">>, Endpoint, wh_json:new()),
    case wh_json:is_true(<<"Format-From-URI">>, CCVs) of
        'true' -> endpoint_format_from(Endpoint, CIDNum, DefaultRealm);
        'false' -> 
            props:set_value(<<"Custom-Channel-Vars">>
                            ,wh_json:delete_keys([<<"Format-From-URI">>
                                                  ,<<"From-URI-Realm">>
                                                 ], CCVs)
                            ,Endpoint)
    end.

-spec endpoint_format_from(wh_proplist(), ne_binary(), api_binary()) -> wh_proplist().
endpoint_format_from(Endpoint, CIDNum, DefaultRealm) ->
    CCVs = props:get_value(<<"Custom-Channel-Vars">>, Endpoint, wh_json:new()),
    Realm = wh_json:get_value(<<"From-URI-Realm">>, CCVs, DefaultRealm),
    case is_binary(Realm) of
        'false' ->            
            props:set_value(<<"Custom-Channel-Vars">>
                            ,wh_json:delete_keys([<<"Format-From-URI">>
                                                  ,<<"From-URI-Realm">>
                                                 ], CCVs)
                            ,Endpoint);
        'true' ->
            FromURI = <<"sip:", CIDNum/binary, "@", Realm/binary>>,
            lager:debug("setting resource ~s from-uri to ~s"
                        ,[wh_json:get_value(<<"Resource-ID">>, CCVs)
                          ,FromURI
                         ]),
            UpdatedCCVs = wh_json:set_value(<<"From-URI">>, FromURI, CCVs),
            props:set_value(<<"Custom-Channel-Vars">>
                            ,wh_json:delete_keys([<<"Format-From-URI">>
                                                  ,<<"From-URI-Realm">>
                                                 ], UpdatedCCVs)
                            ,Endpoint)
    end.

-spec bridge_caller_id(wh_json:objects(), wh_json:object()) -> {api_binary(), api_binary()}.
bridge_caller_id(Endpoints, JObj) ->
    case contains_emergency_endpoints(Endpoints) of
        'true' -> bridge_emergency_caller_id(JObj);
        'false' -> bridge_caller_id(JObj)
    end.

-spec bridge_emergency_caller_id(wh_json:object()) -> {api_binary(), api_binary()}.
bridge_emergency_caller_id(JObj) ->
    lager:debug("outbound call is using an emergency route, attempting to set CID accordingly"),
    {emergency_cid_number(JObj)
     ,wh_json:get_first_defined([<<"Emergency-Caller-ID-Name">>
                                 ,<<"Outbound-Caller-ID-Name">>
                                ], JObj)
    }.

-spec bridge_caller_id(wh_json:object()) -> {api_binary(), api_binary()}.
bridge_caller_id(JObj) ->
    {wh_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                                ,<<"Emergency-Caller-ID-Number">>
                               ], JObj)
     ,wh_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                                 ,<<"Emergency-Caller-ID-Name">>
                                ], JObj)
    }.
    
-spec bridge_from_uri(ne_binary(), wh_json:object()) -> api_binary().
bridge_from_uri(CIDNum, JObj) ->
    Realm = wh_json:get_first_defined([<<"From-URI-Realm">>
                                       ,<<"Account-Realm">>
                                      ], JObj),
    case (whapps_config:get_is_true(?APP_NAME, <<"format_from_uri">>, 'false')
          orelse wh_json:is_true(<<"Format-From-URI">>, JObj))
        andalso (is_binary(CIDNum) andalso is_binary(Realm))
    of
        'false' -> 'undefined';
        'true' -> 
            FromURI = <<"sip:", CIDNum/binary, "@", Realm/binary>>,
            lager:debug("setting bridge from-uri to ~s", [FromURI]),
            FromURI
    end.

-spec emergency_cid_number(wh_json:object()) -> ne_binary().
emergency_cid_number(JObj) ->
    Account = wh_json:get_value(<<"Account-ID">>, JObj),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    Candidates = [wh_json:get_ne_value(<<"Emergency-Caller-ID-Number">>, JObj)
                  ,wh_json:get_ne_value(<<"Outbound-Caller-ID-Number">>, JObj)
                 ],
    Requested = wh_json:get_first_defined([<<"Emergency-Caller-ID-Number">>
                                           ,<<"Outbound-Caller-ID-Number">>
                                          ], JObj),
    case couch_mgr:open_cache_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {'ok', PhoneNumbers} ->
            Numbers = wh_json:get_keys(wh_json:public_fields(PhoneNumbers)),
            E911Enabled = [Number
                           || Number <- Numbers
                                  ,dash_e911_enabled(Number, PhoneNumbers)
                          ],
            emergency_cid_number(Requested, Candidates, E911Enabled);
        {'error', _R} ->
            lager:error("unable to fetch the ~s from account ~s: ~p", [?WNM_PHONE_NUMBER_DOC, Account, _R]),
            emergency_cid_number(Requested, Candidates, [])
    end.

-spec dash_e911_enabled(ne_binary(), wh_json:object()) -> boolean().
dash_e911_enabled(Number, PhoneNumbers) ->
    lists:member(<<"dash_e911">>, wh_json:get_value([Number, <<"features">>], PhoneNumbers, [])).

-spec emergency_cid_number(ne_binary(), api_binaries(), ne_binaries()) -> ne_binary().
%% if there are no e911 enabled numbers then either use the global system default
%% or the requested (if there isnt one)
emergency_cid_number(Requested, _, []) ->
    case whapps_config:get_non_empty(<<"stepswitch">>, <<"default_emergency_cid_number">>) of
        'undefined' -> Requested;
        DefaultE911 -> DefaultE911
    end;
%% If neither their emergency cid or outgoung cid is e911 enabled but their account
%% has other numbers with e911 then use the first...
emergency_cid_number(_, [], [E911Enabled|_]) -> E911Enabled;
%% due to the way we built the candidates list it can contain the atom 'undefined'
%% handle that condition (ignore)
emergency_cid_number(Requested, ['undefined'|Candidates], E911Enabled) ->
    emergency_cid_number(Requested, Candidates, E911Enabled);
%% check if the first non-atom undefined element in the list is in the list of
%% e911 enabled numbers, if so use it otherwise keep checking.
emergency_cid_number(Requested, [Candidate|Candidates], E911Enabled) ->
    case lists:member(Candidate, E911Enabled) of
        'true' -> Candidate;
        'false' -> emergency_cid_number(Requested, Candidates, E911Enabled)
    end.

-spec contains_emergency_endpoints(wh_json:objects()) -> boolean().
contains_emergency_endpoints([]) -> 'false';
contains_emergency_endpoints([Endpoint|Endpoints]) ->
    case wh_json:is_true(?CCV(<<"Emergency-Resource">>), Endpoint) of
        'true' -> 'true';
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
