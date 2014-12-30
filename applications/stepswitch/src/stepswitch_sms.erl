%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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

-define(RESPONDERS, [{{?MODULE, 'handle_message_delivery'}
                      ,[{<<"message">>, <<"delivery">>}]
                     }
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
    Bindings = [{'self', []}],
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
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case wh_json:get_ne_value(<<"Control-Queue">>, JObj, <<>>) of
        'undefined' ->
            lager:debug("Control-Queue is undefined for Call-ID ~s, exiting.", [CallId]),
            {'stop', 'normal'};
        ControlQ ->
            {'ok', #state{endpoints=Endpoints
                          ,resource_req=JObj
                          ,request_handler=self()
                          ,control_queue=ControlQ
                          ,response_queue=wh_json:get_ne_value(<<"Server-ID">>, JObj)
                          ,timeout=erlang:send_after(60000, self(), 'sms_timeout')
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
    'ok' = wapi_sms:publish_message(build_sms(State)),
    lager:debug("sent sms command"),
    {'noreply', State};
handle_cast({'sms_result', _Props}, #state{response_queue='undefined'}=State) ->
    {'stop', 'normal', State};
handle_cast({'sms_result', Props}, #state{response_queue=ResponseQ}=State) ->
    wapi_offnet_resource:publish_resp(ResponseQ, Props),
    {'stop', 'normal', State};
handle_cast({'sms_success', JObj}, #state{resource_req=Request}=State) ->
    gen_listener:cast(self(), {'sms_result', sms_success(JObj, Request)}),
    {'noreply', State};
handle_cast({'sms_failure', JObj}, #state{resource_req=Request}=State) ->
    gen_listener:cast(self(), {'sms_result', sms_failure(JObj, Request)}),
    {'noreply', State};
handle_cast({'sms_error', JObj}, #state{resource_req=Request}=State) ->
    gen_listener:cast(self(), {'sms_result', sms_error(JObj, Request)}),
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
handle_info('sms_timeout', #state{timeout='undefined'}=State) ->
    {'noreply', State};
handle_info('sms_timeout', #state{response_queue=ResponseQ
                                     ,resource_req=JObj
                                    }=State) ->
    wapi_offnet_resource:publish_resp(ResponseQ, sms_timeout(JObj)),
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
handle_event(_JObj, _State) ->
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

-spec handle_message_delivery(wh_json:object(), wh_proplist()) -> no_return().
handle_message_delivery(JObj, Props) ->
    _ = wh_util:put_callid(JObj),
    Server = props:get_value('server',Props),
    'true' = wapi_sms:delivery_v(JObj),
    case wh_json:is_true(<<"Delivery-Failure">>, JObj) of
        'true'  -> gen_listener:cast(Server, {'sms_failure', JObj});
        'false' -> gen_listener:cast(Server, {'sms_success', JObj})
    end.

-spec build_sms(state()) -> wh_proplist().
build_sms(#state{endpoints=Endpoints
                 ,resource_req=JObj
                 ,queue=Q
                }) ->
    {CIDNum, CIDName} = bridge_caller_id(Endpoints, JObj),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AccountRealm = wh_json:get_value(<<"Account-Realm">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    CCVUpdates = props:filter_undefined(
                   [{<<"Ignore-Display-Updates">>, <<"true">>}
                    ,{<<"Account-ID">>, AccountId}
                    ,{<<"Bounce-Back">>, wh_json:get_value(<<"Bounce-Back">>, JObj)}
                    ,{<<"Account-Realm">>, AccountRealm}
                    ,{<<"From-URI">>, bridge_from_uri(CIDNum, JObj)}
                    ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
                   ]),
    props:filter_undefined(
      [{<<"Application-Name">>, <<"send">>}
       ,{<<"Dial-Endpoint-Method">>, <<"single">>}
       ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
       ,{<<"Outbound-Caller-ID-Name">>, CIDName}
       ,{<<"Caller-ID-Number">>, CIDNum}
       ,{<<"Caller-ID-Name">>, CIDName}
       ,{<<"Endpoints">>, maybe_endpoints_format_from(Endpoints, CIDNum, JObj) }
       ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
       ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, CCVs)}
       ,{<<"Custom-SIP-Headers">>, get_sip_headers(JObj)}
       ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
       ,{<<"Outbound-Callee-ID-Number">>, wh_json:get_value(<<"Outbound-Callee-ID-Number">>, JObj)}
       ,{<<"Outbound-Callee-ID-Name">>, wh_json:get_value(<<"Outbound-Callee-ID-Name">>, JObj)}
       ,{<<"Message-ID">>, wh_json:get_value(<<"Message-ID">>, JObj)}
       ,{<<"Body">>, wh_json:get_value(<<"Body">>, JObj)}
       | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
      ]).

-spec maybe_endpoints_format_from(wh_json:objects(), api_binary(), wh_json:object()) ->
                                         wh_json:objects().
maybe_endpoints_format_from([], _ , _) -> [];
maybe_endpoints_format_from(Endpoints, 'undefined', _) -> Endpoints;
maybe_endpoints_format_from(Endpoints, CIDNum, JObj) ->
    DefaultRealm = wh_json:get_first_defined([<<"From-URI-Realm">>
                                              ,<<"Account-Realm">>
                                             ], JObj),
    [maybe_endpoint_format_from(Endpoint, CIDNum, DefaultRealm)
     || Endpoint <- Endpoints
    ].

-spec maybe_endpoint_format_from(wh_json:object(), ne_binary(), api_binary()) ->
                                        wh_json:object().
maybe_endpoint_format_from(Endpoint, CIDNum, DefaultRealm) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, wh_json:new()),
    case wh_json:is_true(<<"Format-From-URI">>, CCVs) of
        'true' -> endpoint_format_from(Endpoint, CIDNum, DefaultRealm);
        'false' ->
            wh_json:set_value(<<"Custom-Channel-Vars">>
                              ,wh_json:delete_keys([<<"Format-From-URI">>
                                                    ,<<"From-URI-Realm">>
                                                   ], CCVs)
                              ,Endpoint
                             )
    end.

-spec endpoint_format_from(wh_json:object(), ne_binary(), api_binary()) -> wh_json:object().
endpoint_format_from(Endpoint, CIDNum, DefaultRealm) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, wh_json:new()),
    Realm = wh_json:get_value(<<"From-URI-Realm">>, CCVs, DefaultRealm),
    case is_binary(Realm) of
        'false' ->
            wh_json:set_value(<<"Custom-Channel-Vars">>
                              ,wh_json:delete_keys([<<"Format-From-URI">>
                                                    ,<<"From-URI-Realm">>
                                                   ], CCVs)
                              ,Endpoint
                             );
        'true' ->
            FromURI = <<"sip:", CIDNum/binary, "@", Realm/binary>>,
            lager:debug("setting resource ~s from-uri to ~s"
                        ,[wh_json:get_value(<<"Resource-ID">>, CCVs)
                          ,FromURI
                         ]),
            UpdatedCCVs = wh_json:set_value(<<"From-URI">>, FromURI, CCVs),
            wh_json:set_value(<<"Custom-Channel-Vars">>
                              ,wh_json:delete_keys([<<"Format-From-URI">>
                                                    ,<<"From-URI-Realm">>
                                                   ], UpdatedCCVs)
                              ,Endpoint
                             )
    end.

-spec bridge_caller_id(wh_json:objects(), wh_json:object()) ->
                              {api_binary(), api_binary()}.
bridge_caller_id(Endpoints, JObj) ->
    case contains_emergency_endpoints(Endpoints) of
        'true' -> bridge_emergency_caller_id(JObj);
        'false' -> bridge_caller_id(JObj)
    end.

-spec bridge_emergency_caller_id(wh_json:object()) ->
                                        {api_binary(), api_binary()}.
bridge_emergency_caller_id(JObj) ->
    lager:debug("outbound call is using an emergency route, attempting to set CID accordingly"),
    {maybe_emergency_cid_number(JObj)
     ,wh_json:get_first_defined([<<"Emergency-Caller-ID-Name">>
                                 ,<<"Outbound-Caller-ID-Name">>
                                ]
                                ,JObj
                               )
    }.

-spec bridge_caller_id(wh_json:object()) ->
                              {api_binary(), api_binary()}.
bridge_caller_id(JObj) ->
    {wh_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                                ,<<"Emergency-Caller-ID-Number">>
                               ]
                               ,JObj
                              )
     ,wh_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                                 ,<<"Emergency-Caller-ID-Name">>
                                ]
                                ,JObj
                               )
    }.

-spec bridge_from_uri(api_binary(), wh_json:object()) ->
                             api_binary().
bridge_from_uri(CIDNum, JObj) ->
    Realm = wh_json:get_first_defined([<<"From-URI-Realm">>
                                       ,<<"Account-Realm">>
                                      ]
                                      ,JObj
                                     ),
    case (whapps_config:get_is_true(?APP_NAME, <<"format_from_uri">>, 'false')
          orelse wh_json:is_true(<<"Format-From-URI">>, JObj)
         )
        andalso (is_binary(CIDNum) andalso is_binary(Realm))
    of
        'false' -> 'undefined';
        'true' ->
            FromURI = <<"sip:", CIDNum/binary, "@", Realm/binary>>,
            lager:debug("setting bridge from-uri to ~s", [FromURI]),
            FromURI
    end.

-spec maybe_emergency_cid_number(wh_json:object()) ->
                                        api_binary().
maybe_emergency_cid_number(JObj) ->
    %% NOTE: if this request had a hunt-account-id then we
    %%   are assuming it was for a local resource (at the
    %%   time of this commit offnet DB is still in use)
    case wh_json:get_value(<<"Hunt-Account-ID">>, JObj) of
        'undefined' -> emergency_cid_number(JObj);
        _Else ->
            wh_json:get_first_defined([<<"Emergency-Caller-ID-Number">>
                                       ,<<"Outbound-Caller-ID-Number">>
                                      ], JObj)
    end.

-spec emergency_cid_number(wh_json:object()) ->
                                  ne_binary().
emergency_cid_number(JObj) ->
    Account = wh_json:get_value(<<"Account-ID">>, JObj),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    Candidates = [wh_json:get_ne_value(<<"Emergency-Caller-ID-Number">>, JObj)
                  ,wh_json:get_ne_value(<<"Outbound-Caller-ID-Number">>, JObj)
                 ],
    Requested = wh_json:get_first_defined([<<"Emergency-Caller-ID-Number">>
                                           ,<<"Outbound-Caller-ID-Number">>
                                          ], JObj),
    lager:debug("ensuring requested CID is e911 enabled: ~s", [Requested]),
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
    case wh_json:is_true([<<"Custom-Channel-Vars">>, <<"Emergency-Resource">>], Endpoint) of
        'true' -> 'true';
        'false' -> contains_emergency_endpoints(Endpoints)
    end.

-spec sms_timeout(wh_json:object()) -> wh_proplist().
sms_timeout(Request) ->
    lager:debug("attempt to connect to resources timed out"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, <<"bridge request timed out">>}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Request)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec sms_error(wh_json:object(), wh_json:object()) -> wh_proplist().
sms_error(JObj, Request) ->
    lager:debug("error during outbound request: ~s", [wh_util:to_binary(wh_json:encode(JObj))]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, wh_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Request)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec sms_success(wh_json:object(), wh_json:object()) -> wh_proplist().
sms_success(JObj, Request) ->
    lager:debug("outbound request successfully completed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"SUCCESS">>}
     ,{<<"Response-Code">>, <<"sip:200">>}
     ,{<<"Resource-Response">>, JObj}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec sms_failure(wh_json:object(), wh_json:object()) -> wh_proplist().
sms_failure(JObj, Request) ->
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

-spec get_sip_headers(wh_json:object()) -> 'undefined' | wh_json:object().
get_sip_headers(JObj) ->
    case get_diversions(JObj) of
        'undefined' -> 'undefined';
        Diversion ->
            wh_json:from_list([{<<"Diversion">>, Diversion}])
    end.

-spec get_diversions(wh_json:object()) -> 'undefined' | wh_json:object().
get_diversions(JObj) ->
    Inception = wh_json:get_value(<<"Inception">>, JObj),
    Diversions = wh_json:get_value([<<"Custom-SIP-Headers">>, <<"Diversion">>], JObj, []),
    get_diversions(Inception, Diversions).

-spec get_diversions(api_binary(), wh_json:object()) -> 'undefined' | wh_json:object().
get_diversions('undefined', _) -> 'undefined';
get_diversions(Inception, Diversions) ->
    wh_json:from_list([{<<"address">>, <<"sip:", Inception/binary>>}
                       ,{<<"counter">>, find_diversion_count(Diversions) + 1}
                      ]).

-spec find_diversion_count(wh_json:objects()) -> non_neg_integer().
find_diversion_count([]) -> 0;
find_diversion_count(Diversions) ->
    lists:max([kzsip_diversion:counter(Diversion) || Diversion <- Diversions]).
