%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% Listener for reg_success, and reg_query AMQP requests
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_registrar).

-behaviour(gen_listener).

-export([start_link/0]).
-export([lookup_contact/2
         ,lookup_original_contact/2
         ,lookup_registration/2
         ,get_registration/2
        ]).
-export([reg_success/2
         ,reg_query/2
         ,reg_flush/2
         ,handle_reg_success/2
         ,summary/0, summary/1
         ,details/0, details/1, details/2
         ,flush/0, flush/1, flush/2
         ,count/0
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-ifdef(TEST).
-export([breakup_contact/1]).
-endif.

-include("ecallmgr.hrl").
-include_lib("nksip/include/nksip.hrl").

-define(RESPONDERS, [{{?MODULE, 'reg_query'}
                      ,[{<<"directory">>, <<"reg_query">>}]
                     }
                     ,{{?MODULE, 'reg_success'}
                       ,[{<<"directory">>, <<"reg_success">>}]
                      }
                     ,{{?MODULE, 'reg_flush'}
                       ,[{<<"directory">>, <<"reg_flush">>}]
                      }
                    ]).
-define(BINDINGS, [{'registration', [{'restrict_to', ['reg_query'
                                                      ,'reg_flush'
                                                     ]}
                                     ,'federate'
                                    ]}
                   ,{'registration', [{'restrict_to', ['reg_success'
                                                      ]}
                                     ]}
                   ,{'self', []}
                  ]).
-define(SERVER, ?MODULE).
-define(REG_QUEUE_NAME, <<>>).
-define(REG_QUEUE_OPTIONS, []).
-define(REG_CONSUME_OPTIONS, []).

-record(state, {started = wh_util:current_tstamp()
                ,queue :: api_binary()
               }).

-record(registration, {id :: {ne_binary(), ne_binary()} | '_' | '$1'
                       ,username :: ne_binary() | '_'
                       ,realm :: ne_binary() | '_' | '$1'
                       ,network_port :: ne_binary() | '_'
                       ,network_ip :: ne_binary() | '_'
                       ,to_host :: ne_binary() | '_'
                       ,to_user :: ne_binary() | '_'
                       ,from_host :: ne_binary() | '_'
                       ,from_user :: ne_binary() | '_'
                       ,call_id :: ne_binary() | '_'
                       ,user_agent :: ne_binary() | '_'
                       ,expires :: non_neg_integer() | '_' | '$1'
                       ,contact :: ne_binary() | '_'
                       ,previous_contact :: api_binary() | '_'
                       ,original_contact :: ne_binary() | '_'
                       ,last_registration :: non_neg_integer() | '_' | '$2'
                       ,initial_registration :: non_neg_integer() | '_'
                       ,registrar_node :: ne_binary() | '_'
                       ,registrar_hostname :: ne_binary() | '_'
                       ,suppress_unregister = 'true' :: boolean() | '_'
                       ,register_overwrite_notify = 'false' :: boolean() | '_'
                       ,account_db :: api_binary() | '_'
                       ,account_id :: api_binary() | '_'
                       ,authorizing_id :: api_binary() | '_'
                       ,authorizing_type :: api_binary() | '_'
                       ,owner_id :: api_binary() | '_'
                       ,initial = 'true' :: boolean() | '_'
                       ,account_realm :: api_binary() | '_' | '$2'
                       ,account_name :: api_binary() | '_'
                       ,proxy :: api_binary() | '_'
                       ,bridge_uri :: api_binary() | '_'
                      }).

-type registration() :: #registration{}.
-type registrations() :: [registration(),...] | [].

-define(EXPIRES_MISSING_VALUE, 0).

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
start_link() ->
    gen_listener:start_link({'local', ?MODULE}
                            ,?MODULE
                            ,[{'responders', ?RESPONDERS}
                              ,{'bindings', ?BINDINGS}
                              ,{'queue_name', ?REG_QUEUE_NAME}
                              ,{'queue_options', ?REG_QUEUE_OPTIONS}
                              ,{'consume_options', ?REG_CONSUME_OPTIONS}
                             ]
                            ,[]
                           ).

-spec reg_success(wh_json:object(), wh_proplist()) -> 'ok'.
reg_success(JObj, _Props) ->
    'true' = wapi_registration:success_v(JObj),
    _ = wh_util:put_callid(JObj),
    Registration = create_registration(JObj),
    reg_success(Registration).

-spec reg_success(registration()) -> 'ok'.
reg_success(#registration{expires=0}=Registration) ->
    lager:info("deleting registration ~s@~s with contact ~s", [Registration#registration.username
                                                               ,Registration#registration.realm
                                                               ,Registration#registration.contact
                                                              ]),
    gen_server:cast(?MODULE, {'delete_registration', Registration});
reg_success(#registration{initial='true'}=Registration) ->
    gen_server:cast(?MODULE, {'insert_registration', Registration}),
    lager:info("inserted registration ~s@~s with contact ~s", [Registration#registration.username
                                                               ,Registration#registration.realm
                                                               ,Registration#registration.contact
                                                              ]),
    whistle_stats:increment_counter("register-success"),
    _ = initial_registration(Registration),
    maybe_registration_notify(Registration);
reg_success(#registration{}=Registration) ->
    gen_server:cast(?MODULE, {'insert_registration', Registration}),
    lager:debug("updated registration ~s@~s with contact ~s", [Registration#registration.username
                                                               ,Registration#registration.realm
                                                               ,Registration#registration.contact
                                                              ]),
    whistle_stats:increment_counter("register-success").

-spec reg_query(wh_json:object(), wh_proplist()) -> 'ok'.
reg_query(JObj, _Props) ->
    'true' = wapi_registration:query_req_v(JObj),
    _ = wh_util:put_callid(JObj),
    maybe_resp_to_query(JObj).

reg_flush(JObj, _Props) ->
    'true' = wapi_registration:flush_v(JObj),
    lager:debug("recv req to flush ~s @ ~s", [wh_json:get_value(<<"Username">>, JObj)
                                              ,wh_json:get_value(<<"Realm">>, JObj)
                                             ]),
    flush(wh_json:get_value(<<"Username">>, JObj)
          ,wh_json:get_value(<<"Realm">>, JObj)
         ).

-spec lookup_contact(ne_binary(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            {'error', 'not_found'}.
lookup_contact(<<>>, _Username) -> {'error', 'not_found'};
lookup_contact(_Realm, <<>>) -> {'error', 'not_found'};
lookup_contact(<<_/binary>> = Realm, <<_/binary>> = Username) ->
    case get_registration(Realm, Username) of
        'undefined' -> maybe_fetch_contact(Username, Realm);
        #registration{contact=Contact
                      ,bridge_uri='undefined'
                     } ->
            lager:info("found user ~s@~s contact ~s"
                       ,[Username, Realm, Contact]
                      ),
            {'ok', Contact};
        #registration{bridge_uri=Contact} ->
            lager:info("found user ~s@~s bridge uri  ~s"
                       ,[Username, Realm, Contact]
                              ),
            {'ok', Contact}
    end.

-spec lookup_original_contact(ne_binary(), ne_binary()) ->
                                     {'ok', ne_binary()} |
                                     {'error', 'not_found'}.
lookup_original_contact(Realm, Username) ->
    case wh_util:is_empty(Realm) orelse wh_util:is_empty(Username) of
        'true' -> {'error', 'not_found'};
        'false' ->
            case get_registration(Realm, Username) of
                #registration{original_contact=Contact} ->
                    lager:info("found user ~s@~s original contact ~s"
                               ,[Username, Realm, Contact]
                              ),
                    {'ok', Contact};
                'undefined' -> maybe_fetch_original_contact(Username, Realm)
            end
    end.

-spec lookup_registration(ne_binary(), ne_binary()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', 'not_found'}.
lookup_registration(Realm, Username) ->
    case get_registration(Realm, Username) of
        #registration{}=Registration ->
            {'ok', wh_json:from_list(to_props(Registration))};
        'undefined' -> maybe_fetch_registration(Username, Realm)
    end.

-spec get_registration(ne_binary(), ne_binary()) -> 'undefined' | registration().
get_registration(Realm, Username) ->
    case ets:lookup(?MODULE, registration_id(Username, Realm)) of
        [#registration{}=Registration] ->
            Registration;
        _ -> 'undefined'
    end.

-spec summary() -> 'ok'.
summary() ->
    MatchSpec = [{#registration{_ = '_'}
                  ,[]
                  ,['$_']
                 }],
    print_summary(ets:select(?MODULE, MatchSpec, 1)).

-spec summary(text()) -> 'ok'.
summary(Realm) when not is_binary(Realm) ->
    summary(wh_util:to_binary(Realm));
summary(Realm) ->
    R = wh_util:to_lower_binary(Realm),
    MatchSpec = [{#registration{realm = '$1'
                                ,account_realm = '$2'
                                ,_ = '_'
                               }
                  ,[{'orelse'
                     ,{'=:=', '$1', {'const', R}}
                     ,{'=:=', '$2', {'const', R}}
                    }
                   ]
                  ,['$_']
                 }],
    print_summary(ets:select(?MODULE, MatchSpec, 1)).

-spec details() -> 'ok'.
details() ->
    MatchSpec = [{#registration{_ = '_'}
                  ,[]
                  ,['$_']
                 }],
    print_details(ets:select(?MODULE, MatchSpec, 1)).

-spec details(text()) -> 'ok'.
details(User) when not is_binary(User) ->
    details(wh_util:to_binary(User));
details(User) ->
    case binary:split(User, <<"@">>) of
        [Username, Realm] -> details(Username, Realm);
        _Else ->
            Realm = wh_util:to_lower_binary(User),
            MatchSpec = [{#registration{realm = '$1'
                                        ,account_realm = '$2'
                                        ,_ = '_'
                                       }
                          ,[{'orelse'
                             ,{'=:=', '$1', {'const', Realm}}
                             ,{'=:=', '$2', {'const', Realm}}
                            }
                           ]
                          ,['$_']
                         }],
            print_details(ets:select(?MODULE, MatchSpec, 1))
    end.

-spec details(text(), text()) -> 'ok'.
details(Username, Realm) when not is_binary(Username) ->
    details(wh_util:to_binary(Username), Realm);
details(Username, Realm) when not is_binary(Realm) ->
    details(Username, wh_util:to_binary(Realm));
details(Username, Realm) ->
    Id =  registration_id(Username, Realm),
    MatchSpec = [{#registration{id = '$1', _ = '_'}
                  ,[{'=:=', '$1', {const, Id}}]
                  ,['$_']
                 }],
    print_details(ets:select(?MODULE, MatchSpec, 1)).

-spec flush() -> 'ok'.
flush() ->
    gen_server:cast(?MODULE, 'flush').

-spec flush(text()) -> 'ok'.
flush(Realm) when not is_binary(Realm)->
    flush(wh_util:to_binary(Realm));
flush(Realm) ->
    case binary:split(Realm, <<"@">>) of
        [Username, Realm] -> flush(Username, Realm);
        _Else -> gen_server:cast(?MODULE, {'flush', Realm})
    end.

-spec flush(text() | 'undefined', text()) -> 'ok'.
flush('undefined', Realm) ->
    flush(Realm);
flush(Username, Realm) when not is_binary(Realm) ->
    flush(Username, wh_util:to_binary(Realm));
flush(Username, Realm) when not is_binary(Username) ->
    flush(wh_util:to_binary(Username), Realm);
flush(Username, Realm) ->
    gen_server:cast(?MODULE, {'flush', Username, Realm}).

-spec count() -> non_neg_integer().
count() -> ets:info(?MODULE, 'size').

-spec handle_reg_success(atom(), wh_proplist()) -> 'ok'.
handle_reg_success(Node, Props) ->
    wh_util:put_callid(props:get_first_defined([<<"Call-ID">>, <<"call-id">>], Props, 'reg_success')),
    Req = lists:foldl(fun(<<"Contact">>=K, Acc) ->
                              [{K, get_fs_contact(Props)} | Acc];
                         (K, Acc) ->
                              case props:get_first_defined([wh_util:to_lower_binary(K), K], Props) of
                                  'undefined' -> Acc;
                                  V -> [{K, V} | Acc]
                              end
                      end
                      ,[{<<"Event-Timestamp">>, round(wh_util:current_tstamp())}
                        ,{<<"FreeSWITCH-Nodename">>, wh_util:to_binary(Node)}
                        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                       ]
                      ,wapi_registration:success_keys()),
    lager:debug("sending successful registration for ~s@~s"
                ,[props:get_value(<<"Username">>, Req), props:get_value(<<"Realm">>, Req)]
               ),
    wh_amqp_worker:cast(Req, fun wapi_registration:publish_success/1).

-spec get_fs_contact(wh_proplist()) -> ne_binary().
get_fs_contact(Props) ->
    Contact = props:get_first_defined([<<"Contact">>, <<"contact">>], Props),
    [User, AfterAt] = binary:split(Contact, <<"@">>), % only one @ allowed
    <<User/binary, "@", (wh_util:to_binary(mochiweb_util:unquote(AfterAt)))/binary>>.

%%%===================================================================
%%% gen_listener callbacks
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
init([]) ->
    process_flag('trap_exit', 'true'),
    lager:debug("starting new ecallmgr registrar"),
    _ = ets:new(?MODULE, ['set', 'protected', 'named_table', {'keypos', #registration.id}]),
    erlang:send_after(2 * ?MILLISECONDS_IN_SECOND, self(), 'expire'),

    gproc:reg({'p', 'l', ?REGISTER_SUCCESS_REG}),

    {'ok', #state{}}.

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
handle_call('registrar_age', _, #state{started=Started}=State) ->
    {'reply', wh_util:current_tstamp() - Started, State};
handle_call(_Msg, _From, State) ->
    {'noreply', State}.

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
handle_cast({'insert_registration', Registration}, State) ->
    _ = ets:insert(?MODULE, Registration#registration{initial='false'}),
    {'noreply', State};
handle_cast({'update_registration', {Username, Realm}=Id, Props}, State) ->
    lager:debug("updated registration ~s@~s", [Username, Realm]),
    _ = ets:update_element(?MODULE, Id, Props),
    {'noreply', State};
handle_cast({'delete_registration', #registration{id=Id}=Reg}, State) ->
    _ = wh_util:spawn(fun() -> maybe_send_deregister_notice(Reg) end),
    ets:delete(?MODULE, Id),
    {'noreply', State};
handle_cast('flush', State) ->
    _ = ets:delete_all_objects(?MODULE),
    {'noreply', State};
handle_cast({'flush', Realm}, State) ->
    R = wh_util:to_lower_binary(Realm),
    MatchSpec = [{#registration{realm = '$1'
                                ,account_realm = '$2'
                                ,_ = '_'
                               }
                  ,[{'orelse', {'=:=', '$1', {'const', R}}
                     ,{'=:=', '$2', {'const', R}}}
                   ]
                  ,['true']
                 }],
    NumberDeleted = ets:select_delete(?MODULE, MatchSpec),
    lager:debug("removed ~p expired registrations", [NumberDeleted]),
    ecallmgr_fs_nodes:flush(),
    {'noreply', State};
handle_cast({'flush', Username, Realm}, State) ->
    _ = ets:delete(?MODULE, registration_id(Username, Realm)),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{queue=Q}};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{queue=Q}=State) ->
    wapi_registration:publish_sync(wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)),
    {'noreply', State};
handle_cast(_Msg, State) ->
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
handle_info('expire', State) ->
    _ = expire_objects(),
    _ = erlang:send_after(2 * ?MILLISECONDS_IN_SECOND, self(), 'expire'),
    {'noreply', State};
handle_info(?REGISTER_SUCCESS_MSG(Node, Props), State) ->
    _ = wh_util:spawn(?MODULE, 'handle_reg_success', [Node, Props]),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {reply, Props}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), term()) -> 'ok'.
terminate(_Reason, _) ->
    lager:debug("ecallmgr registrar ~p termination", [_Reason]).

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
-spec maybe_fetch_registration(ne_binary(), ne_binary()) ->
                           {'ok', ne_binary()} |
                           {'error', 'not_found'}.
maybe_fetch_registration(Username, Realm) ->
    case oldest_registrar() of
        'true' -> {'error', 'not_found'};
        'false' -> fetch_registration(Username, Realm)
    end.

-spec fetch_registration(ne_binary(), ne_binary()) ->
                                {'ok', ne_binary()} |
                                {'error', 'not_found'}.
fetch_registration(Username, Realm) ->
    Reg = [{<<"Username">>, Username}
           ,{<<"Realm">>, Realm}
           ,{<<"Fields">>, []} % will fetch all fields
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case query_for_registration(Reg) of
        {'ok', JObjs} ->
            case [JObj
                  || JObj <- JObjs,
                     wapi_registration:query_resp_v(JObj)
                 ]
            of
                [Registration|_] ->
                    lager:info("fetched user ~s@~s registration", [Username, Realm]),
                    {'ok', Registration};
                _Else ->
                    lager:info("registration query for user ~s@~s returned an empty result", [Username, Realm]),
                    {'error', 'not_found'}
            end;
        _Else ->
            lager:info("registration query for user ~s@~s failed: ~p", [Username, Realm, _Else]),
            {'error', 'not_found'}
    end.

-spec maybe_fetch_contact(ne_binary(), ne_binary()) ->
                           {'ok', ne_binary()} |
                           {'error', 'not_found'}.
maybe_fetch_contact(Username, Realm) ->
    case oldest_registrar() of
        'true' -> {'error', 'not_found'};
        'false' -> fetch_contact(Username, Realm)
    end.

-spec fetch_contact(ne_binary(), ne_binary()) ->
                           {'ok', ne_binary()} |
                           {'error', 'not_found'}.
fetch_contact(Username, Realm) ->
    Reg = [{<<"Username">>, Username}
           ,{<<"Realm">>, Realm}
           ,{<<"Fields">>, [<<"Contact">>, <<"Bridge-RURI">>]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case query_for_registration(Reg) of
        {'ok', JObjs} ->
            process_query_resp_contacts(Username, Realm, JObjs);
        _Else ->
            lager:info("contact query for user ~s@~s failed: ~p", [Username, Realm, _Else]),
            {'error', 'not_found'}
    end.

-spec process_query_resp_contacts(ne_binary(), ne_binary(), wh_json:objects()) ->
                                         {'ok', ne_binary()} |
                                         {'error', 'not_found'}.
process_query_resp_contacts(Username, Realm, JObjs) ->
    case find_contacts_in_query_resp(JObjs) of
        [Contact|_] ->
            lager:info("fetched user ~s@~s contact ~s", [Username, Realm, Contact]),
            {'ok', Contact};
        _Else ->
            lager:info("contact query for user ~s@~s returned an empty result", [Username, Realm]),
            {'error', 'not_found'}
    end.

-spec find_contacts_in_query_resp(wh_json:objects()) -> ne_binaries().
find_contacts_in_query_resp(JObjs) ->
    [Contact
     || JObj <- JObjs,
        wapi_registration:query_resp_v(JObj),
        (Contact = find_contact_in_query_resp(JObj))
            =/= 'undefined'
    ].

-spec find_contact_in_query_resp(wh_json:object()) -> api_binary().
find_contact_in_query_resp(JObj) ->
    wh_json:get_first_defined([[<<"Fields">>, 1, <<"Bridge-RURI">>]
                               ,[<<"Fields">>, 1, <<"Contact">>]
                              ]
                              ,JObj
                             ).

-spec query_for_registration(api_terms()) ->
                                    {'ok', wh_json:objects()} |
                                    {'error', any()}.
query_for_registration(Reg) ->
    wh_amqp_worker:call_collect(Reg
                                ,fun wapi_registration:publish_query_req/1
                                ,{'ecallmgr', fun wapi_registration:query_resp_v/1, 'true'}
                                ,2 * ?MILLISECONDS_IN_SECOND
                               ).

-spec maybe_fetch_original_contact(ne_binary(), ne_binary()) ->
                           {'ok', ne_binary()} |
                           {'error', 'not_found'}.
maybe_fetch_original_contact(Username, Realm) ->
    case oldest_registrar() of
        'true' -> {'error', 'not_found'};
        'false' -> fetch_original_contact(Username, Realm)
    end.

-spec fetch_original_contact(ne_binary(), ne_binary()) ->
                                    {'ok', ne_binary()} |
                                    {'error', 'not_found'}.
fetch_original_contact(Username, Realm) ->
    Reg = [{<<"Username">>, Username}
           ,{<<"Realm">>, Realm}
           ,{<<"Fields">>, [<<"Original-Contact">>]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_amqp_worker:call_collect(Reg
                                     ,fun wapi_registration:publish_query_req/1
                                     ,{'ecallmgr', fun wapi_registration:query_resp_v/1, 'true'}
                                     ,2 * ?MILLISECONDS_IN_SECOND
                                    )
    of
        {'ok', JObjs} ->
            case [Contact
                  || JObj <- JObjs
                         ,wapi_registration:query_resp_v(JObj)
                         ,(Contact = wh_json:get_value([<<"Fields">>, 1, <<"Original-Contact">>]
                                                       ,JObj)) =/= 'undefined'
                 ]
            of
                [Contact|_] ->
                    lager:info("fetched user ~s@~s original contact ~s", [Username, Realm, Contact]),
                    {'ok', Contact};
                _Else ->
                    lager:info("original contact query for user ~s@~s returned an empty result", [Username, Realm]),
                    {'error', 'not_found'}
            end;
        _Else ->
            lager:info("original contact query for user ~s@~s failed: ~p", [Username, Realm, _Else]),
            {'error', 'not_found'}
    end.

-spec expire_objects() -> 'ok'.
expire_objects() ->
    Now = wh_util:current_tstamp(),
    MatchSpec = [{#registration{expires = '$1'
                                ,last_registration = '$2'
                                , _ = '_'}
                  ,[{'>', {const, Now}, {'+', '$1', '$2'}}]
                  ,['$_']
                 }],
    expire_object(ets:select(?MODULE, MatchSpec, 1)).

-spec expire_object(_) -> 'ok'.
expire_object('$end_of_table') -> 'ok';
expire_object({[#registration{id=Id}=Reg], Continuation}) ->
    _ = wh_util:spawn(fun() -> maybe_send_deregister_notice(Reg) end),
    _ = ets:delete(?MODULE, Id),
    expire_object(ets:select(Continuation)).

-spec maybe_resp_to_query(wh_json:object()) -> 'ok'.
maybe_resp_to_query(JObj) ->
    case wh_json:get_value(<<"Node">>, JObj)
        =:= wh_util:to_binary(node())
    of
        'false' -> resp_to_query(JObj);
        'true' ->
            Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    ,{<<"Registrar-Age">>, gen_server:call(?MODULE, 'registrar_age')}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_registration:publish_query_err(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

-spec build_query_spec(wh_json:object(), boolean()) -> ets:match_spec().
build_query_spec(JObj, CountOnly) ->
    {SelectFormat, QueryFormat} =
        case wh_util:to_lower_binary(wh_json:get_value(<<"Realm">>, JObj)) of
            <<"all">> -> {#registration{_='_'}, {'=:=', 'undefined', 'undefined'}};
            Realm ->
                case wh_json:get_value(<<"Username">>, JObj) of
                    'undefined' ->
                        {#registration{realm = '$1'
                                       ,account_realm = '$2'
                                       ,_ = '_'
                                      }
                         ,{'orelse', {'=:=', '$1', {'const', Realm}}
                           ,{'=:=', '$2', {'const', Realm}}}
                        };
                    Username ->
                        Id = registration_id(Username, Realm),
                        {#registration{id = '$1', _ = '_'}
                         ,{'=:=', '$1', {'const', Id}}
                        }
                end
        end,
    ResultFormat = case CountOnly of
                       'true' -> 'true';
                       'false' -> '$_'
                   end,

    [{SelectFormat
      ,[QueryFormat]
      ,[ResultFormat]
     }].

-spec resp_to_query(wh_json:object()) -> 'ok'.
resp_to_query(JObj) ->
    Fields = wh_json:get_value(<<"Fields">>, JObj, []),
    CountOnly = wh_json:is_true(<<"Count-Only">>, JObj, 'false'),

    SelectFun = case CountOnly of
                    'true' -> fun ets:select_count/2;
                    'false' -> fun ets:select/2
                end,
    MatchSpec = build_query_spec(JObj, CountOnly),

    case SelectFun(?MODULE, MatchSpec) of
        [] ->
            Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    ,{<<"Registrar-Age">>, gen_server:call(?MODULE, 'registrar_age')}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_registration:publish_query_err(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
        [_|_]=Registrations ->
            Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    ,{<<"Registrar-Age">>, gen_server:call(?MODULE, 'registrar_age')}
                    ,{<<"Fields">>, [filter(Fields, wh_json:from_list(to_props(Registration)))
                                            || Registration <- Registrations
                                    ]}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_registration:publish_query_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
        Count when is_integer(Count) ->
            Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    ,{<<"Registrar-Age">>, gen_server:call(?MODULE, 'registrar_age')}
                    ,{<<"Fields">>, []}
                    ,{<<"Count">>, Count}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_registration:publish_query_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

-spec registration_id(ne_binary(), ne_binary()) -> {ne_binary(), ne_binary()}.
registration_id(Username, Realm) ->
    {wh_util:to_lower_binary(Username), wh_util:to_lower_binary(Realm)}.

-spec create_registration(wh_json:object()) -> registration().
create_registration(JObj) ->
    Username = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    Proxy = wh_json:get_value(<<"Proxy-Path">>, JObj),
    #registration{initial=Initial}=Reg = existing_or_new_registration(Username, Realm),
    OriginalContact = wh_json:get_first_defined([<<"Original-Contact">>, <<"Contact">>], JObj),

    maybe_add_ccvs(wh_json:get_value(<<"Custom-Channel-Vars">>, JObj)
                   ,Reg#registration{username=Username
                                     ,realm=Realm
                                     ,network_port=wh_json:get_value(<<"Network-Port">>, JObj)
                                     ,network_ip=wh_json:get_value(<<"Network-IP">>, JObj)
                                     ,to_host=wh_json:get_value(<<"To-Host">>, JObj, ?DEFAULT_REALM)
                                     ,to_user=wh_json:get_value(<<"To-User">>, JObj, <<"nouser">>)
                                     ,from_host=wh_json:get_value(<<"From-Host">>, JObj, ?DEFAULT_REALM)
                                     ,from_user=wh_json:get_value(<<"From-User">>, JObj, <<"nouser">>)
                                     ,call_id=wh_json:get_value(<<"Call-ID">>, JObj)
                                     ,user_agent=wh_json:get_value(<<"User-Agent">>, JObj)
                                     ,expires=ecallmgr_util:maybe_add_expires_deviation(
                                                wh_json:get_integer_value(<<"Expires">>, JObj, ?EXPIRES_MISSING_VALUE)
                                               )
                                     ,contact=fix_contact(OriginalContact)
                                     ,original_contact=OriginalContact
                                     ,last_registration=wh_util:current_tstamp()
                                     ,registrar_node=wh_json:get_first_defined([<<"Registrar-Node">>
                                                                                ,<<"FreeSWITCH-Nodename">>
                                                                                ,<<"Node">>
                                                                               ], JObj)
                                     ,registrar_hostname=wh_json:get_value(<<"Hostname">>, JObj)
                                     ,initial = wh_json:is_true(<<"First-Registration">>, JObj, Initial)
                                     ,proxy=Proxy
                                     ,bridge_uri=bridge_uri(OriginalContact, Proxy, Username, Realm)
                                    }
                  ).

-spec maybe_add_ccvs(api_object(), registration()) -> registration().
maybe_add_ccvs('undefined', Reg) -> Reg;
maybe_add_ccvs(CCVs, Reg) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, CCVs),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Reg#registration{account_id = AccountId
                     ,account_db = AccountDb
                     ,authorizing_id = wh_json:get_value(<<"Authorizing-ID">>, CCVs)
                     ,authorizing_type = wh_json:get_value(<<"Authorizing-Type">>, CCVs)
                     ,owner_id = wh_json:get_value(<<"Owner-ID">>, CCVs)
                     ,account_realm = wh_json:get_value(<<"Account-Realm">>, CCVs)
                     ,account_name = wh_json:get_value(<<"Account-Name">>, CCVs)
                     ,suppress_unregister = wh_json:is_true(<<"Suppress-Unregister-Notifications">>, CCVs)
                     ,register_overwrite_notify = wh_json:is_true(<<"Register-Overwrite-Notify">>, CCVs)
                    }.

-spec fix_contact(api_binary()) -> api_binary().
fix_contact('undefined') -> 'undefined';
fix_contact(Contact) ->
    binary:replace(Contact
                   ,[<<"<">>, <<">">>]
                   ,<<>>
                   ,['global']
                  ).

-spec bridge_uri(api_binary(), api_binary(), binary(), binary()) -> api_binary().
bridge_uri(_Contact, 'undefined', _, _) -> 'undefined';
bridge_uri('undefined', _Proxy, _, _) -> 'undefined';
bridge_uri(Contact, Proxy, Username, Realm) ->
    [#uri{}=UriContact] = nksip_parse_uri:uris(Contact),
    [#uri{}=UriProxy] = nksip_parse_uri:uris(Proxy),
    Scheme = UriContact#uri.scheme,
    Transport = props:get_value(<<"transport">>, UriContact#uri.opts),

    BridgeUri = #uri{scheme=Scheme
                     ,user=Username
                     ,domain=Realm
                     ,opts=props:filter_undefined(
                             [{<<"transport">>, Transport}
                              ,{<<"fs_path">>, nksip_unparse:ruri(UriProxy)}
                             ])
                    },
    nksip_unparse:ruri(BridgeUri).

-spec existing_or_new_registration(ne_binary(), ne_binary()) -> registration().
existing_or_new_registration(Username, Realm) ->
    case ets:lookup(?MODULE, registration_id(Username, Realm)) of
        [#registration{contact=Contact}=Reg] ->
            Reg#registration{previous_contact=Contact};
        _Else ->
            lager:debug("new registration ~s@~s", [Username, Realm]),
            #registration{id=registration_id(Username, Realm)
                          ,initial_registration=wh_util:current_tstamp()
                         }
    end.

-spec maybe_registration_notify(registration()) -> 'ok'.
maybe_registration_notify(#registration{register_overwrite_notify = 'false'}) -> 'ok';
maybe_registration_notify(#registration{register_overwrite_notify = 'true'
                                        ,contact = Contact
                                        ,previous_contact = Contact
                                        }) -> 'ok';
maybe_registration_notify(#registration{register_overwrite_notify = 'true'
                                        ,previous_contact = 'undefined'
                                        }) -> 'ok';
maybe_registration_notify(#registration{register_overwrite_notify = 'true'}=Reg) ->
    registration_notify(Reg).

registration_notify(#registration{previous_contact=PrevContact
                                 ,contact=Contact
                                 ,username=Username
                                 ,realm=Realm}) ->
    Props = props:filter_undefined(
              [{<<"Previous-Contact">>, PrevContact}
               ,{<<"Contact">>, Contact}
               ,{<<"Username">>, Username}
               ,{<<"Realm">>, Realm}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ]),
    wapi_presence:publish_register_overwrite(Props).

-spec initial_registration(registration()) -> 'ok'.
initial_registration(#registration{account_id='undefined'}=Reg) ->
    Routines = [fun maybe_query_authn/1
                ,fun update_cache/1
                ,fun maybe_send_register_notice/1
               ],
    initial_registration(Reg, Routines);
initial_registration(#registration{}=Reg) ->
    Routines = [fun maybe_send_register_notice/1],
    initial_registration(Reg, Routines).

-spec initial_registration(registration(), list()) -> 'ok'.
initial_registration(#registration{}=Reg, Routines) ->
    _ = lists:foldl(fun(F, R) -> F(R) end, Reg, Routines),
    'ok'.

-spec maybe_query_authn(registration()) -> registration().
maybe_query_authn(#registration{username=Username
                                ,realm=Realm
                               }=Reg) ->
    case wh_cache:peek_local(?ECALLMGR_AUTH_CACHE, ?CREDS_KEY(Realm, Username)) of
        {'error', 'not_found'} -> query_authn(Reg);
        {'ok', JObj} ->
            CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
            AccountId = wh_json:get_value(<<"Account-ID">>, CCVs),
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            Reg#registration{account_id = AccountId
                             ,account_db = AccountDb
                             ,authorizing_id = wh_json:get_value(<<"Authorizing-ID">>, CCVs)
                             ,authorizing_type = wh_json:get_value(<<"Authorizing-Type">>, CCVs)
                             ,owner_id = wh_json:get_value(<<"Owner-ID">>, CCVs)
                             ,account_realm = wh_json:get_value(<<"Account-Realm">>, CCVs)
                             ,account_name = wh_json:get_value(<<"Account-Name">>, CCVs)
                             ,suppress_unregister = wh_json:is_true(<<"Suppress-Unregister-Notifications">>, JObj)
                             ,register_overwrite_notify = wh_json:is_true(<<"Register-Overwrite-Notify">>, JObj)
                            }
    end.

-spec query_authn(registration()) -> registration().
query_authn(#registration{username=Username
                          ,realm=Realm
                          ,to_user=ToUser
                          ,to_host=ToHost
                          ,from_user=FromUser
                          ,from_host=FromHost
                          ,network_ip=NetworkIP
                          ,network_port=NetworkPort
                          ,registrar_node=Node
                          ,call_id=CallId
                         }=Reg) ->
    lager:debug("looking up credentials of ~s@~s", [Username, Realm]),
    Req = [{<<"To">>, <<ToUser/binary, "@", ToHost/binary>>}
           ,{<<"From">>, <<FromUser/binary, "@", FromHost/binary>>}
           ,{<<"Orig-IP">>, NetworkIP}
           ,{<<"Orig-Port">>, NetworkPort}
           ,{<<"Auth-User">>, Username}
           ,{<<"Auth-Realm">>, Realm}
           ,{<<"Media-Server">>, wh_util:to_binary(Node)}
           ,{<<"Method">>, <<"REGISTER">>}
           ,{<<"Call-ID">>, CallId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = wh_amqp_worker:call(props:filter_undefined(Req)
                                  ,fun wapi_authn:publish_req/1
                                  ,fun wapi_authn:resp_v/1
                                 ),
    case ReqResp of
        {'error', _} -> Reg;
        {'ok', JObj} ->
            lager:debug("received authn information"),
            CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
            AccountId = wh_json:get_value(<<"Account-ID">>, CCVs),
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            AuthorizingId = wh_json:get_value(<<"Authorizing-ID">>, CCVs),
            OwnerIdProp = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Owner-ID">>], JObj) of
                              'undefined' -> [];
                              OwnerId -> [{'db', AccountDb, OwnerId}]
                          end,
            CacheProps = [{'origin', [{'db', AccountDb, AuthorizingId}
                                      ,{'db', AccountDb, AccountId}
                                      | OwnerIdProp
                                     ]}
                         ],
            wh_cache:store_local(?ECALLMGR_AUTH_CACHE
                                 ,?CREDS_KEY(Realm, Username)
                                 ,JObj
                                 ,CacheProps
                                ),
            Reg#registration{account_id = AccountId
                             ,account_db = AccountDb
                             ,authorizing_id = AuthorizingId
                             ,authorizing_type = wh_json:get_value(<<"Authorizing-Type">>, CCVs)
                             ,owner_id = wh_json:get_value(<<"Owner-ID">>, CCVs)
                             ,suppress_unregister = wh_json:is_true(<<"Suppress-Unregister-Notifications">>, JObj)
                             ,register_overwrite_notify = wh_json:is_true(<<"Register-Overwrite-Notify">>, JObj)
                             ,account_realm = wh_json:get_value(<<"Account-Realm">>, CCVs)
                             ,account_name = wh_json:get_value(<<"Account-Name">>, CCVs)
                            }
    end.

-spec update_cache(registration()) -> registration().
update_cache(#registration{authorizing_id=AuthorizingId
                           ,account_id=AccountId
                           ,authorizing_type=AuthorizingType
                           ,account_db=AccountDb
                           ,suppress_unregister=SuppressUnregister
                           ,register_overwrite_notify=RegisterOverwrite
                           ,owner_id=OwnerId
                           ,id=Id
                           ,account_realm=AccountRealm
                           ,account_name=AccountName
                          }=Reg) ->
    Props = [{#registration.account_id, AccountId}
             ,{#registration.account_db, AccountDb}
             ,{#registration.authorizing_id, AuthorizingId}
             ,{#registration.authorizing_type, AuthorizingType}
             ,{#registration.owner_id, OwnerId}
             ,{#registration.suppress_unregister, SuppressUnregister}
             ,{#registration.register_overwrite_notify, RegisterOverwrite}
             ,{#registration.account_realm, AccountRealm}
             ,{#registration.account_name, AccountName}
            ],
    gen_server:cast(?MODULE, {'update_registration', Id, Props}),
    Reg.

-spec maybe_send_register_notice(registration()) -> 'ok'.
maybe_send_register_notice(#registration{username=Username
                                         ,realm=Realm
                                        }=Reg) ->
    case oldest_registrar() of
        'false' -> 'ok';
        'true' ->
            lager:debug("sending register notice for ~s@~s", [Username, Realm]),
            send_register_notice(Reg)
    end.

-spec send_register_notice(registration()) -> 'ok'.
send_register_notice(Reg) ->
    Props = to_props(Reg)
        ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION),
    wapi_notifications:publish_register(Props).

-spec maybe_send_deregister_notice(registration()) -> 'ok'.
maybe_send_deregister_notice(#registration{username=Username
                                           ,realm=Realm
                                           ,suppress_unregister='true'
                                           ,call_id=CallId
                                          }) ->
    wh_util:put_callid(CallId),
    lager:debug("registration ~s@~s expired", [Username, Realm]);
maybe_send_deregister_notice(#registration{username=Username
                                           ,realm=Realm
                                           ,call_id=CallId
                                          }=Reg) ->
    wh_util:put_callid(CallId),
    case oldest_registrar() of
        'false' -> 'ok';
        'true' ->
            lager:debug("sending deregister notice for ~s@~s", [Username, Realm]),
            send_deregister_notice(Reg)
    end.

-spec send_deregister_notice(registration()) -> 'ok'.
send_deregister_notice(Reg) ->
    Props = to_props(Reg)
        ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION),
    wh_amqp_worker:cast(Props, fun wapi_notifications:publish_deregister/1).

-spec to_props(registration()) -> wh_proplist().
to_props(Reg) ->
    [{<<"Username">>, Reg#registration.username}
     ,{<<"Realm">>, Reg#registration.realm}
     ,{<<"User-Agent">>, Reg#registration.user_agent}
     ,{<<"Call-ID">>, Reg#registration.call_id}
     ,{<<"From-User">>, Reg#registration.from_user}
     ,{<<"From-Host">>, Reg#registration.from_host}
     ,{<<"To-User">>, Reg#registration.to_user}
     ,{<<"To-Host">>, Reg#registration.to_host}
     ,{<<"Network-IP">>, Reg#registration.network_ip}
     ,{<<"Network-Port">>, Reg#registration.network_port}
     ,{<<"Event-Timestamp">>, Reg#registration.last_registration}
     ,{<<"Contact">>, Reg#registration.contact}
     ,{<<"Original-Contact">>, Reg#registration.original_contact}
     ,{<<"Expires">>, Reg#registration.expires}
     ,{<<"Account-ID">>, Reg#registration.account_id}
     ,{<<"Account-DB">>, Reg#registration.account_db}
     ,{<<"Account-Realm">>, Reg#registration.account_realm}
     ,{<<"Account-Name">>, Reg#registration.account_name}
     ,{<<"Authorizing-ID">>, Reg#registration.authorizing_id}
     ,{<<"Authorizing-Type">>, Reg#registration.authorizing_type}
     ,{<<"Suppress-Unregister-Notify">>, Reg#registration.suppress_unregister}
     ,{<<"Register-Overwrite-Notify">>, Reg#registration.register_overwrite_notify}
     ,{<<"Owner-ID">>, Reg#registration.owner_id}
     ,{<<"Registrar-Node">>, Reg#registration.registrar_node}
     ,{<<"Registrar-Hostname">>, Reg#registration.registrar_hostname}
     ,{<<"Bridge-RURI">>, Reg#registration.bridge_uri}
    ].

-spec filter(wh_json:keys(), wh_json:object()) -> wh_json:object().
filter([], JObj) -> JObj;
filter(Fields, JObj) ->
    wh_json:from_list(lists:foldl(fun(F, Acc) ->
                                          [{F, wh_json:get_value(F, JObj)} | Acc]
                                  end, [], Fields)).

-spec oldest_registrar() -> boolean().
oldest_registrar() ->
    wh_nodes:whapp_zone_count(?APP_NAME) =:= 1 andalso
    wh_nodes:whapp_oldest_node(?APP_NAME, 'true') =:= node().

-type ets_continuation() :: '$end_of_table' |
                            {registrations(), term()}.

-spec print_summary(ets_continuation()) -> 'ok'.
-spec print_summary(ets_continuation(), non_neg_integer()) -> 'ok'.
print_summary('$end_of_table') ->
    io:format("No registrations found!~n", []);
print_summary(Match) ->
    io:format("+-----------------------------------------------+------------------------+------------------------+----------------------------------+------+~n"),
    io:format("| User                                          | Contact                | Path                   | Call-ID                          |  Exp |~n"),
    io:format("+===============================================+========================+========================+==================================+======+~n"),
    print_summary(Match, 0).

print_summary('$end_of_table', Count) ->
    io:format("+-----------------------------------------------+------------------------+------------------------+----------------------------------+------+~n"),
    io:format("Found ~p registrations~n", [Count]);
print_summary({[#registration{username=Username
                              ,realm=Realm
                              ,contact=Contact
                              ,expires=Expires
                              ,last_registration=LastRegistration
                              ,call_id=CallId
                             }
               ], Continuation}
              ,Count) ->
    User = <<Username/binary, "@", Realm/binary>>,
    Remaining = (LastRegistration + Expires) - wh_util:current_tstamp(),
    Props = breakup_contact(Contact),
    Hostport = props:get_first_defined(['received', 'hostport'], Props),
    _ = case props:get_value('fs_path', Props) of
            'undefined' ->
                io:format("| ~-45s | ~-22s | ~-22s | ~-32s | ~-4B |~n"
                          ,[User, Hostport, <<>>, CallId, Remaining]);
            Path ->
                io:format("| ~-45s | ~-22s | ~-22s | ~-32s | ~-4B |~n"
                         ,[User, Hostport, Path, CallId, Remaining])
        end,
    print_summary(ets:select(Continuation), Count + 1).

-spec print_details(ets_continuation()) -> 'ok'.
-spec print_details(ets_continuation(), non_neg_integer()) -> 'ok'.
print_details('$end_of_table') ->
    io:format("No registrations found!~n", []);
print_details(Match) ->
    print_details(Match, 0).

print_details('$end_of_table', Count) ->
    io:format("~nFound ~p registrations~n", [Count]);
print_details({[#registration{}=Reg], Continuation}, Count) ->
    io:format("~n"),
    _ = [print_property(K, V, Reg)
         || {K, V} <- to_props(Reg)
        ],
    print_details(ets:select(Continuation), Count + 1).

print_property(<<"Expires">> =Key, Value, #registration{expires=Expires
                                                        ,last_registration=LastRegistration
                                                       }) ->
    Remaining = (LastRegistration + Expires) - wh_util:current_tstamp(),
    io:format("~-19s: ~b/~s~n", [Key, Remaining, wh_util:to_binary(Value)]);
print_property(Key, Value, _) ->
    io:format("~-19s: ~s~n", [Key, wh_util:to_binary(Value)]).

-type contact_param() :: {'uri', ne_binary()} |
                         {'hostport', ne_binary()} |
                         {'transport', ne_binary()} |
                         {'fs_path', ne_binary()} |
                         {'received', ne_binary()}.
-type contact_params() :: [contact_param(),...] | [].

-spec breakup_contact(text()) -> contact_params().
breakup_contact(Contact) when is_binary(Contact) ->
    C = binary:replace(Contact, [<<$'>>, <<$<>>, <<$>>>, <<"sip:">>], <<>>, ['global']),
    [Uri|Parameters] = binary:split(C, <<";">>, ['global']),
    Hostport = get_contact_hostport(Uri),
    find_contact_parameters(Parameters, [{'uri', Uri}, {'hostport', Hostport}]);
breakup_contact(Contact) ->
    breakup_contact(wh_util:to_binary(Contact)).

-spec find_contact_parameters(ne_binaries(), wh_proplist()) -> wh_proplist().
find_contact_parameters([], Props) -> Props;
find_contact_parameters([<<"transport=", Transport/binary>>|Parameters], Props) ->
    find_contact_parameters(Parameters, [{'transport', wh_util:to_lower_binary(Transport)}|Props]);
find_contact_parameters([<<"fs_path=", FsPath/binary>>|Parameters], Props) ->
    find_contact_parameters(Parameters, [{'fs_path', FsPath}|Props]);
find_contact_parameters([<<"received=", Received/binary>>|Parameters], Props) ->
    find_contact_parameters(Parameters, [{'received', Received}|Props]);
find_contact_parameters([_|Parameters], Props) ->
     find_contact_parameters(Parameters, Props).

-spec get_contact_hostport(ne_binary()) -> ne_binary().
get_contact_hostport(Uri) ->
    case binary:split(Uri, <<"@">>) of
        [_, Hostport] -> Hostport;
        _Else -> Uri
    end.
