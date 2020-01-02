%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Listener for reg_success, and reg_query AMQP requests
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_registrar).
-behaviour(gen_listener).

-export([start_link/0]).
-export([handle_reg_success/2
        ,handle_reg_query/2
        ,handle_reg_flush/2
        ,handle_fs_reg/2
        ]).
-export([lookup_contact/2
        ,lookup_original_contact/2
        ,lookup_registration/2
        ,lookup_proxy_path/2
        ,get_registration/2
        ]).
-export([summary/0, summary/1
        ,details/0, details/1, details/2
        ,flush/0, flush/1, flush/2
        ,sync/0
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
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{{?MODULE, 'handle_reg_query'}
                     ,[{<<"directory">>, <<"reg_query">>}]
                     }
                    ,{{?MODULE, 'handle_reg_success'}
                     ,[{<<"directory">>, <<"reg_success">>}]
                     }
                    ,{{?MODULE, 'handle_reg_flush'}
                     ,[{<<"directory">>, <<"reg_flush">>}]
                     }
                    ]).
-define(BINDINGS, [{'registration', [{'restrict_to',
                                      ['reg_query'
                                      ,'reg_flush'
                                      ,'reg_success'
                                      ]
                                     }
                                    ,'federate'
                                    ]}
                  ,{'self', []}
                  ]).
-define(REG_QUEUE_NAME, <<>>).
-define(REG_QUEUE_OPTIONS, []).
-define(REG_CONSUME_OPTIONS, []).
-define(EXPIRES_MISSING_VALUE, 0).

-record(state, {started = kz_time:now_s()
               ,queue :: kz_term:api_binary()
               }).
-type state() :: #state{}.

-record(registration, {id :: {kz_term:ne_binary(), kz_term:ne_binary()} | '_' | '$1'
                      ,username :: kz_term:api_ne_binary() | '_'
                      ,realm :: kz_term:api_ne_binary() | '_' | '$1'
                      ,network_port :: kz_term:api_ne_binary() | '_'
                      ,network_ip :: kz_term:api_ne_binary() | '_'
                      ,to_host :: kz_term:api_ne_binary() | '_'
                      ,to_user = <<"nouser">> :: kz_term:ne_binary() | '_'
                      ,from_host :: kz_term:api_ne_binary() | '_'
                      ,from_user = <<"nouser">> :: kz_term:ne_binary() | '_'
                      ,call_id :: kz_term:api_ne_binary() | '_'
                      ,user_agent :: kz_term:api_ne_binary() | '_'
                      ,expires = ?EXPIRES_MISSING_VALUE :: non_neg_integer() | '_' | '$1'
                      ,contact :: kz_term:api_ne_binary() | '_'
                      ,previous_contact :: kz_term:api_binary() | '_'
                      ,original_contact :: kz_term:api_ne_binary() | '_'
                      ,last_registration = kz_time:now_s() :: kz_time:gregorian_seconds() | '_' | '$2'
                      ,initial_registration = kz_time:now_s() :: kz_time:gregorian_seconds() | '_'
                      ,registrar_node :: kz_term:api_ne_binary() | '_'
                      ,registrar_hostname :: kz_term:api_ne_binary() | '_'
                      ,suppress_unregister = 'true' :: boolean() | '_'
                      ,register_overwrite_notify = 'false' :: boolean() | '_'
                      ,account_db :: kz_term:api_binary() | '_'
                      ,account_id :: kz_term:api_binary() | '_'
                      ,authorizing_id :: kz_term:api_binary() | '_'
                      ,authorizing_type :: kz_term:api_binary() | '_'
                      ,owner_id :: kz_term:api_binary() | '_'
                      ,presence_id :: kz_term:api_binary() | '_'
                      ,initial = 'true' :: boolean() | '_'
                      ,account_realm :: kz_term:api_binary() | '_' | '$2'
                      ,account_name :: kz_term:api_binary() | '_'
                      ,proxy :: kz_term:api_binary() | '_'
                      ,proxy_ip :: kz_term:api_binary() | '_'
                      ,proxy_port :: kz_term:api_integer() | '_'
                      ,proxy_proto :: kz_term:api_binary() | '_'
                      ,bridge_uri :: kz_term:api_binary() | '_'
                      ,source_ip :: kz_term:api_binary() | '_'
                      ,source_port :: kz_term:api_binary() | '_'
                      }).

-type registration() :: #registration{}.
-type registrations() :: [registration()].

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                           ,?MODULE
                           ,[{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?REG_QUEUE_NAME}
                            ,{'queue_options', ?REG_QUEUE_OPTIONS}
                            ,{'consume_options', ?REG_CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).

-spec handle_reg_success(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_reg_success(JObj, _Props) ->
    'true' = kapi_registration:success_v(JObj),
    _ = kz_log:put_callid(JObj),
    Registration = create_registration(JObj),
    insert_registration(Registration).

-spec handle_reg_query(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_reg_query(JObj, Props) ->
    'true' = kapi_registration:query_req_v(JObj),
    _ = kz_log:put_callid(JObj),
    maybe_resp_to_query(JObj, props:get_value('registrar_age', Props)).

-spec handle_reg_flush(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_reg_flush(JObj, _Props) ->
    'true' = kapi_registration:flush_v(JObj),
    Username = kz_json:get_value(<<"Username">>, JObj),
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    lager:debug("recv req to flush ~s @ ~s"
               ,[Username, Realm]
               ),
    flush(Username, Realm).

-spec handle_fs_reg(atom(), kzd_freeswitch:data()) -> 'ok'.
handle_fs_reg(Node, FSJObj) ->
    kz_log:put_callid(kzd_freeswitch:call_id(FSJObj)),

    Req = lists:foldl(fun(<<"Contact">>=K, Acc) ->
                              [{K, get_fs_contact(FSJObj)} | Acc];
                         (K, Acc) ->
                              case kz_json:get_first_defined([kz_term:to_lower_binary(K), K], FSJObj) of
                                  'undefined' -> Acc;
                                  V -> [{K, V} | Acc]
                              end
                      end
                     ,[{<<"Event-Timestamp">>, round(kz_time:now_s())}
                      ,{<<"FreeSWITCH-Nodename">>, kz_term:to_binary(Node)}
                       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ]
                     ,kapi_registration:success_keys()
                     ),
    lager:debug("sending successful registration for ~s@~s"
               ,[props:get_value(<<"Username">>, Req), props:get_value(<<"Realm">>, Req)]
               ),
    kz_amqp_worker:cast(Req, fun kapi_registration:publish_success/1).

-spec lookup_proxy_path(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:api_ne_binary(), kz_term:proplist()} |
          {'error', 'not_found'}.
lookup_proxy_path(<<>>, _Username) -> {'error', 'not_found'};
lookup_proxy_path(_Realm, <<>>) -> {'error', 'not_found'};
lookup_proxy_path(<<_/binary>> = Realm, <<_/binary>> = Username) ->
    MatchSpec = #registration{account_id = Realm
                             ,authorizing_id = Username
                             ,_ = '_'
                             },

    case ets:match_object(?MODULE, MatchSpec) of
        [] ->
            {'ok', 'undefined', []};
        [#registration{proxy=ProxyPath}=Reg] ->
            {'ok', ProxyPath, contact_vars(to_props(Reg))}
    end.

-spec lookup_contact(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary(), kz_term:proplist()} |
          {'error', 'not_found'}.
lookup_contact(<<>>, _Username) -> {'error', 'not_found'};
lookup_contact(_Realm, <<>>) -> {'error', 'not_found'};
lookup_contact(<<_/binary>> = Realm, <<_/binary>> = Username) ->
    case get_registration(Realm, Username) of
        'undefined' -> fetch_contact(Username, Realm);
        #registration{contact=Contact
                     ,bridge_uri='undefined'
                     }=Reg ->
            lager:info("found user ~s@~s contact ~s"
                      ,[Username, Realm, Contact]
                      ),
            {'ok', Contact, contact_vars(to_props(Reg))};
        #registration{bridge_uri=Contact}=Reg ->
            lager:info("found user ~s@~s bridge uri  ~s"
                      ,[Username, Realm, Contact]
                      ),
            {'ok', Contact, contact_vars(to_props(Reg))}
    end.

-spec contact_vars(kz_term:proplist()) -> kz_term:proplist().
contact_vars(Props) ->
    lists:usort(lists:foldl(fun contact_vars_fold/2, [], Props)).

-spec contact_vars_fold({kz_term:ne_binary(), term()}, kz_term:proplist()) -> kz_term:proplist().
contact_vars_fold({<<"Proxy-Protocol">>, Proto}, Props) ->
    case kz_term:to_lower_binary(Proto) of
        <<"ws", _/binary>> ->
            [{<<"Media-Webrtc">>, 'true'}
            ,{<<"RTCP-MUX">>, 'true'}
             | Props
            ];
        _ -> Props
    end;
contact_vars_fold({<<"Original-Contact">>, Contact}, Props) ->
    [#uri{}=UriContact] = kzsip_uri:uris(Contact),
    case props:get_value(<<"transport">>, UriContact#uri.opts) of
        'undefined' -> Props;
        Transport -> contact_vars_fold({<<"Proxy-Protocol">>, Transport}, Props)
    end;
contact_vars_fold(_ , Props) -> Props.

-spec lookup_original_contact(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'not_found'}.
lookup_original_contact(Realm, Username) ->
    case kz_term:is_empty(Realm)
        orelse kz_term:is_empty(Username)
    of
        'true' -> {'error', 'not_found'};
        'false' ->
            case get_registration(Realm, Username) of
                #registration{original_contact=Contact} ->
                    lager:info("found user ~s@~s original contact ~s"
                              ,[Username, Realm, Contact]
                              ),
                    {'ok', Contact};
                'undefined' -> fetch_original_contact(Username, Realm)
            end
    end.

-spec lookup_registration(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          {'error', 'not_found'}.
lookup_registration(Realm, Username) ->
    case get_registration(Realm, Username) of
        #registration{}=Registration ->
            {'ok', kz_json:from_list(to_props(Registration))};
        'undefined' -> fetch_registration(Username, Realm)
    end.

-spec get_registration(kz_term:ne_binary(), kz_term:ne_binary()) -> 'undefined' | registration().
get_registration(Realm, Username) ->
    case ets:lookup(?MODULE, registration_id(Username, Realm)) of
        [#registration{}=Registration] -> Registration;
        _ -> 'undefined'
    end.

-spec summary() -> 'ok'.
summary() ->
    MatchSpec =
        [{#registration{_ = '_'}
         ,[]
         ,['$_']
         }
        ],
    print_summary(ets:select(?MODULE, MatchSpec, 1)).

-spec summary(kz_term:text()) -> 'ok'.
summary(Realm) when not is_binary(Realm) ->
    summary(kz_term:to_binary(Realm));
summary(Realm) ->
    R = kz_term:to_lower_binary(Realm),
    MatchSpec =
        [{#registration{realm = '$1'
                       ,account_realm = '$2'
                       ,_ = '_'
                       }
         ,[{'orelse'
           ,{'=:=', '$1', {'const', R}}
           ,{'=:=', '$2', {'const', R}}
           }
          ]
         ,['$_']
         }
        ],
    print_summary(ets:select(?MODULE, MatchSpec, 1)).

-spec details() -> 'ok'.
details() ->
    MatchSpec =
        [{#registration{_ = '_'}
         ,[]
         ,['$_']
         }
        ],
    print_details(ets:select(?MODULE, MatchSpec, 1)).

-spec details(kz_term:text()) -> 'ok'.
details(User) when not is_binary(User) ->
    details(kz_term:to_binary(User));
details(User) ->
    case binary:split(User, <<"@">>) of
        [Username, Realm] -> details(Username, Realm);
        _Else ->
            Realm = kz_term:to_lower_binary(User),
            MatchSpec =
                [{#registration{realm = '$1'
                               ,account_realm = '$2'
                               ,_ = '_'
                               }
                 ,[{'orelse'
                   ,{'=:=', '$1', {'const', Realm}}
                   ,{'=:=', '$2', {'const', Realm}}
                   }
                  ]
                 ,['$_']
                 }
                ],
            print_details(ets:select(?MODULE, MatchSpec, 1))
    end.

-spec details(kz_term:text(), kz_term:text()) -> 'ok'.
details(Username, Realm) when not is_binary(Username) ->
    details(kz_term:to_binary(Username), Realm);
details(Username, Realm) when not is_binary(Realm) ->
    details(Username, kz_term:to_binary(Realm));
details(Username, Realm) ->
    Id =  registration_id(Username, Realm),
    MatchSpec =
        [{#registration{id = '$1', _ = '_'}
         ,[{'=:=', '$1', {const, Id}}]
         ,['$_']
         }
        ],
    print_details(ets:select(?MODULE, MatchSpec, 1)).

-spec sync() -> 'ok'.
sync() ->
    gen_server:cast(?SERVER, 'registrar_sync').

-spec flush() -> 'ok'.
flush() ->
    gen_server:cast(?SERVER, 'flush').

-spec flush(kz_term:text()) -> 'ok'.
flush(Realm) when not is_binary(Realm)->
    flush(kz_term:to_binary(Realm));
flush(Realm) ->
    case binary:split(Realm, <<"@">>) of
        [Username, Realm] -> flush(Username, Realm);
        _Else -> gen_server:cast(?SERVER, {'flush', Realm})
    end.

-spec flush(kz_term:text() | 'undefined', kz_term:text()) -> 'ok'.
flush('undefined', Realm) ->
    flush(Realm);
flush(Username, Realm) when not is_binary(Realm) ->
    flush(Username, kz_term:to_binary(Realm));
flush(Username, Realm) when not is_binary(Username) ->
    flush(kz_term:to_binary(Username), Realm);
flush(Username, Realm) ->
    gen_server:cast(?SERVER, {'flush', Username, Realm}).

-spec count() -> non_neg_integer().
count() -> ets:info(?MODULE, 'size').

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    process_flag('trap_exit', 'true'),
    lager:debug("starting new ecallmgr registrar"),
    _ = ets:new(?MODULE, ['set', 'protected', 'named_table', {'keypos', #registration.id}]),
    erlang:send_after(2 * ?MILLISECONDS_IN_SECOND, self(), 'expire'),
    gproc:reg({'p', 'l', ?REGISTER_SUCCESS_REG}),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Msg, _From, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('registrar_sync', #state{queue=Q}=State) ->
    Payload = kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION),
    _ = kz_amqp_worker:cast(Payload, fun kapi_registration:publish_sync/1),
    {'noreply', State};
handle_cast({'insert_registration', Registration}, State) ->
    kz_log:put_callid(Registration#registration.call_id),
    _ = ets:insert(?MODULE, Registration#registration{initial='false'}),
    {'noreply', State};
handle_cast({'update_registration', {Username, Realm}=Id, Props}, State) ->
    lager:debug("updated registration ~s@~s", [Username, Realm]),
    _ = ets:update_element(?MODULE, Id, Props),
    {'noreply', State};
handle_cast({'delete_registration'
            ,#registration{id=Id
                          ,call_id=CallId
                          }=Reg
            }
           ,State) ->
    kz_log:put_callid(CallId),
    _ = kz_process:spawn(fun maybe_send_deregister_notice/1, [Reg]),
    ets:delete(?MODULE, Id),
    {'noreply', State};
handle_cast('flush', State) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    _ = ets:delete_all_objects(?MODULE),
    {'noreply', State};
handle_cast({'flush', Realm}, State) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    R = kz_term:to_lower_binary(Realm),
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
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    _ = ets:delete(?MODULE, registration_id(Username, Realm)),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    {'noreply', State#state{queue=Q}};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{queue=Q}=State) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    kapi_registration:publish_sync(kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)),
    {'noreply', State};
handle_cast(_Msg, State) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('expire', State) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    _ = expire_objects(),
    _ = erlang:send_after(2 * ?MILLISECONDS_IN_SECOND, self(), 'expire'),
    {'noreply', State};
handle_info(?REGISTER_SUCCESS_MSG(Node, Props), State) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    _ = kz_process:spawn(fun handle_fs_reg/2, [Node, Props]),
    {'noreply', State};
handle_info(_Info, State) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling AMQP event objects
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{started=Started}) ->
    {'reply', [{'registrar_age', kz_time:now_s() - Started}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_listener' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_listener' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), any()) -> 'ok'.
terminate(_Reason, _) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    lager:debug("ecallmgr registrar ~p termination", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec insert_registration(registration()) -> 'ok'.
insert_registration(#registration{expires=0}=Registration) ->
    lager:info("deleting registration ~s@~s with contact ~s"
              ,[Registration#registration.username
               ,Registration#registration.realm
               ,Registration#registration.contact
               ]
              ),
    gen_server:cast(?SERVER, {'delete_registration', Registration});
insert_registration(#registration{initial='true'}=Registration) ->
    gen_server:cast(?SERVER, {'insert_registration', Registration}),
    lager:info("inserted registration ~s@~s with contact ~s"
              ,[Registration#registration.username
               ,Registration#registration.realm
               ,Registration#registration.contact
               ]
              ),
    initial_registration(Registration);
insert_registration(#registration{}=Registration) ->
    gen_server:cast(?SERVER, {'insert_registration', Registration}),
    lager:debug("updated registration ~s@~s with contact ~s"
               ,[Registration#registration.username
                ,Registration#registration.realm
                ,Registration#registration.contact
                ]).

-spec fetch_registration(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          {'error', 'not_found'}.
fetch_registration(Username, Realm) ->
    Reg = [{<<"Username">>, Username}
          ,{<<"Realm">>, Realm}
          ,{<<"Fields">>, []} % will fetch all fields
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case query_for_registration(Reg) of
        {'ok', JObjs} ->
            find_newest_fetched_registration(Username, Realm, JObjs);
        _Else ->
            lager:info("registration query for user ~s@~s failed: ~p", [Username, Realm, _Else]),
            {'error', 'not_found'}
    end.

-spec query_for_registration(kz_term:api_terms()) ->
          {'ok', kz_json:objects()} |
          {'error', any()}.
query_for_registration(Reg) ->
    kz_amqp_worker:call_collect(Reg
                               ,fun kapi_registration:publish_query_req/1
                               ,{'ecallmgr', 'true'}
                               ,2 * ?MILLISECONDS_IN_SECOND
                               ).

-spec find_newest_fetched_registration(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) ->
          {'ok', kz_json:object()} |
          {'error', 'not_found'}.
find_newest_fetched_registration(Username, Realm, JObjs) ->
    Registrations =
        lists:flatten(
          [Replies
           || JObj <- JObjs,
              kz_api:event_name(JObj) =:= <<"reg_query_resp">>
                  andalso kapi_registration:query_resp_v(JObj),
              (Replies = kz_json:get_value(<<"Fields">>, JObj, [])) =/= []
          ]
         ),
    case lists:sort(fun sort_fetched_registrations/2, Registrations) of
        [Registration|_] ->
            lager:info("fetched user ~s@~s registration", [Username, Realm]),
            _ = maybe_insert_fetched_registration(Registration),
            {'ok', Registration};
        _Else ->
            lager:info("registration query for user ~s@~s returned an empty result"
                      ,[Username, Realm]
                      ),
            {'error', 'not_found'}
    end.

-spec maybe_insert_fetched_registration(kz_json:object()) -> 'ok'.
maybe_insert_fetched_registration(JObj) ->
    case kapps_config:get_boolean(?APP_NAME, <<"insert_fetched_registration_locally">>, 'false') of
        'false' -> 'ok';
        'true' -> insert_fetched_registration(JObj)
    end.

-spec insert_fetched_registration(kz_json:object()) -> 'ok'.
insert_fetched_registration(JObj) ->
    %% NOTE: create_registration will pad the registration which
    %%   will cause it to live longer on this server.  If the re-registration
    %%   to the other zone changes the contact this zone will continue to
    %%   use a stale value (also an issue if it re-registers before expiration)
    %%   unless it also expires here at close to the same time (preferably before).
    Expires = kz_json:get_integer_value(<<"Expires">>, JObj, ?EXPIRES_MISSING_VALUE)
        - ?EXPIRES_DEVIATION_TIME,
    Registration = create_registration(JObj),
    insert_registration(Registration#registration{expires=Expires}).

-spec sort_fetched_registrations(kz_json:object(), kz_json:object()) -> boolean().
sort_fetched_registrations(A, B) ->
    kz_json:get_integer_value(<<"Event-Timestamp">>, B) =<
        kz_json:get_integer_value(<<"Event-Timestamp">>, A).

-spec fetch_contact(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'not_found'}.
fetch_contact(Username, Realm) ->
    case fetch_registration(Username, Realm) of
        {'ok', JObj} ->
            Contact = kz_json:get_first_defined([<<"Bridge-RURI">>, <<"Contact">>], JObj),
            lager:info("found user ~s@~s contact ~s via fetch"
                      ,[Username, Realm, Contact]
                      ),
            {'ok', Contact, contact_vars(kz_json:to_proplist(JObj))};
        {'error', _R}=Error ->
            lager:info("original contact query for user ~s@~s failed: ~p", [Username, Realm, _R]),
            Error
    end.

-spec fetch_original_contact(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'not_found'}.
fetch_original_contact(Username, Realm) ->
    case fetch_registration(Username, Realm) of
        {'ok', JObj} ->
            Contact = kz_json:get_value(<<"Original-Contact">>, JObj),
            lager:info("found user ~s@~s original contact ~s via query"
                      ,[Username, Realm, Contact]
                      ),
            {'ok', Contact};
        {'error', _R}=Error ->
            lager:info("original contact query for user ~s@~s failed: ~p", [Username, Realm, _R]),
            Error
    end.

-spec expire_objects() -> 'ok'.
expire_objects() ->
    Now = kz_time:now_s(),
    MatchSpec = [{#registration{expires = '$1'
                               ,last_registration = '$2'
                               , _ = '_'
                               }
                 ,[{'>', {const, Now}, {'+', '$1', '$2'}}]
                 ,['$_']
                 }
                ],
    expire_object(ets:select(?MODULE, MatchSpec, 1)).

-spec expire_object(any()) -> 'ok'.
expire_object('$end_of_table') -> 'ok';
expire_object({[#registration{id=Id}=Reg], Continuation}) ->
    _ = kz_process:spawn(fun maybe_send_deregister_notice/1, [Reg]),
    _ = ets:delete(?MODULE, Id),
    expire_object(ets:select(Continuation)).

-spec maybe_resp_to_query(kz_json:object(), integer()) -> 'ok'.
maybe_resp_to_query(JObj, RegistrarAge) ->
    case kz_json:get_value(<<"Node">>, JObj)
        =:= kz_term:to_binary(node())
        andalso kz_json:get_value(<<"App-Name">>, JObj)
        =:= ?APP_NAME
    of
        'false' -> resp_to_query(JObj,  RegistrarAge);
        'true' ->
            Resp = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                   ,{<<"Registrar-Age">>,  RegistrarAge}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_registration:publish_query_err(kz_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

-spec build_query_spec(kz_json:object(), boolean()) -> ets:match_spec().
build_query_spec(JObj, CountOnly) ->
    {SelectFormat, QueryFormat} =
        case kz_term:to_lower_binary(kz_json:get_value(<<"Realm">>, JObj)) of
            <<"all">> -> {#registration{_='_'}, {'=:=', 'undefined', 'undefined'}};
            Realm -> build_query_spec_maybe_username(Realm, JObj)
        end,
    ResultFormat = case CountOnly of
                       'true' -> 'true';
                       'false' -> '$_'
                   end,

    [{SelectFormat
     ,[QueryFormat]
     ,[ResultFormat]
     }
    ].

-spec build_query_spec_maybe_username(kz_term:ne_binary(), kz_json:object()) -> any().
build_query_spec_maybe_username(Realm, JObj) ->
    case kz_json:get_value(<<"Username">>, JObj) of
        'undefined' ->
            {#registration{realm = '$1'
                          ,account_realm = '$2'
                          ,_ = '_'
                          }
            ,{'orelse', {'=:=', '$1', {'const', Realm}}
             ,{'=:=', '$2', {'const', Realm}}
             }
            };
        Username ->
            Id = registration_id(Username, Realm),
            {#registration{id = '$1', _ = '_'}
            ,{'=:=', '$1', {'const', Id}}
            }
    end.

-spec resp_to_query(kz_json:object(), integer()) -> 'ok'.
resp_to_query(JObj, RegistrarAge) ->
    Fields = kz_json:get_value(<<"Fields">>, JObj, []),
    CountOnly = kz_json:is_true(<<"Count-Only">>, JObj, 'false'),

    SelectFun = case CountOnly of
                    'true' -> fun ets:select_count/2;
                    'false' -> fun ets:select/2
                end,
    MatchSpec = build_query_spec(JObj, CountOnly),

    case SelectFun(?MODULE, MatchSpec) of
        [] ->
            Resp = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                   ,{<<"Registrar-Age">>, RegistrarAge}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_registration:publish_query_err(kz_json:get_value(<<"Server-ID">>, JObj), Resp);
        [_|_]=Registrations ->
            Resp = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                   ,{<<"Registrar-Age">>, RegistrarAge}
                   ,{<<"Fields">>, [filter(Fields, kz_json:from_list(to_props(Registration)))
                                    || Registration <- Registrations
                                   ]
                    }
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_registration:publish_query_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp);
        Count when is_integer(Count) ->
            Resp = [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                   ,{<<"Registrar-Age">>, RegistrarAge}
                   ,{<<"Fields">>, []}
                   ,{<<"Count">>, Count}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            kapi_registration:publish_query_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

-spec filter(kz_json:path(), kz_json:object()) -> kz_json:object().
filter([], JObj) -> JObj;
filter(Fields, JObj) ->
    kz_json:from_list(
      lists:foldl(fun(F, Acc) ->
                          [{F, kz_json:get_value(F, JObj)} | Acc]
                  end
                 ,[]
                 ,Fields
                 )
     ).

-spec registration_id(kz_term:ne_binary(), kz_term:ne_binary()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
registration_id(Username, Realm) ->
    {kz_term:to_lower_binary(Username), kz_term:to_lower_binary(Realm)}.

-spec create_registration(kz_json:object()) -> registration().
create_registration(JObj) ->
    Username = kz_json:get_value(<<"Username">>, JObj),
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    Reg = existing_or_new_registration(Username, Realm),
    Proxy = kz_json:get_value(<<"Proxy-Path">>, JObj, Reg#registration.proxy),
    ProxyIP = kz_json:get_value(<<"Proxy-IP">>, JObj, Reg#registration.proxy_ip),
    ProxyPort = kz_json:get_integer_value(<<"Proxy-Port">>, JObj, Reg#registration.proxy_port),
    ProxyProto = kz_json:get_value(<<"Proxy-Protocol">>, JObj, Reg#registration.proxy_proto),
    OriginalContact =
        kz_json:get_first_defined([<<"Original-Contact">>
                                  ,<<"Contact">>
                                  ]
                                 ,JObj
                                 ,Reg#registration.original_contact
                                 ),
    Expires =
        ecallmgr_util:maybe_add_expires_deviation(
          kz_json:get_integer_value(<<"Expires">>, JObj, Reg#registration.expires)
         ),
    RegistrarNode =
        kz_json:get_first_defined([<<"Registrar-Node">>
                                  ,<<"FreeSWITCH-Nodename">>
                                  ,<<"Node">>
                                  ]
                                 ,JObj
                                 ,Reg#registration.registrar_node
                                 ),
    RegistrarHostname =
        kz_json:get_first_defined([<<"Hostname">>
                                  ,<<"Registrar-Hostname">>
                                  ]
                                 ,JObj
                                 ,Reg#registration.registrar_hostname
                                 ),
    augment_registration(Reg#registration{username=Username
                                         ,realm=Realm
                                         ,proxy=Proxy
                                         ,proxy_ip=ProxyIP
                                         ,proxy_port=ProxyPort
                                         ,proxy_proto=ProxyProto
                                         ,expires=Expires
                                         ,registrar_node=RegistrarNode
                                         ,registrar_hostname=RegistrarHostname
                                         ,contact=fix_contact(OriginalContact)
                                         ,original_contact=OriginalContact
                                         ,bridge_uri=bridge_uri(OriginalContact, Proxy, Username, Realm)
                                         ,previous_contact=kz_json:get_value(<<"Previous-Contact">>, JObj, Reg#registration.previous_contact)
                                         ,last_registration=kz_json:get_integer_value(<<"Last-Registration">>, JObj, Reg#registration.last_registration)
                                         ,initial_registration=kz_json:get_integer_value(<<"Initial-Registration">>, JObj, Reg#registration.initial_registration)
                                         ,network_port=kz_json:get_value(<<"Network-Port">>, JObj, Reg#registration.network_port)
                                         ,network_ip=kz_json:get_value(<<"Network-IP">>, JObj, Reg#registration.network_ip)
                                         ,to_host=get_realm(<<"To-Host">>, JObj)
                                         ,to_user=kz_json:get_value(<<"To-User">>, JObj, Reg#registration.to_user)
                                         ,from_host=get_realm(<<"From-Host">>, JObj)
                                         ,from_user=kz_json:get_value(<<"From-User">>, JObj, Reg#registration.from_user)
                                         ,call_id=kz_json:get_value(<<"Call-ID">>, JObj, Reg#registration.call_id)
                                         ,user_agent=kz_json:get_value(<<"User-Agent">>, JObj, Reg#registration.user_agent)
                                         ,initial=kz_json:is_true(<<"First-Registration">>, JObj, Reg#registration.initial)
                                         ,source_ip=kz_json:get_value(<<"Source-IP">>, JObj)
                                         ,source_port=kz_json:get_value(<<"Source-Port">>, JObj)
                                         }
                        ,JObj
                        ).

-spec get_realm(kz_json:key(), kz_json:object()) -> kz_term:ne_binary().
get_realm(Key, JObj) ->
    case kz_json:get_ne_binary_value(Key, JObj) of
        'undefined' -> ?DEFAULT_REALM;
        Realm -> Realm
    end.

-spec augment_registration(registration(), kz_json:object()) -> registration().
augment_registration(Reg, JObj) ->
    CCVs = kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    AccountId = kz_json:find(<<"Account-ID">>
                            ,[JObj, CCVs]
                            ,Reg#registration.account_id
                            ),
    SuppressUnregister =
        kz_term:is_true(
          case kz_json:find(<<"Suppress-Unregister-Notifications">>, [JObj, CCVs]) of
              'undefined' ->
                  kz_json:find(<<"Suppress-Unregister-Notify">>
                              ,[JObj, CCVs]
                              ,Reg#registration.suppress_unregister
                              );
              Else -> Else
          end
         ),
    OverwriteNotify =
        kz_term:is_true(
          kz_json:find(<<"Register-Overwrite-Notify">>
                      ,[JObj, CCVs]
                      ,Reg#registration.register_overwrite_notify
                      )
         ),
    AccountDb = kzs_util:format_account_db(AccountId),
    Reg#registration{account_id=AccountId
                    ,account_db=AccountDb
                    ,suppress_unregister=SuppressUnregister
                    ,register_overwrite_notify=OverwriteNotify
                    ,account_realm=kz_json:find(<<"Account-Realm">>
                                               ,[JObj, CCVs]
                                               ,Reg#registration.account_realm
                                               )
                    ,account_name=kz_json:find(<<"Account-Name">>
                                              ,[JObj, CCVs]
                                              ,Reg#registration.account_name
                                              )
                    ,owner_id=kz_json:find(<<"Owner-ID">>
                                          ,[JObj, CCVs]
                                          ,Reg#registration.owner_id
                                          )
                    ,presence_id=kz_json:find(<<"Presence-ID">>
                                             ,[JObj, CCVs]
                                             ,Reg#registration.presence_id
                                             )
                    ,authorizing_id=kz_json:find(<<"Authorizing-ID">>
                                                ,[JObj, CCVs]
                                                ,Reg#registration.authorizing_id
                                                )
                    ,authorizing_type=kz_json:find(<<"Authorizing-Type">>
                                                  ,[JObj, CCVs]
                                                  ,Reg#registration.authorizing_type
                                                  )
                    }.

-spec fix_contact(kz_term:api_binary()) -> kz_term:api_binary().
fix_contact('undefined') -> 'undefined';
fix_contact(Contact) ->
    binary:replace(Contact, [<<"<">>, <<">">>], <<>>, ['global']).

-spec bridge_uri(kz_term:api_binary(), kz_term:api_binary(), binary(), binary()) -> kz_term:api_binary().
bridge_uri(_Contact, 'undefined', _, _) -> 'undefined';
bridge_uri('undefined', _Proxy, _, _) -> 'undefined';
bridge_uri(Contact, Proxy, Username, Realm) ->
    [#uri{}=UriContact] = kzsip_uri:uris(Contact),
    [#uri{}=UriProxy] = kzsip_uri:uris(Proxy),
    Scheme = UriContact#uri.scheme,
    Options = #{uri_contact => UriContact
               ,uri_proxy => UriProxy
               },
    BridgeUriOptions = bridge_uri_options(Options),
    BridgeUri = #uri{scheme=Scheme
                    ,user=Username
                    ,domain=Realm
                    ,opts=BridgeUriOptions
                    },
    kzsip_uri:ruri(BridgeUri).

-spec bridge_uri_options(map()) -> kz_term:proplist().
bridge_uri_options(Options) ->
    Routines = [fun bridge_uri_transport/2
               ,fun bridge_uri_path/2
               ],
    lists:foldl(fun(Fun, Acc) -> Fun(Options, Acc) end, [], Routines).

-spec bridge_uri_transport(map(), kz_term:proplist()) -> kz_term:proplist().
bridge_uri_transport(#{uri_contact := UriContact}, Acc) ->
    case application:get_env(?APP, 'use_transport_for_fs_path', 'false') of
        'true' ->
            case props:get_value(<<"transport">>, UriContact#uri.opts) of
                'undefined' -> Acc;
                Transport -> [{<<"transport">>, Transport} | Acc]
            end;
        'false' -> Acc
    end.

-spec bridge_uri_path(map(), kz_term:proplist()) -> kz_term:proplist().
bridge_uri_path(#{uri_proxy := UriProxy}, Acc) ->
    [{<<"fs_path">>, kzsip_uri:ruri(UriProxy)} | Acc].

-spec existing_or_new_registration(kz_term:ne_binary(), kz_term:ne_binary()) -> registration().
existing_or_new_registration(Username, Realm) ->
    case ets:lookup(?MODULE, registration_id(Username, Realm)) of
        [#registration{contact=Contact}=Reg] ->
            Reg#registration{last_registration=kz_time:now_s()
                            ,previous_contact=Contact
                            };
        _Else ->
            lager:debug("new registration ~s@~s", [Username, Realm]),
            #registration{id=registration_id(Username, Realm)}
    end.

-spec initial_registration(registration()) -> 'ok'.
initial_registration(#registration{}=Reg) ->
    Routines = [fun maybe_query_authn/1
               ,fun maybe_send_register_notice/1
               ,fun maybe_registration_notify/1
               ],
    _ = lists:foldl(fun(F, R) -> F(R) end, Reg, Routines),
    'ok'.

-spec maybe_query_authn(registration()) -> registration().
maybe_query_authn(#registration{account_id=AccountId
                               ,authorizing_id=AuthorizingId
                               }=Reg) ->
    case kz_term:is_empty(AccountId)
        orelse kz_term:is_empty(AuthorizingId)
    of
        'true' -> query_authn(Reg);
        'false' -> Reg
    end.

-spec query_authn(registration()) -> registration().
query_authn(#registration{username=Username
                         ,realm=Realm
                         }=Reg) ->
    case kz_cache:peek_local(?ECALLMGR_AUTH_CACHE, ?CREDS_KEY(Realm, Username)) of
        {'error', 'not_found'} -> fetch_authn(Reg);
        {'ok', JObj} ->
            update_registration(
              augment_registration(Reg, JObj)
             )
    end.

-spec fetch_authn(registration()) -> registration().
fetch_authn(#registration{username=Username
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
          ,{<<"Media-Server">>, kz_term:to_binary(Node)}
          ,{<<"Method">>, <<"REGISTER">>}
          ,{<<"Call-ID">>, CallId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = kz_amqp_worker:call(props:filter_undefined(Req)
                                 ,fun kapi_authn:publish_req/1
                                 ,fun kapi_authn:resp_v/1
                                 ),
    case ReqResp of
        {'error', _} -> Reg;
        {'ok', JObj} ->
            lager:debug("received authn information"),
            update_from_authn_response(Reg, JObj)
    end.

-spec update_from_authn_response(registration(), kz_json:object()) -> registration().
update_from_authn_response(#registration{username=Username
                                        ,realm=Realm
                                        }=Reg
                          ,JObj) ->
    CCVs = kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    AccountId = kz_json:get_value(<<"Account-ID">>, CCVs),
    AccountDb = kzs_util:format_account_db(AccountId),
    AuthorizingId = kz_json:get_value(<<"Authorizing-ID">>, CCVs),
    OwnerIdProp =
        case kz_json:get_value(<<"Owner-ID">>, CCVs) of
            'undefined' -> [];
            OwnerId -> [{'db', AccountDb, OwnerId}]
        end,
    CacheProps =
        [{'origin',
          [{'db', AccountDb, AuthorizingId}
          ,{'db', AccountDb, AccountId}
           | OwnerIdProp
          ]
         }
        ],
    kz_cache:store_local(?ECALLMGR_AUTH_CACHE
                        ,?CREDS_KEY(Realm, Username)
                        ,JObj
                        ,CacheProps
                        ),
    update_registration(
      augment_registration(Reg, JObj)
     ).

-spec update_registration(registration()) -> registration().
update_registration(#registration{authorizing_id=AuthorizingId
                                 ,account_id=AccountId
                                 ,authorizing_type=AuthorizingType
                                 ,account_db=AccountDb
                                 ,suppress_unregister=SuppressUnregister
                                 ,register_overwrite_notify=RegisterOverwrite
                                 ,owner_id=OwnerId
                                 ,presence_id=PresenceId
                                 ,id=Id
                                 ,account_realm=AccountRealm
                                 ,account_name=AccountName
                                 }=Reg) ->
    Props = [{#registration.account_id, AccountId}
            ,{#registration.account_db, AccountDb}
            ,{#registration.authorizing_id, AuthorizingId}
            ,{#registration.authorizing_type, AuthorizingType}
            ,{#registration.owner_id, OwnerId}
            ,{#registration.presence_id, PresenceId}
            ,{#registration.suppress_unregister, SuppressUnregister}
            ,{#registration.register_overwrite_notify, RegisterOverwrite}
            ,{#registration.account_realm, AccountRealm}
            ,{#registration.account_name, AccountName}
            ],
    _ = gen_server:cast(?SERVER, {'update_registration', Id, Props}),
    Reg.

-spec maybe_send_register_notice(registration()) -> registration().
maybe_send_register_notice(#registration{username=Username
                                        ,realm=Realm
                                        }=Reg) ->
    case oldest_registrar() of
        'false' -> Reg;
        'true' ->
            lager:debug("sending register notice for ~s@~s", [Username, Realm]),
            _ = send_register_notice(Reg),
            Reg
    end.

-spec send_register_notice(registration()) -> 'ok'.
send_register_notice(Reg) ->
    Props = to_props(Reg)
        ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION),
    kapi_notifications:publish_register(Props).

-spec maybe_send_deregister_notice(registration()) -> 'ok'.
maybe_send_deregister_notice(#registration{username=Username
                                          ,realm=Realm
                                          ,suppress_unregister='true'
                                          ,call_id=CallId
                                          }) ->
    kz_log:put_callid(CallId),
    lager:debug("registration ~s@~s expired", [Username, Realm]);
maybe_send_deregister_notice(#registration{username=Username
                                          ,realm=Realm
                                          ,call_id=CallId
                                          }=Reg) ->
    kz_log:put_callid(CallId),
    case oldest_registrar() of
        'false' -> 'ok';
        'true' ->
            lager:debug("sending deregister notice for ~s@~s", [Username, Realm]),
            send_deregister_notice(Reg)
    end.

-spec send_deregister_notice(registration()) -> 'ok'.
send_deregister_notice(Reg) ->
    Props = to_props(Reg)
        ++ kz_api:default_headers(?APP_NAME, ?APP_VERSION),
    kz_amqp_worker:cast(Props, fun kapi_notifications:publish_deregister/1).

-spec maybe_registration_notify(registration()) -> registration().
maybe_registration_notify(#registration{register_overwrite_notify='false'}=Reg) -> Reg;
maybe_registration_notify(#registration{register_overwrite_notify='true'
                                       ,contact=Contact
                                       ,previous_contact=Contact
                                       }=Reg) -> Reg;
maybe_registration_notify(#registration{register_overwrite_notify='true'
                                       ,previous_contact='undefined'
                                       }=Reg) -> Reg;
maybe_registration_notify(#registration{register_overwrite_notify='true'}=Reg) ->
    _ = registration_notify(Reg),
    Reg.

-spec registration_notify(registration()) -> 'ok'.
registration_notify(#registration{previous_contact=PrevContact
                                 ,contact=Contact
                                 ,username=Username
                                 ,realm=Realm
                                 }) ->
    Props = props:filter_undefined(
              [{<<"Previous-Contact">>, PrevContact}
              ,{<<"Contact">>, Contact}
              ,{<<"Username">>, Username}
              ,{<<"Realm">>, Realm}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ]),
    kapi_presence:publish_register_overwrite(Props).

-spec to_props(registration()) -> kz_term:proplist().
to_props(Reg) ->
    props:filter_undefined(
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
      ,{<<"Previous-Contact">>, Reg#registration.previous_contact}
      ,{<<"Proxy-Path">>, Reg#registration.proxy}
      ,{<<"Proxy-IP">>, Reg#registration.proxy_ip}
      ,{<<"Proxy-Port">>, Reg#registration.proxy_port}
      ,{<<"Proxy-Protocol">>, Reg#registration.proxy_proto}
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
      ,{<<"Presence-ID">>, Reg#registration.presence_id}
      ,{<<"Registrar-Node">>, Reg#registration.registrar_node}
      ,{<<"Registrar-Hostname">>, Reg#registration.registrar_hostname}
      ,{<<"Bridge-RURI">>, Reg#registration.bridge_uri}
      ,{<<"First-Registration">>, Reg#registration.initial}
      ,{<<"Initial-Registration">>, Reg#registration.initial_registration}
      ,{<<"Last-Registration">>, Reg#registration.last_registration}
      ,{<<"Source-IP">>, Reg#registration.source_ip}
      ,{<<"Source-Port">>, Reg#registration.source_port}
      ]
     ).

-spec oldest_registrar() -> boolean().
oldest_registrar() ->
    kz_nodes:whapp_count(?APP_NAME, 'true') =:= 1
        orelse kz_nodes:whapp_oldest_node(?APP_NAME, 'true') =:= node().

-spec get_fs_contact(kzd_freeswitch:data()) -> kz_term:ne_binary().
get_fs_contact(FSJObj) ->
    Contact = kzd_freeswitch:contact(FSJObj),
    [User, AfterAt] = binary:split(Contact, <<"@">>), % only one @ allowed
    <<User/binary, "@", (kz_http_util:urldecode(AfterAt))/binary>>.

-type ets_continuation() :: '$end_of_table' |
                            {registrations(), any()}.

-spec print_summary(ets_continuation()) -> 'ok'.
print_summary('$end_of_table') ->
    io:format("No registrations found!~n", []);
print_summary(Match) ->
    io:format("+-----------------------------------------------+------------------------+------------------------+----------------------------------+------+~n"),
    io:format("| User                                          | Contact                | Path                   | Call-ID                          |  Exp |~n"),
    io:format("+===============================================+========================+========================+==================================+======+~n"),
    print_summary(Match, 0).

-spec print_summary(ets_continuation(), non_neg_integer()) -> 'ok'.
print_summary('$end_of_table', Count) ->
    io:format("+-----------------------------------------------+------------------------+------------------------+----------------------------------+------+~n"),
    io:format("Found ~p registrations~n", [Count]);
print_summary({[#registration{username=Username
                             ,realm=Realm
                             ,contact=Contact
                             ,expires=Expires
                             ,last_registration=LastRegistration
                             ,call_id=CallId
                             ,proxy=Proxy
                             ,proxy_ip=ProxyIP
                             ,proxy_port=ProxyPort
                             ,proxy_proto=ProxyProto
                             }
               ]
              ,Continuation
              }
             ,Count) ->
    User = <<Username/binary, "@", Realm/binary>>,
    Remaining = (LastRegistration + Expires) - kz_time:now_s(),
    Props = breakup_contact(Contact),
    Hostport = props:get_first_defined(['received', 'hostport'], Props),
    Path = proxy_path(Proxy, ProxyIP, ProxyPort, ProxyProto),
    io:format("| ~-45s | ~-22s | ~-22s | ~-32s | ~-4B |~n"
             ,[User, Hostport, Path, CallId, Remaining]
             ),
    print_summary(ets:select(Continuation), Count + 1).

-spec print_details(ets_continuation()) -> 'ok'.
print_details('$end_of_table') ->
    io:format("No registrations found!~n", []);
print_details(Match) ->
    print_details(Match, 0).

-spec print_details(ets_continuation(), non_neg_integer()) -> 'ok'.
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
    Remaining = (LastRegistration + Expires) - kz_time:now_s(),
    io:format("~-19s: ~b/~s~n", [Key, Remaining, kz_term:to_binary(Value)]);
print_property(Key, Value, _) ->
    io:format("~-19s: ~s~n", [Key, kz_term:to_binary(Value)]).

-type contact_param() :: {'uri', kz_term:ne_binary()} |
                         {'hostport', kz_term:ne_binary()} |
                         {'transport', kz_term:ne_binary()} |
                         {'fs_path', kz_term:ne_binary()} |
                         {'received', kz_term:ne_binary()}.
-type contact_params() :: [contact_param()].

-spec breakup_contact(kz_term:text()) -> contact_params().
breakup_contact(Contact) when is_binary(Contact) ->
    C = binary:replace(Contact, [<<$'>>, <<$<>>, <<$>>>, <<"sip:">>], <<>>, ['global']),
    [Uri|Parameters] = binary:split(C, <<";">>, ['global']),
    Hostport = get_contact_hostport(Uri),
    find_contact_parameters(Parameters, [{'uri', Uri}, {'hostport', Hostport}]);
breakup_contact(Contact) ->
    breakup_contact(kz_term:to_binary(Contact)).

-spec proxy_path(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_integer(), kz_term:api_binary()) -> binary().
proxy_path(Proxy, IP, Port, 'undefined') -> proxy_path(Proxy, IP, Port, <<"udp">>);
proxy_path('undefined', 'undefined', 'undefined', _) -> <<>>;
proxy_path('undefined', 'undefined', Port, Proto) -> proxy_path('undefined', <<>>, Port, Proto);
proxy_path('undefined', IP, 'undefined', Proto) -> <<Proto/binary, ":", IP/binary>>;
proxy_path('undefined', IP, Port, Proto) -> <<Proto/binary, ":", IP/binary, ":", (kz_term:to_binary(Port))/binary>>;
proxy_path(Proxy, _, Port, Proto) ->
    Proxy1 = binary:replace(Proxy, <<"sip:">>, <<>>),
    case binary:match(Proxy1, <<":">>) of
        'nomatch' -> <<Proto/binary, ":", Proxy1/binary, ":", (kz_term:to_binary(Port))/binary>>;
        _ -> <<Proto/binary, ":", Proxy1/binary>>
    end.

-spec find_contact_parameters(kz_term:ne_binaries(), kz_term:proplist()) -> kz_term:proplist().
find_contact_parameters([], Props) -> Props;
find_contact_parameters([<<"transport=", Transport/binary>>|Parameters], Props) ->
    find_contact_parameters(Parameters, [{'transport', kz_term:to_lower_binary(Transport)}|Props]);
find_contact_parameters([<<"fs_path=", FsPath/binary>>|Parameters], Props) ->
    find_contact_parameters(Parameters, [{'fs_path', FsPath}|Props]);
find_contact_parameters([<<"received=", Received/binary>>|Parameters], Props) ->
    find_contact_parameters(Parameters, [{'received', Received}|Props]);
find_contact_parameters([_|Parameters], Props) ->
    find_contact_parameters(Parameters, Props).

-spec get_contact_hostport(kz_term:ne_binary()) -> kz_term:ne_binary().
get_contact_hostport(Uri) ->
    case binary:split(Uri, <<"@">>) of
        [_, Hostport] -> Hostport;
        _Else -> Uri
    end.
