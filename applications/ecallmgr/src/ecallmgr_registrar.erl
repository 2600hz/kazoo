%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Listener for reg_success, and reg_query AMQP requests
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_registrar).

-behaviour(gen_listener).

-export([start_link/0]).
-export([lookup_contact/2]).
-export([reg_success/2
         ,reg_query/2
         ,reg_flush/2
         ,handle_reg_success/2
        ]).
-export([summary/0
         ,summary/1
        ]).
-export([details/0
         ,details/1
         ,details/2
        ]).
-export([flush/0
         ,flush/1
         ,flush/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").


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
-define(BINDINGS, [{'registration', [{'retrict_to', ['reg_success', 'reg_query', 'reg_flush']}]}
                   ,{'self', []}
                  ]).
-define(SERVER, ?MODULE).
-define(REG_QUEUE_NAME, <<>>).
-define(REG_QUEUE_OPTIONS, []).
-define(REG_CONSUME_OPTIONS, []).
-define(SUMMARY_REGEX, <<"^.*?:.*@([0-9.:]*)(?:;fs_path=.*?:([0-9.:]*))*">>).

-record(state, {started = wh_util:current_tstamp()}).

-record(registration, {id
                       ,username
                       ,realm
                       ,network_port
                       ,network_ip
                       ,to_host
                       ,to_user
                       ,from_host
                       ,from_user
                       ,call_id
                       ,user_agent
                       ,expires
                       ,contact
                       ,last_registration
                       ,initial_registration
                       ,registrar_node
                       ,registrar_hostname
                       ,suppress_unregister = 'true'
                       ,account_db
                       ,account_id
                       ,authorizing_id
                       ,authorizing_type
                       ,owner_id
                       ,initial = 'true'
                      }).

-type registration() :: #registration{}.

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
    gen_listener:start_link({'local', ?MODULE}, ?MODULE, [{'responders', ?RESPONDERS}
                                                          ,{'bindings', ?BINDINGS}
                                                          ,{'queue_name', ?REG_QUEUE_NAME}
                                                          ,{'queue_options', ?REG_QUEUE_OPTIONS}
                                                          ,{'consume_options', ?REG_CONSUME_OPTIONS}
                                                         ], []).

-spec reg_success(wh_json:object(), wh_proplist()) -> 'ok'.
reg_success(JObj, _Props) ->
    'true' = wapi_registration:success_v(JObj),
    _ = wh_util:put_callid(JObj),
    Registration = create_registration(JObj),
    gen_server:cast(?MODULE, {'insert_registration', Registration}),
    lager:info("inserted registration ~s@~s with contact ~s", [Registration#registration.username
                                                               ,Registration#registration.realm
                                                               ,Registration#registration.contact
                                                              ]),
    whistle_stats:increment_counter("register-success"),
    maybe_initial_registration(Registration).

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
lookup_contact(Realm, Username) ->
    case ets:lookup(?MODULE, registration_id(Username, Realm)) of
        [#registration{contact=Contact}] ->
            lager:info("found user ~s@~s contact ~s"
                       ,[Username, Realm, Contact]),
            {'ok', Contact};
        _Else -> fetch_contact(Username, Realm)
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
    MatchSpec = [{#registration{id = {'_', '$1'}, _ = '_'}
                  ,[{'=:=', '$1', {const, R}}]
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
            MatchSpec = [{#registration{id = {'_', '$1'}, _ = '_'}
                          ,[{'=:=', '$1', {const, Realm}}]
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

-spec handle_reg_success(atom(), wh_proplist()) -> 'ok'.
handle_reg_success(Node, Props) ->
    put('callid', props:get_first_defined([<<"Call-ID">>, <<"call-id">>], Props, 'reg_success')),
    Req = lists:foldl(fun(K, Acc) ->
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
    wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                        ,Req
                        ,fun wapi_registration:publish_success/1
                       ).

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
    erlang:send_after(2000, self(), 'expire'),

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
handle_cast('flush', State) ->
    _ = ets:delete_all_objects(?MODULE),
    {'noreply', State};
handle_cast({'flush', Realm}, State) ->
    R = wh_util:to_lower_binary(Realm),
    MatchSpec = [{#registration{id = {'_', '$1'}, _ = '_'}
                  ,[{'=:=', '$1', {const, R}}]
                  ,['true']
                 }],
    NumberDeleted = ets:select_delete(?MODULE, MatchSpec),
    lager:debug("removed ~p expired registrations", [NumberDeleted]),
    {'noreply', State};
handle_cast({'flush', Username, Realm}, State) ->
    _ = ets:delete(?MODULE, registration_id(Username, Realm)),
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
    _ = erlang:send_after(2000, self(), 'expire'),
    {'noreply', State};
handle_info(?REGISTER_SUCCESS_MSG(Node, Props), State) ->
    spawn(?MODULE, 'handle_reg_success', [Node, Props]),
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
-spec fetch_contact(ne_binary(), ne_binary()) -> {'ok', ne_binary()} | {'error', 'not_found'}.
fetch_contact(Username, Realm) ->
    Reg = [{<<"Username">>, Username}
           ,{<<"Realm">>, Realm}
           ,{<<"Fields">>, [<<"Contact">>]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_amqp_worker:call_collect(?ECALLMGR_AMQP_POOL
                                     ,Reg
                                     ,fun wapi_registration:publish_query_req/1
                                     ,{'ecallmgr', fun wapi_registration:query_resp_v/1}
                                     ,2000)
    of
        {'ok', JObjs} ->
            case [Contact
                  || JObj <- JObjs
                         ,wapi_registration:query_resp_v(JObj)
                         ,(Contact = wh_json:get_value([<<"Fields">>, 1, <<"Contact">>]
                                                       ,JObj)) =/= 'undefined'
                 ]
            of
                [Contact|_] ->
                    lager:info("fetched user ~s@~s contact ~s", [Username, Realm, Contact]),
                    {'ok', Contact};
                _Else ->
                    lager:info("contact query for user ~s@~s returned an empty result", [Username, Realm]),
                    {'error', 'not_found'}
            end;
        _Else ->
            lager:info("contact query for user ~s@~s failed: ~p", [Username, Realm, _Else]),
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
expire_object({[#registration{id=Id
                              ,suppress_unregister='true'
                              ,username=Username
                              ,realm=Realm
                              ,call_id=CallId}
               ], Continuation}) ->
    put(callid, CallId),
    lager:debug("registration ~s@~s expired", [Username, Realm]),
    _ = ets:delete(?MODULE, Id),
    expire_object(ets:select(Continuation));
expire_object({[#registration{id=Id
                              ,username=Username
                              ,realm=Realm
                              ,call_id=CallId}=Reg
               ], Continuation}) ->
    put('callid', CallId),
    lager:debug("registration ~s@~s expired", [Username, Realm]),
    _ = ets:delete(?MODULE, Id),
    _ = spawn(fun() ->
                      put('callid', CallId),
                      case oldest_registrar(Username, Realm) of
                          'false' -> 'ok';
                          'true' ->
                              lager:debug("sending deregister notice for ~s@~s", [Username, Realm]),
                              send_deregister_notice(Reg)
                      end
              end),
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

-spec resp_to_query(wh_json:object()) -> 'ok'.
resp_to_query(JObj) ->
    Fields = wh_json:get_value(<<"Fields">>, JObj, []),
    Realm = wh_util:to_lower_binary(wh_json:get_value(<<"Realm">>, JObj)),
    MatchSpec = case wh_json:get_value(<<"Username">>, JObj) of
                    'undefined' ->
                        [{#registration{id = {'_', '$1'}, _ = '_'}
                          ,[{'=:=', '$1', {const, Realm}}]
                          ,['$_']
                         }];
                    Username ->
                        Id = registration_id(Username, Realm),
                        [{#registration{id = '$1', _ = '_'}
                          ,[{'=:=', '$1', {const, Id}}]
                          ,['$_']
                         }]
                end,
    case [wh_json:from_list(to_props(Reg))
          || Reg <- ets:select(?MODULE, MatchSpec)
         ]
    of
        [] ->
            Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    ,{<<"Registrar-Age">>, gen_server:call(?MODULE, 'registrar_age')}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_registration:publish_query_err(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
        [_|_]=Registrations ->
            Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    ,{<<"Registrar-Age">>, gen_server:call(?MODULE, 'registrar_age')}
                    ,{<<"Fields">>, [filter(Fields, Registration)
                                     || Registration <- Registrations
                                    ]}
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
    Reg = existing_or_new_registration(Username, Realm),
    Reg#registration{username=Username
                     ,realm=Realm
                     ,network_port=wh_json:get_value(<<"Network-Port">>, JObj)
                     ,network_ip=wh_json:get_value(<<"Network-IP">>, JObj)
                     ,to_host=wh_json:get_value(<<"To-Host">>, JObj, ?DEFAULT_REALM)
                     ,to_user=wh_json:get_value(<<"To-User">>, JObj, <<"nouser">>)
                     ,from_host=wh_json:get_value(<<"From-Host">>, JObj, ?DEFAULT_REALM)
                     ,from_user=wh_json:get_value(<<"From-User">>, JObj, <<"nouser">>)
                     ,call_id=wh_json:get_value(<<"Call-ID">>, JObj)
                     ,user_agent=wh_json:get_value(<<"User-Agent">>, JObj)
                     ,expires=wh_json:get_integer_value(<<"Expires">>, JObj, 60)
                     ,contact=fix_contact(wh_json:get_value(<<"Contact">>, JObj))
                     ,last_registration=wh_util:current_tstamp()
                     ,registrar_node=wh_json:get_value(<<"Node">>, JObj)
                     ,registrar_hostname=wh_json:get_value(<<"Hostname">>, JObj)
                    }.

-spec fix_contact(ne_binary()) -> ne_binary().
fix_contact(Contact) ->
    [User, AfterAt] = binary:split(Contact, <<"@">>), % only one @ allowed
    AfterUnquoted = wh_util:to_binary(mochiweb_util:unquote(AfterAt)),
    binary:replace(<<User/binary, "@", AfterUnquoted/binary>>
                   ,[<<"<">>, <<">">>]
                   ,<<>>
                   ,['global']).

-spec existing_or_new_registration(ne_binary(), ne_binary()) -> registration().
existing_or_new_registration(Username, Realm) ->
    case ets:lookup(?MODULE, registration_id(Username, Realm)) of
        [#registration{}=Reg] -> Reg;
        _Else ->
            lager:debug("new registration ~s@~s", [Username, Realm]),
            #registration{id=registration_id(Username, Realm)
                          ,initial_registration=wh_util:current_tstamp()
                         }
    end.

-spec maybe_initial_registration(registration()) -> 'ok'.
maybe_initial_registration(#registration{initial='false'}) -> 'ok';
maybe_initial_registration(#registration{initial='true'}=Reg) ->
    initial_registration(Reg).

-spec initial_registration(registration()) -> 'ok'.
initial_registration(#registration{}=Reg) ->
    Routines = [fun maybe_query_authn/1
                ,fun update_cache/1
                ,fun maybe_send_register_notice/1
               ],
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
                             ,suppress_unregister = wh_json:is_true(<<"Suppress-Unregister-Notifications">>, JObj)
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
                          ,registrar_node=Node
                          ,call_id=CallId
                         }=Reg) ->
    lager:debug("looking up credentials of ~s@~s", [Username, Realm]),
    Req = [{<<"To">>, <<ToUser/binary, "@", ToHost/binary>>}
           ,{<<"From">>, <<FromUser/binary, "@", FromHost/binary>>}
           ,{<<"Orig-IP">>, NetworkIP}
           ,{<<"Auth-User">>, Username}
           ,{<<"Auth-Realm">>, Realm}
           ,{<<"Media-Server">>, wh_util:to_binary(Node)}
           ,{<<"Method">>, <<"REGISTER">>}
           ,{<<"Call-ID">>, CallId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,props:filter_undefined(Req)
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
            CacheProps = [{'origin', [{'db', AccountDb, AuthorizingId}
                                      ,{'db', AccountDb, AccountId}
                                     ]}
                         ],
            wh_cache:store_local(?ECALLMGR_AUTH_CACHE
                                 ,?CREDS_KEY(Realm, Username)
                                 ,JObj
                                 ,CacheProps),
            Reg#registration{account_id = AccountId
                             ,account_db = AccountDb
                             ,authorizing_id = AuthorizingId
                             ,authorizing_type = wh_json:get_value(<<"Authorizing-Type">>, CCVs)
                             ,owner_id = wh_json:get_value(<<"Owner-ID">>, CCVs)
                             ,suppress_unregister = wh_json:is_true(<<"Suppress-Unregister-Notifications">>, JObj)
                            }
    end.

-spec update_cache(registration()) -> registration().
update_cache(#registration{authorizing_id=AuthorizingId
                           ,account_id=AccountId
                           ,authorizing_type=AuthorizingType
                           ,account_db=AccountDb
                           ,suppress_unregister=SuppressUnregister
                           ,owner_id=OwnerId
                           ,id=Id
                          }=Reg) ->
    Props = [{#registration.account_id, AccountId}
             ,{#registration.account_db, AccountDb}
             ,{#registration.authorizing_id, AuthorizingId}
             ,{#registration.authorizing_type, AuthorizingType}
             ,{#registration.owner_id, OwnerId}
             ,{#registration.suppress_unregister, SuppressUnregister}
            ],
    gen_server:cast(?MODULE, {'update_registration', Id, Props}),
    Reg.

-spec maybe_send_register_notice(registration()) -> 'ok'.
maybe_send_register_notice(#registration{username=Username
                                         ,realm=Realm
                                        }=Reg) ->
    case oldest_registrar(Username, Realm) of
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

-spec send_deregister_notice(registration()) -> 'ok'.
send_deregister_notice(Reg) ->
    Props = to_props(Reg)
        ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION),
    wapi_notifications:publish_deregister(Props).

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
     ,{<<"Expires">>, Reg#registration.expires}
     ,{<<"Account-ID">>, Reg#registration.account_id}
     ,{<<"Account-DB">>, Reg#registration.account_db}
     ,{<<"Authorizing-ID">>, Reg#registration.authorizing_id}
     ,{<<"Authorizing-Type">>, Reg#registration.authorizing_type}
     ,{<<"Suppress-Unregister-Notify">>, Reg#registration.suppress_unregister}
     ,{<<"Owner-ID">>, Reg#registration.owner_id}
    ].

-spec filter(wh_json:object(), wh_json:object()) -> wh_json:object().
filter([], JObj) -> JObj;
filter(Fields, JObj) ->
    wh_json:from_list(lists:foldl(fun(F, Acc) ->
                                          [ {F, wh_json:get_value(F, JObj)} | Acc]
                                  end, [], Fields)).

-spec oldest_registrar(ne_binary(), ne_binary()) -> boolean().
oldest_registrar(Username, Realm) ->
    Reg = [{<<"Username">>, Username}
           ,{<<"Realm">>, Realm}
           ,{<<"Fields">>, [<<"Expires">>]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_amqp_worker:call_collect(?ECALLMGR_AMQP_POOL
                                     ,Reg
                                     ,fun wapi_registration:publish_query_req/1
                                     ,'ecallmgr'
                                     ,2000)
    of
        {'ok', JObjs} ->
            case
                [wh_json:get_integer_value(<<"Registrar-Age">>, JObj, 0)
                 || JObj <- JObjs
                ]
            of
                [] -> 'true';
                Ages -> lists:max(Ages) =< gen_server:call(?MODULE, 'registrar_age')
            end;
        _Else -> 'true'
    end.

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
    _ = case re:run(Contact, ?SUMMARY_REGEX, [{'capture', 'all_but_first', 'binary'}]) of
            {'match', [Host, Path]} ->
                io:format("| ~-45s | ~-22s | ~-22s | ~-32s | ~-4B |~n"
                          ,[User, Host, Path, CallId, Remaining]);
            {'match', [Host]} ->
                io:format("| ~-45s | ~-22s | ~-22s | ~-32s | ~-4B |~n"
                          ,[User, Host, <<>>, CallId, Remaining]);
            _Else -> 'ok'
        end,
    print_summary(ets:select(Continuation), Count + 1).

print_details('$end_of_table') ->
    io:format("No registrations found!~n", []);
print_details(Match) ->
    print_details(Match, 0).

print_details('$end_of_table', Count) ->
    io:format("~nFound ~p registrations~n", [Count]);
print_details({[#registration{}=Reg]
               ,Continuation}
              ,Count) ->
    io:format("~n"),
    _ = [io:format("~-19s: ~s~n", [K, wh_util:to_binary(V)])
         || {K, V} <- to_props(Reg)
        ],
    print_details(ets:select(Continuation), Count + 1).
