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
-export([reg_success/2]).
-export([reg_query/2]).
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
                    ]).
-define(BINDINGS, [{'registration', [{'retrict_to', ['reg_success', 'reg_query']}]}
                   ,{'self', []}
                  ]).
-define(SERVER, ?MODULE).
-define(REG_QUEUE_NAME, <<"">>).
-define(REG_QUEUE_OPTIONS, []).
-define(REG_CONSUME_OPTIONS, []).

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

registration_id(Username, Realm) ->
    {Username, Realm}.

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
    maybe_initial_registration(Registration).

reg_query(_, _) ->
    'ok'.

-spec lookup_contact(ne_binary(), ne_binary()) ->
                            {'ok', ne_binary()} |
                            {'error', 'not_found'}.
lookup_contact(Realm, Username) ->
    case ets:lookup(?MODULE, registration_id(Username, Realm)) of
        [#registration{contact=Contact}] -> {'ok', Contact};
        _Else -> {'error', 'not_found'}
    end.

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
    {'ok', []}.

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
handle_cast({'update_registration', Id, Props}, State) ->
    _ = ets:update_element(?MODULE, Id, Props),
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
                     ,contact=fix_contact(JObj)
                     ,last_registration=wh_util:current_tstamp()
                     ,registrar_node=wh_json:get_value(<<"Node">>, JObj)
                     ,registrar_hostname=wh_json:get_value(<<"Hostname">>, JObj)
                    }.
    
fix_contact(JObj) ->
    [User, AfterAt] = binary:split(wh_json:get_value(<<"Contact">>, JObj), <<"@">>), % only one @ allowed
    AfterUnquoted = wh_util:to_binary(mochiweb_util:unquote(AfterAt)),
    binary:replace(<<User/binary, "@", AfterUnquoted/binary>>
                   ,[<<"<">>, <<">">>]
                   ,<<>>
                   ,['global']).

existing_or_new_registration(Username, Realm) ->
    case ets:lookup(?MODULE, registration_id(Username, Realm)) of
        [#registration{}=Reg] -> Reg;
        _Else ->
            #registration{id=registration_id(Username, Realm)
                          ,initial_registration=wh_util:current_tstamp()
                         }
    end.

maybe_initial_registration(#registration{initial='false'}) ->
    'ok';
maybe_initial_registration(#registration{initial='true'}=Reg) ->
    initial_registration(Reg).

initial_registration(#registration{}=Reg) ->
    Routines = [fun maybe_query_authn/1
                ,fun update_cache/1
                ,fun send_new_notice/1
               ],
    _ = lists:foldl(fun(F, R) -> F(R) end, Reg, Routines),
    'ok'.

maybe_query_authn(#registration{username=Username, realm=Realm}=Reg) ->
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
                             ,suppress_unregister = wh_json:is_true(<<"Suppress-Unregister">>, CCVs)
                            }
    end.    

query_authn(#registration{username=Username, realm=Realm
                          ,to_user=ToUser, to_host=ToHost
                          ,from_user=FromUser, from_host=FromHost
                          ,network_ip=NetworkIP, registrar_node=Node
                          ,call_id=CallId}=Reg) ->
    lager:debug("looking up credentials of ~s@~s for a ~s", [Username, Realm]),
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
                             ,suppress_unregister = wh_json:is_true(<<"Suppress-Unregister">>, CCVs)
                            }
    end.

update_cache(#registration{authorizing_id=AuthorizingId, account_id=AccountId
                           ,authorizing_type=AuthorizingType, account_db=AccountDb
                           ,suppress_unregister=SuppressUnregister, owner_id=OwnerId
                           ,id=Id}=Reg) ->
    Props = [{#registration.account_id, AccountId}
             ,{#registration.account_db, AccountDb}
             ,{#registration.authorizing_id, AuthorizingId}
             ,{#registration.authorizing_type, AuthorizingType}
             ,{#registration.owner_id, OwnerId}
             ,{#registration.suppress_unregister, SuppressUnregister}
            ],
    gen_server:cast(?MODULE, {'update_registration', Id, Props}),
    Reg.

send_new_notice(Reg) ->
    Props = to_props(Reg) 
        ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION),
    wapi_notifications:publish_register(Props).

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
