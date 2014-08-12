%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnip_pkg_message_summary).

-behaviour(gen_listener).

-export([start_link/0
         ,handle_mwi_update/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("omnipresence.hrl").

-define(BINDINGS, [{'self', []}                 
                   ,{'presence', [{'restrict_to', ['mwi_update']}
                                  ,'federate'
                                 ]}

                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_mwi_update'}
                       ,[{<<"presence">>, <<"mwi_update">>}]
                      }                    
                    ]).
-define(QUEUE_NAME, <<"omnip_pkg_message_summary_shared_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-record(state, {}).

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
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

-spec handle_mwi_update(wh_json:object(), wh_proplist()) -> any().
handle_mwi_update(JObj, _Props) ->
    'true' = wapi_presence:mwi_update_v(JObj),
    handle_update(JObj).

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
init([]) ->
    put('callid', ?MODULE),
    ensure_template(),
    lager:debug("omnipresence message-summary listener started"),
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
handle_call(_Request, _From, State) ->
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

handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'omnipresence',{'subscribe_notify', <<"message-summary">>, User, _Subscription}}, State) ->
    [Username, Realm] = binary:split(User, <<"@">>),
    Query = [{<<"Username">>, Username}
             ,{<<"Realm">>, Realm}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wh_amqp_worker:cast(Query, fun wapi_presence:publish_mwi_query/1),    
    {'noreply', State};
handle_cast({'omnipresence',{'resubscribe_notify', <<"message-summary">>, User, _Subscription}}, State) ->
    [Username, Realm] = binary:split(User, <<"@">>),
    Query = [{<<"Username">>, Username}
             ,{<<"Realm">>, Realm}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wh_amqp_worker:cast(Query, fun wapi_presence:publish_mwi_query/1),    
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
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


-spec handle_update(wh_json:object()) -> any().
handle_update(JObj) ->
    lager:debug("HANDLE_MWI ~p", [JObj]),
    User = wh_json:get_value(<<"To">>, JObj),
    MessagesNew = wh_json:get_integer_value(<<"Messages-New">>, JObj, 0),
    MessagesSaved = wh_json:get_integer_value(<<"Messages-Waiting">>, JObj, 0),
    MessagesUrgent = wh_json:get_integer_value(<<"Messages-Urgent">>, JObj, 0),
    MessagesUrgentSaved = wh_json:get_integer_value(<<"Messages-Urgent-Waiting">>, JObj, 0),
    MessagesWaiting = case MessagesNew of 0 -> <<"no">>; _ -> <<"yes">> end,
    [Username, Realm] = binary:split(User, <<"@">>),
    Props = props:filter_undefined(
              [{<<"New">>, MessagesNew}
               ,{<<"Saved">>, MessagesSaved}
               ,{<<"Urgent">>, MessagesUrgent}
               ,{<<"Urgent-Saved">>, MessagesUrgentSaved}
               ,{<<"Waiting">>, MessagesWaiting}
               ,{<<"user">>, Username}
               ,{<<"realm">>, Realm}
              ]),
    maybe_send_update(User, Props).
    
-spec maybe_send_update(ne_binary(), wh_proplist()) -> 'ok'.   
maybe_send_update(User, Props) ->
    case omnip_subscriptions:find_subscriptions(?MWI_EVENT, User) of
        {'ok', Subs} ->
            send_update(User, Props, Subs);
        {'error', 'not_found'} ->
            lager:debug("no ~s subscriptions for ~s",[?MWI_EVENT, User])
    end.

-spec send_update(ne_binary(), wh_proplist(), subscriptions()) -> 'ok'.
send_update(User, Props, Subscriptions) ->
    Body = build_body(User, Props),
    Options = [{body, Body}
               ,{content_type, <<"application/simple-message-summary">>}
               ,{subscription_state, active}
               ],
    [nksip_uac:notify(SubscriptionId,
                      Options ++ [{contact, Contact},{route, [Proxy]}])
                    || #omnip_subscription{subscription_id=SubscriptionId
                                           ,contact=Contact
                                           ,proxy_route=Proxy} <- Subscriptions
                                                  , SubscriptionId =/= 'undefined'].


-spec normalize_variables(wh_proplist()) -> wh_proplist().
normalize_variables(Props) ->
    [{wh_json:normalize_key(K), V} || {K, V} <- Props ].


-spec build_variables(ne_binary(), wh_proplist()) -> ne_binary().
build_variables(_User, Props) ->
    normalize_variables(Props).
    

-spec build_body(ne_binary(), wh_proplist()) -> ne_binary().
build_body(User, Props) ->
    Variables = build_variables(User, Props),
    lager:debug("MSG_SUM Vars ~p", [Variables]),
    {'ok', Text} = sub_package_message_summary:render(Variables),
    Body = wh_util:to_binary(Text),
    binary:replace(Body, <<"\n">>, <<"\r\n">>, [global]).


ensure_template() ->
    BasePath = code:lib_dir(omnipresence, priv),
    File = lists:concat([BasePath, "/packages/message-summary.xml"]),
    Mod = wh_util:to_atom(<<"sub_package_message_summary">>, 'true'),
    {'ok', _CompileResult} = erlydtl:compile(File, Mod, []).
