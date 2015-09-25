%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnip_message_summary_sip).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("omnipresence.hrl").

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
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

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
    wh_util:put_callid(?MODULE),
    _ = ensure_template(),
    lager:debug("omnipresence event message-summary sip package started"),
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
handle_cast({'omnipresence',{'mwi_update', JObj}}, State) ->
    _ = wh_util:spawn(fun() -> mwi_event(JObj) end),
    {'noreply', State};
handle_cast({'omnipresence',{'presence_reset', JObj}}, State) ->
    _ = wh_util:spawn(fun() -> presence_reset(JObj) end),
    {'noreply', State};
handle_cast({'omnipresence', _}, State) ->
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
-spec mwi_event(wh_json:object()) -> 'ok'.
mwi_event(JObj) ->
    handle_update(JObj).

-spec handle_update(wh_json:object()) -> 'ok'.
handle_update(JObj) ->
    To = wh_json:get_value(<<"To">>, JObj),
    case omnip_util:is_valid_uri(To) of
        'true' -> handle_update(JObj, To);
        'false' -> lager:warning("mwi handler ignoring update from invalid To: ~s", [To])
    end.

-spec handle_update(wh_json:object(), ne_binary()) -> 'ok'.
handle_update(JObj, To) ->
    [ToUsername, ToRealm] = binary:split(To, <<"@">>),
    MessagesNew = wh_json:get_integer_value(<<"Messages-New">>, JObj, 0),
    MessagesSaved = wh_json:get_integer_value(<<"Messages-Waiting">>, JObj, 0),
    MessagesUrgent = wh_json:get_integer_value(<<"Messages-Urgent">>, JObj, 0),
    MessagesUrgentSaved = wh_json:get_integer_value(<<"Messages-Urgent-Waiting">>, JObj, 0),
    MessagesWaiting = case MessagesNew of 0 -> <<"no">>; _ -> <<"yes">> end,
    Update = props:filter_undefined(
               [{<<"To">>, <<"sip:", To/binary>>}
                ,{<<"To-User">>, ToUsername}
                ,{<<"To-Realm">>, ToRealm}
                ,{<<"From">>, <<"sip:", To/binary>>}
                ,{<<"From-User">>, ToUsername}
                ,{<<"From-Realm">>, ToRealm}
                ,{<<"Call-ID">>, ?FAKE_CALLID(To)}
                ,{<<"Message-Account">>, <<"sip:", To/binary>>}
                ,{<<"Messages-Waiting">>, MessagesWaiting}
                ,{<<"Messages-New">>, MessagesNew}
                ,{<<"Messages-Saved">>, MessagesSaved}
                ,{<<"Messages-Urgent">>, MessagesUrgent}
                ,{<<"Messages-Urgent-Saved">>, MessagesUrgentSaved}
                ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                ,{<<"Event-Package">>, <<"message-summary">>}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    maybe_send_update(To, Update).

-spec maybe_send_update(ne_binary(), wh_proplist()) -> 'ok'.
maybe_send_update(User, Props) ->
    case omnip_subscriptions:get_subscriptions(?MWI_EVENT, User, ?SUBSCRIPTION_SIP_VERSION) of
        {'ok', Subscriptions} ->
            send_update(User, Props, Subscriptions);
        {'error', 'not_found'} ->
            lager:debug("no version ~B ~s subscriptions for ~s",[?SUBSCRIPTION_SIP_VERSION, ?MWI_EVENT, User])
    end.

-spec send_update(ne_binary(), wh_proplist(), subscriptions()) -> 'ok'.
send_update(User, Props, Subscriptions) ->
    Body = build_body(User, Props),
    Options = [{'body', Body}
               ,{'content_type', <<"application/simple-message-summary">>}
               ,{'subscription_state', 'active'}
              ],
    _ = [nksip_uac:notify(SubscriptionId,
                      Options ++ [{'contact', Contact}
                                  ,{'route', [Proxy]}
                                 ]
                     )
     || #omnip_subscription{subscription_id=SubscriptionId
                            ,contact=Contact
                            ,proxy_route=Proxy
                           } <- Subscriptions,
        SubscriptionId =/= 'undefined'
    ],
    lager:debug("sent SIP mwi updates").

-spec build_variables(ne_binary(), wh_proplist()) -> wh_proplist().
build_variables(_User, Props) ->
    omnip_util:normalize_variables(Props).

-spec build_body(ne_binary(), wh_proplist()) -> ne_binary().
build_body(User, Props) ->
    Variables = build_variables(User, Props),
    Mod = wh_util:to_atom(<<"sub_package_message_summary">>, 'true'),
    {'ok', Text} = Mod:render(Variables),
    Body = wh_util:to_binary(Text),
    binary:replace(Body, <<"\n">>, <<"\r\n">>, ['global']).

-spec ensure_template() -> {'ok', any()}.
ensure_template() ->
    BasePath = code:lib_dir('omnipresence', 'priv'),
    File = lists:concat([BasePath, "/packages/message-summary.xml"]),
    Mod = wh_util:to_atom(<<"sub_package_message_summary">>, 'true'),
    {'ok', _CompileResult} = erlydtl:compile(File, Mod, []).

-spec presence_reset(wh_json:object()) -> any().
presence_reset(JObj) ->
    User = <<(wh_json:get_value(<<"Username">>, JObj))/binary, "@", (wh_json:get_value(<<"Realm">>, JObj))/binary>>,
    handle_update(wh_json:new(), User).
