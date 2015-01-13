%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnip_dialog).

-behaviour(gen_server).

-export([start_link/0
         ,reset_blf/1
         ,reset_user_blf/1
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
    put('callid', ?MODULE),
    ensure_template(),
    lager:debug("omnipresence event dialog package started"),
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
handle_cast({'omnipresence',{'subscribe_notify', <<"dialog">>, User, #omnip_subscription{}=_Subscription}}, State) ->
    spawn(fun() -> initial_update(User) end),
    {'noreply', State};
handle_cast({'omnipresence',{'resubscribe_notify', <<"dialog">>, User, #omnip_subscription{}=_Subscription}}, State) ->
    spawn(fun() -> initial_update(User) end),
    {'noreply', State};
handle_cast({'omnipresence',{'channel_event', JObj}}, State) ->
    EventType = wh_json:get_value(<<"Event-Name">>, JObj),
    spawn(fun() -> channel_event(EventType, JObj) end),
    {'noreply', State};
handle_cast({'omnipresence',{'presence_update', JObj}}, State) ->
    spawn(fun() -> presence_event(JObj) end),
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

-spec channel_event(ne_binary(), wh_json:object()) -> 'ok'.
channel_event(<<"CHANNEL_CREATE">>, JObj) -> handle_new_channel(JObj);
channel_event(<<"CHANNEL_ANSWER">>, JObj) -> handle_answered_channel(JObj);
channel_event(<<"CHANNEL_DESTROY">>, JObj) -> handle_destroyed_channel(JObj);
channel_event(<<"CHANNEL_CONNECTED">>, JObj) -> handle_connected_channel(JObj);
channel_event(<<"CHANNEL_DISCONNECTED">>, JObj) -> handle_disconnected_channel(JObj);
channel_event(_, _JObj) -> 'ok'.

-spec handle_new_channel(wh_json:object()) -> 'ok'.
handle_new_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("received channel create, checking for subscribers"),
    handle_update(JObj, ?PRESENCE_RINGING).

-spec handle_answered_channel(wh_json:object()) -> any().
handle_answered_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("received channel answer, checking for subscribers"),
    handle_update(JObj, ?PRESENCE_ANSWERED).

-spec handle_destroyed_channel(wh_json:object()) -> any().
handle_destroyed_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("received channel destroy, checking for subscribers"),
    handle_update(JObj, ?PRESENCE_HANGUP).

-spec handle_disconnected_channel(wh_json:object()) -> any().
handle_disconnected_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("channel has been disconnected, checking status of channel on the cluster"),
    handle_destroyed_channel(JObj).

-spec handle_connected_channel(wh_json:object()) -> 'ok'.
handle_connected_channel(_JObj) ->
    'ok'.

-spec initial_update(ne_binary()) -> any().
initial_update(User) ->
    Headers = [{<<"From">>, User}
               ,{<<"To">>, User}
               ,{<<"Call-ID">>, wh_util:rand_hex_binary(16)}
              ],
    handle_update(wh_json:from_list(Headers), ?PRESENCE_HANGUP).

-spec presence_event(wh_json:object()) -> 'ok'.
presence_event(JObj) ->
    State = wh_json:get_value(<<"State">>, JObj),
    maybe_handle_presence_state(JObj, State).

-spec maybe_handle_presence_state(wh_json:object(), api_binary()) -> 'ok'.
maybe_handle_presence_state(_JObj, <<"online">>) -> 'ok';
maybe_handle_presence_state(_JObj, <<"offline">>) -> 'ok';
maybe_handle_presence_state(JObj, ?PRESENCE_HANGUP=State) ->
    handle_update(JObj, State, 1);
maybe_handle_presence_state(JObj, ?PRESENCE_RINGING=State) ->
    handle_update(JObj, State, 1);
maybe_handle_presence_state(JObj, State) ->
    handle_update(JObj, State, 1).


-spec handle_update(wh_json:object(), ne_binary()) -> any().
handle_update(JObj, ?PRESENCE_HANGUP) ->
    handle_update(JObj, ?PRESENCE_HANGUP, 1);
handle_update(JObj, ?PRESENCE_RINGING) ->
    handle_update(JObj, ?PRESENCE_RINGING, 120);
handle_update(JObj, ?PRESENCE_ANSWERED) ->
    handle_update(JObj, ?PRESENCE_ANSWERED, 36000);
handle_update(_JObj, _State) -> 'ok'.

-spec handle_update(wh_json:object(), ne_binary(), integer()) -> 'ok'.
handle_update(JObj, State, Expires) ->
    To = wh_json:get_first_defined([<<"To">>, <<"Presence-ID">>], JObj),
    From = wh_json:get_first_defined([<<"From">>, <<"Presence-ID">>], JObj),
    [ToUsername, ToRealm] = binary:split(To, <<"@">>),
    [FromUsername, FromRealm] = binary:split(From, <<"@">>),
    Direction = wh_json:get_lower_binary(<<"Call-Direction">>, JObj),
    ToURI = case {State, wh_json:get_value(<<"App-Name">>, JObj)} of
                {?PRESENCE_RINGING, <<"park">>} ->
                    <<"sip:", From/binary,";kazoo-pickup=true">>;
                {_State, _App} ->
                    <<"sip:", From/binary>>
            end,

    {User, Props} =
        case Direction =:= <<"inbound">> of
            'true' ->
                {From, props:filter_undefined(
                         [{<<"From">>, <<"sip:", From/binary>>}
                          ,{<<"From-User">>, FromUsername}
                          ,{<<"From-Realm">>, FromRealm}
                          ,{<<"From-Tag">>, wh_json:get_value(<<"From-Tag">>, JObj)}
                          ,{<<"To">>, <<"sip:", To/binary>>}
                          ,{<<"To-User">>, ToUsername}
                          ,{<<"To-Realm">>, ToRealm}
                          ,{<<"To-Tag">>, wh_json:get_value(<<"To-Tag">>, JObj)}
                          ,{<<"State">>, State}
                          ,{<<"Expires">>, Expires}
                          ,{<<"Flush-Level">>, wh_json:get_value(<<"Flush-Level">>, JObj)}
                          ,{<<"Direction">>, <<"initiator">>}
                          ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj, ?FAKE_CALLID(From))}
                          ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                          ,{<<"Event-Package">>, <<"dialog">>}
                          ,{<<"destination">>, ToUsername}
                          ,{<<"uuid">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                          ,{<<"user">>, FromUsername}
                          ,{<<"realm">>, FromRealm}
                          | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                         ])};
            'false' ->
                {To, props:filter_undefined(
                       [{<<"From">>, <<"sip:", To/binary>>}
                        ,{<<"From-User">>, ToUsername}
                        ,{<<"From-Realm">>, ToRealm}
                        ,{<<"From-Tag">>, wh_json:get_value(<<"To-Tag">>, JObj)}
                        ,{<<"To">>, ToURI}
                        ,{<<"To-URI">>, ToURI}
                        ,{<<"To-User">>, FromUsername}
                        ,{<<"To-Realm">>, FromRealm}
                        ,{<<"To-Tag">>, wh_json:get_value(<<"From-Tag">>, JObj)}
                        ,{<<"State">>, State}
                        ,{<<"Expires">>, Expires}
                        ,{<<"Flush-Level">>, wh_json:get_value(<<"Flush-Level">>, JObj)}
                        ,{<<"Direction">>, <<"recipient">>}
                        ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj, ?FAKE_CALLID(To))}
                        ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                        ,{<<"Event-Package">>, <<"dialog">>}
                        ,{<<"destination">>, FromUsername}
                        ,{<<"uuid">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                        ,{<<"user">>, ToUsername}
                        ,{<<"realm">>, ToRealm}
                        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                       ])}
        end,
    maybe_send_update(User, Props).

-spec maybe_send_update(ne_binary(), wh_proplist()) -> 'ok'.
maybe_send_update(User, Props) ->
    case omnip_subscriptions:get_subscriptions(?DIALOG_EVENT, User) of
        {'ok', Subs} ->
            send_update(User, Props, Subs);
        {'error', 'not_found'} ->
            lager:debug("no ~s subscriptions for ~s",[?DIALOG_EVENT, User])
    end.

-spec send_update(ne_binary(), wh_proplist(), subscriptions()) -> 'ok'.
send_update(User, Props, Subscriptions) ->
    {Amqp, Sip} = lists:partition(fun(#omnip_subscription{version=V})-> V =:= 1 end, Subscriptions),
    send_update(<<"amqp">>, User, Props, Amqp),
    send_update(<<"sip">>, User, Props, Sip).

-spec send_update(ne_binary(), ne_binary(), wh_proplist(), subscriptions()) -> 'ok'.
send_update(_, _User, _Props, []) -> 'ok';
send_update(<<"amqp">>, _User, Props, Subscriptions) ->
    Stalkers = lists:usort([St || #omnip_subscription{stalker=St} <- Subscriptions]),
    [whapps_util:amqp_pool_send(Props
                               ,fun(P) -> wapi_omnipresence:publish_update(S, P) end
                              ) || S <- Stalkers];
send_update(<<"sip">>, User, Props, Subscriptions) ->
    Body = build_body(User, Props),
    Options = [{'body', Body}
               ,{'content_type', <<"application/dialog-info+xml">>}
               ,{'subscription_state', 'active'}
               ],
    [nksip_uac:notify(SubscriptionId
                      ,[{'contact', Contact}
                        ,{'route', [Proxy]}
                        | Options
                       ]
                     )
     || #omnip_subscription{subscription_id=SubscriptionId
                            ,contact=Contact
                            ,proxy_route=Proxy
                           } <- Subscriptions,
        SubscriptionId =/= 'undefined'
    ].

-spec get_user_channels(ne_binary()) -> list().
get_user_channels(User) ->
    [Username, Realm] = binary:split(User, <<"@">>),
    Payload = [{<<"Username">>, Username}
               ,{<<"Realm">>, Realm}
               ,{<<"Active-Only">>, 'false'}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Payload
                                       ,fun wapi_call:publish_query_user_channels_req/1
                                       ,fun wapi_call:query_user_channels_resp_v/1
                                      )
    of
        {'ok', Resp} ->
            case wh_json:get_value(<<"Channels">>, Resp) of
                'undefined' -> [];
                Channels ->  wh_json:to_proplist(Channels)
            end;
        {'error', _E} ->
            lager:debug("query user channels for ~s failed : ~p", [User, _E]),
            []
    end.

-spec map_state(ne_binary() | boolean()) -> ne_binary().
map_state('false') -> <<"early">>;
map_state('true') -> <<"confirmed">>;
map_state(Other) -> Other.

-spec map_direction(ne_binary()) -> ne_binary().
map_direction(<<"inbound">>) -> <<"initiator">>;
map_direction(<<"outbound">>) -> <<"recipient">>;
map_direction(Other) -> Other.

-spec props_to_call(wh_proplist()) -> channel() | 'undefined'.
props_to_call(Props) ->
    case props:get_value(<<"uuid">>, Props) of
        'undefined' -> 'undefined';
        UUID ->
            #channel{call_id = UUID
                     ,direction = map_direction(props:get_value(<<"direction">>, Props))
                     ,state = map_state(props:get_first_defined([<<"state">>, <<"answered">>], Props))
                     ,to = props:get_value(<<"destination">>, Props)
                    }
    end.

-spec build_channels(ne_binary(), wh_proplist()) -> wh_proplist().
build_channels(User, Props) ->
    Channels = [props_to_call(Channel) || Channel  <- get_user_channels(User)],
    case props_to_call(Props) of
        'undefined' -> Channels;
        UUID ->
            [UUID
             |[Channel
               || Channel <- Channels,
                  Channel#channel.call_id =/= UUID#channel.call_id
              ]
            ]
    end.

-spec build_variables(ne_binary(), wh_proplist()) -> wh_proplist().
build_variables(User, Props) ->
    case build_channels(User, Props) of
        [] -> omnip_util:normalize_variables(Props);
        Channels -> omnip_util:normalize_variables(props:set_value(<<"calls">>, Channels, Props))
    end.

-spec build_body(ne_binary(), wh_proplist()) -> ne_binary().
build_body(User, Props) ->
    Variables = build_variables(User, Props),
    {'ok', Text} = sub_package_dialog:render(Variables),
    Body = wh_util:to_binary(Text),
    binary:replace(Body, <<"\n\n">>, <<"\n">>, ['global']).

-spec ensure_template() -> {'ok', _}.
ensure_template() ->
    BasePath = code:lib_dir('omnipresence', 'priv'),
    File = lists:concat([BasePath, "/packages/dialog.xml"]),
    Mod = wh_util:to_atom(<<"sub_package_dialog">>, 'true'),
    {'ok', _CompileResult} = erlydtl:compile(File, Mod, [{'record_info', [{'channel', record_info('fields', 'channel')}]}]).

-spec reset_blf(ne_binary()) -> any().
reset_blf(User) ->
    Headers = [{<<"From">>, User}
               ,{<<"To">>, User}
               ,{<<"Flush-Level">>, 1}
               ,{<<"Call-ID">>, wh_util:to_hex_binary(crypto:md5(User))}
              ],
    handle_update(wh_json:from_list(Headers), ?PRESENCE_HANGUP).

-spec reset_user_blf(ne_binary()) -> any().
reset_user_blf(User) ->
    case omnip_subscriptions:find_user_subscriptions(?DIALOG_EVENT, User) of
        {'ok', Subs} ->
            [ reset_blf(SubUser) || #omnip_subscription{user=SubUser} <- Subs];
        _ -> 'ok'
    end.
