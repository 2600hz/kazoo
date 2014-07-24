%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(omnip_subscriptions).

-behaviour(gen_server).

-export([start_link/0
         ,handle_search_req/2
         ,handle_reset/2
         ,handle_subscribe/2
         ,handle_kamailio_subscribe/2
         ,handle_mwi_update/2
         ,handle_presence_update/2
         ,handle_channel_event/2
         ,table_id/0
         ,table_config/0
         ,find_subscription/1
         ,find_subscriptions/1
         ,find_subscriptions/2
         ,search_for_subscriptions/2
         ,search_for_subscriptions/3
         ,subscription_to_json/1
         ,subscriptions_to_json/1
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("omnipresence.hrl").
-include_lib("kazoo_etsmgr/include/kazoo_etsmgr.hrl").

-define(EXPIRE_SUBSCRIPTIONS, whapps_config:get_integer(?CONFIG_CAT, <<"expire_check_ms">>, 1000)).
-define(EXPIRE_MESSAGE, 'clear_expired').
-define(BLF_EVENT, <<"dialog">>).
-define(MWI_EVENT, <<"message-summary">>).
-define(PRESENCE_EVENT, <<"presence">>).
-define(DEFAULT_EVENT, ?BLF_EVENT).
-define(DEFAULT_SEND_EVENT_LIST, [?BLF_EVENT, ?PRESENCE_EVENT]).

-record(state, {
          expire_ref :: reference()
         }).

-record(omnip_subscription, {
          user                                  :: api_binary() | '_' %% user@realm.com
          ,from                                 :: api_binary() | <<>> | '_' %% user@realm.com
          ,stalker                              :: api_binary() | '_' % amqp queue to publish updates to
          ,expires = 0                          :: non_neg_integer() | '_' | '$2'
          ,timestamp = wh_util:current_tstamp() :: non_neg_integer() | '_' | '$1'
          ,protocol = <<"sip">>                 :: ne_binary() | '_' % protocol
          ,username                             :: api_binary() | '_'
          ,realm                                :: api_binary() | '_'
          ,normalized_user                      :: api_binary() | '_' | '$1'
          ,normalized_from                      :: api_binary() | '_'
          ,event                                :: api_binary() | '_'
          ,from_tag                             :: api_binary() | '_'
          ,to_tag                               :: api_binary() | '_'
          ,contact                              :: api_binary() | '_'
          ,call_id                              :: api_binary() | '_'
         }).
-type subscription() :: #omnip_subscription{}.
-type subscriptions() :: [subscription(),...] | [].

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
    gen_server:start_link(?MODULE, [], []).

-spec handle_search_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_search_req(JObj, _Props) ->
    'true' = wapi_presence:search_req_v(JObj),
    Username = wh_json:get_value(<<"Username">>, JObj, '_'),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    lager:debug("searching for subs for ~p@~s", [Username, Realm]),
    case search_for_subscriptions('_', Realm, Username) of
        [] -> 'ok';
        Subs ->
            Resp = [{<<"Subscriptions">>, subscriptions_to_json(Subs)}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_presence:publish_search_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

-spec handle_reset(wh_json:object(), wh_proplist()) -> 'ok'.
handle_reset(JObj, _Props) ->
    'true' = wapi_presence:reset_v(JObj),
    case find_subscriptions(JObj) of
        {'error', 'not_found'} -> 'ok';
        {'ok', Subscriptions} -> publish_flush(Subscriptions)
    end.

%% Subscribes work like this:
%%   Subscribe comes into shared queue, gets round-robined to next omni whapp
%%   Handling whapp then publishes an internal whapp msg to all other omni whapps
%%   Handling whapp then publishes a status update to the subscribe Queue
%%
%% handle_subscribe_only processes subscribes received on the whapp's dedicated
%% queue, without the lookup of the current state
-spec handle_subscribe(wh_json:object(), wh_proplist()) -> 'ok'.
handle_subscribe(JObj, Props) ->
    'true' = wapi_presence:subscribe_v(JObj),
    case wh_json:get_value(<<"Node">>, JObj) =:= wh_util:to_binary(node()) of
        'true' -> 'ok';
        'false' ->
            gen_listener:call(props:get_value(?MODULE, Props)
                              ,{'subscribe', subscribe_to_record(JObj)}
                             )
    end.

-spec handle_kamailio_subscribe(wh_json:object(), wh_proplist()) -> 'ok'.
handle_kamailio_subscribe(JObj, Props) ->
    'true' = wapi_omnipresence:subscribe_v(JObj),
    case gen_listener:call(props:get_value(?MODULE, Props)
                           ,{'subscribe', subscribe_to_record(JObj)}
                          )
    of
        'invalid' -> 'ok';
        {'unsubscribe', _} -> distribute_subscribe(JObj);
        {'resubscribe', Subscription} ->
            _ = resubscribe_notify(Subscription),
            distribute_subscribe(JObj);
        {'subscribe', Subscription} ->
            _ = subscription_initial_notify(Subscription),
            distribute_subscribe(JObj)
    end.

-spec handle_mwi_update(wh_json:object(), wh_proplist()) -> any().
handle_mwi_update(JObj, _Props) ->
    'true' = wapi_presence:mwi_update_v(JObj),
    maybe_send_mwi_update(JObj).

-spec handle_presence_update(wh_json:object(), wh_proplist()) -> 'ok'.
handle_presence_update(JObj, _Props) ->
    'true' = wapi_presence:update_v(JObj),
    maybe_send_update(JObj, wh_json:get_value(<<"State">>, JObj)).

-spec handle_channel_event(ne_binary(), wh_json:object()) -> 'ok'.
handle_channel_event(<<"CHANNEL_CREATE">>, JObj) -> handle_new_channel(JObj);
handle_channel_event(<<"CHANNEL_ANSWER">>, JObj) -> handle_answered_channel(JObj);
handle_channel_event(<<"CHANNEL_DESTROY">>, JObj) -> handle_destroyed_channel(JObj);
handle_channel_event(<<"CHANNEL_CONNECTED">>, JObj) -> handle_connected_channel(JObj);
handle_channel_event(<<"CHANNEL_DISCONNECTED">>, JObj) -> handle_disconnected_channel(JObj).

-spec table_id() -> 'omnipresence_subscriptions'.
table_id() -> 'omnipresence_subscriptions'.

-spec table_config() -> wh_proplist().
table_config() ->
    ['protected', 'named_table', 'bag'
     ,{'keypos', #omnip_subscription.user}
    ].

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
    {'ok', #state{expire_ref=start_expire_ref()}}.

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
handle_call({'subscribe', #omnip_subscription{from = <<>>}=_S}, _From, State) ->
    lager:debug("subscription with no from: ~p", [subscription_to_json(_S)]),
    {'reply', 'invalid', State};
handle_call({'subscribe', #omnip_subscription{expires=E
                                              ,user=_U
                                              ,from=_F
                                              ,stalker=_S
                                             }=S}
            ,_From, State) when E =< 0 ->
    case find_subscription(S) of
        {'error', 'not_found'} -> {'reply', {'unsubscribe', S}, State};
        {'ok', #omnip_subscription{timestamp=_T
                                   ,expires=_E
                                   }=O} ->
            lager:debug("unsubscribe ~s/~s (had ~p s left)", [_U, _F, _E - wh_util:elapsed_s(_T)]),
            ets:delete_object(table_id(), O),
            {'reply', {'unsubscribe', O}, State}
    end;
handle_call({'subscribe', #omnip_subscription{user=_U
                                              ,from=_F
                                              ,expires=_E1
                                              ,stalker=_S
                                             }=S}
            ,_From, State) ->
    case find_subscription(S) of
        {'ok', #omnip_subscription{timestamp=_T
                                   ,expires=_E2
                                  }=O} ->
            lager:debug("re-subscribe ~s/~s expires in ~ps(prior remaing ~ps)"
                        ,[_U, _F, _E1, _E2 - wh_util:elapsed_s(_T)]),
            ets:delete_object(table_id(), O),
            ets:insert(table_id(), S),
            {'reply', {'resubscribe', S}, State};
        {'error', 'not_found'} ->
            lager:debug("subscribe ~s/~s expires in ~ps", [_U, _F, _E1]),
            ets:insert(table_id(), S),
            {'reply', {'subscribe', S}, State}
    end;
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
handle_info({'timeout', Ref, ?EXPIRE_MESSAGE}=_R, #state{expire_ref=Ref}=State) ->
    case expire_old_subscriptions() of
        0 -> 'ok';
        _N -> lager:debug("expired ~p subscriptions", [_N])
    end,
    {'noreply', State#state{expire_ref=start_expire_ref()}};
handle_info(?TABLE_READY(_Tbl), State) ->
    lager:debug("recv table_ready for ~p", [_Tbl]),
    {'noreply', State, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec distribute_subscribe(wh_json:object()) -> 'ok'.
distribute_subscribe(_JObj) -> 'ok'.
%% TODO: Kamailio sends subscriptions to a fanout
%%  meaning they will not round-robin.  Once the
%%  exchange type is updated this can be uncommented.
%%    whapps_util:amqp_pool_send(
%%      wh_json:delete_key(<<"Node">>, JObj)
%%      ,fun wapi_presence:publish_subscribe/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe_to_record(wh_json:object()) -> subscription().
subscribe_to_record(JObj) ->
    {P, U, [Username, Realm]} = omnip_util:extract_user(wh_json:get_value(<<"User">>, JObj)),
    {P, F, _} = omnip_util:extract_user(wh_json:get_value(<<"From">>, JObj, <<>>)),
    #omnip_subscription{user=U
                        ,from=F
                        ,protocol=P
                        ,expires=expires(JObj)
                        ,normalized_user=wh_util:to_lower_binary(U)
                        ,normalized_from=wh_util:to_lower_binary(F)
                        ,username=wh_util:to_lower_binary(Username)
                        ,realm=wh_util:to_lower_binary(Realm)
                        ,stalker=wh_json:get_first_defined([<<"Queue">>
                                                            ,<<"Server-ID">>
                                                           ], JObj)
                        ,event=wh_json:get_value(<<"Event-Package">>, JObj, ?DEFAULT_EVENT)
                        ,from_tag=wh_json:get_value(<<"From-Tag">>, JObj)
                        ,to_tag=wh_json:get_value(<<"To-Tag">>, JObj)
                        ,contact=wh_json:get_value(<<"Contact">>, JObj)
                        ,call_id=wh_json:get_value(<<"Call-ID">>, JObj)
                       }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec subscriptions_to_json(subscriptions()) -> wh_json:objects().
subscriptions_to_json(Subs) ->
    [subscription_to_json(S) || S <- Subs].

-spec subscription_to_json(subscription()) -> wh_json:object().
subscription_to_json(#omnip_subscription{user=User
                                         ,from=From
                                         ,stalker=Stalker
                                         ,expires=Expires
                                         ,timestamp=Timestamp
                                         ,protocol=Protocol
                                         ,username=Username
                                         ,realm=Realm
                                         ,event=Event
                                         ,from_tag=FromTag
                                         ,to_tag=ToTag
                                         ,contact=Contact
                                         ,call_id=CallId
                                        }) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"user">>, User}
         ,{<<"from">>, From}
         ,{<<"stalker">>, Stalker}
         ,{<<"expires">>, Expires}
         ,{<<"timestamp">>, Timestamp}
         ,{<<"protocol">>, Protocol}
         ,{<<"username">>, Username}
         ,{<<"realm">>, Realm}
         ,{<<"event">>, Event}
         ,{<<"from_tag">>, FromTag}
         ,{<<"to_tag">>, ToTag}
         ,{<<"contact">>, Contact}
         ,{<<"call_id">>, CallId}
        ])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_new_channel(wh_json:object()) -> 'ok'.
handle_new_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("received channel create, checking for subscribers"),
    maybe_send_update(JObj, ?PRESENCE_RINGING).

-spec handle_answered_channel(wh_json:object()) -> any().
handle_answered_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("received channel answer, checking for subscribers"),
    maybe_send_update(JObj, ?PRESENCE_ANSWERED).

-spec handle_destroyed_channel(wh_json:object()) -> any().
handle_destroyed_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("received channel destroy, checking for subscribers"),
    maybe_send_update(JObj, ?PRESENCE_HANGUP),
    %% When multiple omnipresence instances are in multiple
    %% zones its possible (due to the round-robin) for the
    %% instance closest to rabbitmq to process the terminate
    %% before a confirm/early if both are sent immediately.
    timer:sleep(1000),
    maybe_send_update(JObj, ?PRESENCE_HANGUP).

-spec handle_disconnected_channel(wh_json:object()) -> any().
handle_disconnected_channel(JObj) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("channel has been disconnected, checking status of channel on the cluster"),
    handle_destroyed_channel(JObj).

-spec handle_connected_channel(wh_json:object()) -> 'ok'.
handle_connected_channel(_JObj) ->
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_send_update(wh_json:object(), ne_binary()) -> any().
maybe_send_update(JObj, State) ->
    maybe_send_update(JObj, State, ?DEFAULT_SEND_EVENT_LIST).

-spec maybe_send_update(wh_json:object(), ne_binary(), ne_binaries()) -> any().
maybe_send_update(JObj, State, Events) ->
    To = wh_json:get_first_defined([<<"To">>, <<"Presence-ID">>], JObj),
    From = wh_json:get_value(<<"From">>, JObj),
    Direction = wh_json:get_lower_binary(<<"Call-Direction">>, JObj),
    {User, Props} =
        case Direction =:= <<"inbound">> of
            'true' ->
                {From, props:filter_undefined(
                       [{<<"To">>, To}
                        ,{<<"From">>, From}
                        ,{<<"State">>, State}
                        ,{<<"Direction">>, Direction}
                        ,{<<"From-Tag">>, wh_json:get_value(<<"To-Tag">>, JObj, <<" ">>)}
                        ,{<<"To-Tag">>, wh_json:get_value(<<"From-Tag">>, JObj, <<" ">>)}
                        ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                        ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                       ])};
            'false' ->
                {To, props:filter_undefined(
                         [{<<"From">>, From}
                          ,{<<"To">>, To}
                          ,{<<"State">>, State}
                          ,{<<"Direction">>, Direction}
                          ,{<<"From-Tag">>, wh_json:get_value(<<"From-Tag">>, JObj, <<" ">>)}
                          ,{<<"To-Tag">>, wh_json:get_value(<<"To-Tag">>, JObj, <<" ">>)}
                          ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                          ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                          | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                         ])}
            end,
    send_updates(Events, User, Props, Direction).

-spec send_updates(list(), ne_binary(), wh_proplist(), ne_binary()) -> 'ok'.
send_updates([], _User, _Props, _Direction) -> 'ok';
send_updates([Event|Events], User, Props, Direction) ->
    case find_subscriptions(Event, User) of
        {'ok', Subs} ->
            [send_update(Props, Direction, S)
             || S <- Subs
            ];
        {'error', 'not_found'} ->
            lager:debug("no ~s subscriptions for ~s",[Event, User])
    end,
    send_updates(Events, User, Props, Direction).

-spec send_update(wh_proplist(), ne_binary(), subscription()) -> 'ok'.
send_update(Props, <<"inbound">>, #omnip_subscription{user=User}=Subscription) ->
    send_update([{<<"From">>, User} | Props], Subscription);
send_update(Props, _, #omnip_subscription{user=User}=Subscription) ->
    send_update([{<<"To">>, User} | Props], Subscription).

-spec send_update(wh_proplist(), subscription()) -> 'ok'.
send_update(Props, #omnip_subscription{stalker=Q
                                       ,protocol=Protocol
                                       ,from=F
                                       ,event=Event
                                      }) ->
    To = props:get_value(<<"To">>, Props),
    From = props:get_value(<<"From">>, Props, F),
    State = props:get_value(<<"State">>, Props),
    lager:debug("sending ~s update ~s/~s ~s to '~s'", [Event, To, From, State, Q]),
    Payload = props:filter_undefined(
                [{<<"To">>, <<Protocol/binary, ":", To/binary>>}
                 ,{<<"From">>, <<Protocol/binary, ":", From/binary>>}
                 ,{<<"Event-Package">>, Event}
                 | props:delete_keys([<<"To">>, <<"From">>], Props)
                ]),
    whapps_util:amqp_pool_send(Payload, fun(P) -> wapi_omnipresence:publish_update(Q, P) end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec subscription_initial_notify(subscription()) -> 'ok'.
subscription_initial_notify(#omnip_subscription{event = <<"message-summary">>
                                                ,username=Username
                                                ,realm=Realm
                                                ,call_id=CallId}) ->
    lager:debug("publishing message query for ~s@~s", [Username, Realm]),
    Query = [{<<"Username">>, Username}
             ,{<<"Realm">>, Realm}
             ,{<<"Call-ID">>, CallId}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wh_amqp_worker:cast(Query
                        ,fun wapi_presence:publish_mwi_query/1
                       );
subscription_initial_notify(#omnip_subscription{event=EventPackage
                                                ,username=Username
                                                ,realm=Realm
                                                ,call_id=CallId}) ->
    lager:debug("publishing presence probe for ~s@~s", [Username, Realm]),
    Query = [{<<"Username">>, Username}
             ,{<<"Realm">>, Realm}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Event-Package">>, EventPackage}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wh_amqp_worker:cast(Query
                        ,fun wapi_presence:publish_probe/1
                       ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resubscribe_notify(subscription()) -> 'ok'.
resubscribe_notify(#omnip_subscription{event = <<"message-summary">>}) -> 'ok';
resubscribe_notify(#omnip_subscription{event=EventPackage
                                       ,username=Username
                                       ,realm=Realm
                                       ,call_id=CallId}) ->
    lager:debug("publishing presence probe for ~s@~s", [Username, Realm]),
    Query = [{<<"Username">>, Username}
             ,{<<"Realm">>, Realm}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Event-Package">>, EventPackage}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    wh_amqp_worker:cast(Query
                        ,fun wapi_presence:publish_probe/1
                       ).

-spec start_expire_ref() -> reference().
start_expire_ref() ->
    erlang:start_timer(?EXPIRE_SUBSCRIPTIONS, self(), ?EXPIRE_MESSAGE).

-spec expire_old_subscriptions() -> integer().
expire_old_subscriptions() ->
    Now = wh_util:current_tstamp(),
    ets:select_delete(table_id(), [{#omnip_subscription{timestamp='$1'
                                                        ,expires='$2'
                                                        ,_='_'
                                                       }
                                    ,[{'>', {'const', Now}, {'+', '$1', '$2'}}]
                                    ,['true']
                                   }]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_subscription(subscription()) ->
                               {'ok', subscription()} |
                               {'error', 'not_found'}.
find_subscription(#omnip_subscription{normalized_user=U
                                      ,normalized_from=F
                                      ,stalker=S
                                      ,event=Event
                                     }) ->
    MatchSpec = #omnip_subscription{normalized_user=U
                                    ,normalized_from=F
                                    ,stalker=S
                                    ,event=Event
                                    ,_='_'
                                   },
    case ets:match_object(table_id(), MatchSpec) of
        [] -> {'error', 'not_found'};
        [#omnip_subscription{}=Sub] -> {'ok', Sub};
        Subs ->
            {#omnip_subscription{}=Sub, _} =
                lists:foldl(fun(#omnip_subscription{timestamp=T}=SubT, {_, Tc}=Acc) ->
                                    case T > Tc of
                                        'true' -> {SubT, T};
                                        'false' -> Acc
                                    end
                            end, {'ok', 0}, Subs),
            {'ok', Sub}
    end.

-spec find_subscriptions(ne_binary() | wh_json:object()) ->
                                {'ok', subscriptions()} |
                                {'error', 'not_found'}.
-spec find_subscriptions(ne_binary(), ne_binary()) ->
                                {'ok', subscriptions()} |
                                {'error', 'not_found'}.
find_subscriptions(User) when is_binary(User) ->
    find_subscriptions(?DEFAULT_EVENT, User);
find_subscriptions(JObj) ->
    find_subscriptions(
      wh_json:get_value(<<"Event-Package">>, JObj, ?DEFAULT_EVENT),
      <<(wh_json:get_value(<<"Username">>, JObj))/binary
        ,"@"
        ,(wh_json:get_value(<<"Realm">>, JObj))/binary
      >>).

find_subscriptions(Event, User) when is_binary(User) ->
    U = wh_util:to_lower_binary(User),
    MatchSpec = [{#omnip_subscription{normalized_user='$1'
                                      ,event=Event
                                      ,_='_'
                                     }
                  ,[{'=:=', '$1', {'const', U}}]
                  ,['$_']
                 }],
    case ets:select(table_id(), MatchSpec) of
        [] -> {'error', 'not_found'};
        Subs -> {'ok', dedup(Subs)}
    end.

-spec dedup(subscriptions()) -> subscriptions().
dedup(Subscriptions) ->
    dedup(Subscriptions, dict:new()).

-spec dedup(subscriptions(), dict()) -> subscriptions().
dedup([], Dictionary) ->
    [Subscription || {_, Subscription} <- dict:to_list(Dictionary)];
dedup([#omnip_subscription{normalized_user=User
                           ,stalker=Stalker
                          ,event=Event
                          }=Subscription
       | Subscriptions
      ], Dictionary) ->
    D = dict:store({User, Stalker, Event}, Subscription, Dictionary),
    dedup(Subscriptions, D).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_send_mwi_update(wh_json:object()) -> 'ok'.
maybe_send_mwi_update(JObj) ->
    To = wh_json:get_value(<<"To">>, JObj),
    MessagesNew = wh_json:get_integer_value(<<"Messages-New">>, JObj, 0),
    MessagesSaved = wh_json:get_integer_value(<<"Messages-Waiting">>, JObj, 0),
    MessagesUrgent = wh_json:get_integer_value(<<"Messages-Urgent">>, JObj, 0),
    MessagesUrgentSaved = wh_json:get_integer_value(<<"Messages-Urgent-Waiting">>, JObj, 0),
    MessagesWaiting = case MessagesNew of 0 -> <<"no">>; _ -> <<"yes">> end,
    Update = props:filter_undefined(
               [{<<"To">>, To}
                ,{<<"From">>, To}
                ,{<<"Messages-Waiting">>, MessagesWaiting}
                ,{<<"Messages-New">>, MessagesNew}
                ,{<<"Messages-Saved">>, MessagesSaved}
                ,{<<"Messages-Urgent">>, MessagesUrgent}
                ,{<<"Messages-Urgent-Saved">>, MessagesUrgentSaved}
                ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    case find_subscriptions(?MWI_EVENT, To) of
        {'error', 'not_found'} -> lager:debug("no subs for ~s", [To]);
        {'ok', Subs} ->
            _ = [send_mwi_update(Update, S) || S <- Subs],
            'ok'
    end.

-spec send_mwi_update(wh_proplist(), subscription()) -> 'ok'.
send_mwi_update(Update, #omnip_subscription{stalker=S
                                            ,protocol=Proto
                                            ,from=F
                                            ,call_id=CallID
                                            ,from_tag=FromTag
                                            ,to_tag=ToTag
                                            ,event=Event
                                           }) ->
    To = props:get_value(<<"To">>, Update),
    From = props:get_value(<<"From">>, Update, F),

    lager:debug("sending mwi update for '~s' from '~s' to '~s'", [To, From, S]),

    whapps_util:amqp_pool_send([{<<"To">>, <<Proto/binary, ":", To/binary>>}
                                ,{<<"From">>, <<Proto/binary, ":", From/binary>>}
                                ,{<<"Call-ID">>, <<CallID/binary>>}
                                ,{<<"Event-Package">>, Event}
                                ,{<<"From-Tag">>, <<FromTag/binary>>}
                                ,{<<"To-Tag">>, <<ToTag/binary>>}
                                | props:delete_keys([<<"To">>, <<"From">>], Update)
                               ]
                               ,fun(P) -> wapi_omnipresence:publish_update(S, P) end
                              ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec expires(integer() | wh_json:object()) -> non_neg_integer().
expires(I) when is_integer(I), I >= 0 -> I;
expires(JObj) -> expires(wh_json:get_integer_value(<<"Expires">>, JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec search_for_subscriptions(ne_binary() | '_', ne_binary()) -> subscriptions().
-spec search_for_subscriptions(ne_binary() | '_', ne_binary(), ne_binary() | '_') -> subscriptions().
search_for_subscriptions(Event, Realm) ->
    MatchSpec =
        #omnip_subscription{realm=wh_util:to_lower_binary(Realm)
                            ,event=Event
                            ,_='_'
                           },
    ets:match_object(table_id(), MatchSpec).

search_for_subscriptions(Event, Realm, Username) ->
    MatchSpec =
        #omnip_subscription{username=wh_util:to_lower_binary(Username)
                            ,realm=wh_util:to_lower_binary(Realm)
                            ,event=Event
                            ,_='_'
                           },
    ets:match_object(table_id(), MatchSpec).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_flush(subscriptions()) -> 'ok'.
publish_flush([]) -> 'ok';
publish_flush([#omnip_subscription{stalker=Q, user=User, event=Event}
               | Subscriptions
              ]) ->
    Props = [{<<"Type">>, <<"id">>}
             ,{<<"Event-Package">>, Event}
             ,{<<"User">>, <<"sip:", User/binary>>}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    lager:debug("sending flush for ~s to ~s", [User, Q]),
    whapps_util:amqp_pool_send(Props, fun(P) -> wapi_presence:publish_flush(Q, P) end),
    publish_flush(Subscriptions).
