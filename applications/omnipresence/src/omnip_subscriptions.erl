%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
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
         ,find_user_subscriptions/2
         ,get_subscriptions/2
         ,search_for_subscriptions/2
         ,search_for_subscriptions/3
         ,subscription_to_json/1
         ,subscriptions_to_json/1
         ,cached_terminated_callids/0
         ,handle_sync/2
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
-define(EXPIRES_FUDGE, whapps_config:get_integer(?CONFIG_CAT, <<"expires_fudge_s">>, 20)).
-define(EXPIRE_MESSAGE, 'clear_expired').
-define(DEFAULT_EVENT, ?BLF_EVENT).
-define(DEFAULT_SEND_EVENT_LIST, [?BLF_EVENT, ?PRESENCE_EVENT]).

-record(state, {
          expire_ref :: reference(),
          ready = 'false' :: boolean(),
          sync = 'false'  :: boolean(),
          sync_nodes = [] :: list()
         }).

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

-spec handle_search_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_search_req(JObj, _Props) ->
    'true' = wapi_presence:search_req_v(JObj),
    case wh_json:get_value(<<"Node">>, JObj) =:= wh_util:to_binary(node()) of
        'true' -> 'ok';
        'false' ->
            Event = wh_json:get_value(<<"Event-Package">>, JObj, '_'),
            Username = wh_json:get_value(<<"Username">>, JObj, '_'),
            Realm = wh_json:get_value(<<"Realm">>, JObj),
            lager:debug("searching for subs for ~s@~s", [Username, Realm]),
            case search_for_subscriptions(Event, Realm, Username) of
                [] -> 'ok';
                Subs ->
                    Resp = [{<<"Subscriptions">>, subscriptions_to_json(Subs)}
                            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                           ],
                    wapi_presence:publish_search_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
            end
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
handle_subscribe(JObj, _Props) ->
    'true' = wapi_presence:subscribe_v(JObj),
    case wh_json:get_value(<<"Node">>, JObj) =:= wh_util:to_binary(node()) of
        'true' -> 'ok';
        'false' ->
            gen_server:call(?MODULE, {'subscribe', subscribe_to_record(JObj)})
    end.

-spec handle_kamailio_subscribe(wh_json:object(), wh_proplist()) -> 'ok'.
handle_kamailio_subscribe(JObj, _Props) ->
    'true' = wapi_omnipresence:subscribe_v(JObj),
    case gen_server:call(?MODULE, {'subscribe', JObj}) of
        'invalid' -> 'ok';
        {'unsubscribe', _} ->
            distribute_subscribe(JObj);
        {'resubscribe', Subscription} ->
            _ = resubscribe_notify(Subscription),
            distribute_subscribe(JObj);
        {'subscribe', Subscription} ->
            _ = subscribe_notify(Subscription),
            distribute_subscribe(JObj)
    end.

-spec handle_sync(wh_json:object(), wh_proplist()) -> 'ok'.
handle_sync(JObj, _Props) ->
    'true' = wapi_presence:sync_v(JObj),
    Action = wh_json:get_value(<<"Action">>, JObj),
    Node = wh_json:get_value(<<"Node">>, JObj),
    gen_server:cast(?MODULE, {'sync', {Action, Node}}).

-spec handle_mwi_update(wh_json:object(), wh_proplist()) -> any().
handle_mwi_update(JObj, _Props) ->
    'true' = wapi_presence:mwi_update_v(JObj),
    gen_server:cast(?MODULE, {'mwi_update', JObj}).

-spec handle_presence_update(wh_json:object(), wh_proplist()) -> 'ok'.
handle_presence_update(JObj, _Props) ->
    'true' = wapi_presence:update_v(JObj),
    gen_server:cast(?MODULE, {'presence_update', JObj}).

-define(CACHE_TERMINATED_CALLID, whapps_config:get_integer(?CONFIG_CAT, <<"cache_terminated_callid_s">>, 60)).

-spec handle_channel_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_channel_event(JObj, _Props) ->
    EventName = wh_json:get_value(<<"Event-Name">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),

    wh_util:put_callid(CallId),

    maybe_handle_event(JObj, CallId, EventName).

-spec maybe_handle_event(wh_json:object(), ne_binary(), ne_binary()) -> 'ok'.
maybe_handle_event(JObj, CallId, <<"CHANNEL_DESTROY">>) ->
    lager:debug("caching CHANNEL_DESTROY for ~s", [CallId]),
    wh_cache:store_local(?CACHE_NAME
                         ,terminated_cache_key(CallId)
                         ,'terminated'
                         ,[{'expires', ?CACHE_TERMINATED_CALLID}]
                        ),
    handle_the_event(JObj);
maybe_handle_event(JObj, CallId, <<"CHANNEL_CREATE">> = _EventName) ->
    case wh_cache:fetch_local(?CACHE_NAME, terminated_cache_key(CallId)) of
        {'error', 'not_found'} -> handle_the_event(JObj);
        _Else -> lager:warning("received ~s but call is terminated already, dropping", [_EventName])
    end;
maybe_handle_event(JObj, CallId, <<"CHANNEL_ANSWER">> = _EventName) ->
    case wh_cache:fetch_local(?CACHE_NAME, terminated_cache_key(CallId)) of
        {'error', 'not_found'} -> handle_the_event(JObj);
        _Else -> lager:warning("received ~s but call is terminated already, dropping", [_EventName])
    end;
maybe_handle_event(JObj, _CallId, _EventName) ->
    handle_the_event(JObj).

-spec handle_the_event(wh_json:object()) -> 'ok'.
handle_the_event(JObj) ->
    gen_server:cast(?MODULE, {'channel_event', JObj}).

-spec terminated_cache_key(CallId) -> {'terminated', CallId}.
terminated_cache_key(CallId) ->
    {'terminated', CallId}.

-spec cached_terminated_callids() -> ne_binaries().
cached_terminated_callids() ->
    [CallId || {'terminated', CallId} <- wh_cache:fetch_keys_local(?CACHE_NAME)].

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
handle_call({'subscribe', #omnip_subscription{}=Sub}, _From, State) ->
    SubscribeResult = subscribe(Sub),
    {'reply', SubscribeResult, State};
handle_call({'subscribe', Props}, _From, State) when is_list(Props) ->
    handle_call({'subscribe', wh_json:from_list(Props)}, _From, State);
handle_call({'subscribe', JObj}, _From, State) ->
    handle_call({'subscribe', subscribe_to_record(JObj)}, _From, State);
handle_call(_Request, _From, State) ->
    lager:debug("omnipresence subscriptions unhandled call : ~p", [_Request]),
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

handle_cast({'distribute_subscribe', JObj}, State) ->
    distribute_subscribe(JObj),
    {'noreply', State};
handle_cast({'channel_event', JObj}, State) ->
    Msg = {'omnipresence', {'channel_event', JObj}},
    notify_packages(Msg),
    {'noreply', State};
handle_cast({'mwi_update', JObj}, State) ->
    Msg = {'omnipresence', {'mwi_update', JObj}},
    notify_packages(Msg),
    {'noreply', State};
handle_cast({'presence_update', JObj}, State) ->
    Msg = {'omnipresence', {'presence_update', JObj}},
    notify_packages(Msg),
    {'noreply', State};
handle_cast({'sync', {<<"Start">>, Node}}, #state{sync_nodes=Nodes}=State) ->
    {'noreply', State#state{sync_nodes=[Node | Nodes]}};
handle_cast({'sync', {<<"End">>, Node}}, #state{sync_nodes=Nodes} = State) ->
    {'noreply', State#state{sync_nodes=Nodes -- [Node]}};
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
handle_info({'timeout', Ref, ?EXPIRE_MESSAGE}=_R, #state{expire_ref=Ref, ready='true'}=State) ->
    case expire_old_subscriptions() of
        0 -> 'ok';
        _N -> lager:debug("expired ~p subscriptions", [_N])
    end,
    {'noreply', State#state{expire_ref=start_expire_ref()}};
handle_info({'timeout', Ref, ?EXPIRE_MESSAGE}=_R, #state{expire_ref=Ref, ready='false'}=State) ->
    {'noreply', State#state{expire_ref=start_expire_ref()}};
handle_info(?TABLE_READY(_Tbl), State) ->
    lager:debug("recv table_ready for ~p", [_Tbl]),
    {'noreply', State#state{ready='true'}, 'hibernate'};
handle_info('check_sync', #state{sync_nodes=[]} = State) ->
    omnipresence_shared_listener:start_listener(),
    {'noreply', State};
handle_info('check_sync', State) ->
    erlang:send_after(5000, self(), 'check_sync'),
    {'noreply', State};
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

-spec notify_packages(any()) -> 'ok'.
notify_packages(Msg) ->
    _ = [gen_server:cast(Pid, Msg)
         || {_, Pid, _, _} <- supervisor:which_children('omnip_sup'),
            Pid =/= 'restarting'
        ],
    'ok'.

-spec resubscribe_notify(subscription()) -> 'ok'.
resubscribe_notify(#omnip_subscription{event=Package
                                       ,user=User
                                      }=Subscription) ->
    Msg = {'omnipresence', {'resubscribe_notify', Package, User, Subscription}},
    notify_packages(Msg).

-spec subscribe_notify(subscription()) -> 'ok'.
subscribe_notify(#omnip_subscription{event=Package
                                     ,user=User
                                    }=Subscription) ->
    Msg = {'omnipresence', {'subscribe_notify', Package, User, Subscription}},
    notify_packages(Msg).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec distribute_subscribe(wh_json:object()) -> 'ok'.
distribute_subscribe(JObj) ->
    whapps_util:amqp_pool_send(
      wh_json:delete_key(<<"Node">>, JObj)
      ,fun wapi_presence:publish_subscribe/1
     ).

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
    Version = case wh_json:get_value(<<"Subscription-ID">>, JObj) of
                  'undefined' -> 1;
                  _Else -> 2
              end,
    #omnip_subscription{user=U
                        ,from=F
                        ,protocol=P
                        ,expires=expires(JObj)
                        ,normalized_user=wh_util:to_lower_binary(U)
                        ,normalized_from=wh_util:to_lower_binary(F)
                        ,username=wh_util:to_lower_binary(Username)
                        ,realm=wh_util:to_lower_binary(Realm)
                        ,stalker=wh_json:get_first_defined([<<"Subscription-ID">>
                                                            ,<<"Server-ID">>
                                                            ,<<"Queue">>
                                                           ], JObj)
                        ,event=wh_json:get_value(<<"Event-Package">>, JObj, ?DEFAULT_EVENT)
                        ,contact=wh_json:get_value(<<"Contact">>, JObj)
                        ,call_id=wh_json:get_value(<<"Call-ID">>, JObj)
                        ,subscription_id=wh_json:get_value(<<"Subscription-ID">>, JObj)
                        ,proxy_route= wh_json:get_value(<<"Proxy-Route">>, JObj)
                        ,version=Version
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
                                         ,contact=Contact
                                         ,call_id=CallId
                                         ,subscription_id=SubId
                                         ,proxy_route=ProxyRoute
                                         ,version=Version
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
         ,{<<"contact">>, Contact}
         ,{<<"call_id">>, CallId}
         ,{<<"subscription_id">>, SubId}
         ,{<<"proxy_route">>, ProxyRoute}
         ,{<<"version">>, Version}
        ])).

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
                                      ,call_id='undefined'
                                     }) ->
    match_subscription(#omnip_subscription{normalized_user=U
                                           ,normalized_from=F
                                           ,stalker=S
                                           ,event=Event
                                           ,_='_'
                                          });
find_subscription(#omnip_subscription{call_id=CallId}) ->
    match_subscription(#omnip_subscription{call_id=CallId
                                           ,_='_'
                                          }).

-spec match_subscription(subscription()) -> {'ok', subscription()} |
                                            {'error', 'not_found'}.
match_subscription(MatchSpec) ->
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
      wh_json:get_value(<<"Event-Package">>, JObj, ?DEFAULT_EVENT)
      ,<<(wh_json:get_value(<<"Username">>, JObj))/binary
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

-spec find_user_subscriptions(ne_binary(), ne_binary()) ->
                                     {'ok', subscriptions()} |
                                     {'error', 'not_found'}.
find_user_subscriptions(Event, User) when is_binary(User) ->
    U = wh_util:to_lower_binary(User),
    MatchSpec = [{#omnip_subscription{normalized_from='$1'
                                      ,event=Event
                                      ,_='_'
                                     }
                  ,[{'=:=', '$1', {'const', U}}]
                  ,['$_']
                 }],
    case ets:select(table_id(), MatchSpec) of
        [] -> {'error', 'not_found'};
        Subs -> {'ok', Subs}
    end.

-spec get_subscriptions(ne_binary(), ne_binary()) ->
                               {'ok', subscriptions()} |
                               {'error', 'not_found'}.
get_subscriptions(Event, User) ->
    U = wh_util:to_lower_binary(User),
    case find_subscriptions(Event, U) of
        {'ok', Subs} -> {'ok', Subs};
        {'error', _}=Err -> Err
    end.

-spec dedup(subscriptions()) -> subscriptions().
-spec dedup(subscriptions(), dict()) -> subscriptions().
dedup(Subscriptions) ->
    dedup(Subscriptions, dict:new()).

dedup([], Dictionary) ->
    [Subscription || {_, Subscription} <- dict:to_list(Dictionary)];
dedup([#omnip_subscription{normalized_user=User
                           ,stalker=Stalker
                           ,event=Event
                          }=Subscription
       | Subscriptions
      ], Dictionary) ->
    dedup(Subscriptions
          ,dict:store({User, Stalker, Event}, Subscription, Dictionary)
         ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec expires(integer() | wh_json:object()) -> non_neg_integer().
expires(0) -> 0;
expires(I) when is_integer(I), I >= 0 -> I + ?EXPIRES_FUDGE;
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

-spec subscribe(subscription()) -> 'invalid' |
                                   {'subscribe', subscription()} |
                                   {'resubscribe', subscription()} |
                                   {'unsubscribe', subscription()}.
subscribe(#omnip_subscription{from = <<>>}=_S) ->
    lager:debug("subscription with no from: ~p", [subscription_to_json(_S)]),
    'invalid';
subscribe(#omnip_subscription{expires=E
                              ,user=_U
                              ,from=_F
                              ,stalker=_S
                             }=S)
  when E =< 0 ->
    case find_subscription(S) of
        {'error', 'not_found'} -> {'unsubscribe', S};
        {'ok', #omnip_subscription{timestamp=_T
                                   ,expires=_E
                                  }=O} ->
            lager:debug("unsubscribe ~s/~s (had ~p s left)", [_U, _F, _E - wh_util:elapsed_s(_T)]),
            ets:delete_object(table_id(), O),
            {'unsubscribe', O}
    end;
subscribe(#omnip_subscription{user=_U
                              ,from=_F
                              ,expires=E1
                              ,timestamp=T1
                              ,stalker=Stalker
                              ,call_id=CallId
                             }=S) ->
    case find_subscription(S) of
        {'ok', #omnip_subscription{timestamp=_T
                                   ,expires=_E2
                                  }=O
        } ->
            lager:debug("re-subscribe ~s/~s expires in ~ps(prior remaing ~ps)"
                        ,[_U, _F, E1, _E2 - wh_util:elapsed_s(_T)]
                       ),
            ets:delete_object(table_id(), O),
            ets:insert(table_id(), O#omnip_subscription{timestamp=T1
                                                        ,expires=E1
                                                        ,stalker=Stalker
                                                        ,call_id=CallId
                                                       }),
            {'resubscribe', O};
        {'error', 'not_found'} ->
            lager:debug("subscribe ~s/~s expires in ~ps", [_U, _F, E1]),
            ets:insert(table_id(), S),
            {'subscribe', S}
    end.
