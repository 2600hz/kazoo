%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
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
        ,handle_kamailio_notify/2
        ,handle_mwi_update/2
        ,handle_presence_update/2
        ,handle_channel_event/2
        ,table_id/0
        ,table_config/0
        ,find_subscription/1
        ,find_subscriptions/1
        ,find_subscriptions/2
        ,find_user_subscriptions/2
        ,get_subscriptions/2, get_subscriptions/3
        ,get_stalkers/2
        ,search_for_subscriptions/2
        ,search_for_subscriptions/3
        ,subscription_to_json/1
        ,subscriptions_to_json/1
        ,cached_terminated_callids/0
        ,handle_sync/2
        ,proxy_subscribe/1
        ,reset/1
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

-define(SERVER, ?MODULE).

-define(EXPIRE_SUBSCRIPTIONS, kapps_config:get_integer(?CONFIG_CAT, <<"expire_check_ms">>, ?MILLISECONDS_IN_SECOND)).
-define(EXPIRES_FUDGE, kapps_config:get_integer(?CONFIG_CAT, <<"expires_fudge_s">>, 20)).
-define(EXPIRE_MESSAGE, 'clear_expired').
-define(DEFAULT_EVENT, ?BLF_EVENT).
-define(DEFAULT_SEND_EVENT_LIST, [?BLF_EVENT, ?PRESENCE_EVENT]).

-record(state, {expire_ref :: reference()
               ,ready = 'false' :: boolean()
               ,sync = 'false'  :: boolean()
               ,sync_nodes = [] :: list()
               ,other_nodes_count = 0 :: integer()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec handle_search_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_search_req(JObj, _Props) ->
    Event = kz_json:get_value(<<"Event-Package">>, JObj, '_'),
    Username = kz_json:get_value(<<"Username">>, JObj, '_'),
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    lager:debug("searching for subs for ~s@~s", [Username, Realm]),
    Subs = search_for_subscriptions(Event, Realm, Username),
    Resp = [{<<"Subscriptions">>, subscriptions_to_json(Subs)}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_presence:publish_search_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec handle_reset(kz_json:object(), kz_proplist()) -> 'ok'.
handle_reset(JObj, _Props) ->
    'true' = kapi_presence:reset_v(JObj),
    reset(JObj).

-spec reset(kz_json:object()) -> 'ok'.
reset(JObj) ->
    notify_packages({'omnipresence', {'presence_reset', JObj}}).

%% Subscribes work like this:
%%   Subscribe comes into shared queue, gets round-robined to next omni whapp
%%   Handling whapp then publishes an internal whapp msg to all other omni kapps
%%   Handling whapp then publishes a status update to the subscribe Queue
%%
%% handle_subscribe_only processes subscribes received on the whapp's dedicated
%% queue, without the lookup of the current state
-spec handle_subscribe(kz_json:object(), kz_proplist()) -> 'ok'.
handle_subscribe(JObj, _Props) ->
    'true' = kapi_presence:subscribe_v(JObj),
    case kz_json:get_value(<<"Node">>, JObj) =:= kz_util:to_binary(node()) of
        'true' -> 'ok';
        'false' ->
            gen_server:call(?SERVER, {'subscribe', subscribe_to_record(JObj)})
    end.

-spec handle_kamailio_subscribe(kz_json:object(), kz_proplist()) -> 'ok'.
handle_kamailio_subscribe(JObj, _Props) ->
    'true' = kapi_omnipresence:subscribe_v(JObj),
    kz_util:put_callid(JObj),
    case gen_server:call(?SERVER, {'subscribe', JObj}) of
        'invalid' -> 'ok';
        {Count, {'unsubscribe', _}} ->
            distribute_subscribe(Count, JObj);
        {Count, {'resubscribe', Subscription}} ->
            _ = distribute_subscribe(Count, JObj),
            resubscribe_notify(Subscription);
        {Count, {'subscribe', Subscription}} ->
            _ = distribute_subscribe(Count, JObj),
            _ = maybe_probe(Subscription),
            subscribe_notify(Subscription)
    end.

-spec proxy_subscribe(kz_proplist()) -> 'ok'.
proxy_subscribe(Props) ->
    case gen_server:call(?SERVER, {'subscribe', Props}) of
        'invalid' -> 'ok';
        {_, {'resubscribe', Subscription}} ->
            _ = resubscribe_notify(Subscription);
        {_, {'subscribe', Subscription}} ->
            _ = subscribe_notify(Subscription);
        _ -> 'ok'
    end.

-spec handle_kamailio_notify(kz_json:object(), kz_proplist()) -> 'ok'.
handle_kamailio_notify(JObj, _Props) ->
    'true' = kapi_omnipresence:notify_v(JObj),
    gen_server:call(?SERVER, {'notify', JObj}).

-spec handle_sync(kz_json:object(), kz_proplist()) -> 'ok'.
handle_sync(JObj, _Props) ->
    'true' = kapi_presence:sync_v(JObj),
    Action = kz_json:get_value(<<"Action">>, JObj),
    Node = kz_json:get_value(<<"Node">>, JObj),
    gen_server:cast(?SERVER, {'sync', {Action, Node}}).

-spec handle_mwi_update(kz_json:object(), kz_proplist()) -> any().
handle_mwi_update(JObj, _Props) ->
    'true' = kapi_presence:mwi_update_v(JObj),
    notify_packages({'omnipresence', {'mwi_update', JObj}}).

-spec handle_presence_update(kz_json:object(), kz_proplist()) -> 'ok'.
handle_presence_update(JObj, _Props) ->
    'true' = kapi_presence:update_v(JObj),
    notify_packages({'omnipresence', {'presence_update', JObj}}).

-define(CACHE_TERMINATED_CALLID, kapps_config:get_integer(?CONFIG_CAT, <<"cache_terminated_callid_s">>, 60)).

-spec handle_channel_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_channel_event(JObj, _Props) ->
    EventName = kz_json:get_value(<<"Event-Name">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),

    kz_util:put_callid(CallId),

    maybe_handle_event(JObj, CallId, EventName).

-spec maybe_handle_event(kz_json:object(), ne_binary(), ne_binary()) -> 'ok'.
maybe_handle_event(JObj, CallId, <<"CHANNEL_DESTROY">>) ->
    lager:debug("caching CHANNEL_DESTROY for ~s", [CallId]),
    kz_cache:store_local(?CACHE_NAME
                        ,terminated_cache_key(CallId)
                        ,'terminated'
                        ,[{'expires', ?CACHE_TERMINATED_CALLID}]
                        ),
    handle_the_event(JObj);
maybe_handle_event(JObj, CallId, <<"CHANNEL_CREATE">> = _EventName) ->
    case kz_cache:fetch_local(?CACHE_NAME, terminated_cache_key(CallId)) of
        {'error', 'not_found'} -> handle_the_event(JObj);
        _Else -> lager:warning("received ~s but call is terminated already, dropping", [_EventName])
    end;
maybe_handle_event(JObj, CallId, <<"CHANNEL_ANSWER">> = _EventName) ->
    case kz_cache:fetch_local(?CACHE_NAME, terminated_cache_key(CallId)) of
        {'error', 'not_found'} -> handle_the_event(JObj);
        _Else -> lager:warning("received ~s but call is terminated already, dropping", [_EventName])
    end;
maybe_handle_event(JObj, _CallId, _EventName) ->
    handle_the_event(JObj).

-spec handle_the_event(kz_json:object()) -> 'ok'.
handle_the_event(JObj) ->
    notify_packages({'omnipresence', {'channel_event', JObj}}).

-spec terminated_cache_key(CallId) -> {'terminated', CallId}.
terminated_cache_key(CallId) ->
    {'terminated', CallId}.

-spec cached_terminated_callids() -> ne_binaries().
cached_terminated_callids() ->
    [CallId || {'terminated', CallId} <- kz_cache:fetch_keys_local(?CACHE_NAME)].

-spec table_id() -> 'omnipresence_subscriptions'.
table_id() -> 'omnipresence_subscriptions'.

-spec table_config() -> kz_proplist().
table_config() ->
    ['protected', 'named_table', 'set'
    ,{'keypos', #omnip_subscription.call_id}
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
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?MODULE),
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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call({'subscribe', #omnip_subscription{}=Sub}, _From,  #state{other_nodes_count=Count} = State) ->
    SubscribeResult = {Count, subscribe(Sub)},
    {'reply', SubscribeResult, State};
handle_call({'subscribe', Props}, _From, State) when is_list(Props) ->
    handle_call({'subscribe', kz_json:from_list(Props)}, _From, State);
handle_call({'subscribe', JObj}, _From, State) ->
    kz_util:put_callid(JObj),
    handle_call({'subscribe', subscribe_to_record(JObj)}, _From, State);
handle_call({'notify', JObj}, _From, State) ->
    {'reply', notify_update(JObj), State};
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'timeout', Ref, ?EXPIRE_MESSAGE}=_R, #state{expire_ref=Ref, ready='true'}=State) ->
    case expire_old_subscriptions() of
        0 -> 'ok';
        _N -> lager:debug("expired ~p subscriptions", [_N])
    end,
    {'noreply', State#state{expire_ref=start_expire_ref()
                           ,other_nodes_count=kz_nodes:whapp_count(?APP_NAME)
                           }};
handle_info({'timeout', Ref, ?EXPIRE_MESSAGE}=_R, #state{expire_ref=Ref, ready='false'}=State) ->
    {'noreply', State#state{expire_ref=start_expire_ref()
                           ,other_nodes_count=kz_nodes:whapp_count(?APP_NAME)
                           }};
handle_info(?TABLE_READY(_Tbl), State) ->
    lager:debug("recv table_ready for ~p", [_Tbl]),
    {'noreply', State#state{ready='true'}, 'hibernate'};
handle_info('check_sync', #state{sync_nodes=[]} = State) ->
    omnipresence_shared_listener:start_listener(),
    {'noreply', State};
handle_info('check_sync', State) ->
    erlang:send_after(5 * ?MILLISECONDS_IN_SECOND, self(), 'check_sync'),
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
-spec terminate(any(), state()) -> 'ok'.
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
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec notify_packages(any()) -> 'ok'.
notify_packages(Msg) ->
    _ = [gen_server:cast(Pid, Msg)
         || {_, Pid, _, _} <- supervisor:which_children('omnip_sup'),
            is_pid(Pid)
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

-spec probe(subscription()) -> 'ok'.
probe(#omnip_subscription{event=Package
                         ,user=User
                         }=Subscription) ->
    Msg = {'omnipresence', {'probe', Package, User, Subscription}},
    notify_packages(Msg).

-spec maybe_probe(subscription()) -> 'ok'.
maybe_probe(#omnip_subscription{event=Package
                               ,user=User
                               }=Subscription) ->
    case count_subscriptions(Package, User) > 1 of
        'true' -> 'ok';
        'false' -> probe(Subscription)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec distribute_subscribe(integer(), kz_json:object()) -> 'ok'.
distribute_subscribe(Count, JObj)
  when Count > 1 ->
    kapps_util:amqp_pool_send(
      kz_json:delete_key(<<"Node">>, JObj)
                             ,fun kapi_presence:publish_subscribe/1
     );
distribute_subscribe(_Count, _JObj) -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe_to_record(kz_json:object()) -> subscription().
subscribe_to_record(JObj) ->
    {P, U, [Username, Realm]} = omnip_util:extract_user(kz_json:get_value(<<"User">>, JObj)),
    {P, F, _} = omnip_util:extract_user(kz_json:get_value(<<"From">>, JObj, <<>>)),
    Version = case kz_json:get_value(<<"Subscription-ID">>, JObj) of
                  'undefined' -> 1;
                  _Else -> 2
              end,
    #omnip_subscription{user=U
                       ,from=F
                       ,protocol=P
                       ,expires=expires(JObj)
                       ,normalized_user=kz_util:to_lower_binary(U)
                       ,normalized_from=kz_util:to_lower_binary(F)
                       ,username=kz_util:to_lower_binary(Username)
                       ,realm=kz_util:to_lower_binary(Realm)
                       ,stalker=kz_json:get_first_defined([<<"Subscription-ID">>
                                                          ,<<"Server-ID">>
                                                          ,<<"Queue">>
                                                          ], JObj)
                       ,event=kz_json:get_value(<<"Event-Package">>, JObj, ?DEFAULT_EVENT)
                       ,contact=kz_json:get_value(<<"Contact">>, JObj)
                       ,call_id=kz_json:get_value(<<"Call-ID">>, JObj)
                       ,subscription_id=kz_json:get_value(<<"Subscription-ID">>, JObj)
                       ,proxy_route= kz_json:get_value(<<"Proxy-Route">>, JObj)
                       ,version=Version
                       ,user_agent=kz_json:get_binary_value(<<"User-Agent">>, JObj)
                       }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec subscriptions_to_json(subscriptions()) -> kz_json:objects().
subscriptions_to_json(Subs) ->
    [subscription_to_json(S) || S <- Subs].

-spec subscription_to_json(subscription()) -> kz_json:object().
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
                                        ,last_sequence=Sequence
                                        ,last_reply=Reply
                                        ,last_body=Body
                                        ,user_agent=UA
                                        }) ->
    kz_json:from_list(
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
        ,{<<"notify">>, kz_json:from_list([{<<"sequence">>, Sequence}
                                          ,{<<"reply">>, Reply}
                                          ,{<<"body">>, Body}
                                          ])}
        ,{<<"user_agent">>, UA}
        ])).

-spec start_expire_ref() -> reference().
start_expire_ref() ->
    erlang:start_timer(?EXPIRE_SUBSCRIPTIONS, self(), ?EXPIRE_MESSAGE).

-spec expire_old_subscriptions() -> non_neg_integer().
expire_old_subscriptions() ->
    Now = kz_util:current_tstamp(),
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
-spec count_subscriptions(binary(), binary()) -> integer().
count_subscriptions(Event, User) ->
    U = kz_util:to_lower_binary(User),
    MatchSpec = [{#omnip_subscription{normalized_user='$1'
                                     ,event=Event
                                     ,_='_'
                                     }
                 ,[{'=:=', '$1', {'const', U}}]
                 ,['$_']
                 }],
    ets:select_count(table_id(), MatchSpec).

-spec find_subscription(ne_binary()) ->
                               {'ok', subscription()} |
                               {'error', 'not_found'}.
find_subscription(CallId) ->
    case ets:lookup(table_id(), CallId) of
        [] -> {'error', 'not_found'};
        [#omnip_subscription{}=Sub] -> {'ok', Sub}
    end.

-spec find_subscriptions(ne_binary() | kz_json:object()) ->
                                {'ok', subscriptions()} |
                                {'error', 'not_found'}.
-spec find_subscriptions(ne_binary(), ne_binary()) ->
                                {'ok', subscriptions()} |
                                {'error', 'not_found'}.
find_subscriptions(User) when is_binary(User) ->
    find_subscriptions(?DEFAULT_EVENT, User);
find_subscriptions(JObj) ->
    find_subscriptions(
      kz_json:get_value(<<"Event-Package">>, JObj, ?DEFAULT_EVENT)
                      ,<<(kz_json:get_value(<<"Username">>, JObj))/binary
                         ,"@"
                         ,(kz_json:get_value(<<"Realm">>, JObj))/binary
                       >>).

find_subscriptions(Event, User) when is_binary(User) ->
    U = kz_util:to_lower_binary(User),
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
find_user_subscriptions(?OMNIPRESENCE_EVENT_ALL, User) ->
    U = kz_util:to_lower_binary(User),
    MatchSpec = [{#omnip_subscription{normalized_from='$1'
                                     ,_='_'
                                     }
                 ,[{'=:=', '$1', {'const', U}}]
                 ,['$_']
                 }],
    find_user_subscriptions(MatchSpec);
find_user_subscriptions(Event, User) ->
    U = kz_util:to_lower_binary(User),
    MatchSpec = [{#omnip_subscription{normalized_from='$1'
                                     ,event=Event
                                     ,_='_'
                                     }
                 ,[{'=:=', '$1', {'const', U}}]
                 ,['$_']
                 }],
    find_user_subscriptions(MatchSpec).

-spec find_user_subscriptions(ets:match_spec()) -> {'ok', subscriptions()} |
                                                   {'error', 'not_found'}.
find_user_subscriptions(MatchSpec) ->
    try ets:select(table_id(), MatchSpec) of
        [] -> {'error', 'not_found'};
        Subs -> {'ok', Subs}
    catch
        _E:_M -> lager:error("error fetching subscriptions : ~p : ~p", [_E, _M]),
                 {'error', 'not_found'}
    end.

-spec get_stalkers(ne_binary(), ne_binary()) ->
                          {'ok', binaries()} |
                          {'error', 'not_found'}.
get_stalkers(Event, User) ->
    U = kz_util:to_lower_binary(User),
    MatchSpec = [{#omnip_subscription{normalized_user='$1'
                                     ,stalker='$2'
                                     ,event=Event
                                     ,_='_'
                                     }
                 ,[{'=:=', '$1', {'const', U}}]
                 ,['$2']
                 }],
    case ets:select(table_id(), MatchSpec) of
        [] -> {'error', 'not_found'};
        Subs -> {'ok', lists:usort(Subs)}
    end.

-spec get_subscriptions(ne_binary(), ne_binary()) ->
                               {'ok', subscriptions()} |
                               {'error', 'not_found'}.
get_subscriptions(Event, User) ->
    U = kz_util:to_lower_binary(User),
    case find_subscriptions(Event, U) of
        {'ok', Subs} -> {'ok', Subs};
        {'error', _}=Err -> Err
    end.

-spec get_subscriptions(ne_binary(), ne_binary(), non_neg_integer()) ->
                               {'ok', subscriptions()} |
                               {'error', 'not_found'}.
get_subscriptions(Event, User, Version) ->
    U = kz_util:to_lower_binary(User),
    MatchSpec = [{#omnip_subscription{normalized_user='$1'
                                     ,event=Event
                                     ,version=Version
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
-spec dedup(subscriptions(), dict:dict()) -> subscriptions().
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
-spec expires(integer() | kz_json:object()) -> non_neg_integer().
expires(0) -> 0;
expires(I) when is_integer(I), I >= 0 -> I + ?EXPIRES_FUDGE;
expires(JObj) -> expires(kz_json:get_integer_value(<<"Expires">>, JObj)).

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
        #omnip_subscription{realm=kz_util:to_lower_binary(Realm)
                           ,event=Event
                           ,_='_'
                           },
    ets:match_object(table_id(), MatchSpec).

search_for_subscriptions(Event, Realm, '_') ->
    search_for_subscriptions(Event, Realm);
search_for_subscriptions(Event, Realm, Username) ->
    MatchSpec =
        #omnip_subscription{username=kz_util:to_lower_binary(Username)
                           ,realm=kz_util:to_lower_binary(Realm)
                           ,event=Event
                           ,_='_'
                           },
    ets:match_object(table_id(), MatchSpec).

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
                             ,call_id=CallId
                             }=S)
  when E =< 0 ->
    case find_subscription(CallId) of
        {'error', 'not_found'} -> {'unsubscribe', S};
        {'ok', #omnip_subscription{timestamp=_T
                                  ,expires=_E
                                  }=O} ->
            lager:debug("unsubscribe ~s/~s (had ~p s left)", [_U, _F, _E - kz_util:elapsed_s(_T)]),
            ets:delete_object(table_id(), O),
            {'unsubscribe', O}
    end;
subscribe(#omnip_subscription{user=_U
                             ,from=_F
                             ,expires=E1
                             ,timestamp=T1
                             ,call_id=CallId
                             ,stalker=Stalker
                             ,contact=Contact
                             }=S) ->
    case find_subscription(CallId) of
        {'ok', #omnip_subscription{timestamp=_T
                                  ,expires=_E2
                                  }=O
        } ->
            lager:debug("re-subscribe ~s/~s/~s expires in ~ps(prior remaing ~ps)"
                       ,[_U, _F, CallId, E1, _E2 - kz_util:elapsed_s(_T)]
                       ),
            ets:update_element(table_id(), CallId,
                               [{#omnip_subscription.timestamp, T1}
                               ,{#omnip_subscription.expires, E1}
                               ,{#omnip_subscription.stalker, Stalker}
                               ,{#omnip_subscription.contact, Contact}
                               ]),
            {'resubscribe', O#omnip_subscription{timestamp=T1
                                                ,expires=E1
                                                ,stalker=Stalker
                                                ,contact=Contact
                                                }};
        {'error', 'not_found'} ->
            lager:debug("subscribe ~s/~s/~s expires in ~ps", [_U, _F, CallId, E1]),
            ets:insert(table_id(), S),
            {'subscribe', S}
    end.

-spec notify_update(kz_json:object()) -> 'ok' | {'error', any()}.
notify_update(JObj) ->
    Sequence = kz_json:get_integer_value(<<"Sequence">>, JObj),
    Reply = kz_json:get_integer_value(<<"Reply">>, JObj),
    Body = kz_json:get_ne_binary_value(<<"Body">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    case find_subscription(CallId) of
        {'ok', #omnip_subscription{from = From
                                  ,user = User
                                  ,event = Event
                                  }} ->
            lager:debug("received notify reply ~B for ~s subscription (~s) of ~s to ~s", [Reply, Event, CallId, From, User]),
            ets:update_element(table_id(), CallId,
                               [{#omnip_subscription.last_sequence, Sequence}
                               ,{#omnip_subscription.last_reply, Reply}
                               ,{#omnip_subscription.last_body, Body}
                               ]);
        {'error', _} = E ->
            lager:debug("notify received for unexistent subscription ~s", [CallId]),
            E
    end.
