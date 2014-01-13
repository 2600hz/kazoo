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

         ,handle_subscribe/2
         ,handle_new_channel/1
         ,handle_cdr/1
         ,handle_destroyed_channel/1
         ,handle_answered_channel/1
         ,handle_presence_update/2

         ,handle_search_req/2
         ,handle_reset/2

         ,table_id/0
         ,table_config/0

         ,find_subscription/1
         ,find_subscriptions/1

         ,search_for_subscriptions/1
         ,search_for_subscriptions/2

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

-record(state, {
          expire_ref :: reference()
         }).

-record(omnip_subscription, {
          user                                  :: ne_binary() | '_' %% user@realm.com
          ,from                                 :: api_binary() | <<>> | '_' %% user@realm.com
          ,stalker                              :: ne_binary() | '_' % amqp queue to publish updates to
          ,expires = 0                          :: non_neg_integer() | '_' | '$2'
          ,timestamp = wh_util:current_tstamp() :: wh_now() | '_' | '$1'
          ,protocol = <<"sip">>                 :: ne_binary() | '_' % protocol
          ,username                             :: api_binary() | '_'
          ,realm                                :: api_binary() | '_'
          ,normalized_user                      :: api_binary() | '_'
          ,normalized_from                      :: api_binary() | '_'
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

-spec handle_search_req(wh_json:object(), wh_proplist()) -> any().
handle_search_req(JObj, _Props) ->
    'true' = wapi_presence:search_req_v(JObj),
    Username = wh_json:get_value(<<"Username">>, JObj, '_'),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    lager:debug("searching for subs for ~p@~s", [Username, Realm]),
    case search_for_subscriptions(Realm, Username) of
        [] -> 'ok';
        Subs ->
            Resp = [{<<"Subscriptions">>, subscriptions_to_json(Subs)}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_presence:publish_search_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

-spec handle_reset(wh_json:object(), wh_proplist()) -> any().
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
-spec handle_subscribe(wh_json:object(), wh_proplist()) -> any().
handle_subscribe(JObj, Props) ->
    'true' = wapi_presence:subscribe_v(JObj),
    gen_listener:cast(props:get_value(?MODULE, Props)
                      ,{'subscribe', subscribe_to_record(JObj)}
                     ).

-spec handle_presence_update(wh_json:object(), wh_proplist()) -> any().
handle_presence_update(JObj, _Props) ->
    'true' = wapi_notifications:presence_update_v(JObj),
    maybe_send_update(JObj, wh_json:get_value(<<"State">>, JObj)).

-spec handle_new_channel(wh_json:object()) -> any().
handle_new_channel(JObj) ->
    'true' = wapi_call:new_channel_v(JObj),
    wh_util:put_callid(JObj),
    maybe_send_update(JObj, ?PRESENCE_RINGING).

-spec handle_answered_channel(wh_json:object()) -> any().
handle_answered_channel(JObj) ->
    'true' = wapi_call:answered_channel_v(JObj),
    wh_util:put_callid(JObj),
    maybe_send_update(JObj, ?PRESENCE_ANSWERED).

-spec handle_destroyed_channel(wh_json:object()) -> any().
handle_destroyed_channel(JObj) ->
    'true' = wapi_call:destroy_channel_v(JObj),
    wh_util:put_callid(JObj),
    maybe_send_update(JObj, ?PRESENCE_HANGUP),
    %% When multiple omnipresence instances are in multiple
    %% zones its possible (due to the round-robin) for the
    %% instance closest to rabbitmq to process the terminate
    %% before a confirm/early if both are sent immediately.
    timer:sleep(1000),
    maybe_send_update(JObj, ?PRESENCE_HANGUP).

-spec handle_cdr(wh_json:object()) -> any().
handle_cdr(JObj) ->
    'true' = wapi_call:cdr_v(JObj),
    wh_util:put_callid(JObj),
    maybe_send_update(JObj, ?PRESENCE_HANGUP),
    %% When multiple omnipresence instances are in multiple
    %% zones its possible (due to the round-robin) for the
    %% instance closest to rabbitmq to process the terminate
    %% before a confirm/early if both are sent immediately.
    timer:sleep(1000),
    maybe_send_update(JObj, ?PRESENCE_HANGUP).

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

-spec start_expire_ref() -> reference().
start_expire_ref() ->
    erlang:start_timer(?EXPIRE_SUBSCRIPTIONS, self(), ?EXPIRE_MESSAGE).

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
handle_cast({'subscribe', #omnip_subscription{from = <<>>}=_S}, State) ->
    lager:debug("subscription with no from: ~p", [subscription_to_json(_S)]),
    {'noreply', State};
handle_cast({'subscribe', #omnip_subscription{expires=E
                                              ,user=_U
                                              ,from=_F
                                              ,stalker=_S
                                             }=S}, State) when E =< 0 ->
    case find_subscription(S) of
        {'ok', #omnip_subscription{timestamp=_T
                                   ,expires=_E
                                   }=O} ->
            lager:debug("unsubscribe ~s/~s (had ~p s left)", [_U, _F, _E - wh_util:elapsed_s(_T)]),
            ets:delete_object(table_id(), O);
        {'error', 'not_found'} -> 'ok'
    end,
    {'noreply', State};
handle_cast({'subscribe', #omnip_subscription{user=_U
                                              ,from=_F
                                              ,expires=_E1
                                              ,stalker=_S
                                             }=S}, State) ->
    case find_subscription(S) of
        {'ok', #omnip_subscription{timestamp=_T
                                   ,expires=_E2
                                  }=O} ->
            lager:debug("re-subscribe ~s/~s expires in ~ps(prior remaing ~ps)"
                        ,[_U, _F, _E1, _E2 - wh_util:elapsed_s(_T)]),
            ets:delete_object(table_id(), O);
        {'error', 'not_found'} ->
            lager:debug("subscribe ~s/~s expires in ~ps", [_U, _F, _E1]),
            'ok'
    end,
    ets:insert(table_id(), S),
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
-spec find_subscription(subscription()) ->
                               {'ok', subscription()} |
                               {'error', 'not_found'}.
find_subscription(#omnip_subscription{normalized_user=U
                                      ,normalized_from=F
                                      ,stalker=S
                                     }) ->
    MatchSpec = #omnip_subscription{normalized_user=U
                                    ,normalized_from=F
                                    ,stalker=S
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

-spec find_subscriptions(ne_binary()) ->
                                {'ok', subscriptions()} |
                                {'error', 'not_found'}.
find_subscriptions(User) when is_binary(User) ->
    U = wh_util:to_lower_binary(User),
    MatchSpec = [{#omnip_subscription{normalized_user='$1'
                                      ,_='_'
                                     }
                  ,[{'=:=', '$1', {'const', U}}]
                  ,['$_']
                 }],
    case ets:select(table_id(), MatchSpec) of
        [] -> {'error', 'not_found'};
        Subs -> {'ok', dedup(Subs)}
    end;
find_subscriptions(JObj) ->
    find_subscriptions(
      <<(wh_json:get_value(<<"Username">>, JObj))/binary
        ,"@"
        ,(wh_json:get_value(<<"Realm">>, JObj))/binary>>).


-spec dedup(subscriptions()) -> subscriptions().
dedup(Subscriptions) ->
    dedup(Subscriptions, dict:new()).

-spec dedup(subscriptions(), dict()) -> subscriptions().
dedup([], Dictionary) ->
    [Subscription || {_, Subscription} <- dict:to_list(Dictionary)];
dedup([#omnip_subscription{normalized_user=User
                           ,stalker=Stalker
                          }=Subscription
       | Subscriptions
      ], Dictionary) ->
    D = dict:store({User, Stalker}, Subscription, Dictionary),
    dedup(Subscriptions, D).

expire_old_subscriptions() ->
    Now = wh_util:current_tstamp(),
    ets:select_delete(table_id(), [{#omnip_subscription{timestamp='$1'
                                                        ,expires='$2'
                                                        ,_='_'
                                                       }
                                    ,[{'>', {'const', Now}, {'+', '$1', '$2'}}]
                                    ,['true']
                                   }]).

-spec maybe_send_update(wh_json:object(), ne_binary()) -> any().
maybe_send_update(JObj, State) ->
    To = wh_json:get_first_defined([<<"To">>, <<"Presence-ID">>], JObj),
    From = wh_json:get_value(<<"From">>, JObj),
    Direction = wh_json:get_lower_binary(<<"Call-Direction">>, JObj),
    {User, Props} =
        case Direction =:= <<"inbound">> of
            'true' ->
                {From, props:filter_undefined(
                       [{<<"To">>, To}
                        ,{<<"State">>, State}
                        ,{<<"Direction">>, Direction}
                        ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                        ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                        | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                       ])};
            'false' ->
                {To, props:filter_undefined(
                         [{<<"From">>, From}
                          ,{<<"State">>, State}
                          ,{<<"Direction">>, Direction}
                          ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                          ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                          | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                         ])}
            end,
    case find_subscriptions(User) of
        {'ok', Subs} ->
            [send_update(Props, Direction, S)
             || S <- Subs
            ];
        {'error', 'not_found'} ->
            lager:debug("no subscriptions for ~s (state ~s)"
                        ,[User, State])
    end.

-spec send_update(wh_proplist(), ne_binary(), subscription()) -> 'ok'.
send_update(Props, <<"inbound">>, #omnip_subscription{user=User}=Subscription) ->
    send_update([{<<"From">>, User}|Props], Subscription);
send_update(Props, _, #omnip_subscription{user=User}=Subscription) ->
    send_update([{<<"To">>, User}|Props], Subscription).

-spec send_update(wh_proplist(), subscription()) -> 'ok'.
send_update(Props, #omnip_subscription{stalker=Q
                                       ,protocol=Protocol
                                       ,from=F
                                       }) ->
    To = props:get_value(<<"To">>, Props),
    From = props:get_value(<<"From">>, Props, F),
    State = props:get_value(<<"State">>, Props),
    lager:debug("sending update ~s/~s ~s to '~s'", [To, From, State, Q]),
    whapps_util:amqp_pool_send([{<<"To">>, <<Protocol/binary, ":", To/binary>>}
                                ,{<<"From">>, <<Protocol/binary, ":", From/binary>>}
                                | props:delete_keys([<<"To">>, <<"From">>], Props)
                               ]
                               ,fun(P) -> wapi_presence:publish_update(Q, P) end
                              ).

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
                       }.

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
                                        }) ->
    wh_json:from_list(
      [{<<"user">>, User}
       ,{<<"from">>, From}
       ,{<<"stalker">>, Stalker}
       ,{<<"expires">>, Expires}
       ,{<<"timestamp">>, Timestamp}
       ,{<<"protocol">>, Protocol}
       ,{<<"username">>, Username}
       ,{<<"realm">>, Realm}
      ]).

expires(I) when is_integer(I) -> I;
expires(JObj) -> expires(wh_json:get_integer_value(<<"Expires">>, JObj)).

table_id() -> 'omnipresence_subscriptions'.
table_config() ->
    ['protected', 'named_table', 'bag'
     ,{'keypos', #omnip_subscription.user}
    ].

-spec search_for_subscriptions(ne_binary()) -> subscriptions().
-spec search_for_subscriptions(ne_binary(), ne_binary() | '_') -> subscriptions().
search_for_subscriptions(Realm) ->
    MatchSpec =
        #omnip_subscription{realm=wh_util:to_lower_binary(Realm)
                            ,_='_'
                           },
    ets:match_object(table_id(), MatchSpec).

search_for_subscriptions(Realm, Username) ->
    MatchSpec =
        #omnip_subscription{username=wh_util:to_lower_binary(Username)
                            ,realm=wh_util:to_lower_binary(Realm)
                            ,_='_'
                           },
    ets:match_object(table_id(), MatchSpec).

publish_flush([]) -> 'ok';
publish_flush([#omnip_subscription{stalker=Q, user=User} | Subscriptions]) ->
    Props = [{<<"Type">>, <<"id">>}
             ,{<<"User">>, <<"sip:", User/binary>>}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    lager:debug("sending flush for ~s to ~s", [User, Q]),
    whapps_util:amqp_pool_send(Props, fun(P) -> wapi_presence:publish_flush(Q, P) end),
    publish_flush(Subscriptions).
