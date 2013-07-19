%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(omnip_subscriptions).

-behaviour(gen_server).

-export([start_link/0

         ,handle_subscribe/2, handle_subscribe_only/2
         ,handle_new_channel/2
         ,handle_destroy_channel/2
         ,handle_answered_channel/2
         ,handle_search_req/2
         ,handle_reset/2

         ,table_id/0
         ,table_config/0

         ,find_subscription/1
         ,find_subscriptions/1

         ,search_for_subscriptions/1, search_for_subscriptions/2

         ,subscription_to_json/1 ,subscriptions_to_json/1

         ,maybe_send_update/4
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

handle_search_req(JObj, _Props) ->
    'true' = wapi_presence:search_req_v(JObj),
    Username = wh_json:get_value(<<"Username">>, JObj, '_'),
    Realm = wh_json:get_value(<<"Realm">>, JObj),

    lager:debug("searching for subs for ~s@~s", [Username, Realm]),

    case search_for_subscriptions(Realm, Username) of
        [] -> 'ok';
        Subs ->
            Resp = [{<<"Subscriptions">>, subscriptions_to_json(Subs)}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_presence:publish_search_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

-spec search_for_subscriptions(ne_binary()) -> subscriptions().
-spec search_for_subscriptions(ne_binary(), ne_binary() | '_') -> subscriptions().
search_for_subscriptions(Realm) ->
    search_for_subscriptions(Realm, '_').
search_for_subscriptions(Realm, Username) ->
    ets:match_object(table_id(), #omnip_subscription{username=Username
                                                     ,realm=Realm
                                                     ,_='_'
                                                    }).

handle_reset(JObj, _Props) ->
    'true' = wapi_presence:reset_v(JObj),

    Username = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),

    User = <<Username/binary, "@", Realm/binary>>,

    case find_subscriptions(#omnip_subscription{user=User}) of
        {'error', 'not_found'} -> 'ok';
        {'ok', Subs} ->
            lager:debug("sending flushes for ~s", [User]),
            Req = [{<<"Type">>, <<"id">>}
                   ,{<<"User">>, <<"sip:", User/binary>>}
                   | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                  ],
            [whapps_util:amqp_pool_send(Req, fun(API) -> wapi_presence:publish_flush(Q, API) end)
             || #omnip_subscription{stalker=Q} <- Subs
            ]
    end.

%% Subscribes work like this:
%%   Subscribe comes into shared queue, gets round-robined to next omni whapp
%%   Handling whapp then publishes an internal whapp msg to all other omni whapps
%%   Handling whapp then publishes a status update to the subscribe Queue
%%
%% handle_subscribe_only processes subscribes received on the whapp's dedicated
%% queue, without the lookup of the current state
handle_subscribe(JObj, Props) ->
    JObj1 = maybe_patch_msg_id(JObj),
    'true' = wapi_presence:subscribe_v(JObj1),

    %% sending update to whapps
    send_subscribe_to_whapps(JObj1),
    send_update_to_listeners(JObj1, Props).

send_update_to_listeners(JObj, Props) ->
    Sub = #omnip_subscription{user=U} = subscribe_to_record(JObj),

    Srv = props:get_value('omnip_presences', Props),

    case omnip_presences:find_presence_state(U) of
        {'error', 'not_found'} ->
            lager:debug("failed to find presence state for ~s, searching", [U]),
            search_for_presence_state(U, JObj, Srv, Sub);
        {'ok', PS} ->
            lager:debug("found presence state for ~s: ~s", [U, omnip_presences:current_state(PS)]),
            maybe_send_update(U, JObj, Srv, omnip_presences:current_state(PS), Sub)
    end.

search_for_presence_state(U, SubJObj, Srv, SubR) ->
    [User, Realm] = binary:split(U, <<"@">>),

    lager:debug("find presence for ~s@~s", [User, Realm]),

    Req = [{<<"Username">>, User}
           ,{<<"Realm">>, Realm}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],

    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_call:publish_query_user_channels_req/1
                                       ,fun wapi_call:query_user_channels_resp_v/1
                                      )
    of
        {'ok', SearchJObj} ->
            lager:debug("calls in progress: ~p", [SearchJObj]),
            maybe_send_update(U, SearchJObj, Srv, ?PRESENCE_ANSWERED, SubR);
        {'error', _E} ->
            lager:debug("Failed to lookup user channels: ~p", [_E]),
            probe_for_presence(User, Realm, SubJObj, Srv)
    end.

probe_for_presence(User, Realm, SubJObj, _Srv) ->
    URI = <<User/binary, "@", Realm/binary>>,
    lager:debug("probing for presence"),
    Req = [{<<"From">>, URI}
           ,{<<"To">>, URI}
           ,{<<"To-User">>, User}
           ,{<<"To-Realm">>, Realm}
           ,{<<"From-User">>, User}
           ,{<<"From-Realm">>, Realm}
           ,{<<"Switch-Nodename">>, wh_json:get_value(<<"Node">>, SubJObj)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    wapi_notifications:publish_presence_probe(Req).

send_subscribe_to_whapps(JObj) ->
    JObj1 = wh_json:set_values(wh_api:default_headers(?APP_NAME, ?APP_VERSION), JObj),
    wapi_omnipresence:publish_subscribe(JObj1).

handle_subscribe_only(JObj, Props) ->
    JObj1 = maybe_patch_msg_id(JObj),

    'true' = wapi_presence:subscribe_v(JObj1),
    gen_listener:cast(props:get_value(?MODULE, Props)
                      ,{'subscribe', subscribe_to_record(JObj1)}
                     ).

%% needed until Kamailio is patched
maybe_patch_msg_id(JObj) ->
    case wh_json:get_value(<<"Msg-ID">>, JObj) of
        'undefined' -> wh_json:set_value(<<"Msg-ID">>, <<"foo">>, JObj);
        _ -> JObj
    end.

handle_new_channel(JObj, Props) ->
    'true' = wapi_call:new_channel_v(JObj),

    From = wh_json:get_value(<<"From">>, JObj),
    Req = wh_json:get_value(<<"Request">>, JObj),

    maybe_send_update(From, JObj, props:get_value('omnip_presences', Props), ?PRESENCE_RINGING),
    maybe_send_update(Req, JObj, props:get_value('omnip_presences', Props), ?PRESENCE_RINGING).

handle_answered_channel(JObj, Props) ->
    'true' = wapi_call:answered_channel_v(JObj),

    From = wh_json:get_value(<<"From">>, JObj),
    Req = wh_json:get_value(<<"Request">>, JObj),

    maybe_send_update(From, JObj, props:get_value('omnip_presences', Props), ?PRESENCE_ANSWERED),
    maybe_send_update(Req, JObj, props:get_value('omnip_presences', Props), ?PRESENCE_ANSWERED).

handle_destroy_channel(JObj, Props) ->
    'true' = wapi_call:destroy_channel_v(JObj),

    From = wh_json:get_value(<<"From">>, JObj),
    Req = wh_json:get_value(<<"Request">>, JObj),

    maybe_send_update(From, JObj, props:get_value('omnip_presences', Props), ?PRESENCE_HANGUP),
    maybe_send_update(Req, JObj, props:get_value('omnip_presences', Props), ?PRESENCE_HANGUP).

-spec maybe_send_update(ne_binary(), wh_json:object(), pid(), ne_binary()) -> any().
maybe_send_update(User, JObj, Srv, Update) ->
    case find_subscriptions(User) of
        {'ok', Subs} ->
            omnip_presences:update_presence_state(Srv, User, Update),
            lager:debug("updated ~s to ~s in ~p", [User, Update, Srv]),
            [send_update(Update, JObj, S) || S <- Subs];
        {'error', 'not_found'} ->
            lager:debug("no subs for ~s(~s)", [User, Update])
    end.

-spec maybe_send_update(ne_binary(), wh_json:object(), pid(), ne_binary(), subscription()) -> any().
maybe_send_update(User, JObj, Srv, Update, #omnip_subscription{expires=N}=SubR) when N > 0 ->
    case find_subscriptions(User) of
        {'ok', Subs} ->
            omnip_presences:update_presence_state(Srv, User, Update),
            lager:debug("updated ~s to ~s in ~p", [User, Update, Srv]),
            [send_update(Update, JObj, S) || S <- [SubR | Subs]];
        {'error', 'not_found'} ->
            send_update(Update, JObj, SubR),
            lager:debug("no known subs for ~s(~s), sending to originator", [User, Update])
    end;
maybe_send_update(User, JObj, Srv, Update, _SubR) ->
    maybe_send_update(User, JObj, Srv, Update).

-spec send_update(ne_binary(), wh_json:object(), subscription()) -> 'ok'.
send_update(Update, JObj, #omnip_subscription{user=U
                                              ,from=F
                                              ,stalker=S
                                              ,protocol=P
                                             }) ->
    UpdateTo = <<P/binary, ":", U/binary>>,
    UpdateFrom = <<P/binary, ":", F/binary>>,

    lager:debug("sending update '~s' to ~s from ~s (~s)", [Update, UpdateTo, UpdateFrom, S]),

    Prop = [{<<"To">>, UpdateTo}
            ,{<<"From">>, UpdateFrom}
            ,{<<"State">>, Update}
            ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    whapps_util:amqp_pool_send(Prop, fun(API) -> wapi_presence:publish_update(S, API) end),
    whapps_util:amqp_pool_send(Prop, fun wapi_omnipresence:publish_presence_update/1).

-spec subscribe_to_record(wh_json:object()) -> subscription().
subscribe_to_record(JObj) ->
    {P, U, [Username, Realm]} = extract_user(wh_json:get_value(<<"User">>, JObj)),
    {P, F, _} = extract_user(wh_json:get_value(<<"From">>, JObj, <<>>)),

    S = wh_json:get_first_defined([<<"Queue">>, <<"Server-ID">>], JObj),
    E = expires(JObj),

    #omnip_subscription{user=U
                        ,from=F
                        ,stalker=S
                        ,expires=E
                        ,protocol=P
                        ,username=Username
                        ,realm=Realm
                       }.

-spec extract_user(ne_binary()) -> {ne_binary(), ne_binary(), ne_binaries()}.
extract_user(<<"sip:", User/binary>>) -> {<<"sip">>, User, binary:split(User, <<"@">>)};
extract_user(User) -> {<<"sip">>, User, binary:split(User, <<"@">>)}.

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
                                              ,stalker=_S
                                             }=S}, State) when E =< 0 ->
    lager:debug("maybe remove subscription for ~s(~s)", [_U, _S]),
    case find_subscription(S) of
        {'ok', #omnip_subscription{timestamp=_T
                                   ,expires=_E
                                   }=O} ->
            lager:debug("found subscription, removing (had ~p s left)", [_E - wh_util:elapsed_s(_T)]),
            ets:delete_object(table_id(), O);
        {'error', 'not_found'} ->
            lager:debug("subscription not found, ignoring")
    end,
    {'noreply', State};
handle_cast({'subscribe', #omnip_subscription{user=_U
                                              ,stalker=_S
                                             }=S}, State) ->
    case find_subscription(S) of
        {'ok', #omnip_subscription{timestamp=_T
                                   ,expires=_E
                                  }=O} ->
            lager:debug("found subscription, removing old subscription (had ~p s left)", [_E - wh_util:elapsed_s(_T)]),
            ets:delete_object(table_id(), O);
        {'error', 'not_found'} -> 'ok'
    end,
    ets:insert(table_id(), S),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

-spec find_subscription(subscription()) ->
                               {'ok', subscription()} |
                               {'error', 'not_found'}.
find_subscription(#omnip_subscription{user=U
                                      ,stalker=S
                                     }) ->
    case ets:match_object(table_id(), #omnip_subscription{user=U
                                                          ,stalker=S
                                                          ,_='_'
                                                         })
    of
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

-spec find_subscriptions(subscription() | ne_binary()) ->
                                {'ok', subscriptions()} |
                                {'error', 'not_found'}.
find_subscriptions(#omnip_subscription{user=U}) ->
    case ets:match_object(table_id(), #omnip_subscription{user=U
                                                          ,_='_'
                                                         })
    of
        [] -> {'error', 'not_found'};
        Subs -> {'ok', Subs}
    end;
find_subscriptions(User) when is_binary(User) ->
    find_subscriptions(#omnip_subscription{user=User}).

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
