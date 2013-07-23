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

         ,handle_subscribe/2
         ,handle_new_channel/2
         ,handle_destroy_channel/2
         ,handle_answered_channel/2
         ,handle_presence_update/2

         ,handle_search_req/2
         ,handle_reset/2

         ,table_id/0
         ,table_config/0

         ,find_subscription/1
         ,find_subscriptions/2

         ,search_for_subscriptions/1, search_for_subscriptions/2

         ,subscription_to_json/1 ,subscriptions_to_json/1
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

    case find_subscriptions(User) of
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
-spec handle_subscribe(wh_json:object(), wh_proplist()) -> any().
handle_subscribe(JObj, Props) ->
    'true' = wapi_presence:subscribe_v(JObj),
    gen_listener:cast(props:get_value(?MODULE, Props)
                      ,{'subscribe', subscribe_to_record(JObj)}
                     ).

handle_presence_update(JObj, _Props) ->
    'true' = wapi_notifications:presence_update_v(JObj),
    maybe_send_update(JObj, wh_json:get_value(<<"State">>, JObj)).

handle_new_channel(JObj, _Props) ->
    'true' = wapi_call:new_channel_v(JObj),
    wh_util:put_callid(JObj),

    lager:debug("new channel"),
    maybe_send_update(JObj, ?PRESENCE_RINGING).

handle_answered_channel(JObj, _Props) ->
    'true' = wapi_call:answered_channel_v(JObj),
    wh_util:put_callid(JObj),

    lager:debug("answered channel"),
    maybe_send_update(JObj, ?PRESENCE_ANSWERED).

handle_destroy_channel(JObj, _Props) ->
    'true' = wapi_call:destroy_channel_v(JObj),
    wh_util:put_callid(JObj),

    lager:debug("destroy channel"),
    maybe_send_update(JObj, ?PRESENCE_HANGUP).

-spec maybe_send_update(wh_json:object(), ne_binary()) -> any().
maybe_send_update(JObj, State) ->
    Update = props:filter_undefined(
               [{<<"To">>, To = wh_json:get_first_defined([<<"To">>, <<"Presence-ID">>], JObj)}
                ,{<<"From">>, From = wh_json:get_value(<<"From">>, JObj)}
                ,{<<"State">>, State}
                ,{<<"Direction">>, wh_json:get_value(<<"Call-Direction">>, JObj)}
                ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    case find_subscriptions(To, From) of
        {'ok', Subs} ->
            [send_update(Update, S) || S <- Subs];
        {'error', 'not_found'} ->
            lager:debug("no subs for ~s or ~s(~s)", [To, From, State])
    end.

-spec send_update(wh_proplist(), subscription()) -> 'ok'.
send_update(Update, #omnip_subscription{stalker=S
                                        ,protocol=P
                                        ,from=F
                                       }) ->
    To = props:get_value(<<"To">>, Update),
    From = props:get_value(<<"From">>, Update, F),

    lager:debug("sending update '~s' for '~s' from '~s' to '~s'", [props:get_value(<<"State">>, Update), To, From, S]),

    whapps_util:amqp_pool_send([{<<"To">>, <<P/binary, ":", To/binary>>}
                                ,{<<"From">>, <<P/binary, ":", From/binary>>}
                                | props:delete_keys([<<"To">>, <<"From">>], Update)
                               ]
                               ,fun(API) -> wapi_presence:publish_update(S, API) end
                              ).

-spec subscribe_to_record(wh_json:object()) -> subscription().
subscribe_to_record(JObj) ->
    {P, U, [Username, Realm]} = omnip_util:extract_user(wh_json:get_value(<<"User">>, JObj)),
    {P, F, _} = omnip_util:extract_user(wh_json:get_value(<<"From">>, JObj, <<>>)),

    S = wh_json:get_first_defined([<<"Queue">>, <<"Server-ID">>], JObj),
    E = expires(JObj),

    lager:debug("subscribe for ~s from ~s", [U, F]),
    #omnip_subscription{user=U
                        ,from=F
                        ,stalker=S
                        ,expires=E
                        ,protocol=P
                        ,username=Username
                        ,realm=Realm
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
            lager:debug("found subscription for ~s, removing old subscription (had ~p s left)", [_U, _E - wh_util:elapsed_s(_T)]),
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
                                      ,from=F
                                     }) ->
    case ets:match_object(table_id(), #omnip_subscription{user=U
                                                          ,stalker=S
                                                          ,from=F
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

-spec find_subscriptions(ne_binary()) ->
                                {'ok', subscriptions()} |
                                {'error', 'not_found'}.
-spec find_subscriptions(ne_binary(), ne_binary()) ->
                                {'ok', subscriptions()} |
                                {'error', 'not_found'}.
find_subscriptions(User) ->
    case ets:select(table_id(), [{#omnip_subscription{user='$1'
                                                      ,_='_'
                                                     }
                                  ,[{'=:=', '$1', {'const', User}}]
                                  ,['$_']
                                 }])
    of
        [] -> {'error', 'not_found'};
        Subs -> {'ok', Subs}
    end.

find_subscriptions(To, From) ->
    case ets:select(table_id(), [{#omnip_subscription{user='$1'
                                                      ,_='_'
                                                     }
                                  ,[{'orelse'
                                     ,{'=:=', '$1', {'const', To}}
                                     ,{'=:=', '$1', {'const', From}}
                                    }]
                                  ,['$_']
                                 }])
    of
        [] -> {'error', 'not_found'};
        Subs -> {'ok', Subs}
    end.

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
