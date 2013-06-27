%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnip_subscriptions).

-behaviour(gen_server).

-export([start_link/0

         ,handle_subscribe/2
         ,handle_presence_update/2
         ,handle_new_channel/2
         ,handle_destroy_channel/2
         ,handle_answered_channel/2
         ,handle_query_req/2

         ,table_id/0
         ,table_config/0
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

-define(PRESENCE_HANGUP, <<"terminated">>).
-define(PRESENCE_RINGING, <<"early">>).
-define(PRESENCE_ANSWERED, <<"confirmed">>).

-define(EXPIRE_SUBSCRIPTIONS, whapps_config:get_integer(?CONFIG_CAT, <<"expire_check_ms">>, 1000)).
-define(EXPIRE_MESSAGE, 'clear_expired').

-record(state, {
          expire_ref :: reference()
         }).

-record(omnip_subscription, {
          user :: ne_binary() | '_'
          ,stalker :: ne_binary() | '_' % amqp queue to publish updates to
          ,expires = 0 :: non_neg_integer() | '_' | '$2'
          ,timestamp = wh_util:current_tstamp() :: wh_now() | '_' | '$1'
          ,protocol = <<"sip">> :: ne_binary() | '_' % protocol
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

handle_query_req(_JObj, _Props) ->
    'ok'.

handle_subscribe(JObj, Props) ->
    'true' = wapi_presence:subscribe_v(JObj),
    gen_listener:cast(props:get_value(?MODULE, Props)
                      ,{'subscribe', subscribe_to_record(JObj)}
                     ).

handle_presence_update(JObj, _Props) ->
    'true' = wapi_notifications:presence_update_v(JObj),
    lager:debug("presence update recv: ~p", [JObj]).

handle_new_channel(JObj, _Props) ->
    'true' = wapi_call:new_channel_v(JObj),

    To = wh_json:get_value(<<"To">>, JObj),
    From = wh_json:get_value(<<"From">>, JObj),

    maybe_send_update(To, JObj, ?PRESENCE_RINGING),
    maybe_send_update(From, JObj, ?PRESENCE_RINGING).

handle_answered_channel(JObj, _Props) ->
    'true' = wapi_call:answered_channel_v(JObj),

    To = wh_json:get_value(<<"To">>, JObj),
    From = wh_json:get_value(<<"From">>, JObj),

    maybe_send_update(To, JObj, ?PRESENCE_ANSWERED),
    maybe_send_update(From, JObj, ?PRESENCE_ANSWERED).


maybe_send_update(User, JObj, Update) ->
    case find_subscriptions(User) of
        {'ok', Subs} ->
            [send_update(Update, JObj, S) || S <- Subs];
        {'error', 'not_found'} ->
            lager:debug("no subs for ~s(~s)", [User, Update])
    end.
send_update(Update, JObj, #omnip_subscription{user=U
                                              ,stalker=S
                                             }) ->
    lager:debug("sending update ~s to ~s(~s)", [Update, U, S]),

    Prop = [{<<"To">>, U}
            ,{<<"From">>, U}
            ,{<<"State">>, Update}
            ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    whapps_util:amqp_pool_send(Prop, fun(API) -> wapi_presence:publish_update(S, API) end).

handle_destroy_channel(JObj, _Props) ->
    'true' = wapi_call:destroy_channel_v(JObj),

    To = wh_json:get_value(<<"To">>, JObj),
    From = wh_json:get_value(<<"From">>, JObj),

    maybe_send_update(To, JObj, ?PRESENCE_HANGUP),
    maybe_send_update(From, JObj, ?PRESENCE_HANGUP).

subscribe_to_record(JObj) ->
    U = wh_json:get_value(<<"User">>, JObj),
    S = wh_json:get_value(<<"Queue">>, JObj),
    E = expires(JObj),
    P = protocol(U),
    #omnip_subscription{user=U
                        ,stalker=S
                        ,expires=E
                        ,protocol=P
                       }.

expires(I) when is_integer(I) -> I;
expires(JObj) -> expires(wh_json:get_integer_value(<<"Expires">>, JObj)).

protocol(<<"sip:", _/binary>>) -> <<"sip">>;
protocol(_U) -> <<"sip">>.

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
    lager:debug("updating (or creating) subscription for ~s(~s)", [_U, _S]),
    case find_subscription(S) of
        {'ok', #omnip_subscription{timestamp=_T
                                   ,expires=_E
                                  }=O} ->
            lager:debug("found subscription, removing old subscription (had ~p s left)", [_E - wh_util:elapsed_s(_T)]),
            ets:delete_object(table_id(), O);
        {'error', 'not_found'} -> 'ok'
    end,
    lager:debug("creating subscription"),
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
