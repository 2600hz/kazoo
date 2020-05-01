%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(omnip_subscriptions).
-behaviour(gen_server).

-export([start_link/0]).

-export([find_user_subscriptions/2
        ,search_for_subscriptions/2, search_for_subscriptions/3
        ]).

-export([handle_kamailio_subscribe/2
        ,handle_kamailio_notify/2
        ,table_id/0
        ,table_config/0
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

-define(SERVER, ?MODULE).

-define(EXPIRE_SUBSCRIPTIONS, kapps_config:get_integer(?CONFIG_CAT, <<"expire_check_ms">>, ?MILLISECONDS_IN_SECOND)).
-define(EXPIRES_FUDGE, kapps_config:get_integer(?CONFIG_CAT, <<"expires_fudge_s">>, 20)).
-define(EXPIRE_MESSAGE, 'clear_expired').
-define(DEFAULT_EVENT, ?BLF_EVENT).
-define(DEFAULT_SEND_EVENT_LIST, [?BLF_EVENT, ?PRESENCE_EVENT]).

-define(DEFAULT_VM_NUMBER, <<"*98">>).
-define(VM_NUMBER_KEY, <<"dialog_subscribed_mwi_prefix">>).
-define(VM_NUMBER(A), kapps_account_config:get_global(A, ?CONFIG_CAT, ?VM_NUMBER_KEY, ?DEFAULT_VM_NUMBER)).

-record(state, {expire_ref :: reference()
               ,ready = 'false' :: boolean()
               ,sync = 'false'  :: boolean()
               ,sync_nodes = [] :: list()
               ,other_nodes_count = 0 :: integer()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec handle_kamailio_subscribe(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_kamailio_subscribe(JObj, _Props) ->
    'true' = kapi_omnipresence:subscribe_v(JObj),
    gen_server:cast(?SERVER, {'subscribe', JObj}).

-spec handle_kamailio_notify(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_kamailio_notify(JObj, _Props) ->
    'true' = kapi_omnipresence:notify_v(JObj),
    gen_server:cast(?SERVER, {'notify', JObj}).

-spec table_id() -> 'omnipresence_subscriptions'.
table_id() -> 'omnipresence_subscriptions'.

-spec table_config() -> kz_term:proplist().
table_config() ->
    ['protected', 'named_table', 'set'
    ,{'keypos', #omnip_subscription.call_id}
    ].

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    {'ok', #state{expire_ref=start_expire_ref()}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    lager:debug("omnipresence subscriptions unhandled call : ~p", [_Request]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'sync', {<<"Start">>, Node}}, #state{sync_nodes=Nodes}=State) ->
    {'noreply', State#state{sync_nodes=[Node | Nodes]}};
handle_cast({'sync', {<<"End">>, Node}}, #state{sync_nodes=Nodes} = State) ->
    {'noreply', State#state{sync_nodes=Nodes -- [Node]}};

handle_cast({'subscribe', #omnip_subscription{}=Sub},  State) ->
    _ = subscribe(Sub),
    {'noreply', State};
handle_cast({'subscribe', Props}, State) when is_list(Props) ->
    handle_cast({'subscribe', kz_json:from_list(Props)}, State);
handle_cast({'subscribe', JObj}, State) ->
    handle_cast({'subscribe', subscribe_to_record(JObj)}, State);

handle_cast({'notify', JObj}, State) ->
    _ = notify(JObj),
    {'noreply', State};

handle_cast({'after', {'notify', Msg}}, State) ->
    kz_process:spawn(fun on_notify/1, [Msg]),
    {'noreply', State};

handle_cast({'after', {'subscribe', Msg}}, State) ->
    kz_process:spawn(fun on_subscribe/1, [Msg]),
    {'noreply', State};

handle_cast({'after', {'resubscribe', Msg}}, State) ->
    kz_process:spawn(fun on_resubscribe/1, [Msg]),
    {'noreply', State};

handle_cast({'after', _Msg}, State) ->
    {'noreply', State};

handle_cast(_Msg, State) ->
    lager:debug("unhandled info msg : ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec subscribe_to_record(kz_json:object()) -> subscription().
subscribe_to_record(JObj) ->
    {_, U, [Username, Realm]} = omnip_util:extract_user(kz_json:get_value(<<"User">>, JObj)),
    {_, F, _} = omnip_util:extract_user(kz_json:get_value(<<"From">>, JObj, <<>>)),
    Version = case kz_json:get_value(<<"Subscription-ID">>, JObj) of
                  'undefined' -> 1;
                  _Else -> 2
              end,
    #omnip_subscription{user=U
                       ,from=F
                       ,expires=expires(JObj)
                       ,normalized_user=kz_term:to_lower_binary(U)
                       ,normalized_from=kz_term:to_lower_binary(F)
                       ,username=kz_term:to_lower_binary(Username)
                       ,realm=kz_term:to_lower_binary(Realm)
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec subscriptions_to_json(subscriptions()) -> kz_json:objects().
subscriptions_to_json(Subs) ->
    [subscription_to_json(S) || S <- Subs].

-spec subscription_to_json(subscription()) -> kz_json:object().
subscription_to_json(#omnip_subscription{user=User
                                        ,from=From
                                        ,stalker=Stalker
                                        ,expires=Expires
                                        ,timestamp=Timestamp
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
      [{<<"user">>, User}
      ,{<<"from">>, From}
      ,{<<"stalker">>, Stalker}
      ,{<<"expires">>, Expires}
      ,{<<"timestamp">>, Timestamp}
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
      ]).

-spec start_expire_ref() -> reference().
start_expire_ref() ->
    erlang:start_timer(?EXPIRE_SUBSCRIPTIONS, self(), ?EXPIRE_MESSAGE).

-spec expire_old_subscriptions() -> non_neg_integer().
expire_old_subscriptions() ->
    Now = kz_time:now_s(),
    ets:select_delete(table_id(), [{#omnip_subscription{timestamp='$1'
                                                       ,expires='$2'
                                                       ,_='_'
                                                       }
                                   ,[{'>', {'const', Now}, {'+', '$1', '$2'}}]
                                   ,['true']
                                   }]).

-spec find_subscription(kz_term:ne_binary()) ->
          {'ok', subscription()} |
          {'error', 'not_found'}.
find_subscription(CallId) ->
    case ets:lookup(table_id(), CallId) of
        [] -> {'error', 'not_found'};
        [#omnip_subscription{}=Sub] -> {'ok', Sub}
    end.

-spec find_user_subscriptions(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', subscriptions()} |
          {'error', 'not_found'}.
find_user_subscriptions(?OMNIPRESENCE_EVENT_ALL, User) ->
    U = kz_term:to_lower_binary(User),
    MatchSpec = [{#omnip_subscription{normalized_from='$1'
                                     ,_='_'
                                     }
                 ,[{'=:=', '$1', {'const', U}}]
                 ,['$_']
                 }],
    find_subscriptions(MatchSpec);
find_user_subscriptions(Event, User) ->
    U = kz_term:to_lower_binary(User),
    MatchSpec = [{#omnip_subscription{normalized_from='$1'
                                     ,event=Event
                                     ,_='_'
                                     }
                 ,[{'=:=', '$1', {'const', U}}]
                 ,['$_']
                 }],
    find_subscriptions(MatchSpec).

-spec find_subscriptions(ets:match_spec()) -> {'ok', subscriptions()} |
          {'error', 'not_found'}.
find_subscriptions(MatchSpec) ->
    try ets:select(table_id(), MatchSpec) of
        [] -> {'error', 'not_found'};
        Subs -> {'ok', Subs}
    catch
        _E:_M -> lager:error("error fetching subscriptions : ~p : ~p", [_E, _M]),
                 {'error', 'not_found'}
    end.

-spec search_for_subscriptions(kz_term:ne_binary() | '_', kz_term:ne_binary()) -> subscriptions().
search_for_subscriptions(Event, Realm) ->
    MatchSpec =
        #omnip_subscription{realm=kz_term:to_lower_binary(Realm)
                           ,event=Event
                           ,_='_'
                           },
    ets:match_object(table_id(), MatchSpec).

-spec search_for_subscriptions(kz_term:ne_binary() | '_', kz_term:ne_binary(), kz_term:ne_binary() | '_') -> subscriptions().
search_for_subscriptions(Event, Realm, '_') ->
    search_for_subscriptions(Event, Realm);
search_for_subscriptions(Event, Realm, Username) ->
    MatchSpec =
        #omnip_subscription{username=kz_term:to_lower_binary(Username)
                           ,realm=kz_term:to_lower_binary(Realm)
                           ,event=Event
                           ,_='_'
                           },
    ets:match_object(table_id(), MatchSpec).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec expires(integer() | kz_json:object()) -> non_neg_integer().
expires(0) -> 0;
expires(I) when is_integer(I), I >= 0 -> I + ?EXPIRES_FUDGE;
expires(JObj) -> expires(kz_json:get_integer_value(<<"Expires">>, JObj)).

-spec subscribe(subscription()) -> any().
subscribe(#omnip_subscription{from = <<>>}=_S) ->
    lager:debug("subscription with no from: ~p", [subscription_to_json(_S)]);
subscribe(#omnip_subscription{expires=E
                             ,user=_U
                             ,from=_F
                             ,stalker=_S
                             ,call_id=CallId
                             })
  when E =< 0 ->
    case find_subscription(CallId) of
        {'error', 'not_found'} -> 'ok';
        {'ok', #omnip_subscription{timestamp=_T
                                  ,expires=_E
                                  }=O} ->
            lager:debug("unsubscribe ~s/~s (had ~p s left)", [_U, _F, _E - kz_time:elapsed_s(_T)]),
            ets:delete_object(table_id(), O)
    end;
subscribe(#omnip_subscription{user=_U
                             ,from=_F
                             ,expires=E1
                             ,timestamp=T1
                             ,call_id=CallId
                             ,stalker=Stalker
                             ,contact=Contact
                             ,username=Username
                             ,realm=Realm
                             ,event=Event
                             }=S) ->
    case find_subscription(CallId) of
        {'ok', #omnip_subscription{timestamp=_T
                                  ,expires=_E2
                                  }
        } ->
            lager:debug("re-subscribe ~s/~s/~s expires in ~ps(prior remaining ~ps)"
                       ,[_U, _F, CallId, E1, _E2 - kz_time:elapsed_s(_T)]
                       ),
            ets:update_element(table_id(), CallId,
                               [{#omnip_subscription.timestamp, T1}
                               ,{#omnip_subscription.expires, E1}
                               ,{#omnip_subscription.stalker, Stalker}
                               ,{#omnip_subscription.contact, Contact}
                               ]),
            gen_server:cast(self(), {'after', {'resubscribe', {Event, Username, Realm, CallId}}});
        {'error', 'not_found'} ->
            lager:debug("subscribe ~s/~s/~s expires in ~ps", [_U, _F, CallId, E1]),
            ets:insert(table_id(), S),
            gen_server:cast(self(), {'after', {'subscribe', {Event, Username, Realm, CallId}}})
    end.

-spec notify(kz_json:object()) -> 'ok' | {'error', any()}.
notify(JObj) ->
    Sequence = kz_json:get_integer_value(<<"Sequence">>, JObj),
    Reply = kz_json:get_integer_value(<<"Reply">>, JObj),
    Body = kz_json:get_ne_binary_value(<<"Body">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    case find_subscription(CallId) of
        {'ok', #omnip_subscription{normalized_from=From
                                  ,normalized_user=User
                                  ,realm=Realm
                                  ,event = Event
                                  }} ->
            lager:debug("received notify reply ~B for ~s subscription (~s) of ~s to ~s", [Reply, Event, CallId, From, User]),
            ets:update_element(table_id(), CallId,
                               [{#omnip_subscription.last_sequence, Sequence}
                               ,{#omnip_subscription.last_reply, Reply}
                               ,{#omnip_subscription.last_body, Body}
                               ]),
            gen_server:cast(self(), {'after', {'notify', {Event, User, Realm, CallId}}});
        {'error', _} ->
            lager:debug("notify received for nonexistent subscription ~s", [CallId])
    end.

-type msg() :: {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}.
-type exec_fun() :: fun((atom(), msg()) -> 'ok').

-spec on_subscribe(msg()) -> 'ok'.
on_subscribe(Msg) ->
    Routines = [fun maybe_probe/2],
    exec(Routines, 'subscribe', Msg).

-spec on_resubscribe(msg()) -> 'ok'.
on_resubscribe(Msg) ->
    Routines = [fun maybe_probe/2],
    exec(Routines, 'resubscribe', Msg).

-spec on_notify(msg()) -> 'ok'.
on_notify(Msg) ->
    Routines = [],
    exec(Routines, 'notify', Msg).

-spec exec([exec_fun()], atom(), msg()) -> 'ok'.
exec([], _Reason, _Msg) -> 'ok';
exec([Fun|Funs], Reason, Msg) ->
    Fun(Reason, Msg),
    exec(Funs, Reason, Msg).

-spec maybe_probe(atom(), msg()) -> 'ok'.
maybe_probe(_, {<<"message-summary">> = Package, Username, Realm, _}) ->
    omnip_util:request_probe(Package, Username, Realm);
maybe_probe(_, {<<"dialog">>, <<"*", _/binary>> = Username, Realm, _}) ->
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', Account} ->
            VM = ?VM_NUMBER(kzs_util:format_account_id(Account)),
            S = size(VM),
            case Username of
                <<VM:S/binary, New/binary>> -> omnip_util:request_probe(<<"message-summary">>, New, Realm);
                _ -> 'ok'
            end;
        _ -> 'ok'
    end;
maybe_probe(_, _) -> 'ok'.
