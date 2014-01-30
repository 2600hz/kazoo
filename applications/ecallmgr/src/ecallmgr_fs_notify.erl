%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Notify-type requests, like MWI updates, received and processed here
%%% @end
%%% @contributors
%%%    Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_notify).

-behaviour(gen_listener).

-export([start_link/1, start_link/2]).
-export([presence_update/2]).
-export([mwi_update/2]).
-export([relay_presence/5]).
-export([publish_presence_event/3]).
-export([handle_message_query/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {
          node :: atom()
          ,options :: wh_proplist()
         }).

-define(SERVER, ?MODULE).
-define(MWI_BODY, "Message-Account: sip:~s\r\nMessages-Waiting: ~s\r\nVoice-Message: ~b/~b (~b/~b)\r\n\r\n").

-define(BINDINGS, [{'notifications', [{'restrict_to', ['mwi_update']}]}]).
-define(RESPONDERS, [{{?MODULE, 'mwi_update'}
                      ,[{<<"notification">>, <<"mwi">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<"ecallmgr_fs_notify">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-include_lib("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), wh_proplist()) -> startlink_ret().

start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_listener:start_link(?MODULE
                            ,[{'responders', ?RESPONDERS}
                              ,{'bindings', ?BINDINGS}
                              ,{'queue_name', ?QUEUE_NAME}
                              ,{'queue_options', ?QUEUE_OPTIONS}
                              ,{'consume_options', ?CONSUME_OPTIONS}
                             ]
                            ,[Node, Options]).

-spec presence_update(wh_json:object(), wh_proplist()) -> any().
presence_update(JObj, _Props) ->
    do_presence_update(wh_json:get_value(<<"State">>, JObj), JObj),
    whistle_stats:increment_counter("presence").

-spec do_presence_update(api_binary(), wh_json:object()) -> any().
do_presence_update('undefined', JObj) ->
    PresenceId = wh_json:get_value(<<"Presence-ID">>, JObj),
    Switch = wh_json:get_value(<<"Switch-Nodename">>, JObj),
    case ecallmgr_fs_channels:match_presence(PresenceId) of
        [] ->
            Event = empty_presence_event(PresenceId),
            relay_presence('PRESENCE_IN', PresenceId, Event, node(), Switch);
        Channels ->
            case wh_json:get_value(<<"Dialog-State">>, JObj, <<"new">>) of
                <<"new">> ->
                    lists:foreach(fun({CallId, Node}) ->
                                          Channel = wh_json:from_list([{<<"Call-ID">>, CallId}]),
                                          Event = confirmed_presence_event(PresenceId, Channel),
                                          relay_presence('PRESENCE_IN', PresenceId, Event, Node, Switch)
                                  end, Channels);
                _Else ->
                    lager:info("skipping channel updates for ~s subscription dialog", [_Else])
            end
    end;
do_presence_update(State, JObj) ->
    PresenceId = wh_json:get_value(<<"Presence-ID">>, JObj),
    Switch = wh_json:get_string_value(<<"Switch-Nodename">>, JObj),
    Event = case State of
                <<"early">> -> early_presence_event(PresenceId, JObj);
                <<"confirmed">> -> confirmed_presence_event(PresenceId, JObj);
                <<"terminated">> -> terminated_presence_event(PresenceId, JObj)
            end,
    relay_presence('PRESENCE_IN', PresenceId, Event, node(), Switch).

-spec mwi_update(wh_json:object(), wh_proplist()) -> no_return().
mwi_update(JObj, Props) ->
    _ = wh_util:put_callid(JObj),
    'true' = wapi_notifications:mwi_update_v(JObj),
    Username = wh_json:get_value(<<"Notify-User">>, JObj),
    Realm = wh_json:get_value(<<"Notify-Realm">>, JObj),
    case ecallmgr_registrar:lookup_contact(Realm, Username) of
        {'error', 'not_found'} ->
            lager:warning("failed to find contact for ~s@~s, dropping MWI update", [Username, Realm]);
        {'ok', Contact} ->
            Node = props:get_value('node', Props),
            send_mwi_update(JObj, Node, Username, Realm, Contact)
    end. 

send_mwi_update(JObj, Node, Username, Realm, Contact) ->
    NewMessages = wh_json:get_integer_value(<<"Messages-New">>, JObj, 0),
    Body = io_lib:format(?MWI_BODY, [<<Username/binary, "@", Realm/binary>>
                                     ,case NewMessages of 0 -> "no"; _ -> "yes" end
                                     ,NewMessages
                                     ,wh_json:get_integer_value(<<"Messages-Saved">>, JObj, 0)
                                     ,wh_json:get_integer_value(<<"Messages-Urgent">>, JObj, 0)
                                     ,wh_json:get_integer_value(<<"Messages-Urgent-Saved">>, JObj, 0)
                                    ]),
    Headers = [{"profile", ?DEFAULT_FS_PROFILE}
               ,{"contact", Contact}
               ,{"to-uri", <<"sip:", Username/binary, "@", Realm/binary>>}
               ,{"from-uri", <<"sip:", Username/binary, "@", Realm/binary>>}
               ,{"event-str", "message-summary"}
               ,{"content-type", "application/simple-message-summary"}
               ,{"content-length", wh_util:to_list(length(Body))}
               ,{"body", lists:flatten(Body)}
              ],
    Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
    lager:debug("sent MWI update to '~s@~s' via ~s: ~p", [Username, Realm, Node, Resp]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    put(callid, Node),
    lager:debug("starting new ecallmgr notify process"),
    gproc:reg({'p', 'l', 'fs_notify'}),
    gen_server:cast(self(), 'bind_to_presence_events'),
    {'ok', #state{node=Node, options=Options}}.

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
handle_cast('bind_to_presence_events', #state{node=Node}=State) ->
    _ = case ecallmgr_config:get_boolean(<<"distribute_presence">>, 'false') of
            'false' -> gen_server:cast(self(), 'bind_to_message_events');
            'true' ->
                %%                ok = freeswitch:event(Node, ['PRESENCE_IN', 'PRESENCE_OUT', 'PRESENCE_PROBE']),
                'true' = gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"PRESENCE_IN">>)}),
                'true' = gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"PRESENCE_OUT">>)}),
                'true' = gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"PRESENCE_PROBE">>)}),
                gen_server:cast(self(), 'bind_to_notify_presence')
        end,
    {'noreply', State};
handle_cast('bind_to_notify_presence', #state{node=Node}=State) ->
    Self = self(),
    spawn(fun() ->
                  put('callid', Node),
                  QueueName = <<"ecallmgr_fs_notify_presence">>,
                  Options = [{'queue_options', [{'exclusive', 'false'}]}
                             ,{'consume_options', [{'exclusive', 'false'}]}
                            ],
                  Bindings= [{'notifications', [{'restrict_to', ['presence_update']}]}],
                  case gen_listener:add_queue(Self, QueueName, Options, Bindings) of
                      {'ok', _NewQ} -> lager:debug("handling presence updates on queue ~s", [_NewQ]);
                      {'error', _E} -> lager:debug("failed to add queue ~s to ~p: ~p", [QueueName, Self, _E])
                  end
          end),
    gen_server:cast(self(), 'bind_to_message_events'),
    {'noreply', State};
handle_cast('bind_to_message_events', #state{node=Node}=State) ->
    _ = case ecallmgr_config:get_boolean(<<"distribute_message_query">>, 'false') of
            'false' -> 'ok';
            'true' ->
                %%            ok = freeswitch:event(Node, ['MESSAGE_QUERY']),
                gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"MESSAGE_QUERY">>)})
        end,
     _ = case ecallmgr_config:get(<<"distribute_recv_message">>, 'false') of
        false -> ok;
        true ->
            %% ok = freeswitch:event(Node, ['RECV_MESSAGE']),
            gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"MESSAGE">>)}),
            lager:debug("bound to recv_message events on node ~s", [Node])
    end,
    {'noreply', State};
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
handle_info({'event', [_ | Props]}, #state{node=Node}=State) ->
    _ = case props:get_value(<<"Event-Name">>, Props) of
            <<"PRESENCE_PROBE">> -> maybe_handle_presence_probe(Props, Node);
            <<"PRESENCE_IN">> -> maybe_handle_presence_in(Props, Node);
            <<"PRESENCE_OUT">> -> maybe_handle_presence_out(Props, Node);
            <<"MESSAGE_QUERY">> -> spawn_link(?MODULE, 'handle_message_query', [Props, Node]);
            <<"MESSAGE">> -> spawn_link(ecallmgr_message, process_msg, [Props, Node]);
            _ -> 'ok'
        end,
    {'noreply', State, 'hibernate'};
handle_info({'EXIT', _, _}, State) ->
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', Node}]}.

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
terminate(_Reason, #state{node=Node}) ->
    lager:info("notify listener for ~s terminating: ~p", [Node, _Reason]).

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
maybe_handle_presence_probe(Props, Node) ->
    %% New logic: if it is not a presence_probe for a subscription then ignore it...
    case wh_util:is_empty(props:get_value(<<"sub-call-id">>, Props)) of
        'true' -> 'ok';
        'false' -> handle_presence_probe(Props, Node)
    end.

handle_presence_probe(Props, Node) ->
    To = props:get_value(<<"to">>, Props, <<"noname@nodomain">>),
    From = props:get_value(<<"from">>, Props, <<"noname@nodomain">>),
    Key = wh_util:to_hex_binary(crypto:md5(<<To/binary, "|", From/binary>>)),
    Expires = ecallmgr_util:get_expires(Props),
    case wh_util:is_empty(Expires)  of
        'true' ->
            %% If the expires was empty or 0 then delete the subscription, might need to
            %% remove the specific sub-call-id... lets see how it goes
            lager:debug("removing sip subscription from '~s' to '~s'", [From, To]),
            DeleteSpec = [{#sip_subscription{to = '$1', from = '$2', _ = '_'}
                           ,[{'=:=', '$1', {'const', To}}
                             ,{'=:=', '$2', {'const', From}}
                            ]
                           ,['true']}
                         ],
            ets:select_delete('sip_subscriptions', DeleteSpec);
        'false' ->
            lager:debug("sip subscription from '~s' subscribing to '~s' via node '~s' for ~ps", [From, To, Node, Expires]),
            ets:insert('sip_subscriptions', #sip_subscription{key=Key
                                                              ,to=To
                                                              ,from=From
                                                              ,node=Node
                                                              ,expires=Expires
                                                             }),
            spawn_link(?MODULE, 'publish_presence_event', [<<"PRESENCE_PROBE">>, Props, Node])
    end.

maybe_handle_presence_in(Props, Node) ->
    case props:get_value(<<"Distributed-From">>, Props) of
        'undefined' ->
            PresenceId = props:get_value(<<"Channel-Presence-ID">>, Props,
                                         props:get_value(<<"from">>, Props)),
            spawn_link(?MODULE, 'relay_presence', ['PRESENCE_IN', PresenceId, Props, Node, 'undefined']);
        _Else -> 'ok'
    end.

maybe_handle_presence_out(Props, Node) ->
    case props:get_value(<<"Distributed-From">>, Props) of
        'undefined' ->
            PresenceId = props:get_value(<<"to">>, Props),
            spawn_link(?MODULE, 'relay_presence', ['PRESENCE_OUT', PresenceId, Props, Node, 'undefined']);
        _Else -> 'ok'
    end.

-spec confirmed_presence_event(ne_binary(), wh_json:object()) -> wh_proplist().
-spec confirmed_presence_event(ne_binary(), ne_binary(), wh_json:object()) -> wh_proplist().

confirmed_presence_event(PresenceId, JObj) ->
    UniqueId = case wh_json:get_ne_value(<<"Call-ID">>, JObj) of
                   'undefined' -> wh_util:to_hex_binary(crypto:md5(PresenceId));
                   Else -> Else
               end,
    confirmed_presence_event(PresenceId, UniqueId, JObj).

confirmed_presence_event(PresenceId, UniqueId, JObj) ->
    [Number, _] = binary:split(PresenceId, <<"@">>),
    [{"unique-id", wh_util:to_list(UniqueId)}
     ,{"channel-state", "CS_ROUTING"}
     ,{"answer-state", "confirmed"}
     ,{"proto", "any"}
     ,{"login", "kazoo by 2600hz"}
     ,{"from", wh_util:to_list(PresenceId)}
     ,{"variable_sip_to_user", wh_util:to_list(Number)}
     ,{"variable_sip_from_user", wh_util:to_list(Number)}
     ,{"rpid", "unknown"}
     ,{"status", "Active"}
     ,{"event_type", "presence"}
     ,{"alt_event_type", "dialog"}
     ,{"presence-call-direction", wh_json:get_string_value(<<"Direction">>, JObj, "inbound")}
     ,{"Caller-Caller-ID-Number", wh_json:get_string_value(<<"Caller-ID-Number">>, JObj)}
     ,{"Caller-Caller-ID-Name", wh_json:get_string_value(<<"Caller-ID-Name">>, JObj)}
    ].

-spec early_presence_event(ne_binary(), wh_json:object()) -> wh_proplist().
-spec early_presence_event(ne_binary(), ne_binary(), wh_json:object()) -> wh_proplist().

early_presence_event(PresenceId, JObj) ->
    UniqueId = case wh_json:get_ne_value(<<"Call-ID">>, JObj) of
                   'undefined' -> wh_util:to_hex_binary(crypto:md5(PresenceId));
                   Else -> Else
               end,
    early_presence_event(PresenceId, UniqueId, JObj).

early_presence_event(PresenceId, UniqueId, JObj) ->
    [Number, _] = binary:split(PresenceId, <<"@">>),
    [{"unique-id", wh_util:to_list(UniqueId)}
     ,{"channel-state", "CS_ROUTING"}
     ,{"answer-state", "early"}
     ,{"proto", "any"}
     ,{"login", "kazoo by 2600hz"}
     ,{"from", wh_util:to_list(PresenceId)}
     ,{"variable_sip_to_user", wh_util:to_list(Number)}
     ,{"variable_sip_from_user", wh_util:to_list(Number)}
     ,{"rpid", "unknown"}
     ,{"status", "Active"}
     ,{"event_type", "presence"}
     ,{"alt_event_type", "dialog"}
     ,{"presence-call-direction", wh_json:get_string_value(<<"Direction">>, JObj, "inbound")}
     ,{"Caller-Caller-ID-Number", wh_json:get_string_value(<<"Caller-ID-Number">>, JObj)}
     ,{"Caller-Caller-ID-Name", wh_json:get_string_value(<<"Caller-ID-Name">>, JObj)}
    ].

-spec terminated_presence_event(ne_binary(), wh_json:object()) -> wh_proplist().
-spec terminated_presence_event(ne_binary(), ne_binary(), wh_json:object()) -> wh_proplist().

terminated_presence_event(PresenceId, JObj) ->
    UniqueId = case wh_json:get_ne_value(<<"Call-ID">>, JObj) of
                   'undefined' -> wh_util:to_hex_binary(crypto:md5(PresenceId));
                   Else -> Else
               end,
    terminated_presence_event(PresenceId, UniqueId, JObj).

terminated_presence_event(PresenceId, UniqueId, JObj) ->
    [Number, _] = binary:split(PresenceId, <<"@">>),
    [{"unique-id", wh_util:to_list(UniqueId)}
     ,{"channel-state", "CS_HANGUP"}
     ,{"answer-state", "terminated"}
     ,{"proto", "any"}
     ,{"login", "kazoo by 2600hz"}
     ,{"from", wh_util:to_list(PresenceId)}
     ,{"variable_sip_to_user", wh_util:to_list(Number)}
     ,{"variable_sip_from_user", wh_util:to_list(Number)}
     ,{"rpid", "unknown"}
     ,{"status", "Inactive"}
     ,{"event_type", "presence"}
     ,{"alt_event_type", "dialog"}
     ,{"presence-call-direction", wh_json:get_string_value(<<"Direction">>, JObj, "inbound")}
     ,{"Caller-Caller-ID-Number", wh_json:get_string_value(<<"Caller-ID-Number">>, JObj)}
     ,{"Caller-Caller-ID-Name", wh_json:get_string_value(<<"Caller-ID-Name">>, JObj)}
    ].

-spec empty_presence_event(ne_binary()) -> wh_proplist().
empty_presence_event(PresenceId) ->
    [Number, _] = binary:split(PresenceId, <<"@">>),
    [{"proto", "any"}
     ,{"login", "kazoo by 2600hz"}
     ,{"from", wh_util:to_list(PresenceId)}
     ,{"variable_sip_to_user", wh_util:to_list(Number)}
     ,{"variable_sip_from_user", wh_util:to_list(Number)}
     ,{"rpid", "unknown"}
     ,{"status", "Idle"}
     ,{"event_type", "presence"}
     ,{"alt_event_type", "dialog"}
     ,{"force-full-dialog", "true"}
    ].

-spec publish_presence_event(ne_binary(), wh_proplist(), ne_binary() | atom()) -> 'ok'.
publish_presence_event(EventName, Props, Node) ->
    From = props:get_value(<<"from">>, Props, <<"nouser@nodomain">>),
    To = props:get_value(<<"to">>, Props, <<"nouser@nodomain">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    Req = [{<<"From">>, From}
           ,{<<"From-User">>, FromUser}
           ,{<<"From-Realm">>, FromRealm}
           ,{<<"To">>, To}
           ,{<<"To-User">>, ToUser}
           ,{<<"To-Realm">>, ToRealm}
           ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
           ,{<<"Expires">>, props:get_value(<<"expires">>, Props)}
           ,{<<"Subscription-Call-ID">>, props:get_value(<<"sub-call-id">>, Props)}
           ,{<<"Subscription-Type">>, props:get_value(<<"alt_event_type">>, Props)}
           ,{<<"Subscription">>, props:get_value(<<"proto-specific-event-name">>, Props)}
           ,{<<"Dialog-State">>, props:get_value(<<"dialog_state">>, Props)}
           | wh_api:default_headers(<<>>, <<"notification">>, wh_util:to_lower_binary(EventName), ?APP_NAME, ?APP_VERSION)
          ],
    wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                        ,Req
                        ,fun wapi_notifications:publish_presence_probe/1
                       ).

-spec relay_presence(atom(), ne_binary(), wh_proplist(), atom(), api_binary()) -> [fs_sendevent_ret(),...].
relay_presence(EventName, PresenceId, Props, Node, undefined) ->
    Match = #sip_subscription{to=PresenceId
                              ,node='$1'
                              ,_ = '_'
                             },
    Subs = lists:concat(ets:match('sip_subscriptions', Match)),
    Headers = [{"Distributed-From", wh_util:to_list(Node)}
               |[{wh_util:to_list(K), wh_util:to_list(V)}
                 || {K, V} <- lists:foldr(fun(Header, Prop) ->
                                                  proplists:delete(Header, Prop)
                                          end, Props, ?FS_DEFAULT_HDRS),
                    not is_tuple(V)
                ]
              ],
    [begin
         lager:debug("relay presence event from '~s' to '~s'", [Node, Switch]),
         freeswitch:sendevent(Switch, EventName, Headers)
     end
     || Switch <- sets:to_list(sets:del_element(Node, sets:from_list(Subs)))
    ];
relay_presence(_, _, _, Switch, Switch) ->
    lager:info("skipping presence relay for a call on the same node as the intended recipient (~s):", [Switch]);
relay_presence(EventName, _, Props, Node, Switch) ->
    lager:debug("send presence event to '~s'", [Switch]),
    Headers = [{"Distributed-From", wh_util:to_list(Node)}
               |[{wh_util:to_list(K), wh_util:to_list(V)}
                 || {K, V} <- lists:foldr(fun(Header, Prop) ->
                                                  proplists:delete(Header, Prop)
                                          end, Props, ?FS_DEFAULT_HDRS),
                    not is_tuple(V)
                ]
              ],
    freeswitch:sendevent(wh_util:to_atom(Switch, true), EventName, Headers).

-spec handle_message_query(wh_proplist(), atom()) -> 'ok'.
handle_message_query(Data, Node) ->
    MessageAccount = props:get_value(<<"VM-User">>, Data, props:get_value(<<"Message-Account">>, Data)),
    case re:run(MessageAccount, <<"(?:sip:)?(.*)@(.*)$">>, [{'capture', 'all', 'binary'}]) of
        {'match', [_, Username, Realm]} ->
            lager:debug("publishing message query for ~s@~s", [Username, Realm]),
            Query = [{<<"Username">>, Username}
                     ,{<<"Realm">>, Realm}
                     ,{<<"Call-ID">>, props:get_value(<<"VM-Call-ID">>, Data)}
                     ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
                     ,{<<"Subscription-Call-ID">>, props:get_value(<<"VM-sub-call-id">>, Data)}
                     ,{<<"Message-Account">>, props:get_value(<<"Message-Account">>, Data)}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            _ = wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                                    ,Query
                                    ,fun wapi_notifications:publish_mwi_query/1
                                   ),
            'ok';
        _Else ->
            lager:debug("unknown message query format: ~p", [MessageAccount]),
            'ok'
    end.
