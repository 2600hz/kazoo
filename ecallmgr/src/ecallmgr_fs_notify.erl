%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
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
-export([process_message_query_event/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {node :: atom()
                        ,options :: proplist()}).

-define(SERVER, ?MODULE).
-define(MWI_BODY, "~b/~b (~b/~b)").

-define(RESPONDERS, [{{?MODULE, mwi_update}, [{<<"notification">>, <<"mwi">>}]}
                     ,{{?MODULE, presence_update}, [{<<"notification">>, <<"presence_update">>}]}
                    ]).
-define(BINDINGS, [{notifications, [{restrict_to, [mwi_update]}]}]).
-define(QUEUE_NAME, <<"ecallmgr_fs_notify">>).
-define(QUEUE_OPTIONS, [{exclusive, false}]).
-define(CONSUME_OPTIONS, [{exclusive, false}]).

-include_lib("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link/1 :: (atom()) -> startlink_ret().
-spec start_link/2 :: (atom(), wh_proplist()) -> startlink_ret().

start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_listener:start_link(?MODULE
                            ,[{responders, ?RESPONDERS}
                              ,{bindings, ?BINDINGS}
                              ,{queue_name, ?QUEUE_NAME}
                              ,{queue_options, ?QUEUE_OPTIONS}
                              ,{consume_options, ?CONSUME_OPTIONS}
                              ,{basic_qos, 1}
                             ]
                            ,[Node, Options]).

-spec presence_update/2 :: (wh_json:json_object(), proplist()) -> any().
presence_update(JObj, _Props) ->
    do_presence_update(wh_json:get_value(<<"State">>, JObj), JObj).

-spec do_presence_update/2 :: ('undefined' | ne_binary(), wh_json:json_object()) -> any().
do_presence_update(undefined, JObj) ->
    PresenceId = wh_json:get_value(<<"Presence-ID">>, JObj),
    Switch = wh_json:get_string_value(<<"Switch-Nodename">>, JObj),
    case ecallmgr_fs_nodes:channel_match_presence(PresenceId) of
        [] -> 
            Event = empty_presence_event(PresenceId),
            relay_presence('PRESENCE_IN', PresenceId, Event, wh_util:to_list(node()), Switch);
        Channels ->
            case wh_json:get_value(<<"Dialog-State">>, JObj, <<"new">>) of
                <<"new">> ->
                    lists:foreach(fun({CallId, Node}) ->
                                          Channel = wh_json:from_list([{<<"Call-ID">>, CallId}]),
                                          Event = confirmed_presence_event(PresenceId, Channel),
                                          relay_presence('PRESENCE_IN', PresenceId, Event, wh_util:to_list(Node), Switch)
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
    relay_presence('PRESENCE_IN', PresenceId, Event, wh_util:to_list(node()), Switch).

-spec mwi_update/2 :: (wh_json:json_object(), proplist()) -> no_return().
mwi_update(JObj, _Props) ->
    _ = wh_util:put_callid(JObj),
    true = wapi_notifications:mwi_update_v(JObj),
    Node = case wh_json:get_value(<<"Switch-Nodename">>, JObj) of
               undefined ->
                   Username = wh_json:get_value(<<"Notify-User">>, JObj),
                   Realm = wh_json:get_value(<<"Notify-Realm">>, JObj),                   
                   {ok, N} = ecallmgr_registrar:endpoint_node(Realm, Username),
                   N;
               N -> wh_util:to_atom(N)
           end,
    NewMessages = wh_json:get_integer_value(<<"Messages-New">>, JObj, 0),
    MessageAccount = wh_json:get_value(<<"Message-Account">>, JObj),
    Body = io_lib:format(?MWI_BODY, [NewMessages
                                     ,wh_json:get_integer_value(<<"Messages-Saved">>, JObj, 0)
                                     ,wh_json:get_integer_value(<<"Messages-Urgent">>, JObj, 0)
                                     ,wh_json:get_integer_value(<<"Messages-Urgent-Saved">>, JObj, 0)
                                    ]),
    Headers = [{"MWI-Message-Account", wh_json:get_string_value(<<"Message-Account">>, JObj)}
               ,{"MWI-Messages-Waiting", case NewMessages of 0 -> "no"; _ -> "yes" end}
               ,{"MWI-Voice-Message", lists:flatten(Body)}
               ,{"Sofia-Profile", ?DEFAULT_FS_PROFILE}
               ,{"Call-ID", wh_json:get_string_value(<<"Call-ID">>, JObj)}
               ,{"Sub-Call-ID", wh_json:get_string_value(<<"Subscription-Call-ID">>, JObj)}
              ],
    Resp = freeswitch:sendevent(Node, 'MESSAGE_WAITING', props:filter_undefined(Headers)),
    lager:debug("send MWI update for ~s to node ~s: ~p", [MessageAccount, Node, Resp]).

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
    process_flag(trap_exit, true),
    lager:debug("starting new ecallmgr notify process"),
    gproc:reg({p, l, fs_notify}),
    case ecallmgr_config:get(<<"distribute_presence">>, true) of
        false -> ok;
        true ->
            ok = freeswitch:event(Node, ['PRESENCE_IN', 'PRESENCE_OUT', 'PRESENCE_PROBE']),
            lager:debug("bound to presence events on node ~s", [Node]),
            gproc:reg({p, l, {call_event, Node, <<"PRESENCE_IN">>}}),
            gproc:reg({p, l, {call_event, Node, <<"PRESENCE_OUT">>}}),
            gproc:reg({p, l, {call_event, Node, <<"PRESENCE_PROBE">>}}),
            Self = self(),
            spawn(
              fun() ->
                      put(callid, Node),
                      QueueName = <<"ecallmgr_fs_notify_presence">>,
                      QOptions = [{queue_options, [{exclusive, false}]}
                                  ,{consume_options, [{exclusive, false}]}
                                  ,{basic_qos, 1}
                                 ],
                      Bindings= [{notifications, [{restrict_to, [presence_update]}]}],
                      case gen_listener:add_queue(Self, QueueName, QOptions, Bindings) of
                          {ok, _NewQ} -> lager:debug("handling presence updates on queue ~s", [_NewQ]);
                          {error, _E} -> lager:debug("failed to add queue ~s to ~p: ~p", [QueueName, Self, _E])
                      end
              end),
            ok
    end,
    case ecallmgr_config:get(<<"distribute_message_query">>, true) of
        false -> ok;
        true ->
            ok = freeswitch:event(Node, ['MESSAGE_QUERY']),
            gproc:reg({p, l, {call_event, Node, <<"MESSAGE_QUERY">>}}),
            lager:debug("bound to message_query events on node ~s", [Node])
    end,
    {ok, #state{node=Node, options=Options}}.

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
    {reply, {error, not_implemented}, State}.

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
    {noreply, State}.

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
handle_info({event, [_ | Props]}, #state{node=Node}=State) ->
    case props:get_value(<<"Event-Name">>, Props) of
        <<"PRESENCE_PROBE">> ->
            %% New logic: if it is not a presence_probe for a subscription then ignore it...
            case wh_util:is_empty(props:get_value(<<"sub-call-id">>, Props)) of
                true -> {noreply, State, hibernate};
                false ->
                    To = props:get_value(<<"to">>, Props, <<"noname@nodomain">>),
                    From = props:get_value(<<"from">>, Props, <<"noname@nodomain">>),
                    Key = wh_util:to_hex_binary(crypto:md5(<<To/binary, "|", From/binary>>)),
                    Expires = ecallmgr_util:get_expires(Props),
                    case wh_util:is_empty(Expires)  of
                        true -> 
                            %% If the expires was empty or 0 then delete the subscription, might need to 
                            %% remove the specific sub-call-id... lets see how it goes
                            lager:debug("removing sip subscription from '~s' to '~s'", [From, To]),
                            DeleteSpec = [{#sip_subscription{to = '$1', from = '$2', _ = '_'}
                                           ,[{'=:=', '$1', {const, To}}
                                             ,{'=:=', '$2', {const, From}}
                                            ]
                                           ,[true]}
                                         ],
                            ets:select_delete(sip_subscriptions, DeleteSpec),
                            {noreply, State, hibernate};
                        false ->
                            lager:debug("sip subscription from '~s' subscribing to '~s' via node '~s' for ~ps", [From, To, Node, Expires]),
                            ets:insert(sip_subscriptions, #sip_subscription{key=Key
                                                                            ,to=To
                                                                            ,from=From
                                                                            ,node=Node
                                                                            ,expires=Expires
                                                                           }),
                            _ = spawn_link(?MODULE, publish_presence_event, [<<"PRESENCE_PROBE">>, Props, Node]),
                            {noreply, State, hibernate}
                    end
            end;
        <<"PRESENCE_IN">> ->
            _ = case props:get_value(<<"Distributed-From">>, Props) of
                    undefined ->
                        PresenceId = props:get_value(<<"Channel-Presence-ID">>, Props,
                                                     props:get_value(<<"from">>, Props)),
                        spawn_link(?MODULE, relay_presence, ['PRESENCE_IN', PresenceId, Props, Node, undefined]);
                    _Else -> ok
                end,
            {noreply, State, hibernate};
        <<"PRESENCE_OUT">> ->
            _ = case props:get_value(<<"Distributed-From">>, Props) of
                    undefined ->
                        PresenceId = props:get_value(<<"to">>, Props),
                        spawn_link(?MODULE, relay_presence, ['PRESENCE_OUT', PresenceId, Props, Node, undefined]);
                    _Else -> ok
                end,
            {noreply, State, hibernate};
        <<"MESSAGE_QUERY">> ->
            _ = spawn_link(?MODULE, process_message_query_event, [Props, Node]),
            {noreply, State, hibernate};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{node=Node}) ->
    {reply, [{node, Node}]}.

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
    lager:debug("ecallmgr notify ~p termination", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec confirmed_presence_event/2 :: (ne_binary(), wh_json:json_object()) -> proplist().
-spec confirmed_presence_event/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> proplist().

confirmed_presence_event(PresenceId, JObj) ->
    UniqueId = case wh_json:get_ne_value(<<"Call-ID">>, JObj) of
                   undefined  -> wh_util:to_hex_binary(crypto:md5(PresenceId));
                   Else -> Else
               end,
    confirmed_presence_event(PresenceId, UniqueId, JObj).

confirmed_presence_event(PresenceId, UniqueId, JObj) ->
    [{"unique-id", wh_util:to_list(UniqueId)}
     ,{"channel-state", "CS_ROUTING"}
     ,{"answer-state", "confirmed"}
     ,{"proto", "any"}
     ,{"login", "kazoo by 2600hz"}
     ,{"from", wh_util:to_list(PresenceId)}
     ,{"rpid", "unknown"}
     ,{"status", "Active"}
     ,{"event_type", "presence"}
     ,{"alt_event_type", "dialog"}
     ,{"presence-call-direction", wh_json:get_string_value(<<"Direction">>, JObj, "inbound")}
     ,{"Caller-Caller-ID-Number", wh_json:get_string_value(<<"Caller-ID-Number">>, JObj)}
     ,{"Caller-Caller-ID-Name", wh_json:get_string_value(<<"Caller-ID-Name">>, JObj)}
    ].

-spec early_presence_event/2 :: (ne_binary(), wh_json:json_object()) -> proplist().
-spec early_presence_event/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> proplist().

early_presence_event(PresenceId, JObj) ->
    UniqueId = case wh_json:get_ne_value(<<"Call-ID">>, JObj) of
                   undefined  -> wh_util:to_hex_binary(crypto:md5(PresenceId));
                   Else -> Else
               end,
    early_presence_event(PresenceId, UniqueId, JObj).

early_presence_event(PresenceId, UniqueId, JObj) ->
    [{"unique-id", wh_util:to_list(UniqueId)}
     ,{"channel-state", "CS_ROUTING"}
     ,{"answer-state", "early"}
     ,{"proto", "any"}
     ,{"login", "kazoo by 2600hz"}
     ,{"from", wh_util:to_list(PresenceId)}
     ,{"rpid", "unknown"}
     ,{"status", "Active"}
     ,{"event_type", "presence"}
     ,{"alt_event_type", "dialog"}
     ,{"presence-call-direction", wh_json:get_string_value(<<"Direction">>, JObj, "inbound")}
     ,{"Caller-Caller-ID-Number", wh_json:get_string_value(<<"Caller-ID-Number">>, JObj)}
     ,{"Caller-Caller-ID-Name", wh_json:get_string_value(<<"Caller-ID-Name">>, JObj)}
    ].

-spec terminated_presence_event/2 :: (ne_binary(), wh_json:json_object()) -> proplist().
-spec terminated_presence_event/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> proplist().

terminated_presence_event(PresenceId, JObj) ->
    UniqueId = case wh_json:get_ne_value(<<"Call-ID">>, JObj) of
                   undefined  -> wh_util:to_hex_binary(crypto:md5(PresenceId));
                   Else -> Else
               end,
    terminated_presence_event(PresenceId, UniqueId, JObj).

terminated_presence_event(PresenceId, UniqueId, JObj) ->
    [{"unique-id", wh_util:to_list(UniqueId)}
     ,{"channel-state", "CS_HANGUP"}
     ,{"answer-state", "terminated"}
     ,{"proto", "any"}
     ,{"login", "kazoo by 2600hz"}
     ,{"from", wh_util:to_list(PresenceId)}
     ,{"rpid", "unknown"}
     ,{"status", "Inactive"}
     ,{"event_type", "presence"}
     ,{"alt_event_type", "dialog"}
     ,{"presence-call-direction", wh_json:get_string_value(<<"Direction">>, JObj, "inbound")}
     ,{"Caller-Caller-ID-Number", wh_json:get_string_value(<<"Caller-ID-Number">>, JObj)}
     ,{"Caller-Caller-ID-Name", wh_json:get_string_value(<<"Caller-ID-Name">>, JObj)}
    ].

-spec empty_presence_event/1 :: (ne_binary()) -> proplist().
empty_presence_event(PresenceId) ->
    [{"proto", "any"}
     ,{"login", "kazoo by 2600hz"}
     ,{"from", wh_util:to_list(PresenceId)}
     ,{"rpid", "unknown"}
     ,{"status", "Idle"}
     ,{"event_type", "presence"}
     ,{"alt_event_type", "dialog"}
     ,{"force-full-dialog", "true"}
    ].    

-spec publish_presence_event/3 :: (ne_binary(), proplist(), ne_binary() | atom()) -> 'ok'.
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
    wapi_notifications:publish_presence_probe(Req).

-spec relay_presence/5 :: (atom(), ne_binary(), proplist(), atom(), 'undefined' | ne_binary()) -> [fs_sendevent_ret(),...].
relay_presence(EventName, PresenceId, Props, Node, undefined) ->
    Match = #sip_subscription{key='_'
                              ,to=PresenceId
                              ,from='_'
                              ,node='$1'
                              ,expires='_'
                              ,timestamp='_'
                             },
    Subs = lists:concat(ets:match(sip_subscriptions, Match)),
    Headers = [{"Distributed-From", wh_util:to_list(Node)}
               |[{wh_util:to_list(K), wh_util:to_list(V)}
                 || {K, V} <- lists:foldr(fun(Header, Prop) ->
                                                  proplists:delete(Header, Prop)
                                          end, Props, ?FS_DEFAULT_HDRS)
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
                                          end, Props, ?FS_DEFAULT_HDRS)
                ]
              ],
    freeswitch:sendevent(wh_util:to_atom(Switch), EventName, Headers).

-spec process_message_query_event/2 :: (proplist(), atom()) -> 'ok'.
process_message_query_event(Data, Node) ->
    MessageAccount = props:get_value(<<"VM-User">>, Data, props:get_value(<<"Message-Account">>, Data)),
    case re:run(MessageAccount, <<"(?:sip:)?(.*)@(.*)$">>, [{capture, all, binary}]) of
        {match, [_, Username, Realm]} ->
            lager:debug("publishing message query for ~s@~s", [Username, Realm]),
            Query = [{<<"Username">>, Username}
                     ,{<<"Realm">>, Realm}
                     ,{<<"Call-ID">>, props:get_value(<<"VM-Call-ID">>, Data)}
                     ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
                     ,{<<"Subscription-Call-ID">>, props:get_value(<<"VM-sub-call-id">>, Data)}
                     ,{<<"Message-Account">>, props:get_value(<<"Message-Account">>, Data)}    
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_notifications:publish_mwi_query(Query),
            ok;
        _Else ->
            lager:debug("unknown message query format: ~p", [MessageAccount]),
            ok
    end.
