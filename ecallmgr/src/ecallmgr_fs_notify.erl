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
-export([relay_presence/4]).
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
-define(MWI_BODY, "Messages-Waiting: ~s\r\nMessage-Account: sip:~s\r\nVoice-Message: ~b/~b (~b/~b)\r\n\r\n").

-define(RESPONDERS, [{{?MODULE, mwi_update}, [{<<"notification">>, <<"mwi">>}]}
                     ,{{?MODULE, presence_update}, [{<<"notification">>, <<"presence_update">>}]}
                    ]).
-define(BINDINGS, [{notifications, [{restrict_to, [mwi_update, presence_update]}]}]).
-define(QUEUE_NAME, <<"ecallmgr_notify">>).
-define(QUEUE_OPTIONS, [{exclusive, false}]).
-define(CONSUME_OPTIONS, [{exclusive, false}]).

-include("ecallmgr.hrl").

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

-spec presence_update/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
presence_update(JObj, Props) ->
    PresenceId = wh_json:get_value(<<"Presence-ID">>, JObj),
    Event = case wh_json:get_value(<<"State">>, JObj) of
                undefined ->
                    case ecallmgr_fs_nodes:channel_match_presence(PresenceId) of
                        [] ->
                            create_presence_in(PresenceId, "Available", undefined, wh_json:new());
                        [{CallId, _}|_] ->
                            Channel = wh_json:set_values([{<<"Channel-State">>, <<"CS_EXECUTE">>}
                                                          ,{<<"Call-ID">>, CallId}
                                                         ], wh_json:new()),
                            create_presence_in(PresenceId, "answered", "confirmed", Channel)
                    end;
                <<"early">> -> create_presence_in(PresenceId, "CS_ROUTING", "early", JObj);
                <<"confirmed">> -> create_presence_in(PresenceId, "CS_ROUTING", "confirmed", JObj);
                <<"answered">> -> create_presence_in(PresenceId, "answered", "confirmed", JObj);
                <<"terminated">> -> create_presence_in(PresenceId, "CS_ROUTING", "terminated", JObj);
                _ -> create_presence_in(PresenceId, "Available", undefined, wh_json:new())
            end,
    Node = props:get_value(node, Props),
    lager:debug("sending presence in event to ~p", [Node]),
    relay_presence('PRESENCE_IN', PresenceId, Event, 'nonode@nodomain').

-spec mwi_update/2 :: (wh_json:json_object(), proplist()) -> no_return().
mwi_update(JObj, _Props) ->
    _ = wh_util:put_callid(JObj),
    true = wapi_notifications:mwi_update_v(JObj),
    Username = wh_json:get_value(<<"Notify-User">>, JObj),
    Realm  = wh_json:get_value(<<"Notify-Realm">>, JObj),
    case get_endpoint(Username, Realm) of
        {error, _R} ->
            lager:debug("MWI update error ~s while fetching contact for ~s@~s", [_R, Username, Realm]);
        Endpoint ->
            NewMessages = wh_json:get_integer_value(<<"Messages-New">>, JObj, 0),
            Body = io_lib:format(?MWI_BODY, [case NewMessages of 0 -> "no"; _ -> "yes" end
                                             ,<<Username/binary, "@", Realm/binary>>
                                                 ,NewMessages
                                             ,wh_json:get_integer_value(<<"Messages-Saved">>, JObj, 0)
                                             ,wh_json:get_integer_value(<<"Messages-Urgent">>, JObj, 0)
                                             ,wh_json:get_integer_value(<<"Messages-Urgent-Saved">>, JObj, 0)
                                            ]),
            lager:debug("created mwi notify body ~s", [Body]),
            Headers = [{"profile", ?DEFAULT_FS_PROFILE}
                       ,{"to-uri", Endpoint}
                       ,{"from-uri", "sip:2600hz@2600hz.com"}
                       ,{"event-str", "message-summary"}
                       ,{"content-type", "application/simple-message-summary"}
                       ,{"content-length", wh_util:to_list(length(Body))}
                       ,{"body", lists:flatten(Body)}
                      ],
            {ok, Node} = ecallmgr_registrar:endpoint_node(Realm, Username),
            Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
            lager:debug("sent MWI update to '~s@~s' via ~s: ~p", [Username, Realm, Node, Resp])
    end.

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
            gproc:reg({p, l, {call_event, Node, <<"PRESENCE_IN">>}}),
            gproc:reg({p, l, {call_event, Node, <<"PRESENCE_OUT">>}}),
            gproc:reg({p, l, {call_event, Node, <<"PRESENCE_PROBE">>}}),
            lager:debug("bound to presence events on node ~s", [Node])
    end,
    case ecallmgr_config:get(<<"distribute_message_query">>, false) of
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
            To = props:get_value(<<"to">>, Props, <<"noname@nodomain">>),
            From = props:get_value(<<"from">>, Props, <<"noname@nodomain">>),
            Key = wh_util:to_hex_binary(crypto:md5(<<To/binary, "|", From/binary>>)),
            Expires = ecallmgr_util:get_expires(Props),
            lager:debug("sip subscription from '~s' subscribing to '~s' via node '~s' for ~ps", [From, To, Node, Expires]),
            ets:insert(sip_subscriptions, #sip_subscription{key=Key
                                                            ,to=To
                                                            ,from=From
                                                            ,node=Node
                                                            ,expires=Expires
                                                           }),
            spawn_link(?MODULE, publish_presence_event, [<<"PRESENCE_PROBE">>, Props, Node]),
            {noreply, State, hibernate};
        <<"PRESENCE_IN">> ->
            case props:get_value(<<"Distributed-From">>, Props) of
                undefined ->
                    PresenceId = props:get_value(<<"Channel-Presence-ID">>, Props,
                                                 props:get_value(<<"from">>, Props)),
                    spawn_link(?MODULE, relay_presence, ['PRESENCE_IN', PresenceId, Props, Node]);
                _Else -> ok
            end,
            {noreply, State, hibernate};
        <<"PRESENCE_OUT">> ->
            case props:get_value(<<"Distributed-From">>, Props) of
                undefined ->
                    PresenceId = props:get_value(<<"to">>, Props),
                    spawn_link(?MODULE, relay_presence, ['PRESENCE_OUT', PresenceId, Props, Node]);
                _Else -> ok
            end,
            {noreply, State, hibernate};
        <<"MESSAGE_QUERY">> ->
            spawn_link(?MODULE, process_message_query_event, [Props, Node]),
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
    lager:debug("ecallmgr notify ~p termination", [_Reason]),
    ok.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Request the current registration contact string for the user/realm
%% replacing everything before the first '@' with:
%% 'sip:{User}'
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint/2 :: (ne_binary(), ne_binary()) -> {'error', 'timeout'} | nonempty_string().
get_endpoint(Username, Realm) ->
    case ecallmgr_registrar:lookup_contact(Realm, Username, false) of
        {ok, Contact} ->
            RURI = binary:replace(re:replace(Contact, "^[^\@]+", Username, [{return, binary}]), <<">">>, <<"">>),
            wh_util:to_list(<<"sip:", (RURI)/binary>>);
        {error, timeout}=E ->
            E
    end.

-spec create_presence_in/4 :: (ne_binary(), undefined | string(), undefined | string(), wh_json:json_object()) -> proplist().
create_presence_in(PresenceId, Status, State, JObj) ->
    lager:debug("creating presence in event for '~s' with status ~s and state ~s", [PresenceId, Status, State]),
    [KV || {_, V}=KV <- [{"unique-id", wh_json:get_string_value(<<"Call-ID">>, JObj)}
                         ,{"channel-state", wh_json:get_string_value(<<"Channel-State">>, JObj, State)}
                         ,{"answer-state", wh_util:to_list(State)}
                         ,{"proto", "any"}
                         ,{"login", "src/mod/event_handlers/mod_erlang_event/handle_msg.c"}
                         ,{"from", wh_util:to_list(PresenceId)}
                         ,{"rpid", "unknown"}
                         ,{"status", wh_util:to_list(Status)}
                         ,{"event_type", "presence"}
                         ,{"alt_event_type", "dialog"}
                         ,{"presence-call-direction", "outbound"}
                        ]
               ,V =/= undefined
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
           ,{<<"Node">>, wh_util:to_binary(Node)}
           ,{<<"Expires">>, props:get_value(<<"expires">>, Props)}
           ,{<<"Subscription-Call-ID">>, props:get_value(<<"sub-call-id">>, Props)}
           ,{<<"Subscription-Type">>, props:get_value(<<"alt_event_type">>, Props)}
           ,{<<"Subscription">>, props:get_value(<<"proto-specific-event-name">>, Props)}
           | wh_api:default_headers(<<>>, <<"notification">>, wh_util:to_lower_binary(EventName), ?APP_NAME, ?APP_VERSION)
          ],
    wapi_notifications:publish_presence_probe(Req).

-spec relay_presence/4 :: (atom(), ne_binary(), proplist(), atom()) -> term().
relay_presence(EventName, PresenceId, Props, Node) ->
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
    ].

-spec process_message_query_event/2 :: (proplist(), atom()) -> 'ok'.
process_message_query_event(Data, Node) ->
    MessageAccount = props:get_value(<<"Message-Account">>, Data),
    case re:run(MessageAccount, <<"(?:sip:)?(.*)@(.*)$">>, [{capture, all, binary}]) of
        {match, [_, Username, Realm]} ->
            lager:debug("publishing message query for ~s@~s", [Username, Realm]),
            Query = [{<<"Username">>, Username}
                     ,{<<"Realm">>, Realm}
                     ,{<<"Call-ID">>, props:get_value(<<"VM-Call-ID">>, Data)}
                     ,{<<"Node">>, wh_util:to_binary(Node)}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ],
            wapi_notifications:publish_mwi_query(Query),
            ok;
        _Else ->
            lager:debug("unknown message query format: ~p", [MessageAccount]),
            ok
    end.
