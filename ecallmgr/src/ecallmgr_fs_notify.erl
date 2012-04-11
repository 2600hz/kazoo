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
-export([distributed_presence/3]).
-export([presence_update/2]).
-export([mwi_update/2]).
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
-spec start_link/2 :: (atom(), proplist()) -> startlink_ret().

start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_listener:start_link(?MODULE,
                            [{responders, ?RESPONDERS}
                             ,{bindings, ?BINDINGS}
                             ,{queue_name, ?QUEUE_NAME}
                             ,{queue_options, ?QUEUE_OPTIONS}
                             ,{consume_options, ?CONSUME_OPTIONS}
                             ,{basic_qos, 1}
                            ], [Node, Options]).

-spec distributed_presence/3 :: (pid(), ne_binary(), proplist()) -> 'ok'.
distributed_presence(Srv, Type, Event) ->
    gen_server:cast(Srv, {distributed_presence, Type, Event}).

-spec presence_update/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
presence_update(JObj, Props) ->
    PresenceId = wh_json:get_value(<<"Presence-ID">>, JObj),
    Event = case wh_json:get_value(<<"State">>, JObj) of
                undefined ->
                    Channels = ecallmgr_fs_query:channel_query(wh_json:from_list([{<<"Presence-ID">>, PresenceId}])),
                    case try_find_ringing_channel(Channels) of
                        undefined -> 
                            create_presence_in(PresenceId, "Available", undefined, wh_json:new());
                        Channel -> 
                            State = wh_json:get_string_value(<<"Answer-State">>, Channel),
                            Status = case State of <<"answered">> -> "answered"; _Else -> "CS_ROUTING" end, 
                            create_presence_in(PresenceId, Status, State, Channel)
                    end;
                <<"early">> -> create_presence_in(PresenceId, "CS_ROUTING", "early", JObj);
                <<"confirmed">> -> create_presence_in(PresenceId, "CS_ROUTING", "confirmed", JObj);
                <<"answered">> -> create_presence_in(PresenceId, "answered", "confirmed", JObj);
                <<"terminated">> -> create_presence_in(PresenceId, "CS_ROUTING", "terminated", JObj);
                _ -> create_presence_in(PresenceId, "Available", undefined, wh_json:new())
            end,
    Node = props:get_value(node, Props),
    lager:debug("sending presence in event to ~p~n", [Node]),
    ok = freeswitch:sendevent(Node, 'PRESENCE_IN', [{"Distributed-From", wh_util:to_list(Node)} | Event]).
    

-spec mwi_update/2 :: (wh_json:json_object(), proplist()) -> no_return().
mwi_update(JObj, Props) ->
    _ = wh_util:put_callid(JObj),
    true = wapi_notifications:mwi_update_v(JObj),
    User = wh_json:get_value(<<"Notify-User">>, JObj),
    Realm  = wh_json:get_value(<<"Notify-Realm">>, JObj),
    case get_endpoint(User, Realm) of
        {error, timeout} ->
            lager:debug("mwi timed out looking up contact for ~s@~s", [User, Realm]);
        Endpoint ->
            NewMessages = wh_json:get_integer_value(<<"Messages-New">>, JObj, 0),
            Body = io_lib:format(?MWI_BODY, [case NewMessages of 0 -> "no"; _ -> "yes" end
                                             ,<<User/binary, "@", Realm/binary>>
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
            Node = props:get_value(node, Props),
            Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
            lager:debug("sending of MWI update to ~s resulted in: ~p", [Node, Resp])
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
    case freeswitch:event(Node, ['PRESENCE_IN', 'PRESENCE_OUT', 'PRESENCE_PROBE']) of
        ok ->
            gproc:reg({p, l, fs_notify}),
            gproc:reg({p, l, {call_event, <<"PRESENCE_IN">>}}),
            gproc:reg({p, l, {call_event, <<"PRESENCE_OUT">>}}),
            gproc:reg({p, l, {call_event, <<"PRESENCE_PROBE">>}}),
            lager:debug("bound to switch presence events on node ~s", [Node]),
            {ok, #state{node=Node, options=Options}};
        {error, Reason} ->
            lager:warning("error when trying to bind to presence events on node ~s: ~p", [Node, Reason]),
            {stop, Reason};
        timeout ->
            lager:warning("timeout when trying to bind to presence events on node ~s", [Node]),
            {stop, timeout}
    end.

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
handle_cast({distributed_presence, Type, Event}, #state{node=Node}=State) ->
    Headers = [{wh_util:to_list(K), wh_util:to_list(V)}
               || {K, V} <- lists:foldr(fun(Header, Props) ->
                                                proplists:delete(Header, Props)
                                        end, Event, ?FS_DEFAULT_HDRS)
              ],
    EventName = wh_util:to_atom(Type, true),
    _ = freeswitch:sendevent(Node, EventName, [{"Distributed-From", wh_util:to_list(Node)} | Headers]),
    {noreply, State};
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
handle_info({event, [_ | Data]}, #state{node=Node}=State) ->
    case props:get_value(<<"Event-Name">>, Data) of
        <<"PRESENCE_", _/binary>> = EvtName ->
            ShouldDistribute = ecallmgr_config:get(<<"distribute_presence">>, true),
            ShouldDistribute andalso process_presence_event(EvtName, Data, Node),
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
get_endpoint(User, Realm) ->
    case ecallmgr_registrar:lookup(Realm, User, [<<"Contact">>]) of
        [{<<"Contact">>, Contact}] ->
            RURI = binary:replace(re:replace(Contact, "^[^\@]+", User, [{return, binary}]), <<">">>, <<"">>),
            wh_util:to_list(<<"sip:", (RURI)/binary>>);
        {error, timeout}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the first channel in a list of channels with the answer
%% state, ringing or the last channel if no prior was ringing.  If
%% the list is empty it returns undefined
%% @end
%%--------------------------------------------------------------------
-spec try_find_ringing_channel/1 :: (wh_json:json_objects()) -> undefined | wh_json:json_object().
try_find_ringing_channel([]) -> undefined;
try_find_ringing_channel([Channel]) -> Channel; 
try_find_ringing_channel([Channel|Channels]) -> 
    case wh_json:get_value(<<"Answer-State">>, Channel) of
        <<"ringing">> -> Channel;
        _Else -> try_find_ringing_channel(Channels)
    end.

-spec create_presence_in/4 :: (ne_binary(), undefined | string(), undefined | string(), wh_json:json_object()) -> proplist().
create_presence_in(PresenceId, Status, State, JObj) ->
    [KV || {_, V}=KV <- [{"unique-id", wh_json:get_string_value(<<"Call-ID">>, JObj)}
                         ,{"channel-state", wh_json:get_string_value(<<"Channel-State">>, JObj, State)}
                         ,{"answer-state", State}
                         ,{"proto", "any"}
                         ,{"login", "src/mod/event_handlers/mod_erlang_event/handle_msg.c"}
                         ,{"from", wh_util:to_list(PresenceId)}
                         ,{"rpid", "unknown"}
                         ,{"status", Status}
                         ,{"event_type", "presence"}
                         ,{"alt_event_type", "dialog"}
                         ,{"presence-call-direction", "outbound"}
                        ]
               ,V =/= undefined
    ].

-spec send_presence_event/3 :: (ne_binary(), ne_binary() | atom(), proplist()) -> 'ok'.
send_presence_event(<<"PRESENCE_PROBE">>, Node, Data) ->
    From = props:get_value(<<"from">>, Data, <<"nouser@nodomain">>),
    To = props:get_value(<<"to">>, Data, <<"nouser@nodomain">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    Req = [{<<"From">>, From}
           ,{<<"From-User">>, FromUser}
           ,{<<"From-Realm">>, FromRealm}
           ,{<<"To">>, To}
           ,{<<"To-User">>, ToUser}
           ,{<<"To-Realm">>, ToRealm}
           ,{<<"Node">>, wh_util:to_binary(Node)}
           ,{<<"Expires">>, props:get_value(<<"expires">>, Data)}
           ,{<<"Subscription-Call-ID">>, props:get_value(<<"sub-call-id">>, Data)}
           ,{<<"Subscription-Type">>, props:get_value(<<"alt_event_type">>, Data)}
           ,{<<"Subscription">>, props:get_value(<<"proto-specific-event-name">>, Data)}
           | wh_api:default_headers(<<>>, <<"notification">>, <<"presence_probe">>, ?APP_NAME, ?APP_VERSION)
          ],
    wapi_notifications:publish_presence_probe(Req);
send_presence_event(_, _, _) ->
    ok.

-spec process_presence_event/3 :: (ne_binary(), proplist(), atom()) -> 'ok'.
process_presence_event(EvtName, Data, Node) ->
    %% if the distributed-from is on the request then we already saw it
    case props:get_value(<<"Distributed-From">>, Data) of
        undefined ->
            NodeBin = wh_util:to_binary(Node),
            Headers = [{<<"Distributed-From">>, NodeBin} | Data],
            %% send it out over AMQP
            send_presence_event(EvtName, NodeBin, Headers),
            %% reply it on all the other connected nodes...
            %% "mod_multicast" style
            [distributed_presence(Srv, EvtName, Headers)
             || Srv <- gproc:lookup_pids({p, l, fs_notify}),
                Srv =/= self()
            ];
        _Else ->
            ok
    end.
