%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Notify-type requests, like MWI updates, received and processed here
%%% @end
%%% Created :  18 Jul 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_notify).

-behaviour(gen_listener).

%% API
-export([start_link/0]).
-export([presence_update/2]).
-export([mwi_update/2]).
-export([send_presence_event/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2,
         terminate/2, code_change/3]).

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
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE,
                            [{responders, ?RESPONDERS}
                             ,{bindings, ?BINDINGS}
                             ,{queue_name, ?QUEUE_NAME}
                             ,{queue_options, ?QUEUE_OPTIONS}
                             ,{consume_options, ?CONSUME_OPTIONS}
                             ,{basic_qos, 1}
                            ], []).

-spec presence_update/2 :: (wh_json:json_object(), proplist()) -> ok.
presence_update(JObj, _Props) ->
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
    NodeHandlers = ecallmgr_fs_sup:node_handlers(),
    _ = [begin
             ?LOG("sending presence in event to ~p~n", [Node]),
             freeswitch:sendevent(Node, 'PRESENCE_IN', [{"Distributed-From", wh_util:to_list(Node)} | Event])
         end
         || NodeHandler <- NodeHandlers,
            (Node = ecallmgr_fs_node:fs_node(NodeHandler)) =/= undefined
        ],
    ok.

-spec mwi_update/2 :: (wh_json:json_object(), proplist()) -> no_return().
mwi_update(JObj, _Props) ->
    _ = wh_util:put_callid(JObj),

    true = wapi_notifications:mwi_update_v(JObj),

    User = wh_json:get_value(<<"Notify-User">>, JObj),
    Realm  = wh_json:get_value(<<"Notify-Realm">>, JObj),

    case get_endpoint(User, Realm) of
        {error, timeout} ->
            ?LOG_END("mwi timed out looking up contact for ~s@~s", [User, Realm]);
        Endpoint ->
            NewMessages = wh_json:get_integer_value(<<"Messages-New">>, JObj, 0),
            Body = io_lib:format(?MWI_BODY, [case NewMessages of 0 -> "no"; _ -> "yes" end
                                             ,<<User/binary, "@", Realm/binary>>
                                             ,NewMessages
                                             ,wh_json:get_integer_value(<<"Messages-Saved">>, JObj, 0)
                                             ,wh_json:get_integer_value(<<"Messages-Urgent">>, JObj, 0)
                                             ,wh_json:get_integer_value(<<"Messages-Urgent-Saved">>, JObj, 0)
                                            ]),
            ?LOG("created mwi notify body ~s", [Body]),
            Headers = [{"profile", ?DEFAULT_FS_PROFILE}
                       ,{"to-uri", Endpoint}
                       ,{"from-uri", "sip:2600hz@2600hz.com"}
                       ,{"event-str", "message-summary"}
                       ,{"content-type", "application/simple-message-summary"}
                       ,{"content-length", wh_util:to_list(length(Body))}
                       ,{"body", lists:flatten(Body)}
                      ],
            {ok, Node} = ecallmgr_fs_handler:request_node(<<"audio">>),
            Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
            ?LOG("sending of MWI update to ~s resulted in: ~p", [Node, Resp])
    end.

-spec send_presence_event/3 :: (ne_binary(), ne_binary(), proplist()) -> ok.
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
           ,{<<"Node">>, Node}
           ,{<<"Expires">>, props:get_value(<<"expires">>, Data)}
           ,{<<"Subscription-Call-ID">>, props:get_value(<<"sub-call-id">>, Data)}
           ,{<<"Subscription-Type">>, props:get_value(<<"alt_event_type">>, Data)}
           ,{<<"Subscription">>, props:get_value(<<"proto-specific-event-name">>, Data)}
           | wh_api:default_headers(<<>>, <<"notification">>, <<"presence_probe">>, ?APP_NAME, ?APP_VERSION)
          ],
    wapi_notifications:publish_presence_probe(Req);
send_presence_event(_, _, _) ->
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?LOG_SYS("starting new ecallmgr notify process"),
    {ok, ok}.

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
    {reply, ok, State}.

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
handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, []}.

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
    ?LOG_SYS("ecallmgr notify ~p termination", [_Reason]),
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
