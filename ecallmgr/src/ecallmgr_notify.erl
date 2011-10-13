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
-export([mwi_notification/2, send_mwi/4]).
-export([subscribe/2, callstate_change/1, presence_out/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MWI_BODY, "Messages-Waiting: ~s~nVoice-Message: ~b/~b (~b/~b)~nMessage-Account: ~s~n").
-define(RESPONDERS, [{{ecallmgr_notify, mwi_notification}
                      ,[{<<"notification">>, <<"mwi">>}]}
		     ,{{ecallmgr_notify, presence_out}
                      ,[{<<"presence">>, <<"subscribers_query_resp">>}]}
		     ,{{ecallmgr_notify, subscribe}, [{<<"presence">>, <<"subscribe">>}]}
		    ]).

-define(BINDINGS, [{notifications, []}]).

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
%%			     ,{basic_qos, 1}
			    ], []).

callstate_change(Data) ->
    User = case wh_json:get_value(<<"Presence-Call-Direction">>, Data) of
               <<"outbound">> ->
                   wh_json:get_value(<<"Caller-Destination-Number">>, Data);
               _ ->
                   wh_json:get_value(<<"Caller-Username">>, Data)
           end,
    Request = [{<<"User">>, User}
               ,{<<"Account-ID">>, wh_json:get_value(<<"variable_ecallmgr_Account-ID">>, Data)}
               ,{<<"Fields">>, [<<"From-User">>, <<"From-Host">>, <<"From-Tag">>
                                    ,<<"To-User">>, <<"To-Host">>, <<"To-Tag">>
                                    ,<<"Call-ID">>, <<"Accept">>, <<"Agent">>
                               ]}
               ,{<<"Event">>, {struct, [{<<"Call-ID">>, wh_json:get_value(<<"Channel-Call-UUID">>, Data)}
                                        ,{<<"Call-State">>, wh_json:get_value(<<"Channel-Call-State">>, Data)}
                                        ,{<<"Presence-ID">>, wh_json:get_value(<<"Channel-Presence-ID">>, Data)}
                                        ,{<<"Presence-Direction">>, wh_json:get_value(<<"Presence-Call-Direction">>, Data)}
                                        ,{<<"Caller-ID-Name">>, wh_json:get_value(<<"Caller-Caller-ID-Name">>, Data)}
                                        ,{<<"Caller-ID-Number">>, wh_json:get_value(<<"Caller-Caller-ID-Number">>, Data)}
                                        ,{<<"Callee-ID-Name">>, wh_json:get_value(<<"Other-Leg-Caller-ID-Name">>, Data)}
                                        ,{<<"Callee-ID-Number">>, wh_json:get_value(<<"Other-Leg-Caller-ID-Number">>, Data)}
                                       ]}}
               | wh_api:default_headers(<<>>, <<"presence">>, <<"subscribers_query">>, ?APP_NAME, ?APP_VERSION)
              ],
    ?LOG("sending channel state change notification"),
    {ok, Payload} = wh_api:presence_subscrs_query([ KV || {_, V}=KV <- Request, V =/= undefined ]),
    amqp_util:callmgr_publish(Payload, <<"application/json">>, ?KEY_PRESENCE_IN).

-spec send_mwi/4 :: (User, Realm, New, Saved) -> no_return() when
      User :: string() | binary(),
      Realm :: string() | binary(),
      New :: integer() | binary(),
      Saved :: integer() | binary().
send_mwi(User, Realm, New, Saved) ->
    JObj = wh_json:from_list([{<<"Notify-User">>, wh_util:to_binary(User)}
			      ,{<<"Notify-Realm">>, wh_util:to_binary(Realm)}
			      ,{<<"Messages-New">>, wh_util:to_binary(New)}
			      ,{<<"Messages-Saved">>, wh_util:to_binary(Saved)}
			      | wh_api:default_headers(<<>>, <<"notification">>, <<"mwi">>, ?APP_NAME, ?APP_VERSION)
			     ]),
    mwi_notification(JObj, []).

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
    {ok, Cache} = ecallmgr_sup:cache_proc(),
    CSeq = case wh_cache:fetch_local(Cache, {notify_cseq}) of
               {ok, Seq} ->
                   wh_cache:store_local(Cache, {notify_cseq}, Seq + 1),
                   Seq;
               {error, not_found} ->
                   wh_cache:store_local(Cache, {notify_cseq}, 42),
                   42
           end,
    {reply, [{cseq, CSeq}]}.

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
-spec get_endpoint/2 :: (User, Realm) -> tuple(error, timeout) | string() when
      User :: binary(),
      Realm :: binary().
get_endpoint(User, Realm) ->
    case ecallmgr_registrar:lookup(Realm, User, [<<"Contact">>]) of
        [{<<"Contact">>, Contact}] ->
            RURI = binary:replace(re:replace(Contact, "^[^\@]+", User, [{return, binary}]), <<">">>, <<"">>),
            wh_util:to_list(<<"sip:", (RURI)/binary>>);
        {error, timeout}=E ->
            E
    end.

-spec mwi_notification/2 :: (JObj, Props) -> no_return() when
      JObj :: json_object(),
      Props :: proplist().
mwi_notification(JObj, _Props) ->
    wh_util:put_callid(JObj),

    true = wh_api:mwi_update_v(JObj),

    User = wh_json:get_value(<<"Notify-User">>, JObj),
    Realm  = wh_json:get_value(<<"Notify-Realm">>, JObj),

    case get_endpoint(User, Realm) of
        {error, timeout} ->
            ?LOG_END("mwi timed out looking up contact for ~s@~s", [User, Realm]);
        Endpoint ->
            NewMessages = wh_json:get_integer_value(<<"Messages-New">>, JObj, 0),
            Body = io_lib:format(?MWI_BODY, [case NewMessages of 0 -> "no"; _ -> "yes" end,
                                             NewMessages
                                             ,wh_json:get_integer_value(<<"Messages-Saved">>, JObj, 0)
                                             ,wh_json:get_integer_value(<<"Messages-Urgent">>, JObj, 0)
                                             ,wh_json:get_integer_value(<<"Messages-Urgent-Saved">>, JObj, 0)
                                             ,<<User/binary, "@", Realm/binary>>
                                            ]),
            ?LOG("created mwi notify body ~s", [Body]),
            Headers = [{"profile", ?DEFAULT_FS_PROFILE}
                       ,{"to-uri", Endpoint}
                       ,{"from-uri", "sip:2600hz@2600hz.com"}
                       ,{"event-string", "message-summary"}
                       ,{"content-type", "application/simple-message-summary"}
                       ,{"content-length", wh_util:to_list(length(Body))}
                       ,{"body", lists:flatten(Body)}
                      ],
            {ok, Node} = ecallmgr_fs_handler:request_node(<<"audio">>),
            Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
            ?LOG("sending of MWI update to ~s resulted in: ~p", [Node, Resp])
    end.

subscribe(_JObj, _Props) ->
    ok.
%%    true = wh_api:presence_subscr_v(JObj),
%%    timer:sleep(500),
%%    send_presence_dialog(JObj, {struct, [<<"Alt-Event-Type">>, <<"dialog">>]}).

presence_out(JObj, Props) ->
    true = wh_api:presence_subscrs_query_resp_v(JObj),
    ?LOG("received presence event, forwarding to subscribers"),

    CSeq = props:get_value(cseq, Props, 42),

    Event = wh_json:get_value(<<"Event">>, JObj, ?EMPTY_JSON_OBJECT),
    Subscribers = wh_json:get_value(<<"Subscribers">>, JObj, []),

    [send_presence_dialog(Subscriber, Event, CSeq) || Subscriber <- Subscribers].


send_presence_dialog(Subscriber, Event, CSeq) ->
    ToUser = wh_json:get_value(<<"From-User">>, Subscriber),
    ToHost = wh_json:get_value(<<"From-Host">>, Subscriber),
    ToTag = wh_json:get_value(<<"From-Tag">>, Subscriber),

    FromUser = wh_json:get_value(<<"To-User">>, Subscriber),
    FromHost = wh_json:get_value(<<"To-Host">>, Subscriber),
    FromTag = wh_json:get_value(<<"To-Tag">>, Subscriber),

    NotifyEvent = wh_json:get_value(<<"Alt-Event-Type">>, Event, <<"dialog">>),
    CallId = wh_json:get_value(<<"Call-ID">>, Subscriber),
    Accept = wh_json:get_value(<<"Accepts">>, Subscriber, <<"application/dialog-info+xml">>),

    put(callid, wh_json:get_value(<<"Call-ID">>, Subscriber, <<"000000000000">>)),
    ?LOG_START("sending presence update for ~s@~s to ~s@~s", [FromUser, FromHost, ToUser, ToHost]),

    Entity = lists:flatten(io_lib:format("sip:~s@~s", [FromUser, FromHost])),

    case get_endpoint(ToUser, ToHost) of
        {error, timeout} ->
            ?LOG_END("failed to find registration of ~s@~s for presence update", [ToUser, ToHost]),
            ok;
        Endpoint ->
            Body = build_presence_body(Entity, Event, <<"application/dialog-info+xml">>),
            Headers = [{"profile", ?DEFAULT_FS_PROFILE}
                       ,{"route-uri", wh_util:to_list(Endpoint)}
                       ,{"to-uri", lists:flatten(io_lib:format("sip:~s@~s;tag=~s", [ToUser, ToHost, ToTag]))}
                       ,{"from-uri", lists:flatten(io_lib:format("sip:~s@~s;tag=~s", [FromUser, FromHost, FromTag]))}
                       ,{"contact", Entity}
                       ,{"call-id", wh_util:to_list(CallId)}
                       ,{"event-string", wh_util:to_list(NotifyEvent)}
                       ,{"cseq", wh_util:to_list(CSeq) ++ " NOTIFY"}
                       ,{"subscription-state", "active;expires=3600"}
                       ,{"content-type", wh_util:to_list(Accept)}
                       ,{"body", lists:flatten(Body)}
                      ],
            {ok, Node} = ecallmgr_fs_handler:request_node(<<"audio">>),
            ?LOG_END("sent presence ~p notify via ~s", [CSeq, Node]),
            freeswitch:sendevent(Node, 'NOTIFY', Headers)
    end.

build_presence_body(Entity, Event, <<"application/dialog-info+xml">>) ->
    EventCallId = wh_json:get_value(<<"Call-ID">>, Event),

    Dialog = build_presence_dialog(EventCallId, Event),
    Props = [{'dialog-info', [{'xmlns', "urn:ietf:params:xml:ns:dialog-info"}
                              ,{'version', round(wh_util:current_tstamp())}
                              ,{'state', "full"}
                              ,{'entity', Entity}], Dialog}],
    wh_util:to_list(make_doc_xml(Props, undefined)).


build_presence_dialog(undefined, _) ->
    [];
build_presence_dialog(CallId, Event) ->
    Direction = case wh_json:get_value(<<"Presence-Direction">>, Event) of
                    <<"inbound">> -> "initiator";
                    <<"outbound">> -> "recipient"
                end,
    State = case wh_json:get_value(<<"Call-State">>, Event) of
                <<"DOWN">> when Direction =:= "recipient" ->
                    "early";
                <<"DOWN">> ->
                    "confirmed";
                <<"DIALING">> when Direction =:= "recipient" ->
                    "early";
                <<"DIALING">> ->
                    "confirmed";
                <<"RINGING">> when Direction =:= "recipient" ->
                    "early";
                <<"RINGING">> ->
                    "confirmed";
                <<"EARLY">> when Direction =:= "recipient" ->
                    "early";
                <<"EARLY">> ->
                    "confirmed";
                <<"ACTIVE">> ->
                    "confirmed";
                <<"HELD">> ->
                    "early";
                <<"HANGUP">> ->
                    "terminated"
            end,
    ?LOG("created presence dialog for ~s with state ~s", [CallId, State]),
    [{'dialog', [{'id', wh_util:to_list(CallId)}
                 ,{'direction', Direction}]
      ,[{'state', State}]}].

make_doc_xml(Props, undefined) ->
    XMerL = props_to_xml(Props, []),
    Xml = xmerl:export_simple(XMerL, xmerl_xml),
    unicode:characters_to_binary(Xml);
make_doc_xml(Props, Root) ->
    XMerL = doc_xml_simple(Props, Root),
    Xml = xmerl:export_simple(XMerL, xmerl_xml),
    unicode:characters_to_binary(Xml).

doc_xml_simple(Props, Root) ->
    {Root, props_to_xml(Props, [])}.

props_to_xml([], Xml) ->
    Xml;
props_to_xml([{_, undefined}|T], Xml) ->
    props_to_xml(T, Xml);
props_to_xml([{_, []}=V|T], Xml) ->
    props_to_xml(T, [V|Xml]);
props_to_xml([{_, _, []}=V|T], Xml) ->
    props_to_xml(T, [V|Xml]);

props_to_xml([{K, [{_, _, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, Attr, [{_, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, Attr, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, Attr, [{_, _, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, Attr, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, Attr, V}|T], Xml) when is_boolean(V) ->
    props_to_xml(T, [{K, [{type, "boolean"}|Attr], [wh_util:to_list(V)]}|Xml]);
props_to_xml([{K, Attr, V}|T], Xml) ->
    props_to_xml(T, [{K, Attr, [wh_util:to_list(V)]}|Xml]);

props_to_xml([{K, [{_, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, V}|T], Xml) when is_boolean(V) ->
    props_to_xml(T, [{K, [{type, "boolean"}], [wh_util:to_list(V)]}|Xml]);
props_to_xml([{K, V}|T], Xml) ->
    props_to_xml(T, [{K, [wh_util:to_list(V)]}|Xml]).
