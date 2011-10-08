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
-export([presence_in/1, presence_out/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MWI_BODY, "Messages-Waiting: ~s~nVoice-Message: ~b/~b (~b/~b)~nMessage-Account: ~s~n").
-define(PRESENCE_DIALOG, "<?xml version=\"1.0\"?><dialog-info xmlns=\"urn:ietf:params:xml:ns:dialog-info\" version=\"~p\" state=\"partial\" entity=\"~s\"><dialog id=\"as7d900as8\" call-id=\"~s\" direction=\"~s\"><state>~s</state></dialog></dialog-info>").
-define(RESPONDERS, [{{ecallmgr_notify, mwi_notification}
                      ,[{<<"notification">>, <<"mwi">>}]}
		     ,{{ecallmgr_notify, presence_out}
                      ,[{<<"presence">>, <<"subscribers_query_resp">>}]}
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

presence_in(Data) ->
    PresenceId = wh_json:get_value(<<"Channel-Presence-ID">>, Data),
    [User, Realm] = binary:split(PresenceId, <<"@">>),
    Request = [{<<"User">>, User}
               ,{<<"Realm">>, Realm}
               ,{<<"Fields">>, [<<"From-User">>, <<"From-Host">>, <<"From-Tag">>
                                    ,<<"To-User">>, <<"To-Host">>, <<"To-Tag">>
                                    ,<<"Call-ID">>, <<"Accept">>, <<"Agent">>
                               ]}
               ,{<<"Event">>, {struct, [{<<"From">>, wh_json:get_value(<<"from">>, Data)}
                                        ,{<<"RPID">>, wh_json:get_value(<<"rpid">>, Data)}
                                        ,{<<"Status">>, wh_json:get_value(<<"status">>, Data)}
                                        ,{<<"Event-Type">>, wh_json:get_value(<<"event_type">>, Data)}
                                        ,{<<"Alt-Event-Type">>, wh_json:get_value(<<"alt_event_type">>, Data)}
                                        ,{<<"Call-ID">>, wh_json:get_value(<<"Channel-Call-UUID">>, Data)}
                                        ,{<<"Call-Direction">>, wh_json:get_value(<<"presence-call-direction">>, Data)}]}}
               | wh_api:default_headers(<<>>, <<"presence">>, <<"subscribers_query">>, ?APP_NAME, ?APP_VERSION)
              ],
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
                       ,{"event-str", "message-summary"}
                       ,{"content-type", "application/simple-message-summary"}
                       ,{"content-length", wh_util:to_list(length(Body))}
                       ,{"body", lists:flatten(Body)}
                      ],
            {ok, Node} = ecallmgr_fs_handler:request_node(<<"audio">>),
            Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
            ?LOG("sending of MWI update to ~s resulted in: ~p", [Node, Resp])
    end.

presence_out(JObj, _Props) ->
    true = wh_api:presence_subscrs_query_resp_v(JObj),

    Event = wh_json:get_value(<<"Event">>, JObj, ?EMPTY_JSON_OBJECT),
    Subscribers = wh_json:get_value(<<"Subscribers">>, JObj, []),

    [send_presence_dialog(Subscriber, Event) || Subscriber <- Subscribers].


send_presence_dialog(Subscriber, Event) ->
    ToUser = wh_json:get_value(<<"From-User">>, Subscriber),
    ToHost = wh_json:get_value(<<"From-Host">>, Subscriber),
    ToTag = wh_json:get_value(<<"From-Tag">>, Subscriber),

    FromUser = wh_json:get_value(<<"To-User">>, Subscriber),
    FromHost = wh_json:get_value(<<"To-Host">>, Subscriber),
    FromTag = wh_json:get_value(<<"To-Tag">>, Subscriber),

    NotifyEvent = wh_json:get_value(<<"Alt-Event-Type">>, Event, <<"presence">>),
    CallId = wh_json:get_value(<<"Call-ID">>, Subscriber),
    Accept = wh_json:get_value(<<"Accepts">>, Subscriber, <<"application/dialog-info+xml">>),

    Entity = io_lib:format("sip:~s@~s", [FromUser, FromHost]),
    Direction = wh_json:get_value(<<"Call-Direction">>, Event),
    Status = wh_json:get_value(<<"Status">>, Event),
    Dialog = wh_json:get_value(<<"Call-ID">>, Event),

    case get_endpoint(ToUser, ToHost) of
        {error, timeout} ->
            io:format("TIMEOUT!~n", []),
            ok;
        Endpoint ->
            Body = build_presence_body(Status, Dialog, Entity, Direction, Accept),
            Headers = [{"profile", ?DEFAULT_FS_PROFILE}
                       ,{"route-uri", wh_util:to_list(Endpoint)}
                       ,{"to-uri", lists:flatten(io_lib:format("sip:~s@~s;tag=~s", [ToUser, ToHost, ToTag]))}
                       ,{"from-uri", lists:flatten(io_lib:format("sip:~s@~s;tag=~s", [FromUser, FromHost, FromTag]))}
                       ,{"call-id", wh_util:to_list(CallId)}
                       ,{"event-str", wh_util:to_list(NotifyEvent)}
                       ,{"subscription-state", "active;expires=3600"}
                       ,{"content-type", wh_util:to_list(Accept)}
                       ,{"content-length", wh_util:to_list(length(Body))}
                       ,{"body", lists:flatten(Body)}
                      ],
            io:format("~p~n", [wh_util:to_list(length(Body))]),
            {ok, Node} = ecallmgr_fs_handler:request_node(<<"audio">>),
            io:format("~p~n", [Node]),
            R = freeswitch:sendevent(Node, 'NOTIFY', Headers),
            io:format("Result ~p~n", [R])
    end.

build_presence_body(Status, Dialog, Entity, CallDirection, <<"application/dialog-info+xml">>) ->
    {ok, Cache} = ecallmgr_sup:cache_proc(),
    Version = case wh_cache:fetch_local(Cache, {presence_version, Entity}) of
                  {ok, V} ->
                      wh_cache:store_local(Cache, {presence_version, Entity}, V + 1),
                      V;
                  _ ->
                      wh_cache:store_local(Cache, {presence_version, Entity}, 0),
                      0
              end,
    Direction = case CallDirection of
                    <<"outbound">> -> "recipient";
                    _ -> "initiator"
                end,
    State = case Status of
                <<"CS_HANGUP">> -> "terminated";
                _ -> "confirmed"
            end,
    io_lib:format(?PRESENCE_DIALOG, [Version, Entity, Dialog, Direction, State]).
