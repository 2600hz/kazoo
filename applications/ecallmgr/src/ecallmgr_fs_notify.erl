%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%% Notify-type requests, like MWI updates, received and processed here
%%% @end
%%% @contributors
%%%    Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_notify).

-behaviour(gen_listener).

-export([start_link/1, start_link/2]).
-export([presence_probe/2]).
-export([check_sync_api/2, check_sync/2]).
-export([mwi_update/2]).
-export([register_overwrite/2]).
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
               ,options :: kz_proplist()
         }).

-define(MWI_BODY, "Messages-Waiting: ~s\r\nMessage-Account: ~s\r\nVoice-Message: ~b/~b (~b/~b)\r\n\r\n").

-define(BINDINGS, [{'presence', [{'restrict_to', ['mwi_unsolicited_update'
                                                 ,'register_overwrite'
                                                 ,'probe'
                                                 ]}
                                ,{'probe_type', <<"presence">>}
                                ]}
                  ,{'switch', [{'restrict_to', ['check_sync']}]}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'presence_probe'}
                     ,[{<<"presence">>, <<"probe">>}]
                     }
                    ,{{?MODULE, 'mwi_update'}
                     ,[{<<"presence">>, <<"mwi_unsolicited_update">>}]
                     }
                    ,{{?MODULE, 'register_overwrite'}
                     ,[{<<"presence">>, <<"register_overwrite">>}]
                     }
                    ,{{?MODULE, 'check_sync_api'}
                     ,[{<<"switch_event">>, <<"check_sync">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<"ecallmgr_fs_notify">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-include("ecallmgr.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), kz_proplist()) -> startlink_ret().
start_link(Node) -> start_link(Node, []).
start_link(Node, Options) ->
    gen_listener:start_link(?SERVER
                           ,[{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[Node, Options]).

-spec presence_probe(kz_json:object(), kz_proplist()) -> 'ok'.
presence_probe(JObj, _Props) ->
    'true' = kapi_presence:probe_v(JObj),
    _ = kz_util:put_callid(JObj),
    Username = kz_json:get_value(<<"Username">>, JObj),
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    State = case ecallmgr_registrar:get_registration(Realm, Username) of
                'undefined' -> <<"offline">>;
                _Else -> <<"online">>
            end,
    resp_to_probe(State, Username, Realm).

-spec resp_to_probe(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
resp_to_probe(State, User, Realm) ->
    PresenceId = <<User/binary, "@", Realm/binary>>,
    PresenceUpdate = [{<<"Presence-ID">>, PresenceId}
                     ,{<<"State">>, State}
                     ,{<<"Call-ID">>, kz_util:to_hex_binary(crypto:hash(md5, PresenceId))}
                      | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                     ],
    kz_amqp_worker:cast(PresenceUpdate, fun kapi_presence:publish_update/1).

-spec check_sync_api(kz_json:object(), kz_proplist()) -> 'ok'.
check_sync_api(JObj, _Props) ->
    'true' = kapi_switch:check_sync_v(JObj),
    kz_util:put_callid(JObj),
    check_sync(kapi_switch:check_sync_username(JObj)
              ,kapi_switch:check_sync_realm(JObj)
              ).

-spec check_sync(ne_binary(), ne_binary()) -> 'ok'.
check_sync(Username, Realm) ->
    lager:info("looking up registration information for ~s@~s", [Username, Realm]),
    case ecallmgr_registrar:lookup_registration(Realm, Username) of
        {'error', 'not_found'} ->
            lager:warning("failed to find contact for ~s@~s, not sending check-sync", [Username, Realm]);
        {'ok', Registration} ->
            Contact = kz_json:get_first_defined([<<"Bridge-RURI">>, <<"Contact">>], Registration),
            [Node|_] = kz_util:shuffle_list(ecallmgr_fs_nodes:connected()),
            lager:info("calling check sync on ~s for ~s@~s and contact ~s", [Node, Username, Realm, Contact]),
            case ensure_contact_user(Contact, Username, Realm) of
                'undefined' ->
                    lager:error("invalid contact : ~p : ~p", [Contact, Registration]);
                Valid -> send_check_sync(Node, Username, Realm, Valid)
            end
    end.

-spec send_check_sync(atom(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_check_sync(Node, Username, Realm, Contact) ->
    To = kzsip_uri:uri(#uri{user=Username, domain=Realm}),
    From = kzsip_uri:uri(#uri{user=Username, domain=Realm}),
    AOR = kzsip_uri:ruri(#uri{user=Username, domain=Realm}),
    SIPHeaders = <<"X-KAZOO-AOR : ", AOR/binary, "\r\n">>,
    Headers = [{"profile", ?DEFAULT_FS_PROFILE}
              ,{"contact-uri", Contact}
              ,{"extra-headers", SIPHeaders}
              ,{"to-uri", To}
              ,{"from-uri", From}
              ,{"event-string", "check-sync"}
              ],
    Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
    lager:info("send check-sync to '~s@~s' via ~s: ~p", [Username, Realm, Node, Resp]).

-spec mwi_update(kz_json:object(), kz_proplist()) -> no_return().
mwi_update(JObj, Props) ->
    _ = kz_util:put_callid(JObj),
    'true' = kapi_presence:mwi_unsolicited_update_v(JObj),
    [Username, Realm] = binary:split(kz_json:get_value(<<"To">>, JObj), <<"@">>),
    case ecallmgr_registrar:lookup_registration(Realm, Username) of
        {'error', 'not_found'} ->
            lager:warning("failed to find registration for ~s@~s, dropping MWI update", [Username, Realm]);
        {'ok', Registration} ->
            Node = props:get_value('node', Props),
            send_mwi_update(JObj, Username, Realm, Node, Registration)
    end.

-spec send_mwi_update(kz_json:object(), ne_binary(), ne_binary(), atom(), kz_json:object()) -> 'ok'.
send_mwi_update(JObj, Username, Realm, Node, Registration) ->
    ToURI = #uri{user=kz_json:get_value(<<"To-User">>, Registration, Username)
                ,domain=kz_json:get_value(<<"To-Host">>, Registration, Realm)
                },
    To = kzsip_uri:uri(ToURI),
    ToAccount = kzsip_uri:ruri(ToURI),
    From = kzsip_uri:uri(#uri{user=kz_json:get_value(<<"From-User">>, Registration, Username)
                             ,domain=kz_json:get_value(<<"From-Host">>, Registration, Realm)
                             }),
    NewMessages = kz_json:get_integer_value(<<"Messages-New">>, JObj, 0),
    Body = io_lib:format(?MWI_BODY, [case NewMessages of 0 -> "no"; _ -> "yes" end
                                    ,ToAccount
                                    ,NewMessages
                                    ,kz_json:get_integer_value(<<"Messages-Saved">>, JObj, 0)
                                    ,kz_json:get_integer_value(<<"Messages-Urgent">>, JObj, 0)
                                    ,kz_json:get_integer_value(<<"Messages-Urgent-Saved">>, JObj, 0)
                                    ]),
    RegistrationContact = kz_json:get_first_defined([<<"Bridge-RURI">>, <<"Contact">>], Registration),
    case ensure_contact_user(RegistrationContact, Username, Realm) of
        'undefined' ->
            lager:error("invalid contact : ~p : ~p", [RegistrationContact, Registration]);
        Contact ->
            SIPHeaders = <<"X-KAZOO-AOR : ", ToAccount/binary, "\r\n">>,
            Headers = [{"profile", ?DEFAULT_FS_PROFILE}
                      ,{"contact-uri", Contact}
                      ,{"extra-headers", SIPHeaders}
                      ,{"to-uri", To}
                      ,{"from-uri", From}
                      ,{"event-string", "message-summary"}
                      ,{"content-type", "application/simple-message-summary"}
                      ,{"content-length", kz_util:to_list(length(Body))}
                      ,{"body", lists:flatten(Body)}
                      ],
            Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
            lager:debug("sent MWI update to '~s' via ~s: ~p", [Contact, Node, Resp])
    end.

-spec register_overwrite(kz_json:object(), kz_proplist()) -> no_return().
register_overwrite(JObj, Props) ->
    Node = props:get_value('node', Props),
    Username = kz_json:get_binary_value(<<"Username">>, JObj, <<"unknown">>),
    Realm = kz_json:get_binary_value(<<"Realm">>, JObj, <<"unknown">>),
    PrevContact = ensure_contact_user(
                    kz_json:get_value(<<"Previous-Contact">>, JObj)
                                     ,Username
                                     ,Realm
                   ),
    NewContact = ensure_contact_user(
                   kz_json:get_value(<<"Contact">>, JObj)
                                    ,Username
                                    ,Realm
                  ),
    SipUri = kzsip_uri:uri(#uri{user=Username, domain=Realm}),
    PrevBody = kz_util:to_list(<<"Replaced-By:", (kz_util:to_binary(NewContact))/binary>>),
    NewBody = kz_util:to_list(<<"Overwrote:", (kz_util:to_binary(PrevContact))/binary>>),
    PrevContactHeaders = [{"profile", ?DEFAULT_FS_PROFILE}
                         ,{"contact", PrevContact}
                         ,{"contact-uri", PrevContact}
                         ,{"to-uri", SipUri}
                         ,{"from-uri", SipUri}
                         ,{"event-str", "registration-overwrite"}
                         ,{"content-type", "text/plain"}
                         ,{"content-length", kz_util:to_list(length(PrevBody))}
                         ,{"body", PrevBody}
                         ],
    NewContactHeaders = [{"profile", ?DEFAULT_FS_PROFILE}
                        ,{"contact", NewContact}
                        ,{"contact-uri", NewContact}
                        ,{"to-uri", SipUri}
                        ,{"from-uri", SipUri}
                        ,{"event-str", "registration-overwrite"}
                        ,{"content-type", "text/plain"}
                        ,{"content-length", kz_util:to_list(length(NewBody))}
                        ,{"body", NewBody}
                        ],
    case PrevContact of
        'undefined' ->
            lager:error("previous contact is invalid : ~p : ~p", [PrevContact, JObj]);
        _ ->
            freeswitch:sendevent(Node, 'NOTIFY', PrevContactHeaders)
    end,
    case NewContact of
        'undefined' ->
            lager:error("new contact is invalid : ~p : ~p", [NewContact, JObj]);
        _ ->
            freeswitch:sendevent(Node, 'NOTIFY', NewContactHeaders)
    end,
    lager:debug("sent registration overwrite update of old '~s' new '~s' via '~s'"
               ,[PrevContact
                ,NewContact
                ,Node
                ]).

-spec ensure_contact_user(ne_binary(), ne_binary(), ne_binary()) -> api_binary().
ensure_contact_user(OriginalContact, Username, Realm) ->
    ecallmgr_util:fix_contact(OriginalContact, Username, Realm).

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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
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
