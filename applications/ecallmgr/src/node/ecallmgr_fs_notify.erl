%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Notify-type requests, like MWI updates, received and processed here
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_notify).
-behaviour(gen_listener).

-export([start_link/1, start_link/2]).
-export([maybe_presence_probe/2]).
-export([notify_api/2, notify/3]).
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

-record(state, {node :: atom()
               ,options :: kz_term:proplist()
               }).
-type state() :: #state{}.

-define(MWI_BODY, "Messages-Waiting: ~s\r\nMessage-Account: ~s\r\nVoice-Message: ~b/~b (~b/~b)\r\n\r\n").

-define(BINDINGS, [{'presence', [{'restrict_to', ['mwi_unsolicited_update'
                                                 ,'register_overwrite'
                                                 ,'probe'
                                                 ]}
                                ,{'probe_type', <<"presence">>}
                                ]}
                  ,{'switch', [{'restrict_to', ['notify']}]}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'maybe_presence_probe'}
                     ,[{<<"presence">>, <<"probe">>}]
                     }
                    ,{{?MODULE, 'mwi_update'}
                     ,[{<<"presence">>, <<"mwi_unsolicited_update">>}]
                     }
                    ,{{?MODULE, 'register_overwrite'}
                     ,[{<<"presence">>, <<"register_overwrite">>}]
                     }
                    ,{{?MODULE, 'notify_api'}
                     ,[{<<"switch_event">>, <<"notify">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<"ecallmgr_fs_notify">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-include("ecallmgr.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------

-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Node) -> start_link(Node, []).

-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) ->
    gen_listener:start_link(?SERVER
                           ,[{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[Node, Options]).

-spec maybe_presence_probe(kz_json:object(), kz_term:proplist()) -> 'ok'.
maybe_presence_probe(JObj, _Props) ->
    'true' = kapi_presence:probe_v(JObj),
    _ = kz_log:put_callid(JObj),
    Username = kz_json:get_value(<<"Username">>, JObj),
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    presence_probe(Username, Realm).

-spec presence_probe(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
presence_probe(<<"*", _/binary>>, _Realm) -> 'ok';
presence_probe(Username, Realm) ->
    State = case ecallmgr_registrar:get_registration(Realm, Username) of
                'undefined' -> <<"offline">>;
                _Else -> <<"online">>
            end,
    resp_to_probe(State, Username, Realm).

-spec resp_to_probe(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
resp_to_probe(State, User, Realm) ->
    PresenceId = <<User/binary, "@", Realm/binary>>,
    PresenceUpdate = [{<<"Presence-ID">>, PresenceId}
                     ,{<<"From">>, <<"sip:", PresenceId/binary>>}
                     ,{<<"To">>, <<"sip:", PresenceId/binary>>}
                     ,{<<"State">>, State}
                     ,{<<"Call-ID">>, kz_term:to_hex_binary(crypto:hash(md5, PresenceId))}
                      | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                     ],
    kz_amqp_worker:cast(PresenceUpdate, fun kapi_presence:publish_update/1).

-spec notify_api(kz_json:object(), kz_term:proplist()) -> 'ok'.
notify_api(JObj, _Props) ->
    'true' = kapi_switch:notify_v(JObj),
    kz_log:put_callid(JObj),
    maybe_send_notify(kapi_switch:notify_username(JObj)
                     ,kapi_switch:notify_realm(JObj)
                     ,JObj
                     ).

-spec notify(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
notify(Username, Realm, Event) ->
    JObj = kz_json:from_list([{<<"Event">>, Event}]),
    maybe_send_notify(Username, Realm, JObj).

-spec maybe_send_notify(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_send_notify(Username, Realm, JObj) ->
    lager:info("looking up registration information for ~s@~s", [Username, Realm]),
    case ecallmgr_registrar:lookup_registration(Realm, Username) of
        {'error', 'not_found'} ->
            lager:warning("failed to find contact ~s@~s, not sending NOTIFY", [Username, Realm]);
        {'ok', Registration} ->
            Contact = kz_json:get_first_defined([<<"Bridge-RURI">>, <<"Contact">>], Registration),
            [Node|_] = kz_term:shuffle_list(ecallmgr_fs_nodes:connected()),
            lager:info("sending NOTIFY on ~s for ~s@~s and contact ~s", [Node, Username, Realm, Contact]),
            case ensure_contact_user(Contact, Username, Realm) of
                'undefined' ->
                    lager:error("invalid contact ~p: ~p", [Contact, Registration]);
                Valid -> send_notify(Node, Username, Realm, JObj, Valid)
            end
    end.

-spec send_notify(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_notify(Node, Username, Realm, JObj, Contact) ->
    AOR = To = From = kzsip_uri:ruri(#uri{user=Username, domain=Realm}),
    SIPHeaders = <<"X-KAZOO-AOR : ", AOR/binary, "\r\n">>,
    Event = kz_json:get_ne_binary_value(<<"Event">>, JObj),
    Body = kz_json:get_ne_binary_value(<<"Body">>, JObj),
    ContentType = kz_json:get_ne_binary_value(<<"Content-Type">>, JObj),
    Headers = props:filter_undefined(
                [{<<"body">>, Body}
                ,{<<"contact-uri">>, Contact}
                ,{<<"content-type">>, ContentType}
                ,{<<"event-string">>, Event}
                ,{<<"extra-headers">>, SIPHeaders}
                ,{<<"from-uri">>, From}
                ,{<<"profile">>, <<?DEFAULT_FS_PROFILE>>}
                ,{<<"to-uri">>, To}
                ]),
    Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
    lager:info("send NOTIFY with Event '~s' (has body? ~w) to '~s@~s' via ~s: ~p"
              ,[Event, (Body =/= 'undefined'), Username, Realm, Node, Resp]).

-spec mwi_update(kz_json:object(), kz_term:proplist()) -> no_return().
mwi_update(JObj, Props) ->
    _ = kz_log:put_callid(JObj),
    'true' = kapi_presence:mwi_unsolicited_update_v(JObj),
    [Username, Realm] = binary:split(kz_json:get_value(<<"To">>, JObj), <<"@">>),
    case ecallmgr_registrar:lookup_registration(Realm, Username) of
        {'error', 'not_found'} ->
            lager:warning("failed to find registration for ~s@~s, dropping MWI update", [Username, Realm]);
        {'ok', Registration} ->
            Node = props:get_value('node', Props),
            send_mwi_update(JObj, Username, Realm, Node, Registration)
    end.

-spec send_mwi_update(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), atom(), kz_json:object()) -> 'ok'.
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
            Headers = [{<<"profile">>, <<?DEFAULT_FS_PROFILE>>}
                      ,{<<"contact-uri">>, Contact}
                      ,{<<"extra-headers">>, SIPHeaders}
                      ,{<<"to-uri">>, To}
                      ,{<<"from-uri">>, From}
                      ,{<<"event-string">>, <<"message-summary">>}
                      ,{<<"content-type">>, <<"application/simple-message-summary">>}
                      ,{<<"body">>, kz_term:to_binary(Body)}
                      ],
            Resp = freeswitch:sendevent(Node, 'NOTIFY', Headers),
            lager:debug("sent MWI update to '~s' via ~s: ~p", [Contact, Node, Resp])
    end.

-spec register_overwrite(kz_json:object(), kz_term:proplist()) -> no_return().
register_overwrite(JObj, Props) ->
    Node = props:get_value('node', Props),
    Username = kz_json:get_binary_value(<<"Username">>, JObj, <<"unknown">>),
    Realm = kz_json:get_binary_value(<<"Realm">>, JObj, <<"unknown">>),
    PrevContact = ensure_contact_user(kz_json:get_value(<<"Previous-Contact">>, JObj)
                                     ,Username
                                     ,Realm
                                     ),
    NewContact = ensure_contact_user(kz_json:get_value(<<"Contact">>, JObj)
                                    ,Username
                                    ,Realm
                                    ),
    SipUri = kzsip_uri:uri(#uri{user=Username, domain=Realm}),
    PrevBody = <<"Replaced-By:", (kz_term:to_binary(NewContact))/binary>>,
    NewBody = <<"Overwrote:", (kz_term:to_binary(PrevContact))/binary>>,
    PrevContactHeaders = [{<<"profile">>, <<?DEFAULT_FS_PROFILE>>}
                         ,{<<"contact">>, PrevContact}
                         ,{<<"contact-uri">>, PrevContact}
                         ,{<<"to-uri">>, SipUri}
                         ,{<<"from-uri">>, SipUri}
                         ,{<<"event-str">>, <<"registration-overwrite">>}
                         ,{<<"content-type">>, <<"text/plain">>}
                         ,{<<"body">>, PrevBody}
                         ],
    NewContactHeaders = [{<<"profile">>, <<?DEFAULT_FS_PROFILE>>}
                        ,{<<"contact">>, NewContact}
                        ,{<<"contact-uri">>, NewContact}
                        ,{<<"to-uri">>, SipUri}
                        ,{<<"from-uri">>, SipUri}
                        ,{<<"event-str">>, <<"registration-overwrite">>}
                        ,{<<"content-type">>, <<"text/plain">>}
                        ,{<<"body">>, NewBody}
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

-spec ensure_contact_user(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binary().
ensure_contact_user(OriginalContact, Username, Realm) ->
    ecallmgr_util:fix_contact(OriginalContact, Username, Realm).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_log:put_callid(Node),
    lager:debug("starting new ecallmgr notify process"),
    gproc:reg({'p', 'l', 'fs_notify'}),
    {'ok', #state{node=Node, options=Options}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', Node}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("notify listener for ~s terminating: ~p", [Node, _Reason]).

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
