%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016 2600Hz Inc
%%% @doc
%%% Conference participant process
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(conf_participant).
-behaviour(gen_listener).

%% API
-export([start_link/1]).
-export([relay_amqp/2]).
-export([handle_conference_error/2]).

-export([consume_call_events/1]).
-export([conference/1, set_conference/2]).
-export([discovery_event/1, set_discovery_event/2]).
-export([call/1]).

-export([join_local/1, join_remote/2]).

-export([set_name_pronounced/2]).

-export([mute/1, unmute/1, toggle_mute/1]).
-export([deaf/1, undeaf/1, toggle_deaf/1]).
-export([hangup/1]).
-export([dtmf/2]).
-export([state/1]).

-export([handle_conference_event/2]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("conference.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{{?MODULE, 'relay_amqp'}
                     ,[{<<"call_event">>, <<"*">>}]
                     },
                     {{?MODULE, 'handle_conference_event'}
                     ,[{<<"conference">>, <<"event">>}]
                     }
                    ,{{?MODULE, 'handle_conference_error'}
                     ,[{<<"conference">>, <<"error">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(participant, {participant_id = 0 :: non_neg_integer()
                     ,call :: kapps_call:call()
                     ,moderator = 'false' :: boolean()
                     ,muted = 'false' :: boolean()
                     ,deaf = 'false' :: boolean()
                     ,waiting_for_mod = 'false' :: boolean()
                     ,call_event_consumers = [] :: pids()
                     ,in_conference = 'false' :: boolean()
                     ,join_attempts = 0 :: non_neg_integer()
                     ,conference :: kapps_conference:conference()
                     ,discovery_event = kz_json:new() :: kz_json:object()
                     ,last_dtmf = <<>> :: binary()
                     ,server = self() :: pid()
                     ,remote = 'false' :: boolean()
                     ,name_pronounced :: conf_pronounced_name:name_pronounced()
                     }).
-type participant() :: #participant{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
start_link(Call) ->
    CallId = kapps_call:call_id(Call),
    Bindings = [{'call', [{'callid', CallId}]}
               ,{'self', []}
               ],
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', Bindings}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [Call]).

-spec conference(pid()) -> {'ok', kapps_conference:conference()}.
conference(Srv) -> gen_listener:call(Srv, {'get_conference'}).

-spec set_conference(kapps_conference:conference(), pid()) -> 'ok'.
set_conference(Conference, Srv) -> gen_listener:cast(Srv, {'set_conference', Conference}).

-spec discovery_event(pid()) -> {'ok', kz_json:object()}.
discovery_event(Srv) -> gen_listener:call(Srv, {'get_discovery_event'}).

-spec set_discovery_event(kz_json:object(), pid()) -> 'ok'.
set_discovery_event(DE, Srv) -> gen_listener:cast(Srv, {'set_discovery_event', DE}).

-spec set_name_pronounced(conf_pronounced_name:name_pronounced(), pid()) -> 'ok'.
set_name_pronounced(Name, Srv) -> gen_listener:cast(Srv, {'set_name_pronounced', Name}).

-spec call(pid()) -> {'ok', kapps_call:call()}.
call(Srv) -> gen_listener:call(Srv, {'get_call'}).

-spec join_local(pid()) -> 'ok'.
join_local(Srv) -> gen_listener:cast(Srv, 'join_local').

-spec join_remote(pid(), kz_json:object()) -> 'ok'.
join_remote(Srv, JObj) -> gen_listener:cast(Srv, {'join_remote', JObj}).

-spec state(pid()) -> 'ok'.
state(Srv) -> gen_listener:call(Srv, {'state'}).

-spec mute(pid()) -> 'ok'.
mute(Srv) -> gen_listener:cast(Srv, 'mute').

-spec unmute(pid()) -> 'ok'.
unmute(Srv) -> gen_listener:cast(Srv, 'unmute').

-spec toggle_mute(pid()) -> 'ok'.
toggle_mute(Srv) -> gen_listener:cast(Srv, 'toggle_mute').

-spec deaf(pid()) -> 'ok'.
deaf(Srv) -> gen_listener:cast(Srv, 'deaf').

-spec undeaf(pid()) -> 'ok'.
undeaf(Srv) -> gen_listener:cast(Srv, 'undeaf').

-spec toggle_deaf(pid()) -> 'ok'.
toggle_deaf(Srv) -> gen_listener:cast(Srv, 'toggle_deaf').

-spec hangup(pid()) -> 'ok'.
hangup(Srv) -> gen_listener:cast(Srv, 'hangup').

-spec dtmf(pid(), ne_binary()) -> 'ok'.
dtmf(Srv, Digit) -> gen_listener:cast(Srv,{'dtmf', Digit}).

-spec consume_call_events(pid()) -> 'ok'.
consume_call_events(Srv) -> gen_listener:cast(Srv, {'add_consumer', self()}).

-spec relay_amqp(kz_json:object(), kz_proplist()) -> 'ok'.
relay_amqp(JObj, Props) ->
    _ = [kapps_call_command:relay_event(Pid, JObj)
         || Pid <- props:get_value('call_event_consumers', Props, []),
            is_pid(Pid)
        ],
    Digit = kz_json:get_value(<<"DTMF-Digit">>, JObj),
    case is_binary(Digit) of
        'false' -> 'ok';
        'true' ->
            Srv = props:get_value('server', Props),
            dtmf(Srv, Digit)
    end.

-spec handle_conference_error(kz_json:object(), kz_proplist()) -> 'ok'.
handle_conference_error(JObj, Props) ->
    'true' = kapi_conference:conference_error_v(JObj),
    lager:debug("conference error: ~p", [JObj]),
    case kz_json:get_value([<<"Request">>, <<"Application-Name">>], JObj) of
        <<"participants">> ->
            Srv = props:get_value('server', Props),
            gen_listener:cast(Srv, {'sync_participant', []});
        _Else -> 'ok'
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {'ok', State} |
%%                     {'ok', State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([kapps_call:call()]) -> {'ok', participant()}.
init([Call]) ->
    process_flag('trap_exit', 'true'),
    kapps_call:put_callid(Call),
    {'ok', #participant{call=Call}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {'reply', Reply, State} |
%%                                   {'reply', Reply, State, Timeout} |
%%                                   {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), participant()) -> handle_call_ret_state(participant()).
handle_call({'get_conference'}, _, #participant{conference='undefined'}=P) ->
    {'reply', {'error', 'not_provided'}, P};
handle_call({'get_conference'}, _, #participant{conference=Conf}=P) ->
    {'reply', {'ok', Conf}, P};
handle_call({'get_discovery_event'}, _, #participant{discovery_event=DE}=P) ->
    {'reply', {'ok', DE}, P};
handle_call({'get_call'}, _, #participant{call=Call}=P) ->
    {'reply', {'ok', Call}, P};
handle_call({'state'}, _, Participant) ->
    {reply, Participant, Participant};
handle_call(_Request, _, P) ->
    {'reply', {'error', 'unimplemented'}, P}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {'noreply', State} |
%%                                  {'noreply', State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), participant()) -> handle_cast_ret_state(participant()).
handle_cast('hungup', Participant) ->
    {'stop', {'shutdown', 'hungup'}, Participant};
handle_cast({'gen_listener', {'created_queue', Q}}, #participant{conference='undefined'
                                                                ,call=Call
                                                                }=P) ->
    {'noreply', P#participant{call=kapps_call:set_controller_queue(Q, Call)}};
handle_cast({'gen_listener', {'created_queue', Q}}, #participant{conference=Conference
                                                                ,call=Call
                                                                }=P) ->
    {'noreply', P#participant{call=kapps_call:set_controller_queue(Q, Call)
                             ,conference=kapps_conference:set_controller_queue(Q, Conference)
                             }};
handle_cast('hangup', Participant) ->
    lager:debug("received in-conference command, hangup participant"),
    gen_listener:cast(self(), 'hungup'),
    {'noreply', Participant};
handle_cast({'add_consumer', C}, #participant{call_event_consumers=Cs}=P) ->
    lager:debug("adding call event consumer ~p", [C]),
    link(C),
    {'noreply', P#participant{call_event_consumers=[C|Cs]}};
handle_cast({'remove_consumer', C}, #participant{call_event_consumers=Cs}=P) ->
    lager:debug("removing call event consumer ~p", [C]),
    {'noreply', P#participant{call_event_consumers=[C1 || C1 <- Cs, C=/=C1]}};
handle_cast({'set_conference', Conference}, Participant=#participant{call=Call}) ->
    ConferenceId = kapps_conference:id(Conference),
    CallId = kapps_call:call_id(Call),
    lager:debug("received conference data for conference ~s", [ConferenceId]),
    gen_listener:add_binding(self(), 'conference', [{ 'restrict_to', [{'event', {ConferenceId,CallId}}] }]),
    {'noreply', Participant#participant{conference=Conference}};
handle_cast({'set_discovery_event', DE}, #participant{}=Participant) ->
    {'noreply', Participant#participant{discovery_event=DE}};
handle_cast({'set_name_pronounced', Name}, #participant{}=Participant) ->
    _ = set_enter_exit_sounds(Name, Participant),
    {'noreply', Participant#participant{name_pronounced = Name}};
handle_cast({'gen_listener',{'is_consuming','true'}}, Participant) ->
    lager:debug("now consuming messages"),
    {'noreply', Participant};
handle_cast(_Message, #participant{conference='undefined'}=Participant) ->
    %% ALL MESSAGES BELLOW THIS ARE CONSUMED HERE UNTIL THE CONFERENCE IS KNOWN
    lager:debug("ignoring message prior to conference discovery: ~p"
               ,[_Message]
               ),
    {'noreply', Participant};
handle_cast('join_local', #participant{call=Call
                                      ,conference=Conference
                                      }=Participant) ->
    send_conference_command(Conference, Call),
    {'noreply', Participant};
handle_cast({'join_remote', JObj}, #participant{call=Call
                                               ,conference=Conference
                                               }=Participant) ->
    Route = binary:replace(kz_json:get_value(<<"Switch-URL">>, JObj)
                          ,<<"mod_sofia">>
                          ,<<"conference">>
                          ),
    bridge_to_conference(Route, Conference, Call),
    {'noreply', Participant#participant{remote='true'}};
handle_cast({'sync_participant', JObj}, #participant{call=Call}=Participant) ->
    Event = kz_json:get_ne_binary_value(<<"Event">>, JObj),
    {'noreply', sync_participant(Event, JObj, Call, Participant)};
handle_cast(_Cast, Participant) ->
    lager:debug("unhandled cast: ~p", [_Cast]),
    {'noreply', Participant}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), participant()) -> handle_info_ret_state(participant()).
handle_info({'EXIT', Consumer, _R}, #participant{call_event_consumers=Consumers}=P) ->
    lager:debug("call event consumer ~p died: ~p", [Consumer, _R]),
    Cs = [C || C <- Consumers, C =/= Consumer],
    {'noreply', P#participant{call_event_consumers=Cs}, 'hibernate'};
handle_info(_Msg, Participant) ->
    {'noreply', Participant}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), participant()) -> gen_listener:handle_event_return().
handle_event(JObj, #participant{call_event_consumers=Consumers
                               ,call=Call
                               ,server=Srv
                               }) ->
    CallId = kapps_call:call_id(Call),
    case {kapps_util:get_event_type(JObj)
         ,kz_json:get_value(<<"Call-ID">>, JObj)
         }
    of
        {{<<"call_event">>, <<"CHANNEL_DESTROY">>}, CallId} ->
            lager:debug("received channel hangup event, terminate"),
            gen_listener:cast(Srv, 'hungup');
        {_, _} -> 'ok'
    end,
    {'reply', [{'call_event_consumers', Consumers}]}.

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
-spec terminate(any(), participant()) -> 'ok'.
terminate(_Reason, #participant{name_pronounced = Name}) ->
    maybe_clear(Name),
    lager:debug("conference participant execution has been stopped: ~p", [_Reason]).

-spec maybe_clear(conf_pronounced_name:name_pronounced()) -> 'ok'.
maybe_clear({'temp_doc_id', AccountId, MediaId}) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    lager:debug("deleting doc: ~s/~s", [AccountDb, MediaId]),
    kz_datamgr:del_doc(AccountDb, MediaId),
    'ok';
maybe_clear(_) -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {'ok', NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), participant(), any()) -> {'ok', participant()}.
code_change(_OldVsn, Participant, _Extra) ->
    {'ok', Participant}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec log_conference_join(boolean(), non_neg_integer(), kapps_conference:conference()) -> 'ok'.
log_conference_join('true'=_Moderator, ParticipantId, Conference) ->
    lager:debug("caller has joined the local conference ~s as moderator ~p", [kapps_conference:name(Conference), ParticipantId]);
log_conference_join('false'=_Moderator, ParticipantId, Conference) ->
    lager:debug("caller has joined the local conference ~s as member ~p", [kapps_conference:name(Conference), ParticipantId]).

-spec sync_participant(ne_binary(), kz_json:objects(), kapps_call:call(), participant()) -> participant().
sync_participant(<<"add-member">>, JObj, Call, Participant) ->
    sync_participant(JObj, Call, Participant);
sync_participant(<<"conference-destroyed">>, _JObj, _Call, Participant) -> Participant;
sync_participant(_Event, _JObj, _Call, Participant) -> Participant.

-spec sync_participant(kz_json:objects(), kapps_call:call(), participant()) ->
                              participant().
sync_participant(JObj, Call, #participant{in_conference='false'
                                         ,conference=Conference
                                         ,discovery_event=DiscoveryEvent
                                         }=Participant) ->
    ParticipantId = kz_json:get_value(<<"Participant-ID">>, JObj),
    IsModerator = kz_json:is_true([<<"Conference-Channel-Vars">>, <<"Is-Moderator">>], JObj),
    log_conference_join(IsModerator, ParticipantId, Conference),
    _ = kz_util:spawn(fun notify_requestor/4, [kapps_call:controller_queue(Call)
                                              ,ParticipantId
                                              ,DiscoveryEvent
                                              ,kapps_conference:id(Conference)
                                              ]),
    Muted = kz_json:is_false([<<"Conference-Channel-Vars">>, <<"Speak">>], JObj),
    Deaf = kz_json:is_false([<<"Conference-Channel-Vars">>, <<"Hear">>], JObj),
    Participant#participant{in_conference='true'
                           ,participant_id=ParticipantId
                           ,muted=Muted
                           ,deaf=Deaf
                           ,moderator=IsModerator
                           };
sync_participant(JObj, _Call, #participant{in_conference='true'}=Participant) ->
    lager:debug("caller has is still in the conference"),
    Muted = kz_json:is_false([<<"Conference-Channel-Vars">>, <<"Speak">>], JObj),
    Deaf = kz_json:is_false([<<"Conference-Channel-Vars">>, <<"Hear">>], JObj),
    Participant#participant{in_conference='true'
                           ,muted=Muted
                           ,deaf=Deaf
                           }.

-spec notify_requestor(ne_binary(), non_neg_integer(), kz_json:object(), ne_binary()) -> 'ok'.
notify_requestor(MyQ, MyId, DiscoveryEvent, ConferenceId) ->
    case kz_api:server_id(DiscoveryEvent) of
        'undefined' -> 'ok';
        <<>> -> 'ok';
        RequestorQ ->
            Resp = [{<<"Conference-ID">>, ConferenceId}
                   ,{<<"Participant-ID">>, MyId}
                    | kz_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
                   ],
            Publisher = fun(P) -> kapi_conference:publish_discovery_resp(RequestorQ, P) end,
            kz_amqp_worker:cast(Resp, Publisher)
    end.


-spec bridge_to_conference(ne_binary(), kapps_conference:conference(), kapps_call:call()) -> 'ok'.
bridge_to_conference(Route, Conference, Call) ->
    lager:debug("bridging to conference running at '~s'", [Route]),
    Endpoint = kz_json:from_list([{<<"Invite-Format">>, <<"route">>}
                                 ,{<<"Route">>, Route}
                                 ,{<<"Auth-User">>, kapps_conference:bridge_username(Conference)}
                                 ,{<<"Auth-Password">>, kapps_conference:bridge_password(Conference)}
                                 ,{<<"Outbound-Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
                                 ,{<<"Outbound-Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
                                 ,{<<"Ignore-Early-Media">>, <<"true">>}
                                 ,{<<"To-URI">>, <<"sip:", (kapps_conference:id(Conference))/binary
                                                   ,"@", (get_account_realm(Call))/binary
                                                 >>
                                  }
                                 ]),
    Command = [{<<"Application-Name">>, <<"bridge">>}
              ,{<<"Endpoints">>, [Endpoint]}
              ,{<<"Timeout">>, 20}
              ,{<<"Dial-Endpoint-Method">>, <<"single">>}
              ,{<<"Ignore-Early-Media">>, <<"false">>}
              ,{<<"Hold-Media">>, <<"silence">>}
              ],
    kapps_call_command:send_command(Command, Call).

-spec get_account_realm(kapps_call:call()) -> ne_binary().
get_account_realm(Call) ->
    case kapps_call:account_id(Call) of
        'undefined' -> <<"unknown">>;
        AccountId ->
            case kz_account:fetch(AccountId) of
                {'ok', JObj} -> kz_account:realm(JObj, <<"unknown">>);
                {'error', R} ->
                    lager:debug("error while looking up account realm: ~p", [R]),
                    <<"unknown">>
            end
    end.

-spec send_conference_command(kapps_conference:conference(), kapps_call:call()) -> 'ok'.
send_conference_command(Conference, Call) ->
    {Mute, Deaf} =
        case kapps_conference:moderator(Conference) of
            'true' ->
                {kapps_conference:moderator_join_muted(Conference)
                ,kapps_conference:moderator_join_deaf(Conference)
                };
            'false' ->
                {kapps_conference:member_join_muted(Conference)
                ,kapps_conference:member_join_deaf(Conference)
                }
        end,
    kapps_call_command:conference(kapps_conference:id(Conference)
                                 ,Mute
                                 ,Deaf
                                 ,kapps_conference:moderator(Conference)
                                 ,kapps_conference:id(Conference)
                                 ,Call
                                 ).


-spec handle_conference_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_conference_event(JObj, Props) ->
    Srv = props:get_value('server', Props),
    gen_listener:cast(Srv, {'sync_participant', JObj}).

set_enter_exit_sounds({_, AccountId, MediaId}, #participant{conference=Conference
                                                           ,call=Call
                                                           ,moderator=IsModerator
                                                           }) ->
    EntrySounds = [play_entry_tone(IsModerator, Conference)
                  ,kz_media_util:media_path(MediaId, AccountId)
                  ,kz_media_util:get_prompt(<<"conf-has_joined">>, kapps_conference:call(Conference))
                  ],

    ExitSounds = [play_exit_tone(IsModerator, Conference)
                 ,kz_media_util:media_path(MediaId, AccountId)
                 ,kz_media_util:get_prompt(<<"conf-has_left">>, kapps_conference:call(Conference))
                 ],

    Fun = fun('undefined') -> 'false';
             (_) -> 'true'
          end,

    Sounds = [{<<"Conference-Entry-Sound">>, lists:filter(Fun, EntrySounds)}
             ,{<<"Conference-Exit-Sound">>, lists:filter(Fun, ExitSounds)}
             ],
    kapps_call_command:media_macro(Sounds, Call).

%% @private
-spec play_exit_tone(boolean(), kapps_conference:conference()) -> api_binary().
play_exit_tone('false', Conference) ->
    play_exit_tone_media(?EXIT_TONE(kapps_conference:account_id(Conference)), Conference);
play_exit_tone('true', Conference) ->
    play_exit_tone_media(?MOD_EXIT_TONE(kapps_conference:account_id(Conference)), Conference).

%% @private
-spec play_exit_tone_media(ne_binary(), kapps_conference:conference()) -> api_binary().
play_exit_tone_media(Tone, Conference) ->
    case kapps_conference:play_exit_tone(Conference) of
        'false' -> 'undefined';
        MediaId = ?NE_BINARY -> kz_media_util:media_path(MediaId, kapps_conference:account_id(Conference));
        _Else -> Tone
    end.

%% @private
-spec play_entry_tone(boolean(), kapps_conference:conference()) -> api_binary().
play_entry_tone('false', Conference) ->
    play_entry_tone_media(?ENTRY_TONE(kapps_conference:account_id(Conference)), Conference);
play_entry_tone('true', Conference) ->
    play_entry_tone_media(?MOD_ENTRY_TONE(kapps_conference:account_id(Conference)), Conference).

-spec play_entry_tone_media(ne_binary(), kapps_conference:conference()) -> api_binary().
play_entry_tone_media(Tone, Conference) ->
    case kapps_conference:play_entry_tone(Conference) of
        'false' -> 'false';
        MediaId = ?NE_BINARY -> kz_media_util:media_path(MediaId, kapps_conference:account_id(Conference));
        _Else -> Tone
    end.
