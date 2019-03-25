%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Sponsored by Velvetech LLC, Implemented by SIPLABS LLC
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_call_command).

-include("kapps_call_command.hrl").

-export([presence/2, presence/3, presence/4, presence/5]).
-export([channel_status/1, channel_status/2
        ,channel_status_command/1, channel_status_command/2
        ,b_channel_status/1
        ]).

-export([response/2, response/3, response/4]).

-export([relay_event/2, relay_event/3
        ,receive_event/1, receive_event/2
        ,get_event_type/1
        ]).

-export([audio_macro/2, audio_macro/3]).
-ifdef(TEST).
-export([macros_to_commands/3]).
-endif.

-export([pickup/2, pickup/3, pickup/4, pickup/5, pickup/6
        ,pickup_command/2, pickup_command/3, pickup_command/4, pickup_command/5, pickup_command/6
        ,b_pickup/2, b_pickup/3, b_pickup/4, b_pickup/5, b_pickup/6
        ]).
-export([connect_leg/2, connect_leg/3, connect_leg/4, connect_leg/5, connect_leg/6
        ,connect_leg_command/2, connect_leg_command/3, connect_leg_command/4, connect_leg_command/5, connect_leg_command/6
        ,b_connect_leg/2, b_connect_leg/3, b_connect_leg/4, b_connect_leg/5, b_connect_leg/6
        ]).
-export([redirect/2
        ,redirect/3
        ,redirect_to_node/3
        ]).
-export([answer/1, answer_now/1
        ,hangup/1, hangup/2
        ,break/1
        ,queued_hangup/1
        ,set/3, set/4, set/5, set_terminators/2
        ,fetch/1, fetch/2
        ]).
-export([echo/1]).
-export([ring/1]).
-export([receive_fax/1
        ,receive_fax/2
        ,receive_fax/3
        ,receive_fax/4
        ,b_receive_fax/1
        ]).
-export([bridge/2, bridge/3, bridge/4, bridge/5, bridge/6, bridge/7, bridge/8, bridge/9
        ,b_bridge/2, b_bridge/3, b_bridge/4, b_bridge/5, b_bridge/6, b_bridge/7, b_bridge/8, b_bridge/9
        ,unbridge/1, unbridge/2, unbridge/3
        ,b_bridge_wait/2
        ]).
-export([page/2, page/3, page/4, page/5, page/6, page/7, page/8]).
-export([hold/1, hold/2
        ,hold_command/1, hold_command/2
        ,b_hold/1, b_hold/2, b_hold/3
        ,park/1
        ,park_command/1
        ]).
-export([soft_hold/1, soft_hold/2
        ,soft_hold_command/2, soft_hold_command/4, soft_hold_command/5
        ]).
-export([play/2, play/3, play/4, play/5
        ,play_command/2, play_command/3, play_command/4, play_command/5
        ,b_play/2, b_play/3, b_play/4, b_play/5
        ,play_terminators/1
        ,play_leg/1
        ]).
-export([prompt/2, prompt/3]).

-export([b_seek/3, seek/1, seek/2, seek/3]).

-export([tts/2, tts/3, tts/4, tts/5, tts/6
        ,b_tts/2, b_tts/3, b_tts/4, b_tts/5, b_tts/6
        ,tts_command/2, tts_command/3, tts_command/4, tts_command/5, tts_command/6
        ,tts_terminators/1, tts_voice/1, tts_language/2, tts_engine/2
        ]).

-export([record/2, record/3, record/4, record/5, record/6]).
-export([record_call/2, record_call/3, record_call/4, record_call/5
        ,b_record_call/2, b_record_call/3, b_record_call/4, b_record_call/5
        ,start_record_call/2, start_record_call/3, start_record_call/4
        ,mask_record_call/2
        ,unmask_record_call/2
        ,stop_record_call/2
        ]).
-export([store/3, store/4, store/5, store/6
        ,store_fax/2, store_fax/3
        ]).
-export([tones/2]).
-export([prompt_and_collect_digit/2]).
-export([prompt_and_collect_digits/4, prompt_and_collect_digits/5
        ,prompt_and_collect_digits/6, prompt_and_collect_digits/7
        ,prompt_and_collect_digits/8, prompt_and_collect_digits/9
        ]).
-export([play_and_collect_digit/2]).
-export([play_and_collect_digits/4, play_and_collect_digits/5
        ,play_and_collect_digits/6, play_and_collect_digits/7
        ,play_and_collect_digits/8, play_and_collect_digits/9
        ]).
-export([say/2, say/3, say/4, say/5, say/6
        ,say_command/2, say_command/3, say_command/4, say_command/5, say_command/6
        ,b_say/2, b_say/3, b_say/4, b_say/5, b_say/6
        ]).

-export([conference/2, conference/3
        ,conference/4, conference/5
        ,conference/6, conference/7
        ]).
-export([b_conference/2, b_conference/3
        ,b_conference/4, b_conference/5
        ,b_conference/6, b_conference/7
        ]).

-export([noop/1, noop_id/0]).
-export([flush/1, flush_dtmf/1
        ,send_dtmf/2, send_dtmf/3
        ,recv_dtmf/2
        ]).
-export([privacy/1
        ,privacy/2
        ]).

-export([b_answer/1, b_hangup/1, b_hangup/2, b_fetch/1, b_fetch/2]).
-export([b_echo/1]).
-export([b_ring/1]).

-export([b_page/2, b_page/3, b_page/4, b_page/5, b_page/6, b_page/7, b_page/8]).

-export([b_prompt/2, b_prompt/3]).
-export([b_record/2, b_record/3, b_record/4, b_record/5, b_record/6]).
-export([b_store/3, b_store/4, b_store/5, b_store/6
        ,b_store_fax/2
        ,b_store_vm/6
        ]).
-export([b_prompt_and_collect_digit/2]).
-export([b_prompt_and_collect_digits/4, b_prompt_and_collect_digits/5
        ,b_prompt_and_collect_digits/6, b_prompt_and_collect_digits/7
        ,b_prompt_and_collect_digits/8, b_prompt_and_collect_digits/9
        ]).
-export([b_play_and_collect_digit/2]).
-export([b_play_and_collect_digits/4, b_play_and_collect_digits/5
        ,b_play_and_collect_digits/6, b_play_and_collect_digits/7
        ,b_play_and_collect_digits/8, b_play_and_collect_digits/9
        ]).

-export([b_noop/1]).
-export([b_flush/1]).
-export([b_privacy/1
        ,b_privacy/2
        ]).

-export([wait_for_message/2, wait_for_message/3
        ,wait_for_message/4, wait_for_message/5
        ]).
-export([wait_for_application/2, wait_for_application/3
        ,wait_for_application/4, wait_for_application/5
        ]).
-export([wait_for_headless_application/1, wait_for_headless_application/2
        ,wait_for_headless_application/3, wait_for_headless_application/4
        ,wait_for_headless_application/5
        ]).
-export([wait_for_bridge/2, wait_for_bridge/3]).
-export([wait_for_channel_bridge/0, wait_for_channel_unbridge/0]).
-export([wait_for_dtmf/1]).
-export([wait_for_noop/2]).
-export([wait_for_hangup/0, wait_for_hangup/1
        ,wait_for_unbridge/0, wait_for_unbridge/1
        ]).
-export([wait_for_application_or_dtmf/2]).
-export([collect_digits/2, collect_digits/3
        ,collect_digits/4, collect_digits/5
        ,collect_digits/6, collect_digits/7
        ]).
-export([send_command/2]).

-export([default_collect_timeout/0
        ,default_digit_timeout/0
        ,default_interdigit_timeout/0
        ,default_message_timeout/0
        ,default_application_timeout/0
        ]).

-export([start_fax_detection/3
        ,stop_fax_detection/1
        ,fax_detection/3
        ,wait_for_fax_detection/2
        ]).

-export([wait_for_unparked_call/1, wait_for_unparked_call/2]).

-export([get_outbound_t38_settings/1, get_outbound_t38_settings/2]).
-export([get_inbound_t38_settings/1, get_inbound_t38_settings/2]).
-export([audio_level_command/4, audio_level/4]).
-export([store_file/3, store_file/4]).

-export([attended_transfer/2, attended_transfer/3
        ,blind_transfer/2, blind_transfer/3
        ,transfer/3, transfer/4
        ,transfer_command/3, transfer_command/4
        ]).

-export([play_macro/2, b_play_macro/2, play_macro_command/2
        ,media_macro/2, media_macro_command/2
        ]).

-export([sound_touch_command/2, start_sound_touch/2, stop_sound_touch/1]).
-export([hold_control/1, hold_control/2
        ,hold_control_command/1, hold_control_command/2
        ]).

-type audio_macro_prompt() ::
        %% {'play', MediaName [, Terminators [, Leg]]}
        {'play', binary()} |
        {'play', binary(), kz_term:binaries()} |
        {'play', binary(), kz_term:binaries(), binary()} |

        %% {'prompt', Media [,Lang [,Leg]]}
        {'prompt', binary()} |
        {'prompt', binary(), binary()} |
        {'prompt', binary(), binary(), binary()} |

        %% {'say', Say [, Type [, Method [, Language]]]}
        {'say', binary()} |
        {'say', binary(), binary()} |
        {'say', binary(), binary(), binary()} |
        {'say', binary(), binary(), binary(), binary()} |

        {'tones', kz_json:objects()} |

        %% {'tts', Text [, Voice [, Lang [, Terminators]]]}
        {'tts', kz_term:ne_binary()} |
        {'tts', kz_term:ne_binary(), kz_term:ne_binary()} |
        {'tts', kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()} |
        {'tts', kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()}.
-type audio_macro_prompts() :: [audio_macro_prompt()].
-export_type([audio_macro_prompt/0
             ,audio_macro_prompts/0
             ,store_fun/0
             ,collect_digits_return/0
             ]).

-type store_fun() :: kz_term:ne_binary() | fun(() -> kz_term:ne_binary()).

-define(CONFIG_CAT, <<"call_command">>).

-define(DEFAULT_COLLECT_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, <<"collect_timeout">>, 5 * ?MILLISECONDS_IN_SECOND)).
-define(DEFAULT_DIGIT_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, <<"digit_timeout">>, 3 * ?MILLISECONDS_IN_SECOND)).
-define(DEFAULT_INTERDIGIT_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, <<"interdigit_timeout">>, 2 * ?MILLISECONDS_IN_SECOND)).

-define(DEFAULT_MESSAGE_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, <<"message_timeout">>, 5 * ?MILLISECONDS_IN_SECOND)).
-define(DEFAULT_APPLICATION_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, <<"application_timeout">>, 500 * ?MILLISECONDS_IN_SECOND)).

-define(STORAGE_TIMEOUT(App), kapps_config:get_integer(?CONFIG_CAT, [<<"store_file">>, kz_term:to_binary(App), <<"save_timeout_ms">>], 5 * ?MILLISECONDS_IN_MINUTE, <<"default">>)).
-define(STORAGE_RETRIES(App), kapps_config:get_integer(?CONFIG_CAT, [<<"store_file">>, kz_term:to_binary(App), <<"retries">>], 5, <<"default">>)).

-define(EXTRA_BRIDGE_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, <<"bridge_timeout_extended_ms">>, 20 * ?MILLISECONDS_IN_SECOND)).
-define(BRIDGE_DEFAULT_TIMEOUT, ?BRIDGE_DEFAULT_SYSTEM_TIMEOUT_S * ?MILLISECONDS_IN_SECOND).

-define(BRIDGE_EXPORT_VARS, kapps_config:get_ne_binaries(?CONFIG_CAT, <<"export_bridge_variables">>, ?BRIDGE_DEFAULT_EXPORT_VARS)).
-define(BRIDGE_DEFAULT_EXPORT_VARS, [<<"hold_music">>]).
-define(DEFAULT_SEEK_DURATION, 10000).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec default_collect_timeout() -> pos_integer().
default_collect_timeout() ->
    ?DEFAULT_COLLECT_TIMEOUT.

-spec default_digit_timeout() -> pos_integer().
default_digit_timeout() ->
    ?DEFAULT_DIGIT_TIMEOUT.

-spec default_interdigit_timeout() -> pos_integer().
default_interdigit_timeout() ->
    ?DEFAULT_INTERDIGIT_TIMEOUT.

-spec default_message_timeout() -> pos_integer().
default_message_timeout() ->
    ?DEFAULT_MESSAGE_TIMEOUT.

-spec default_application_timeout() -> pos_integer().
default_application_timeout() ->
    ?DEFAULT_APPLICATION_TIMEOUT.

-spec storage_timeout(kz_term:ne_binary()) -> pos_integer().
storage_timeout(App) ->
    ?STORAGE_TIMEOUT(App).

-spec storage_retries(kz_term:ne_binary()) -> pos_integer().
storage_retries(App) ->
    ?STORAGE_RETRIES(App).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec presence(kz_term:ne_binary(), kz_term:ne_binary() | kapps_call:call()) -> 'ok'.
presence(State, <<_/binary>> = PresenceId) ->
    presence(State, PresenceId, 'undefined');
presence(State, Call) ->
    presence(State, kapps_call:from(Call)).

-spec presence(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary() | kapps_call:call()) -> 'ok'.
presence(State, PresenceId, 'undefined') ->
    presence(State, PresenceId, ?FAKE_CALLID(PresenceId), 'undefined');
presence(State, PresenceId, <<_/binary>> = CallId) ->
    presence(State, PresenceId, CallId, 'undefined');
presence(State, PresenceId, Call) ->
    presence(State, PresenceId, kapps_call:call_id(Call), Call).

-spec presence(kz_term:ne_binary(), kz_term:ne_binary() , kz_term:api_binary() , kapps_call:call() | 'undefined') -> 'ok'.
presence(State, PresenceId, CallId, Call) ->
    presence(State, PresenceId, CallId, 'undefined', Call).

-spec presence(kz_term:ne_binary(), kz_term:ne_binary() , kz_term:api_binary() , kz_term:api_binary(), kapps_call:call() | 'undefined') -> 'ok'.
presence(State, PresenceId, CallId, 'undefined', 'undefined') ->
    [User, Realm] = binary:split(PresenceId, <<"@">>),
    Command = props:filter_undefined(
                [{<<"Presence-ID">>, PresenceId}
                ,{<<"From">>, <<"sip:", PresenceId/binary>>}
                ,{<<"From-User">>, User}
                ,{<<"From-Realm">>, Realm}
                ,{<<"To">>, <<"sip:", PresenceId/binary>>}
                ,{<<"To-User">>, User}
                ,{<<"To-Realm">>, Realm}
                ,{<<"State">>, State}
                ,{<<"Call-ID">>, CallId}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    kapi_presence:publish_update(Command);
presence(State, PresenceId, CallId, TargetURI, 'undefined') ->
    [User, Realm] = binary:split(PresenceId, <<"@">>),
    Command = props:filter_undefined(
                [{<<"Presence-ID">>, PresenceId}
                ,{<<"From">>, TargetURI}
                ,{<<"From-User">>, User}
                ,{<<"From-Realm">>, Realm}
                ,{<<"To">>, TargetURI}
                ,{<<"To-User">>, User}
                ,{<<"To-Realm">>, Realm}
                ,{<<"State">>, State}
                ,{<<"Call-ID">>, CallId}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    kapi_presence:publish_update(Command);
presence(State, PresenceId, CallId, TargetURI, Call) ->
    [User, Realm] = binary:split(PresenceId, <<"@">>),
    Command = props:filter_undefined(
                [{<<"Presence-ID">>, PresenceId}
                ,{<<"From">>, TargetURI}
                ,{<<"From-User">>, User}
                ,{<<"From-Realm">>, Realm}
                ,{<<"To">>, TargetURI}
                ,{<<"To-User">>, User}
                ,{<<"To-Realm">>, Realm}
                ,{<<"From-Tag">>, kapps_call:from_tag(Call)}
                ,{<<"To-Tag">>, kapps_call:to_tag(Call)}
                ,{<<"State">>, State}
                ,{<<"Call-ID">>, CallId}
                 | kz_api:default_headers(module_as_app(Call), ?APP_VERSION)
                ]),
    kapi_presence:publish_update(Command).

-spec module_as_app( kapps_call:call() ) -> kz_term:ne_binary().
module_as_app(Call) ->
    JObj = kapps_call:kvs_fetch(<<"cf_flow">>, kz_json:new(), Call),
    kz_json:get_value(<<"module">>, JObj, ?APP_NAME).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to get the channel status.
%% This request will execute immediately.
%% @end
%%------------------------------------------------------------------------------
-spec channel_status(kz_term:api_binary(), kz_term:api_binary()) ->
                            'ok' | {'error', 'no_channel_id'}.
channel_status('undefined', _) -> {'error', 'no_channel_id'};
channel_status(CallId, SrvQueue) when is_binary(CallId), is_binary(SrvQueue) ->
    Command = channel_status_command(CallId)
        ++ kz_api:default_headers(SrvQueue, ?APP_NAME, ?APP_VERSION),
    kapi_call:publish_channel_status_req(Command);
channel_status(Call, SrvQueue) ->
    channel_status(kapps_call:call_id(Call), SrvQueue).

-spec channel_status_command(kz_term:ne_binary() | kapps_call:call()) -> kz_term:proplist().
channel_status_command(CallId) ->
    channel_status_command(CallId, 'undefined').

-spec channel_status_command(kz_term:ne_binary() | kapps_call:call(), kz_term:api_boolean()) -> kz_term:proplist().
channel_status_command(<<_/binary>> = CallId, ActiveOnly) ->
    props:filter_undefined(
      [{<<"Call-ID">>, CallId}
      ,{<<"Active-Only">>, ActiveOnly}
      ]);
channel_status_command(Call, ActiveOnly) ->
    channel_status_command(kapps_call:call_id(Call), ActiveOnly).

-spec channel_status(kapps_call:call()) ->
                            'ok' | {'error', 'no_channel_id'}.
channel_status(Call) ->
    'true' = kapps_call:is_call(Call),
    channel_status(kapps_call:call_id(Call), kapps_call:controller_queue(Call)).

-spec b_channel_status(kz_term:api_binary() | kapps_call:call()) ->
                              kapps_api_std_return().
b_channel_status('undefined') -> {'error', 'no_channel_id'};
b_channel_status(ChannelId) when is_binary(ChannelId) ->
    Command = [{<<"Call-ID">>, ChannelId}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Resp = kz_amqp_worker:call_collect(Command
                                      ,fun kapi_call:publish_channel_status_req/1
                                      ,{'ecallmgr', 'true'}
                                      ),
    case Resp of
        {'error', _}=E -> E;
        {_, JObjs} -> channel_status_filter(JObjs)
    end;
b_channel_status(Call) -> b_channel_status(kapps_call:call_id(Call)).

-spec channel_status_filter(kz_json:objects()) ->
                                   {'ok', kz_json:object()} |
                                   {'error', 'not_found'}.
channel_status_filter([]) -> {'error', 'not_found'};
channel_status_filter([JObj|JObjs]) ->
    case kapi_call:channel_status_resp_v(JObj)
        andalso kz_json:get_value(<<"Status">>, JObj) =:= <<"active">>
    of
        'true' -> {'ok', JObj};
        'false' -> channel_status_filter(JObjs)
    end.

%%------------------------------------------------------------------------------
%% @doc How AMQP messages are sent to the mailboxes of processes waiting
%% for them in the receive blocks below.
%% @end
%%------------------------------------------------------------------------------
-type relay_fun() :: fun((pid() | atom(), any()) -> any()).

-spec relay_event(pid(), kz_json:object()) -> any().
relay_event(Pid, JObj) ->
    relay_event(Pid, JObj, fun erlang:send/2).

-spec relay_event(pid(), kz_json:object(), relay_fun()) -> any().
relay_event(Pid, JObj, RelayFun) ->
    RelayFun(Pid, {'amqp_msg', JObj}).

-spec receive_event(timeout()) ->
                           {'ok', kz_json:object()} |
                           {'error', 'timeout'}.
receive_event(Timeout) -> receive_event(Timeout, 'true').

-spec receive_event(timeout(), boolean()) ->
                           {'ok', kz_json:object()} |
                           {'other', kz_json:object() | any()} |
                           {'error', 'timeout'}.
receive_event(T, _) when T =< 0 -> {'error', 'timeout'};
receive_event(Timeout, IgnoreOthers) ->
    Start = os:timestamp(),
    receive
        {'amqp_msg', JObj} -> {'ok', JObj};
        _ when IgnoreOthers ->
            receive_event(kz_time:decr_timeout(Timeout, Start), IgnoreOthers);
        Other -> {'other', Other}
    after
        Timeout -> {'error', 'timeout'}
    end.

-spec audio_macro(audio_macro_prompts(), kapps_call:call()) ->
                         kz_term:ne_binary().
audio_macro([], Call) -> noop(Call);
audio_macro(Prompts, Call) -> audio_macro(Prompts, Call, kz_binary:rand_hex(3)).

-spec audio_macro(audio_macro_prompts(), kapps_call:call(), kz_term:ne_binary()) ->
                         binary().
audio_macro(Prompts, Call, GroupId) ->
    Queue = macros_to_commands(Prompts, Call, GroupId),

    NoopId = noop_id(),
    Commands = [kz_json:from_list(
                  [{<<"Application-Name">>, <<"noop">>}
                  ,{<<"Msg-ID">>, NoopId}
                  ,{<<"Call-ID">>, kapps_call:call_id(Call)}
                  ])
                | Queue
               ],
    Command = [{<<"Application-Name">>, <<"queue">>}
              ,{<<"Commands">>, Commands}
              ],
    send_command(Command, Call),
    NoopId.

-spec macros_to_commands(audio_macro_prompts(), kapps_call:call(), kz_term:ne_binary()) ->
                                kz_json:objects().
macros_to_commands(Prompts, Call, GroupId) ->
    {_, _, Queue} = lists:foldl(fun build_macro/2, {Call, GroupId, []}, Prompts),
    Queue.

-type build_acc() :: {kapps_call:call(), kz_term:ne_binary(), kz_json:objects()}.
-spec build_macro(audio_macro_prompt(), build_acc()) -> build_acc().
build_macro({'play', MediaName}, {Call, GroupId, Queue}) ->
    Command = play_command(MediaName, ?ANY_DIGIT, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'play', MediaName, Terminators}, {Call, GroupId, Queue}) ->
    Command = play_command(MediaName, Terminators, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'play', MediaName, Terminators, Leg}, {Call, GroupId, Queue}) ->
    Command = play_command(MediaName, Terminators, Leg, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'prompt', PromptName}, {Call, GroupId, Queue}) ->
    Command = play_command(kapps_call:get_prompt(Call, PromptName), ?ANY_DIGIT, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'prompt', PromptName, Lang}, {Call, GroupId, Queue}) ->
    Command = play_command(kapps_call:get_prompt(Call, PromptName, Lang), ?ANY_DIGIT, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'prompt', PromptName, Lang, Leg}, {Call, GroupId, Queue}) ->
    Command = play_command(kapps_call:get_prompt(Call, PromptName, Lang)
                          ,?ANY_DIGIT
                          ,Leg
                          ,Call
                          ),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'say', Say}, {Call, GroupId, Queue}) ->
    Command = say_command(Say, <<"name_spelled">>, <<"pronounced">>, kapps_call:language(Call), Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'say', Say, Type}, {Call, GroupId, Queue}) ->
    Command = say_command(Say, Type, <<"pronounced">>, kapps_call:language(Call), Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'say', Say, Type, Method}, {Call, GroupId, Queue}) ->
    Command = say_command(Say, Type, Method, kapps_call:language(Call), Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'say', Say, Type, Method, Language}, {Call, GroupId, Queue}) ->
    Command = say_command(Say, Type, Method, Language, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'tones', Tones}, {Call, GroupId, Queue}) ->
    Command = tones_command(Tones, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'tts', Text}, {Call, GroupId, Queue}) ->
    Command = tts_command(Text, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'tts', Text, Voice}, {Call, GroupId, Queue}) ->
    Command = tts_command(Text, Voice, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'tts', Text, Voice, Lang}, {Call, GroupId, Queue}) ->
    Command = tts_command(Text, Voice, Lang, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]};
build_macro({'tts', Text, Voice, Lang, Terminators}, {Call, GroupId, Queue}) ->
    Command = tts_command(Text, Voice, Lang, Terminators, Call),
    {Call, GroupId, [kz_json:set_value(<<"Group-ID">>, GroupId, Command) | Queue]}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec response(kz_term:ne_binary(), kapps_call:call()) ->
                      {'ok', kz_term:ne_binary()} |
                      {'error', 'no_response'}.
response(Code, Call) ->
    response(Code, 'undefined', Call).

-spec response(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) ->
                      {'ok', kz_term:ne_binary()} |
                      {'error', 'no_response'}.
response(Code, Cause, Call) ->
    response(Code, Cause, 'undefined', Call).

-spec response(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) ->
                      {'ok', kz_term:ne_binary()} |
                      {'error', 'no_response'}.
response(Code, Cause, Media, Call) ->
    kz_call_response:send(Call, Code, Cause, Media).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pickup(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
pickup(TargetCallId, Call) ->
    Command = pickup_command(TargetCallId, Call),
    send_command(Command, Call).

-spec pickup(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
pickup(TargetCallId, Insert, Call) ->
    Command = pickup_command(TargetCallId, Insert, Call),
    send_command(Command, Call).

-spec pickup(kz_term:ne_binary(), kz_term:api_binary(), boolean(), kapps_call:call()) -> 'ok'.
pickup(TargetCallId, Insert, ContinueOnFail, Call) ->
    Command = pickup_command(TargetCallId, Insert, ContinueOnFail, Call),
    send_command(Command, Call).

-spec pickup(kz_term:ne_binary(), kz_term:api_binary(), boolean(), boolean(), kapps_call:call()) -> 'ok'.
pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call) ->
    Command = pickup_command(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call),
    send_command(Command, Call).

-spec pickup(kz_term:ne_binary(), kz_term:api_binary(), boolean(), boolean(), boolean(), kapps_call:call()) -> 'ok'.
pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call) ->
    Command = pickup_command(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call),
    send_command(Command, Call).

-spec pickup_command(kz_term:ne_binary(), kapps_call:call()) -> kz_term:proplist().
pickup_command(TargetCallId, Call) ->
    pickup_command(TargetCallId, <<"tail">>, Call).

-spec pickup_command(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kz_term:proplist().
pickup_command(TargetCallId, Insert, Call) ->
    pickup_command(TargetCallId, Insert, 'false', Call).

-spec pickup_command(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), kapps_call:call()) -> kz_term:proplist().
pickup_command(TargetCallId, Insert, ContinueOnFail, Call) ->
    pickup_command(TargetCallId, Insert, ContinueOnFail, 'true', Call).

-spec pickup_command(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), boolean(), kapps_call:call()) -> kz_term:proplist().
pickup_command(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call) ->
    pickup_command(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, 'false', Call).

-spec pickup_command(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), boolean(), boolean(), kapps_call:call()) -> kz_term:proplist().
pickup_command(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call) ->
    [{<<"Application-Name">>, <<"call_pickup">>}
    ,{<<"Target-Call-ID">>, TargetCallId}
    ,{<<"Insert-At">>, Insert}
    ,{<<"Continue-On-Fail">>, ContinueOnFail}
    ,{<<"Continue-On-Cancel">>, ContinueOnCancel}
    ,{<<"Park-After-Pickup">>, ParkAfterPickup}
    ,{<<"Call-ID">>, kapps_call:call_id(Call)}
    ].

-spec b_pickup(kz_term:ne_binary(), kapps_call:call()) ->
                      {'ok', kz_json:object()}.
b_pickup(TargetCallId, Call) ->
    pickup(TargetCallId, Call),
    wait_for_channel_unbridge().

-spec b_pickup(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) ->
                      {'ok', kz_json:object()}.
b_pickup(TargetCallId, Insert, Call) ->
    pickup(TargetCallId, Insert, Call),
    wait_for_channel_unbridge().

-spec b_pickup(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), kapps_call:call()) ->
                      {'ok', kz_json:object()}.
b_pickup(TargetCallId, Insert, ContinueOnFail, Call) ->
    pickup(TargetCallId, Insert, ContinueOnFail, Call),
    wait_for_channel_unbridge().

-spec b_pickup(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), boolean(), kapps_call:call()) ->
                      {'ok', kz_json:object()}.
b_pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call) ->
    pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call),
    wait_for_channel_unbridge().

-spec b_pickup(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), boolean(), boolean(), kapps_call:call()) ->
                      {'ok', kz_json:object()}.
b_pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call) ->
    pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call),
    wait_for_channel_unbridge().

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec connect_leg(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
connect_leg(TargetCallId, Call) ->
    Command = connect_leg_command(TargetCallId, Call),
    send_command(Command, Call).

-spec connect_leg(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
connect_leg(TargetCallId, Insert, Call) ->
    Command = connect_leg_command(TargetCallId, Insert, Call),
    send_command(Command, Call).

-spec connect_leg(kz_term:ne_binary(), kz_term:api_binary(), boolean(), kapps_call:call()) -> 'ok'.
connect_leg(TargetCallId, Insert, ContinueOnFail, Call) ->
    Command = connect_leg_command(TargetCallId, Insert, ContinueOnFail, Call),
    send_command(Command, Call).

-spec connect_leg(kz_term:ne_binary(), kz_term:api_binary(), boolean(), boolean(), kapps_call:call()) -> 'ok'.
connect_leg(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call) ->
    Command = connect_leg_command(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call),
    send_command(Command, Call).

-spec connect_leg(kz_term:ne_binary(), kz_term:api_binary(), boolean(), boolean(), boolean(), kapps_call:call()) -> 'ok'.
connect_leg(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterConnect_Leg, Call) ->
    Command = connect_leg_command(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterConnect_Leg, Call),
    send_command(Command, Call).

-spec connect_leg_command(kz_term:ne_binary(), kapps_call:call()) -> kz_term:proplist().
connect_leg_command(TargetCallId, Call) ->
    connect_leg_command(TargetCallId, <<"tail">>, Call).

-spec connect_leg_command(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kz_term:proplist().
connect_leg_command(TargetCallId, Insert, Call) ->
    connect_leg_command(TargetCallId, Insert, 'false', Call).

-spec connect_leg_command(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), kapps_call:call()) -> kz_term:proplist().
connect_leg_command(TargetCallId, Insert, ContinueOnFail, Call) ->
    connect_leg_command(TargetCallId, Insert, ContinueOnFail, 'true', Call).

-spec connect_leg_command(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), boolean(), kapps_call:call()) -> kz_term:proplist().
connect_leg_command(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call) ->
    connect_leg_command(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, 'false', Call).

-spec connect_leg_command(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), boolean(), boolean(), kapps_call:call()) -> kz_term:proplist().
connect_leg_command(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call) ->
    [{<<"Application-Name">>, <<"connect_leg">>}
    ,{<<"Target-Call-ID">>, TargetCallId}
    ,{<<"Insert-At">>, Insert}
    ,{<<"Continue-On-Fail">>, ContinueOnFail}
    ,{<<"Continue-On-Cancel">>, ContinueOnCancel}
    ,{<<"Park-After-Pickup">>, ParkAfterPickup}
    ,{<<"Call-ID">>, kapps_call:call_id(Call)}
    ].

-spec b_connect_leg(kz_term:ne_binary(), kapps_call:call()) ->
                           {'ok', kz_json:object()}.
b_connect_leg(TargetCallId, Call) ->
    connect_leg(TargetCallId, Call),
    wait_for_channel_unbridge().

-spec b_connect_leg(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) ->
                           {'ok', kz_json:object()}.
b_connect_leg(TargetCallId, Insert, Call) ->
    connect_leg(TargetCallId, Insert, Call),
    wait_for_channel_unbridge().

-spec b_connect_leg(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), kapps_call:call()) ->
                           {'ok', kz_json:object()}.
b_connect_leg(TargetCallId, Insert, ContinueOnFail, Call) ->
    connect_leg(TargetCallId, Insert, ContinueOnFail, Call),
    wait_for_channel_unbridge().

-spec b_connect_leg(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), boolean(), kapps_call:call()) ->
                           {'ok', kz_json:object()}.
b_connect_leg(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call) ->
    connect_leg(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call),
    wait_for_channel_unbridge().

-spec b_connect_leg(kz_term:ne_binary(), kz_term:ne_binary(), boolean(), boolean(), boolean(), kapps_call:call()) ->
                           {'ok', kz_json:object()}.
b_connect_leg(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call) ->
    connect_leg(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call),
    wait_for_channel_unbridge().

%%------------------------------------------------------------------------------
%% @doc Create a redirect request to the Contact on Server.
%% @end
%%------------------------------------------------------------------------------
-spec redirect(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
redirect(Contact, Call) ->
    redirect(Contact, 'undefined', Call).

-spec redirect(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
redirect(Contact, Server, Call) ->
    lager:debug("redirect to ~s on ~s", [Contact, Server]),
    Command = [{<<"Redirect-Contact">>, Contact}
              ,{<<"Redirect-Server">>, Server}
              ,{<<"Application-Name">>, <<"redirect">>}
              ],
    send_command(Command, Call),
    timer:sleep(2 * ?MILLISECONDS_IN_SECOND),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Create a redirect request to Node.
%% @end
%%------------------------------------------------------------------------------
-spec redirect_to_node(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
redirect_to_node(Contact, Node, Call) ->
    lager:debug("redirect ~s to ~s", [Contact, Node]),
    Command = [{<<"Redirect-Contact">>, Contact}
              ,{<<"Redirect-Node">>, Node}
              ,{<<"Application-Name">>, <<"redirect">>}
              ],
    send_command(Command, Call),
    timer:sleep(2 * ?MILLISECONDS_IN_SECOND),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flush_dtmf(kapps_call:call()) -> kz_term:ne_binary().
flush_dtmf(Call) -> play(<<"silence_stream://50">>, Call).

-spec send_dtmf(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
send_dtmf(DTMFs, Call) ->
    send_dtmf(DTMFs, 'undefined', Call).

-spec send_dtmf(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
send_dtmf(DTMFs, Duration, Call) ->
    Cmd = send_dtmf_command(DTMFs, Duration),
    send_command(Cmd, Call).

-spec send_dtmf_command(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:proplist().
send_dtmf_command(DTMFs, Duration) ->
    props:filter_undefined(
      [{<<"DTMFs">>, DTMFs}
      ,{<<"Duration">>, Duration}
      ,{<<"Application-Name">>, <<"send_dtmf">>}
      ]).

-spec recv_dtmf(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
recv_dtmf(DTMFs, Call) ->
    Cmd = recv_dtmf_command(DTMFs),
    send_command(Cmd, Call).

-spec recv_dtmf_command(kz_term:ne_binary()) -> kz_term:proplist().
recv_dtmf_command(DTMFs) ->
    props:filter_undefined(
      [{<<"DTMFs">>, DTMFs}
      ,{<<"Application-Name">>, <<"recv_dtmf">>}
      ]).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to set channel or call variables.
%%
%% <div class="notice">These are `custom' channel variables for state info only, and
%% can not be used to set system settings</div>
%% @end
%%------------------------------------------------------------------------------

-spec set(kz_term:api_object(), kz_term:api_object(), kapps_call:call()) -> 'ok'.
set(ChannelVars, CallVars, Call) ->
    set(ChannelVars, CallVars, 'undefined', Call).

-spec set(kz_term:api_object(), kz_term:api_object(), kz_term:api_object(), kapps_call:call()) -> 'ok'.
set(ChannelVars, CallVars, AppVars, Call) -> set(ChannelVars, CallVars, AppVars, 'false', Call).

-spec set(kz_term:api_object(), kz_term:api_object(), kz_term:api_object(), boolean(), kapps_call:call()) -> 'ok'.
set('undefined', CallVars, AppVars, ExportAll, Call) -> set(kz_json:new(), CallVars, AppVars, ExportAll, Call);
set(ChannelVars, 'undefined', AppVars, ExportAll, Call) -> set(ChannelVars, kz_json:new(), AppVars, ExportAll, Call);
set(ChannelVars, CallVars, AppVars, ExportAll, Call) ->
    case kz_json:is_empty(ChannelVars)
        andalso kz_json:is_empty(CallVars)
        andalso kz_json:is_empty(AppVars)
    of
        'true' -> 'ok';
        'false' ->
            Command = [{<<"Application-Name">>, <<"set">>}
                      ,{<<"Custom-Application-Vars">>, AppVars}
                      ,{<<"Custom-Call-Vars">>, CallVars}
                      ,{<<"Custom-Channel-Vars">>, ChannelVars}
                      ,{<<"Export-All">>, ExportAll}
                      ,{<<"Insert-At">>, <<"now">>}
                      ],
            send_command(Command, Call)
    end.

-spec set_terminators(kz_term:api_binaries(), kapps_call:call()) -> 'ok'.
set_terminators(Terminators, Call) ->
    Command = [{<<"Application-Name">>, <<"set_terminators">>}
              ,{<<"Terminators">>, Terminators}
              ],
    send_command(Command, Call).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to fetch channel variables.
%%
%% <div class="notice">These are `custom' channel variables for state `info'
%% only, and can not be sued to get switch variables</div>
%% @end
%%------------------------------------------------------------------------------


-spec fetch(kapps_call:call()) -> 'ok'.
fetch(Call) -> fetch('false', Call).

-spec fetch(boolean(), kapps_call:call()) -> 'ok'.
fetch(FromOtherLeg, Call) ->
    Command = [{<<"Application-Name">>, <<"fetch">>}
              ,{<<"Insert-At">>, <<"now">>}
              ,{<<"From-Other-Leg">>, FromOtherLeg}
              ],
    send_command(Command, Call).

-spec b_fetch(kapps_call:call()) -> kapps_api_std_return().
b_fetch(Call) -> b_fetch('false', Call).

-spec b_fetch(boolean(), kapps_call:call()) -> kapps_api_std_return().
b_fetch(FromOtherLeg, Call) ->
    fetch(FromOtherLeg, Call),
    case wait_for_message(Call, <<"fetch">>) of
        {'ok', JObj} ->
            {'ok', kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new())};
        {'error', _}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to ring the channel.
%% @end
%%------------------------------------------------------------------------------

-spec ring(kapps_call:call()) -> 'ok'.
ring(Call) ->
    Command = [{<<"Application-Name">>, <<"ring">>}],
    send_command(Command, Call).

-spec b_ring(kapps_call:call()) ->
                    kapps_api_error() |
                    {'ok', kz_json:object()}.
b_ring(Call) ->
    ring(Call),
    wait_for_message(Call, <<"ring">>).

%%------------------------------------------------------------------------------
%% @doc Instructs the switch to expect to receive a fax.
%% @end
%%------------------------------------------------------------------------------

-spec receive_fax(kapps_call:call()) -> 'ok'.
receive_fax(Call) ->
    receive_fax(get_default_t38_setting(), Call).

-spec receive_fax(boolean(), kapps_call:call()) -> 'ok'.
receive_fax(DefaultFlag, Call) ->
    receive_fax(DefaultFlag, 'undefined', 'undefined', Call).

-spec receive_fax(boolean() | kz_term:api_binary()
                 ,boolean() | kz_term:api_binary()
                 ,kapps_call:call()) -> 'ok'.
receive_fax('undefined', 'undefined', Call) ->
    receive_fax(get_default_t38_setting(), 'undefined', 'undefined', Call);
receive_fax(ResourceFlag, ReceiveFlag, Call) ->
    receive_fax(ResourceFlag, ReceiveFlag, 'undefined', Call).

-spec receive_fax(boolean() | kz_term:api_binary()
                 ,boolean() | kz_term:api_binary()
                 ,kz_term:api_binary()
                 ,kapps_call:call()) -> 'ok'.
receive_fax(ResourceFlag, ReceiveFlag, LocalFilename, Call) ->
    Commands = props:filter_undefined([{<<"Application-Name">>, <<"receive_fax">>}
                                      ,{<<"Fax-Local-Filename">>, LocalFilename}
                                       | get_inbound_t38_settings(ResourceFlag, ReceiveFlag)
                                      ]),
    send_command(Commands, Call).

-spec b_receive_fax(kapps_call:call()) -> wait_for_fax_ret().
b_receive_fax(Call) ->
    receive_fax(Call),
    wait_for_fax().

-spec get_default_t38_setting() -> boolean() | kz_term:ne_binary().
get_default_t38_setting() ->
    case kapps_config:get_ne_binary(<<"fax">>, <<"inbound_t38_default">>, <<"true">>) of
        <<"auto">> -> <<"auto">>;
        Otherwise -> kz_term:is_true(Otherwise)
    end.

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to answer the channel.
%% @end
%%------------------------------------------------------------------------------

-spec answer(kapps_call:call()) -> 'ok'.
answer(Call) -> send_command([{<<"Application-Name">>, <<"answer">>}], Call).

-spec answer_now(kapps_call:call()) -> 'ok'.
answer_now(Call) -> send_command([{<<"Application-Name">>, <<"answer">>}
                                 ,{<<"Insert-At">>, <<"now">>}
                                 ], Call).

-spec b_answer(kapps_call:call()) ->
                      kapps_api_error() |
                      {'ok', kz_json:object()}.
b_answer(Call) ->
    answer(Call),
    wait_for_message(Call, <<"answer">>).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to echo the channel.
%% @end
%%------------------------------------------------------------------------------

-spec echo(kapps_call:call()) -> 'ok'.
echo(Call) -> send_command([{<<"Application-Name">>, <<"echo">>}], Call).

-spec b_echo(kapps_call:call()) ->
                    kapps_api_error() |
                    {'ok', kz_json:object()}.
b_echo(Call) ->
    echo(Call),
    wait_for_message(Call, <<"echo">>).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to break the channel.
%% This request will execute immediately.
%% @end
%%------------------------------------------------------------------------------
-spec break(kapps_call:call()) -> 'ok'.
break(Call) ->
    Command = [{<<"Application-Name">>, <<"break">>}
              ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to hangup the channel.
%% This request will execute immediately.
%% @end
%%------------------------------------------------------------------------------


-spec hangup(kapps_call:call()) -> 'ok'.
hangup(Call) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
              ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

-spec queued_hangup(kapps_call:call()) -> 'ok'.
queued_hangup(Call) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}],
    send_command(Command, Call).

-spec hangup(boolean(), kapps_call:call()) -> 'ok'.
hangup(OtherLegOnly, Call) when is_boolean(OtherLegOnly) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
              ,{<<"Other-Leg-Only">>, OtherLegOnly}
              ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

-spec b_hangup(kapps_call:call()) ->
                      {'ok', 'channel_hungup'}.
b_hangup(Call) ->
    hangup(Call),
    wait_for_hangup().

-spec b_hangup(boolean(), kapps_call:call()) ->
                      {'ok', 'channel_hungup' | 'leg_hangup'}.
b_hangup('false', Call) ->
    hangup(Call),
    wait_for_hangup();
b_hangup('true', Call) ->
    hangup('true', Call),
    wait_for_unbridge().

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to page the call.
%% @end
%%------------------------------------------------------------------------------
-spec page(kz_json:objects(), kapps_call:call()) -> 'ok'.
page(Endpoints, Call) ->
    page(Endpoints, ?DEFAULT_TIMEOUT_S, Call).

-spec page(kz_json:objects(), integer(), kapps_call:call()) -> 'ok'.
page(Endpoints, Timeout, Call) ->
    page(Endpoints, Timeout, 'undefined', Call).

-spec page(kz_json:objects(), integer(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
page(Endpoints, Timeout, CIDName, Call) ->
    page(Endpoints, Timeout, CIDName, 'undefined', Call).

-spec page(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
page(Endpoints, Timeout, CIDName, CIDNumber, Call) ->
    page(Endpoints, Timeout, CIDName, CIDNumber, 'undefined', Call).

-spec page(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kapps_call:call()) -> 'ok'.
page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, Call) ->
    page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, 'undefined', Call).

-spec page(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kz_term:api_object(), kapps_call:call()) -> 'ok'.
page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, CCVs, Call) ->
    page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, CCVs, 'undefined', Call).

-spec page(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kz_term:api_object(), kz_term:api_object(), kapps_call:call()) -> 'ok'.
page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, CCVs, Options, Call) ->
    Command = [{<<"Application-Name">>, <<"page">>}
              ,{<<"Endpoints">>, Endpoints}
              ,{<<"Timeout">>, Timeout}
              ,{<<"Caller-ID-Name">>, CIDName}
              ,{<<"Caller-ID-Number">>, CIDNumber}
              ,{<<"Custom-SIP-Headers">>, SIPHeaders}
              ,{<<"Custom-Channel-Vars">>, CCVs}
              ,{<<"Page-Options">>, Options}
              ],
    send_command(Command, Call).

-spec b_page(kz_json:objects(), kapps_call:call()) ->
                    wait_for_application_return().
b_page(Endpoints, Call) ->
    b_page(Endpoints, ?DEFAULT_TIMEOUT_S, Call).

-spec b_page(kz_json:objects(), integer(), kapps_call:call()) ->
                    wait_for_application_return().
b_page(Endpoints, Timeout, Call) ->
    b_page(Endpoints, Timeout, 'undefined', Call).

-spec b_page(kz_json:objects(), integer(), kz_term:api_binary(), kapps_call:call()) ->
                    wait_for_application_return().
b_page(Endpoints, Timeout, CIDName, Call) ->
    b_page(Endpoints, Timeout, CIDName, 'undefined', Call).

-spec b_page(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) ->
                    wait_for_application_return().
b_page(Endpoints, Timeout, CIDName, CIDNumber, Call) ->
    b_page(Endpoints, Timeout, CIDName, CIDNumber, 'undefined', Call).

-spec b_page(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kapps_call:call()) ->
                    wait_for_application_return().
b_page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, Call) ->
    b_page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, 'undefined', Call).

-spec b_page(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kz_term:api_object(), kapps_call:call()) ->
                    wait_for_application_return().
b_page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, CCVs, Call) ->
    b_page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, CCVs, 'undefined', Call).

-spec b_page(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kz_term:api_object(), kz_term:api_object(), kapps_call:call()) ->
                    wait_for_application_return().
b_page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, CCVs, Options, Call) ->
    page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, CCVs, Options, Call),
    wait_for_application(Call, <<"page">>).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to bridge the call.
%% @end
%%------------------------------------------------------------------------------
bridge_command(Endpoints, Call) ->
    bridge_command(Endpoints, ?DEFAULT_TIMEOUT_S, Call).
bridge_command(Endpoints, Timeout, Call) ->
    bridge_command(Endpoints, Timeout, kapi_dialplan:dial_method_single(), Call).
bridge_command(Endpoints, Timeout, Strategy, Call) ->
    bridge_command(Endpoints, Timeout, Strategy, <<"true">>, Call).
bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Call) ->
    bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, 'undefined', Call).
bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, Call) ->
    bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, 'undefined', Call).
bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, Call) ->
    bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, <<"false">>, Call).
bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, Call) ->
    bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, 'undefined', Call).
bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, FailOnSingleReject, Call) ->
    [{<<"Application-Name">>, <<"bridge">>}
    ,{<<"Endpoints">>, Endpoints}
    ,{<<"Timeout">>, Timeout}
    ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
    ,{<<"Ringback">>, kz_media_util:media_path(Ringback, kapps_call:account_id(Call))}
    ,{<<"Fail-On-Single-Reject">>, FailOnSingleReject}
    ,{<<"Dial-Endpoint-Method">>, Strategy}
    ,{<<"Custom-SIP-Headers">>, SIPHeaders}
    ,{<<"Ignore-Forward">>, IgnoreForward}
    ,{<<"Export-Bridge-Variables">>, ?BRIDGE_EXPORT_VARS}
    ].

-spec bridge(kz_json:objects(), kapps_call:call()) -> 'ok'.
bridge(Endpoints, Call) ->
    Command = bridge_command(Endpoints, Call),
    send_command(Command, Call).

-spec bridge(kz_json:objects(), integer(), kapps_call:call()) -> 'ok'.
bridge(Endpoints, Timeout, Call) ->
    Command = bridge_command(Endpoints, Timeout, Call),
    send_command(Command, Call).

-spec bridge(kz_json:objects(), integer(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
bridge(Endpoints, Timeout, Strategy, Call) ->
    Command = bridge_command(Endpoints, Timeout, Strategy, Call),
    send_command(Command, Call).

-spec bridge(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Call) ->
    Command = bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Call),
    send_command(Command, Call).

-spec bridge(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, Call) ->
    Command = bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, Call),
    send_command(Command, Call).

-spec bridge(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kapps_call:call()) -> 'ok'.
bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, Call) ->
    Command = bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, Call),
    send_command(Command, Call).

-spec bridge(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, Call) ->
    Command = bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, Call),
    send_command(Command, Call).

-spec bridge(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, FailOnSingleReject, Call) ->
    Command = bridge_command(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, FailOnSingleReject, Call),
    send_command(Command, Call).

-spec b_bridge(kz_json:objects(), kapps_call:call()) ->
                      kapps_api_bridge_return().
b_bridge(Endpoints, Call) ->
    bridge(Endpoints, Call),
    b_bridge_wait(?DEFAULT_TIMEOUT_S, Call).

-spec b_bridge(kz_json:objects(), integer(), kapps_call:call()) ->
                      kapps_api_bridge_return().
b_bridge(Endpoints, Timeout, Call) ->
    bridge(Endpoints, Timeout, Call),
    b_bridge_wait(Timeout, Call).

-spec b_bridge(kz_json:objects(), integer(), kz_term:api_binary(), kapps_call:call()) ->
                      kapps_api_bridge_return().
b_bridge(Endpoints, Timeout, Strategy, Call) ->
    bridge(Endpoints, Timeout, Strategy, Call),
    b_bridge_wait(Timeout, Call).

-spec b_bridge(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) ->
                      kapps_api_bridge_return().
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Call),
    b_bridge_wait(Timeout, Call).

-spec b_bridge(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) ->
                      kapps_api_bridge_return().
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, Call),
    b_bridge_wait(Timeout, Call).

-spec b_bridge(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kapps_call:call()) ->
                      kapps_api_bridge_return().
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, Call),
    b_bridge_wait(Timeout, Call).

-spec b_bridge(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kz_term:api_binary(), kapps_call:call()) ->
                      kapps_api_bridge_return().
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, Call),
    b_bridge_wait(Timeout, Call).

-spec b_bridge(kz_json:objects(), integer(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_object(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) ->
                      kapps_api_bridge_return().
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, FailOnSingleReject, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, IgnoreForward, FailOnSingleReject, Call),
    b_bridge_wait(Timeout, Call).

-spec b_bridge_wait(pos_integer(), kapps_call:call()) -> kapps_api_bridge_return().
b_bridge_wait(0, Call) ->
    wait_for_bridge(?BRIDGE_DEFAULT_TIMEOUT + ?EXTRA_BRIDGE_TIMEOUT , Call);
b_bridge_wait(Timeout, Call) ->
    wait_for_bridge((kz_term:to_integer(Timeout) * ?MILLISECONDS_IN_SECOND) + ?EXTRA_BRIDGE_TIMEOUT , Call).

-spec unbridge(kapps_call:call()) -> 'ok'.
unbridge(Call) ->
    Command = unbridge_command(Call),
    send_command(Command, Call).

-spec unbridge(kapps_call:call(), kz_term:api_ne_binary()) -> 'ok'.
unbridge(Call, Leg) ->
    Command = unbridge_command(Call, Leg),
    send_command(Command, Call).

-spec unbridge(kapps_call:call(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> 'ok'.
unbridge(Call, Leg, Insert) ->
    Command = unbridge_command(Call, Leg, Insert),
    send_command(Command, Call).

-spec unbridge_command(kapps_call:call()) -> kz_term:proplist().
unbridge_command(Call) ->
    unbridge_command(Call, <<"Both">>).

-spec unbridge_command(kapps_call:call(), kz_term:api_ne_binary()) -> kz_term:proplist().
unbridge_command(Call, Leg) ->
    unbridge_command(Call, Leg, <<"now">>).

-spec unbridge_command(kapps_call:call(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
unbridge_command(Call, Leg, Insert) ->
    [{<<"Application-Name">>, <<"unbridge">>}
    ,{<<"Insert-At">>, Insert}
    ,{<<"Leg">>, Leg}
    ,{<<"Call-ID">>, kapps_call:call_id(Call)}
    ].

-spec soft_hold(kapps_call:call()) -> 'ok'.
soft_hold(Call) ->
    soft_hold(Call, <<"1">>).

-spec soft_hold(kapps_call:call(), kz_term:api_binary()) -> 'ok'.
soft_hold(Call, UnholdKey) ->
    Command = soft_hold_command(kapps_call:call_id(Call), UnholdKey),
    send_command(Command, Call).

-spec soft_hold_command(kz_term:ne_binary(), kz_term:ne_binary()) ->
                               kz_term:proplist().
soft_hold_command(CallId, UnholdKey) ->
    soft_hold_command(CallId, UnholdKey, 'undefined', 'undefined').

-spec soft_hold_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary()) ->
                               kz_term:proplist().
soft_hold_command(CallId, UnholdKey, AMOH, BMOH) ->
    soft_hold_command(CallId, UnholdKey, AMOH, BMOH, <<"now">>).

-spec soft_hold_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) ->
                               kz_term:proplist().
soft_hold_command(CallId, UnholdKey, AMOH, BMOH, InsertAt) ->
    props:filter_undefined([{<<"Application-Name">>, <<"soft_hold">>}
                           ,{<<"Call-ID">>, CallId}
                           ,{<<"Insert-At">>, InsertAt}
                           ,{<<"Unhold-Key">>, UnholdKey}
                            | build_moh_keys(AMOH, BMOH)
                           ]).

-spec build_moh_keys(kz_term:api_binary(), kz_term:api_binary()) ->
                            kz_term:proplist_kv(kz_term:ne_binary(), kz_term:api_binary()).
build_moh_keys('undefined', _) -> [];
build_moh_keys(AMOH, BMOH) ->
    [{<<"A-MOH">>, AMOH}
    ,{<<"B-MOH">>, BMOH}
    ].

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to park the channel.
%% @end
%%------------------------------------------------------------------------------
-spec hold(kapps_call:call()) -> 'ok'.
hold(Call) -> hold('undefined', Call).

-spec hold(kz_term:api_binary(), kapps_call:call()) -> 'ok'.
hold(MOH, Call) ->
    Command = hold_command(MOH, Call),
    send_command(Command, Call).

-spec hold_command(kapps_call:call() | kz_term:ne_binary()) ->
                          kz_json:object().
hold_command(Call) ->
    hold_command('undefined', Call).

-spec hold_command(kz_term:api_binary(), kapps_call:call() | kz_term:ne_binary()) ->
                          kz_json:object().
hold_command(MOH, CallId=?NE_BINARY) ->
    kz_json:from_list(
      [{<<"Application-Name">>, <<"hold">>}
      ,{<<"Insert-At">>, <<"now">>}
      ,{<<"Hold-Media">>, MOH}
      ,{<<"Call-ID">>, CallId}
      ]);
hold_command(MOH, Call) ->
    hold_command(kz_media_util:media_path(MOH, kapps_call:account_id(Call))
                ,kapps_call:call_id_direct(Call)
                ).

-spec b_hold(kapps_call:call()) ->
                    kapps_api_std_return().
b_hold(Call) -> b_hold('infinity', 'undefined', Call).

-spec b_hold(timeout() | kz_term:api_binary(), kapps_call:call()) ->
                    kapps_api_std_return().
b_hold(Timeout, Call) when is_integer(Timeout);
                           Timeout =:= 'infinity' ->
    b_hold(Timeout, 'undefined', Call);
b_hold(MOH, Call) -> b_hold('infinity', MOH, Call).

-spec b_hold(timeout(), kz_term:api_binary(), kapps_call:call()) ->
                    kapps_api_std_return().
b_hold(Timeout, MOH, Call) ->
    hold(MOH, Call),
    wait_for_message(Call, <<"hold">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, Timeout).



-spec hold_control(kapps_call:call()) -> 'ok'.
hold_control(Call) -> hold_control(<<"toggle">>, Call).

-spec hold_control(kz_term:api_binary(), kapps_call:call()) -> 'ok'.
hold_control(Action, Call) ->
    Command = hold_control_command(Action, Call),
    send_command(Command, Call).

-spec hold_control_command(kapps_call:call() | kz_term:ne_binary()) ->
                                  kz_json:object().
hold_control_command(Call) ->
    hold_control_command(<<"toggle">>, Call).

-spec hold_control_command(kz_term:api_binary(), kapps_call:call() | kz_term:ne_binary()) ->
                                  kz_json:object().
hold_control_command(Action, CallId=?NE_BINARY) ->
    kz_json:from_list(
      [{<<"Application-Name">>, <<"hold_control">>}
      ,{<<"Insert-At">>, <<"now">>}
      ,{<<"Action">>, Action}
      ,{<<"Call-ID">>, CallId}
      ]);
hold_control_command(Action, Call) ->
    hold_control_command(Action, kapps_call:call_id_direct(Call)).

-spec park(kapps_call:call()) -> 'ok'.
park(Call) ->
    Command = park_command(Call),
    send_command(Command, Call).

-spec park_command(kapps_call:call() | kz_term:ne_binary()) -> kz_json:object().
park_command(CallId=?NE_BINARY) ->
    kz_json:from_list(
      [{<<"Application-Name">>, <<"park">>}
      ,{<<"Insert-At">>, <<"now">>}
      ,{<<"Call-ID">>, CallId}
      ]);
park_command(Call) ->
    park_command(kapps_call:call_id(Call)).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to play media to the
%% caller.
%% @end
%%------------------------------------------------------------------------------
-spec prompt(kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binary().
prompt(Prompt, Call) ->
    play(kapps_call:get_prompt(Call, Prompt), Call).

-spec prompt(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binary().
prompt(Prompt, Lang, Call) ->
    play(kapps_call:get_prompt(Call, Prompt, Lang), Call).

-spec b_prompt(kz_term:ne_binary(), kapps_call:call()) -> kapps_api_std_return().
b_prompt(Prompt, Call) ->
    b_play(kapps_call:get_prompt(Call, Prompt), Call).

-spec b_prompt(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kapps_api_std_return().
b_prompt(Prompt, Lang, Call) ->
    b_play(kapps_call:get_prompt(Call, Prompt, Lang), Call).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to play media to the
%% caller.  A list of terminators can be provided that the caller
%% can use to skip playback.
%% @end
%%------------------------------------------------------------------------------
-spec play_command(kz_term:ne_binary(), kapps_call:call() | kz_term:ne_binary()) ->
                          kz_json:object().
play_command(Media, Call) ->
    play_command(Media, ?ANY_DIGIT, Call).

-spec play_command(kz_term:ne_binary(), kz_term:api_binaries(), kapps_call:call() | kz_term:ne_binary()) ->
                          kz_json:object().
play_command(Media, Terminators, Call) ->
    play_command(Media, Terminators, 'undefined', Call).

-spec play_command(kz_term:ne_binary(), kz_term:api_binaries(), kz_term:api_binary(), kapps_call:call() | kz_term:ne_binary()) ->
                          kz_json:object().
play_command(Media, Terminators, Leg, Call) ->
    play_command(Media, Terminators, Leg, 'false', Call).

-spec play_command(kz_term:ne_binary(), kz_term:api_binaries(), kz_term:api_binary(), kz_term:api_boolean(), kapps_call:call() | kz_term:ne_binary()) ->
                          kz_json:object().
play_command(Media, Terminators, Leg, Endless, CallId=?NE_BINARY) ->
    kz_json:from_list(
      [{<<"Application-Name">>, <<"play">>}
      ,{<<"Media-Name">>, Media}
      ,{<<"Terminators">>, play_terminators(Terminators)}
      ,{<<"Leg">>, play_leg(Leg)}
      ,{<<"Call-ID">>, CallId}
      ,{<<"Endless-Playback">>, Endless}
      ]);
play_command(Media, Terminators, Leg, Endless, Call) ->
    play_command(Media, Terminators, Leg, Endless, kapps_call:call_id(Call)).

-spec play_terminators(kz_term:api_binaries()) -> kz_term:ne_binaries().
play_terminators('undefined') -> ?ANY_DIGIT;
play_terminators(Ts) -> lists:usort(Ts).

-spec play_leg(kz_term:api_binary()) -> kz_term:api_binary().
play_leg('undefined') -> 'undefined';
play_leg(Leg) -> kz_binary:ucfirst(Leg).

-spec play(kz_term:ne_binary(), kapps_call:call()) ->
                  kz_term:ne_binary().
play(Media, Call) -> play(Media, ?ANY_DIGIT, Call).

-spec play(kz_term:ne_binary(), kz_term:api_binaries(), kapps_call:call()) ->
                  kz_term:ne_binary().
play(Media, Terminators, Call) ->
    play(Media, Terminators, 'undefined', Call).

-spec play(kz_term:ne_binary(), kz_term:api_binaries(), kz_term:api_binary(), kapps_call:call()) ->
                  kz_term:ne_binary().
play(Media, Terminators, Leg, Call) ->
    play(Media, Terminators, Leg, 'false', Call).

-spec play(kz_term:ne_binary(), kz_term:api_binaries(), kz_term:api_binary(), kz_term:api_boolean(), kapps_call:call()) ->
                  kz_term:ne_binary().
play(Media, Terminators, Leg, Endless, Call) ->
    NoopId = noop_id(),
    Commands = [kz_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                  ,{<<"Call-ID">>, kapps_call:call_id(Call)}
                                  ,{<<"Msg-ID">>, NoopId}
                                  ])
               ,play_command(Media, Terminators, Leg, Endless, Call)
               ],
    Command = [{<<"Application-Name">>, <<"queue">>}
              ,{<<"Commands">>, Commands}
              ],
    send_command(Command, Call),
    NoopId.

-spec b_play(kz_term:ne_binary(), kapps_call:call()) ->
                    kapps_api_std_return().
b_play(Media, Call) ->
    b_play(Media, ?ANY_DIGIT, Call).

-spec b_play(kz_term:ne_binary(), kz_term:api_binaries(), kapps_call:call()) ->
                    kapps_api_std_return().
b_play(Media, Terminators, Call) ->
    b_play(Media, Terminators, 'undefined', Call).

-spec b_play(kz_term:ne_binary(), kz_term:api_binaries(), kz_term:api_binary(), kapps_call:call()) ->
                    kapps_api_std_return().
b_play(Media, Terminators, Leg, Call) ->
    b_play(Media, Terminators, Leg, 'false', Call).

-spec b_play(kz_term:ne_binary(), kz_term:api_binaries(), kz_term:api_binary(), kz_term:api_boolean(), kapps_call:call()) ->
                    kapps_api_std_return().
b_play(Media, Terminators, Leg, Endless, Call) ->
    wait_for_noop(Call, play(Media, Terminators, Leg, Endless, Call)).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to seek through the playing media.
%% This request will execute immediately.
%% @end
%%------------------------------------------------------------------------------
-spec seek(kapps_call:call()) -> kapps_api_std_return().
seek(Call) ->
    seek(?DEFAULT_SEEK_DURATION, Call).

-spec seek(kz_term:api_integer(), kapps_call:call()) -> kapps_api_std_return().
seek(Duration, Call) when Duration > 0 ->
    seek(fastforward, Duration, Call);
seek(Duration, Call) when Duration < 0 ->
    seek(rewind, -Duration, Call);
seek(_Duration, _Call) ->
    ok.

-spec b_seek(atom(), kz_term:api_pos_integer(), kapps_call:call()) -> kapps_api_std_return().
b_seek(Direction, Duration, Call) ->
    wait_for_noop(Call, seek(Direction, Duration, Call)).

-spec seek(atom(), kz_term:api_pos_integer(), kapps_call:call()) -> kapps_api_std_return().
seek(_Direction, 0, _Call) ->
    ok;
seek(Direction, Duration, Call) ->
    NoopId = noop_id(),
    %Commands = [kz_json:from_list([{<<"Application-Name">>, <<"noop">>}
    %                               ,{<<"Call-ID">>, kapps_call:call_id(Call)}
    %                               ,{<<"Msg-ID">>, NoopId}
    %                              ])
    %            ,seek_command(Direction, Duration)
    %           ],
    %Command = [{<<"Application-Name">>, <<"queue">>}
    %           ,{<<"Commands">>, Commands}
    %           ,{<<"Insert-At">>, <<"now">>}
    %          ],
    Command = seek_command(Direction, Duration),
    send_command(Command, Call),
    NoopId.

-spec seek_command(atom(), kz_term:api_pos_integer()) -> kz_json:object().
seek_command(Direction, Duration) ->
    kz_json:from_list([{<<"Application-Name">>, <<"playseek">>}
                       ,{<<"Direction">>,Direction}
                       ,{<<"Duration">>,Duration}
                       ,{<<"Insert-At">>, <<"now">>}
                      ]).

%%------------------------------------------------------------------------------
%% @doc requests the TTS engine to create an audio file to play the desired
%% text.
%% @end
%%------------------------------------------------------------------------------
-spec tts(kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binary().
tts(SayMe, Call) -> tts(SayMe, kazoo_tts:default_voice(), Call).

-spec tts(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> kz_term:ne_binary().
tts(SayMe, Voice, Call) -> tts(SayMe, Voice, kapps_call:language(Call), Call).

-spec tts(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> kz_term:ne_binary().
tts(SayMe, Voice, Lang, Call) -> tts(SayMe, Voice, Lang, ?ANY_DIGIT, Call).

-spec tts(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binaries(), kapps_call:call()) -> kz_term:ne_binary().
tts(SayMe, Voice, Lang, Terminators, Call) ->
    tts(SayMe, Voice, Lang, Terminators, kazoo_tts:default_provider(Call), Call).

-spec tts(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binaries(), kz_term:api_binary(), kapps_call:call()) -> kz_term:ne_binary().
tts(?NE_BINARY = SayMe, Voice, Lang, Terminators, Engine, Call) ->
    NoopId = noop_id(),

    Commands = [kz_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                  ,{<<"Call-ID">>, kapps_call:call_id(Call)}
                                  ,{<<"Msg-ID">>, NoopId}
                                  ])
               ,tts_command(SayMe, Voice, Lang, Terminators, Engine, Call)
               ],
    Command = [{<<"Application-Name">>, <<"queue">>}
              ,{<<"Commands">>, Commands}
              ],
    send_command(Command, Call),
    NoopId.

-spec tts_command(kz_term:api_binary(), kapps_call:call()) -> kz_json:object().
tts_command(SayMe, Call) ->
    tts_command(SayMe, kazoo_tts:default_voice(), Call).

-spec tts_command(kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> kz_json:object().
tts_command(SayMe, Voice, Call) ->
    tts_command(SayMe, Voice, kapps_call:language(Call), Call).

-spec tts_command(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> kz_json:object().
tts_command(SayMe, Voice, Language, Call) ->
    tts_command(SayMe, Voice, Language, ?ANY_DIGIT, Call).

-spec tts_command(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binaries(), kapps_call:call()) -> kz_json:object().
tts_command(SayMe, Voice, Language, Terminators, Call) ->
    tts_command(SayMe, Voice, Language, Terminators, kazoo_tts:default_provider(Call), Call).

-spec tts_command(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binaries(), kz_term:api_binary(), kapps_call:call()) -> kz_json:object().
tts_command(SayMe, Voice, Language, Terminators, Engine, Call) ->
    kz_json:from_list(
      [{<<"Application-Name">>, <<"tts">>}
      ,{<<"Text">>, SayMe}
      ,{<<"Terminators">>, tts_terminators(Terminators)}
      ,{<<"Voice">>, tts_voice(Voice)}
      ,{<<"Language">>, tts_language(Language, Call)}
      ,{<<"Engine">>, tts_engine(Engine, Call)}
      ,{<<"Call-ID">>, kapps_call:call_id(Call)}
      ]).

-spec tts_terminators(kz_term:api_ne_binaries()) -> kz_term:api_ne_binaries().
tts_terminators('undefined') -> ?ANY_DIGIT;
tts_terminators([]) -> 'undefined';
tts_terminators(Terminators) -> Terminators.

-spec tts_voice(kz_term:api_binary()) -> kz_term:ne_binary().
tts_voice('undefined') -> kazoo_tts:default_voice();
tts_voice(Voice) -> Voice.

-spec tts_language(kz_term:api_ne_binary(), kapps_call:call()) -> kz_term:ne_binary().
tts_language('undefined', Call) -> kapps_call:language(Call);
tts_language(Language, _Call) -> Language.

-spec tts_engine(kz_term:api_ne_binary(), kapps_call:call()) -> kz_term:ne_binary().
tts_engine('undefined', Call) -> kazoo_tts:default_provider(Call);
tts_engine(Engine, _Call) -> Engine.

-spec b_tts(kz_term:api_binary(), kapps_call:call()) -> kapps_api_std_return().
b_tts(SayMe, Call) ->
    wait_for_noop(Call, tts(SayMe, Call)).

-spec b_tts(kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> kapps_api_std_return().
b_tts(SayMe, Voice, Call) ->
    wait_for_noop(Call, tts(SayMe, Voice, Call)).

-spec b_tts(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> kapps_api_std_return().
b_tts(SayMe, Voice, Lang, Call) ->
    wait_for_noop(Call, tts(SayMe, Voice, Lang, Call)).

-spec b_tts(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binaries(), kapps_call:call()) -> kapps_api_std_return().
b_tts(SayMe, Voice, Lang, Terminators, Call) ->
    wait_for_noop(Call, tts(SayMe, Voice, Lang, Terminators, Call)).

-spec b_tts(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binaries(), kz_term:api_binary(), kapps_call:call()) -> kapps_api_std_return().
b_tts(SayMe, Voice, Lang, Terminators, Engine, Call) ->
    wait_for_noop(Call, tts(SayMe, Voice, Lang, Terminators, Engine, Call)).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to record a file.
%% A list of keys can be used as the terminator or a silence threshold.
%% @end
%%------------------------------------------------------------------------------
-spec record(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
record(MediaName, Call) ->
    record(MediaName, ?ANY_DIGIT, Call).

-spec record(kz_term:ne_binary(), kz_term:binaries(), kapps_call:call()) -> 'ok'.
record(MediaName, Terminators, Call) ->
    record(MediaName, Terminators, <<"300">>, Call).

-spec record(kz_term:ne_binary(), kz_term:binaries(),  kz_term:api_binary() | integer(), kapps_call:call()) -> 'ok'.
record(MediaName, Terminators, TimeLimit, Call) ->
    record(MediaName, Terminators, TimeLimit, <<"200">>,  Call).

-spec record(kz_term:ne_binary(), kz_term:binaries(),  kz_term:api_binary() | integer(), kz_term:api_binary() | integer(), kapps_call:call()) -> 'ok'.
record(MediaName, Terminators, TimeLimit, SilenceThreshold, Call) ->
    record(MediaName, Terminators, TimeLimit, SilenceThreshold, <<"5">>, Call).

-spec record(kz_term:ne_binary(), kz_term:binaries(),  kz_term:api_binary() | integer(), kz_term:api_binary() | integer(),  kz_term:api_binary() | integer(), kapps_call:call()) -> 'ok'.
record(MediaName, Terminators, TimeLimit, SilenceThreshold, SilenceHits, Call) ->
    Command = [{<<"Application-Name">>, <<"record">>}
              ,{<<"Media-Name">>, MediaName}
              ,{<<"Terminators">>, Terminators}
              ,{<<"Time-Limit">>, TimeLimit}
              ,{<<"Silence-Threshold">>, SilenceThreshold}
              ,{<<"Silence-Hits">>, SilenceHits}
              ],
    send_command(Command, Call).

-spec b_record(kz_term:ne_binary(), kapps_call:call()) -> kapps_api_std_return().
b_record(MediaName, Call) ->
    b_record(MediaName, ?ANY_DIGIT, Call).

-spec b_record(kz_term:ne_binary(), kz_term:binaries(), kapps_call:call()) -> kapps_api_std_return().
b_record(MediaName, Terminators, Call) ->
    b_record(MediaName, Terminators, <<"300">>, Call).

-spec b_record(kz_term:ne_binary(), kz_term:binaries(), kz_term:api_binary() | integer(), kapps_call:call()) -> kapps_api_std_return().
b_record(MediaName, Terminators, TimeLimit, Call) ->
    b_record(MediaName, Terminators, TimeLimit, <<"200">>,  Call).

-spec b_record(kz_term:ne_binary(), kz_term:binaries(), kz_term:api_binary() | integer(), kz_term:api_binary() | integer(), kapps_call:call()) -> kapps_api_std_return().
b_record(MediaName, Terminators, TimeLimit, SilenceThreshold, Call) ->
    b_record(MediaName, Terminators, TimeLimit, SilenceThreshold, <<"5">>, Call).

-spec b_record(kz_term:ne_binary(), kz_term:binaries(), kz_term:api_binary() | integer(), kz_term:api_binary() | integer(), kz_term:api_binary() | integer(), kapps_call:call()) -> kapps_api_std_return().
b_record(MediaName, Terminators, TimeLimit, SilenceThreshold, SilenceHits, Call) ->
    record(MediaName, Terminators, TimeLimit, SilenceThreshold, SilenceHits, Call),
    wait_for_headless_application(<<"record">>
                                 ,{<<"RECORD_START">>, <<"RECORD_STOP">>}
                                 ,<<"call_event">>
                                 ,fun(JObj) -> verify_media_name(JObj, MediaName) end
                                 ,'infinity'
                                 ).

-spec verify_media_name(kz_json:object(), kz_term:ne_binary()) -> boolean().
verify_media_name(JObj, MediaName) ->
    case kzc_recording:get_response_media(JObj) of
        {_, MediaName} -> 'true';
        _ -> 'false'
    end.

-spec start_record_call(kz_term:proplist(), kapps_call:call()) -> 'ok'.
start_record_call(Media, Call) ->
    record_call(Media, <<"start">>, Call).

-spec start_record_call(kz_term:proplist(), kz_term:api_binary() | pos_integer(), kapps_call:call()) -> 'ok'.
start_record_call(Media, TimeLimit, Call) ->
    record_call(Media, <<"start">>, TimeLimit, Call).

-spec start_record_call(kz_term:proplist(), kz_term:api_binary() | pos_integer(), kz_term:ne_binaries(), kapps_call:call()) -> 'ok'.
start_record_call(Media, TimeLimit, Terminators, Call) ->
    record_call(Media, <<"start">>, TimeLimit, Terminators, Call).

-spec stop_record_call(kz_term:proplist(), kapps_call:call()) -> 'ok'.
stop_record_call(Media, Call) ->
    record_call(Media, <<"stop">>, Call).

-spec mask_record_call(kz_term:proplist(), kapps_call:call()) -> 'ok'.
mask_record_call(Media, Call) ->
    record_call(Media, <<"mask">>, Call).

-spec unmask_record_call(kz_term:proplist(), kapps_call:call()) -> 'ok'.
unmask_record_call(Media, Call) ->
    record_call(Media, <<"unmask">>, Call).

-spec record_call(kz_term:proplist(), kapps_call:call()) -> 'ok'.
record_call(Media, Call) ->
    record_call(Media, <<"start">>, Call).

-spec record_call(kz_term:proplist(), kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
record_call(Media, Action, Call) ->
    record_call(Media, Action, 600, Call).

-spec record_call(kz_term:proplist(), kz_term:ne_binary(),  kz_term:api_binary() | pos_integer(), kapps_call:call()) -> 'ok'.
record_call(Media, Action, TimeLimit, Call) ->
    record_call(Media, Action, TimeLimit, ?ANY_DIGIT, Call).

-spec record_call(kz_term:proplist(), kz_term:ne_binary(),  kz_term:api_binary() | pos_integer(), list(), kapps_call:call()) -> 'ok'.
record_call(Media, Action, TimeLimit, Terminators, Call) ->
    Limit = props:get_value(<<"Time-Limit">>, Media, kz_term:to_binary(TimeLimit)),
    Command = props:filter_undefined(
                [{<<"Application-Name">>, <<"record_call">>}
                ,{<<"Record-Action">>, Action}
                ,{<<"Time-Limit">>, Limit}
                ,{<<"Terminators">>, Terminators}
                ,{<<"Insert-At">>, <<"now">>}
                 | Media
                ]),
    send_command(Command, Call).

-spec b_record_call(kz_term:proplist(), kapps_call:call()) ->
                           wait_for_headless_application_return().
b_record_call(MediaName, Call) ->
    record_call(MediaName, Call),
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, 'infinity').

-spec b_record_call(kz_term:proplist(), kz_term:ne_binary(), kapps_call:call()) ->
                           wait_for_headless_application_return().
b_record_call(MediaName, Action, Call) ->
    record_call(MediaName, Action, Call),
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, 'infinity').

-spec b_record_call(kz_term:proplist(), kz_term:ne_binary(), kz_term:api_binary() | pos_integer(), kapps_call:call()) ->
                           wait_for_headless_application_return().
b_record_call(MediaName, Action, TimeLimit, Call) ->
    b_record_call(MediaName, Action, TimeLimit, ?ANY_DIGIT, Call).

-spec b_record_call(kz_term:proplist(), kz_term:ne_binary(), kz_term:api_binary() | pos_integer(), list(), kapps_call:call()) ->
                           wait_for_headless_application_return().
b_record_call(MediaName, Action, TimeLimit, Terminators, Call) ->
    record_call(MediaName, Action, TimeLimit, Terminators, Call),
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, 'infinity').

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to store the file.
%% @end
%%------------------------------------------------------------------------------
-type b_store_return() :: {'error', 'timeout' | kz_json:object()} |
                          {'ok', kz_json:object()}.



-spec store(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
store(MediaName, Transfer, Call) ->
    store(MediaName, Transfer, <<"put">>, Call).

-spec store(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
store(MediaName, Transfer, Method, Call) ->
    store(MediaName, Transfer, Method, [kz_json:new()], Call).

-spec store(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_json:objects(), kapps_call:call()) -> 'ok'.
store(MediaName, Transfer, Method, Headers, Call) ->
    store(MediaName, Transfer, Method, Headers, 'false', Call).

-spec store(kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_json:objects(), boolean(), kapps_call:call()) -> 'ok'.
store(MediaName, Transfer, Method, Headers, SuppressReport, Call) ->
    Command = [{<<"Application-Name">>, <<"store">>}
              ,{<<"Media-Name">>, MediaName}
              ,{<<"Media-Transfer-Method">>, Method}
              ,{<<"Media-Transfer-Destination">>, Transfer}
              ,{<<"Additional-Headers">>, Headers}
              ,{<<"Insert-At">>, <<"now">>}
              ,{<<"Suppress-Error-Report">>, SuppressReport}
              ],
    send_command(Command, Call).

-spec b_store(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> b_store_return().
b_store(MediaName, Transfer, Call) ->
    b_store(MediaName, Transfer, <<"put">>, Call).

-spec b_store(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> b_store_return().
b_store(MediaName, Transfer, Method, Call) ->
    b_store(MediaName, Transfer, Method, [kz_json:new()], Call).

-spec b_store(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects(), kapps_call:call()) -> b_store_return().
b_store(MediaName, Transfer, Method, Headers, Call) ->
    b_store(MediaName, Transfer, Method, Headers, 'false', Call).

-spec b_store(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects(), boolean(), kapps_call:call()) -> b_store_return().
b_store(MediaName, Transfer, Method, Headers, SuppressReport, Call) ->
    store(MediaName, Transfer, Method, Headers, SuppressReport, Call),
    wait_for_headless_application(<<"store">>).

-spec b_store_vm(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects(), boolean(), kapps_call:call()) ->
                        wait_for_headless_application_return().
b_store_vm(MediaName, Transfer, Method, Headers, SuppressReport, Call) ->
    Command = [{<<"Application-Name">>, <<"store_vm">>}
              ,{<<"Media-Name">>, MediaName}
              ,{<<"Media-Transfer-Method">>, Method}
              ,{<<"Media-Transfer-Destination">>, Transfer}
              ,{<<"Additional-Headers">>, Headers}
              ,{<<"Insert-At">>, <<"now">>}
              ,{<<"Suppress-Error-Report">>, SuppressReport}
              ],
    send_command(Command, Call),
    wait_for_headless_application(<<"store_vm">>).

-spec audio_level_command(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary() | integer()) ->
                                 kz_term:proplist().
audio_level_command(_Call, Mode, Action, Level) ->
    [{<<"Application-Name">>, <<"audio_level">>}
    ,{<<"Action">>, Action}
    ,{<<"Level">>, kz_term:to_binary(Level)}
    ,{<<"Mode">>, Mode}
    ,{<<"Insert-At">>, <<"now">>}
    ].

-spec audio_level(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary() | integer()) -> 'ok'.
audio_level(Call, Mode, Action, Level) ->
    Command = audio_level_command(Call, Mode, Action, Level),
    send_command(Command, Call).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to store a fax document caller.
%% @end
%%------------------------------------------------------------------------------
-spec store_fax(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
store_fax(URL, Call) ->
    store_fax(URL, 'undefined', Call).

-spec store_fax(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
store_fax(URL, LocalFile, Call) ->
    Command = props:filter_undefined([{<<"Application-Name">>, <<"store_fax">>}
                                     ,{<<"Media-Transfer-Method">>, <<"put">>}
                                     ,{<<"Media-Transfer-Destination">>, URL}
                                     ,{<<"Fax-Local-Filename">>, LocalFile}
                                     ,{<<"Insert-At">>, <<"now">>}
                                     ]),
    send_command(Command, Call).

-spec b_store_fax(kz_term:ne_binary(), kapps_call:call()) -> b_store_return().
b_store_fax(URL, Call) ->
    store_fax(URL, Call),
    wait_for_headless_application(<<"store_fax">>).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to play tones to the caller.
%% @end
%%------------------------------------------------------------------------------
-spec tones(kz_json:objects(), kapps_call:call()) -> 'ok'.
tones(Tones, Call) ->
    Command = [{<<"Application-Name">>, <<"tones">>}
              ,{<<"Tones">>, Tones}
              ],
    send_command(Command, Call).

-spec tones_command(kz_json:objects(), kapps_call:call()) -> kz_json:object().
tones_command(Tones, Call) ->
    CallId = kapps_call:call_id(Call),
    kz_json:from_list([{<<"Application-Name">>, <<"tones">>}
                      ,{<<"Tones">>, Tones}
                      ,{<<"Call-ID">>, CallId}
                      ]).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to prompt a
%% caller, and collect a number of DTMF events.
%% @end
%%------------------------------------------------------------------------------
-spec prompt_and_collect_digit(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
prompt_and_collect_digit(Prompt, Call) ->
    prompt_and_collect_digits(1, 1, Prompt, Call).

-spec prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,kapps_call:call()) ->
                                       'ok'.
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, 1,  Call).

-spec prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,integer(), kapps_call:call()) ->
                                       'ok'.
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, 3 * ?MILLISECONDS_IN_SECOND, Call).

-spec prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,integer(), integer(), kapps_call:call()) ->
                                       'ok'.
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, 'undefined', Call).

-spec prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,integer(), integer(), kz_term:api_binary()
                               ,kapps_call:call()) ->
                                       'ok'.
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, <<"\\d+">>, Call).

-spec prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,integer(), integer(), kz_term:api_binary()
                               ,kz_term:ne_binary(), kapps_call:call()) ->
                                       'ok'.
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, [<<"#">>], Call).

-spec prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,integer(), integer(), kz_term:api_binary()
                               ,kz_term:ne_binary(), kz_term:ne_binaries(), kapps_call:call()) ->
                                       'ok'.
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Terminators, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, kapps_call:get_prompt(Call, Prompt), Tries, Timeout, InvalidPrompt, Regex, Terminators, Call).

-spec b_prompt_and_collect_digit(kz_term:ne_binary(), kapps_call:call()) ->
                                        b_play_and_collect_digits_return().
b_prompt_and_collect_digit(Prompt, Call) ->
    b_prompt_and_collect_digits(1, 1, Prompt, Call).

-spec b_prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                                 ,kapps_call:call()) ->
                                         b_play_and_collect_digits_return().
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, 3,  Call).

-spec b_prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                                 ,integer(), kapps_call:call()) ->
                                         b_play_and_collect_digits_return().
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, ?DEFAULT_COLLECT_TIMEOUT, Call).

-spec b_prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                                 ,integer(), integer(), kapps_call:call()) ->
                                         b_play_and_collect_digits_return().
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, 'undefined', Call).

-spec b_prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                                 ,integer(), integer(), kz_term:api_binary()
                                 ,kapps_call:call()) ->
                                         b_play_and_collect_digits_return().
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, <<"\\d+">>, Call).

-spec b_prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                                 ,integer(), integer(), kz_term:api_binary()
                                 ,kz_term:ne_binary(), kapps_call:call()) ->
                                         b_play_and_collect_digits_return().
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, ?ANY_DIGIT, Call).

-spec b_prompt_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                                 ,integer(), integer(), kz_term:api_binary()
                                 ,kz_term:ne_binary(), kz_term:ne_binaries(), kapps_call:call()) ->
                                         b_play_and_collect_digits_return().
b_prompt_and_collect_digits(_MinDigits, _MaxDigits, _Prompt, 0, _Timeout, 'undefined', _Regex, _Terminators, _Call) ->
    {'ok', <<>>};
b_prompt_and_collect_digits(_MinDigits, _MaxDigits, _Prompt, 0, _Timeout, InvalidPrompt, _Regex, _Terminators, Call) ->
    _ = b_prompt(InvalidPrompt, Call),
    {'ok', <<>>};
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Terminators, Call) ->
    b_play_and_collect_digits(MinDigits
                             ,MaxDigits
                             ,kapps_call:get_prompt(Call, Prompt)
                             ,Tries
                             ,Timeout
                             ,InvalidPrompt
                             ,Regex
                             ,Terminators
                             ,Call
                             ).

-type b_play_and_collect_digits_return() ::
        {'error', 'channel_hungup' | 'channel_unbridge' | kz_json:object()} |
        {'ok', binary()}.

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to play media to a
%% caller, and collect a number of DTMF events.
%% @end
%%------------------------------------------------------------------------------
-spec play_and_collect_digit(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
play_and_collect_digit(Media, Call) ->
    play_and_collect_digits(1, 1, Media, Call).

-spec play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                             ,kapps_call:call()) ->
                                     'ok'.
play_and_collect_digits(MinDigits, MaxDigits, Media, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, 1,  Call).

-spec play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                             ,integer(), kapps_call:call()) ->
                                     'ok'.
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, 3 * ?MILLISECONDS_IN_SECOND, Call).

-spec play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                             ,integer(), integer(), kapps_call:call()) ->
                                     'ok'.
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, 'undefined', Call).

-spec play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                             ,integer(), integer(), kz_term:api_binary()
                             ,kapps_call:call()) ->
                                     'ok'.
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, <<"\\d+">>, Call).

-spec play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                             ,integer(), integer(), kz_term:api_binary()
                             ,kz_term:ne_binary(), kapps_call:call()) ->
                                     'ok'.
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, [<<"#">>], Call).

-spec play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                             ,integer(), integer(), kz_term:api_binary()
                             ,kz_term:ne_binary(), kz_term:ne_binaries(), kapps_call:call()) ->
                                     'ok'.
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, Call) ->
    Command = [{<<"Application-Name">>, <<"play_and_collect_digits">>}
              ,{<<"Minimum-Digits">>, MinDigits}
              ,{<<"Maximum-Digits">>, MaxDigits}
              ,{<<"Timeout">>, Timeout}
              ,{<<"Terminators">>, Terminators}
              ,{<<"Media-Name">>, Media}
              ,{<<"Media-Tries">>, Tries}
              ,{<<"Failed-Media-Name">>, MediaInvalid}
              ,{<<"Digits-Regex">>, Regex}
              ],
    send_command(Command, Call).

-spec b_play_and_collect_digit(kz_term:ne_binary(), kapps_call:call()) ->
                                      b_play_and_collect_digits_return().
b_play_and_collect_digit(Media, Call) ->
    b_play_and_collect_digits(1, 1, Media, Call).

-spec b_play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,kapps_call:call()) ->
                                       b_play_and_collect_digits_return().
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, 3,  Call).

-spec b_play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,integer(), kapps_call:call()) ->
                                       b_play_and_collect_digits_return().
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, ?DEFAULT_COLLECT_TIMEOUT, Call).

-spec b_play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,integer(), integer(), kapps_call:call()) ->
                                       b_play_and_collect_digits_return().
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, 'undefined', Call).

-spec b_play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,integer(), integer(), kz_term:api_binary()
                               ,kapps_call:call()) ->
                                       b_play_and_collect_digits_return().
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, <<"[\\d\\*\\#]+">>, Call).

-spec b_play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,integer(), integer(), kz_term:api_binary()
                               ,kz_term:ne_binary(), kapps_call:call()) ->
                                       b_play_and_collect_digits_return().
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, ?ANY_DIGIT, Call).

-spec b_play_and_collect_digits(integer(), integer(), kz_term:ne_binary()
                               ,integer(), integer(), kz_term:api_binary()
                               ,kz_term:ne_binary(), kz_term:ne_binaries(), kapps_call:call()) ->
                                       b_play_and_collect_digits_return().
b_play_and_collect_digits(_MinDigits, _MaxDigits, _Media, 0, _Timeout, 'undefined', _Regex, _Terminators, _Call) ->
    {'ok', <<>>};
b_play_and_collect_digits(_MinDigits, _MaxDigits, _Media, 0, _Timeout, MediaInvalid, _Regex, _Terminators, Call) ->
    _ = b_play(MediaInvalid, Call),
    {'ok', <<>>};
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, Call) ->
    NoopId = play(Media, Terminators, Call),
    case collect_digits(MaxDigits, Timeout, ?DEFAULT_INTERDIGIT_TIMEOUT, NoopId, Call) of
        {'ok', Digits}=Ok ->
            case re:run(Digits, Regex) of
                {'match', _} when byte_size(Digits) >= MinDigits ->
                    Ok;
                _ ->
                    b_play_and_collect_digits(MinDigits, MaxDigits
                                             ,Media, Tries - 1
                                             ,Timeout, MediaInvalid
                                             ,Regex, Terminators
                                             ,Call
                                             )
            end;
        {'error', _}=Else -> Else
    end.

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to say text to a caller.
%% @end
%%------------------------------------------------------------------------------
-spec say(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
say(Say, Call) ->
    say(Say, <<"name_spelled">>, Call).

-spec say(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
say(Say, Type, Call) ->
    say(Say, Type, <<"pronounced">>, Call).

-spec say(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
say(Say, Type, Method, Call) ->
    say(Say, Type, Method, kapps_call:language(Call), Call).

-spec say(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
say(Say, Type, Method, Language, Call) ->
    say(Say, Type, Method, Language, 'undefined', Call).

-spec say(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
say(Say, Type, Method, Language, Gender, Call) ->
    Command = say_command(Say, Type, Method, Language, Gender, Call),
    send_command(Command, Call).


-spec say_command(kz_term:ne_binary(), kapps_call:call()) -> kz_json:object().
say_command(Say, Call) ->
    say_command(Say, <<"name_spelled">>, Call).

-spec say_command(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kz_json:object().
say_command(Say, Type, Call) ->
    say_command(Say, Type, <<"pronounced">>, Call).

-spec say_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kz_json:object().
say_command(Say, Type, Method, Call) ->
    say_command(Say, Type, Method, kapps_call:language(Call), Call).

-spec say_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kz_json:object().
say_command(Say, Type, Method, Language, Call) ->
    say_command(Say, Type, Method, Language, 'undefined', Call).

-spec say_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> kz_json:object().
say_command(Say, Type, Method, Language, Gender, Call) ->
    kz_json:from_list(
      [{<<"Application-Name">>, <<"say">>}
      ,{<<"Say-Text">>, Say}
      ,{<<"Type">>, say_type(Type)}
      ,{<<"Method">>, say_method(Method)}
      ,{<<"Language">>, say_language(Language, Call)}
      ,{<<"Gender">>, say_gender(Gender)}
      ,{<<"Call-ID">>, kapps_call:call_id(Call)}
      ]).

say_type('undefined') -> <<"name_spelled">>;
say_type(T) -> T.

say_method('undefined') -> <<"pronounced">>;
say_method(M) -> M.

say_language('undefined', Call) -> kapps_call:language(Call);
say_language(L, _Call) -> L.

-spec say_gender(kz_term:api_binary()) -> kz_term:api_binary().
say_gender('undefined') -> 'undefined';
say_gender(Gender) ->
    say_gender_validate(kz_term:to_lower_binary(Gender)).

-spec say_gender_validate(kz_term:ne_binary()) -> kz_term:ne_binary().
say_gender_validate(<<"masculine">> = G) -> G;
say_gender_validate(<<"feminine">> = G) -> G;
say_gender_validate(<<"neuter">> = G) -> G.

-spec b_say(kz_term:ne_binary(), kapps_call:call()) -> kapps_api_std_return().
b_say(Say, Call) ->
    say(Say, Call),
    wait_for_say(Call).

-spec b_say(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kapps_api_std_return().
b_say(Say, Type, Call) ->
    say(Say, Type, Call),
    wait_for_say(Call).

-spec b_say(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kapps_api_std_return().
b_say(Say, Type, Method, Call) ->
    say(Say, Type, Method, Call),
    wait_for_say(Call).

-spec b_say(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kapps_api_std_return().
b_say(Say, Type, Method, Language, Call) ->
    say(Say, Type, Method, Language, Call),
    wait_for_say(Call).

-spec b_say(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> kapps_api_std_return().
b_say(Say, Type, Method, Language, Gender, Call) ->
    say(Say, Type, Method, Language, Gender, Call),
    wait_for_say(Call).

-spec wait_for_say(kapps_call:call()) -> kapps_api_std_return().
wait_for_say(Call) ->
    wait_for_message(Call, <<"say">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, 'infinity').

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to bridge a caller
%% with a conference, with optional entry flags.
%% @end
%%------------------------------------------------------------------------------
-spec conference(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
conference(ConfId, Call) ->
    conference(ConfId, 'false', Call).

-spec conference(kz_term:ne_binary(), boolean(), kapps_call:call()) -> 'ok'.
conference(ConfId, Mute, Call) ->
    conference(ConfId, Mute, 'false', Call).

-spec conference(kz_term:ne_binary(), boolean(), boolean(), kapps_call:call()) -> 'ok'.
conference(ConfId, Mute, Deaf, Call) ->
    conference(ConfId, Mute, Deaf, 'false', Call).

-spec conference(kz_term:ne_binary(), boolean(), boolean(), boolean(), kapps_call:call()) -> 'ok'.
conference(ConfId, Mute, Deaf, Moderator, Call) ->
    conference(ConfId, Mute, Deaf, Moderator, <<"undefined">>, Call).

-spec conference(kz_term:ne_binary(), boolean(), boolean(), boolean(), kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
conference(ConfId, Mute, Deaf, Moderator, ProfileName, Call) ->
    conference(ConfId, Mute, Deaf, Moderator, ProfileName, 'false', Call).

-spec conference(kz_term:ne_binary(), boolean(), boolean(), boolean(), kz_term:ne_binary(), boolean(), kapps_call:call()) -> 'ok'.
conference(ConfId, Mute, Deaf, Moderator, ProfileName, Reinvite, Call) ->
    Command = [{<<"Application-Name">>, <<"conference">>}
              ,{<<"Conference-ID">>, ConfId}
              ,{<<"Mute">>, Mute}
              ,{<<"Deaf">>, Deaf}
              ,{<<"Moderator">>, Moderator}
              ,{<<"Profile">>, ProfileName}
              ,{<<"Reinvite">>, Reinvite}
              ,{<<"Account-ID">>, kapps_call:account_id(Call)}
              ],
    send_command(Command, Call).

-spec b_conference(kz_term:ne_binary(), kapps_call:call()) -> kapps_api_std_return().
b_conference(ConfId, Call) ->
    b_conference(ConfId, 'false', Call).

-spec b_conference(kz_term:ne_binary(), boolean(), kapps_call:call()) -> kapps_api_std_return().
b_conference(ConfId, Mute, Call) ->
    b_conference(ConfId, Mute, 'false', Call).

-spec b_conference(kz_term:ne_binary(), boolean(), boolean(), kapps_call:call()) -> kapps_api_std_return().
b_conference(ConfId, Mute, Deaf, Call) ->
    b_conference(ConfId, Mute, Deaf, 'false', Call).

-spec b_conference(kz_term:ne_binary(), boolean(), boolean(), boolean(), kapps_call:call()) -> kapps_api_std_return().
b_conference(ConfId, Mute, Deaf, Moderator, Call) ->
    b_conference(ConfId, Mute, Deaf, Moderator, <<"default">>, Call).

-spec b_conference(kz_term:ne_binary(), boolean(), boolean(), boolean(), kz_term:ne_binary(), kapps_call:call()) -> kapps_api_std_return().
b_conference(ConfId, Mute, Deaf, Moderator, Profile, Call) ->
    b_conference(ConfId, Mute, Deaf, Moderator, Profile, 'false', Call).

-spec b_conference(kz_term:ne_binary(), boolean(), boolean(), boolean(), kz_term:ne_binary(), boolean(), kapps_call:call()) -> kapps_api_std_return().
b_conference(ConfId, Mute, Deaf, Moderator, Profile, Reinvite, Call) ->
    conference(ConfId, Mute, Deaf, Moderator, Profile, Reinvite, Call),
    wait_for_message(Call, <<"conference">>, <<"CHANNEL_EXECUTE">>).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to preform a `noop'.
%% @end
%%------------------------------------------------------------------------------
-spec noop_id() -> kz_term:ne_binary().
noop_id() -> kz_datamgr:get_uuid().

-spec noop(kapps_call:call()) -> kz_term:ne_binary().
noop(Call) ->
    NoopId = noop_id(),
    Command = [{<<"Application-Name">>, <<"noop">>}
              ,{<<"Msg-ID">>, NoopId}
              ],
    send_command(Command, Call),
    NoopId.

-spec b_noop(kapps_call:call()) -> kapps_api_std_return().
b_noop(Call) -> wait_for_noop(Call, noop(Call)).

%%------------------------------------------------------------------------------
%% @doc Produces the low level AMQP request to flush the command
%% queue.
%% @end
%%------------------------------------------------------------------------------
-spec flush(kapps_call:call()) -> binary().
flush(Call) ->
    NoopId = noop_id(),
    Command = [{<<"Application-Name">>, <<"noop">>}
              ,{<<"Msg-ID">>, NoopId}
              ,{<<"Insert-At">>, <<"flush">>}
              ],
    send_command(Command, Call),
    NoopId.

-spec b_flush(kapps_call:call()) -> kapps_api_std_return().
b_flush(Call) -> wait_for_noop(Call, flush(Call)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec privacy(kapps_call:call()) -> 'ok'.
privacy(Call) -> privacy(<<"full">>, Call).

-spec privacy(kz_term:api_ne_binary(), kapps_call:call()) -> 'ok'.
privacy('undefined', Call) -> privacy(Call);
privacy(Mode, Call) ->
    Command = [{<<"Application-Name">>, <<"privacy">>}
              ,{<<"Privacy-Mode">>, Mode}
              ],
    send_command(Command, Call).

-spec b_privacy(kapps_call:call()) ->
                       kapps_api_error() |
                       {'ok', kz_json:object()}.
b_privacy(Call) -> b_privacy(<<"full">>, Call).

-spec b_privacy(kz_term:api_ne_binary(), kapps_call:call()) ->
                       kapps_api_error() |
                       {'ok', kz_json:object()}.
b_privacy('undefined', Call) -> b_privacy(Call);
b_privacy(Mode, Call) ->
    privacy(Mode, Call),
    wait_for_message(Call, <<"privacy">>).

%%------------------------------------------------------------------------------
%% @doc This function is intended for use with `audio_macro' or manually started
%% media playback queued with a `noop' as the final action.
%% This function will wait forever for the (or any) `noop' event,
%% collecting digits while it does so.  When the `noop' comes in the Timeout
%% timer is started (unless any digit has been pressed in which case the
%% `Interdigit' timer is used).
%%
%% Once the timer has expired the collected digits are
%% returned (possibly just an empty binary).  However, digits can
%% be returned prior to the timer expiration if the last collected
%% digit is in the list of terminators (no returned if so). Digits
%% can also be returned if the number of collected digits exceeds the
%% `MaxDigits'.
%%
%% <div class="notice">This function should NOT be called if `ecallmgr' control
%% queue does not have a `noop' queued.  Otherwise this will block execution until
%% the call is terminated.</div>
%% @end
%%------------------------------------------------------------------------------
-type collect_digits_return() :: {'error','channel_hungup' | 'channel_unbridge' | kz_json:object()} |
                                 {'ok', binary()}.

-record(wcc_collect_digits, {max_digits :: pos_integer()
                            ,timeout = ?DEFAULT_DIGIT_TIMEOUT :: timeout()
                            ,interdigit = ?DEFAULT_INTERDIGIT_TIMEOUT :: pos_integer()
                            ,noop_id :: kz_term:api_binary()
                            ,terminators = [<<"#">>] :: kz_term:ne_binaries()
                            ,call :: kapps_call:call()
                            ,digits_collected = <<>> :: binary()
                            ,after_timeout = ?MILLISECONDS_IN_DAY :: pos_integer()
                            ,flush_on_digit = true
                            }).
-type wcc_collect_digits() :: #wcc_collect_digits{}.

-spec collect_digits(integer() | kz_term:ne_binary(), kapps_call:call()) -> collect_digits_return().
collect_digits(MaxDigits, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=kz_term:to_integer(MaxDigits)
                                         ,call=Call
                                         ,after_timeout=?DEFAULT_DIGIT_TIMEOUT
                                         }).

-spec collect_digits(integer() | kz_term:ne_binary(), integer() | kz_term:ne_binary(), kapps_call:call()) -> collect_digits_return().
collect_digits(MaxDigits, Timeout, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=kz_term:to_integer(MaxDigits)
                                         ,timeout=kz_term:to_integer(Timeout)
                                         ,call=Call
                                         ,after_timeout=kz_term:to_integer(Timeout)
                                         }).

-spec collect_digits(integer() | kz_term:ne_binary(), integer() | kz_term:ne_binary(), integer() | kz_term:ne_binary(), kapps_call:call()) ->
                            collect_digits_return().
collect_digits(MaxDigits, Timeout, Interdigit, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=kz_term:to_integer(MaxDigits)
                                         ,timeout=kz_term:to_integer(Timeout)
                                         ,interdigit=kz_term:to_integer(Interdigit)
                                         ,call=Call
                                         ,after_timeout=kz_term:to_integer(Timeout)
                                         }).

-spec collect_digits(integer() | kz_term:ne_binary(), integer() | kz_term:ne_binary(), integer() | kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) ->
                            collect_digits_return().
collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=kz_term:to_integer(MaxDigits)
                                         ,timeout=kz_term:to_integer(Timeout)
                                         ,interdigit=kz_term:to_integer(Interdigit)
                                         ,noop_id=NoopId
                                         ,call=Call
                                         }).

-spec collect_digits(integer(), integer(), integer(), kz_term:api_binary(), list(), kapps_call:call()) ->
                            collect_digits_return().
collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=kz_term:to_integer(MaxDigits)
                                         ,timeout=kz_term:to_integer(Timeout)
                                         ,interdigit=kz_term:to_integer(Interdigit)
                                         ,noop_id=NoopId
                                         ,terminators=Terminators
                                         ,call=Call
                                         ,after_timeout=kz_term:to_integer(Timeout)
                                         }).

-spec collect_digits(integer(), integer(), integer(), kz_term:api_binary(), list(), boolean(), kapps_call:call()) ->
                            collect_digits_return().
collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, FlushOnDigit, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=kz_term:to_integer(MaxDigits)
                                         ,timeout=kz_term:to_integer(Timeout)
                                         ,interdigit=kz_term:to_integer(Interdigit)
                                         ,noop_id=NoopId
                                         ,terminators=Terminators
                                         ,call=Call
                                         ,flush_on_digit=FlushOnDigit
                                         }).

-spec do_collect_digits(wcc_collect_digits()) -> collect_digits_return().
do_collect_digits(#wcc_collect_digits{max_digits=MaxDigits
                                     ,timeout=Timeout
                                     ,interdigit=Interdigit
                                     ,noop_id=NoopId
                                     ,call=Call
                                     ,terminators=Terminators
                                     ,digits_collected=Digits
                                     ,after_timeout=After
                                     ,flush_on_digit=FlushOnDigit
                                     }=Collect) ->
    Start = os:timestamp(),
    case receive_event(After) of
        {'ok', JObj} ->
            case handle_collect_digit_event(JObj, NoopId) of
                {'error', _}=Error -> Error;
                {'noop_complete'} ->
                    %% if we were given the NoopId of the noop and this is it, then start the timer
                    %% unless we have already started collecting digits when the noop came in
                    T = case Digits of <<>> -> Timeout; _ -> After end,
                    do_collect_digits(Collect#wcc_collect_digits{after_timeout=T});
                {'continue'} ->
                    %% if we were given the NoopId of the noop and this is not it, then keep waiting
                    do_collect_digits(Collect);
                {'decrement'} ->
                    do_collect_digits(Collect#wcc_collect_digits{after_timeout=kz_time:decr_timeout(After, Start)});
                {'ok', Digit} ->
                    %% DTMF received, collect and start interdigit timeout
                    FlushOnDigit
                        andalso Digits =:= <<>>
                            andalso flush(Call),

                    case lists:member(Digit, Terminators) of
                        'true' ->
                            lager:debug("collected digits ('~s') from caller, terminated with ~s", [Digits, Digit]),
                            {'ok', Digits};
                        'false' ->
                            case <<Digits/binary, Digit/binary>> of
                                D when byte_size(D) < MaxDigits ->
                                    do_collect_digits(Collect#wcc_collect_digits{digits_collected=D
                                                                                ,after_timeout=Interdigit
                                                                                });
                                D ->
                                    lager:debug("collected maximum digits ('~s') from caller", [D]),
                                    {'ok', D}
                            end
                    end
            end;
        {'error', 'timeout'} -> {'ok', Digits}
    end.

-spec handle_collect_digit_event(kz_json:object(), kz_term:api_binary()) ->
                                        {'dtmf', kz_term:ne_binary()} |
                                        {'noop_complete'} |
                                        {'continue'} |
                                        {'decrement'} |
                                        {'error', any()}.
handle_collect_digit_event(JObj, NoopId) ->
    handle_collect_digit_event(JObj, NoopId, get_event_type(JObj)).

-spec handle_collect_digit_event(kz_json:object(), kz_term:api_binary(), {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}) ->
                                        {'dtmf', kz_term:ne_binary()} |
                                        {'noop_complete'} |
                                        {'continue'} |
                                        {'decrement'} |
                                        {'error', any()}.
handle_collect_digit_event(_JObj, _NoopId, {<<"call_event">>, <<"CHANNEL_DESTROY">>, _}) ->
    lager:debug("channel was hungup while collecting digits"),
    {'error', 'channel_hungup'};
handle_collect_digit_event(JObj, NoopId, {<<"error">>, _, <<"noop">>}) ->
    case kz_json:get_value([<<"Request">>, <<"Msg-ID">>], JObj, NoopId) of
        NoopId when is_binary(NoopId), NoopId =/= <<>> ->
            lager:debug("channel execution error while collecting digits: ~s"
                       ,[kz_json:get_value(<<"Error-Message">>, JObj)]
                       ),
            {'error', JObj};
        _NID when is_binary(NoopId), NoopId =/= <<>> ->
            {'continue'};
        _NID ->
            lager:debug("channel execution error while collecting digits with noop-id ~s: ~s"
                       ,[NoopId, kz_json:encode(JObj)]
                       ),
            {'error', JObj}
    end;
handle_collect_digit_event(JObj, NoopId, {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>}) ->
    %% Playback completed start timeout
    case kz_json:get_value(<<"Application-Response">>, JObj) of
        NoopId when is_binary(NoopId), NoopId =/= <<>> ->
            {'noop_complete'};
        _NID when is_binary(NoopId), NoopId =/= <<>> ->
            {'continue'};
        _ ->
            {'noop_complete'}
    end;
handle_collect_digit_event(JObj, _NoopId, {<<"call_event">>, <<"DTMF">>, _}) ->
    {'ok', kz_json:get_value(<<"DTMF-Digit">>, JObj, <<>>)};
handle_collect_digit_event(_JObj, _NoopId, _EventType) ->
    {'decrement'}.

%%------------------------------------------------------------------------------
%% @doc Low level function to consume call events, looping until a specific
%% one occurs.  If the channel is hungup or no call events are received
%% for the optional timeout period then errors are returned.
%% @end
%%------------------------------------------------------------------------------

-spec wait_for_message(kapps_call:call(), binary()) ->
                              kapps_api_std_return().
wait_for_message(Call, Application) ->
    wait_for_message(Call, Application, <<"CHANNEL_EXECUTE_COMPLETE">>).

-spec wait_for_message(kapps_call:call(), binary(), kz_term:ne_binary()) ->
                              kapps_api_std_return().
wait_for_message(Call, Application, Event) ->
    wait_for_message(Call, Application, Event, <<"call_event">>).

-spec wait_for_message(kapps_call:call(), binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              kapps_api_std_return().
wait_for_message(Call, Application, Event, Type) ->
    wait_for_message(Call, Application, Event, Type, ?DEFAULT_MESSAGE_TIMEOUT).

-spec wait_for_message(kapps_call:call(), binary(), kz_term:ne_binary(), kz_term:ne_binary(), timeout()) ->
                              kapps_api_std_return().
wait_for_message(Call, Application, Event, Type, Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj}=Ok ->
            case get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("channel was destroyed while waiting for ~s", [Application]),
                    {'error', 'channel_hungup'};
                {<<"error">>, _, Application} ->
                    lager:debug("channel execution error while waiting for ~s: ~s", [Application, kz_json:encode(JObj)]),
                    {'error', JObj};
                {Type, Event, Application} ->
                    Ok;
                _ ->
                    wait_for_message(Call, Application, Event, Type, kz_time:decr_timeout(Timeout, Start))
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Wait for an application to complete, ignoring channel state.  This
%% is only interested in events for the application.
%% @end
%%------------------------------------------------------------------------------
-type wait_for_application_return() :: {'error', 'timeout' | kz_json:object()} |
                                       {'ok', kz_json:object()}.

-spec wait_for_application(kapps_call:call(), kz_term:ne_binary()) ->
                                  wait_for_application_return().
wait_for_application(Call, Application) ->
    wait_for_application(Call, Application, <<"CHANNEL_EXECUTE_COMPLETE">>).

-spec wait_for_application(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                  wait_for_application_return().
wait_for_application(Call, Application, Event) ->
    wait_for_application(Call, Application, Event, <<"call_event">>).

-spec wait_for_application(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                  wait_for_application_return().
wait_for_application(Call, Application, Event, Type) ->
    wait_for_application(Call, Application, Event, Type, ?DEFAULT_APPLICATION_TIMEOUT).

-spec wait_for_application(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), timeout()) ->
                                  wait_for_application_return().
wait_for_application(Call, Application, Event, Type, Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj}=Ok ->
            case get_event_type(JObj) of
                {<<"error">>, _, Application} ->
                    lager:debug("channel execution error while waiting for ~s: ~s", [Application, kz_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("channel was hungup while waiting for ~s", [Application]),
                    {'error', 'channel_hungup'};
                {Type, Event, Application} ->
                    Ok;
                _ ->
                    wait_for_application(Call, Application, Event, Type, kz_time:decr_timeout(Timeout, Start))
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Wait for an application to complete, ignoring channel state.  This
%% is only interested in events for the application.
%% @end
%%------------------------------------------------------------------------------
-type headless_event() :: kz_term:ne_binary() | %% STOP EVENT
                          {kz_term:ne_binary(), kz_term:ne_binary()}. %% {START, STOP}

-type wait_for_headless_application_return() :: {'error', 'timeout' | kz_json:object()} |
                                                {'ok', kz_json:object()}.

-spec wait_for_headless_application(kz_term:ne_binary()) ->
                                           wait_for_headless_application_return().
wait_for_headless_application(Application) ->
    wait_for_headless_application(Application, <<"CHANNEL_EXECUTE_COMPLETE">>).

-spec wait_for_headless_application(kz_term:ne_binary(), headless_event()) ->
                                           wait_for_headless_application_return().
wait_for_headless_application(Application, Event) ->
    wait_for_headless_application(Application, Event, <<"call_event">>).

-spec wait_for_headless_application(kz_term:ne_binary(), headless_event(), kz_term:ne_binary()) ->
                                           wait_for_headless_application_return().
wait_for_headless_application(Application, Event, Type) ->
    wait_for_headless_application(Application, Event, Type, ?DEFAULT_APPLICATION_TIMEOUT).

-spec wait_for_headless_application(kz_term:ne_binary()
                                   ,headless_event()
                                   ,kz_term:ne_binary()
                                   ,timeout()
                                   ) ->
                                           wait_for_headless_application_return().
wait_for_headless_application(Application, Event, Type, Timeout) ->
    wait_for_headless_application(Application, Event, Type, fun(_) -> 'true' end, Timeout).

-spec wait_for_headless_application(kz_term:ne_binary()
                                   ,headless_event()
                                   ,kz_term:ne_binary()
                                   ,fun((kz_json:object()) -> boolean())
                                   ,timeout()
                                   ) ->
                                           wait_for_headless_application_return().
wait_for_headless_application(Application, {StartEv, StopEv}=Event, Type, Fun, Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'ok', JObj}=Ok ->
            case get_event_type(JObj) of
                {<<"error">>, _, Application} ->
                    lager:debug("channel execution error while waiting for ~s: ~s"
                               ,[Application, kz_json:encode(JObj)]
                               ),
                    {'error', JObj};
                {<<"call_event">>,<<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("channel has gone down before app ~s started", [Application]),
                    {'error', 'channel_hungup'};
                {Type, StartEv, Application} ->
                    lager:debug("start event ~s has been received for ~s", [StartEv, Application]),
                    wait_for_headless_application(Application, StopEv, Type, Fun, kz_time:decr_timeout(Timeout, Start));
                {Type, StopEv, Application} ->
                    case Fun(JObj) of
                        'true' -> Ok;
                        'false' -> wait_for_headless_application(Application, Event, Type, Fun, kz_time:decr_timeout(Timeout, Start))
                    end;
                _T ->
                    lager:debug("headless application ~s ignoring ~p", [Application, _T]),
                    wait_for_headless_application(Application, Event, Type, Fun, kz_time:decr_timeout(Timeout, Start))
            end;
        {'error', _E}=E -> E
    end;
wait_for_headless_application(Application, Event, Type, Fun, Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'ok', JObj}=Ok ->
            case get_event_type(JObj) of
                {<<"error">>, _, Application} ->
                    lager:debug("channel execution error while waiting for ~s: ~s", [Application, kz_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>,<<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("destroy occurred, waiting 60000 ms for ~s event", [Application]),
                    wait_for_headless_application(Application, Event, Type, Fun, 60 * ?MILLISECONDS_IN_SECOND);
                {Type, Event, Application} ->
                    case Fun(JObj) of
                        'true' -> Ok;
                        'false' -> wait_for_headless_application(Application, Event, Type, Fun, kz_time:decr_timeout(Timeout, Start))
                    end;
                _T ->
                    lager:debug("ignore ~p", [_T]),
                    wait_for_headless_application(Application, Event, Type, Fun, kz_time:decr_timeout(Timeout, Start))
            end;
        {'error', _E}=E -> E
    end.

%%------------------------------------------------------------------------------
%% @doc Wait for a DTMF event and extract the digits when it comes.
%% @end
%%------------------------------------------------------------------------------
-spec wait_for_dtmf(timeout()) ->
                           {'error', 'channel_hungup' | 'timeout' | kz_json:object()} |
                           {'ok', binary()}.
wait_for_dtmf(Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'ok', JObj} ->
            case kz_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                    lager:debug("channel was destroyed while waiting for DTMF"),
                    {'error', 'channel_hungup'};
                {<<"error">>, _} ->
                    lager:debug("channel execution error while waiting for DTMF: ~s", [kz_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>, <<"DTMF">>} ->
                    {'ok', kz_json:get_value(<<"DTMF-Digit">>, JObj)};
                _ ->
                    wait_for_dtmf(kz_time:decr_timeout(Timeout, Start))
            end;
        {'error', 'timeout'}=E ->
            lager:debug("timed out after ~p ms waiting for DTMF", [Timeout]),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc Waits for and determines the status of the bridge command.
%% @end
%%------------------------------------------------------------------------------

-spec wait_for_bridge(timeout(), kapps_call:call()) ->
                             kapps_api_bridge_return().
wait_for_bridge(0, Call) ->
    wait_for_bridge(?BRIDGE_DEFAULT_TIMEOUT, 'undefined', Call);
wait_for_bridge(Timeout, Call) ->
    wait_for_bridge(Timeout, 'undefined', Call).

-spec wait_for_bridge(timeout(), 'undefined' | fun((kz_json:object()) -> any()), kapps_call:call()) ->
                             kapps_api_bridge_return().
wait_for_bridge(Timeout, _, _) when Timeout < 0 ->
    {'error', 'timeout'};
wait_for_bridge(Timeout, Fun, Call) ->
    Start = os:timestamp(),
    lager:debug("waiting for bridge for ~p ms", [Timeout]),
    wait_for_bridge(Timeout, Fun, Call, Start, receive_event(Timeout)).

wait_for_bridge(_Timeout, _Fun, _Call, _Start, {'error', 'timeout'}=E) -> E;
wait_for_bridge(Timeout, Fun, Call, Start, {'ok', JObj}) ->
    Disposition = kz_json:get_value(<<"Disposition">>, JObj),
    Cause = kz_json:get_first_defined([<<"Application-Response">>
                                      ,<<"Hangup-Cause">>
                                      ], JObj, <<"UNSPECIFIED">>),
    Result = case Disposition =:= <<"SUCCESS">>
                 orelse Cause =:= <<"SUCCESS">>
             of
                 'true' -> 'ok';
                 'false' -> 'fail'
             end,
    case get_event_type(JObj) of
        {<<"error">>, _, <<"bridge">>} ->
            lager:debug("channel execution error while waiting for bridge: ~s", [kz_json:encode(JObj)]),
            {'error', JObj};
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>, _} ->
            CallId = kz_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
            lager:debug("channel bridged to ~s", [CallId]),
            case is_function(Fun, 1) of
                'false' -> 'ok';
                'true' -> Fun(JObj)
            end,
            wait_for_bridge('infinity', Fun, Call);
        {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
            %% TODO: reduce log level if no issue is found with
            %%    basing the Result on Disposition
            lager:info("bridge channel destroy completed with result ~s(~s)", [Disposition, Result]),
            {Result, JObj};
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>} ->
            %% TODO: reduce log level if no issue is found with
            %%    basing the Result on Disposition
            lager:info("bridge channel execute completed with result ~s(~s)", [Disposition, Result]),
            {Result, JObj};
        _E ->
            NewTimeout = kz_time:decr_timeout(Timeout, Start),
            NewStart = os:timestamp(),
            wait_for_bridge(NewTimeout, Fun, Call, NewStart, receive_event(NewTimeout))
    end.

%%------------------------------------------------------------------------------
%% @doc Wait for a `noop' or a specific `noop' to occur.
%% @end
%%------------------------------------------------------------------------------
-spec wait_for_noop(kapps_call:call(), kz_term:api_binary()) -> kapps_api_std_return().
wait_for_noop(Call, NoopId) ->
    case wait_for_message(Call, <<"noop">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, 'infinity') of
        {'ok', JObj}=OK ->
            case kz_json:get_value(<<"Application-Response">>, JObj) of
                NoopId when is_binary(NoopId), NoopId =/= <<>> -> OK;
                _No when is_binary(NoopId), NoopId =/= <<>> -> wait_for_noop(Call, NoopId);
                _ -> OK
            end;
        Else -> Else
    end.

%%------------------------------------------------------------------------------
%% @doc Wait for a channel to be unbridged from (or destroyed).
%% @end
%%------------------------------------------------------------------------------
-spec wait_for_channel_unbridge() -> {'ok', kz_json:object()}.
wait_for_channel_unbridge() ->
    receive
        {'amqp_msg', JObj} ->
            case kz_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_UNBRIDGE">>} -> {'ok', JObj};
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> {'ok', JObj};
                _ -> wait_for_channel_unbridge()
            end;
        _ -> wait_for_channel_unbridge()
    end.

%%------------------------------------------------------------------------------
%% @doc Wait for a channel to be bridged to (or destroyed).
%% @end
%%------------------------------------------------------------------------------
-spec wait_for_channel_bridge() -> {'ok', kz_json:object()}.
wait_for_channel_bridge() ->
    case receive_event('infinity') of
        {'ok', JObj}=Ok ->
            case kz_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>} -> Ok;
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> Ok;
                _ -> wait_for_channel_bridge()
            end;
        _ -> wait_for_channel_bridge()
    end.

%%------------------------------------------------------------------------------
%% @doc Wait forever for the channel to hangup.
%% @end
%%------------------------------------------------------------------------------

-spec wait_for_hangup() -> {'ok', 'channel_hungup'} |
                           {'error', 'timeout'}.
wait_for_hangup() ->
    wait_for_hangup('infinity').

-spec wait_for_hangup(timeout()) ->
                             {'ok', 'channel_hungup'} |
                             {'error', 'timeout'}.
wait_for_hangup(Timeout) ->
    Start = os:timestamp(),
    receive
        {'amqp_msg', JObj} ->
            case kz_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                    {'ok', 'channel_hungup'};
                _Evt ->
                    lager:debug("ignoring: ~p", [_Evt]),
                    wait_for_hangup(kz_time:decr_timeout(Timeout, Start))
            end;
        _ -> wait_for_hangup(kz_time:decr_timeout(Timeout, Start))
    after
        Timeout ->
            {'error', 'timeout'}
    end.

%%------------------------------------------------------------------------------
%% @doc Wait forever for the channel to hangup.
%% @end
%%------------------------------------------------------------------------------

-spec wait_for_unbridge() ->   {'ok', 'leg_hungup'} |
                               {'error', 'timeout'}.
wait_for_unbridge() ->
    wait_for_unbridge('infinity').

-spec wait_for_unbridge(timeout()) ->
                               {'ok', 'leg_hungup'} |
                               {'error', 'timeout'}.
wait_for_unbridge(Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj} ->
            case kz_util:get_event_type(JObj) of
                {<<"call_event">>, <<"LEG_DESTROYED">>} -> {'ok', 'leg_hungup'};
                _ -> wait_for_unbridge(kz_time:decr_timeout(Timeout, Start))
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Waits for and determines the status of the bridge command.
%% @end
%%------------------------------------------------------------------------------
-spec wait_for_application_or_dtmf(kz_term:ne_binary(), timeout()) ->
                                          kapps_api_std_return() |
                                          {'dtmf', binary()}.
wait_for_application_or_dtmf(Application, Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj}=Ok ->
            case get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("channel was destroyed while waiting for ~s or DTMF", [Application]),
                    {'error', 'channel_hungup'};
                {<<"error">>, _, Application} ->
                    lager:debug("channel execution error while waiting ~s or DTMF: ~s", [Application, kz_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, Application} -> Ok;
                {<<"call_event">>, <<"DTMF">>, _} -> {'dtmf', kz_json:get_value(<<"DTMF-Digit">>, JObj)};
                _ ->
                    wait_for_application_or_dtmf(Application, kz_time:decr_timeout(Timeout, Start))
            end
    end.

-type wait_for_fax_ret() :: {'ok', kz_json:object()} |
                            {'error', 'timeout' | kz_json:object()}.

-define(WAIT_FOR_FAX_TIMEOUT, kapps_config:get_integer(<<"fax">>, <<"wait_for_fax_timeout_ms">>, ?MILLISECONDS_IN_HOUR)).

-spec wait_for_fax() -> wait_for_fax_ret().
wait_for_fax() -> wait_for_fax(?WAIT_FOR_FAX_TIMEOUT).

-spec wait_for_fax(timeout()) -> wait_for_fax_ret().
wait_for_fax(Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj} ->
            case get_event_type(JObj) of
                {<<"error">>, _, <<"receive_fax">>} ->
                    lager:debug("channel execution error while waiting for fax: ~s", [kz_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>, <<"CHANNEL_EXECUTE">>, <<"receive_fax">>} ->
                    wait_for_fax('infinity');
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"receive_fax">>} ->
                    {'ok', kz_json:set_value(<<"Fax-Success">>, 'true', JObj)};
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    %% NOTE:
                    lager:debug("channel hungup but no end of fax, maybe its coming next..."),
                    wait_for_fax(5 * ?MILLISECONDS_IN_SECOND);
                _ -> wait_for_fax(kz_time:decr_timeout(Timeout, Start))
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Retrieves event category, type, application from AMQP payload.
%% @end
%%------------------------------------------------------------------------------
-spec get_event_type(kz_json:object()) -> {kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()}.
get_event_type(JObj) ->
    {C, N} = kz_util:get_event_type(JObj),
    {C, N, get_app(C, JObj)}.

-spec get_app(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
get_app(<<"error">>, JObj) ->
    kz_json:get_value([<<"Request">>, <<"Application-Name">>], JObj);
get_app(_EvtCategory, JObj) ->
    kz_json:get_value(<<"Application-Name">>, JObj).

%%------------------------------------------------------------------------------
%% @doc Sends call commands to the appropriate call control process.
%% @end
%%------------------------------------------------------------------------------
-spec send_command(kz_term:api_terms(), kapps_call:call()) -> 'ok'.
send_command(Command, Call) when is_list(Command) ->
    'true' = kapps_call:is_call(Call),

    CustomPublisher = kapps_call:custom_publish_function(Call),
    CtrlQ = kapps_call:control_queue(Call),
    case is_function(CustomPublisher, 2) of
        'true' -> CustomPublisher(Command, Call);
        'false' when is_binary(CtrlQ) ->
            Q = kapps_call:controller_queue(Call),
            CallId = kapps_call:call_id(Call),
            AppName = kapps_call:application_name(Call),
            AppVersion = kapps_call:application_version(Call),
            case kapps_call:kvs_fetch('consumer_pid', Call) of
                Pid when is_pid(Pid) -> _ = kz_amqp_channel:consumer_pid(Pid), 'ok';
                _Else -> 'ok'
            end,
            Prop =
                props:insert_value(<<"Call-ID">>, CallId, Command) ++
                kz_api:default_headers(Q, <<"call">>, <<"command">>, AppName, AppVersion),
            kapi_dialplan:publish_command(CtrlQ, props:filter_undefined(Prop));
        'false' -> 'ok'
    end;
send_command(JObj, Call) -> send_command(kz_json:to_proplist(JObj), Call).

%%------------------------------------------------------------------------------
%% @doc Get the T38 settings for an endpoint based on carrier and device.
%% @end
%%------------------------------------------------------------------------------
-spec get_outbound_t38_settings(boolean(), kz_term:api_binary() | boolean()) ->
                                       kz_term:proplist().
get_outbound_t38_settings(CarrierFlag, <<"auto">>) ->
    get_outbound_t38_settings(CarrierFlag, 'true');
get_outbound_t38_settings(CarrierFlag, 'undefined') ->
    get_outbound_t38_settings(CarrierFlag);
get_outbound_t38_settings(CarrierFlag, CallerFlag) when not is_boolean(CallerFlag) ->
    get_outbound_t38_settings(CarrierFlag, kz_term:is_true(CallerFlag));
get_outbound_t38_settings('true', 'true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
    ,{<<"Enable-T38-Passthrough">>, 'true'}
    ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_outbound_t38_settings('true', 'false') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'true'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, <<"self">>}
    ];
get_outbound_t38_settings('false', 'false') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
    ,{<<"Enable-T38-Passthrough">>, 'true'}
    ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_outbound_t38_settings('false','true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'true'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, <<"peer">>}
    ].

-spec get_outbound_t38_settings(boolean()) -> kz_term:proplist().
get_outbound_t38_settings('true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'true'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_outbound_t38_settings('false') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
    ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_inbound_t38_settings(boolean(), kz_term:api_binary() | boolean()) ->
                                      kz_term:proplist().
get_inbound_t38_settings(CarrierFlag, <<"auto">>) ->
    get_inbound_t38_settings(CarrierFlag, 'true');
get_inbound_t38_settings(CarrierFlag, 'undefined') ->
    get_inbound_t38_settings(CarrierFlag);
get_inbound_t38_settings(CarrierFlag, CallerFlag) when not is_boolean(CallerFlag) ->
    get_inbound_t38_settings(CarrierFlag, kz_term:is_true(CallerFlag));
get_inbound_t38_settings('true', 'true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
    ,{<<"Enable-T38-Passthrough">>, 'true'}
    ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_inbound_t38_settings('true', 'false') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'true'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, <<"peer">>}
    ];
get_inbound_t38_settings('true', 'undefined') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'true'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, <<"peer">>}
    ];
get_inbound_t38_settings('false', 'false') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
    ,{<<"Enable-T38-Passthrough">>, 'true'}
    ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_inbound_t38_settings('false','true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'true'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, <<"self">>}
    ];
get_inbound_t38_settings('undefined','true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'true'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, <<"self">>}
    ];
get_inbound_t38_settings('undefined','false') ->
    [{<<"Enable-T38-Fax">>, 'false'}
    ,{<<"Enable-T38-Fax-Request">>, 'false'}
    ,{<<"Enable-T38-Passthrough">>, 'false'}
    ];
get_inbound_t38_settings('undefined','undefined') ->
    [{<<"Enable-T38-Fax">>, 'false'}
    ,{<<"Enable-T38-Fax-Request">>, 'false'}
    ,{<<"Enable-T38-Passthrough">>, 'false'}
    ];
get_inbound_t38_settings(_Carrier, _CallerFlag) ->
    [{<<"Enable-T38-Fax">>, 'false'}
    ,{<<"Enable-T38-Fax-Request">>, 'false'}
    ,{<<"Enable-T38-Passthrough">>, 'false'}
    ].

-spec get_inbound_t38_settings(boolean()) -> kz_term:proplist().
get_inbound_t38_settings('true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
    ,{<<"Enable-T38-Fax-Request">>, 'true'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_inbound_t38_settings('false') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
    ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_inbound_t38_settings('undefined') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
    ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
    ,{<<"Enable-T38-Passthrough">>, 'undefined'}
    ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ].

-spec start_fax_detection(kz_term:ne_binary(), integer(), kapps_call:call()) -> 'ok'.
start_fax_detection(Direction, Duration, Call) ->
    CallId = kapps_call:call_id(Call),
    Command = [{<<"Application-Name">>, <<"fax_detection">>}
              ,{<<"Action">>, <<"start">>}
              ,{<<"Duration">>, Duration}
              ,{<<"Direction">>, Direction}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

-spec stop_fax_detection(kapps_call:call()) -> 'ok'.
stop_fax_detection(Call) ->
    CallId = kapps_call:call_id(Call),
    Command = [{<<"Application-Name">>, <<"fax_detection">>}
              ,{<<"Action">>, <<"stop">>}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

-spec fax_detection(kz_term:ne_binary(), integer(), kapps_call:call()) -> 'true' | 'false'.
fax_detection(Direction, Duration, Call) ->
    start_fax_detection(Direction, Duration, Call),
    Result = case wait_for_fax_detection((Duration + 2) * ?MILLISECONDS_IN_SECOND, Call) of
                 {'error', 'timeout'} -> 'false';
                 {'ok', _JObj} -> 'true'
             end,
    stop_fax_detection(Call),
    Result.

-spec wait_for_fax_detection(integer(), kapps_call:call()) ->
                                    {'error', 'timeout'} |
                                    {'ok', kz_json:object()}.
wait_for_fax_detection(Timeout, Call) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj} ->
            case get_event_type(JObj) of
                {<<"call_event">>, <<"FAX_DETECTED">>, _ } ->
                    {'ok', kz_json:set_value(<<"Fax-Success">>, 'true', JObj)};
                _ -> wait_for_fax_detection(kz_time:decr_timeout(Timeout, Start), Call)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Low level function to consume call events, looping until a specific
%% one occurs.  If the channel is hungup or no call events are received
%% for the optional timeout period then errors are returned.
%% @end
%%------------------------------------------------------------------------------
-spec wait_for_unparked_call(kapps_call:call()) ->
                                    kapps_api_std_return().
wait_for_unparked_call(Call) ->
    wait_for_unparked_call(Call, ?DEFAULT_MESSAGE_TIMEOUT).

-spec wait_for_unparked_call(kapps_call:call(), timeout()) ->
                                    kapps_api_std_return().
wait_for_unparked_call(Call, Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj}=Ok ->
            case get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("channel was destroyed while waiting for unparked call"),
                    {'error', 'channel_hungup'};
                {<<"call_event">>, <<"CHANNEL_DISCONNECTED">>, _} ->
                    lager:debug("channel was disconnected while waiting for unparked call"),
                    {'error', 'channel_disconnected'};
                {<<"call_event">>, <<"CHANNEL_INTERCEPTED">>, _} ->
                    lager:debug("channel was intercepted while waiting for unparked call"),
                    Ok;
                {<<"error">>, _, <<"hold">>} ->
                    lager:debug("channel execution error while waiting for unparked call: ~s", [kz_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"hold">>} ->
                    Ok;
                _ ->
                    wait_for_unparked_call(Call, kz_time:decr_timeout(Timeout, Start))
            end
    end.

-spec store_file_args(kz_term:ne_binary(), store_fun()) -> kz_term:proplist().
store_file_args(Filename, URLFun) ->
    [{<<"File-Name">>, Filename}
    ,{<<"Url">>, maybe_call_store_fun(URLFun)}
    ,{<<"Http-Method">>, <<"put">>}
    ].

-spec maybe_call_store_fun(store_fun()) -> kz_term:ne_binary().
maybe_call_store_fun(URLFun) when is_function(URLFun, 0) ->
    URLFun();
maybe_call_store_fun(URL) -> URL.

-spec store_file(kz_term:ne_binary(), store_fun(), kapps_call:call()) -> 'ok' | {'error', any()}.
store_file(Filename, URLFun, Call) ->
    App = kz_util:calling_app(),
    store_file(Filename, URLFun, storage_retries(App), storage_timeout(App), Call).

-spec store_file(kz_term:ne_binary(), store_fun(), pos_integer(), kapps_call:call()) ->
                        'ok' | {'error', any()}.
store_file(Filename, URLFun, Tries, Call) ->
    App = kz_util:calling_app(),
    store_file(Filename, URLFun, Tries, storage_timeout(App), Call).

-spec store_file(kz_term:ne_binary(), store_fun(), pos_integer(), timeout(), kapps_call:call()) ->
                        'ok' | {'error', any()}.
store_file(Filename, URLFun, Tries, Timeout, Call) ->
    Msg = case kapps_call:kvs_fetch('alert_msg', Call) of
              'undefined' ->
                  io_lib:format("Error Storing File ~s From Media Server ~s"
                               ,[Filename, kapps_call:switch_nodename(Call)]
                               );
              ErrorMsg -> ErrorMsg
          end,
    {AppName, AppVersion} = kz_util:calling_app_version(),
    API = fun() -> [{<<"Command">>, <<"send_http">>}
                   ,{<<"Args">>, kz_json:from_list(store_file_args(Filename, URLFun))}
                   ,{<<"FreeSWITCH-Node">>, kapps_call:switch_nodename(Call)}
                    | kz_api:default_headers(AppName, AppVersion)
                   ]
          end,
    do_store_file(Tries, Timeout, API, Msg, Call).

-spec do_store_file(pos_integer(), timeout(), function()
                   ,kz_term:ne_binary(), kapps_call:call()) ->
                           'ok' | {'error', any()}.
do_store_file(Tries, Timeout, API, Msg, Call) ->
    Payload = API(),
    case kz_amqp_worker:call(Payload, fun kapi_switch:publish_command/1, fun kapi_switch:fs_reply_v/1, Timeout) of
        {'ok', JObj} ->
            case kz_json:get_ne_binary_value(<<"Result">>, JObj) of
                <<"success">> -> 'ok';
                <<"error">> ->
                    Error = kz_json:get_first_defined([[<<"Event-Data">>, <<"API-Error">>], <<"Error">>], JObj, <<"error not available">>),
                    retry_store_file(Tries - 1, Timeout, API, Msg, Error, maybe_add_debug_data(JObj, Call));
                _Other ->
                    Error = kz_term:to_binary(io_lib:format("unhandled return ('~s') from store file", [_Other])),
                    retry_store_file(Tries - 1, Timeout, API, Msg, Error, maybe_add_debug_data(JObj, Call))
            end;
        {'returned', _JObj, _Basic} ->
            Error = io_lib:format("message returned from amqp. is ~s down ?"
                                 ,[kapps_call:switch_nodename(Call)]
                                 ),
            Funs = [{fun kapps_call:kvs_store/3, 'basic_return', kz_json:to_proplist(_Basic)}],
            retry_store_file(Tries - 1, Timeout, API, Msg, kz_term:to_binary(Error), kapps_call:exec(Funs, Call));
        {'timeout', _JObj} ->
            Error = io_lib:format("timeout publishing message to amqp. is ~s down ?"
                                 ,[kapps_call:switch_nodename(Call)]
                                 ),
            retry_store_file(Tries - 1, Timeout, API, Msg, kz_term:to_binary(Error), Call);
        {'error', Err} ->
            Error = io_lib:format("error publishing message to amqp. is ~s down ? : ~p"
                                 ,[kapps_call:switch_nodename(Call), Err]
                                 ),
            retry_store_file(Tries - 1, Timeout, API, Msg, kz_term:to_binary(Error), Call)
    end.

-spec retry_store_file(integer(), timeout(), kz_term:proplist() | function()
                      ,kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) ->
                              'ok' | {'error', any()}.
retry_store_file(0, _Timeout, _API, Msg, Error, Call) ->
    lager:critical("~s : ~s", [Msg, Error]),
    Funs = [{fun kapps_call:kvs_store/3, 'store_error', Error}
           ,{fun kapps_call:kvs_store/3, 'media_server', kapps_call:switch_nodename(Call)}
           ],
    kz_notify:detailed_alert(kz_term:to_binary(Msg)
                            ,kz_term:to_binary(Error)
                            ,kapps_call:to_proplist(kapps_call:exec(Funs, Call))
                            ,[]
                            ),
    {'error', Error};
retry_store_file(Tries, Timeout, API, Msg, Error, Call) ->
    lager:critical("~s : ~s", [Msg, Error]),
    timer:sleep(5 * ?MILLISECONDS_IN_SECOND),
    do_store_file(Tries, Timeout, API, Msg, Call).

maybe_add_debug_data(JObj, Call) ->
    case kz_json:get_value(<<"Event-Data">>, JObj) of
        'undefined' -> Call;
        Data -> kapps_call:kvs_store('error_details', Data, Call)
    end.

-spec attended_transfer(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
attended_transfer(To, Call) ->
    attended_transfer(To, 'undefined', Call).

-spec attended_transfer(kz_term:ne_binary(), kz_term:api_ne_binary(), kapps_call:call()) -> 'ok'.
attended_transfer(To, TransferLeg, Call) ->
    Command = transfer_command(<<"attended">>, To, TransferLeg, Call),
    send_command(Command, Call).

-spec blind_transfer(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
blind_transfer(To, Call) ->
    blind_transfer(To, 'undefined', Call).

-spec blind_transfer(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
blind_transfer(To, TransferLeg, Call) ->
    Command = transfer_command(<<"blind">>, To, TransferLeg, Call),
    send_command(Command, Call).

-spec transfer(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
transfer(TransferType, To, Call) ->
    transfer(TransferType, To, 'undefined', Call).

-spec transfer(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
transfer(TransferType, To, TransferLeg, Call) ->
    Command = transfer_command(TransferType, To, TransferLeg, Call),
    send_command(Command, Call).

-spec transfer_command(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_terms().
transfer_command(TransferType, TransferTo, Call) ->
    transfer_command(TransferType, TransferTo, 'undefined', Call).

-spec transfer_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kapps_call:call()) -> kz_term:api_terms().
transfer_command(TransferType, TransferTo, TransferLeg, Call) ->
    kz_json:from_list([{<<"Application-Name">>, <<"transfer">>}
                      ,{<<"Transfer-Type">>, TransferType}
                      ,{<<"Transfer-To">>, TransferTo}
                      ,{<<"Transfer-Leg">>, TransferLeg}
                      ,{<<"Caller-ID-Number">>, kapps_call:callee_id_number(Call)}
                      ,{<<"Caller-ID-Name">>, kapps_call:callee_id_name(Call)}
                      ,{<<"Insert-At">>, <<"now">>}
                      ,{<<"Call-ID">>, kapps_call:call_id(Call)}
                      ,{<<"Custom-Channel-Vars">>, kapps_call:custom_channel_vars(Call)}
                      ]
                     ).

-spec play_macro_command(kz_term:ne_binaries(), kapps_call:call()) -> kz_term:api_terms().
play_macro_command(Media, Call) ->
    kz_json:from_list([{<<"Application-Name">>, <<"play_macro">>}
                      ,{<<"Media-Macro">>, Media}
                      ,{<<"Insert-At">>, <<"now">>}
                      ,{<<"Call-ID">>, kapps_call:call_id(Call)}
                      ]
                     ).

-spec b_play_macro(kz_term:ne_binaries(), kapps_call:call()) -> kapps_api_std_return().
b_play_macro(Media, Call) ->
    wait_for_noop(Call, play_macro(Media, Call)).

-spec play_macro(kz_term:ne_binaries(), kapps_call:call()) -> kz_term:ne_binary().
play_macro(Media, Call) ->
    NoopId = noop_id(),
    Commands = [kz_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                  ,{<<"Call-ID">>, kapps_call:call_id(Call)}
                                  ,{<<"Msg-ID">>, NoopId}
                                  ])
               ,play_macro_command(Media, Call)
               ],
    Command = [{<<"Application-Name">>, <<"queue">>}
              ,{<<"Commands">>, Commands}
              ],
    send_command(Command, Call),
    NoopId.

-type media_macro() :: {kz_term:ne_binary(), kz_term:ne_binaries()}.
-type media_macros() :: [media_macro()].
-spec media_macro_command(media_macros(), kapps_call:call()) ->kz_term:api_terms().
media_macro_command(Media, Call) ->
    kz_json:from_list(
      [{<<"Application-Name">>, <<"media_macro">>}
      ,{<<"Media-Macros">>, kz_json:from_list(Media)}
      ,{<<"Insert-At">>, <<"now">>}
      ,{<<"Call-ID">>, kapps_call:call_id(Call)}
      ]).

-spec media_macro(media_macros(), kapps_call:call()) -> 'ok'.
media_macro(Media, Call) ->
    Command = media_macro_command(Media, Call),
    send_command(Command, Call).

-spec sound_touch_command(kz_term:proplist(), kapps_call:call()) ->kz_term:api_terms().
sound_touch_command(Options, Call) ->
    kz_json:from_list(
      [{<<"Application-Name">>, <<"sound_touch">>}
      ,{<<"Insert-At">>, <<"now">>}
      ,{<<"Call-ID">>, kapps_call:call_id(Call)}
       | Options
      ]).

-spec start_sound_touch(kz_term:proplist(), kapps_call:call()) -> 'ok'.
start_sound_touch(Options, Call) ->
    Command = sound_touch_command([{<<"Action">>, <<"start">>} | Options], Call),
    send_command(Command, Call).

-spec stop_sound_touch(kapps_call:call()) -> 'ok'.
stop_sound_touch(Call) ->
    Command = sound_touch_command([{<<"Action">>, <<"stop">>}], Call),
    send_command(Command, Call).
