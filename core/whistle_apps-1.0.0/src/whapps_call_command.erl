%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whapps_call_command).

-include("./whapps_call_command.hrl").

-export([presence/2, presence/3]).
-export([channel_status/1, channel_status/2]).
-export([b_channel_status/1]).
-export([response/2, response/3, response/4]).

-export([relay_event/2
         ,receive_event/1, receive_event/2
        ]).

-export([audio_macro/2]).
-export([pickup/2, pickup/3, pickup/4, pickup/5, pickup/6
         ,b_pickup/2, b_pickup/3, b_pickup/4, b_pickup/5, b_pickup/6
        ]).
-export([redirect/2
         ,redirect/3
        ]).
-export([answer/1, answer_now/1
         ,hangup/1, hangup/2
         ,queued_hangup/1
         ,set/3, set_terminators/2
         ,fetch/1, fetch/2
        ]).
-export([echo/1]).
-export([ring/1]).
-export([receive_fax/1
         ,receive_fax/2
         ,receive_fax/3
         ,b_receive_fax/1
        ]).
-export([bridge/2, bridge/3, bridge/4, bridge/5, bridge/6, bridge/7]).
-export([page/2, page/3, page/4, page/5, page/6]).
-export([hold/1, hold/2
         ,b_hold/1, b_hold/2, b_hold/3
         ,park/1
        ]).
-export([play/2, play/3]).
-export([prompt/2, prompt/3]).

-export([tts/2, tts/3, tts/4, tts/5, tts/6
         ,b_tts/2, b_tts/3, b_tts/4, b_tts/5, b_tts/6
        ]).

-export([record/2, record/3, record/4, record/5, record/6]).
-export([record_call/2, record_call/3, record_call/4, record_call/5
         ,b_record_call/2, b_record_call/3, b_record_call/4, b_record_call/5
        ]).
-export([store/3, store/4, store/5
         ,store_fax/2
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
-export([say/2, say/3, say/4, say/5]).

-export([conference/2, conference/3
         ,conference/4, conference/5
         ,conference/6
        ]).
-export([b_conference/2, b_conference/3
         ,b_conference/4, b_conference/5
         ,b_conference/6
        ]).

-export([noop/1]).
-export([flush/1, flush_dtmf/1]).
-export([privacy/1
         ,privacy/2
        ]).

-export([b_answer/1, b_hangup/1, b_hangup/2, b_fetch/1, b_fetch/2]).
-export([b_echo/1]).
-export([b_ring/1]).
-export([b_bridge/2, b_bridge/3, b_bridge/4, b_bridge/5, b_bridge/6, b_bridge/7]).
-export([b_page/2, b_page/3, b_page/4, b_page/5, b_page/6]).
-export([b_play/2, b_play/3]).
-export([b_prompt/2, b_prompt/3]).
-export([b_record/2, b_record/3, b_record/4, b_record/5, b_record/6]).
-export([b_store/3, b_store/4, b_store/5
         ,b_store_fax/2
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
-export([b_say/2, b_say/3, b_say/4, b_say/5]).
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
         ,collect_digits/6
        ]).
-export([send_command/2]).

-export([default_collect_timeout/0
         ,default_digit_timeout/0
         ,default_interdigit_timeout/0
         ,default_message_timeout/0
         ,default_application_timeout/0
        ]).

-export([get_outbound_t38_settings/1, get_outbound_t38_settings/2]).
-export([get_inbound_t38_settings/1, get_inbound_t38_settings/2]).

-type audio_macro_prompt() :: {'play', binary()} | {'play', binary(), binaries()} |
                              {'prompt', binary()} | {'prompt', binary(), binary()} |
                              {'say', binary()} | {'say', binary(), binary()} |
                              {'say', binary(), binary(), binary()} |
                              {'say', binary(), binary(), binary(), binary()} |
                              {'tones', wh_json:objects()} |
                              {'tts', ne_binary()} |
                              {'tts', ne_binary(), ne_binary()} |
                              {'tts', ne_binary(), ne_binary(), ne_binary()}.
-type audio_macro_prompts() :: [audio_macro_prompt(),...] | [].
-export_type([audio_macro_prompt/0]).

-define(CONFIG_CAT, <<"call_command">>).

-define(DEFAULT_COLLECT_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, <<"collect_timeout">>, 5000)).
-define(DEFAULT_DIGIT_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, <<"digit_timeout">>, 3000)).
-define(DEFAULT_INTERDIGIT_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, <<"interdigit_timeout">>, 2000)).

-define(DEFAULT_MESSAGE_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, <<"message_timeout">>, 5000)).
-define(DEFAULT_APPLICATION_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, <<"application_timeout">>, 500000)).

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec presence(ne_binary(), ne_binary() | whapps_call:call()) -> 'ok'.
-spec presence(ne_binary(), ne_binary() | whapps_call:call(), api_binary() | whapps_call:call()) -> 'ok'.
presence(State, PresenceId) when is_binary(PresenceId) ->
    presence(State, PresenceId, 'undefined');
presence(State, Call) ->
    presence(State, whapps_call:from(Call)).

presence(State, PresenceId, CallId) when is_binary(CallId) orelse CallId =:= 'undefined' ->
    Command = [{<<"Presence-ID">>, PresenceId}
               ,{<<"State">>, State}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    wapi_notifications:publish_presence_update(Command);
presence(State, PresenceId, Call) ->
    presence(State, PresenceId, whapps_call:call_id(Call)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to get the channel status.
%% This request will execute immediately
%% @end
%%--------------------------------------------------------------------
-spec channel_status(whapps_call:call()) ->
                            'ok' | {'error', 'no_channel_id'}.
-spec channel_status(api_binary(), api_binary()) ->
                            'ok' | {'error', 'no_channel_id'}.
channel_status('undefined', _) -> {'error', 'no_channel_id'};
channel_status(CallId, SrvQueue) when is_binary(CallId), is_binary(SrvQueue) ->
    Command = [{<<"Call-ID">>, CallId}
               | wh_api:default_headers(SrvQueue, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_call:publish_channel_status_req(CallId, Command).

channel_status(Call) ->
    'true' = whapps_call:is_call(Call),
    channel_status(whapps_call:call_id(Call), whapps_call:controller_queue(Call)).

-spec b_channel_status(api_binary() | whapps_call:call()) ->
                              whapps_api_std_return().
b_channel_status('undefined') -> {'error', 'no_channel_id'};
b_channel_status(ChannelId) when is_binary(ChannelId) ->
    Command = [{<<"Call-ID">>, ChannelId}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Resp = wh_amqp_worker:call_collect('whapps_amqp_pool'
                                       ,Command
                                       ,fun(C) -> wapi_call:publish_channel_status_req(ChannelId, C) end
                                       ,{'ecallmgr', 'true'}
                                      ),
    case Resp of
        {'error', _}=E -> E;
        {_, JObjs} -> channel_status_filter(JObjs)
    end;
b_channel_status(Call) -> b_channel_status(whapps_call:call_id(Call)).

-spec channel_status_filter(wh_json:objects()) ->
                                   {'ok', wh_json:object()} |
                                   {'error', 'not_found'}.
channel_status_filter([]) -> {'error', 'not_found'};
channel_status_filter([JObj|JObjs]) ->
    case wapi_call:channel_status_resp_v(JObj) andalso
        wh_json:get_value(<<"Status">>, JObj) =:= <<"active">>
    of
        'true' -> {'ok', JObj};
        'false' -> channel_status_filter(JObjs)
    end.

%%--------------------------------------------------------------------
%% @pubic
%% @doc How amqp messages are sent to the mailboxes of processes waiting
%%      for them in the receive blocks below.
%% @end
%%--------------------------------------------------------------------
-spec relay_event(pid(), wh_json:object()) -> any().
relay_event(Pid, JObj) -> Pid ! {'amqp_msg', JObj}.

-spec receive_event(wh_timeout()) ->
                           {'ok', wh_json:object()} |
                           {'error', 'timeout'}.
-spec receive_event(wh_timeout(), boolean()) ->
                           {'ok', wh_json:object()} |
                           {'other', wh_json:object()} |
                           {'error', 'timeout'}.
receive_event(Timeout) -> receive_event(Timeout, 'true').
receive_event(T, _) when T =< 0 -> {'error', 'timeout'};
receive_event(Timeout, IgnoreOthers) ->
    Start = os:timestamp(),
    receive
        {'amqp_msg', JObj} -> {'ok', JObj};
        _ when IgnoreOthers ->
            receive_event(Timeout - wh_util:elapsed_ms(Start), IgnoreOthers);
        Other -> {'other', Other}
    after
        Timeout -> {'error', 'timeout'}
    end.

-spec audio_macro(audio_macro_prompts(), whapps_call:call()) ->
                         ne_binary().
-spec audio_macro(audio_macro_prompts(), whapps_call:call(), wh_json:objects()) ->
                         binary().
audio_macro([], Call) -> noop(Call);
audio_macro(Prompts, Call) -> audio_macro(Prompts, Call, []).

audio_macro([], Call, Queue) ->
    NoopId = couch_mgr:get_uuid(),
    Prompts = [wh_json:from_list(
                 [{<<"Application-Name">>, <<"noop">>}
                  ,{<<"Msg-ID">>, NoopId}
                  ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                 ]) | Queue
              ],
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Commands">>, Prompts}
              ],
    send_command(Command, Call),
    NoopId;
audio_macro([{'play', MediaName}|T], Call, Queue) ->
    audio_macro(T, Call, [play_command(MediaName, ?ANY_DIGIT, Call) | Queue]);
audio_macro([{'play', MediaName, Terminators}|T], Call, Queue) ->
    audio_macro(T, Call, [play_command(MediaName, Terminators, Call) | Queue]);
audio_macro([{'prompt', PromptName}|T], Call, Queue) ->
    audio_macro(T, Call, [play_command(whapps_util:get_prompt(PromptName, Call), ?ANY_DIGIT, Call) | Queue]);
audio_macro([{'prompt', PromptName, Lang}|T], Call, Queue) ->
    audio_macro(T, Call, [play_command(whapps_util:get_prompt(PromptName, Lang, Call), ?ANY_DIGIT, Call) | Queue]);
audio_macro([{'say', Say}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, <<"name_spelled">>, <<"pronounced">>, <<"en">>, Call) | Queue]);
audio_macro([{'say', Say, Type}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, Type, <<"pronounced">>, <<"en">>, Call) | Queue]);
audio_macro([{'say', Say, Type, Method}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, Type, Method, <<"en">>, Call) | Queue]);
audio_macro([{'say', Say, Type, Method, Language}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, Type, Method, Language, Call) | Queue]);
audio_macro([{'tones', Tones}|T], Call, Queue) ->
    audio_macro(T, Call, [tones_command(Tones, Call) | Queue]);
audio_macro([{'tts', Text}|T], Call, Queue) ->
    audio_macro(T, Call, [tts_command(Text, Call) | Queue]);
audio_macro([{'tts', Text, Voice}|T], Call, Queue) ->
    audio_macro(T, Call, [tts_command(Text, Voice, Call) | Queue]);
audio_macro([{'tts', Text, Voice, Lang}|T], Call, Queue) ->
    audio_macro(T, Call, [tts_command(Text, Voice, Lang, Call) | Queue]);
audio_macro([{'tts', Text, Voice, Lang, Engine}|T], Call, Queue) ->
    audio_macro(T, Call, [tts_command(Text, Voice, Lang, ?ANY_DIGIT, Engine, Call) | Queue]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec response(ne_binary(), whapps_call:call()) ->
                      {'ok', ne_binary()} |
                      {'error', 'no_response'}.
-spec response(ne_binary(), api_binary(), whapps_call:call()) ->
                      {'ok', ne_binary()} |
                      {'error', 'no_response'}.
-spec response(ne_binary(), api_binary(), api_binary(), whapps_call:call()) ->
                      {'ok', ne_binary()} |
                      {'error', 'no_response'}.
response(Code, Call) ->
    response(Code, 'undefined', Call).
response(Code, Cause, Call) ->
    response(Code, Cause, 'undefined', Call).
response(Code, Cause, Media, Call) ->
    CallId = whapps_call:call_id(Call),
    CtrlQ = whapps_call:control_queue(Call),
    wh_call_response:send(CallId, CtrlQ, Code, Cause, Media).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec pickup(ne_binary(), whapps_call:call()) -> 'ok'.
-spec pickup(ne_binary(), api_binary(), whapps_call:call()) -> 'ok'.
-spec pickup(ne_binary(), api_binary(), boolean(), whapps_call:call()) -> 'ok'.
-spec pickup(ne_binary(), api_binary(), boolean(), boolean(), whapps_call:call()) -> 'ok'.
-spec pickup(ne_binary(), api_binary(), boolean(), boolean(), boolean(), whapps_call:call()) -> 'ok'.
pickup(TargetCallId, Call) ->
    pickup(TargetCallId, <<"tail">>, Call).
pickup(TargetCallId, Insert, Call) ->
    pickup(TargetCallId, Insert, 'false', Call).
pickup(TargetCallId, Insert, ContinueOnFail, Call) ->
    pickup(TargetCallId, Insert, ContinueOnFail, 'true', Call).
pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call) ->
    pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, 'false', Call).
pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call) ->
    Command = [{<<"Application-Name">>, <<"call_pickup">>}
               ,{<<"Target-Call-ID">>, TargetCallId}
               ,{<<"Insert-At">>, Insert}
               ,{<<"Continue-On-Fail">>, ContinueOnFail}
               ,{<<"Continue-On-Cancel">>, ContinueOnCancel}
               ,{<<"Park-After-Pickup">>, ParkAfterPickup}
              ],
    send_command(Command, Call).

-spec b_pickup(ne_binary(), whapps_call:call()) ->
                      {'ok', wh_json:object()}.
-spec b_pickup(ne_binary(), ne_binary(), whapps_call:call()) ->
                      {'ok', wh_json:object()}.
-spec b_pickup(ne_binary(), ne_binary(), boolean(), whapps_call:call()) ->
                      {'ok', wh_json:object()}.
-spec b_pickup(ne_binary(), ne_binary(), boolean(), boolean(), whapps_call:call()) ->
                      {'ok', wh_json:object()}.
-spec b_pickup(ne_binary(), ne_binary(), boolean(), boolean(), boolean(), whapps_call:call()) ->
                      {'ok', wh_json:object()}.
b_pickup(TargetCallId, Call) ->
    pickup(TargetCallId, Call),
    wait_for_channel_unbridge().
b_pickup(TargetCallId, Insert, Call) ->
    pickup(TargetCallId, Insert, Call),
    wait_for_channel_unbridge().
b_pickup(TargetCallId, Insert, ContinueOnFail, Call) ->
    pickup(TargetCallId, Insert, ContinueOnFail, Call),
    wait_for_channel_unbridge().
b_pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call) ->
    pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call),
    wait_for_channel_unbridge().
b_pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call) ->
    pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call),
    wait_for_channel_unbridge().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a redirect request to the Contact on Server
%% @end
%%--------------------------------------------------------------------
-spec redirect(ne_binary(), whapps_call:call()) -> 'ok'.
redirect(Contact, Call) ->
    redirect(Contact, 'undefined', Call).

-spec redirect(ne_binary(), api_binary(), whapps_call:call()) -> 'ok'.
redirect(Contact, Server, Call) ->
    lager:debug("redirect to ~s on ~s", [Contact, Server]),
    Command = [{<<"Redirect-Contact">>, Contact}
               ,{<<"Redirect-Server">>, Server}
               ,{<<"Application-Name">>, <<"redirect">>}
              ],
    send_command(Command, Call),
    timer:sleep(2000),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec flush_dtmf(whapps_call:call()) -> ne_binary().
flush_dtmf(Call) -> play(<<"silence_stream://50">>, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to set channel/call vars
%% NOTICE: These are 'custom' channel vars for state info only, and
%%   can not be used to set system settings
%% @end
%%--------------------------------------------------------------------
-spec set(api_object(), api_object(), whapps_call:call()) -> 'ok'.
set('undefined', CallVars, Call) -> set(wh_json:new(), CallVars, Call);
set(ChannelVars, 'undefined', Call) -> set(ChannelVars, wh_json:new(), Call);
set(ChannelVars, CallVars, Call) ->
    case wh_json:is_empty(ChannelVars) andalso wh_json:is_empty(CallVars) of
        'true' -> 'ok';
        'false' ->
            Command = [{<<"Application-Name">>, <<"set">>}
                       ,{<<"Insert-At">>, <<"now">>}
                       ,{<<"Custom-Channel-Vars">>, ChannelVars}
                       ,{<<"Custom-Call-Vars">>, CallVars}
                      ],
            send_command(Command, Call)
    end.

set_terminators(Terminators, Call) ->
    Command = [{<<"Application-Name">>, <<"set_terminators">>}
               ,{<<"Terminators">>, Terminators}
              ],
    send_command(Command, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to fetch channe vars
%% NOTICE: These are 'custom' channel vars for state info only, and
%%   can not the switch vars
%% @end
%%--------------------------------------------------------------------
-spec fetch(whapps_call:call()) -> 'ok'.
-spec fetch(boolean(), whapps_call:call()) -> 'ok'.

-spec b_fetch(whapps_call:call()) -> whapps_api_std_return().
-spec b_fetch(boolean(), whapps_call:call()) -> whapps_api_std_return().

fetch(Call) -> fetch('false', Call).
fetch(FromOtherLeg, Call) ->
    Command = [{<<"Application-Name">>, <<"fetch">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"From-Other-Leg">>, FromOtherLeg}
              ],
    send_command(Command, Call).

b_fetch(Call) -> b_fetch('false', Call).
b_fetch(FromOtherLeg, Call) ->
    fetch(FromOtherLeg, Call),
    case wait_for_message(Call, <<"fetch">>) of
        {'ok', JObj} ->
            {'ok', wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())};
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to ring the channel
%% @end
%%--------------------------------------------------------------------
-spec ring(whapps_call:call()) -> 'ok'.
-spec b_ring(whapps_call:call()) ->
                    whapps_api_error() |
                    {'ok', wh_json:object()}.
ring(Call) ->
    Command = [{<<"Application-Name">>, <<"ring">>}],
    send_command(Command, Call).

b_ring(Call) ->
    ring(Call),
    wait_for_message(Call, <<"ring">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Instructs the switch to expect to receive a fax
%% @end
%%--------------------------------------------------------------------
-spec receive_fax(whapps_call:call()) -> 'ok'.
-spec receive_fax(boolean(), whapps_call:call()) -> 'ok'.
-spec receive_fax(boolean() | api_binary()
                 ,boolean() | api_binary()
                 ,whapps_call:call()) -> 'ok'.
-spec b_receive_fax(whapps_call:call()) -> wait_for_fax_ret().
receive_fax(Call) ->
    receive_fax(get_default_t38_setting(), Call).

receive_fax(DefaultFlag, Call) ->
    T38Settings = props:filter_undefined(get_inbound_t38_settings(DefaultFlag)),
    Commands = [{<<"Application-Name">>, <<"receive_fax">>}] ++ T38Settings,
    send_command(Commands, Call).

receive_fax('undefined', 'undefined', Call) ->
    receive_fax(get_default_t38_setting(), Call);
receive_fax(ResourceFlag, ReceiveFlag, Call) ->
    T38Settings = props:filter_undefined(get_inbound_t38_settings(ResourceFlag, ReceiveFlag)),
    Commands = [{<<"Application-Name">>, <<"receive_fax">>}] ++ T38Settings,
    send_command(Commands, Call).

b_receive_fax(Call) ->
    receive_fax(Call),
    wait_for_fax().

-spec get_default_t38_setting() -> boolean() | ne_binary().
get_default_t38_setting() ->
    case whapps_config:get_binary(<<"fax">>, <<"inbound_t38_default">>, 'true') of
        <<"auto">> -> <<"auto">>;
        Otherwise -> wh_util:is_true(Otherwise)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to answer the channel
%% @end
%%--------------------------------------------------------------------
-spec answer(whapps_call:call()) -> 'ok'.
-spec answer_now(whapps_call:call()) -> 'ok'.
-spec b_answer(whapps_call:call()) ->
                      whapps_api_error() |
                      {'ok', wh_json:object()}.
answer(Call) -> send_command([{<<"Application-Name">>, <<"answer">>}], Call).
answer_now(Call) -> send_command([{<<"Application-Name">>, <<"answer">>}
                                  ,{<<"Insert-At">>, <<"now">>}
                                 ], Call).

b_answer(Call) ->
    answer(Call),
    wait_for_message(Call, <<"answer">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to echo the channel
%% @end
%%--------------------------------------------------------------------
-spec echo(whapps_call:call()) -> 'ok'.
-spec b_echo(whapps_call:call()) ->
                      whapps_api_error() |
                      {'ok', wh_json:object()}.
echo(Call) -> send_command([{<<"Application-Name">>, <<"echo">>}], Call).

b_echo(Call) ->
    echo(Call),
    wait_for_message(Call, <<"echo">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to hangup the channel.
%% This request will execute immediately
%% @end
%%--------------------------------------------------------------------
-spec queued_hangup(whapps_call:call()) -> 'ok'.
-spec hangup(whapps_call:call()) -> 'ok'.
-spec hangup(boolean(), whapps_call:call()) -> 'ok'.

-spec b_hangup(whapps_call:call()) ->
                      {'ok', 'channel_hungup'}.
-spec b_hangup(boolean(), whapps_call:call()) ->
                      {'ok', 'channel_hungup' | 'leg_hangup'}.

hangup(Call) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

queued_hangup(Call) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}],
    send_command(Command, Call).

hangup(OtherLegOnly, Call) when is_boolean(OtherLegOnly) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
               ,{<<"Other-Leg-Only">>, OtherLegOnly}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

b_hangup(Call) ->
    hangup(Call),
    wait_for_hangup().
b_hangup('false', Call) ->
    hangup(Call),
    wait_for_hangup();
b_hangup('true', Call) ->
    hangup('true', Call),
    wait_for_unbridge().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to page the call
%% @end
%%--------------------------------------------------------------------
-spec page(wh_json:objects(), whapps_call:call()) -> 'ok'.
-spec page(wh_json:objects(), integer(), whapps_call:call()) -> 'ok'.
-spec page(wh_json:objects(), integer(), api_binary(), whapps_call:call()) -> 'ok'.
-spec page(wh_json:objects(), integer(), api_binary(), api_binary(), whapps_call:call()) -> 'ok'.
-spec page(wh_json:objects(), integer(), api_binary(), api_binary(), api_object(), whapps_call:call()) -> 'ok'.

-spec b_page(wh_json:objects(), whapps_call:call()) ->
                    wait_for_application_return().
-spec b_page(wh_json:objects(), integer(), whapps_call:call()) ->
                    wait_for_application_return().
-spec b_page(wh_json:objects(), integer(), api_binary(), whapps_call:call()) ->
                    wait_for_application_return().
-spec b_page(wh_json:objects(), integer(), api_binary(), api_binary(), whapps_call:call()) ->
                    wait_for_application_return().
-spec b_page(wh_json:objects(), integer(), api_binary(), api_binary(), api_object(), whapps_call:call()) ->
                    wait_for_application_return().

page(Endpoints, Call) ->
    page(Endpoints, ?DEFAULT_TIMEOUT_S, Call).
page(Endpoints, Timeout, Call) ->
    page(Endpoints, Timeout, 'undefined', Call).
page(Endpoints, Timeout, CIDName, Call) ->
    page(Endpoints, Timeout, CIDName, 'undefined', Call).
page(Endpoints, Timeout, CIDName, CIDNumber, Call) ->
    page(Endpoints, Timeout, CIDName, CIDNumber, 'undefined', Call).
page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, Call) ->
    Command = [{<<"Application-Name">>, <<"page">>}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Timeout">>, Timeout}
               ,{<<"Caller-ID-Name">>, CIDName}
               ,{<<"Caller-ID-Number">>, CIDNumber}
               ,{<<"SIP-Headers">>, SIPHeaders}
              ],
    send_command(Command, Call).

b_page(Endpoints, Call) ->
    b_page(Endpoints, ?DEFAULT_TIMEOUT_S, Call).
b_page(Endpoints, Timeout, Call) ->
    b_page(Endpoints, Timeout, 'undefined', Call).
b_page(Endpoints, Timeout, CIDName, Call) ->
    b_page(Endpoints, Timeout, CIDName, 'undefined', Call).
b_page(Endpoints, Timeout, CIDName, CIDNumber, Call) ->
    b_page(Endpoints, Timeout, CIDName, CIDNumber, 'undefined', Call).
b_page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, Call) ->
    page(Endpoints, Timeout, CIDName, CIDNumber, SIPHeaders, Call),
    wait_for_application(Call, <<"page">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to bridge the call
%% @end
%%--------------------------------------------------------------------
-spec bridge(wh_json:objects(), whapps_call:call()) -> 'ok'.
-spec bridge(wh_json:objects(), integer(), whapps_call:call()) -> 'ok'.
-spec bridge(wh_json:objects(), integer(), api_binary(), whapps_call:call()) -> 'ok'.
-spec bridge(wh_json:objects(), integer(), api_binary(), api_binary(), whapps_call:call()) -> 'ok'.
-spec bridge(wh_json:objects(), integer(), api_binary(), api_binary(), api_binary(), whapps_call:call()) -> 'ok'.
-spec bridge(wh_json:objects(), integer(), api_binary(), api_binary(), api_binary(), api_object(), whapps_call:call()) -> 'ok'.

-spec b_bridge(wh_json:objects(), whapps_call:call()) ->
                      whapps_api_bridge_return().
-spec b_bridge(wh_json:objects(), integer(), whapps_call:call()) ->
                      whapps_api_bridge_return().
-spec b_bridge(wh_json:objects(), integer(), api_binary(), whapps_call:call()) ->
                      whapps_api_bridge_return().
-spec b_bridge(wh_json:objects(), integer(), api_binary(), api_binary(), whapps_call:call()) ->
                      whapps_api_bridge_return().
-spec b_bridge(wh_json:objects(), integer(), api_binary(), api_binary(), api_binary(), whapps_call:call()) ->
                      whapps_api_bridge_return().
-spec b_bridge(wh_json:objects(), integer(), api_binary(), api_binary(), api_binary(), api_object(), whapps_call:call())
              -> whapps_api_bridge_return().

bridge(Endpoints, Call) ->
    bridge(Endpoints, ?DEFAULT_TIMEOUT_S, Call).
bridge(Endpoints, Timeout, Call) ->
    bridge(Endpoints, Timeout, wapi_dialplan:dial_method_single(), Call).
bridge(Endpoints, Timeout, Strategy, Call) ->
    bridge(Endpoints, Timeout, Strategy, <<"true">>, Call).
bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, 'undefined', Call).
bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, 'undefined', Call).
bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, Call) ->
    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Timeout">>, Timeout}
               ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
               ,{<<"Ringback">>, cf_util:correct_media_path(Ringback, Call)}
               ,{<<"Dial-Endpoint-Method">>, Strategy}
               ,{<<"SIP-Headers">>, SIPHeaders}
              ],
    send_command(Command, Call).

b_bridge(Endpoints, Call) ->
    b_bridge(Endpoints, ?DEFAULT_TIMEOUT_S, Call).
b_bridge(Endpoints, Timeout, Call) ->
    b_bridge(Endpoints, Timeout, wapi_dialplan:dial_method_single(), Call).
b_bridge(Endpoints, Timeout, Strategy, Call) ->
    b_bridge(Endpoints, Timeout, Strategy, <<"false">>, Call).
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Call) ->
    b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, 'undefined', Call).
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, Call) ->
    b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, 'undefined', Call).
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, Call),
    wait_for_bridge((wh_util:to_integer(Timeout)*1000) + 10000, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to park the channel
%% @end
%%--------------------------------------------------------------------
-spec hold(whapps_call:call()) -> 'ok'.
-spec hold(api_binary(), whapps_call:call()) -> 'ok'.

-spec b_hold(whapps_call:call()) -> whapps_api_std_return().
-spec b_hold(wh_timeout() | api_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_hold(wh_timeout(), api_binary(), whapps_call:call()) -> whapps_api_std_return().

hold(Call) -> hold('undefined', Call).
hold(MOH, Call) ->
    Command = [{<<"Application-Name">>, <<"hold">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Hold-Media">>, MOH}
              ],
    send_command(Command, Call).

b_hold(Call) -> b_hold('infinity', 'undefined', Call).
b_hold(Timeout, Call) when is_integer(Timeout) orelse Timeout =:= 'infinity' ->
    b_hold(Timeout, 'undefined', Call);
b_hold(MOH, Call) -> b_hold('infinity', MOH, Call).
b_hold(Timeout, MOH, Call) ->
    hold(MOH, Call),
    wait_for_message(Call, <<"hold">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, Timeout).

-spec park(whapps_call:call()) -> 'ok'.
park(Call) ->
    Command = [{<<"Application-Name">>, <<"park">>}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to play media to the
%% caller.
%% @end
%%--------------------------------------------------------------------
-spec prompt(ne_binary(), whapps_call:call()) -> ne_binary().
-spec prompt(ne_binary(), ne_binary(), whapps_call:call()) -> ne_binary().

-spec b_prompt(ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_prompt(ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().

prompt(Prompt, Call) -> prompt(Prompt, <<"en">>, Call).
prompt(Prompt, Lang, Call) -> play(whapps_util:get_prompt(Prompt, Lang, Call), Call).

b_prompt(Prompt, Call) -> b_prompt(Prompt, <<"en">>, Call).
b_prompt(Prompt, Lang, Call) -> b_play(whapps_util:get_prompt(Prompt, Lang, Call), Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to play media to the
%% caller.  A list of terminators can be provided that the caller
%% can use to skip playback.
%% @end
%%--------------------------------------------------------------------
-spec play(ne_binary(), whapps_call:call()) -> ne_binary().
-spec play(ne_binary(), ne_binaries(), whapps_call:call()) -> ne_binary().

-spec b_play(ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_play(ne_binary(), ne_binaries(), whapps_call:call()) -> whapps_api_std_return().

play(Media, Call) -> play(Media, ?ANY_DIGIT, Call).
play(Media, Terminators, Call) ->
    NoopId = couch_mgr:get_uuid(),
    CallId = whapps_call:call_id(Call),

    Commands = [wh_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                   ,{<<"Call-ID">>, CallId}
                                   ,{<<"Msg-ID">>, NoopId}
                                  ])
                ,wh_json:from_list(
                   props:filter_undefined(
                     [{<<"Application-Name">>, <<"play">>}
                      ,{<<"Media-Name">>, Media}
                      ,{<<"Terminators">>, Terminators}
                      ,{<<"Call-ID">>, CallId}
                     ])
                  )
               ],
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Commands">>, Commands}
              ],
    send_command(Command, Call),
    NoopId.

b_play(Media, Call) ->
    b_play(Media, ?ANY_DIGIT, Call).
b_play(Media, Terminators, Call) ->
    wait_for_noop(Call, play(Media, Terminators, Call)).

-spec play_command(ne_binary(), ne_binaries(), whapps_call:call()) -> wh_json:object().
play_command(Media, Terminators, Call) ->
    wh_json:from_list([{<<"Application-Name">>, <<"play">>}
                       ,{<<"Media-Name">>, Media}
                       ,{<<"Terminators">>, Terminators}
                       ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% requests the TTS engine to create an audio file to play the desired
%% text.
%%
%% @end
%%--------------------------------------------------------------------
-spec tts(api_binary(), whapps_call:call()) -> ne_binary().
-spec tts(api_binary(), api_binary(), whapps_call:call()) -> ne_binary().
-spec tts(api_binary(), api_binary(), api_binary(), whapps_call:call()) -> ne_binary().
-spec tts(api_binary(), api_binary(), api_binary(), api_binaries(), whapps_call:call()) -> ne_binary().
-spec tts(api_binary(), api_binary(), api_binary(), api_binaries(), api_binary(), whapps_call:call()) -> ne_binary().

tts(SayMe, Call) -> tts(SayMe, <<"female">>, Call).
tts(SayMe, Voice, Call) -> tts(SayMe, Voice, <<"en-US">>, Call).
tts(SayMe, Voice, Lang, Call) -> tts(SayMe, Voice, Lang, ?ANY_DIGIT, Call).
tts(SayMe, Voice, Lang, Terminators, Call) ->
    tts(SayMe, Voice, Lang, Terminators
        ,whapps_account_config:get_global(Call, ?MOD_CONFIG_CAT, <<"tts_provider">>, <<"flite">>)
        ,Call
       ).
tts(SayMe, Voice, Lang, Terminators, Engine, Call) ->
    NoopId = couch_mgr:get_uuid(),

    Commands = [wh_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                   ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                                   ,{<<"Msg-ID">>, NoopId}
                                  ])
                ,tts_command(SayMe, Voice, Lang, Terminators, Engine, Call)
               ],
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Commands">>, Commands}
              ],
    send_command(Command, Call),
    NoopId.

-spec tts_command(api_binary(), whapps_call:call()) -> wh_json:object().
-spec tts_command(api_binary(), api_binary(), whapps_call:call()) -> wh_json:object().
-spec tts_command(api_binary(), api_binary(), api_binary(), whapps_call:call()) -> wh_json:object().
-spec tts_command(api_binary(), api_binary(), api_binary(), api_binaries(), whapps_call:call()) -> wh_json:object().
-spec tts_command(api_binary(), api_binary(), api_binary(), api_binaries(), api_binary(), whapps_call:call()) -> wh_json:object().
tts_command(SayMe, Call) -> tts_command(SayMe, <<"female">>, Call).
tts_command(SayMe, Voice, Call) -> tts_command(SayMe, Voice, <<"en-US">>, Call).
tts_command(SayMe, Voice, Lang, Call) -> tts_command(SayMe, Voice, Lang, ?ANY_DIGIT, Call).
tts_command(SayMe, Voice, Lang, Terminators, Call) ->
    tts_command(SayMe, Voice, Lang, Terminators
                ,whapps_account_config:get_global(Call, ?MOD_CONFIG_CAT, <<"tts_provider">>, <<"flite">>)
                ,Call
               ).
tts_command(SayMe, Voice, Lang, Terminators, Engine, Call) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Application-Name">>, <<"tts">>}
         ,{<<"Text">>, SayMe}
         ,{<<"Terminators">>, Terminators}
         ,{<<"Voice">>, Voice}
         ,{<<"Language">>, Lang}
         ,{<<"Engine">>, Engine}
         ,{<<"Call-ID">>, whapps_call:call_id(Call)}
        ])).

-spec b_tts(api_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_tts(api_binary(), api_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_tts(api_binary(), api_binary(), api_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_tts(api_binary(), api_binary(), api_binary(), api_binaries(), whapps_call:call()) -> whapps_api_std_return().
-spec b_tts(api_binary(), api_binary(), api_binary(), api_binaries(), api_binary(), whapps_call:call()) -> whapps_api_std_return().

b_tts(SayMe, Call) ->
    wait_for_noop(Call, tts(SayMe, Call)).
b_tts(SayMe, Voice, Call) ->
    wait_for_noop(Call, tts(SayMe, Voice, Call)).
b_tts(SayMe, Voice, Lang, Call) ->
    wait_for_noop(Call, tts(SayMe, Voice, Lang, Call)).
b_tts(SayMe, Voice, Lang, Terminators, Call) ->
    wait_for_noop(Call, tts(SayMe, Voice, Lang, Terminators, Call)).
b_tts(SayMe, Voice, Lang, Terminators, Engine, Call) ->
    wait_for_noop(Call, tts(SayMe, Voice, Lang, Terminators, Engine, Call)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to record a file.
%% A list of keys can be used as the terminator or a silence threshold.
%% @end
%%--------------------------------------------------------------------
-spec record(ne_binary(), whapps_call:call()) -> 'ok'.
-spec record(ne_binary(), binaries(), whapps_call:call()) -> 'ok'.
-spec record(ne_binary(), binaries(),  api_binary() | integer(), whapps_call:call()) -> 'ok'.
-spec record(ne_binary(), binaries(),  api_binary() | integer(), api_binary() | integer(), whapps_call:call()) -> 'ok'.
-spec record(ne_binary(), binaries(),  api_binary() | integer(), api_binary() | integer(),  api_binary() | integer(), whapps_call:call()) -> 'ok'.

-spec b_record(ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_record(ne_binary(), binaries(), whapps_call:call()) -> whapps_api_std_return().
-spec b_record(ne_binary(), binaries(), api_binary() | integer(), whapps_call:call()) -> whapps_api_std_return().
-spec b_record(ne_binary(), binaries(), api_binary() | integer(), api_binary() | integer(), whapps_call:call()) -> whapps_api_std_return().
-spec b_record(ne_binary(), binaries(), api_binary() | integer(), api_binary() | integer(), api_binary() | integer(), whapps_call:call()) -> whapps_api_std_return().

record(MediaName, Call) ->
    record(MediaName, ?ANY_DIGIT, Call).
record(MediaName, Terminators, Call) ->
    record(MediaName, Terminators, <<"300">>, Call).
record(MediaName, Terminators, TimeLimit, Call) ->
    record(MediaName, Terminators, TimeLimit, <<"200">>,  Call).
record(MediaName, Terminators, TimeLimit, SilenceThreshold, Call) ->
    record(MediaName, Terminators, TimeLimit, SilenceThreshold, <<"5">>, Call).
record(MediaName, Terminators, TimeLimit, SilenceThreshold, SilenceHits, Call) ->
    Command = [{<<"Application-Name">>, <<"record">>}
               ,{<<"Media-Name">>, MediaName}
               ,{<<"Terminators">>, Terminators}
               ,{<<"Time-Limit">>, TimeLimit}
               ,{<<"Silence-Threshold">>, SilenceThreshold}
               ,{<<"Silence-Hits">>, SilenceHits}
              ],
    send_command(Command, Call).

b_record(MediaName, Call) ->
    b_record(MediaName, ?ANY_DIGIT, Call).
b_record(MediaName, Terminators, Call) ->
    b_record(MediaName, Terminators, <<"300">>, Call).
b_record(MediaName, Terminators, TimeLimit, Call) ->
    b_record(MediaName, Terminators, TimeLimit, <<"200">>,  Call).
b_record(MediaName, Terminators, TimeLimit, SilenceThreshold, Call) ->
    b_record(MediaName, Terminators, TimeLimit, SilenceThreshold, <<"5">>, Call).
b_record(MediaName, Terminators, TimeLimit, SilenceThreshold, SilenceHits, Call) ->
    record(MediaName, Terminators, TimeLimit, SilenceThreshold, SilenceHits, Call),
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, 'infinity').

-spec record_call(ne_binary(), whapps_call:call()) -> 'ok'.
-spec record_call(ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec record_call(ne_binary(), ne_binary(),  api_binary() | pos_integer(), whapps_call:call()) -> 'ok'.
-spec record_call(ne_binary(), ne_binary(),  api_binary() | pos_integer(), list(), whapps_call:call()) -> 'ok'.
record_call(MediaName, Call) ->
    record_call(MediaName, <<"start">>, Call).
record_call(MediaName, Action, Call) ->
    record_call(MediaName, Action, 600, Call).
record_call(MediaName, Action, TimeLimit, Call) ->
    record_call(MediaName, Action, TimeLimit, ?ANY_DIGIT, Call).
record_call(MediaName, Action, TimeLimit, Terminators, Call) ->
    Command = [{<<"Application-Name">>, <<"record_call">>}
               ,{<<"Media-Name">>, MediaName}
               ,{<<"Record-Action">>, Action}
               ,{<<"Time-Limit">>, wh_util:to_binary(TimeLimit)}
               ,{<<"Terminators">>, Terminators}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

-spec b_record_call(ne_binary(), whapps_call:call()) ->
                                 wait_for_headless_application_return().
-spec b_record_call(ne_binary(), ne_binary(), whapps_call:call()) ->
                                 wait_for_headless_application_return().
-spec b_record_call(ne_binary(), ne_binary(), api_binary() | pos_integer(), whapps_call:call()) ->
                                 wait_for_headless_application_return().
-spec b_record_call(ne_binary(), ne_binary(), api_binary() | pos_integer(), list(), whapps_call:call()) ->
                                 wait_for_headless_application_return().
b_record_call(MediaName, Call) ->
    record_call(MediaName, Call),
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, 'infinity').
b_record_call(MediaName, Action, Call) ->
    record_call(MediaName, Action, Call),
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, 'infinity').
b_record_call(MediaName, Action, TimeLimit, Call) ->
    b_record_call(MediaName, Action, TimeLimit, ?ANY_DIGIT, Call).
b_record_call(MediaName, Action, TimeLimit, Terminators, Call) ->
    record_call(MediaName, Action, TimeLimit, Terminators, Call),
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, 'infinity').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to store the file
%% @end
%%--------------------------------------------------------------------
-type b_store_return() :: {'error', 'timeout' | wh_json:object()} |
                          {'ok', wh_json:object()}.

-spec store(ne_binary(), api_binary(), whapps_call:call()) -> 'ok'.
-spec store(ne_binary(), api_binary(), api_binary(), whapps_call:call()) -> 'ok'.
-spec store(ne_binary(), api_binary(), api_binary(), wh_json:objects(), whapps_call:call()) -> 'ok'.

-spec b_store(ne_binary(), ne_binary(), whapps_call:call()) -> b_store_return().
-spec b_store(ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> b_store_return().
-spec b_store(ne_binary(), ne_binary(), ne_binary(), wh_json:objects(), whapps_call:call()) -> b_store_return().

store(MediaName, Transfer, Call) ->
    store(MediaName, Transfer, <<"put">>, Call).
store(MediaName, Transfer, Method, Call) ->
    store(MediaName, Transfer, Method, [wh_json:new()], Call).
store(MediaName, Transfer, Method, Headers, Call) ->
    Command = [{<<"Application-Name">>, <<"store">>}
               ,{<<"Media-Name">>, MediaName}
               ,{<<"Media-Transfer-Method">>, Method}
               ,{<<"Media-Transfer-Destination">>, Transfer}
               ,{<<"Additional-Headers">>, Headers}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

b_store(MediaName, Transfer, Call) ->
    b_store(MediaName, Transfer, <<"put">>, Call).
b_store(MediaName, Transfer, Method, Call) ->
    b_store(MediaName, Transfer, Method, [wh_json:new()], Call).
b_store(MediaName, Transfer, Method, Headers, Call) ->
    store(MediaName, Transfer, Method, Headers, Call),
    wait_for_headless_application(<<"store">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to store a fax document
%% caller
%% @end
%%--------------------------------------------------------------------
-spec store_fax(ne_binary(), whapps_call:call()) -> 'ok'.
store_fax(URL, Call) ->
    Command = [{<<"Application-Name">>, <<"store_fax">>}
               ,{<<"Media-Transfer-Method">>, <<"put">>}
               ,{<<"Media-Transfer-Destination">>, URL}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

-spec b_store_fax(ne_binary(), whapps_call:call()) -> b_store_return().
b_store_fax(URL, Call) ->
    store_fax(URL, Call),
    wait_for_headless_application(<<"store_fax">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to play tones to the
%% caller
%% @end
%%--------------------------------------------------------------------
-spec tones(wh_json:objects(), whapps_call:call()) -> 'ok'.
tones(Tones, Call) ->
    Command = [{<<"Application-Name">>, <<"tones">>}
               ,{<<"Tones">>, Tones}
              ],
    send_command(Command, Call).

-spec tones_command(wh_json:objects(), whapps_call:call()) -> wh_json:object().
tones_command(Tones, Call) ->
    CallId = whapps_call:call_id(Call),
    wh_json:from_list([{<<"Application-Name">>, <<"tones">>}
                       ,{<<"Tones">>, Tones}
                       ,{<<"Call-ID">>, CallId}
                      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to prompt a
%% caller, and collect a number of DTMF events.
%% @end
%%--------------------------------------------------------------------
-spec prompt_and_collect_digit(ne_binary(), whapps_call:call()) -> 'ok'.
-spec prompt_and_collect_digits(integer(), integer(), ne_binary()
                              ,whapps_call:call()) ->
                                     'ok'.
-spec prompt_and_collect_digits(integer(), integer(), ne_binary()
                              ,integer(), whapps_call:call()) ->
                                     'ok'.
-spec prompt_and_collect_digits(integer(), integer(), ne_binary()
                              ,integer(), integer(), whapps_call:call()) ->
                                     'ok'.
-spec prompt_and_collect_digits(integer(), integer(), ne_binary()
                              ,integer(), integer(), api_binary()
                              ,whapps_call:call()) ->
                                     'ok'.
-spec prompt_and_collect_digits(integer(), integer(), ne_binary()
                              ,integer(), integer(), api_binary()
                              ,ne_binary(), whapps_call:call()) ->
                                     'ok'.
-spec prompt_and_collect_digits(integer(), integer(), ne_binary()
                              ,integer(), integer(), api_binary()
                              ,ne_binary(), ne_binaries(), whapps_call:call()) ->
                                     'ok'.

-spec b_prompt_and_collect_digit(ne_binary(), whapps_call:call()) ->
                                      b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits(integer(), integer(), ne_binary()
                                ,whapps_call:call()) ->
                                       b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits(integer(), integer(), ne_binary()
                                ,integer(), whapps_call:call()) ->
                                       b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits(integer(), integer(), ne_binary()
                                ,integer(), integer(), whapps_call:call()) ->
                                       b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits(integer(), integer(), ne_binary()
                                ,integer(), integer(), api_binary()
                                ,whapps_call:call()) ->
                                       b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits(integer(), integer(), ne_binary()
                                ,integer(), integer(), api_binary()
                                ,ne_binary(), whapps_call:call()) ->
                                       b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits(integer(), integer(), ne_binary()
                                ,integer(), integer(), api_binary()
                                ,ne_binary(), ne_binaries(), whapps_call:call()) ->
                                       b_play_and_collect_digits_return().

prompt_and_collect_digit(Prompt, Call) ->
    prompt_and_collect_digits(1, 1, Prompt, Call).

prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, 1,  Call).
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, 3000, Call).
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, 'undefined', Call).
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, <<"\\d+">>, Call).
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, [<<"#">>], Call).
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Terminators, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, whapps_util:get_prompt(Prompt, Call), Tries, Timeout, InvalidPrompt, Regex, Terminators, Call).

b_prompt_and_collect_digit(Prompt, Call) ->
    b_prompt_and_collect_digits(1, 1, Prompt, Call).
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, 3,  Call).
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, ?DEFAULT_COLLECT_TIMEOUT, Call).
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, 'undefined', Call).
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, <<"\\d+">>, Call).
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, ?ANY_DIGIT, Call).

b_prompt_and_collect_digits(_MinDigits, _MaxDigits, _Prompt, 0, _Timeout, 'undefined', _Regex, _Terminators, _Call) ->
    {'ok', <<>>};
b_prompt_and_collect_digits(_MinDigits, _MaxDigits, _Prompt, 0, _Timeout, InvalidPrompt, _Regex, _Terminators, Call) ->
    _ = b_prompt(InvalidPrompt, Call),
    {'ok', <<>>};
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Terminators, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, whapps_util:get_prompt(Prompt, Call), Tries, Timeout, InvalidPrompt, Regex, Terminators, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to play media to a
%% caller, and collect a number of DTMF events.
%% @end
%%--------------------------------------------------------------------
-type b_play_and_collect_digits_return() ::
        {'error', 'channel_hungup' | 'channel_unbridge' | wh_json:object()} |
        {'ok', binary()}.

-spec play_and_collect_digit(ne_binary(), whapps_call:call()) -> 'ok'.
-spec play_and_collect_digits(integer(), integer(), ne_binary()
                              ,whapps_call:call()) ->
                                     'ok'.
-spec play_and_collect_digits(integer(), integer(), ne_binary()
                              ,integer(), whapps_call:call()) ->
                                     'ok'.
-spec play_and_collect_digits(integer(), integer(), ne_binary()
                              ,integer(), integer(), whapps_call:call()) ->
                                     'ok'.
-spec play_and_collect_digits(integer(), integer(), ne_binary()
                              ,integer(), integer(), api_binary()
                              ,whapps_call:call()) ->
                                     'ok'.
-spec play_and_collect_digits(integer(), integer(), ne_binary()
                              ,integer(), integer(), api_binary()
                              ,ne_binary(), whapps_call:call()) ->
                                     'ok'.
-spec play_and_collect_digits(integer(), integer(), ne_binary()
                              ,integer(), integer(), api_binary()
                              ,ne_binary(), ne_binaries(), whapps_call:call()) ->
                                     'ok'.

-spec b_play_and_collect_digit(ne_binary(), whapps_call:call()) ->
                                      b_play_and_collect_digits_return().
-spec b_play_and_collect_digits(integer(), integer(), ne_binary()
                                ,whapps_call:call()) ->
                                       b_play_and_collect_digits_return().
-spec b_play_and_collect_digits(integer(), integer(), ne_binary()
                                ,integer(), whapps_call:call()) ->
                                       b_play_and_collect_digits_return().
-spec b_play_and_collect_digits(integer(), integer(), ne_binary()
                                ,integer(), integer(), whapps_call:call()) ->
                                       b_play_and_collect_digits_return().
-spec b_play_and_collect_digits(integer(), integer(), ne_binary()
                                ,integer(), integer(), api_binary()
                                ,whapps_call:call()) ->
                                       b_play_and_collect_digits_return().
-spec b_play_and_collect_digits(integer(), integer(), ne_binary()
                                ,integer(), integer(), api_binary()
                                ,ne_binary(), whapps_call:call()) ->
                                       b_play_and_collect_digits_return().
-spec b_play_and_collect_digits(integer(), integer(), ne_binary()
                                ,integer(), integer(), api_binary()
                                ,ne_binary(), ne_binaries(), whapps_call:call()) ->
                                       b_play_and_collect_digits_return().

play_and_collect_digit(Media, Call) ->
    play_and_collect_digits(1, 1, Media, Call).

play_and_collect_digits(MinDigits, MaxDigits, Media, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, 1,  Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, 3000, Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, 'undefined', Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, <<"\\d+">>, Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, [<<"#">>], Call).
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

b_play_and_collect_digit(Media, Call) ->
    b_play_and_collect_digits(1, 1, Media, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, 3,  Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, ?DEFAULT_COLLECT_TIMEOUT, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, 'undefined', Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, <<"[\\d\\*\\#]+">>, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, ?ANY_DIGIT, Call).

b_play_and_collect_digits(_MinDigits, _MaxDigits, _Media, 0, _Timeout, 'undefined', _Regex, _Terminators, _Call) ->
    {'ok', <<>>};
b_play_and_collect_digits(_MinDigits, _MaxDigits, _Media, 0, _Timeout, MediaInvalid, _Regex, _Terminators, Call) ->
    _ = b_play(MediaInvalid, Call),
    {'ok', <<>>};
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, Call) ->
    NoopId = play(Media, Terminators, Call),
    case collect_digits(MaxDigits, Timeout, ?DEFAULT_INTERDIGIT_TIMEOUT, NoopId, Call) of
        {'ok', Digits} ->
            case re:run(Digits, Regex) of
                {'match', _} when byte_size(Digits) >= MinDigits ->
                    {'ok', Digits};
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to say text to a caller
%% @end
%%--------------------------------------------------------------------
-spec say(ne_binary(), whapps_call:call()) -> 'ok'.
-spec say(ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec say(ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec say(ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.

-spec b_say(ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_say(ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_say(ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_say(ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().

say(Say, Call) ->
    say(Say, <<"name_spelled">>, Call).
say(Say, Type, Call) ->
    say(Say, Type, <<"pronounced">>, Call).
say(Say, Type, Method, Call) ->
    say(Say, Type, Method, <<"en">>, Call).
say(Say, Type, Method, Language,Call) ->
    Command = [{<<"Application-Name">>, <<"say">>}
               ,{<<"Say-Text">>, Say}
               ,{<<"Type">>, Type}
               ,{<<"Method">>, Method}
               ,{<<"Language">>, Language}
              ],
    send_command(Command, Call).

-spec say_command(ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> wh_json:object().
say_command(Say, Type, Method, Language, Call) ->
    CallId = whapps_call:call_id(Call),
    wh_json:from_list([{<<"Application-Name">>, <<"say">>}
                       ,{<<"Say-Text">>, Say}
                       ,{<<"Type">>, Type}
                       ,{<<"Method">>, Method}
                       ,{<<"Language">>, Language}
                       ,{<<"Call-ID">>, CallId}
                      ]).

b_say(Say, Call) ->
    b_say(Say, <<"name_spelled">>, Call).
b_say(Say, Type, Call) ->
    b_say(Say, Type, <<"pronounced">>, Call).
b_say(Say, Type, Method, Call) ->
    b_say(Say, Type, Method, <<"en">>, Call).
b_say(Say, Type, Method, Language, Call) ->
    say(Say, Type, Method, Language, Call),
    wait_for_message(Call, <<"say">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, 'infinity').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to bridge a caller
%% with a conference, with optional entry flags
%% @end
%%--------------------------------------------------------------------
-spec conference(ne_binary(), whapps_call:call()) -> 'ok'.
-spec conference(ne_binary(), boolean(), whapps_call:call()) -> 'ok'.
-spec conference(ne_binary(), boolean(), boolean(), whapps_call:call()) -> 'ok'.
-spec conference(ne_binary(), boolean(), boolean(), boolean(), whapps_call:call()) -> 'ok'.
-spec conference(ne_binary(), boolean(), boolean(), boolean(), boolean(), whapps_call:call()) -> 'ok'.

-spec b_conference(ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_conference(ne_binary(), boolean(), whapps_call:call()) -> whapps_api_std_return().
-spec b_conference(ne_binary(), boolean(), boolean(), whapps_call:call()) -> whapps_api_std_return().
-spec b_conference(ne_binary(), boolean(), boolean(), boolean(), whapps_call:call()) -> whapps_api_std_return().
-spec b_conference(ne_binary(), boolean(), boolean(), boolean(), boolean(), whapps_call:call()) -> whapps_api_std_return().

conference(ConfId, Call) ->
    conference(ConfId, 'false', Call).
conference(ConfId, Mute, Call) ->
    conference(ConfId, Mute, 'false', Call).
conference(ConfId, Mute, Deaf, Call) ->
    conference(ConfId, Mute, Deaf, 'false', Call).
conference(ConfId, Mute, Deaf, Moderator, Call) ->
    conference(ConfId, Mute, Deaf, Moderator, 'false', Call).
conference(ConfId, Mute, Deaf, Moderator, Reinvite, Call) ->
    Command = [{<<"Application-Name">>, <<"conference">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Mute">>, Mute}
               ,{<<"Deaf">>, Deaf}
               ,{<<"Moderator">>, Moderator}
               ,{<<"Reinvite">>, Reinvite}
              ],
    send_command(Command, Call).

b_conference(ConfId, Call) ->
    b_conference(ConfId, 'false', Call).
b_conference(ConfId, Mute, Call) ->
    b_conference(ConfId, Mute, 'false', Call).
b_conference(ConfId, Mute, Deaf, Call) ->
    b_conference(ConfId, Mute, Deaf, 'false', Call).
b_conference(ConfId, Mute, Deaf, Moderator, Call) ->
    b_conference(ConfId, Mute, Deaf, Moderator, 'false', Call).
b_conference(ConfId, Mute, Deaf, Moderator, Reinvite, Call) ->
    conference(ConfId, Mute, Deaf, Moderator, Reinvite, Call),
    wait_for_message(Call, <<"conference">>, <<"CHANNEL_EXECUTE">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to preform a noop
%% @end
%%--------------------------------------------------------------------
-spec noop(whapps_call:call()) -> ne_binary().
-spec b_noop(whapps_call:call()) -> whapps_api_std_return().

noop(Call) ->
    NoopId = couch_mgr:get_uuid(),
    Command = [{<<"Application-Name">>, <<"noop">>}
               ,{<<"Msg-ID">>, NoopId}
              ],
    send_command(Command, Call),
    NoopId.

b_noop(Call) -> wait_for_noop(Call, noop(Call)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to flush the command
%% queue
%% @end
%%--------------------------------------------------------------------
-spec flush(whapps_call:call()) -> binary().
-spec b_flush(whapps_call:call()) -> whapps_api_std_return().

flush(Call) ->
    NoopId = couch_mgr:get_uuid(),
    Command = [{<<"Application-Name">>, <<"noop">>}
               ,{<<"Msg-ID">>, NoopId}
               ,{<<"Insert-At">>, <<"flush">>}
              ],
    send_command(Command, Call),
    NoopId.

b_flush(Call) -> wait_for_noop(Call, flush(Call)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec privacy(whapps_call:call()) -> 'ok'.
-spec privacy(ne_binary(), whapps_call:call()) -> 'ok'.

-spec b_privacy(whapps_call:call()) ->
                       whapps_api_error() |
                       {'ok', wh_json:object()}.
-spec b_privacy(ne_binary(), whapps_call:call()) ->
                       whapps_api_error() |
                       {'ok', wh_json:object()}.

privacy(Call) -> privacy(<<"full">>, Call).
privacy('undefined', Call) -> privacy(Call);
privacy(Mode, Call) ->
    Command = [{<<"Application-Name">>, <<"privacy">>}
               ,{<<"Privacy-Mode">>, Mode}
              ],
    send_command(Command, Call).

b_privacy(Call) -> b_privacy(<<"full">>, Call).
b_privacy('undefined', Call) -> b_privacy(Call);
b_privacy(Mode, Call) ->
    privacy(Mode, Call),
    wait_for_message(Call, <<"privacy">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is intended for use with audio_macro or manually started
%% media playback queued with a NoOp as the final action.  This function
%% will wait forever for the (or any) NoOp event, collecting digits
%% while it does so.  When the NoOp comes in the Timeout timer is started
%% (unless any digit has been pressed in which case the Interdigit
%% timer is used). Once the timer has expired the collected digits are
%% returned (possibly just an empty binary).  However, digits can
%% be returned prior to the timer expiration if the last collected
%% digit is in the list of terminators (no returned if so). Digits
%% can also be returned if the number of collected digits exceeds the
%% MaxDigits.
%%
%% NOTICE: This function should NOT be called if ecallmgr control
%% queue does not have a NoOp queued.  Otherwise this will block
%% execution untill the call is terminated.
%% @end
%%--------------------------------------------------------------------
-type collect_digits_return() :: {'error','channel_hungup' | 'channel_unbridge' | wh_json:object()} |
                                 {'ok', binary()}.
-spec collect_digits(integer() | ne_binary(), whapps_call:call()) -> collect_digits_return().
-spec collect_digits(integer() | ne_binary(), integer() | ne_binary(), whapps_call:call()) -> collect_digits_return().
-spec collect_digits(integer() | ne_binary(), integer() | ne_binary(), integer() | ne_binary(), whapps_call:call()) ->
                            collect_digits_return().
-spec collect_digits(integer() | ne_binary(), integer() | ne_binary(), integer() | ne_binary(), api_binary(), whapps_call:call()) ->
                            collect_digits_return().
-spec collect_digits(integer(), integer(), integer(), api_binary(), list(), whapps_call:call()) ->
                            collect_digits_return().

-record(wcc_collect_digits, {max_digits :: pos_integer()
                             ,timeout = ?DEFAULT_DIGIT_TIMEOUT :: wh_timeout()
                             ,interdigit = ?DEFAULT_INTERDIGIT_TIMEOUT :: pos_integer()
                             ,noop_id :: api_binary()
                             ,terminators = [<<"#">>] :: ne_binaries()
                             ,call :: whapps_call:call()
                             ,digits_collected = <<>> :: binary()
                             ,after_timeout = ?MILLISECONDS_IN_DAY :: pos_integer()
                            }).
-type wcc_collect_digits() :: #wcc_collect_digits{}.

collect_digits(MaxDigits, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=wh_util:to_integer(MaxDigits)
                                          ,call=Call
                                         }).

collect_digits(MaxDigits, Timeout, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=wh_util:to_integer(MaxDigits)
                                          ,timeout=wh_util:to_integer(Timeout)
                                          ,call=Call
                                         }).

collect_digits(MaxDigits, Timeout, Interdigit, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=wh_util:to_integer(MaxDigits)
                                          ,timeout=wh_util:to_integer(Timeout)
                                          ,interdigit=wh_util:to_integer(Interdigit)
                                          ,call=Call
                                         }).

collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=wh_util:to_integer(MaxDigits)
                                          ,timeout=wh_util:to_integer(Timeout)
                                          ,interdigit=wh_util:to_integer(Interdigit)
                                          ,noop_id=NoopId
                                          ,call=Call
                                         }).

collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call) ->
    do_collect_digits(#wcc_collect_digits{max_digits=wh_util:to_integer(MaxDigits)
                                          ,timeout=wh_util:to_integer(Timeout)
                                          ,interdigit=wh_util:to_integer(Interdigit)
                                          ,noop_id=NoopId
                                          ,terminators=Terminators
                                          ,call=Call
                                         }).

-spec do_collect_digits(wcc_collect_digits()) -> collect_digits_return().
do_collect_digits(#wcc_collect_digits{max_digits=MaxDigits
                                      ,timeout=Timeout
                                      ,interdigit=Interdigit
                                      ,noop_id=NoopId
                                      ,terminators=Terminators
                                      ,call=Call
                                      ,digits_collected=Digits
                                      ,after_timeout=After
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
                    do_collect_digits(Collect#wcc_collect_digits{after_timeout=whapps_util:decr_timeout(After, Start)});
                {'ok', Digit} ->
                    %% DTMF received, collect and start interdigit timeout
                    Digits =:= <<>> andalso flush(Call),

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

-spec handle_collect_digit_event(wh_json:object(), api_binary()) ->
                                        {'dtmf', ne_binary()} |
                                        {'noop_complete'} |
                                        {'continue'} |
                                        {'decrement'} |
                                        {'error', _}.
-spec handle_collect_digit_event(wh_json:object(), api_binary(), {ne_binary(), ne_binary(), ne_binary()}) ->
                                        {'dtmf', ne_binary()} |
                                        {'noop_complete'} |
                                        {'continue'} |
                                        {'decrement'} |
                                        {'error', _}.
handle_collect_digit_event(JObj, NoopId) ->
    handle_collect_digit_event(JObj, NoopId, get_event_type(JObj)).

handle_collect_digit_event(_JObj, _NoopId, {<<"call_event">>, <<"CHANNEL_DESTROY">>, _}) ->
    lager:debug("channel was hungup while collecting digits"),
    {'error', 'channel_hungup'};
handle_collect_digit_event(JObj, NoopId, {<<"error">>, _, <<"noop">>}) ->
    case wh_json:get_value([<<"Request">>, <<"Msg-ID">>], JObj, NoopId) of
        NoopId when is_binary(NoopId), NoopId =/= <<>> ->
            lager:debug("channel execution error while collecting digits: ~s"
                        ,[wh_json:get_value(<<"Error-Message">>, JObj)]
                       ),
            {'error', JObj};
        _NID when is_binary(NoopId), NoopId =/= <<>> ->
            {'continue'};
        _NID ->
            lager:debug("channel execution error while collecting digits with noop-id ~s: ~s"
                        ,[NoopId, wh_json:encode(JObj)]
                       ),
            {'error', JObj}
    end;
handle_collect_digit_event(JObj, NoopId, {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>}) ->
    %% Playback completed start timeout
    case wh_json:get_value(<<"Application-Response">>, JObj) of
        NoopId when is_binary(NoopId), NoopId =/= <<>> ->
            {'noop_complete'};
        _NID when is_binary(NoopId), NoopId =/= <<>> ->
            {'continue'};
        _ ->
            {'noop_complete'}
    end;
handle_collect_digit_event(JObj, _NoopId, {<<"call_event">>, <<"DTMF">>, _}) ->
    {'ok', wh_json:get_value(<<"DTMF-Digit">>, JObj, <<>>)};
handle_collect_digit_event(_JObj, _NoopId, _EventType) ->
    {'decrement'}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Low level function to consume call events, looping until a specific
%% one occurs.  If the channel is hungup or no call events are recieved
%% for the optional timeout period then errors are returned.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_message(whapps_call:call(), binary()) ->
                              whapps_api_std_return().
-spec wait_for_message(whapps_call:call(), binary(), ne_binary()) ->
                              whapps_api_std_return().
-spec wait_for_message(whapps_call:call(), binary(), ne_binary(), ne_binary()) ->
                              whapps_api_std_return().
-spec wait_for_message(whapps_call:call(), binary(), ne_binary(), ne_binary(), wh_timeout()) ->
                              whapps_api_std_return().

wait_for_message(Call, Application) ->
    wait_for_message(Call, Application, <<"CHANNEL_EXECUTE_COMPLETE">>).
wait_for_message(Call, Application, Event) ->
    wait_for_message(Call, Application, Event, <<"call_event">>).
wait_for_message(Call, Application, Event, Type) ->
    wait_for_message(Call, Application, Event, Type, ?DEFAULT_MESSAGE_TIMEOUT).

wait_for_message(Call, Application, Event, Type, Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj} ->
            case get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("channel was destroyed while waiting for ~s", [Application]),
                    {'error', 'channel_destroy'};
                {<<"error">>, _, Application} ->
                    lager:debug("channel execution error while waiting for ~s: ~s", [Application, wh_json:encode(JObj)]),
                    {'error', JObj};
                {Type, Event, Application} ->
                    {'ok', JObj};
                _ ->
                    wait_for_message(Call, Application, Event, Type, whapps_util:decr_timeout(Timeout, Start))
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for an application to complete, ignoring channel state.  This
%% is only interested in events for the application.
%% @end
%%--------------------------------------------------------------------
-type wait_for_application_return() :: {'error', 'timeout' | wh_json:object()} |
                                       {'ok', wh_json:object()}.
-spec wait_for_application(whapps_call:call(), ne_binary()) ->
                                  wait_for_application_return().
-spec wait_for_application(whapps_call:call(), ne_binary(), ne_binary()) ->
                                  wait_for_application_return().
-spec wait_for_application(whapps_call:call(), ne_binary(), ne_binary(), ne_binary()) ->
                                  wait_for_application_return().
-spec wait_for_application(whapps_call:call(), ne_binary(), ne_binary(), ne_binary(), wh_timeout()) ->
                                  wait_for_application_return().

wait_for_application(Call, Application) ->
    wait_for_application(Call, Application, <<"CHANNEL_EXECUTE_COMPLETE">>).
wait_for_application(Call, Application, Event) ->
    wait_for_application(Call, Application, Event, <<"call_event">>).
wait_for_application(Call, Application, Event, Type) ->
    wait_for_application(Call, Application, Event, Type, ?DEFAULT_APPLICATION_TIMEOUT).

wait_for_application(Call, Application, Event, Type, Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj} ->
            case get_event_type(JObj) of
                {<<"error">>, _, Application} ->
                    lager:debug("channel execution error while waiting for ~s: ~s", [Application, wh_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("channel was hungup while waiting for ~s", [Application]),
                    {'error', 'channel_hungup'};
                {Type, Event, Application} ->
                    {'ok', JObj};
                _ ->
                    wait_for_application(Call, Application, Event, Type, whapps_util:decr_timeout(Timeout, Start))
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for an application to complete, ignoring channel state.  This
%% is only interested in events for the application.
%% @end
%%--------------------------------------------------------------------
-type wait_for_headless_application_return() :: {'error', 'timeout' | wh_json:object()} |
                                                {'ok', wh_json:object()}.
-spec wait_for_headless_application(ne_binary()) ->
                                           wait_for_headless_application_return().
-spec wait_for_headless_application(ne_binary(), ne_binary()) ->
                                           wait_for_headless_application_return().
-spec wait_for_headless_application(ne_binary(), ne_binary(), ne_binary()) ->
                                           wait_for_headless_application_return().
-spec wait_for_headless_application(ne_binary(), ne_binary(), ne_binary(), wh_timeout()) ->
                                           wait_for_headless_application_return().

wait_for_headless_application(Application) ->
    wait_for_headless_application(Application, <<"CHANNEL_EXECUTE_COMPLETE">>).
wait_for_headless_application(Application, Event) ->
    wait_for_headless_application(Application, Event, <<"call_event">>).
wait_for_headless_application(Application, Event, Type) ->
    wait_for_headless_application(Application, Event, Type, ?DEFAULT_APPLICATION_TIMEOUT).

wait_for_headless_application(Application, Event, Type, Timeout) ->
    Start = os:timestamp(),
    receive
        {'amqp_msg', JObj} ->
            case get_event_type(JObj) of
                {<<"error">>, _, Application} ->
                    lager:debug("channel execution error while waiting for ~s: ~s", [Application, wh_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>,<<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("destroy occurred, waiting 60000 ms for ~s event", [Application]),
                    wait_for_headless_application(Application, Event, Type, 60000);
                { Type, Event, Application } ->
                    {'ok', JObj};
                _T ->
                    lager:debug("ignore ~p", [_T]),
                    wait_for_headless_application(Application, Event, Type, whapps_util:decr_timeout(Timeout, Start))
            end;
        _ ->
            wait_for_headless_application(Application, Event, Type, whapps_util:decr_timeout(Timeout, Start))
    after
        Timeout -> {'error', 'timeout'}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for a DTMF event and extract the digits when it comes
%% @end
%%--------------------------------------------------------------------
-spec wait_for_dtmf(wh_timeout()) ->
                           {'error', 'channel_hungup' | wh_json:object()} |
                           {'ok', binary()}.
wait_for_dtmf(Timeout) ->
    Start = os:timestamp(),
    receive
        {'amqp_msg', JObj} ->
            case whapps_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                    lager:debug("channel was destroyed while waiting for DTMF"),
                    {'error', 'channel_destroy'};
                {<<"error">>, _} ->
                    lager:debug("channel execution error while waiting for DTMF: ~s", [wh_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>, <<"DTMF">>} ->
                    {'ok', wh_json:get_value(<<"DTMF-Digit">>, JObj)};
                _ ->
                    wait_for_dtmf(whapps_util:decr_timeout(Timeout, Start))
            end;
        _E ->
            lager:debug("unexpected ~p", [_E]),
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_dtmf(whapps_util:decr_timeout(Timeout, Start))
    after
        Timeout -> {'ok', <<>>}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Waits for and determines the status of the bridge command
%% @end
%%--------------------------------------------------------------------
-spec wait_for_bridge(wh_timeout(), whapps_call:call()) ->
                             whapps_api_bridge_return().
-spec wait_for_bridge(wh_timeout(), 'undefined' | fun((wh_json:object()) -> any()), whapps_call:call()) ->
                             whapps_api_bridge_return().
wait_for_bridge(Timeout, Call) ->
    wait_for_bridge(Timeout, 'undefined', Call).

wait_for_bridge(Timeout, _, _) when Timeout < 0 ->
    {'error', 'timeout'};
wait_for_bridge(Timeout, Fun, Call) ->
    Start = os:timestamp(),
    receive
        {'amqp_msg', JObj} ->
            Disposition = wh_json:get_value(<<"Disposition">>, JObj),
            Cause = wh_json:get_first_defined([<<"Application-Response">>
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
                    lager:debug("channel execution error while waiting for bridge: ~s", [wh_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>, _} ->
                    CallId = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj),
                    lager:debug("channel bridged to ~s", [CallId]),
                    case is_function(Fun, 1) of
                        'false' -> 'ok';
                        'true' -> Fun(JObj)
                    end,
                    wait_for_bridge('infinity', Fun, Call);
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    %% TODO: reduce log level if no issue is found with
                    %%    basing the Result on Disposition
                    lager:info("bridge completed with result ~s(~s)", [Disposition, Result]),
                    {Result, JObj};
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>} ->
                    %% TODO: reduce log level if no issue is found with
                    %%    basing the Result on Disposition
                    lager:info("bridge completed with result ~s(~s)", [Disposition, Result]),
                    {Result, JObj};
                _ ->
                    wait_for_bridge(whapps_util:decr_timeout(Timeout, Start), Fun, Call)
            end;
        %% dont let the mailbox grow unbounded if
        %%   this process hangs around...
        _ -> wait_for_bridge(whapps_util:decr_timeout(Timeout, Start), Fun, Call)
    after
        Timeout -> {'error', 'timeout'}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for a noop or a specific noop to occur
%% @end
%%--------------------------------------------------------------------
-spec wait_for_noop(whapps_call:call(), api_binary()) -> whapps_api_std_return().
wait_for_noop(Call, NoopId) ->
    case wait_for_message(Call, <<"noop">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, 'infinity') of
        {'ok', JObj}=OK ->
            case wh_json:get_value(<<"Application-Response">>, JObj) of
                NoopId when is_binary(NoopId), NoopId =/= <<>> -> OK;
                _No when is_binary(NoopId), NoopId =/= <<>> -> wait_for_noop(Call, NoopId);
                _ -> OK
            end;
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for a channel to be unbridged from (or destroyed)
%% @end
%%--------------------------------------------------------------------
-spec wait_for_channel_unbridge() -> {'ok', wh_json:object()}.
wait_for_channel_unbridge() ->
    receive
        {'amqp_msg', JObj} ->
            case whapps_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_UNBRIDGE">>} -> {'ok', JObj};
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> {'ok', JObj};
                _ -> wait_for_channel_unbridge()
            end;
        _ -> wait_for_channel_unbridge()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for a channel to be bridged to (or destroyed)
%% @end
%%--------------------------------------------------------------------
-spec wait_for_channel_bridge() -> {'ok', wh_json:object()}.
wait_for_channel_bridge() ->
    receive
        {'amqp_msg', JObj} ->
            case whapps_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>} -> {'ok', JObj};
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} -> {'ok', JObj};
                _ -> wait_for_channel_bridge()
            end;
        _ -> wait_for_channel_bridge()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait forever for the channel to hangup
%% @end
%%--------------------------------------------------------------------
-spec wait_for_hangup() -> {'ok', 'channel_hungup'} |
                           {'error', 'timeout'}.
-spec wait_for_hangup(wh_timeout()) ->
                             {'ok', 'channel_hungup'} |
                             {'error', 'timeout'}.
wait_for_hangup() ->
    wait_for_hangup('infinity').

wait_for_hangup(Timeout) ->
    Start = os:timestamp(),
    receive
        {'amqp_msg', JObj} ->
            case whapps_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                    {'ok', 'channel_hungup'};
                _Evt ->
                    lager:debug("ignoring: ~p", [_Evt]),
                    wait_for_hangup(wh_util:decr_timeout(Timeout, Start))
            end;
        _ -> wait_for_hangup(wh_util:decr_timeout(Timeout, Start))
    after
        Timeout ->
            {'error', 'timeout'}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait forever for the channel to hangup
%% @end
%%--------------------------------------------------------------------
-spec wait_for_unbridge() ->
                               {'ok', 'leg_hungup'} |
                               {'error', 'timeout'}.
-spec wait_for_unbridge(wh_timeout()) ->
                               {'ok', 'leg_hungup'} |
                               {'error', 'timeout'}.
wait_for_unbridge() ->
    wait_for_unbridge('infinity').

wait_for_unbridge(Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj} ->
            case whapps_util:get_event_type(JObj) of
                {<<"call_event">>, <<"LEG_DESTROYED">>} -> {'ok', 'leg_hungup'};
                _ -> wait_for_unbridge(wh_util:decr_timeout(Timeout, Start))
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Waits for and determines the status of the bridge command
%% @end
%%--------------------------------------------------------------------
-spec wait_for_application_or_dtmf(ne_binary(), wh_timeout()) ->
                                          whapps_api_std_return() |
                                          {'dtmf', binary()}.
wait_for_application_or_dtmf(Application, Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj} ->
            case get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("channel was destroyed while waiting for ~s or DTMF", [Application]),
                    {'error', 'channel_destroy'};
                {<<"error">>, _, Application} ->
                    lager:debug("channel execution error while waiting ~s or DTMF: ~s", [Application, wh_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, Application} -> {'ok', JObj};
                {<<"call_event">>, <<"DTMF">>, _} -> {'dtmf', wh_json:get_value(<<"DTMF-Digit">>, JObj)};
                _ ->
                    wait_for_application_or_dtmf(Application, whapps_util:decr_timeout(Timeout, Start))
            end
    end.

-type wait_for_fax_ret() :: {'ok', wh_json:object()} |
                            {'error', 'timeout' | wh_json:object()}.

-define(WAIT_FOR_FAX_TIMEOUT, whapps_config:get_integer(<<"fax">>, <<"wait_for_fax_timeout_ms">>, 3600000)).

-spec wait_for_fax() -> wait_for_fax_ret().
-spec wait_for_fax(wh_timeout()) -> wait_for_fax_ret().
wait_for_fax() -> wait_for_fax(?WAIT_FOR_FAX_TIMEOUT).
wait_for_fax(Timeout) ->
    Start = os:timestamp(),
    case receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj} ->
            case get_event_type(JObj) of
                {<<"error">>, _, <<"receive_fax">>} ->
                    lager:debug("channel execution error while waiting for fax: ~s", [wh_json:encode(JObj)]),
                    {'error', JObj};
                {<<"call_event">>, <<"CHANNEL_EXECUTE">>, <<"receive_fax">>} ->
                    wait_for_fax('infinity');
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"receive_fax">>} ->
                    {'ok', wh_json:set_value(<<"Fax-Success">>, 'true', JObj)};
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    %% NOTE:
                    lager:debug("channel hungup but no end of fax, maybe its coming next..."),
                    wait_for_fax(5000);
                _ -> wait_for_fax(whapps_util:decr_timeout(Timeout, Start))
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait forever for the channel to hangup
%% @end
%%--------------------------------------------------------------------
-spec get_event_type(wh_json:object()) -> {api_binary(), api_binary(), api_binary()}.
get_event_type(JObj) ->
    {C, N} = wh_util:get_event_type(JObj),
    {C, N, get_app(JObj)}.

-spec get_app(wh_json:object()) -> api_binary().
get_app(JObj) ->
    wh_json:get_first_defined([<<"Application-Name">>
                               ,[<<"Request">>, <<"Application-Name">>]
                              ], JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
-spec send_command(api_terms(), whapps_call:call()) -> 'ok'.
send_command(Command, Call) when is_list(Command) ->
    'true' = whapps_call:is_call(Call),

    CustomPublisher = whapps_call:custom_publish_function(Call),
    CtrlQ = whapps_call:control_queue(Call),
    case is_function(CustomPublisher, 2) of
        'true' -> CustomPublisher(Command, Call);
        'false' when is_binary(CtrlQ) ->
            Q = whapps_call:controller_queue(Call),
            CallId = whapps_call:call_id(Call),
            AppName = whapps_call:application_name(Call),
            AppVersion = whapps_call:application_version(Call),
            case whapps_call:kvs_fetch('cf_exe_pid', Call) of
                Pid when is_pid(Pid) -> _ = wh_amqp_channel:consumer_pid(Pid), 'ok';
                _Else -> 'ok'
            end,
            Prop = Command ++ [{<<"Call-ID">>, CallId}
                               | wh_api:default_headers(Q, <<"call">>, <<"command">>, AppName, AppVersion)
                              ],
            wapi_dialplan:publish_command(CtrlQ, props:filter_undefined(Prop));
        'false' -> 'ok'
    end;
send_command(JObj, Call) -> send_command(wh_json:to_proplist(JObj), Call).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the t38 settings for an endpoint based on carrier and device
%% @end
%%--------------------------------------------------------------------
-spec get_outbound_t38_settings(boolean(), api_binary() | boolean()) -> wh_proplist().
get_outbound_t38_settings(CarrierFlag, <<"auto">>) ->
    get_outbound_t38_settings(CarrierFlag, 'true');
get_outbound_t38_settings(CarrierFlag, 'undefined') ->
    get_outbound_t38_settings(CarrierFlag);
get_outbound_t38_settings(CarrierFlag, CallerFlag) when not is_boolean(CallerFlag) ->
    get_outbound_t38_settings(CarrierFlag, wh_util:is_true(CallerFlag));
get_outbound_t38_settings('true', 'true') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
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
    [{<<"Enable-T38-Fax">>, 'undefined'}
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

-spec get_outbound_t38_settings(boolean()) -> wh_proplist().
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

-spec get_inbound_t38_settings(boolean(), api_binary() | boolean()) -> wh_proplist().
get_inbound_t38_settings(CarrierFlag, <<"auto">>) ->
    get_inbound_t38_settings(CarrierFlag, 'true');
get_inbound_t38_settings(CarrierFlag, 'undefined') ->
    get_inbound_t38_settings(CarrierFlag);
get_inbound_t38_settings(CarrierFlag, CallerFlag) when not is_boolean(CallerFlag) ->
    get_inbound_t38_settings(CarrierFlag, wh_util:is_true(CallerFlag));
get_inbound_t38_settings('true', 'true') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
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
    [{<<"Enable-T38-Fax">>, 'undefined'}
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
    ].


-spec get_inbound_t38_settings(boolean()) -> wh_proplist().
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
    ].
