%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
-export([call_status/1, channel_status/1]).
-export([b_call_status/1, b_channel_status/1]).
-export([response/2, response/3, response/4]).

-export([relay_event/2]).

-export([audio_macro/2]).
-export([pickup/2, pickup/3, pickup/4, pickup/5, pickup/6
         ,b_pickup/2, b_pickup/3, b_pickup/4, b_pickup/5, b_pickup/6
        ]).
-export([redirect/3]).
-export([answer/1
         ,hangup/1, hangup/2
         ,set/3, set_terminators/2
         ,fetch/1, fetch/2
        ]).
-export([ring/1]).
-export([receive_fax/1
         ,b_receive_fax/1
        ]).
-export([bridge/2, bridge/3, bridge/4, bridge/5, bridge/6, bridge/7]).
-export([hold/1, hold/2
         ,b_hold/1, b_hold/2, b_hold/3
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
-export([prompt_and_collect_digits/4, prompt_and_collect_digits/5, prompt_and_collect_digits/6,
         prompt_and_collect_digits/7, prompt_and_collect_digits/8, prompt_and_collect_digits/9
        ]).
-export([play_and_collect_digit/2]).
-export([play_and_collect_digits/4, play_and_collect_digits/5, play_and_collect_digits/6,
         play_and_collect_digits/7, play_and_collect_digits/8, play_and_collect_digits/9
        ]).
-export([say/2, say/3, say/4, say/5]).
-export([conference/2, conference/3, conference/4, conference/5]).
-export([noop/1]).
-export([flush/1, flush_dtmf/1]).
-export([privacy/1
         ,privacy/2
        ]).

-export([b_answer/1, b_hangup/1, b_hangup/2, b_fetch/1, b_fetch/2]).
-export([b_ring/1]).
-export([b_bridge/2, b_bridge/3, b_bridge/4, b_bridge/5, b_bridge/6, b_bridge/7]).
-export([b_play/2, b_play/3]).
-export([b_prompt/2, b_prompt/3]).
-export([b_record/2, b_record/3, b_record/4, b_record/5, b_record/6]).
-export([b_store/3, b_store/4, b_store/5
         ,b_store_fax/2
        ]).
-export([b_prompt_and_collect_digit/2]).
-export([b_prompt_and_collect_digits/4, b_prompt_and_collect_digits/5, b_prompt_and_collect_digits/6,
         b_prompt_and_collect_digits/7, b_prompt_and_collect_digits/8, b_prompt_and_collect_digits/9
        ]).
-export([b_play_and_collect_digit/2]).
-export([b_play_and_collect_digits/4, b_play_and_collect_digits/5, b_play_and_collect_digits/6,
         b_play_and_collect_digits/7, b_play_and_collect_digits/8, b_play_and_collect_digits/9
        ]).
-export([b_say/2, b_say/3, b_say/4, b_say/5]).
-export([b_conference/2, b_conference/3, b_conference/4, b_conference/5]).
-export([b_noop/1]).
-export([b_flush/1]).
-export([b_privacy/1
         ,b_privacy/2
        ]).

-export([wait_for_message/1, wait_for_message/2, wait_for_message/3, wait_for_message/4]).
-export([wait_for_application/1, wait_for_application/2, wait_for_application/3, wait_for_application/4]).
-export([wait_for_headless_application/1, wait_for_headless_application/2
         ,wait_for_headless_application/3, wait_for_headless_application/4
        ]).
-export([wait_for_bridge/2, wait_for_bridge/3]).
-export([wait_for_channel_bridge/0, wait_for_channel_unbridge/0]).
-export([wait_for_dtmf/1]).
-export([wait_for_noop/1]).
-export([wait_for_hangup/0, wait_for_unbridge/0]).
-export([wait_for_application_or_dtmf/2]).
-export([collect_digits/2, collect_digits/3, collect_digits/4, collect_digits/5, collect_digits/6]).
-export([send_command/2]).

-type audio_macro_prompt() :: {'play', binary()} | {'play', binary(), [binary(),...]} |
                              {'prompt', binary()} | {'prompt', binary(), binary()} |
                              {'say', binary()} | {'say', binary(), binary()} |
                              {'say', binary(), binary(), binary()} | {'say', binary(), binary(), binary(), binary()} |
                              {'tones', wh_json:objects()} |
                              {'tts', ne_binary()} | {'tts', ne_binary(), ne_binary()} | {'tts', ne_binary(), ne_binary(), ne_binary()}.
-export_type([audio_macro_prompt/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec presence/2 :: (ne_binary(), ne_binary() | whapps_call:call()) -> 'ok'.
-spec presence/3 :: (ne_binary(), ne_binary() | whapps_call:call(), api_binary() | whapps_call:call()) -> 'ok'.

presence(State, PresenceId) when is_binary(PresenceId) ->
    presence(State, PresenceId, undefined);
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
%% Produces the low level wh_api request to get the call status.
%% This request will execute immediately
%% @end
%%--------------------------------------------------------------------
-spec call_status/1 :: (api_binary() | whapps_call:call()) -> 'ok' |
                                                              {'error', 'no_call_id'}.
call_status(undefined) -> {error, no_call_id};
call_status(CallId) when is_binary(CallId) ->
    Command = [{<<"Call-ID">>, CallId}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    wapi_call:publish_call_status_req(CallId, Command);
call_status(Call) ->
    call_status(whapps_call:call_id(Call)).

-spec b_call_status/1 :: (api_binary() | whapps_call:call()) -> whapps_api_std_return().
b_call_status(undefined) -> {error, no_call_id};
b_call_status(CallId) when is_binary(CallId) ->
    Command = [{<<"Call-ID">>, CallId}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Resp = wh_amqp_worker:call(whapps_amqp_pool
                               ,Command
                               ,fun(C) -> wapi_call:publish_call_status_req(CallId, C) end
                               ,fun wapi_call:call_status_resp_v/1
                              ),
    case Resp of
        {error, _}=E -> E;
        {ok, JObj}=Ok ->
            case wh_json:get_value(<<"Status">>, JObj) of
                <<"active">> -> Ok;
                _Else -> {error, JObj}
            end
    end;
b_call_status(Call) ->
    b_call_status(whapps_call:call_id(Call)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to get the channel status.
%% This request will execute immediately
%% @end
%%--------------------------------------------------------------------
-spec channel_status/1 :: (api_binary() | whapps_call:call()) ->
                                  'ok' |
                                  {'error', 'no_channel_id'}.
channel_status(undefined) -> {error, no_channel_id};
channel_status(CallId) when is_binary(CallId) ->
    Command = [{<<"Call-ID">>, CallId}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    wapi_call:publish_channel_status_req(CallId, Command);
channel_status(Call) ->
    channel_status(whapps_call:call_id(Call)).

-spec b_channel_status/1 :: (api_binary() | whapps_call:call()) -> whapps_api_std_return().
b_channel_status(undefined) -> {error, no_channel_id};
b_channel_status(ChannelId) when is_binary(ChannelId) ->
    Command = [{<<"Call-ID">>, ChannelId}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    Resp = wh_amqp_worker:call(whapps_amqp_pool
                               ,Command
                               ,fun(C) -> wapi_call:publish_channel_status_req(ChannelId, C) end
                               ,fun wapi_call:channel_status_resp_v/1),
    case Resp of
        {error, _}=E -> E;
        {ok, JObj}=Ok ->
            case wh_json:get_value(<<"Status">>, JObj) of
                <<"active">> -> Ok;
                _Else -> {error, JObj}
            end
    end;
b_channel_status(Call) ->
    b_channel_status(whapps_call:call_id(Call)).

%%--------------------------------------------------------------------
%% @pubic
%% @doc How amqp messages are sent to the mailboxes of processes waiting
%%      for them in the receive blocks below.
%% @end
%%--------------------------------------------------------------------
-spec relay_event/2 :: (pid(), wh_json:object()) -> any().
relay_event(Pid, JObj) ->
    Pid ! {amqp_msg, JObj}.

-spec audio_macro/2 :: ([audio_macro_prompt(),...], whapps_call:call()) -> ne_binary().
-spec audio_macro/3 :: ([audio_macro_prompt(),...], whapps_call:call(), wh_json:objects()) -> binary().

audio_macro([], Call) ->
    noop(Call);
audio_macro(Prompts, Call) ->
    audio_macro(Prompts, Call, []).

audio_macro([], Call, Queue) ->
    NoopId = couch_mgr:get_uuid(),
    Prompts = [wh_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                  ,{<<"Msg-ID">>, NoopId}
                                  ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                                 ]) | Queue
              ],
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Commands">>, Prompts }
              ],
    send_command(Command, Call),
    NoopId;
audio_macro([{play, MediaName}|T], Call, Queue) ->
    audio_macro(T, Call, [play_command(MediaName, ?ANY_DIGIT, Call) | Queue]);
audio_macro([{play, MediaName, Terminators}|T], Call, Queue) ->
    audio_macro(T, Call, [play_command(MediaName, Terminators, Call) | Queue]);
audio_macro([{prompt, PromptName}|T], Call, Queue) ->
    audio_macro(T, Call, [play_command(whapps_util:get_prompt(PromptName, Call), ?ANY_DIGIT, Call) | Queue]);
audio_macro([{prompt, PromptName, Lang}|T], Call, Queue) ->
    audio_macro(T, Call, [play_command(whapps_util:get_prompt(PromptName, Lang, Call), ?ANY_DIGIT, Call) | Queue]);
audio_macro([{say, Say}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, <<"name_spelled">>, <<"pronounced">>, <<"en">>, Call) | Queue]);
audio_macro([{say, Say, Type}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, Type, <<"pronounced">>, <<"en">>, Call) | Queue]);
audio_macro([{say, Say, Type, Method}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, Type, Method, <<"en">>, Call) | Queue]);
audio_macro([{say, Say, Type, Method, Language}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, Type, Method, Language, Call) | Queue]);
audio_macro([{tones, Tones}|T], Call, Queue) ->
    audio_macro(T, Call, [tones_command(Tones, Call) | Queue]);
audio_macro([{tts, Text}|T], Call, Queue) ->
    audio_macro(T, Call, [tts_command(Text, Call) | Queue]);
audio_macro([{tts, Text, Voice}|T], Call, Queue) ->
    audio_macro(T, Call, [tts_command(Text, Voice, Call) | Queue]);
audio_macro([{tts, Text, Voice, Lang}|T], Call, Queue) ->
    audio_macro(T, Call, [tts_command(Text, Voice, Lang, Call) | Queue]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec response/2 :: (ne_binary(), whapps_call:call()) -> {'ok', ne_binary()} |
                                                         {'error', 'no_response'}.
-spec response/3 :: (ne_binary(), api_binary(), whapps_call:call()) -> {'ok', ne_binary()} |
                                                                       {'error', 'no_response'}.
-spec response/4 :: (ne_binary(), 'undefined' | binary(), 'undefined' | binary(), whapps_call:call()) -> {'ok', ne_binary()} |
                                                                                                         {'error', 'no_response'}.

response(Code, Call) ->
    response(Code, undefined, Call).
response(Code, Cause, Call) ->
    response(Code, Cause, undefined, Call).
response(Code, Cause, Media, Call) ->
    CallId = whapps_call:call_id(Call),
    CtrlQ = whapps_call:control_queue(Call),
    wh_call_response:send(CallId, CtrlQ, Code, Cause, Media).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec pickup/2 :: (ne_binary(), whapps_call:call()) -> 'ok'.
-spec pickup/3 :: (ne_binary(), api_binary(), whapps_call:call()) -> 'ok'.
-spec pickup/4 :: (ne_binary(), api_binary(), api_binary() | boolean(), whapps_call:call()) -> 'ok'.
-spec pickup/5 :: (ne_binary(), api_binary(), api_binary() | boolean(), api_binary() | boolean(), whapps_call:call()) -> 'ok'.
-spec pickup/6 :: (ne_binary(), api_binary(), api_binary() | boolean(), api_binary() | boolean(), api_binary() | boolean(), whapps_call:call()) -> 'ok'.
pickup(TargetCallId, Call) ->
    pickup(TargetCallId, undefined, Call).
pickup(TargetCallId, Insert, Call) ->
    pickup(TargetCallId, Insert, undefined, Call).
pickup(TargetCallId, Insert, ContinueOnFail, Call) ->
    pickup(TargetCallId, Insert, ContinueOnFail, undefined, Call).
pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, Call) ->
    pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, undefined, Call).
pickup(TargetCallId, Insert, ContinueOnFail, ContinueOnCancel, ParkAfterPickup, Call) ->
    Command = [{<<"Application-Name">>, <<"call_pickup">>}
               ,{<<"Target-Call-ID">>, TargetCallId}
               ,{<<"Insert-At">>, Insert}
               ,{<<"Continue-On-Fail">>, ContinueOnFail}
               ,{<<"Continue-On-Cancel">>, ContinueOnCancel}
               ,{<<"Park-After-Pickup">>, ParkAfterPickup}
              ],
    send_command(Command, Call).

-spec b_pickup/2 :: (ne_binary(), whapps_call:call()) -> {'ok', wh_json:object()}.
-spec b_pickup/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> {'ok', wh_json:object()}.
-spec b_pickup/4 :: (ne_binary(), ne_binary(), ne_binary() | boolean(), whapps_call:call()) -> {'ok', wh_json:object()}.
-spec b_pickup/5 :: (ne_binary(), ne_binary(), ne_binary() | boolean(), ne_binary() | boolean(), whapps_call:call()) -> {'ok', wh_json:object()}.
-spec b_pickup/6 :: (ne_binary(), ne_binary(), ne_binary() | boolean(), ne_binary() | boolean(), ne_binary() | boolean(), whapps_call:call()) -> {'ok', wh_json:object()}.
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
-spec redirect/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
redirect(Contact, Server, Call) ->
    lager:debug("redirect to ~s on ~s", [Contact, Server]),
    Command = [{<<"Redirect-Contact">>, Contact}
               ,{<<"Redirect-Server">>, Server}
               ,{<<"Application-Name">>, <<"redirect">>}
              ],
    send_command(Command, Call),
    timer:sleep(2000),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec flush_dtmf/1 :: (whapps_call:call()) -> ne_binary().
flush_dtmf(Call) ->
    play(<<"silence_stream://50">>, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to set channel/call vars
%% NOTICE: These are 'custom' channel vars for state info only, and
%%   can not be used to set system settings
%% @end
%%--------------------------------------------------------------------
-spec set/3 :: ('undefined' | wh_json:object(), 'undefined' | wh_json:object(), whapps_call:call()) -> 'ok'.
set(undefined, CallVars, Call) ->
    set(wh_json:new(), CallVars, Call);
set(ChannelVars, undefined, Call) ->
    set(ChannelVars, wh_json:new(), Call);
set(ChannelVars, CallVars, Call) ->
    case wh_json:is_empty(ChannelVars) andalso wh_json:is_empty(CallVars) of
        true -> ok;
        false ->
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
-spec fetch/1 :: (whapps_call:call()) -> 'ok'.
-spec fetch/2 :: (boolean(), whapps_call:call()) -> 'ok'.

-spec b_fetch/1 :: (whapps_call:call()) -> whapps_api_std_return().
-spec b_fetch/2 :: (boolean(), whapps_call:call()) -> whapps_api_std_return().

fetch(Call) ->
    fetch(false, Call).
fetch(FromOtherLeg, Call) ->
    Command = [{<<"Application-Name">>, <<"fetch">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"From-Other-Leg">>, FromOtherLeg}
              ],
    send_command(Command, Call).

b_fetch(Call) ->
    b_fetch(false, Call).
b_fetch(FromOtherLeg, Call) ->
    fetch(FromOtherLeg, Call),
    case wait_for_message(<<"fetch">>) of
        {ok, JObj} ->
            {ok, wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())};
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to ring the channel
%% @end
%%--------------------------------------------------------------------
-spec ring/1 :: (whapps_call:call()) -> 'ok'.
-spec b_ring/1 :: (whapps_call:call()) -> whapps_api_error() | {'ok', wh_json:object()}.

ring(Call) ->
    Command = [{<<"Application-Name">>, <<"ring">>}],
    send_command(Command, Call).

b_ring(Call) ->
    ring(Call),
    wait_for_message(<<"ring">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Instructs the switch to expect to receive a fax
%% @end
%%--------------------------------------------------------------------
-spec receive_fax/1 :: (whapps_call:call()) -> 'ok'.
-spec b_receive_fax/1 :: (whapps_call:call()) -> whapps_api_error() |
                                                 {'ok', wh_json:object()}.

receive_fax(Call) ->
    Command = [{<<"Application-Name">>, <<"receive_fax">>}],
    send_command(Command, Call).

b_receive_fax(Call) ->
    receive_fax(Call),
    wait_for_fax().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to answer the channel
%% @end
%%--------------------------------------------------------------------
-spec answer/1 :: (whapps_call:call()) -> 'ok'.
-spec b_answer/1 :: (whapps_call:call()) -> whapps_api_error() | {'ok', wh_json:object()}.

answer(Call) ->
    Command = [{<<"Application-Name">>, <<"answer">>}],
    send_command(Command, Call).

b_answer(Call) ->
    answer(Call),
    wait_for_message(<<"answer">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to hangup the channel.
%% This request will execute immediately
%% @end
%%--------------------------------------------------------------------
-spec hangup/1 :: (whapps_call:call()) -> 'ok'.
-spec hangup/2 :: (boolean(), whapps_call:call()) -> 'ok'.

-spec b_hangup/1 :: (whapps_call:call()) -> {'ok', 'channel_hungup'}.
-spec b_hangup/2 :: (boolean(), whapps_call:call()) -> {'ok', 'channel_hungup' | 'leg_hangup'}.

hangup(Call) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
               ,{<<"Insert-At">>, <<"now">>}
              ],
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
b_hangup(false, Call) ->
    hangup(Call),
    wait_for_hangup();
b_hangup(true, Call) ->
    hangup(true, Call),
    wait_for_unbridge().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to bridge the call
%% @end
%%--------------------------------------------------------------------
-spec bridge/2 :: (wh_json:objects(), whapps_call:call()) -> 'ok'.
-spec bridge/3 :: (wh_json:objects(), whapps_api_binary(), whapps_call:call()) -> 'ok'.
-spec bridge/4 :: (wh_json:objects(), whapps_api_binary(), whapps_api_binary(), whapps_call:call()) -> 'ok'.
-spec bridge/5 :: (wh_json:objects(), whapps_api_binary(), whapps_api_binary(), whapps_api_binary(), whapps_call:call()) -> 'ok'.
-spec bridge/6 :: (wh_json:objects(), whapps_api_binary(), whapps_api_binary(), whapps_api_binary(), whapps_api_binary(), whapps_call:call()) -> 'ok'.
-spec bridge/7 :: (wh_json:objects(), whapps_api_binary(), whapps_api_binary(), whapps_api_binary(), whapps_api_binary(), wh_json:object() | 'undefined', whapps_call:call()) -> 'ok'.

-spec b_bridge/2 :: (wh_json:objects(), whapps_call:call()) -> whapps_api_bridge_return().
-spec b_bridge/3 :: (wh_json:objects(), whapps_api_binary(), whapps_call:call()) -> whapps_api_bridge_return().
-spec b_bridge/4 :: (wh_json:objects(), whapps_api_binary(), whapps_api_binary(), whapps_call:call()) -> whapps_api_bridge_return().
-spec b_bridge/5 :: (wh_json:objects(), whapps_api_binary(), whapps_api_binary(), whapps_api_binary(), whapps_call:call()) -> whapps_api_bridge_return().
-spec b_bridge/6 :: (wh_json:objects(), whapps_api_binary(), whapps_api_binary(), whapps_api_binary(), whapps_api_binary(), whapps_call:call()) 
                    -> whapps_api_bridge_return().
-spec b_bridge/7 :: (wh_json:objects(), whapps_api_binary(), whapps_api_binary(), whapps_api_binary(), whapps_api_binary(), wh_json:object() | 'undefined', whapps_call:call())
                    -> whapps_api_bridge_return().

bridge(Endpoints, Call) ->
    bridge(Endpoints, ?DEFAULT_TIMEOUT, Call).
bridge(Endpoints, Timeout, Call) ->
    bridge(Endpoints, Timeout, wapi_dialplan:dial_method_single(), Call).
bridge(Endpoints, Timeout, Strategy, Call) ->
    bridge(Endpoints, Timeout, Strategy, <<"false">>, Call).
bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, undefined, Call).
bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, undefined, Call).
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
    b_bridge(Endpoints, ?DEFAULT_TIMEOUT, Call).
b_bridge(Endpoints, Timeout, Call) ->
    b_bridge(Endpoints, Timeout, wapi_dialplan:dial_method_single(), Call).
b_bridge(Endpoints, Timeout, Strategy, Call) ->
    b_bridge(Endpoints, Timeout, Strategy, <<"false">>, Call).
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Call) ->
    b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, undefined, Call).
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, Call) ->
    b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, undefined, Call).
b_bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, Call) ->
    bridge(Endpoints, Timeout, Strategy, IgnoreEarlyMedia, Ringback, SIPHeaders, Call),
    case wait_for_bridge((wh_util:to_integer(Timeout)*1000) + 10000, Call) of
        {ok, _}=Ok ->
            Ok;
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to park the channel
%% @end
%%--------------------------------------------------------------------
-spec hold/1 :: (whapps_call:call()) -> 'ok'.
-spec hold/2 :: (api_binary(), whapps_call:call()) -> 'ok'.

-spec b_hold/1 :: (whapps_call:call()) -> whapps_api_std_return().
-spec b_hold/2 :: (wh_timeout() | api_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_hold/3 :: (wh_timeout(), api_binary(), whapps_call:call()) -> whapps_api_std_return().

hold(Call) ->
    hold(undefined, Call).
hold(MOH, Call) ->
    Command = [{<<"Application-Name">>, <<"hold">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Hold-Media">>, MOH}
              ],
    send_command(Command, Call).

b_hold(Call) ->
    b_hold(infinity, undefined, Call).
b_hold(Timeout, Call) when is_integer(Timeout) orelse Timeout =:= infinity ->
    b_hold(Timeout, undefined, Call);
b_hold(MOH, Call) ->
    b_hold(infinity, MOH, Call).
b_hold(Timeout, MOH, Call) ->
    hold(MOH, Call),
    wait_for_message(<<"hold">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, Timeout).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to play media to the
%% caller.
%% @end
%%--------------------------------------------------------------------
-spec prompt/2 :: (ne_binary(), whapps_call:call()) -> ne_binary().
-spec prompt/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> ne_binary().

-spec b_prompt/2 :: (ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_prompt/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().

prompt(Prompt, Call) ->
    prompt(Prompt, <<"en">>, Call).

prompt(Prompt, Lang, Call) ->
    play(whapps_util:get_prompt(Prompt, Lang, Call), Call).

b_prompt(Prompt, Call) ->
    b_prompt(Prompt, <<"en">>, Call).

b_prompt(Prompt, Lang, Call) ->
    b_play(whapps_util:get_prompt(Prompt, Lang, Call), Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to play media to the
%% caller.  A list of terminators can be provided that the caller
%% can use to skip playback.
%% @end
%%--------------------------------------------------------------------
-spec play/2 :: (ne_binary(), whapps_call:call()) -> ne_binary().
-spec play/3 :: (ne_binary(), [ne_binary(),...], whapps_call:call()) -> ne_binary().

-spec b_play/2 :: (ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_play/3 :: (ne_binary(), [ne_binary(),...], whapps_call:call()) -> whapps_api_std_return().

play(Media, Call) ->
    play(Media, ?ANY_DIGIT, Call).
play(Media, Terminators, Call) ->
    NoopId = couch_mgr:get_uuid(),
    CallId = whapps_call:call_id(Call),

    Commands = [wh_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                   ,{<<"Call-ID">>, CallId}
                                   ,{<<"Msg-ID">>, NoopId}
                                  ])
                ,wh_json:from_list([{<<"Application-Name">>, <<"play">>}
                                    ,{<<"Media-Name">>, Media}
                                    ,{<<"Terminators">>, Terminators}
                                    ,{<<"Call-ID">>, CallId}
                                   ])
               ],
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Commands">>, Commands}
              ],
    send_command(Command, Call),
    NoopId.

b_play(Media, Call) ->
    b_play(Media, ?ANY_DIGIT, Call).
b_play(Media, Terminators, Call) ->
    wait_for_noop(play(Media, Terminators, Call)).

-spec play_command/3 :: (ne_binary(), [ne_binary(),...], whapps_call:call()) -> wh_json:object().
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
-spec tts/2 :: (api_binary(), whapps_call:call()) -> ne_binary().
-spec tts/3 :: (api_binary(), api_binary(), whapps_call:call()) -> ne_binary().
-spec tts/4 :: (api_binary(), api_binary(), api_binary(), whapps_call:call()) -> ne_binary().
-spec tts/5 :: (api_binary(), api_binary(), api_binary(), api_binaries(), whapps_call:call()) -> ne_binary().
-spec tts/6 :: (api_binary(), api_binary(), api_binary(), api_binaries(), api_binary(), whapps_call:call()) -> ne_binary().

tts(SayMe, Call) -> tts(SayMe, <<"female">>, Call).
tts(SayMe, Voice, Call) -> tts(SayMe, Voice, <<"en-US">>, Call).
tts(SayMe, Voice, Lang, Call) -> tts(SayMe, Voice, Lang, ?ANY_DIGIT, Call).
tts(SayMe, Voice, Lang, Terminators, Call) ->
    tts(SayMe, Voice, Lang, Terminators
        ,whapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_provider">>, <<"flite">>)
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

-spec tts_command/2 :: (api_binary(), whapps_call:call()) -> wh_json:object().
-spec tts_command/3 :: (api_binary(), api_binary(), whapps_call:call()) -> wh_json:object().
-spec tts_command/4 :: (api_binary(), api_binary(), api_binary(), whapps_call:call()) -> wh_json:object().
-spec tts_command/5 :: (api_binary(), api_binary(), api_binary(), api_binaries(), whapps_call:call()) -> wh_json:object().
-spec tts_command/6 :: (api_binary(), api_binary(), api_binary(), api_binaries(), api_binary(), whapps_call:call()) -> wh_json:object().
tts_command(SayMe, Call) -> tts_command(SayMe, <<"female">>, Call).
tts_command(SayMe, Voice, Call) -> tts_command(SayMe, Voice, <<"en-US">>, Call).
tts_command(SayMe, Voice, Lang, Call) -> tts_command(SayMe, Voice, Lang, ?ANY_DIGIT, Call).
tts_command(SayMe, Voice, Lang, Terminators, Call) ->
    tts_command(SayMe, Voice, Lang, Terminators
                ,whapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_provider">>, <<"flite">>)
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

-spec b_tts/2 :: (api_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_tts/3 :: (api_binary(), api_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_tts/4 :: (api_binary(), api_binary(), api_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_tts/5 :: (api_binary(), api_binary(), api_binary(), api_binaries(), whapps_call:call()) -> whapps_api_std_return().
-spec b_tts/6 :: (api_binary(), api_binary(), api_binary(), api_binaries(), api_binary(), whapps_call:call()) -> whapps_api_std_return().

b_tts(SayMe, Call) -> wait_for_noop(tts(SayMe, Call)).
b_tts(SayMe, Voice, Call) -> wait_for_noop(tts(SayMe, Voice, Call)).
b_tts(SayMe, Voice, Lang, Call) -> wait_for_noop(tts(SayMe, Voice, Lang, Call)).
b_tts(SayMe, Voice, Lang, Terminators, Call) -> wait_for_noop(tts(SayMe, Voice, Lang, Terminators, Call)).
b_tts(SayMe, Voice, Lang, Terminators, Engine, Call) -> wait_for_noop(tts(SayMe, Voice, Lang, Terminators, Engine, Call)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to record a file.
%% A list of keys can be used as the terminator or a silence threshold.
%% @end
%%--------------------------------------------------------------------
-spec record/2 :: (ne_binary(), whapps_call:call()) -> 'ok'.
-spec record/3 :: (ne_binary(), [binary(),...], whapps_call:call()) -> 'ok'.
-spec record/4 :: (ne_binary(), [binary(),...],  whapps_api_binary() | integer(), whapps_call:call()) -> 'ok'.
-spec record/5 :: (ne_binary(), [binary(),...],  whapps_api_binary() | integer(), whapps_api_binary() | integer(), whapps_call:call()) -> 'ok'.
-spec record/6 :: (ne_binary(), [binary(),...],  whapps_api_binary() | integer(), whapps_api_binary() | integer(),  whapps_api_binary() | integer(), whapps_call:call()) -> 'ok'.

-spec b_record/2 :: (ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_record/3 :: (ne_binary(), [binary(),...], whapps_call:call()) -> whapps_api_std_return().
-spec b_record/4 :: (ne_binary(), [binary(),...], whapps_api_binary() | integer(), whapps_call:call()) -> whapps_api_std_return().
-spec b_record/5 :: (ne_binary(), [binary(),...], whapps_api_binary() | integer(), whapps_api_binary() | integer(), whapps_call:call()) -> whapps_api_std_return().
-spec b_record/6 :: (ne_binary(), [binary(),...], whapps_api_binary() | integer(), whapps_api_binary() | integer(), whapps_api_binary() | integer(), whapps_call:call()) -> whapps_api_std_return().

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
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, infinity).

-spec record_call/2 :: (ne_binary(), whapps_call:call()) -> 'ok'.
-spec record_call/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec record_call/4 :: (ne_binary(), ne_binary(),  whapps_api_binary() | pos_integer(), whapps_call:call()) -> 'ok'.
-spec record_call/5 :: (ne_binary(), ne_binary(),  whapps_api_binary() | pos_integer(), list(), whapps_call:call()) -> 'ok'.
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

-spec b_record_call/2 :: (ne_binary(), whapps_call:call()) ->
                                 wait_for_headless_application_return().
-spec b_record_call/3 :: (ne_binary(), ne_binary(), whapps_call:call()) ->
                                 wait_for_headless_application_return().
-spec b_record_call/4 :: (ne_binary(), ne_binary(), whapps_api_binary() | pos_integer(), whapps_call:call()) ->
                                 wait_for_headless_application_return().
-spec b_record_call/5 :: (ne_binary(), ne_binary(), whapps_api_binary() | pos_integer(), list(), whapps_call:call()) ->
                                 wait_for_headless_application_return().
b_record_call(MediaName, Call) ->
    record_call(MediaName, Call),
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, infinity).
b_record_call(MediaName, Action, Call) ->
    record_call(MediaName, Action, Call),
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, infinity).
b_record_call(MediaName, Action, TimeLimit, Call) ->
    b_record_call(MediaName, Action, TimeLimit, ?ANY_DIGIT, Call).
b_record_call(MediaName, Action, TimeLimit, Terminators, Call) ->
    record_call(MediaName, Action, TimeLimit, Terminators, Call),
    wait_for_headless_application(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, infinity).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to store the file
%% @end
%%--------------------------------------------------------------------
-type b_store_return() :: {'error', 'timeout' | wh_json:object()} | {'ok', wh_json:object()}.

-spec store/3 :: (ne_binary(), whapps_api_binary(), whapps_call:call()) -> 'ok'.
-spec store/4 :: (ne_binary(), whapps_api_binary(), whapps_api_binary(), whapps_call:call()) -> 'ok'.
-spec store/5 :: (ne_binary(), whapps_api_binary(), whapps_api_binary(), wh_json:objects(), whapps_call:call()) -> 'ok'.

-spec b_store/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> b_store_return().
-spec b_store/4 :: (ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> b_store_return().
-spec b_store/5 :: (ne_binary(), ne_binary(), ne_binary(), wh_json:objects(), whapps_call:call()) -> b_store_return().

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
-spec store_fax/2 :: (ne_binary(), whapps_call:call()) -> 'ok'.
store_fax(URL, Call) ->
    Command = [{<<"Application-Name">>, <<"store_fax">>}
               ,{<<"Media-Transfer-Method">>, <<"put">>}
               ,{<<"Media-Transfer-Destination">>, URL}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    send_command(Command, Call).

-spec b_store_fax/2 :: (ne_binary(), whapps_call:call()) -> b_store_return().
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
-spec tones/2 :: (wh_json:objects(), whapps_call:call()) -> 'ok'.
tones(Tones, Call) ->
    Command = [{<<"Application-Name">>, <<"tones">>}
               ,{<<"Tones">>, Tones}
              ],
    send_command(Command, Call).

-spec tones_command/2 :: (wh_json:objects(), whapps_call:call()) -> wh_json:object().
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
-spec prompt_and_collect_digit/2 :: (ne_binary(), whapps_call:call()) -> 'ok'.
-spec prompt_and_collect_digits/4 :: (ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec prompt_and_collect_digits/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec prompt_and_collect_digits/6 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec prompt_and_collect_digits/7 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), whapps_call:call()) -> 'ok'.
-spec prompt_and_collect_digits/8 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), ne_binary()
                                    ,whapps_call:call()) -> ok.
-spec prompt_and_collect_digits/9 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), ne_binary()
                                    ,[ne_binary(),...], whapps_call:call()) -> 'ok'.


-spec b_prompt_and_collect_digit/2 :: (ne_binary(), whapps_call:call()) -> b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits/4 :: (ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits/6 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) 
                                     -> b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits/7 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), whapps_call:call()) 
                                     -> b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits/8 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), ne_binary()
                                      ,whapps_call:call()) -> b_play_and_collect_digits_return().
-spec b_prompt_and_collect_digits/9 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), ne_binary()
                                      ,[ne_binary(),...], whapps_call:call()) -> b_play_and_collect_digits_return().

prompt_and_collect_digit(Prompt, Call) ->
    prompt_and_collect_digits(<<"1">>, <<"1">>, Prompt, Call).

prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, <<"1">>,  Call).
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, <<"3000">>, Call).
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, undefined, Call).
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, <<"\\d+">>, Call).
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Call) ->
    prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, [<<"#">>], Call).
prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Terminators, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, whapps_util:get_prompt(Prompt, Call), Tries, Timeout, InvalidPrompt, Regex, Terminators, Call).

b_prompt_and_collect_digit(Prompt, Call) ->
    b_prompt_and_collect_digits(<<"1">>, <<"1">>, Prompt, Call).
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, <<"3">>,  Call).
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, <<"5000">>, Call).
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, undefined, Call).
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, <<"\\d+">>, Call).
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Call) ->
    b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, ?ANY_DIGIT, Call).

b_prompt_and_collect_digits(_MinDigits, _MaxDigits, _Prompt, <<"0">>, _Timeout, undefined, _Regex, _Terminators, _Call) ->
    {ok, <<>>};
b_prompt_and_collect_digits(_MinDigits, _MaxDigits, _Prompt, <<"0">>, _Timeout, InvalidPrompt, _Regex, _Terminators, Call) ->
    _ = b_prompt(InvalidPrompt, Call),
    {ok, <<>>};
b_prompt_and_collect_digits(MinDigits, MaxDigits, Prompt, Tries, Timeout, InvalidPrompt, Regex, Terminators, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, whapps_util:get_prompt(Prompt, Call), Tries, Timeout, InvalidPrompt, Regex, Terminators, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to play media to a
%% caller, and collect a number of DTMF events.
%% @end
%%--------------------------------------------------------------------
-type b_play_and_collect_digits_return() :: {'error', 'channel_hungup' | 'channel_unbridge' | wh_json:object()} | {'ok', binary()}.

-spec play_and_collect_digit/2 :: (ne_binary(), whapps_call:call()) -> 'ok'.
-spec play_and_collect_digits/4 :: (ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec play_and_collect_digits/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec play_and_collect_digits/6 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec play_and_collect_digits/7 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), whapps_call:call()) -> 'ok'.
-spec play_and_collect_digits/8 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), ne_binary()
                                    ,whapps_call:call()) -> 'ok'.
-spec play_and_collect_digits/9 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), ne_binary()
                                    ,[ne_binary(),...], whapps_call:call()) -> 'ok'.


-spec b_play_and_collect_digit/2 :: (ne_binary(), whapps_call:call()) -> b_play_and_collect_digits_return().
-spec b_play_and_collect_digits/4 :: (ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> b_play_and_collect_digits_return().
-spec b_play_and_collect_digits/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> b_play_and_collect_digits_return().
-spec b_play_and_collect_digits/6 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) 
                                     -> b_play_and_collect_digits_return().
-spec b_play_and_collect_digits/7 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), whapps_call:call()) 
                                     -> b_play_and_collect_digits_return().
-spec b_play_and_collect_digits/8 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), ne_binary()
                                      ,whapps_call:call()) -> b_play_and_collect_digits_return().
-spec b_play_and_collect_digits/9 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_api_binary(), ne_binary()
                                      ,[ne_binary(),...], whapps_call:call()) -> b_play_and_collect_digits_return().

play_and_collect_digit(Media, Call) ->
    play_and_collect_digits(<<"1">>, <<"1">>, Media, Call).

play_and_collect_digits(MinDigits, MaxDigits, Media, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, <<"1">>,  Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, <<"3000">>, Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, undefined, Call).
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
    b_play_and_collect_digits(<<"1">>, <<"1">>, Media, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, <<"3">>,  Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, <<"5000">>, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, undefined, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, <<"[\\d\\*\\#]+">>, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, ?ANY_DIGIT, Call).

b_play_and_collect_digits(_MinDigits, _MaxDigits, _Media, <<"0">>, _Timeout, undefined, _Regex, _Terminators, _Call) ->
    {ok, <<>>};
b_play_and_collect_digits(_MinDigits, _MaxDigits, _Media, <<"0">>, _Timeout, MediaInvalid, _Regex, _Terminators, Call) ->
    _ = b_play(MediaInvalid, Call),
    {ok, <<>>};
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, Call) ->
    NoopId = play(Media, Terminators, Call),
    case collect_digits(MaxDigits, Timeout, <<"2000">>, NoopId, Call) of
        {ok, Digits} ->
            MinSize = wh_util:to_integer(MinDigits),
            case re:run(Digits, Regex) of
                {match, _} when byte_size(Digits) >= MinSize ->
                    {ok, Digits};
                _ ->
                    RemainingTries = wh_util:to_binary(wh_util:to_integer(Tries) - 1),
                    b_play_and_collect_digits(MinDigits, MaxDigits, Media, RemainingTries
                                              ,Timeout, MediaInvalid, Regex, Terminators, Call)
            end;
        {error, _}=Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to say text to a caller
%% @end
%%--------------------------------------------------------------------
-spec say/2 :: (ne_binary(), whapps_call:call()) -> 'ok'.
-spec say/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec say/4 :: (ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec say/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.

-spec b_say/2 :: (ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_say/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_say/4 :: (ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_say/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().

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

-spec say_command/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> wh_json:object().
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
    wait_for_message(<<"say">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, infinity).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to bridge a caller
%% with a conference, with optional entry flags
%% @end
%%--------------------------------------------------------------------
-spec conference/2 :: (ne_binary(), whapps_call:call()) -> 'ok'.
-spec conference/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec conference/4 :: (ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
-spec conference/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.

-spec b_conference/2 :: (ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_conference/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_conference/4 :: (ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().
-spec b_conference/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) -> whapps_api_std_return().

conference(ConfId, Call) ->
    conference(ConfId, <<"false">>, Call).
conference(ConfId, Mute, Call) ->
    conference(ConfId, Mute, <<"false">>, Call).
conference(ConfId, Mute, Deaf, Call) ->
    conference(ConfId, Mute, Deaf, <<"false">>, Call).
conference(ConfId, Mute, Deaf, Moderator, Call) ->
    Command = [{<<"Application-Name">>, <<"conference">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Mute">>, Mute}
               ,{<<"Deaf">>, Deaf}
               ,{<<"Moderator">>, Moderator}
              ],
    send_command(Command, Call).

b_conference(ConfId, Call) ->
    b_conference(ConfId, <<"false">>, Call).
b_conference(ConfId, Mute, Call) ->
    b_conference(ConfId, Mute, <<"false">>, Call).
b_conference(ConfId, Mute, Deaf, Call) ->
    b_conference(ConfId, Mute, Deaf, <<"false">>, Call).
b_conference(ConfId, Mute, Deaf, Moderator, Call) ->
    conference(ConfId, Mute, Deaf, Moderator, Call),
    wait_for_message(<<"conference">>, <<"CHANNEL_EXECUTE">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to preform a noop
%% @end
%%--------------------------------------------------------------------
-spec noop/1 :: (whapps_call:call()) -> ne_binary().
-spec b_noop/1 :: (whapps_call:call()) -> whapps_api_std_return().

noop(Call) ->
    NoopId = couch_mgr:get_uuid(),
    Command = [{<<"Application-Name">>, <<"noop">>}
               ,{<<"Msg-ID">>, NoopId}
              ],
    send_command(Command, Call),
    NoopId.

b_noop(Call) ->
    wait_for_noop(noop(Call)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to flush the command
%% queue
%% @end
%%--------------------------------------------------------------------
-spec flush/1 :: (whapps_call:call()) -> binary().
-spec b_flush/1 :: (whapps_call:call()) -> whapps_api_std_return().

flush(Call) ->
    NoopId = couch_mgr:get_uuid(),
    Command = [{<<"Application-Name">>, <<"noop">>}
               ,{<<"Msg-ID">>, NoopId}
               ,{<<"Insert-At">>, <<"flush">>}
              ],
    send_command(Command, Call),
    NoopId.

b_flush(Call) ->
    wait_for_noop(flush(Call)).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec privacy/1 ::  (whapps_call:call()) -> 'ok'.
-spec b_privacy/1 :: (whapps_call:call()) -> whapps_api_error() | {'ok', wh_json:object()}.
-spec privacy/2 :: (ne_binary(), whapps_call:call()) -> 'ok'.
-spec b_privacy/2 :: (ne_binary(), whapps_call:call()) -> whapps_api_error() | {'ok', wh_json:object()}.

privacy(Call) ->
    privacy(<<"full">>, Call).
privacy(undefined, Call) ->
    privacy(Call);
privacy(Mode, Call) ->
    Command = [{<<"Application-Name">>, <<"privacy">>}
               ,{<<"Privacy-Mode">>, Mode}
              ],
    send_command(Command, Call).

b_privacy(Call) ->
    b_privacy(<<"full">>, Call).
b_privacy(undefined, Call) ->
    b_privacy(Call);
b_privacy(Mode, Call) ->
    privacy(Mode, Call),
    wait_for_message(<<"privacy">>).

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
-type collect_digits_return() :: {'error','channel_hungup' | 'channel_unbridge' | wh_json:object()} | {'ok', ne_binary()}.
-spec collect_digits/2 :: (integer() | ne_binary(), whapps_call:call()) -> collect_digits_return().
-spec collect_digits/3 :: (integer() | ne_binary(), integer() | ne_binary(), whapps_call:call()) -> collect_digits_return().
-spec collect_digits/4 :: (integer() | ne_binary(), integer() | ne_binary(), integer() | ne_binary(), whapps_call:call()) 
                          -> collect_digits_return().
-spec collect_digits/5 :: (integer() | ne_binary(), integer() | ne_binary(), integer() | ne_binary(), api_binary(), whapps_call:call()) 
                          -> collect_digits_return().
-spec collect_digits/6 :: (integer() | ne_binary(), integer() | ne_binary(), integer() | ne_binary(), api_binary(), list()
                           ,whapps_call:call()) -> collect_digits_return().

collect_digits(MaxDigits, Call) ->
    collect_digits(MaxDigits, 3000, Call).
collect_digits(MaxDigits, Timeout, Call) ->
    collect_digits(MaxDigits, Timeout, 2000, Call).
collect_digits(MaxDigits, Timeout, Interdigit, Call) ->
    collect_digits(MaxDigits, Timeout, Interdigit, undefined, Call).
collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Call) ->
    collect_digits(MaxDigits, Timeout, Interdigit, NoopId, [<<"#">>], Call).

collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call) when is_binary(MaxDigits) ->
    collect_digits(wh_util:to_integer(MaxDigits), Timeout, Interdigit, NoopId, Terminators, Call);
collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call) when is_binary(Timeout) ->
    collect_digits(MaxDigits, wh_util:to_integer(Timeout), Interdigit, NoopId, Terminators, Call);
collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call) when is_binary(Interdigit) ->
    collect_digits(MaxDigits, Timeout, wh_util:to_integer(Interdigit), NoopId, Terminators, Call);
collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call) ->
    collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, <<>>, ?MILLISECONDS_IN_DAY).

collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After) ->
    Start = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            case get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_UNBRIDGE">>, _ } ->
                    lager:debug("channel was unbridged while collecting digits"),
                    {error, channel_unbridge};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    lager:debug("channel was hungup while collecting digits"),
                    {error, channel_hungup};
                { <<"error">>, _, <<"noop">> } ->
                    case wh_json:get_value([<<"Request">>, <<"Msg-ID">>], JObj, NoopId) of
                        NoopId when is_binary(NoopId), NoopId =/= <<>> ->
                            lager:debug("channel execution error while collecting digits: ~s", [wh_json:encode(JObj)]),
                            {error, JObj};
                        _NID when is_binary(NoopId), NoopId =/= <<>> ->
                            collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After);
                        _ ->
                            lager:debug("channel execution error while collecting digits: ~s", [wh_json:encode(JObj)]),
                            {error, JObj}
                    end;
                { <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">> } ->
                    %% Playback completed start timeout
                    case wh_json:get_value(<<"Application-Response">>, JObj) of
                        NoopId when is_binary(NoopId), NoopId =/= <<>> ->
                            %% if we were given the NoopId of the noop and this is it, then start the timer
                            %% unless we have already started collecting digits when the noop came in
                            T = case Digits of <<>> -> Timeout; _ -> After end,
                            collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, T);
                        _NID when is_binary(NoopId), NoopId =/= <<>> ->
                            %% if we were given the NoopId of the noop and this is not it, then keep waiting
                            collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After);
                        _ ->
                            %% if we are not given the NoopId of the noop then just use the first to start the timer
                            %% unless we have already started collecting digits when the noop came in
                            T = case Digits of <<>> -> Timeout; _ -> After end,
                            collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, T)
                    end;
                { <<"call_event">>, <<"DTMF">>, _ } ->
                    %% DTMF received, collect and start interdigit timeout
                    Digit = wh_json:get_value(<<"DTMF-Digit">>, JObj, <<>>),
                    case lists:member(Digit, Terminators) of
                        true ->
                            lager:debug("collected digits ('~s') from caller, terminated with ~s", [Digits, Digit]),
                            {ok, Digits};
                        false ->
                            case <<Digits/binary, Digit/binary>> of
                                D when size(D) < MaxDigits ->
                                    collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, D, Interdigit);
                                D ->
                                    lager:debug("collected maximum digits ('~s') from caller", [D]),
                                    {ok, D}
                            end
                    end;
                _ when After =:= infinity ->
                    collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After);
                _ ->
                    collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After - wh_util:elapsed_ms(Start))
            end;
        _ when After =:= infinity ->
            collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After);
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After - wh_util:elapsed_ms(Start))
    after
        After ->
            lager:debug("collect digits timeout"),
            {ok, Digits}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Low level function to consume call events, looping until a specific
%% one occurs.  If the channel is hungup or no call events are recieved
%% for the optional timeout period then errors are returned.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_message/1 :: (binary()) -> whapps_api_std_return().
-spec wait_for_message/2 :: (binary(), ne_binary()) -> whapps_api_std_return().
-spec wait_for_message/3 :: (binary(), ne_binary(), ne_binary()) -> whapps_api_std_return().
-spec wait_for_message/4 :: (binary(), ne_binary(), ne_binary(), wh_timeout()) -> whapps_api_std_return().

wait_for_message(Application) ->
    wait_for_message(Application, <<"CHANNEL_EXECUTE_COMPLETE">>).
wait_for_message(Application, Event) ->
    wait_for_message(Application, Event, <<"call_event">>).
wait_for_message(Application, Event, Type) ->
    wait_for_message(Application, Event, Type, 5000).

wait_for_message(Application, Event, Type, Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            case get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_DESTROY">>, _ } ->
                    lager:debug("channel was destroyed while waiting for ~s", [Application]),
                    {error, channel_destroy};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    lager:debug("channel was hungup while waiting for ~s", [Application]),
                    {error, channel_hungup};
                { <<"error">>, _, Application } ->
                    lager:debug("channel execution error while waiting for ~s: ~s", [Application, wh_json:encode(JObj)]),
                    {error, JObj};
                { Type, Event, Application } ->
                    {ok, JObj};
                _ when Timeout =:= infinity ->
                    wait_for_message(Application, Event, Type, Timeout);
                _ ->
                    wait_for_message(Application, Event, Type, Timeout - wh_util:elapsed_ms(Start))
            end;
        _ when Timeout =:= infinity ->
            wait_for_message(Application, Event, Type, Timeout);
        _ ->
            wait_for_message(Application, Event, Type, Timeout - wh_util:elapsed_ms(Start))
    after
        Timeout ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for an application to complete, ignoring channel state.  This
%% is only interested in events for the application.
%% @end
%%--------------------------------------------------------------------
-type wait_for_application_return() :: {'error', 'timeout' | wh_json:object()} | {'ok', wh_json:object()}.
-spec wait_for_application/1 :: (ne_binary()) -> wait_for_application_return().
-spec wait_for_application/2 :: (ne_binary(), ne_binary()) -> wait_for_application_return().
-spec wait_for_application/3 :: (ne_binary(), ne_binary(), ne_binary()) -> wait_for_application_return().
-spec wait_for_application/4 :: (ne_binary(), ne_binary(), ne_binary(), wh_timeout()) -> wait_for_application_return().

wait_for_application(Application) ->
    wait_for_application(Application, <<"CHANNEL_EXECUTE_COMPLETE">>).
wait_for_application(Application, Event) ->
    wait_for_application(Application, Event, <<"call_event">>).
wait_for_application(Application, Event, Type) ->
    wait_for_application(Application, Event, Type, 500000).

wait_for_application(Application, Event, Type, Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            case get_event_type(JObj) of
                { <<"error">>, _, Application } ->
                    lager:debug("channel execution error while waiting for ~s: ~s", [Application, wh_json:encode(JObj)]),
                    {error, JObj};
                { <<"call_event">>, <<"CHANNEL_DESTROY">>, _ } ->
                    lager:debug("channel was hungup while waiting for ~s", [Application]),
                    {error, channel_hungup};
                { Type, Event, Application } ->
                    {ok, JObj};
                _ when Timeout =:= infinity ->
                    wait_for_application(Application, Event, Type, Timeout);
                _ ->
                    wait_for_application(Application, Event, Type, Timeout - wh_util:elapsed_ms(Start))
            end;
        _ when Timeout =:= infinity ->
            wait_for_application(Application, Event, Type, Timeout);
        _ ->
            wait_for_application(Application, Event, Type, Timeout - wh_util:elapsed_ms(Start))
    after
        Timeout ->
            {error, timeout}
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
-spec wait_for_headless_application/1 :: (ne_binary()) ->
                                                 wait_for_headless_application_return().
-spec wait_for_headless_application/2 :: (ne_binary(), ne_binary()) ->
                                                 wait_for_headless_application_return().
-spec wait_for_headless_application/3 :: (ne_binary(), ne_binary(), ne_binary()) ->
                                                 wait_for_headless_application_return().
-spec wait_for_headless_application/4 :: (ne_binary(), ne_binary(), ne_binary(), wh_timeout()) ->
                                                 wait_for_headless_application_return().

wait_for_headless_application(Application) ->
    wait_for_headless_application(Application, <<"CHANNEL_EXECUTE_COMPLETE">>).
wait_for_headless_application(Application, Event) ->
    wait_for_headless_application(Application, Event, <<"call_event">>).
wait_for_headless_application(Application, Event, Type) ->
    wait_for_headless_application(Application, Event, Type, 500000).

wait_for_headless_application(Application, Event, Type, Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            case get_event_type(JObj) of
                { <<"error">>, _, Application } ->
                    lager:debug("channel execution error while waiting for ~s: ~s", [Application, wh_json:encode(JObj)]),
                    {error, JObj};
                {<<"call_event">>,<<"CHANNEL_HANGUP_COMPLETE">>,_} ->
                    lager:debug("hangup occurred, waiting 5000 ms for ~s event", [Application]),
                    wait_for_headless_application(Application, Event, Type, 5000);
                {<<"call_event">>,<<"CHANNEL_DESTROY">>, _} ->
                    lager:debug("destroy occurred, waiting 5000 ms for ~s event", [Application]),
                    wait_for_headless_application(Application, Event, Type, 5000);
                { Type, Event, Application } ->
                    {ok, JObj};
                _T when Timeout =:= infinity ->
                    lager:debug("ignore ~p", [_T]),
                    wait_for_headless_application(Application, Event, Type, Timeout);
                _T ->
                    lager:debug("ignore ~p", [_T]),
                    wait_for_headless_application(Application, Event, Type, Timeout - wh_util:elapsed_ms(Start))
            end;
        _ when Timeout =:= infinity ->
            wait_for_headless_application(Application, Event, Type, Timeout);
        _ ->
            wait_for_headless_application(Application, Event, Type, Timeout - wh_util:elapsed_ms(Start))
    after
        Timeout ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for a DTMF event and extract the digits when it comes
%% @end
%%--------------------------------------------------------------------
-spec wait_for_dtmf/1 :: (wh_timeout()) -> {'error', 'channel_hungup' | wh_json:object()} | {'ok', binary()}.
wait_for_dtmf(Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            case whapps_util:get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_DESTROY">> } ->
                    lager:debug("channel was destroyed while waiting for DTMF"),
                    {error, channel_destroy};
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
                    lager:debug("channel was destroyed while waiting for DTMF"),
                    {error, channel_hungup};
                { <<"error">>, _ } ->
                    lager:debug("channel execution error while waiting for DTMF: ~s", [wh_json:encode(JObj)]),
                    {error, JObj};
                { <<"call_event">>, <<"DTMF">> } ->
                    {ok, wh_json:get_value(<<"DTMF-Digit">>, JObj)};
                _ when Timeout =:= infinity ->
                    wait_for_dtmf(Timeout);
                _ ->
                    wait_for_dtmf(Timeout - wh_util:elapsed_ms(Start))
            end;
        _E when Timeout =:= infinity ->
            lager:debug("unexpected ~p", [_E]),
            wait_for_dtmf(Timeout);
        _E ->
            lager:debug("unexpected ~p", [_E]),
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_dtmf(Timeout - wh_util:elapsed_ms(Start))
    after
        Timeout ->
            {ok, <<>>}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Waits for and determines the status of the bridge command
%% @end
%%--------------------------------------------------------------------
-spec wait_for_bridge/2 :: (wh_timeout(), whapps_call:call()) ->
                                   whapps_api_bridge_return().
-spec wait_for_bridge/3 :: (wh_timeout(), 'undefined' | fun((wh_json:object()) -> any()), whapps_call:call()) ->
                                   whapps_api_bridge_return().
wait_for_bridge(Timeout, Call) ->
    wait_for_bridge(Timeout, undefined, Call).

wait_for_bridge(Timeout, Fun, Call) ->
    Start = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            AppResponse = wh_json:get_value(<<"Application-Response">>, JObj,
                                            wh_json:get_value(<<"Hangup-Cause">>, JObj)),
            Result = case lists:member(AppResponse, ?SUCCESSFUL_HANGUPS) of
                         true -> ok;
                         false -> fail
                     end,
            case get_event_type(JObj) of               
                {<<"error">>, _, <<"bridge">>} ->
                    lager:debug("channel execution error while waiting for bridge: ~s", [wh_json:encode(JObj)]),
                    {error, JObj};
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>, _} ->
                    CallId = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
                    lager:debug("channel bridged to ~s", [CallId]),
                    case is_function(Fun, 1) of
                        false -> ok;
                        true -> Fun(JObj)
                    end,
                    wait_for_bridge(infinity, Fun, Call);
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    {Result, JObj};
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>} ->
                    lager:debug("bridge completed with result ~s catagorized as ~s", [AppResponse, Result]),
                    {Result, JObj};
                _ when Timeout =:= infinity ->
                    wait_for_bridge(Timeout, Fun, Call);
                _ ->
                    wait_for_bridge(Timeout - wh_util:elapsed_ms(Start), Fun, Call)
            end;
        _ when Timeout =:= infinity ->
            wait_for_bridge(Timeout, Fun, Call);
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_bridge(Timeout - wh_util:elapsed_ms(Start), Fun, Call)
    after
        Timeout -> {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for a noop or a specific noop to occur
%% @end
%%--------------------------------------------------------------------
-spec wait_for_noop/1 :: (api_binary()) -> whapps_api_std_return().
wait_for_noop(NoopId) ->
    case wait_for_message(<<"noop">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, infinity) of
        {ok, JObj}=OK ->
            case wh_json:get_value(<<"Application-Response">>, JObj) of
                NoopId when is_binary(NoopId), NoopId =/= <<>> -> OK;
                _No when is_binary(NoopId), NoopId =/= <<>> -> wait_for_noop(NoopId);
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
-spec wait_for_channel_unbridge/0 :: () -> {'ok', wh_json:object()}.
wait_for_channel_unbridge() ->
    receive
        {amqp_msg, JObj} ->
            case whapps_util:get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_UNBRIDGE">> } ->
                    {ok, JObj};
                { <<"call_event">>, <<"CHANNEL_DESTROY">> } ->
                    {ok, JObj};
                _ ->
                    wait_for_channel_unbridge()
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_channel_unbridge()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for a channel to be bridged to (or destroyed)
%% @end
%%--------------------------------------------------------------------
-spec wait_for_channel_bridge/0 :: () -> {'ok', wh_json:object()}.
wait_for_channel_bridge() ->
    receive
        {amqp_msg, JObj} ->
            case whapps_util:get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_BRIDGE">> } ->
                    {ok, JObj};
                { <<"call_event">>, <<"CHANNEL_DESTROY">> } ->
                    {ok, JObj};
                _ ->
                    wait_for_channel_bridge()
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_channel_bridge()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait forever for the channel to hangup
%% @end
%%--------------------------------------------------------------------
-spec wait_for_hangup/0 :: () -> {'ok', 'channel_hungup'}.
wait_for_hangup() ->
    receive
        {amqp_msg, JObj} ->
            case whapps_util:get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
                    {ok, channel_hungup};
                _ ->
                    wait_for_hangup()
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_hangup()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait forever for the channel to hangup
%% @end
%%--------------------------------------------------------------------
-spec wait_for_unbridge/0 :: () -> {'ok', 'leg_hungup'}.
wait_for_unbridge() ->
    receive
        {amqp_msg, JObj} ->
            case whapps_util:get_event_type(JObj) of
                { <<"call_event">>, <<"LEG_DESTROYED">> } ->
                    {ok, leg_hungup};
                _ ->
                    wait_for_unbridge()
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_unbridge()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Waits for and determines the status of the bridge command
%% @end
%%--------------------------------------------------------------------
-spec wait_for_application_or_dtmf/2 :: (ne_binary(), wh_timeout()) ->
                                                whapps_api_std_return() |
                                                {'dtmf', binary()}.
wait_for_application_or_dtmf(Application, Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            case get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_DESTROY">>, _ } ->
                    lager:debug("channel was destroyed while waiting for ~s or DTMF", [Application]),
                    {error, channel_destroy};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    lager:debug("channel was hungup while waiting for ~s or DTMF", [Application]),
                    {error, channel_hungup};
                { <<"error">>, _, Application } ->
                    lager:debug("channel execution error while waiting ~s or DTMF: ~s", [Application, wh_json:encode(JObj)]),
                    {error, JObj};
                { <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, Application} ->
                    {ok, JObj};
                { <<"call_event">>, <<"DTMF">>, _ } ->
                    {dtmf, wh_json:get_value(<<"DTMF-Digit">>, JObj)};
                _ when Timeout =:= infinity ->
                    wait_for_application_or_dtmf(Application, Timeout);
                _ ->
                    wait_for_application_or_dtmf(Application, Timeout - wh_util:elapsed_ms(Start))
            end;
        _ when Timeout =:= infinity ->
            wait_for_application_or_dtmf(Application, Timeout);
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_application_or_dtmf(Application, Timeout - wh_util:elapsed_ms(Start))
    after
        Timeout ->
            {error, timeout}
    end.

-type wait_for_fax_ret() :: {'ok', wh_json:object()} |
                            {'error', 'channel_destroy' | 'channel_hungup' | wh_json:object()}.
-spec wait_for_fax/0 :: () -> wait_for_fax_ret().
-spec wait_for_fax/1 :: (wh_timeout()) -> wait_for_fax_ret().
wait_for_fax() ->
    wait_for_fax(10000).
wait_for_fax(Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, JObj} ->
            case get_event_type(JObj) of
                { <<"error">>, _, <<"receive_fax">> } ->
                    lager:debug("channel execution error while waiting for fax: ~s", [wh_json:encode(JObj)]),
                    {error, JObj};
                { <<"call_event">>, <<"CHANNEL_EXECUTE">>, <<"receive_fax">> } ->
                    wait_for_fax(infinity);
                { <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"receive_fax">> } ->
                    {ok, wh_json:set_value(<<"Fax-Success">>, true, JObj)};
                _ when Timeout =:= infinity ->
                    wait_for_fax(Timeout);
                _ ->
                    wait_for_fax(Timeout - wh_util:elapsed_ms(Start))
            end;
        _ when Timeout =:= infinity ->
            wait_for_fax(Timeout);
        _ ->
            wait_for_fax(Timeout - wh_util:elapsed_ms(Start))
    after
        Timeout ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait forever for the channel to hangup
%% @end
%%--------------------------------------------------------------------
-spec get_event_type/1 :: (wh_json:object()) -> {binary(), binary(), binary()}.
get_event_type(JObj) ->
    { wh_json:get_value(<<"Event-Category">>, JObj, <<>>)
      ,wh_json:get_value(<<"Event-Name">>, JObj, <<>>)
      ,wh_json:get_value(<<"Application-Name">>, JObj, wh_json:get_value([<<"Request">>, <<"Application-Name">>], JObj, <<>>)) }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
-spec send_command/2 :: (api_terms(), whapps_call:call()) -> 'ok'.
send_command(Command, Call) when is_list(Command) ->
    true = whapps_call:is_call(Call),
    CustomPublisher = whapps_call:custom_publish_function(Call),
    CtrlQ = whapps_call:control_queue(Call),    
    case is_function(CustomPublisher, 2) of
        true -> CustomPublisher(Command, Call);
        false when is_binary(CtrlQ) ->
            Q = whapps_call:controller_queue(Call),
            CallId = whapps_call:call_id(Call),
            AppName = whapps_call:application_name(Call),
            AppVersion = whapps_call:application_version(Call),
            _ = case whapps_call:kvs_fetch(cf_exe_pid, Call) of
                    Pid when is_pid(Pid) -> put(amqp_publish_as, Pid);
                    _Else -> lager:debug("cf_exe_pid down, publish as self(~p)", [self()])
                end,
            Prop = Command ++ [{<<"Call-ID">>, CallId}
                               | wh_api:default_headers(Q, <<"call">>, <<"command">>, AppName, AppVersion)
                              ],

            wapi_dialplan:publish_command(CtrlQ, Prop);
        false -> ok
    end;
send_command(JObj, Call) ->
    case whapps_call:kvs_fetch(cf_exe_pid, Call) of
        Pid when is_pid(Pid) -> put(amqp_publish_as, Pid);
        _Else -> ok
    end,
    send_command(wh_json:to_proplist(JObj), Call).
