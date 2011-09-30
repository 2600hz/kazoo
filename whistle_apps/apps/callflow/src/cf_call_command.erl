%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 19 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_call_command).

-include("callflow.hrl").

-export([audio_macro/2]).
-export([answer/1, hangup/1, set/3, fetch/1, fetch/2]).
-export([bridge/2, bridge/3, bridge/4, bridge/5, bridge/6, bridge/7]).
-export([play/2, play/3]).
-export([record/2, record/3, record/4, record/5, record/6]).
-export([store/3, store/4, store/5]).
-export([tones/2]).
-export([play_and_collect_digit/2]).
-export([play_and_collect_digits/4, play_and_collect_digits/5, play_and_collect_digits/6,
         play_and_collect_digits/7, play_and_collect_digits/8, play_and_collect_digits/9]).
-export([say/2, say/3, say/4, say/5]).
-export([conference/2, conference/3, conference/4, conference/5]).
-export([noop/1]).
-export([flush/1, flush_dtmf/1]).

-export([b_answer/1, b_hangup/1, b_fetch/1, b_fetch/2]).
-export([b_bridge/2, b_bridge/3, b_bridge/4, b_bridge/5, b_bridge/6, b_bridge/7]).
-export([b_play/2, b_play/3]).
-export([b_record/2, b_record/3, b_record/4, b_record/5, b_record/6]).
-export([b_store/3, b_store/4, b_store/5]).
-export([b_play_and_collect_digit/2]).
-export([b_play_and_collect_digits/4, b_play_and_collect_digits/5, b_play_and_collect_digits/6,
         b_play_and_collect_digits/7, b_play_and_collect_digits/8, b_play_and_collect_digits/9]).
-export([b_say/2, b_say/3, b_say/4, b_say/5]).
-export([b_conference/2, b_conference/3, b_conference/4, b_conference/5]).
-export([b_noop/1]).
-export([b_flush/1]).

-export([wait_for_message/1, wait_for_message/2, wait_for_message/3, wait_for_message/4]).
-export([wait_for_application/1, wait_for_application/2, wait_for_application/3, wait_for_application/4]).
-export([wait_for_bridge/2, wait_for_unbridge/0]).
-export([wait_for_dtmf/1]).
-export([wait_for_noop/1]).
-export([wait_for_hangup/0]).
-export([wait_for_application_or_dtmf/2]).
-export([collect_digits/2, collect_digits/3, collect_digits/4, collect_digits/5, collect_digits/6]).
-export([send_callctrl/2]).

-export([find_failure_branch/2]).
-export([update_fallback/2]).

%%--------------------------------------------------------------------
%% @pubic
%% @doc
%% @end
%%--------------------------------------------------------------------
-type audio_macro_prompt() :: {'play', binary()} | {'play', binary(), [binary(),...]} |
			      {'say', binary()} | {'say', binary(), binary()} |
			      {'say', binary(), binary(), binary()} | {'say', binary(), binary(), binary(), binary()} |
			      {'tones', json_objects()}.
-export_type([audio_macro_prompt/0]).

-spec audio_macro/2 :: (Prompts, Call) -> binary() when
      Prompts :: [audio_macro_prompt(),...],
      Call :: #cf_call{}.
-spec audio_macro/3 :: (Prompts, Call, Queue) -> binary() when
      Prompts :: [audio_macro_prompt(),...],
      Call :: #cf_call{},
      Queue :: json_objects().

audio_macro([], Call) ->
    noop(Call);
audio_macro(Prompts, Call) ->
    audio_macro(Prompts, Call, []).

audio_macro([], #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call, Queue) ->
    NoopId = couch_mgr:get_uuid(),
    Prompts = [{struct, [{<<"Application-Name">>, <<"noop">>}
                         ,{<<"Msg-ID">>, NoopId}
                         ,{<<"Call-ID">>, CallId}]}
                | Queue],
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Commands">>, Prompts }
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:queue_req(Command),
    send_callctrl(Payload, Call),
    NoopId;
audio_macro([{play, MediaName}|T], Call, Queue) ->
    audio_macro(T, Call, [play_command(MediaName, ?ANY_DIGIT, Call) | Queue]);
audio_macro([{play, MediaName, Terminators}|T], Call, Queue) ->
    audio_macro(T, Call, [play_command(MediaName, Terminators, Call) | Queue]);
audio_macro([{say, Say}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, <<"name_spelled">>, <<"pronounced">>, <<"en">>, Call) | Queue]);
audio_macro([{say, Say, Type}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, Type, <<"pronounced">>, <<"en">>, Call) | Queue]);
audio_macro([{say, Say, Type, Method}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, Type, Method, <<"en">>, Call) | Queue]);
audio_macro([{say, Say, Type, Method, Language}|T], Call, Queue) ->
    audio_macro(T, Call, [say_command(Say, Type, Method, Language, Call) | Queue]);
audio_macro([{tones, Tones}|T], Call, Queue) ->
    audio_macro(T, Call, [tones_command(Tones, Call) | Queue]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec flush_dtmf/1 :: (Call) -> 'ok' when
      Call :: #cf_call{}.
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
-spec set/3 :: (ChannelVars, CallVars, Call) -> 'ok' when
      ChannelVars :: 'undefined' | json_object(),
      CallVars :: 'undefined' | json_object(),
      Call :: #cf_call{}.

set(undefined, CallVars, Call) ->
    set(?EMPTY_JSON_OBJECT, CallVars, Call);
set(ChannelVars, undefined, Call) ->
    set(ChannelVars, ?EMPTY_JSON_OBJECT, Call);
set(?EMPTY_JSON_OBJECT, ?EMPTY_JSON_OBJECT, _) ->
    ok;
set(ChannelVars, CallVars, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [{<<"Application-Name">>, <<"set">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Custom-Channel-Vars">>, ChannelVars}
               ,{<<"Custom-Call-Vars">>, CallVars}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:set_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    send_callctrl(Payload, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to fetch channe vars
%% NOTICE: These are 'custom' channel vars for state info only, and
%%   can not the switch vars
%% @end
%%--------------------------------------------------------------------
-spec fetch/1 :: (Call) -> 'ok' when
      Call :: #cf_call{}.
-spec fetch/2 :: (FromOtherLeg, Call) -> 'ok' when
      FromOtherLeg :: boolean(),
      Call :: #cf_call{}.

-spec b_fetch/1 :: (Call) -> cf_api_std_return() when
      Call :: #cf_call{}.
-spec b_fetch/2 :: (FromOtherLeg, Call) -> cf_api_std_return() when
      FromOtherLeg :: boolean(),
      Call :: #cf_call{}.

fetch(Call) ->
    fetch(false, Call).
fetch(FromOtherLeg, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [{<<"Application-Name">>, <<"fetch">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"From-Other-Leg">>, FromOtherLeg}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:fetch_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    send_callctrl(Payload, Call).

b_fetch(Call) ->
    b_fetch(false, Call).
b_fetch(FromOtherLeg, Call) ->
    fetch(FromOtherLeg, Call),
    case wait_for_message(<<"fetch">>) of
        {ok, JObj} ->
            {ok, wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, ?EMPTY_JSON_OBJECT)};
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to answer the channel
%% @end
%%--------------------------------------------------------------------
-spec answer/1 :: (Call) -> 'ok' when
      Call :: #cf_call{}.
-spec b_answer/1 :: (Call) -> cf_api_error() | {'ok', json_object()} when
      Call :: #cf_call{}.

answer(#cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [{<<"Application-Name">>, <<"answer">>}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:answer_req(Command),
    send_callctrl(Payload, Call).

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
-spec hangup/1 :: (Call) -> 'ok' when
      Call :: #cf_call{}.
-spec b_hangup/1 :: (Call) -> {'ok', 'channel_hungup'} when
      Call :: #cf_call{}.

hangup(#cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:hangup_req(Command),
    send_callctrl(Payload, Call).

b_hangup(Call) ->
    hangup(Call),
    wait_for_hangup().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to bridge the call
%% @end
%%--------------------------------------------------------------------
-type cf_cid_types() :: binary() | 'undefined'.
-spec bridge/2 :: (Endpoints :: json_objects(), Call :: #cf_call{}) -> 'ok'.
-spec bridge/3 :: (Endpoints :: json_objects(), Timeout :: cf_api_binary(), Call :: #cf_call{}) -> 'ok'.
-spec bridge/5 :: (Endpoints :: json_objects(), Timeout :: cf_api_binary(), CIDType :: cf_cid_types(), Strategy :: cf_api_binary(), Call :: #cf_call{}) -> 'ok'.
-spec bridge/6 :: (Endpoints :: json_objects(), Timeout :: cf_api_binary(), CIDType :: cf_cid_types(), Strategy :: cf_api_binary(), IgnoreEarlyMedia :: cf_api_binary(), Call :: #cf_call{}) -> 'ok'.
-spec bridge/7 :: (Endpoints :: json_objects(), Timeout :: cf_api_binary(), CIDType :: cf_cid_types(), Strategy :: cf_api_binary(), IgnoreEarlyMedia :: cf_api_binary(), Ringback :: cf_api_binary(), Call :: #cf_call{}) -> 'ok'.

-spec b_bridge/2 :: (Endpoints :: json_objects(), Call :: #cf_call{}) -> cf_api_bridge_return().
-spec b_bridge/3 :: (Endpoints :: json_objects(), Timeout :: cf_api_binary(), Call :: #cf_call{}) -> cf_api_bridge_return().
-spec b_bridge/4 :: (Endpoints :: json_objects(), Timeout :: cf_api_binary(), CIDType :: cf_cid_types(), Call :: #cf_call{}) -> cf_api_bridge_return().
-spec b_bridge/5 :: (Endpoints :: json_objects(), Timeout :: cf_api_binary(), CIDType :: cf_cid_types(), Strategy :: cf_api_binary(), Call :: #cf_call{}) -> cf_api_bridge_return().
-spec b_bridge/6 :: (Endpoints :: json_objects(), Timeout :: cf_api_binary(), CIDType :: cf_cid_types(), Strategy :: cf_api_binary(), IgnoreEarlyMedia :: cf_api_binary(), Call :: #cf_call{}) -> cf_api_bridge_return().
-spec b_bridge/7 :: (Endpoints :: json_objects(), Timeout :: cf_api_binary(), CIDType :: cf_cid_types(), Strategy :: cf_api_binary(), IgnoreEarlyMedia :: cf_api_binary(), Ringback :: cf_api_binary(), Call :: #cf_call{}) -> cf_api_bridge_return().

bridge(Endpoints, Call) ->
    bridge(Endpoints, <<"26">>, Call).
bridge(Endpoints, Timeout, Call) ->
    bridge(Endpoints, Timeout, undefined, Call).
bridge(Endpoints, Timeout, CIDType, Call) ->
    bridge(Endpoints, Timeout, CIDType, <<"single">>, Call).
bridge(Endpoints, Timeout, CIDType, Strategy, Call) ->
    bridge(Endpoints, Timeout, CIDType, Strategy, <<"false">>, Call).
bridge(Endpoints, Timeout, CIDType, Strategy, IgnoreEarlyMedia, Call) ->
    bridge(Endpoints, Timeout, CIDType, Strategy, IgnoreEarlyMedia, undefined, Call).
bridge(Endpoints, Timeout, CIDType, Strategy, IgnoreEarlyMedia, Ringback
       , #cf_call{call_id=CallId, amqp_q=AmqpQ, owner_id=OwnerId, authorizing_id=AuthId, inception=Inception, channel_vars=CCVs}=Call) ->
    {CIDNum, CIDName} = case Inception of
                            <<"off-net">> -> {undefined, undefined};
                            _ -> cf_attributes:caller_id(CIDType, AuthId, OwnerId, Call)
                        end,
    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Timeout">>, Timeout}
               ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
               ,{<<"Outgoing-Caller-ID-Number">>, CIDNum}
               ,{<<"Outgoing-Caller-ID-Name">>, CIDName}
               ,{<<"Ringback">>, Ringback}
               ,{<<"Dial-Endpoint-Method">>, Strategy}
               ,{<<"Custom-Channel-Vars">>, CCVs}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = wh_api:bridge_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    send_callctrl(Payload, Call).

b_bridge(Endpoints, Call) ->
    b_bridge(Endpoints, <<"26">>, Call).
b_bridge(Endpoints, Timeout, Call) ->
    b_bridge(Endpoints, Timeout, <<"default">>, Call).
b_bridge(Endpoints, Timeout, CIDType, Call) ->
    b_bridge(Endpoints, Timeout, CIDType, <<"single">>, Call).
b_bridge(Endpoints, Timeout, CIDType, Strategy, Call) ->
    b_bridge(Endpoints, Timeout, CIDType, Strategy, <<"false">>, Call).
b_bridge(Endpoints, Timeout, CIDType, Strategy, IgnoreEarlyMedia, Call) ->
    b_bridge(Endpoints, Timeout, CIDType, Strategy, IgnoreEarlyMedia, undefined, Call).
b_bridge(Endpoints, Timeout, CIDType, Strategy, IgnoreEarlyMedia, Ringback, Call) ->
    bridge(Endpoints, Timeout, CIDType, Strategy, IgnoreEarlyMedia, Ringback, Call),
    case wait_for_bridge((wh_util:to_integer(Timeout)*1000) + 10000, Call) of
        {ok, Bridge}=Ok ->
            spawn(fun() -> update_fallback(Bridge, Call) end),
            Ok;
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to play media to the
%% caller.  A list of terminators can be provided that the caller
%% can use to skip playback.
%% @end
%%--------------------------------------------------------------------
-spec play/2 :: (Media, Call) -> 'ok' when
      Media :: binary(),
      Call :: #cf_call{}.
-spec play/3 :: (Media, Terminators, Call) -> 'ok' when
      Media :: binary(),
      Terminators :: [binary(),...],
      Call :: #cf_call{}.

-spec b_play/2 :: (Media, Call) -> cf_api_std_return() when
      Media :: binary(),
      Call :: #cf_call{}.
-spec b_play/3 :: (Media, Terminators, Call) -> cf_api_std_return() when
      Media :: binary(),
      Terminators :: [binary(),...],
      Call :: #cf_call{}.

play(Media, Call) ->
    play(Media, ?ANY_DIGIT, Call).
play(Media, Terminators, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [{<<"Application-Name">>, <<"play">>}
               ,{<<"Media-Name">>, Media}
               ,{<<"Terminators">>, Terminators}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:play_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    send_callctrl(Payload, Call).

b_play(Media, Call) ->
    b_play(Media, ?ANY_DIGIT, Call).
b_play(Media, Terminators, Call) ->
    play(Media, Terminators, Call),
    wait_for_message(<<"play">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, infinity).

-spec play_command/3 :: (Media, Terminators, Call) -> json_object() when
      Media :: binary(),
      Terminators :: [binary(),...],
      Call :: #cf_call{}.
play_command(Media, Terminators, #cf_call{call_id=CallId}) ->
    wh_json:from_list([{<<"Application-Name">>, <<"play">>}
		       ,{<<"Media-Name">>, Media}
		       ,{<<"Terminators">>, Terminators}
		       ,{<<"Call-ID">>, CallId}
		      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to record a file.
%% A list of keys can be used as the terminator or a silence threshold.
%% @end
%%--------------------------------------------------------------------
-spec record/2 :: (MediaName :: binary(), Call :: #cf_call{}) -> 'ok'.
-spec record/3 :: (MediaName :: binary(), Terminators :: [binary(),...], Call :: #cf_call{}) -> 'ok'.
-spec record/4 :: (MediaName :: binary(), Terminators :: [binary(),...], TimeLimit :: binary(), Call :: #cf_call{}) -> 'ok'.
-spec record/5 :: (MediaName :: binary(), Terminators :: [binary(),...], TimeLimit :: binary(), SilenceThreshold :: binary(), Call :: #cf_call{}) -> 'ok'.
-spec record/6 :: (MediaName :: binary(), Terminators :: [binary(),...], TimeLimit :: binary(), SilenceThreshold :: binary(), SilenceHits :: binary(), Call :: #cf_call{}) -> 'ok'.

-spec b_record/2 :: (MediaName :: binary(), Call :: #cf_call{}) -> cf_api_std_return().
-spec b_record/3 :: (MediaName :: binary(), Terminators :: [binary(),...], Call :: #cf_call{}) -> cf_api_std_return().
-spec b_record/4 :: (MediaName :: binary(), Terminators :: [binary(),...], TimeLimit :: binary(), Call :: #cf_call{}) -> cf_api_std_return().
-spec b_record/5 :: (MediaName :: binary(), Terminators :: [binary(),...], TimeLimit :: binary(), SilenceThreshold :: binary(), Call :: #cf_call{}) -> cf_api_std_return().
-spec b_record/6 :: (MediaName :: binary(), Terminators :: [binary(),...], TimeLimit :: binary(), SilenceThreshold :: binary(), SilenceHits :: binary(), Call :: #cf_call{}) -> cf_api_std_return().

record(MediaName, Call) ->
    record(MediaName, ?ANY_DIGIT, Call).
record(MediaName, Terminators, Call) ->
    record(MediaName, Terminators, <<"120">>, Call).
record(MediaName, Terminators, TimeLimit, Call) ->
    record(MediaName, Terminators, TimeLimit, <<"250">>,  Call).
record(MediaName, Terminators, TimeLimit, SilenceThreshold, Call) ->
    record(MediaName, Terminators, TimeLimit, SilenceThreshold, <<"3">>, Call).
record(MediaName, Terminators, TimeLimit, SilenceThreshold, SilenceHits, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [{<<"Application-Name">>, <<"record">>}
               ,{<<"Media-Name">>, MediaName}
               ,{<<"Terminators">>, Terminators}
               ,{<<"Time-Limit">>, TimeLimit}
               ,{<<"Silence-Threshold">>, SilenceThreshold}
               ,{<<"Silence-Hits">>, SilenceHits}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:record_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    send_callctrl(Payload, Call).

b_record(MediaName, Call) ->
    b_record(MediaName, ?ANY_DIGIT, Call).
b_record(MediaName, Terminators, Call) ->
    b_record(MediaName, Terminators, <<"120">>, Call).
b_record(MediaName, Terminators, TimeLimit, Call) ->
    b_record(MediaName, Terminators, TimeLimit, <<"250">>,  Call).
b_record(MediaName, Terminators, TimeLimit, SilenceThreshold, Call) ->
    b_record(MediaName, Terminators, TimeLimit, SilenceThreshold, <<"3">>, Call).
b_record(MediaName, Terminators, TimeLimit, SilenceThreshold, SilenceHits, Call) ->
    record(MediaName, Terminators, TimeLimit, SilenceThreshold, SilenceHits, Call),
    wait_for_message(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, infinity).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to store the file
%% @end
%%--------------------------------------------------------------------
-spec store/3 :: (MediaName, Transfer, Call) -> 'ok' when
      MediaName :: binary(),
      Transfer :: binary(),
      Call :: #cf_call{}.
-spec store/4 :: (MediaName, Transfer, Method, Call) -> 'ok' when
      MediaName :: binary(),
      Transfer :: binary(),
      Method :: binary(),
      Call :: #cf_call{}.
-spec store/5 :: (MediaName, Transfer, Method, Headers, Call) -> 'ok' when
      MediaName :: binary(),
      Transfer :: binary(),
      Method :: binary(),
      Headers :: json_objects(),
      Call :: #cf_call{}.

-type b_store_return() :: {'error', 'execution_failure' | 'timeout'} | {'ok', json_object()}.

-spec b_store/3 :: (MediaName, Transfer, Call) -> b_store_return() when
      MediaName :: binary(),
      Transfer :: binary(),
      Call :: #cf_call{}.
-spec b_store/4 :: (MediaName, Transfer, Method, Call) -> b_store_return() when
      MediaName :: binary(),
      Transfer :: binary(),
      Method :: binary(),
      Call :: #cf_call{}.
-spec b_store/5 :: (MediaName, Transfer, Method, Headers, Call) -> b_store_return() when
      MediaName :: binary(),
      Transfer :: binary(),
      Method :: binary(),
      Headers :: json_objects(),
      Call :: #cf_call{}.

store(MediaName, Transfer, Call) ->
    store(MediaName, Transfer, <<"put">>, Call).
store(MediaName, Transfer, Method, Call) ->
    store(MediaName, Transfer, Method, [?EMPTY_JSON_OBJECT], Call).
store(MediaName, Transfer, Method, Headers, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [{<<"Application-Name">>, <<"store">>}
               ,{<<"Media-Name">>, MediaName}
               ,{<<"Media-Transfer-Method">>, Method}
               ,{<<"Media-Transfer-Destination">>, Transfer}
               ,{<<"Additional-Headers">>, Headers}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:store_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    send_callctrl(Payload, Call).

b_store(MediaName, Transfer, Call) ->
    b_store(MediaName, Transfer, <<"put">>, Call).
b_store(MediaName, Transfer, Method, Call) ->
    b_store(MediaName, Transfer, Method, [?EMPTY_JSON_OBJECT], Call).
b_store(MediaName, Transfer, Method, Headers, Call) ->
    store(MediaName, Transfer, Method, Headers, Call),
    wait_for_application(<<"store">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to play tones to the
%% caller
%% @end
%%--------------------------------------------------------------------
-spec tones/2 :: (Tones, Call) -> 'ok' when
      Tones :: json_objects(),
      Call :: #cf_call{}.
tones(Tones, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [{<<"Application-Name">>, <<"tones">>}
               ,{<<"Tones">>, Tones}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:tones_req(Command),
    send_callctrl(Payload, Call).

-spec tones_command/2 :: (Tones, Call) -> json_object() when
      Tones :: json_objects(),
      Call :: #cf_call{}.
tones_command(Tones, #cf_call{call_id=CallId}) ->
    wh_json:from_list([{<<"Application-Name">>, <<"tones">>}
		       ,{<<"Tones">>, Tones}
		       ,{<<"Call-ID">>, CallId}
		      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Produces the low level wh_api request to play media to a
%% caller, and collect a number of DTMF events.
%% @end
%%--------------------------------------------------------------------
-spec play_and_collect_digit/2 :: (Media, Call) -> 'ok' when
      Media :: binary(),
      Call :: #cf_call{}.
-spec play_and_collect_digits/4 :: (MinDigits, MaxDigits, Media, Call) -> 'ok' when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Call :: #cf_call{}.
-spec play_and_collect_digits/5 :: (MinDigits, MaxDigits, Media, Tries, Call) -> 'ok' when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Tries :: binary(),
      Call :: #cf_call{}.
-spec play_and_collect_digits/6 :: (MinDigits, MaxDigits, Media, Tries, Timeout, Call) -> 'ok' when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Tries :: binary(),
      Timeout :: binary(),
      Call :: #cf_call{}.
-spec play_and_collect_digits/7 :: (MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Call) -> 'ok' when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Tries :: binary(),
      Timeout :: binary(),
      MediaInvalid :: 'undefined' | binary(),
      Call :: #cf_call{}.
-spec play_and_collect_digits/8 :: (MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Call) -> 'ok' when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Tries :: binary(),
      Timeout :: binary(),
      MediaInvalid :: 'undefined' | binary(),
      Regex :: binary(),
      Call :: #cf_call{}.
-spec play_and_collect_digits/9 :: (MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, Call) -> 'ok' when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Tries :: binary(),
      Timeout :: binary(),
      MediaInvalid :: 'undefined' | binary(),
      Regex :: binary(),
      Terminators :: [binary(),...],
      Call :: #cf_call{}.

-type b_play_and_collect_digits_return() :: {'error', 'channel_hungup' | 'channel_unbridge' | 'execution_failure'} | {'ok', binary()}.

-spec b_play_and_collect_digit/2 :: (Media, Call) -> b_play_and_collect_digits_return() when
      Media :: binary(),
      Call :: #cf_call{}.
-spec b_play_and_collect_digits/4 :: (MinDigits, MaxDigits, Media, Call) -> b_play_and_collect_digits_return() when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Call :: #cf_call{}.
-spec b_play_and_collect_digits/5 :: (MinDigits, MaxDigits, Media, Tries, Call) -> b_play_and_collect_digits_return() when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Tries :: binary(),
      Call :: #cf_call{}.
-spec b_play_and_collect_digits/6 :: (MinDigits, MaxDigits, Media, Tries, Timeout, Call) -> b_play_and_collect_digits_return() when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Tries :: binary(),
      Timeout :: binary(),
      Call :: #cf_call{}.
-spec b_play_and_collect_digits/7 :: (MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Call) -> b_play_and_collect_digits_return() when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Tries :: binary(),
      Timeout :: binary(),
      MediaInvalid :: 'undefined' | binary(),
      Call :: #cf_call{}.
-spec b_play_and_collect_digits/8 :: (MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Call) -> b_play_and_collect_digits_return() when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Tries :: binary(),
      Timeout :: binary(),
      MediaInvalid :: 'undefined' | binary(),
      Regex :: binary(),
      Call :: #cf_call{}.
-spec b_play_and_collect_digits/9 :: (MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, Call) -> b_play_and_collect_digits_return() when
      MinDigits :: binary(),
      MaxDigits :: binary(),
      Media :: binary(),
      Tries :: binary(),
      Timeout :: binary(),
      MediaInvalid :: 'undefined' | binary(),
      Regex :: binary(),
      Terminators :: [binary(),...],
      Call :: #cf_call{}.

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
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [{<<"Application-Name">>, <<"play_and_collect_digits">>}
               ,{<<"Minimum-Digits">>, MinDigits}
               ,{<<"Maximum-Digits">>, MaxDigits}
               ,{<<"Timeout">>, Timeout}
               ,{<<"Terminators">>, Terminators}
               ,{<<"Media-Name">>, Media}
               ,{<<"Media-Tries">>, Tries}
               ,{<<"Failed-Media-Name">>, MediaInvalid}
               ,{<<"Digits-Regex">>, Regex}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:play_collect_digits_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    send_callctrl(Payload, Call).

b_play_and_collect_digit(Media, Call) ->
    b_play_and_collect_digits(<<"1">>, <<"1">>, Media, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, <<"3">>,  Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, <<"5000">>, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, undefined, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, <<"\\d+">>, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, ?ANY_DIGIT, Call).

b_play_and_collect_digits(_MinDigits, _MaxDigits, _Media, <<"0">>, _Timeout, undefined, _Regex, _Terminators, _Call) ->
    {ok, <<>>};
b_play_and_collect_digits(_MinDigits, _MaxDigits, _Media, <<"0">>, _Timeout, MediaInvalid, _Regex, Terminators, Call) ->
    _ = b_play(MediaInvalid, Terminators, Call),
    {ok, <<>>};
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, #cf_call{call_id=CallId, amqp_q=Q}=Call) ->
    NoopId = couch_mgr:get_uuid(),
    Commands = [{struct, [{<<"Application-Name">>, <<"noop">>}
                          ,{<<"Call-ID">>, CallId}
                          ,{<<"Msg-ID">>, NoopId}
                          | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)]}
                ,{struct, [{<<"Application-Name">>, <<"play">>}
                           ,{<<"Media-Name">>, Media}
                           ,{<<"Terminators">>, Terminators}
                           ,{<<"Call-ID">>, CallId}
                           | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)]}
               ],
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Commands">>, Commands}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:queue_req(Command),
    send_callctrl(Payload, Call),
    case collect_digits(MaxDigits, Timeout, <<"2000">>, NoopId, Call) of
        {ok, Digits} ->
            MinSize = wh_util:to_integer(MinDigits),
            case re:run(Digits, Regex) of
                {match, _} when size(Digits) >= MinSize ->
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
-spec say/2 :: (Say, Call) -> 'ok' when
      Say :: binary(),
      Call :: #cf_call{}.
-spec say/3 :: (Say, Type, Call) -> 'ok' when
      Say :: binary(),
      Type :: binary(),
      Call :: #cf_call{}.
-spec say/4 :: (Say, Type, Method, Call) -> 'ok' when
      Say :: binary(),
      Type :: binary(),
      Method :: binary(),
      Call :: #cf_call{}.
-spec say/5 :: (Say, Type, Method, Language, Call) -> 'ok' when
      Say :: binary(),
      Type :: binary(),
      Method :: binary(),
      Language :: binary(),
      Call :: #cf_call{}.

-spec b_say/2 :: (Say, Call) -> cf_api_std_return() when
      Say :: binary(),
      Call :: #cf_call{}.
-spec b_say/3 :: (Say, Type, Call) -> cf_api_std_return() when
      Say :: binary(),
      Type :: binary(),
      Call :: #cf_call{}.
-spec b_say/4 :: (Say, Type, Method, Call) -> cf_api_std_return() when
      Say :: binary(),
      Type :: binary(),
      Method :: binary(),
      Call :: #cf_call{}.
-spec b_say/5 :: (Say, Type, Method, Language, Call) -> cf_api_std_return() when
      Say :: binary(),
      Type :: binary(),
      Method :: binary(),
      Language :: binary(),
      Call :: #cf_call{}.

say(Say, Call) ->
    say(Say, <<"name_spelled">>, Call).
say(Say, Type, Call) ->
    say(Say, Type, <<"pronounced">>, Call).
say(Say, Type, Method, Call) ->
    say(Say, Type, Method, <<"en">>, Call).
say(Say, Type, Method, Language, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [{<<"Application-Name">>, <<"say">>}
               ,{<<"Say-Text">>, Say}
               ,{<<"Type">>, Type}
               ,{<<"Method">>, Method}
               ,{<<"Language">>, Language}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:say_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    send_callctrl(Payload, Call).

-spec say_command/5 :: (Say, Type, Method, Language, Call) -> json_object() when
      Say :: binary(),
      Type :: binary(),
      Method :: binary(),
      Language :: binary(),
      Call :: #cf_call{}.
say_command(Say, Type, Method, Language, #cf_call{call_id=CallId}) ->
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
-spec conference/2 :: (ConfId, Call) -> 'ok' when
      ConfId :: binary(),
      Call :: #cf_call{}.
-spec conference/3 :: (ConfId, Mute, Call) -> 'ok' when
      ConfId :: binary(),
      Mute :: binary(),
      Call :: #cf_call{}.
-spec conference/4 :: (ConfId, Mute, Deaf, Call) -> 'ok' when
      ConfId :: binary(),
      Mute :: binary(),
      Deaf :: binary(),
      Call :: #cf_call{}.
-spec conference/5 :: (ConfId, Mute, Deaf, Moderator, Call) -> 'ok' when
      ConfId :: binary(),
      Mute :: binary(),
      Deaf :: binary(),
      Moderator :: binary(),
      Call :: #cf_call{}.

-spec b_conference/2 :: (ConfId, Call) -> cf_api_std_return() when
      ConfId :: binary(),
      Call :: #cf_call{}.
-spec b_conference/3 :: (ConfId, Mute, Call) -> cf_api_std_return() when
      ConfId :: binary(),
      Mute :: binary(),
      Call :: #cf_call{}.
-spec b_conference/4 :: (ConfId, Mute, Deaf, Call) -> cf_api_std_return() when
      ConfId :: binary(),
      Mute :: binary(),
      Deaf :: binary(),
      Call :: #cf_call{}.
-spec b_conference/5 :: (ConfId, Mute, Deaf, Moderator, Call) -> cf_api_std_return() when
      ConfId :: binary(),
      Mute :: binary(),
      Deaf :: binary(),
      Moderator :: binary(),
      Call :: #cf_call{}.

conference(ConfId, Call) ->
    conference(ConfId, <<"false">>, Call).
conference(ConfId, Mute, Call) ->
    conference(ConfId, Mute, <<"false">>, Call).
conference(ConfId, Mute, Deaf, Call) ->
    conference(ConfId, Mute, Deaf, <<"false">>, Call).
conference(ConfId, Mute, Deaf, Moderator, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [{<<"Application-Name">>, <<"conference">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Mute">>, Mute}
               ,{<<"Deaf">>, Deaf}
               ,{<<"Moderator">>, Moderator}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:conference_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    send_callctrl(Payload, Call).

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
-spec noop/1 :: (Call) -> binary() when
      Call :: #cf_call{}.
-spec b_noop/1 :: (Call) -> cf_api_std_return() when
      Call :: #cf_call{}.

noop(#cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    NoopId = couch_mgr:get_uuid(),
    Command = [{<<"Application-Name">>, <<"noop">>}
               ,{<<"Msg-ID">>, NoopId}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:noop_req(Command),
    send_callctrl(Payload, Call),
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
-spec flush/1 :: (Call) -> binary() when
      Call :: #cf_call{}.
-spec b_flush/1 :: (Call) -> cf_api_std_return() when
      Call :: #cf_call{}.

flush(#cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    NoopId = couch_mgr:get_uuid(),
    Command = [{<<"Application-Name">>, <<"noop">>}
               ,{<<"Msg-ID">>, NoopId}
               ,{<<"Insert-At">>, <<"flush">>}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:noop_req(Command),
    send_callctrl(Payload, Call),
    NoopId.

b_flush(Call) ->
    wait_for_noop(flush(Call)).

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
-type collect_digits_return() :: {'error','channel_hungup' | 'channel_unbridge' | 'execution_failure'} | {'ok', binary()}.

-spec collect_digits/2 :: (MaxDigits, Call) -> collect_digits_return() when
      MaxDigits :: integer() | binary(),
      Call :: #cf_call{}.
-spec collect_digits/3 :: (MaxDigits, Timeout, Call) -> collect_digits_return() when
      MaxDigits :: integer() | binary(),
      Timeout :: integer() | binary(),
      Call :: #cf_call{}.
-spec collect_digits/4 :: (MaxDigits, Timeout, Interdigit, Call) -> collect_digits_return() when
      MaxDigits :: integer() | binary(),
      Timeout :: integer() | binary(),
      Interdigit :: integer() | binary(),
      Call :: #cf_call{}.
-spec collect_digits/5 :: (MaxDigits, Timeout, Interdigit, NoopId, Call) -> collect_digits_return() when
      MaxDigits :: integer() | binary(),
      Timeout :: integer() | binary(),
      Interdigit :: integer() | binary(),
      NoopId :: 'undefined' | binary(),
      Call :: #cf_call{}.
-spec collect_digits/6 :: (MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call) -> collect_digits_return() when
      MaxDigits :: integer() | binary(),
      Timeout :: integer() | binary(),
      Interdigit :: integer() | binary(),
      NoopId :: 'undefined' | binary(),
      Terminators :: list(),
      Call :: #cf_call{}.

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
    collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, <<>>, infinity).

collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After) ->
    Start = erlang:now(),
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_UNBRIDGE">>, _ } ->
                    ?LOG("channel was unbridged while collecting digits"),
                    {error, channel_unbridge};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    ?LOG("channel was hungup while collecting digits"),
                    {error, channel_hungup};
                { <<"error">>, _, _ } ->
                    ?LOG("channel execution error while collecting digits"),
                    {error, execution_failure};
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
                    %% remove any queued prompts, and start collecting digits
                    Digits =:= <<>> andalso flush(Call),
                    %% DTMF received, collect and start interdigit timeout
                    Digit = wh_json:get_value(<<"DTMF-Digit">>, JObj, <<>>),
                    case lists:member(Digit, Terminators) of
                        true ->
                            ?LOG("collected digits ('~s') from caller, terminated with ~s", [Digits, Digit]),
                            {ok, Digits};
                        false ->
                            case <<Digits/binary, Digit/binary>> of
                                D when size(D) < MaxDigits ->
                                    collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, D, Interdigit);
                                D ->
                                    ?LOG("collected maximum digits ('~s') from caller", [D]),
                                    {ok, D}
                            end
                    end;
                _ when After =:= infinity ->
                    collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After);
                _ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After - (DiffMicro div 1000))
            end;
        _ when After =:= infinity ->
            collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After);
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            DiffMicro = timer:now_diff(erlang:now(), Start),
            collect_digits(MaxDigits, Timeout, Interdigit, NoopId, Terminators, Call, Digits, After - (DiffMicro div 1000))
    after
        After ->
            ?LOG("collect digits timeout"),
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
-spec wait_for_message/1 :: (Application) -> cf_api_std_return() when
      Application :: binary().
-spec wait_for_message/2 :: (Application, Event) -> cf_api_std_return() when
      Application :: binary(),
      Event :: binary().
-spec wait_for_message/3 :: (Application, Event, Type) -> cf_api_std_return() when
      Application :: binary(),
      Event :: binary(),
      Type :: binary().
-spec wait_for_message/4 :: (Application, Event, Type, Timeout) -> cf_api_std_return() when
      Application :: binary(),
      Event :: binary(),
      Type :: binary(),
      Timeout :: 'infinity' | integer().

wait_for_message(Application) ->
    wait_for_message(Application, <<"CHANNEL_EXECUTE_COMPLETE">>).
wait_for_message(Application, Event) ->
    wait_for_message(Application, Event, <<"call_event">>).
wait_for_message(Application, Event, Type) ->
    wait_for_message(Application, Event, Type, 5000).

wait_for_message(Application, Event, Type, Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_UNBRIDGE">>, _ } ->
                    ?LOG("channel was unbridged while waiting for ~s", [Application]),
                    {error, channel_unbridge};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    ?LOG("channel was hungup while waiting for ~s", [Application]),
                    {error, channel_hungup};
                { <<"error">>, _, _ } ->
                    ?LOG("channel execution error while waiting for ~s", [Application]),
                    {error, execution_failure};
                { Type, Event, Application } ->
                    {ok, JObj};
                _ when Timeout =:= infinity ->
                    wait_for_message(Application, Event, Type, Timeout);
		_ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_message(Application, Event, Type, Timeout - (DiffMicro div 1000))
            end;
        _ when Timeout =:= infinity ->
            wait_for_message(Application, Event, Type, Timeout);
        _ ->
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_message(Application, Event, Type, Timeout - (DiffMicro div 1000))
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
-type wait_for_application_return() :: {'error','execution_failure' | 'timeout'} | {'ok', json_object()}.
-spec wait_for_application/1 :: (Application) -> wait_for_application_return() when
      Application :: binary().
-spec wait_for_application/2 :: (Application, Event) -> wait_for_application_return() when
      Application :: binary(),
      Event :: binary().
-spec wait_for_application/3 :: (Application, Event, Type) -> wait_for_application_return() when
      Application :: binary(),
      Event :: binary(),
      Type :: binary().
-spec wait_for_application/4 :: (Application, Event, Type, Timeout) -> wait_for_application_return() when
      Application :: binary(),
      Event :: binary(),
      Type :: binary(),
      Timeout :: 'infinity' | integer().

wait_for_application(Application) ->
    wait_for_application(Application, <<"CHANNEL_EXECUTE_COMPLETE">>).
wait_for_application(Application, Event) ->
    wait_for_application(Application, Event, <<"call_event">>).
wait_for_application(Application, Event, Type) ->
    wait_for_application(Application, Event, Type, 500000).

wait_for_application(Application, Event, Type, Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case get_event_type(JObj) of
                { <<"error">>, _, _ } ->
                    ?LOG("channel execution error while waiting for ~s", [Application]),
                    {error, execution_failure};
                { Type, Event, Application } ->
                    {ok, JObj};
                _ when Timeout =:= infinity ->
                    wait_for_application(Application, Event, Type, Timeout);
		_ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_application(Application, Event, Type, Timeout - (DiffMicro div 1000))
            end;
        _ when Timeout =:= infinity ->
            wait_for_application(Application, Event, Type, Timeout);
        _ ->
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_application(Application, Event, Type, Timeout - (DiffMicro div 1000))
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
-spec wait_for_dtmf/1 :: (Timeout) -> {'error', 'channel_hungup' | 'execution_failure'} | {'ok', binary()} when
      Timeout :: 'infinity' | integer().
wait_for_dtmf(Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case whapps_util:get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_UNBRIDGE">> } ->
                    ?LOG("channel was unbridged while waiting for DTMF"),
                    {error, channel_unbridge};
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
                    ?LOG("channel was hungup while waiting for DTMF"),
                    {error, channel_hungup};
                { <<"error">>, _ } ->
                    ?LOG("channel execution error while waiting for DTMF"),
                    {error, execution_failure};
                { <<"call_event">>, <<"DTMF">> } ->
                    {ok, wh_json:get_value(<<"DTMF-Digit">>, JObj)};
                _ when Timeout =:= infinity ->
                    wait_for_dtmf(Timeout);
                _ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_dtmf(Timeout - (DiffMicro div 1000))
            end;
        _ when Timeout =:= infinity ->
            wait_for_dtmf(Timeout);
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_dtmf(Timeout - (DiffMicro div 1000))
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
-spec wait_for_bridge/2 :: (Timeout, Call) -> cf_api_bridge_return() when
      Timeout :: 'infinity' | integer(),
      Call :: #cf_call{}.
wait_for_bridge(Timeout, Call) ->
    Start = erlang:now(),
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_DESTROY">>, _ } ->
                    ?LOG("channel was destroyed while waiting for bridge"),
                    {ok, JObj};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    ?LOG("channel was hungup while waiting for bridge"),
                    {ok, JObj};
                { <<"error">>, _, _ } ->
                    ?LOG("channel execution error while waiting for bridge"),
                    {error, execution_failure};
                { <<"call_event">>, <<"CHANNEL_BRIDGE">>, _ } ->
                    spawn(fun() -> update_fallback(JObj, Call) end),
                    wait_for_bridge(infinity, Call);
                { <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">> } ->
                    case wh_json:get_value(<<"Application-Response">>, JObj, <<>>) of
                        <<"SUCCESS">> -> {ok, JObj};
                        _ -> {fail, JObj}
                    end;
                _ when Timeout =:= infinity ->
                    wait_for_bridge(Timeout, Call);
                _ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_bridge(Timeout - (DiffMicro div 1000), Call)
            end;
        _ when Timeout =:= infinity ->
            wait_for_bridge(Timeout, Call);
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_bridge(Timeout - (DiffMicro div 1000), Call)
    after
        Timeout ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait for a noop or a specific noop to occur
%% @end
%%--------------------------------------------------------------------
-spec wait_for_noop/1 :: (NoopId) -> cf_api_std_return() when
      NoopId :: 'undefined' | binary().
wait_for_noop(NoopId) ->
    case wait_for_message(<<"noop">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, infinity) of
        {ok, JObj}=OK ->
            case wh_json:get_value(<<"Application-Response">>, JObj) of
                NoopId when is_binary(NoopId), NoopId =/= <<>> ->
                    OK;
                _ when is_binary(NoopId), NoopId =/= <<>> ->
                    wait_for_noop(NoopId);
                _ ->
                    OK
            end;
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Wait forever for the channel to hangup
%% @end
%%--------------------------------------------------------------------
-spec wait_for_unbridge/0 :: () -> {'ok', json_object()}.
wait_for_unbridge() ->
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case whapps_util:get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_UNBRIDGE">> } ->
                    {ok, JObj};
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
                    {ok, JObj};
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
%% Wait forever for the channel to hangup
%% @end
%%--------------------------------------------------------------------
-spec wait_for_hangup/0 :: () -> {'ok', 'channel_hungup'}.
wait_for_hangup() ->
    receive
        {amqp_msg, {struct, _}=JObj} ->
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
%% Waits for and determines the status of the bridge command
%% @end
%%--------------------------------------------------------------------
-spec wait_for_application_or_dtmf/2 :: (Application, Timeout) -> cf_api_std_return() | {'dtmf', binary()} when
      Application :: binary(),
      Timeout :: 'infinity' | integer().
wait_for_application_or_dtmf(Application, Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_UNBRIDGE">>, _ } ->
                    ?LOG("channel was unbridged while waiting for ~s or DTMF", [Application]),
                    {error, channel_unbridge};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    ?LOG("channel was hungup while waiting for ~s or DTMF", [Application]),
                    {error, channel_hungup};
                { <<"error">>, _, _ } ->
                    ?LOG("channel execution error while waiting ~s or DTMF", [Application]),
                    {error, execution_failure};
                { <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, Application} ->
                    {ok, JObj};
                { <<"call_event">>, <<"DTMF">>, _ } ->
                    {dtmf, wh_json:get_value(<<"DTMF-Digit">>, JObj)};
                _ when Timeout =:= infinity ->
                    wait_for_application_or_dtmf(Application, Timeout);
                _ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_application_or_dtmf(Application, Timeout - (DiffMicro div 1000))
            end;
        _ when Timeout =:= infinity ->
            wait_for_application_or_dtmf(Application, Timeout);
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_application_or_dtmf(Application, Timeout - (DiffMicro div 1000))
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
-spec get_event_type/1 :: (JObj) -> {binary(), binary(), binary()} when
      JObj :: json_object().
get_event_type(JObj) ->
    { wh_json:get_value(<<"Event-Category">>, JObj, <<>>)
      ,wh_json:get_value(<<"Event-Name">>, JObj, <<>>)
      ,wh_json:get_value(<<"Application-Name">>, JObj, <<>>) }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
-spec send_callctrl/2 :: (Payload, Call) -> 'ok' when
      Payload :: binary(),
      Call :: #cf_call{}.
send_callctrl(Payload, #cf_call{ctrl_q=CtrlQ}) ->
    amqp_util:callctl_publish(CtrlQ, Payload).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Look for children branches to handle the failure replies of
%% certain actions, like cf_offnet and cf_resources
%% @end
%%--------------------------------------------------------------------
-spec find_failure_branch/2 :: (Failure, Call) -> boolean() when
      Failure :: {binary(), binary()} | binary(),
      Call :: #cf_call{}.
find_failure_branch({Cause, Code}, Call) ->
    find_failure_branch(Cause, Call)
        orelse find_failure_branch(Code, Call);

find_failure_branch(Error, #cf_call{cf_pid=CFPid}) ->
    CFPid ! {attempt, Error},
    receive
        {attempt_resp, ok} ->
            ?LOG("found child branch to handle failure code ~s", [Error]),
            true;
        {attempt_resp, _} ->
            false
    after
        1000 ->
            false
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%.
%% @end
%%--------------------------------------------------------------------
-spec update_fallback/2 :: (Bridge, Call) -> 'ok' when
      Bridge :: json_object(),
      Call :: #cf_call{}.
update_fallback(Bridge, Call) ->
    Fallback = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], Bridge) of
                   undefined ->
                       case b_fetch(true, Call) of
                           {ok, OtherLeg} ->
                               wh_json:get_value(<<"Endpoint-ID">>, OtherLeg);
                           _ ->
                               undefined
                       end;
                   AuthId ->
                       AuthId
               end,
    case Fallback =/= undefined of
        true ->
            io:format("set transfer fallback to ~s~n", [Fallback]),
            ?LOG("set transfer fallback to ~s", [Fallback]),
            set(undefined, {struct, [{<<"Transfer-Fallback">>, Fallback}]}, Call),
            ok;
        false ->
            ok
    end.
