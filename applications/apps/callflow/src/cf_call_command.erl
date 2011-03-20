%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 19 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_call_command).

-include("callflow.hrl").

-export([answer/1, hangup/1]).
-export([bridge/2, bridge/3, bridge/4, bridge/5, bridge/6, bridge/7, bridge/8]).
-export([play/2, play/3]).
-export([record/2, record/3, record/4, record/5, record/6]).
-export([tones/2]).
-export([play_and_collect_digit/2]).
-export([play_and_collect_digits/4, play_and_collect_digits/5, play_and_collect_digits/6, 
         play_and_collect_digits/7, play_and_collect_digits/8, play_and_collect_digits/9]).
-export([say/3, say/4, say/5]).
-export([conference/2, conference/3, conference/4, conference/5]).

-export([b_answer/1, b_hangup/1]).
-export([b_bridge/2, b_bridge/3, b_bridge/4, b_bridge/5, b_bridge/6, b_bridge/7, b_bridge/8]).
-export([b_play/2, b_play/3]).
-export([b_play_and_collect_digit/2]).
-export([b_play_and_collect_digits/4, b_play_and_collect_digits/5, b_play_and_collect_digits/6, 
         b_play_and_collect_digits/7, b_play_and_collect_digits/8, b_play_and_collect_digits/9]).
-export([b_conference/2, b_conference/3, b_conference/4, b_conference/5]).

-export([wait_for_message/1, wait_for_message/2, wait_for_message/3, wait_for_message/4]).
-export([wait_for_bridge/1, wait_for_unbridge/0]).
-export([wait_for_dtmf/1]).
-export([wait_for_hangup/0]).
-export([send_callctrl/2]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-define(APP_NAME, <<"cf_call_command">>).
-define(APP_VERSION, <<"0.5">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to answer the channel
%% @end
%%--------------------------------------------------------------------
-spec(answer/1 :: (Call :: #cf_call{}) -> ok | tuple(error, atom())).                       
-spec(b_answer/1 :: (Call :: #cf_call{}) -> ok | tuple(error, atom())).                       

answer(#cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"answer">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:answer_req(Command),
    send_callctrl(Json, Call).       

b_answer(Call) ->
    answer(Call),
    wait_for_message(<<"answer">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to hangup the channel.
%% This request will execute immediately
%% @end
%%--------------------------------------------------------------------
-spec(hangup/1 :: (Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(b_hangup/1 :: (Call :: #cf_call{}) -> ok | tuple(error, atom())).

hangup(#cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"hangup">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = whistle_api:hangup_req(Command),
    send_callctrl(Json, Call).

b_hangup(Call) ->
    hangup(Call),
    wait_for_hangup().


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to bridge the call
%% @end
%%--------------------------------------------------------------------
-spec(bridge/2 :: (Endpoints :: list(json_object()), Call :: #cf_call{}) -> ok).
-spec(bridge/3 :: (Endpoints :: list(json_object()), Timeout :: binary(), Call :: #cf_call{}) -> ok).
-spec(bridge/4 :: (Endpoints :: list(json_object()), Timeout :: binary(), Strategy :: binary(), Call :: #cf_call{}) -> ok).
-spec(bridge/5 :: (Endpoints :: list(json_object()), Timeout :: binary(), Strategy :: binary(), CIDNum :: binary(), Call :: #cf_call{}) -> ok).
-spec(bridge/6 :: (Endpoints :: list(json_object()), Timeout :: binary(), Strategy :: binary(), CIDNum :: binary(), CIDName :: binary(), Call :: #cf_call{}) -> ok).
-spec(bridge/7 :: (Endpoints :: list(json_object()), Timeout :: binary(), Strategy :: binary(), CIDNum :: binary(), CIDName :: binary(), ContinueOnFail :: binary(), Call :: #cf_call{}) -> ok).
-spec(bridge/8 :: (Endpoints :: list(json_object()), Timeout :: binary(), Strategy :: binary(), CIDNum :: binary(), CIDName :: binary(), ContinueOnFail :: binary(), Ringback :: binary(), Call :: #cf_call{}) -> ok).
-spec(b_bridge/2 :: (Endpoints :: list(json_object()), Call :: #cf_call{}) -> ok).
-spec(b_bridge/3 :: (Endpoints :: list(json_object()), Timeout :: binary(), Call :: #cf_call{}) -> ok).
-spec(b_bridge/4 :: (Endpoints :: list(json_object()), Timeout :: binary(), Strategy :: binary(), Call :: #cf_call{}) -> ok).
-spec(b_bridge/5 :: (Endpoints :: list(json_object()), Timeout :: binary(), Strategy :: binary(), CIDNum :: binary(), Call :: #cf_call{}) -> ok).
-spec(b_bridge/6 :: (Endpoints :: list(json_object()), Timeout :: binary(), Strategy :: binary(), CIDNum :: binary(), CIDName :: binary(), Call :: #cf_call{}) -> ok).
-spec(b_bridge/7 :: (Endpoints :: list(json_object()), Timeout :: binary(), Strategy :: binary(), CIDNum :: binary(), CIDName :: binary(), ContinueOnFail :: binary(), Call :: #cf_call{}) -> ok).
-spec(b_bridge/8 :: (Endpoints :: list(json_object()), Timeout :: binary(), Strategy :: binary(), CIDNum :: binary(), CIDName :: binary(), ContinueOnFail :: binary(), Ringback :: binary(), Call :: #cf_call{}) -> ok).

bridge(Endpoints, Call) ->
    bridge(Endpoints, <<"26">>, Call).
bridge(Endpoints, Timeout, Call) ->
    bridge(Endpoints, Timeout, <<"single">>, Call).
bridge(Endpoints, Timeout, Strategy, Call) ->
    bridge(Endpoints, Timeout, Strategy, <<>>, Call).
bridge(Endpoints, Timeout, Strategy, CIDNum, Call) ->
    bridge(Endpoints, Timeout, Strategy, CIDNum, <<>>, Call).
bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, Call) ->
    bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, <<"true">>, <<"us-ring">>, Call).
bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, ContinueOnFail, Call) ->
    bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, ContinueOnFail, <<"us-ring">>, Call).
bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, ContinueOnFail, Ringback, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Timeout">>, Timeout}
               ,{<<"Continue-On-Fail">>, ContinueOnFail}
               ,{<<"Outgoing-Caller-ID-Name">>, CIDName}
               ,{<<"Outgoing-Caller-ID-Number">>, CIDNum}
               ,{<<"Ringback">>, Ringback}
               ,{<<"Dial-Endpoint-Method">>, Strategy}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Json} = whistle_api:bridge_req(Command),
    send_callctrl(Json, Call).

b_bridge(Endpoints, Call) ->
    b_bridge(Endpoints, <<"26">>, Call).
b_bridge(Endpoints, Timeout, Call) ->
    b_bridge(Endpoints, Timeout, <<"single">>, Call).
b_bridge(Endpoints, Timeout, Strategy, Call) ->
    b_bridge(Endpoints, Timeout, Strategy, <<>>, Call).
b_bridge(Endpoints, Timeout, Strategy, CIDNum, Call) ->
    b_bridge(Endpoints, Timeout, Strategy, CIDNum, <<>>, Call).
b_bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, Call) ->
    b_bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, <<"true">>, <<"us-ring">>, Call).
b_bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, ContinueOnFail, Call) ->
    b_bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, ContinueOnFail, <<"us-ring">>, Call).
b_bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, ContinueOnFail, Ringback, Call) ->
    bridge(Endpoints, Timeout, Strategy, CIDNum, CIDName, ContinueOnFail, Ringback, Call),
    wait_for_bridge((whistle_util:to_integer(Timeout)*1000) + 5000).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to play media to the
%% caller.  A list of terminators can be provided that the caller 
%% can use to skip playback.
%% @end
%%--------------------------------------------------------------------
-spec(play/2 :: (Media :: binary(), Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(play/3 :: (Media :: binary(), Terminators :: list(binary()), Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(b_play/2 :: (Media :: binary(), Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(b_play/3 :: (Media :: binary(), Terminators :: list(binary()), Call :: #cf_call{}) -> ok | tuple(error, atom())).

play(Media, Call) ->
    play(Media, [<<"#">>], Call).
play(Media, Terminators, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"play">>}
               ,{<<"Media-Name">>, Media}
               ,{<<"Terminators">>, Terminators}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:play_req(Command),
    send_callctrl(Json, Call).

b_play(Media, Call) ->
    b_play(Media, [<<"#">>], Call).    
b_play(Media, Terminators, Call) ->
    play(Media, Terminators, Call),
    wait_for_message(<<"play">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to record a file.
%% A list of keys can be used as the terminator or a silence threshold.
%% @end
%%--------------------------------------------------------------------
-spec(record/2 :: (MediaName :: binary(), Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(record/3 :: (MediaName :: binary(), Terminators :: binary(), Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(record/4 :: (MediaName :: binary(), Terminators :: binary(), TimeLimit :: binary(), Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(record/5 :: (MediaName :: binary(), Terminators :: binary(), TimeLimit :: binary(), SilenceThreshold :: binary(), Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(record/6 :: (MediaName :: binary(), Terminators :: binary(), TimeLimit :: binary(), SilenceThreshold :: binary(), SilenceHits :: binary(), Call :: #cf_call{}) -> ok | tuple(error, atom())).

record(MediaName, Call) ->
    record(MediaName, ["#","1","2","3","4","5","6","7","8","9","0","*"], Call).
record(MediaName, Terminators, Call) ->
    record(MediaName, Terminators, <<"20">>, Call).
record(MediaName, Terminators, TimeLimit, Call) ->
    record(MediaName, Terminators, TimeLimit, <<"200">>,  Call).
record(MediaName, Terminators, TimeLimit, SilenceThreshold, Call) ->
    record(MediaName, Terminators, TimeLimit, SilenceThreshold, <<"3">>, Call).
record(MediaName, Terminators, TimeLimit, SilenceThreshold, SilenceHits, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"record">>}
               ,{<<"Media-Name">>, MediaName}
               ,{<<"Terminators">>, Terminators}
               ,{<<"Time-Limit">>, TimeLimit}
               ,{<<"Silence-Threshold">>, SilenceThreshold}
               ,{<<"Silence-Hits">>, SilenceHits}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = whistle_api:record_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to play tones to the 
%% caller
%% @end
%%--------------------------------------------------------------------
-spec(tones/2 :: (Tones :: list(json_object()), Call :: #cf_call{}) -> ok | tuple(error, atom())).

tones(Tones, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"tones">>}
               ,{<<"Tones">>, Tones}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = whistle_api:tones_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to play media to a
%% caller, and collect a number of DTMF events.
%% @end
%%--------------------------------------------------------------------
-spec(play_and_collect_digit/2 :: (Media :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(play_and_collect_digits/4 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(play_and_collect_digits/5 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Tries :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(play_and_collect_digits/6 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Tries :: binary(), Timeout :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(play_and_collect_digits/7 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Tries :: binary(), Timeout :: binary(), MediaInvalid :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(play_and_collect_digits/8 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Tries :: binary(), Timeout :: binary(), MediaInvalid :: binary(), Regex :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(play_and_collect_digits/9 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Tries :: binary(), Timeout :: binary(), MediaInvalid :: binary(), Regex :: binary(), Terminators :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_play_and_collect_digit/2 :: (Media :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_play_and_collect_digits/4 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_play_and_collect_digits/5 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Tries :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_play_and_collect_digits/6 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Tries :: binary(), Timeout :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_play_and_collect_digits/7 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Tries :: binary(), Timeout :: binary(), MediaInvalid :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_play_and_collect_digits/8 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Tries :: binary(), Timeout :: binary(), MediaInvalid :: binary(), Regex :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_play_and_collect_digits/9 :: (MinDigits :: binary(), MaxDigits :: binary(), Media :: binary(), Tries :: binary(), Timeout :: binary(), MediaInvalid :: binary(), Regex :: binary(), Terminators :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
 
play_and_collect_digit(Media, Call) ->
    play_and_collect_digits(<<"1">>, <<"1">>, Media, Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, <<"3">>,  Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, <<"3000">>, Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, <<"silence_stream://250">>, Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, <<"\\d+">>, Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, [<<"#">>], Call).
play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"play_and_collect_digits">>}
               ,{<<"Minimum-Digits">>, MinDigits}
               ,{<<"Maximum-Digits">>, MaxDigits}
               ,{<<"Timeout">>, Timeout}
               ,{<<"Terminators">>, Terminators}
               ,{<<"Media-Name">>, Media}
               ,{<<"Media-Tries">>, Tries}
               ,{<<"Failed-Media-Name">>, MediaInvalid}
               ,{<<"Digits-Regex">>, Regex}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = whistle_api:play_collect_digits_req(Command),
    send_callctrl(Json, Call).

b_play_and_collect_digit(Media, Call) ->
    b_play_and_collect_digits(<<"1">>, <<"1">>, Media, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, <<"3">>,  Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, <<"3000">>, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, <<"silence_stream://250">>, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, <<"\\d+">>, Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Call) ->
    b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, [<<"#">>], Call).
b_play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Tries, Timeout, MediaInvalid, Regex, Terminators, Call),
    Wait = (whistle_util:to_integer(Timeout) * whistle_util:to_integer(Tries)) + 5000,
    case wait_for_message(<<"play_and_collect_digits">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, Wait) of
        {ok, Msg} ->
            {ok, get_value(<<"Application-Response">>, Msg)};
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to say text to a caller
%% @end
%%--------------------------------------------------------------------
-spec(say/3 :: (Say :: binary(), Type :: binary(), Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(say/4 :: (Say :: binary(), Type :: binary(), Method :: binary(), Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(say/5 :: (Say :: binary(), Type :: binary(), Method :: binary(), Language :: binary(), Call :: #cf_call{}) -> ok | tuple(error, atom())).

say(Say, Type, Call) -> 
    say(Say, Type, <<"pronounced">>, <<"en">>, Call).
say(Say, Type, Method, Call) -> 
    say(Say, Type, Method, <<"en">>, Call).
say(Say, Type, Method, Language, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) -> 
    Command = [
                {<<"Application-Name">>, <<"say">>}
               ,{<<"Say-Text">>, Say}
               ,{<<"Type">>, Type}
               ,{<<"Method">>, Method}
               ,{<<"Language">>, Language}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:say_req(Command),
    send_callctrl(Json, Call).       

%%--------------------------------------------------------------------
%% @private
%% @doc 
%% Produces the low level whistle_api request to bridge a caller
%% with a conference, with optional entry flags
%% @end
%%--------------------------------------------------------------------
-spec(conference/2 :: (ConfId :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(conference/3 :: (ConfId :: binary(), Mute :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(conference/4 :: (ConfId :: binary(), Mute :: binary(), Deaf :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(conference/5 :: (ConfId :: binary(), Mute :: binary(), Deaf :: binary(), Moderator :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_conference/2 :: (ConfId :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_conference/3 :: (ConfId :: binary(), Mute :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_conference/4 :: (ConfId :: binary(), Mute :: binary(), Deaf :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).
-spec(b_conference/5 :: (ConfId :: binary(), Mute :: binary(), Deaf :: binary(), Moderator :: binary(), Call :: #cf_call{}) -> tuple(ok, binary()) | tuple(error, atom())).

conference(ConfId, Call) ->
    conference(ConfId, <<"false">>, Call).
conference(ConfId, Mute, Call) ->
    conference(ConfId, Mute, <<"false">>, Call).
conference(ConfId, Mute, Deaf, Call) ->
    conference(ConfId, Mute, Deaf, <<"false">>, Call).
conference(ConfId, Mute, Deaf, Moderator, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"conference">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Mute">>, Mute}
               ,{<<"Deaf">>, Deaf}
               ,{<<"Moderator">>, Moderator}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_req(Command),
    send_callctrl(Json, Call).

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
%% @private
%% @doc
%% Low level function to consume call events, looping until a specific
%% one occurs.  If the channel is hungup or no call events are recieved
%% for the optional timeout period then errors are returned.
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_message/1 :: (Application :: binary()) -> tuple(ok, json_object()) | tuple(error, atom())).
-spec(wait_for_message/2 :: (Application :: binary(), Event :: binary()) -> tuple(ok, json_object()) | tuple(error, atom())).
-spec(wait_for_message/3 :: (Application :: binary(), Event :: binary(), Type :: binary()) -> tuple(ok, json_object()) | tuple(error, atom())).
-spec(wait_for_message/4 :: (Application :: binary(), Event :: binary(), Type :: binary(), Timeout :: integer()) -> tuple(ok, json_object()) | tuple(error, atom())).

wait_for_message(Application) ->
    wait_for_message(Application, <<"CHANNEL_EXECUTE_COMPLETE">>).    
wait_for_message(Application, Event) ->
    wait_for_message(Application, Event, <<"call_event">>).    
wait_for_message(Application, Event, Type) ->
    wait_for_message(Application, Event, Type, 5000).    
wait_for_message(Application, Event, Type, Timeout) ->
    receive
        {amqp_msg, {struct, Msg}} ->
            case { get_value(<<"Application-Name">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Event-Category">>, Msg) } of
                { Application, Event, Type } ->
                    {ok, Msg};
                { _Name, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
                    {error, channel_hungup};
                _ ->
                    wait_for_message(Application, Event, Type, Timeout)
            end
    after
        Timeout ->
            {error, timeout}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wait for a DTMF event and extract the digits when it comes
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_dtmf/1 :: (Timeout :: integer()) -> tuple(ok, binary()) | tuple(error, atom())).    
wait_for_dtmf(Timeout) ->
    receive
        {amqp_msg, {struct, Msg}} ->
            case { get_value(<<"Event-Name">>, Msg), get_value(<<"Event-Category">>, Msg) } of
                { <<"DTMF">>, <<"call_event">> } ->
                    {ok, get_value(<<"DTMF-Digit">>, Msg)};
                { <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
                    {error, channel_hungup};
                _ ->
                    wait_for_dtmf(Timeout)
            end
    after
        Timeout ->
            {error, timeout}
    end.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for and determines the status of the bridge command
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_bridge/1 :: (Timeout :: integer()) -> tuple(ok, json_object()) | tuple(error, atom())).    
wait_for_bridge(Timeout) -> 
    receive
        {amqp_msg, {struct, Msg}} ->
            case { get_value(<<"Application-Name">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Event-Category">>, Msg) } of
                { _, <<"CHANNEL_BRIDGE">>, <<"call_event">> } ->
                    {ok, Msg};
                { _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
                    {error, channel_hungup};
                { <<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
                    {error, bridge_failed};
                _ ->
                    wait_for_bridge(Timeout)
            end
    after
        Timeout ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wait forever for the channel to hangup
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_unbridge/0 :: () -> tuple(ok, channel_hangup)).    
wait_for_unbridge() -> 
    receive
        {amqp_msg, {struct, Msg}} ->
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg) } of
                { <<"call_event">>, <<"CHANNEL_UNBRIDGE">> } ->
                    {ok, channel_unbridge};
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
                    {ok, channel_hungup};
                _ ->
                    wait_for_unbridge()
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wait forever for the channel to hangup
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_hangup/0 :: () -> tuple(ok, channel_hangup)).    
wait_for_hangup() -> 
    receive
        {amqp_msg, {struct, Msg}} ->
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg) } of
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
                    {ok, channel_hungup};
                _ ->
                    wait_for_hangup()
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
-spec(send_callctrl/2 :: (JSON :: json_object(), Call :: #cf_call{}) -> ok | tuple(error, atom())).
send_callctrl(Json, #cf_call{amqp_h=AHost, ctrl_q=CtrlQ}) ->
    amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>).
