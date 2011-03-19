%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 16 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_conference).

-include("../callflow.hrl").

-export([handle/2]).

-define(APP_NAME, <<"cf_conference">>).
-define(APP_VERSION, <<"0.5">>).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-record(prompts, {
           greeting = <<"shout://translate.google.com/translate_tts?tl=en&q=Welcome+to+the+conference!">> 
          ,request_pin = <<"shout://translate.google.com/translate_tts?tl=en&q=Please+enter+the+conference+pin+number.">>         
          ,alone_enter = <<"shout://translate.google.com/translate_tts?tl=en&q=You+are+currently+the+only+participant.">>
          ,single_enter = <<"shout://translate.google.com/translate_tts?tl=en&q=There+is+only+one+other+participant.">>
          ,multiple_enter = <<"shout://translate.google.com/translate_tts?tl=en&q=There+are+~p+other+participants.">>
          ,announce_join = <<"tone_stream://%(200,0,500,600,700)">>
          ,announce_leave = <<"tone_stream://%(500,0,300,200,100,50,25)">>
         }).

-record(conf, {
           id = <<"fe306820-51c4-11e0-b8af-0800200c9a66">>
          ,members = []
          ,member_pins=[<<"1234">>]
          ,moderator_pins=[]
          ,prompts=#prompts{}
         }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or 
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> stop | continue).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->
    Conf = members(#conf{}, Call),
    answer(Call),
    play_conference_name(Conf, Call),
    grant_access(Conf, Call),
    play_conference_count(Conf, Call),
    conference(Conf, Call),
    wait_for_message(<<"CHANNEL_EXECUTE">>, <<"conference">>),
    announce_join(Conf, Call),
    wait_for_hangup(),
    announce_leave(Conf, Call),
    stop.

play_conference_name(#conf{prompts=Prompts}, Call) ->
    play(Prompts#prompts.greeting, Call).

grant_access(#conf{prompts=Prompts} = Conf, Call) ->
    case play_and_collect_digits(<<"1">>, <<"6">>, Prompts#prompts.request_pin, Call) of
        {ok, Pin} ->
            format_log(info, "member(~p, ~p) = ~p", [Pin, Conf#conf.member_pins, lists:member(Pin, Conf#conf.member_pins)]),
            case lists:member(Pin, Conf#conf.member_pins) of
                true -> ok;
                false -> grant_access(Conf, Call)
            end;
        _ ->
            grant_access(Conf, Call)
    end.

play_conference_count(#conf{prompts=Prompts, members=Members}, Call) ->
    case length(Members) of 
        0 ->
            play(Prompts#prompts.alone_enter, Call);
        1 ->
            play(Prompts#prompts.single_enter, Call);
        Count ->            
            Prompt = io_lib:format(whistle_util:to_list(Prompts#prompts.multiple_enter), [Count]),
            play(whistle_util:to_binary(Prompt), Call)
    end.

announce_join(#conf{prompts=Prompts} = Conf, Call) ->
    conference_play(Prompts#prompts.announce_join, Conf, Call).

announce_leave(#conf{prompts=Prompts} = Conf, Call) ->
    conference_play(Prompts#prompts.announce_leave, Conf, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to connect to a conference
%% @end
%%--------------------------------------------------------------------
-spec(conference/2 :: (Conference :: #conf{}, Call :: #cf_call{}) -> ok).
conference(#conf{id=ConfId}, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"conference">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to mute a memeber
%% @end
%%--------------------------------------------------------------------
-spec(members/2 :: (Conference :: #conf{}, Call :: #cf_call{}) -> ok).
members(#conf{id=ConfId} = Conf, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->        
    Command = [
                {<<"Application-Name">>, <<"members">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_members_req(Command),
    send_callctrl(Json, Call),
    Members = case wait_for_message(<<"members">>, <<"response">>, <<"conference">>) of
                  {ok, Response} ->
                      get_value(<<"Members">>, Response, []);
                  {error, _} -> 
                      []
              end,                               
    Conf#conf{members=Members}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to mute a memeber
%% @end
%%--------------------------------------------------------------------
-spec(conference_play/3 :: (Media :: binary(), Conference :: #conf{}, Call :: #cf_call{}) -> ok).
conference_play(Media, #conf{id=ConfId}, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"play">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Media-Name">>, Media}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_play_req(Command),
    send_callctrl(Json, Call).
        
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to mute a memeber
%% @end
%%--------------------------------------------------------------------
-spec(mute_member/3 :: (MemberID :: binary(), Conference :: #conf{}, Call :: #cf_call{}) -> ok).
mute_member(MemberId, #conf{id=ConfId}, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"mute">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_mute_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to unmute a memeber
%% @end
%%--------------------------------------------------------------------
-spec(unmute_member/3 :: (MemberID :: binary(), Conference :: #conf{}, Call :: #cf_call{}) -> ok).
unmute_member(MemberId, #conf{id=ConfId}, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"unmute">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_unmute_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to deaf a memeber
%% @end
%%--------------------------------------------------------------------
-spec(deaf_member/3 :: (MemberID :: binary(), Conference :: #conf{}, Call :: #cf_call{}) -> ok).
deaf_member(MemberId, #conf{id=ConfId}, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"deaf">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_deaf_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to undeaf a memeber
%% @end
%%--------------------------------------------------------------------
-spec(undeaf_member/3 :: (MemberID :: binary(), Conference :: #conf{}, Call :: #cf_call{}) -> ok).
undeaf_member(MemberId, #conf{id=ConfId}, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"undeaf">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_undeaf_req(Command),
    send_callctrl(Json, Call).













%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to answer a call
%% @end
%%--------------------------------------------------------------------
answer(#cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"answer">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:answer_req(Command),
    send_callctrl(Json, Call).       

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to answer a call
%% @end
%%--------------------------------------------------------------------
play(Media, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"play">>}
               ,{<<"Media-Name">>, Media}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:play_req(Command),
    send_callctrl(Json, Call).

play_and_collect_digit(Media, Call) ->
    play_and_collect_digits(<<"1">>, <<"1">>, Media, Call).

play_and_collect_digits(MinDigits, MaxDigits, Media, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, <<"3">>, <<"3000">>, <<"silence_stream://250">>, <<"\\d+">>, [<<"#">>], Call).

play_and_collect_digits(MinDigits, MaxDigits, Media, Retries, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Retries, <<"3000">>, <<"silence_stream://250">>, <<"\\d+">>, [<<"#">>], Call).

play_and_collect_digits(MinDigits, MaxDigits, Media, Retries, Timeout, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Retries, Timeout, <<"silence_stream://250">>, <<"\\d+">>, [<<"#">>], Call).

play_and_collect_digits(MinDigits, MaxDigits, Media, Restries, Timeout, MediaInvalid, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Restries, Timeout, MediaInvalid, <<"\\d+">>, [<<"#">>], Call).

play_and_collect_digits(MinDigits, MaxDigits, Media, Retries, Timeout, MediaInvalid, Regex, Call) ->
    play_and_collect_digits(MinDigits, MaxDigits, Media, Retries, Timeout, MediaInvalid, Regex, [<<"#">>], Call).

play_and_collect_digits(MinDigits, MaxDigits, Media, Retries, Timeout, MediaInvalid, Regex, Terminators, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"play_and_collect_digits">>}
               ,{<<"Minimum-Digits">>, MinDigits}
               ,{<<"Maximum-Digits">>, MaxDigits}
               ,{<<"Timeout">>, Timeout}
               ,{<<"Terminators">>, Terminators}
               ,{<<"Media-Name">>, Media}
               ,{<<"Media-Tries">>, Retries}
               ,{<<"Failed-Media-Name">>, MediaInvalid}
               ,{<<"Digits-Regex">>, Regex}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = whistle_api:play_collect_digits_req(Command),
    send_callctrl(Json, Call),
    case wait_for_message(<<"play_and_collect_digits">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">>, whistle_util:to_integer(Timeout) + 1000) of
        {ok, Msg} ->
            {ok, get_value(<<"Application-Response">>, Msg)};
        _Else ->
            {stop}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to answer a call
%% @end
%%--------------------------------------------------------------------
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
%% Low level function to consume call events, looping until a specific
%% one occurs.  If the channel is hungup or no call events are recieved
%% for the optional timeout period then errors are returned.
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_message/2 :: (Name :: binary(), Application :: binary()) -> tuple(ok, json_object()) | tuple(error, atom())).            
wait_for_message(Application) ->
    wait_for_message(Application, <<"CHANNEL_EXECUTE_COMPLETE">>).    

wait_for_message(Application, Event) ->
    wait_for_message(Application, Event, <<"call_event">>).    

wait_for_message(Application, Event, Type) ->
    wait_for_message(Application, Event, Type, 5000).    

wait_for_message(Application, Event, Type, Timeout) ->    
    receive
        {amqp_msg, {struct, Msg}} ->
            format_log(info, "CF_CONFERENCE Rx ~p", [Msg]),
            case { get_value(<<"Application-Name">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Event-Category">>, Msg) } of
                { Application, Event, Type } ->                                
                    format_log(info, "CF_CONFERENCE FOUND ~p", [Msg]),
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
%% When the bridge command is successfull this waits for the call to
%% hangup
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_hangup/0 :: () -> tuple(ok, channel_hangup)).    
wait_for_hangup() -> 
    receive
        {amqp_msg, {struct, Msg}} ->
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
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
-spec(send_callctrl/2 :: (JSON :: json_object(), Call :: #cf_call{}) -> no_return()).
send_callctrl(Json, #cf_call{amqp_h=AHost, ctrl_q=CtrlQ}) ->
    amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>).
