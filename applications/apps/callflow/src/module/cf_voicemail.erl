%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_voicemail).

-include("../callflow.hrl").

-export([handle/2]).

-define(APP_NAME, <<"cf_voicemail">>).
-define(APP_VERSION, <<"0.5">>).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-define(DEFAULT_TIMEOUT, 30).
-define(VM_SOUNDS, "/usr/local/freeswitch/sounds/en/us/callie/voicemail/8000/").

-define(PROMPT_GENERIC_GREETING, <<"phrase:voicemail_play_greeting">>).
-define(PROMPT_RECORD_MESSAGE, <<"phrase:voicemail_record_message">>).
-define(PROMPT_RECORD_REVIEW, <<"phrase:voicemail_record_file_check">>).

-record(keys, {
           %% Compose Voicemail
           operator = <<"0">>
          ,login = <<"*">>
              
           %% Record Review
          ,listen = <<"1">>
          ,save = <<"2">>
          ,record = <<"3">>

           %% Urgent
          ,mark_urgent = <<"5">>
         }).

-record(mailbox, {
           prompt_greeting = undefined
          ,prompt_generic_greeting = ?PROMPT_GENERIC_GREETING
          ,prompt_instructions = ?PROMPT_RECORD_MESSAGE
          ,prompt_record_review = ?PROMPT_RECORD_REVIEW
          ,file_id = undefined
          ,database = undefined
          ,mailbox_id = undefined
          ,action = undefined
          ,rev = undefined
          ,skip_instructions = <<"false">>
          ,skip_greeting = <<"false">>
          ,tone_spec = [{struct, [{<<"Frequencies">>, [440]},{<<"Duration-ON">>, 500},{<<"Duration-OFF">>, 100}]}]
          ,keys = #keys{}
         }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, based on the payload will either
%% connect a caller to check_voicemail or compose_voicemail.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> stop | continue).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->
    Box = get_mailbox(Data),
    if
        Box#mailbox.action == <<"compose">> ->
            answer(Call),
            CFPid ! compose_voicemail(Box, Call);
        Box#mailbox.action == <<"check">> ->
            answer(Call),
            CFPid ! check_voicemail(Box, Call);
        true ->
            CFPid ! {continue}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the mailbox parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec(get_mailbox/1 :: (Data :: json_object()) -> #mailbox{} | tuple(stop)).
get_mailbox({struct, Props}) ->
    Db = get_value(<<"database">>, Props),
    Id = get_value(<<"id">>, Props),
    case couch_mgr:open_doc(Db, Id) of
        {ok, Doc} ->
            #mailbox{
                       file_id = list_to_binary(whistle_util:to_hex(crypto:rand_bytes(16)))
                      ,database = Db
                      ,mailbox_id = Id
                      ,action = get_value(<<"action">>, Props)
                      ,rev = whapps_json:get_value(["_rev"], Doc)
                    };
        _-> 
            #mailbox{}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The entry point for checking a voicemail box
%% @end
%%--------------------------------------------------------------------
-spec(check_voicemail/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> cf_exe_response()).
check_voicemail(_Box, _Call) ->
    {stop}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The entry point for checking a voicemail box, typically plays the 
%% voicemail greeting then goes to the voicemail instructions. However
%% the greeting can be optionaly skipped.  Any digits skip to record
%% unless they match the operator or login keys.
%% @end
%%--------------------------------------------------------------------
-spec(compose_voicemail/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> cf_exe_response()).
compose_voicemail(#mailbox{skip_greeting = <<"true">>}=Box, Call) ->
    play_instructions(Box, Call);
compose_voicemail(#mailbox{prompt_greeting=undefined, prompt_generic_greeting=GenericGreeting}=Box, Call) ->
    Greeting = case binary:split(get_value(<<"To">>, Call#cf_call.route_request), <<"@">>) of
                   [Number|_] ->
                       <<GenericGreeting/binary, $:, Number/binary>>;
                   _ ->
                       GenericGreeting
               end,
    compose_voicemail(Box#mailbox{prompt_greeting = Greeting}, Call);
compose_voicemail(#mailbox{prompt_greeting=Greeting, keys=Keys}=Box, Call) ->
    case play_and_collect_digits(<<"1">>, <<"1">>, Greeting, <<"1">>, <<"0">>, Call) of
        {ok, Digits} ->
            if
                Digits == <<>> ->
                    play_instructions(Box, Call);
                Digits == Keys#keys.operator ->
                    {hunt, operator};
                Digits == Keys#keys.login ->
                    check_voicemail(Box, Call);
                true ->
                    record_message(Box, Call)
            end;
        _Else ->
            {stop}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Typically plays the voicemail  recording instructions then goes to the 
%% record message function. However the instructions can be optionaly 
%% skipped.  Any digits skip to record unless they match the operator 
%% or login keys.
%% @end
%%--------------------------------------------------------------------
-spec(play_instructions/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> cf_exe_response()).
play_instructions(#mailbox{skip_instructions = <<"true">>}=Box, Call) ->
    record_message(Box, Call);
play_instructions(#mailbox{prompt_instructions=Instructions, keys=Keys}=Box, Call) ->
    case play_and_collect_digits(<<"1">>, <<"1">>, Instructions, <<"1">>, <<"0">>, Call) of
        {ok, Digits} ->
            if
                Digits == Keys#keys.operator ->
                    {hunt, operator};
                Digits == Keys#keys.login ->
                    check_voicemail(Box, Call);
                true ->
                    record_message(Box, Call)
            end;
        _Else ->
            {continue}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Records a voicemail message.  If the channel is hungup at this stage
%% then the message is stored.  Any key press will end the recording
%% and, unless that key matches the operator or login, the caller will
%% be given review options for the voicemail.
%% @end
%%--------------------------------------------------------------------
-spec(record_message/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> cf_exe_response()).
record_message(#mailbox{keys=Keys}=Box, Call) ->
    play_tones(Box, Call),
    record_file(Box, Call),
    case wait_for_call_event(<<"RECORD_STOP">>, <<"record">>) of
        {ok, Msg} ->
            Digits = get_value(<<"Terminator">>, Msg),
            if
                Digits == Keys#keys.operator ->
                    {hunt, operator};
                Digits == Keys#keys.login ->
                    check_voicemail(Box, Call);
                true ->
                    message_options(Box, Call)
            end;
        _Else ->                
            store(Box, Call),
            {stop}                
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Provides review options for a recorded message, the caller can 
%% choose to listen, re-record, or save the message.  Optionally
%% they can also login to the mailbox or be transfered to the operator
%% @end
%%--------------------------------------------------------------------
-spec(message_options/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> cf_exe_response()).
message_options(#mailbox{file_id=FileId, keys=Keys}=Box, Call) ->
    Prompt = <<(Box#mailbox.prompt_record_review)/binary, $:, (Keys#keys.listen)/binary, $:, (Keys#keys.save)/binary, $:, (Keys#keys.record)/binary>>,
    case play_and_collect_digits(<<"1">>, <<"1">>, Prompt, <<"1">>, <<"0">>, Call) of
        {ok, Digits} ->
            if
                Digits == Keys#keys.operator ->
                    {hunt, operator};
                Digits == Keys#keys.listen ->
                    play_and_collect_digits(<<"1">>, <<"1">>, FileId, <<"1">>, <<"0">>, Call),
                    message_options(Box, Call);
                Digits == Keys#keys.record ->
                    record_message(Box, Call);
                true ->
                    store(Box, Call),
                    {stop}                
            end;
        _Else ->
            store(Box, Call),
            {stop}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to record a file, using
%% any key as the terminator.
%% @end
%%--------------------------------------------------------------------
-spec(record_file/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
record_file(#mailbox{file_id=FileId}, #cf_call{call_id=CallId}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"record">>}
               ,{<<"Media-Name">>, FileId}
               ,{<<"Terminators">>, ["#","1","2","3","4","5","6","7","8","9","0","*"]}
               ,{<<"Time-Limit">>, <<"120">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = whistle_api:record_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to store the file as a
%% couchdb attachement on the voicemail defintion document
%% @end
%%--------------------------------------------------------------------
-spec(store/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
store(#mailbox{database=Db, mailbox_id=Id, file_id=FileId, rev=Rev}, #cf_call{call_id=CallId}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"store">>}
               ,{<<"Media-Name">>, FileId}
               ,{<<"Media-Transfer-Method">>, <<"put">>}
               ,{<<"Media-Transfer-Destination">>, <<(couch_mgr:get_url())/binary
                                                     ,Db/binary
                                                     ,$/, Id/binary
                                                     ,$/, FileId/binary
                                                     ,"?rev=", Rev/binary
                                                   >>}
               ,{<<"Additional-Headers">>, [{struct, [{<<"Content-Type">>, <<"audio/x-wav">>}]}]}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = whistle_api:store_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to play a tone to the 
%% caller, used to denote when recording is about to begin
%% @end
%%--------------------------------------------------------------------
-spec(play_tones/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
play_tones(#mailbox{tone_spec=Tones}, #cf_call{call_id=CallId}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"tone">>}
               ,{<<"Tones">>, Tones}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = whistle_api:tones_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to answer the channel 
%% and begin exchanging media with the caller
%% @end
%%--------------------------------------------------------------------
-spec(answer/1 :: (Call :: #cf_call{}) -> no_return()).
answer(#cf_call{call_id=CallId}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"answer">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = whistle_api:answer_req(Command),
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

play_and_collect_digits(MinDigits, MaxDigits, Media, Retries, Timeout, MediaInvalid, Regex, Terminators, #cf_call{call_id=CallId}=Call) ->
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
               | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = whistle_api:play_collect_digits_req(Command),
    send_callctrl(Json, Call),
    case wait_for_call_event(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"play_and_collect_digits">>) of
        {ok, Msg} ->
            {ok, get_value(<<"Application-Response">>, Msg)};
        _Else ->
            {stop}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Low level function to consume call events, looping until a specific
%% one occurs.  If the channel is hungup or no call events are recieved
%% for the optional timeout period then errors are returned.
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_call_event/2 :: (Name :: binary(), Application :: binary()) -> tuple(ok, json_object()) | tuple(error, atom())).            
wait_for_call_event(Name, Application) ->    
    receive
        {call_event, {struct, Msg}} ->
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                { <<"call_event">>, Name, Application } ->                                
                    {ok, Msg};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _Name } ->
                    {error, channel_hungup};
                _ ->
                    wait_for_call_event(Name, Application)
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
