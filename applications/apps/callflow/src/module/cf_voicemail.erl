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

-define(PROMPT_GENERIC_GREETING, <<"phrase:voicemail_play_greeting:1005">>).
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
           prompt_greeting = ?PROMPT_GENERIC_GREETING
          ,prompt_instructions = ?PROMPT_RECORD_MESSAGE
          ,prompt_record_review = ?PROMPT_RECORD_REVIEW
          ,file_id = undefined
          ,database = undefined
          ,mailbox_id = undefined
          ,rev = undefined
          ,skip_instructions = <<"false">>
          ,skip_greeting = <<"false">>
          ,tone_spec = [{struct, [{<<"Frequencies">>, [440]},{<<"Duration-ON">>, 500},{<<"Duration-OFF">>, 100}]}]
          ,keys = #keys{}
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
handle(Props, #cf_call{call_id=CallId, cf_pid=CFPid, amqp_h=AHost}=Call) ->
    init_amqp(Call),
    answer(Call),
    compose_voicemail(get_mailbox(Props, Call), Call),
    CFPid ! {stop}.

get_mailbox(Props, Call) ->
    Db = <<"crossbar%2Fclients%2F58%2F39%2Fa3ed904ecdaeedebc8af6a1b7a1f">>,
    Mailbox = <<"7818711acb94ddc2f5ac26da6c7c8731">>,
    case couch_mgr:open_doc(Db, Mailbox) of
        {ok, Doc} ->
            #mailbox{
                       file_id = list_to_binary(whistle_util:to_hex(crypto:rand_bytes(16)))
                      ,database = Db
                      ,mailbox_id = Mailbox
                      ,rev = whapps_json:get_value(["_rev"], Doc)
                    };
        _-> 
            error
    end.

compose_voicemail(#mailbox{skip_greeting = <<"true">>}=Box, Call) ->
    play_instructions(Box, Call);

compose_voicemail(Box, Call) ->
    Oper = Box#mailbox.keys#keys.operator,
    case play_and_collect_digits(<<"1">>, <<"1">>, Box#mailbox.prompt_greeting, <<"1">>, <<"0">>, Call) of
        {ok, Oper} ->
            format_log(info, "Transfer the caller to operator~n", []),
            {hunt, operator};
        {ok, <<>>} ->
            play_instructions(Box, Call);
        {ok, _} ->
            record_message(Box, Call);
        Else ->
            Else
    end.

play_instructions(#mailbox{skip_instructions = <<"true">>}=Box, Call) ->
    record_message(Box, Call);

play_instructions(Box, Call) ->
    Oper = Box#mailbox.keys#keys.operator,
    case play_and_collect_digits(<<"1">>, <<"1">>, Box#mailbox.prompt_instructions, <<"1">>, <<"0">>, Call) of
        {ok, Oper} ->
            format_log(info, "Transfer the caller to operator~n", []),
            {hunt, operator};
        {ok, _} ->
            record_message(Box, Call);
        Else ->
            Else
    end.

record_message(Box, Call) ->
    play_tones(Box#mailbox.tone_spec, Call),
    record_file(Box#mailbox.file_id, Call),
    case wait_for_call_event(<<"RECORD_STOP">>, <<"record">>, 30000) of
        {ok, Msg} ->
            Oper = Box#mailbox.keys#keys.operator,
            case get_value(<<"Terminator">>, Msg) of
                Oper -> 
                    format_log(info, "Transfer the caller to operator~n", []),
                    {hunt, operator};        
                _ ->
                    message_options(Box, Call)
            end;                
        {error, channel_hungup} ->            
            format_log(info, "Channel hungup, store in phase 1~n", []),
            {stop};
        _ ->                
            format_log(info, "Terminated~n", []),
            {stop}                
    end.

message_options(Box, Call) ->
    Keys = Box#mailbox.keys,
    Oper = Keys#keys.operator,
    Listen = Keys#keys.listen,
    Save = Keys#keys.save,
    Record = Keys#keys.record,
    Tmp  = Box#mailbox.prompt_record_review,
    Prompt = <<Tmp/binary, $:, Listen/binary, $:, Save/binary, $:, Record/binary>>, %%        io_lib:format(Box#mailbox.prompt_record_review, [Listen, Save, Record]),
    case play_and_collect_digits(<<"1">>, <<"1">>, Prompt, <<"1">>, <<"0">>, Call) of
        {ok, Oper} ->
            format_log(info, "Store voicemail then transfer the caller to operator~n", []),
            {hunt, operator};
        {ok, Listen} ->
            play_and_collect_digits(<<"1">>, <<"1">>, Box#mailbox.file_id, <<"1">>, <<"0">>, Call),
            message_options(Box, Call);
        {ok, Record} ->
            record_message(Box, Call);
        _ ->
            store(Box, Call),
            format_log(info, "Store voicemail~n", []),
            {stop}
    end.

record_file(FileId, #cf_call{call_id=CallId}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"record">>}
               ,{<<"Media-Name">>, FileId}
               ,{<<"Terminators">>, ["#","1","2","3","4","5","6","7","8","9","0","*"]}
               ,{<<"Time-Limit">>, <<"120">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    format_log(info, "CF_VOICEMAIL(~p): Sent command~nPayload: ~p~n", [self(), Command]),
    {ok, Json} = whistle_api:record_req(Command),
    send_callctrl(Json, Call).

store(#mailbox{database = Db, mailbox_id = Id, file_id = FileId, rev = Rev}, #cf_call{call_id=CallId}=Call) ->
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
    format_log(info, "CF_VOICEMAIL(~p): Sent command~nPayload: ~p~n", [self(), Command]),
    {ok, Json} = whistle_api:store_req(Command),
    send_callctrl(Json, Call).

play_tones(Tones, #cf_call{call_id=CallId}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"tone">>}
               ,{<<"Tones">>, Tones}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    format_log(info, "CF_VOICEMAIL(~p): Sent command~nPayload: ~p~n", [self(), Command]),
    {ok, Json} = whistle_api:tones_req(Command),
    send_callctrl(Json, Call).

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
    format_log(info, "CF_VOICEMAIL(~p): Sent command~nPayload: ~p~n", [self(), Command]),
    {ok, Json} = whistle_api:play_collect_digits_req(Command),
    send_callctrl(Json, Call),
    case wait_for_call_event(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"play_and_collect_digits">>, 30000) of
        {ok, Msg} ->
            {ok, get_value(<<"Application-Response">>, Msg)};
        Else ->
            Else
    end.
            
wait_for_call_event(Name, Application, Timeout) ->    
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->            
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
            format_log(info, "CF_VOICEMAIL(~p): Recieved~nPayload: ~p~n", [self(), Msg]),
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                { <<"call_event">>, Name, Application } ->                                
                    {ok, Msg};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _Name } ->
                    {error, channel_hungup};
                _ ->
                    wait_for_call_event(Name, Application, Timeout)
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
-spec(wait_for_hangup/0 :: () -> {ok, channel_hangup}).    
wait_for_hangup() -> 
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->            
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
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
%% Initializes a AMQP queue and consumer to recieve call events
%% @end
%%--------------------------------------------------------------------
-spec(init_amqp/1 :: (Call :: #cf_call{}) -> no_return()).                          
init_amqp(#cf_call{amqp_h=AHost, call_id=CallId}) ->
    AmqpQ = amqp_util:new_queue(AHost),
    amqp_util:bind_q_to_callevt(AHost, AmqpQ, CallId),
    amqp_util:basic_consume(AHost, AmqpQ).    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
-spec(send_callctrl/2 :: (JSON :: iolist(), Call :: #cf_call{}) -> no_return()).
send_callctrl(Json, #cf_call{amqp_h=AHost, ctrl_q=CtrlQ}) ->
    amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>).
