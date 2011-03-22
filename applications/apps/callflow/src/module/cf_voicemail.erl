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
-import(cf_call_command, [
                           answer/1, play/3, say/3, tones/2, noop/1, record/2
                          ,wait_for_hangup/0, wait_for_application_or_dtmf/2, flush/1
                         ]).

-define(DEFAULT_TIMEOUT, 30).
-define(ANY_DIGIT, [
                     <<"1">>, <<"2">>, <<"3">>
                    ,<<"4">>, <<"5">>, <<"6">>    
                    ,<<"7">>, <<"8">>, <<"9">>    
                    ,<<"*">>, <<"0">>, <<"#">>
                   ]).

-record(keys, {
          %% Compose Voicemail
           operator = <<"0">>
          ,login = <<"*">>
              
           %% Record Review
          ,listen = <<"1">>
          ,save = <<"2">>
          ,record = <<"3">>
         }).

-record(prompts, {
           person_at_exten = <<"/system_media/the-person-at-exten">>
          ,not_available = <<"/system_media/is-not-available">>
          ,record_instructions = <<"/system_media/record-message-instructions">>
          ,press = <<"/system_media/press">>
          ,to_listen = <<"/system_media/listen-to-recording">>
          ,to_save = <<"/system_media/save-recording">>
          ,to_rerecord = <<"/system_media/rerecord">>
          ,tone_spec = [{struct, [{<<"Frequencies">>, [440]},{<<"Duration-ON">>, 500},{<<"Duration-OFF">>, 100}]}]          
         }).

-record(mailbox, {
           greeting = undefined
          ,file_id = undefined
          ,database = undefined
          ,mailbox_id = undefined
          ,action = undefined
          ,skip_instructions = <<"false">>
          ,skip_greeting = <<"false">>
          ,keys = #keys{}
          ,prompts = #prompts{}
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
    Box = get_mailbox_profile(Data),
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

check_voicemail(_Box, _Call) ->
    {stop}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the mailbox parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec(get_mailbox_profile/1 :: (JOBj :: json_object()) -> #mailbox{} | tuple(stop)).
get_mailbox_profile({struct, Props}) ->
    Db = get_value(<<"database">>, Props),
    Id = get_value(<<"id">>, Props),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            #mailbox{          
                       file_id = <<(list_to_binary(whistle_util:to_hex(crypto:rand_bytes(16))))/binary, ".wav">>
                      ,greeting = whapps_json:get_value(["base", "greeting"], JObj)
                      ,database = Db
                      ,mailbox_id = Id
                      ,skip_instructions = whapps_json:get_value(["base", "skip-instructions"], JObj, #mailbox.skip_instructions)
                      ,skip_greeting = whapps_json:get_value(["base", "skip-greeting"], JObj, #mailbox.skip_greeting)
                      ,action = get_value(<<"action">>, Props)
                    };
        _ -> 
            #mailbox{}
    end.

compose_voicemail(#mailbox{prompts=Prompts}=Box, Call) ->
    case whistle_util:is_true(Box#mailbox.skip_greeting) of
        false ->
            play_greeting(Box, Call);
        true ->
            ok
    end,
    case whistle_util:is_true(Box#mailbox.skip_instructions) of
        false ->
            play(Prompts#prompts.record_instructions, ?ANY_DIGIT, Call);
        true ->
            ok
    end,
    noop(Call),
    case wait_for_application_or_dtmf(<<"noop">>, 25000) of
        {ok, _} ->
            record_voicemail(Box, Call);
        {dtmf, _} ->
            record_voicemail(Box, Call);
        {error, _} ->
            {stop}
    end.

play_greeting(#mailbox{prompts=Prompts, greeting=undefined}, #cf_call{to_number=Exten} = Call) ->
    play(Prompts#prompts.person_at_exten, ?ANY_DIGIT, Call),
    say(Exten, <<"name_spelled">>, Call),
    play(Prompts#prompts.not_available, ?ANY_DIGIT, Call);
play_greeting(#mailbox{database=Db, mailbox_id=Id, greeting=Greeting}, Call) ->
    play(<<$/, Db/binary, $/, Id/binary, $/, Greeting/binary>>, ?ANY_DIGIT, Call).

record_voicemail(#mailbox{prompts=Prompts}=Box, Call) -> 
    flush(Call),
    tones(Prompts#prompts.tone_spec, Call),  
    record(Box#mailbox.file_id, Call),
    case cf_call_command:wait_for_message(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, 120000) of
        {ok, Msg} ->
            _Digits = get_value(<<"Terminator">>, Msg),
            review_recording(Box, Call);
        _Else ->                
            store(Box, Call),
            {stop}                
    end.

review_recording(#mailbox{prompts=Prompts, file_id=FileId, keys=Keys}=Box, Call) ->
    play(Prompts#prompts.press, ?ANY_DIGIT, Call),
    say(Keys#keys.listen, <<"name_spelled">>, Call),
    play(Prompts#prompts.to_listen, ?ANY_DIGIT, Call),
    play(Prompts#prompts.press, ?ANY_DIGIT, Call),
    say(Keys#keys.record, <<"name_spelled">>, Call),
    play(Prompts#prompts.to_save, ?ANY_DIGIT, Call),
    play(Prompts#prompts.press, ?ANY_DIGIT, Call),
    say(Keys#keys.save, <<"name_spelled">>, Call),
    play(Prompts#prompts.to_rerecord, ?ANY_DIGIT, Call),    
    case cf_call_command:wait_for_dtmf(10000) of
        {error, _} ->
            store(Box, Call),
            {stop};
        {ok, Digit} ->
            flush(Call),
            if 
                Digit == Keys#keys.listen ->
                    cf_call_command:b_play(FileId, ?ANY_DIGIT, Call),
                    review_recording(Box, Call);
                Digit == Keys#keys.save ->                   
                    record_voicemail(Box, Call);
                true ->
                    store(Box, Call),
                    {stop}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to store the file as a
%% couchdb attachement on the voicemail defintion document
%% @end
%%--------------------------------------------------------------------
-spec(store/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
store(#mailbox{database=Db, mailbox_id=Id, file_id=FileId}, #cf_call{call_id=CallId, amqp_q=AmqpQ}=Call) ->
    Command = [
                {<<"Application-Name">>, <<"store">>}
               ,{<<"Media-Name">>, FileId}
               ,{<<"Media-Transfer-Method">>, <<"put">>}
               ,{<<"Media-Transfer-Destination">>, <<(couch_mgr:get_url())/binary
                                                     ,Db/binary
                                                     ,$/, Id/binary
                                                     ,$/, FileId/binary
                                                     ,"?rev=", (couch_mgr:lookup_doc_rev(Db, Id))/binary
                                                   >>}
               ,{<<"Additional-Headers">>, [{struct, [{<<"Content-Type">>, <<"audio/x-wav">>}]}]}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Payload} = whistle_api:store_req(Command),
    cf_call_command:send_callctrl(Payload, Call).
