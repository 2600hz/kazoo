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
                           answer/1, play/2, b_play/2, say/3, tones/2, b_record/2
                          ,store/3, b_play_and_collect_digits/6, noop/1, flush/1
                          ,wait_for_dtmf/1, wait_for_application_or_dtmf/2, audio_macro/2
                         ]).

-define(DEFAULT_TIMEOUT, 30).

-record(keys, {
          %% Compose Voicemail
           operator = <<"0">>
          ,login = <<"*">>
              
           %% Record Review
          ,listen = <<"1">>
          ,save = <<"2">>
          ,record = <<"3">>

          %% Config Menu
          ,rec_unavailable  = <<"1">>
          ,rec_name = <<"2">>
          ,set_pin = <<"3">>
          ,return_main = <<"0">>             
         }).

-record(prompts, {
           person_at_exten = <<"/system_media/vm-person">>
          ,not_available = <<"/system_media/vm-not_available">>
          ,record_instructions = <<"/system_media/vm-record_greeting">>
          ,press = <<"/system_media/vm-press">>
          ,to_listen = <<"/system_media/vm-listen_to_recording">>
          ,to_save = <<"/system_media/vm-save_recording">>
          ,to_rerecord = <<"/system_media/vm-rerecord">>
          ,enter_password = <<"/system_media/vm-enter_pass">>
          ,invalid_login = <<"/system_media/vm-fail_auth">>
          ,to_change_pin = <<"/system_media/vm-change_password">>
          ,to_rec_name = <<"/system_media/vm-record_name2">>
          ,to_rec_unavailable = <<"/system_media/vm-to_record_greeting">>
          ,to_return_main = <<"/system_media/vm-main_menu">>
          ,record_name = <<"/system_media/vm-record_name1">>
          ,enter_new_pin = <<"shout://translate.google.com/translate_tts?tl=en&q=Enter+your+new+password+followed+by+the+pound+key.">>
          ,reenter_new_pin = <<"shout://translate.google.com/translate_tts?tl=en&q=Re-enter+your+new+password+followed+by+the+pound+key+to+confirm.">>
          ,tone_spec = [{struct, [{<<"Frequencies">>, [440]},{<<"Duration-ON">>, 500},{<<"Duration-OFF">>, 100}]}]
         }).

-record(mailbox, {
           unavailable_greeting = undefined
          ,database = undefined
          ,mailbox_id = undefined
          ,action = undefined
          ,skip_instructions = <<"false">>
          ,skip_greeting = <<"false">>
          ,pin = <<>>
          ,max_login_attempts = 3
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
            compose_voicemail(Box, Call),
            CFPid ! {stop};
        Box#mailbox.action == <<"check">> ->
            answer(Call),
            check_voicemail(Box, Call),
            CFPid ! {stop};
        true ->
            CFPid ! {continue}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(compose_voicemail/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> tuple(stop)).
compose_voicemail(#mailbox{prompts=Prompts}=Box, Call) ->
    case whistle_util:is_true(Box#mailbox.skip_greeting) of
        false ->
            play_greeting(Box, Call);
        true ->
            ok
    end,
    case whistle_util:is_true(Box#mailbox.skip_instructions) of
        false ->
            play(Prompts#prompts.record_instructions, Call);
        true ->
            ok
    end,
    noop(Call),
    case wait_for_application_or_dtmf(<<"noop">>, 25000) of
        {ok, _} ->
            record_voicemail(tmp_file(), Box, Call);
        {dtmf, _} ->
            flush(Call),
            record_voicemail(tmp_file(), Box, Call);
        {error, _} ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(play_greeting/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> ok | tuple(error, atom())).
play_greeting(#mailbox{prompts=Prompts, unavailable_greeting=undefined}, #cf_call{to_number=Exten} = Call) ->
    audio_macro([
                  {play, Prompts#prompts.person_at_exten}
                 ,{say, Exten}
                 ,{play, Prompts#prompts.not_available}
                ], Call);
play_greeting(#mailbox{database=Db, mailbox_id=Id, unavailable_greeting=Greeting}, Call) ->
    play(<<$/, Db/binary, $/, Id/binary, $/, Greeting/binary>>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(record_voicemail/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> tuple(stop)).
record_voicemail(MediaName, #mailbox{prompts=Prompts}=Box, Call) -> 
    tones(Prompts#prompts.tone_spec, Call),      
    case b_record(MediaName, Call) of
        {ok, _Msg} ->
            case review_recording(MediaName, Box, Call) of
                {ok, record} ->
                    record_voicemail(MediaName, Box, Call);
                _Else ->
                    new_message(MediaName, Box, Call)
            end;                
        _Else ->                
            new_message(MediaName, Box, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(new_message/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> ok | tuple(error, atom())).
new_message(MediaName, #mailbox{database=Db, mailbox_id=Id}=Box, #cf_call{route_request=RR}=Call) ->
    store_recording(MediaName, Box, Call),
    receive after 5000 -> ok end,
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            NewMessages=[{struct, [
                                    {<<"Timestamp">>, <<>>}
                                   ,{<<"From">>, whapps_json:get_value(<<"From">>, RR)}
                                   ,{<<"To">>, whapps_json:get_value(<<"To">>, RR)}
                                   ,{<<"Caller-ID-Number">>, whapps_json:get_value(<<"Caller-ID-Number">>, RR)}
                                   ,{<<"Caller-ID-Name">>, whapps_json:get_value(<<"Caller-ID-Name">>, RR)}
                                   ,{<<"Call-ID">>, whapps_json:get_value(<<"Call-ID">>, RR)}
                                   ,{<<"Folder">>, <<"new">>}
                                   ,{<<"Attachment">>, MediaName}
                                  ]}] ++ whapps_json:get_value([<<"messages">>, <<"new">>], JObj, []),            
            couch_mgr:save_doc(Db, whapps_json:set_value([<<"messages">>, <<"new">>], NewMessages, JObj));
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(check_voicemail/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> tuple(stop)).
check_voicemail(Box, Call) ->
    case check_pin(Box, Call, 1) of
        invalid ->
            {error, invalid_pin};
        ok ->
            config_menu(Box, Call)
    end.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(check_pin/3 :: (Box :: #mailbox{}, Call :: #cf_call{}, Loop :: non_neg_integer()) -> invalid | ok).                           
check_pin(#mailbox{max_login_attempts=MaxLoginAttempts}, _Call, Loop) when Loop > MaxLoginAttempts ->
    invalid;                                                                         
check_pin(#mailbox{prompts=Prompts, pin=Pin}=Box, Call, Loop) ->
    try
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, Prompts#prompts.enter_password, <<"1">>, <<"8000">>, Call),
        ok
    catch
        _:_ ->
            play(Prompts#prompts.invalid_login, Call),
            check_pin(Box, Call, Loop+1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(config_menu/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> stop | continue).
config_menu(#mailbox{prompts=Prompts, keys=Keys}=Box, Call) ->
    audio_macro([
                  {play, Prompts#prompts.to_rec_unavailable}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.rec_unavailable}
                 ,{play, Prompts#prompts.to_rec_name}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.rec_name}
                 ,{play, Prompts#prompts.to_change_pin}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.set_pin}
                 ,{play, Prompts#prompts.to_return_main}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.return_main}
                ], Call),
    case wait_for_dtmf(30000) of
        {error, _}=E ->
            E;
        {ok, Digit} ->
            flush(Call),
            if 
                Digit == Keys#keys.rec_unavailable ->
                    record_name(tmp_file(), Box, Call);
                Digit == Keys#keys.rec_name -> 
                    record_name(tmp_file(), Box, Call);
                Digit == Keys#keys.set_pin ->                   
                    change_pin(Box, Call);
                true ->
                    ok
            end,
            config_menu(Box, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(record_name/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> ok).
record_name(MediaName, #mailbox{prompts=Prompts}=Box, Call) -> 
    audio_macro([
                  {play, Prompts#prompts.record_name}
                 ,{tones, Prompts#prompts.tone_spec}
                ], Call),
    case b_record(MediaName, Call) of
        {ok, _Msg} ->
            case review_recording(MediaName, Box, Call) of
                {ok, record} ->
                    record_name(MediaName, Box, Call);
                {ok, save} ->
                    store_recording(MediaName, <<"name_recording.wav">>, Box, Call);                    
                _Else ->
                    ok
            end;                
        _Else ->                
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(change_pin/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> stop | continue).
change_pin(#mailbox{prompts=Prompts, database=Db, mailbox_id=Id}=Box, Call) ->
    try
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, Prompts#prompts.enter_new_pin, <<"1">>, <<"8000">>, Call),
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, Prompts#prompts.reenter_new_pin, <<"1">>, <<"8000">>, Call),
        if byte_size(Pin) == 0 -> throw(pin_empty); true -> ok end,
        {ok, JObj} = couch_mgr:open_doc(Db, Id),
        couch_mgr:save_doc(Db, whapps_json:set_value([<<"base">>, <<"pin-number">>], Pin, JObj))
    catch
        _:_ ->
            change_pin(Box, Call)
    end.    


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
                       database = Db
                      ,mailbox_id = Id
                      ,skip_instructions = whapps_json:get_value(["base", "skip-instructions"], JObj, #mailbox.skip_instructions)
                      ,skip_greeting = whapps_json:get_value(["base", "skip-greeting"], JObj, #mailbox.skip_greeting)
                      ,action = get_value(<<"action">>, Props)
                      ,unavailable_greeting = whapps_json:get_value(["base", "unavailable-greeting"], JObj)
                      ,pin = whapps_json:get_value(["base", "pin-number"], JObj, <<"1010">>)
                    };
        _ -> 
            #mailbox{}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(review_recording/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> stop | continue).
review_recording(MediaName, #mailbox{prompts=Prompts, keys=Keys}=Box, Call) ->
    audio_macro([
                  {play, Prompts#prompts.press}
                 ,{say,  Keys#keys.listen}
                 ,{play, Prompts#prompts.to_listen}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.save}
                 ,{play, Prompts#prompts.to_save}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.record}
                 ,{play, Prompts#prompts.to_rerecord}
                ], Call),
    case wait_for_dtmf(5000) of
        {error, _}=E ->
            E;
        {ok, Digit} ->
            flush(Call),
            if 
                Digit == Keys#keys.listen ->
                    b_play(MediaName, Call),
                    review_recording(MediaName, Box, Call);
                Digit == Keys#keys.record -> 
                    {ok, record};
                Digit == Keys#keys.save ->                   
                    {ok, save};
                true ->
                    review_recording(MediaName, Box, Call)
            end
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(store_recording/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> ok | tuple(error, atom())).
-spec(store_recording/4 :: (MediaName :: binary(), DestName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> ok | tuple(error, atom())).
store_recording(MediaName, Box, Call) ->
    store_recording(MediaName, MediaName, Box, Call).
store_recording(MediaName, DestName, Box, Call) ->
    store(MediaName, get_attachment_path(DestName, Box), Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_attachment_path/2 :: (MediaName :: binary(), Box :: #mailbox{}) -> binary()).
get_attachment_path(MediaName, #mailbox{database=Db, mailbox_id=Id}) ->
    <<(couch_mgr:get_url())/binary
      ,Db/binary
      ,$/, Id/binary
      ,$/, MediaName/binary
      ,"?rev=", (couch_mgr:lookup_doc_rev(Db, Id))/binary>>.   

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_unavailable_greeting/1 :: (JObj :: json_object()) -> undefined | binary()).
get_unavailable_greeting(JObj) ->
    case whapps_json:get_value(["base", "unavailable-greeting"], JObj) of
        undefined ->
            undefined;
        MediaName ->
            case whapps_json:get_value([<<"_attachments">>, MediaName], JObj) of
                undefined ->
                    undefined;
                _ ->
                    MediaName
            end
    end.
   
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(tmp_file/0 :: () -> binary()).
tmp_file() ->
     <<(list_to_binary(whistle_util:to_hex(crypto:rand_bytes(16))))/binary, ".wav">>.
