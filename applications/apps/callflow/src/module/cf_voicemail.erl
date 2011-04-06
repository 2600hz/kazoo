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

-define(FOLDER_NEW, <<"new">>).
-define(FOLDER_SAVED, <<"saved">>).
-define(FOLDER_DELETED, <<"deleted">>).

-define(UNAVAILABLE_GREETING, <<"unavailable_greeting.wav">>).
-define(NAME_RECORDING, <<"name_recording.wav">>).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).
-import(cf_call_command, [
                           answer/1, play/2, b_play/2, say/3, tones/2, b_record/2
                          ,store/3, b_play_and_collect_digits/6, noop/1, flush/1
                          ,wait_for_dtmf/1, wait_for_application_or_dtmf/2, audio_macro/2
                         ]).

-record(keys, {
          %% Compose Voicemail
           operator = <<"0">>
          ,login = <<"*">>
              
           %% Record Review
          ,listen = <<"1">>
          ,save = <<"2">>
          ,record = <<"3">>

          %% Main Menu
          ,hear_new = <<"1">>
          ,hear_saved = <<"2">>
          ,configure = <<"5">>
          ,exit = <<"#">>

          %% Config Menu
          ,rec_unavailable  = <<"1">>
          ,rec_name = <<"2">>
          ,set_pin = <<"3">>
          ,return_main = <<"0">>             

          %% Post playbak
          ,replay = <<"1">>
          ,keep = <<"2">>
          ,delete = <<"3">>         
         }).

-record(prompts, {
           person_at_exten = <<"/system_media/vm-person">>
          ,not_available = <<"/system_media/vm-not_available">>
          ,no_mailbox = <<"/system_media/vm-not_available_no_voicemail">>
          ,mailbox_full = <<"/system_media/vm-mailbox_full">>

          ,record_instructions = <<"/system_media/vm-record_message">>

          ,goodbye = <<"/system_media/vm-goodbye">>
          ,received = <<"/system_media/vm-received">>
          ,press = <<"/system_media/vm-press">>

          ,to_listen = <<"/system_media/vm-listen_to_recording">>
          ,to_save = <<"/system_media/vm-save_recording">>
          ,to_rerecord = <<"/system_media/vm-rerecord">>

          ,enter_mailbox = <<"/system_media/vm-enter_id">>
          ,enter_password = <<"/system_media/vm-enter_pass">>
          ,invalid_login = <<"/system_media/vm-fail_auth">>
          ,abort_login = <<"/system_media/vm-abort">>

          ,you_have = <<"/system_media/vm-you_have">>
          ,new = <<"/system_media/vm-new">>
          ,messages = <<"/system_media/vm-messages">>
          ,saved = <<"/system_media/vm-saved">>
          ,to_hear_new = <<"/system_media/vm-listen_new">>
          ,to_hear_saved = <<"/system_media/vm-listen_saved">>
          ,to_configure = <<"/system_media/vm-advanced">>
          ,to_exit = <<"/system_media/vm-to_exit">>

          ,to_change_pin = <<"/system_media/vm-change_password">>
          ,to_rec_name = <<"/system_media/vm-record_name2">>
          ,to_rec_unavailable = <<"/system_media/vm-to_record_greeting">>
          ,to_return_main = <<"/system_media/vm-main_menu">>

          ,to_replay = <<"/system_media/vm-listen_to_recording">>
          ,to_keep = <<"/system_media/vm-save_recording">>
          ,to_delete = <<"/system_media/vm-delete_recording">>

          ,message_saved = <<"/system_media/vm-saved">>
          ,message_deleted = <<"/system_media/vm-deleted">>

          ,record_name = <<"/system_media/vm-record_name1">>
          ,record_unavail_greeting = <<"/system_media/vm-record_greeting">>

          ,enter_new_pin = <<"shout://translate.google.com/translate_tts?tl=en&q=Enter+your+new+password+followed+by+the+pound+key.">>
          ,reenter_new_pin = <<"shout://translate.google.com/translate_tts?tl=en&q=Re-enter+your+new+password+followed+by+the+pound+key+to+confirm.">>

          ,tone_spec = [{struct, [{<<"Frequencies">>, [440]},{<<"Duration-ON">>, 500},{<<"Duration-OFF">>, 100}]}]
         }).

-record(mailbox, {
           unavailable_greeting = undefined
          ,database = undefined
          ,mailbox_id = undefined
          ,exists = false
          ,skip_instructions = <<"false">>
          ,skip_greeting = <<"false">>
          ,pin = <<>>
          ,timezone = <<"America/Los_Angeles">>
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
    case whapps_json:get_value(<<"action">>, Data) of 
        <<"compose">> ->
            answer(Call),
            compose_voicemail(get_mailbox_profile(Data), Call),
            CFPid ! {stop};
        <<"check">> ->
            answer(Call),
            case whapps_json:get_value(<<"id">>, Data) of
                undefined ->                    
                    find_mailbox(whapps_json:get_value(<<"database">>, Data), Call);
                _ ->
                    check_mailbox(get_mailbox_profile(Data), Call)
            end,
            CFPid ! {stop};
        _ ->
            CFPid ! {continue}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(check_mailbox/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> invalid | ok).
-spec(check_mailbox/3 :: (Box :: #mailbox{}, Call :: #cf_call{}, Loop :: non_neg_integer()) -> invalid | ok).

check_mailbox(#mailbox{prompts=Prompts, pin = <<>>}, Call) ->
    b_play(Prompts#prompts.goodbye, Call);
check_mailbox(Box, Call) ->
    check_mailbox(Box, Call, 1).

check_mailbox(#mailbox{prompts=Prompts, pin=Pin}=Box, Call, Loop) ->
    try
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, Prompts#prompts.enter_password, <<"1">>, <<"8000">>, Call),
        main_menu(Box, Call)
    catch
        _:_ ->
            b_play(Prompts#prompts.invalid_login, Call),
            if 
                Loop < Box#mailbox.max_login_attempts ->
                    check_mailbox(Box, Call, Loop+1);
                true ->
                    b_play(Prompts#prompts.abort_login, Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(find_mailbox/2 :: (Db :: binary(), Call :: #cf_call{}) -> invalid | ok).
-spec(find_mailbox/3 :: (Db :: binary(), Call :: #cf_call{}, Loop :: non_neg_integer()) -> invalid | ok).

find_mailbox(Db, Call) ->
    find_mailbox(Db, Call, 1).

find_mailbox(Db, Call, Loop) ->    
    Prompts = #prompts{},
    try
        {ok, Mailbox} = b_play_and_collect_digits(<<"1">>, <<"6">>, Prompts#prompts.enter_mailbox, <<"1">>, <<"8000">>, Call),
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, Prompts#prompts.enter_password, <<"1">>, <<"8000">>, Call),
        {ok, [JObj]} = couch_mgr:get_results(Db, {<<"vmboxes">>, <<"listing_by_mailbox">>}, [{<<"key">>, Mailbox}]),
        Box = get_mailbox_profile({struct, [{<<"database">>, Db}, {<<"id">>, whapps_json:get_value(<<"id">>, JObj)}]}),
        Pin = Box#mailbox.pin,
        main_menu(Box, Call)        
    catch
        _:_ ->
            B = #mailbox{},
            b_play(Prompts#prompts.invalid_login, Call),
            if 
                Loop < B#mailbox.max_login_attempts ->
                    find_mailbox(Db, Call, Loop+1);
                true ->
                    b_play(Prompts#prompts.abort_login, Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(compose_voicemail/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> tuple(stop)).
compose_voicemail(#mailbox{exists=false, prompts=Prompts}, Call) ->
    b_play(Prompts#prompts.no_mailbox, Call),
    ok;
compose_voicemail(#mailbox{prompts=Prompts, keys=Keys}=Box, Call) ->
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
        {dtmf, Digit} ->            
            flush(Call),
            if 
                Digit == Keys#keys.login ->
                    find_mailbox(Box#mailbox.database, Call);
                true ->
                    record_voicemail(tmp_file(), Box, Call)
            end;
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
play_greeting(#mailbox{prompts=Prompts, unavailable_greeting=false}, #cf_call{to_number=Exten} = Call) ->
    audio_macro([
                  {play, Prompts#prompts.person_at_exten}
                 ,{say,  Exten}
                 ,{play, Prompts#prompts.not_available}
                ], Call);
play_greeting(#mailbox{database=Db, mailbox_id=Id, unavailable_greeting=true}, Call) ->
    play(<<$/, Db/binary, $/, Id/binary, $/, "unavailable_greeting.wav">>, Call).

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
%% 
%% @end
%%--------------------------------------------------------------------
-spec(main_menu/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> stop | continue).
main_menu(#mailbox{prompts=Prompts, keys=Keys}=Box, Call) ->
    Messages = get_messages(Box),
    audio_macro([
                  {play, Prompts#prompts.you_have}
                 ,{say,  whistle_util:to_binary(count_messages(Messages, ?FOLDER_NEW))}
                 ,{play, Prompts#prompts.new}
                 ,{play, Prompts#prompts.messages}

                 ,{play, Prompts#prompts.you_have}
                 ,{say,  whistle_util:to_binary(count_messages(Messages, ?FOLDER_SAVED))}
                 ,{play, Prompts#prompts.saved}
                 ,{play, Prompts#prompts.messages}

                 ,{play, Prompts#prompts.to_hear_new}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.hear_new}

                 ,{play, Prompts#prompts.to_hear_saved}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.hear_saved}

                 ,{play, Prompts#prompts.to_configure}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.configure}

                 ,{play, Prompts#prompts.to_exit}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.exit}
                ], Call),
    case wait_for_dtmf(30000) of
        {error, _}=E ->
            E;
        {ok, Digit} ->
            flush(Call),
            if 
                Digit == Keys#keys.hear_new ->
                    play_messages(get_folder(Messages, ?FOLDER_NEW), Box, Call),
                    main_menu(Box, Call);
                Digit == Keys#keys.hear_saved -> 
                    play_messages(get_folder(Messages, ?FOLDER_SAVED), Box, Call),
                    main_menu(Box, Call);
                Digit == Keys#keys.configure -> 
                    config_menu(Box, Call);
                Digit == Keys#keys.exit -> 
                    ok;
                true ->
                    main_menu(Box, Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(play_messages/3 :: (Messages :: json_objects(), Box :: #mailbox{}, Call :: #cf_call{}) -> ok).
play_messages([], _, _) ->    
    ok;
play_messages([H|T], #mailbox{prompts=Prompts, keys=Keys}=Box, Call) ->
    Message = get_message(H, Box),
    audio_macro([
                  {play, Prompts#prompts.received}
                 ,{say,  get_unix_epoch(whapps_json:get_value(<<"timestamp">>, H), Box#mailbox.timezone), <<"current_date_time">>}
                 ,{play, Message}

                 ,{play, Prompts#prompts.to_replay}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.replay}

                 ,{play, Prompts#prompts.to_keep}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.keep}

                 ,{play, Prompts#prompts.to_delete}
                 ,{play, Prompts#prompts.press}
                 ,{say,  Keys#keys.delete}

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
                Digit == Keys#keys.replay ->
                    play_messages([H|T], Box, Call);
                Digit == Keys#keys.keep -> 
                    play(Prompts#prompts.message_saved, Call),
                    set_folder(?FOLDER_SAVED, H, Box),
                    play_messages(T, Box, Call);
                Digit == Keys#keys.delete -> 
                    play(Prompts#prompts.message_deleted, Call),
                    set_folder(?FOLDER_DELETED, H, Box),
                    play_messages(T, Box, Call);
                Digit == Keys#keys.return_main -> 
                    play(Prompts#prompts.message_saved, Call),
                    set_folder(?FOLDER_SAVED, H, Box);
                true ->
                    play(Prompts#prompts.message_saved, Call),
                    set_folder(?FOLDER_SAVED, H, Box),
                    play_messages(T, Box, Call)
            end
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
                    record_unavailable_greeting(tmp_file(), Box, Call),
                    config_menu(Box, Call);
                Digit == Keys#keys.rec_name -> 
                    record_name(tmp_file(), Box, Call),
                    config_menu(Box, Call);
                Digit == Keys#keys.set_pin ->                   
                    change_pin(Box, Call),
                    config_menu(Box, Call);
                Digit == Keys#keys.return_main ->                   
                    main_menu(Box, Call);
                %% Bulk delete -> delete all voicemails
                %% Reset -> delete all voicemails, greetings, name, and reset pin
                true ->
                    config_menu(Box, Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(record_unavailable_greeting/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> ok).
record_unavailable_greeting(MediaName, #mailbox{prompts=Prompts}=Box, Call) -> 
    audio_macro([
                  {play,  Prompts#prompts.record_unavail_greeting}
                 ,{tones, Prompts#prompts.tone_spec}
                ], Call),
    case b_record(MediaName, Call) of
        {ok, _Msg} ->
            case review_recording(MediaName, Box, Call) of
                {ok, record} ->
                    record_unavailable_greeting(MediaName, Box, Call);
                {ok, save} ->
                    store_recording(MediaName, ?UNAVAILABLE_GREETING, Box, Call);
                _Else ->
                    ok
            end;                
        _Else ->                
            ok
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
                                    {<<"timestamp">>, new_timestamp()}
                                   ,{<<"from">>, whapps_json:get_value(<<"From">>, RR)}
                                   ,{<<"to">>, whapps_json:get_value(<<"To">>, RR)}
                                   ,{<<"caller-id-number">>, whapps_json:get_value(<<"Caller-ID-Number">>, RR)}
                                   ,{<<"caller-id-name">>, whapps_json:get_value(<<"Caller-ID-Name">>, RR)}
                                   ,{<<"call-iD">>, whapps_json:get_value(<<"Call-ID">>, RR)}
                                   ,{<<"folder">>, ?FOLDER_NEW}
                                   ,{<<"attachment">>, MediaName}
                                  ]}] ++ whapps_json:get_value([<<"messages">>], JObj, []),            
            couch_mgr:save_doc(Db, whapps_json:set_value([<<"messages">>], NewMessages, JObj));
        {error, _}=E ->
            E
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
                  {play,  Prompts#prompts.record_name}
                 ,{tones, Prompts#prompts.tone_spec}
                ], Call),
    case b_record(MediaName, Call) of
        {ok, _Msg} ->
            case review_recording(MediaName, Box, Call) of
                {ok, record} ->
                    record_name(MediaName, Box, Call);
                {ok, save} ->
                    store_recording(MediaName, ?NAME_RECORDING, Box, Call);                    
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
        couch_mgr:save_doc(Db, whapps_json:set_value([<<"base">>, <<"pin">>], Pin, JObj))
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
-spec(get_mailbox_profile/1 :: (Data :: json_object()) -> #mailbox{} | tuple(stop)).
get_mailbox_profile(Data) ->
    Db = whapps_json:get_value(<<"database">>, Data),
    Id = whapps_json:get_value(<<"id">>, Data),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            #mailbox{         
                       database = Db
                      ,mailbox_id = Id
                      ,skip_instructions = whapps_json:get_value([<<"base">>, <<"skip-instructions">>], JObj, #mailbox.skip_instructions)
                      ,skip_greeting = whapps_json:get_value([<<"base">>, <<"skip-greeting">>], JObj, #mailbox.skip_greeting)
                      ,unavailable_greeting = whapps_json:get_value([<<"_attachments">>, ?UNAVAILABLE_GREETING], JObj) =/= undefined
                      ,pin = whapps_json:get_value([<<"base">>, <<"pin">>], JObj, <<>>)
                      ,timezone = whapps_json:get_value([<<"base">>, <<"timezone">>], JObj, #mailbox.timezone)
                      ,exists=true
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
-spec(get_messages/1 :: (Mailbox :: #mailbox{}) -> json_object()).
get_messages(#mailbox{database=Db, mailbox_id=Id}) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            whapps_json:get_value(<<"messages">>, JObj, []);
        _ ->
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(get_message/2 :: (Message :: binary(), Mailbox :: #mailbox{}) -> binary()).
get_message(Message, #mailbox{database=Db, mailbox_id=Id}) ->
    <<$/, Db/binary, $/, Id/binary, $/, (whapps_json:get_value(<<"attachment">>, Message))/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(count_messages/2 :: (Message :: json_object(), Folder :: binary()) -> integer()).
count_messages(Messages, Folder) ->
    lists:foldr(fun(Message, Count) ->
                       case whapps_json:get_value(<<"folder">>, Message) of
                           Folder ->
                               Count + 1;
                           _ ->
                               Count
                       end
               end, 0, Messages).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(get_folder/2 :: (Message :: json_object(), Folder :: binary()) -> list()).
get_folder(Messages, Folder) ->
    lists:foldr(fun(Message, Acc) ->
                       case whapps_json:get_value(<<"folder">>, Message) of
                           Folder ->
                               [Message|Acc];
                           _ ->
                               Acc
                       end
               end, [], Messages).    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(set_folder/3 :: (Folder :: binary(), Message :: json_object(), Box :: #mailbox{}) -> ok | tuple(error, atom())).
set_folder(Folder, Message, Box) ->
    case whapps_json:get_value(<<"folder">>, Message) of 
        Folder ->
            ok;
        _ ->
            update_folder(Folder, whapps_json:get_value(<<"attachment">>, Message), Box)             
    end.
       
%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec(update_folder/3 :: (Folder :: binary(), Attachment :: binary(), Box :: #mailbox{}) -> ok | tuple(error, atom())).
update_folder(_, undefined, _) ->     
    {error, attachment_undefined};
update_folder(Folder, Attachment, #mailbox{database=Db, mailbox_id=Id}) -> 
    case couch_mgr:open_doc(Db, Id) of 
        {ok, JObj} ->
            Messages = lists:map(fun(Message) ->
                                         case whapps_json:get_value(<<"attachment">>, Message) of
                                             Attachment ->
                                                 whapps_json:set_value(<<"folder">>, Folder, Message);
                                             _ ->
                                                 Message
                                         end
                                 end, whapps_json:get_value(<<"messages">>, JObj, [])),
            couch_mgr:save_doc(Db, whapps_json:set_value(<<"messages">>, Messages, JObj));
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(tmp_file/0 :: () -> binary()).
tmp_file() ->
     <<(list_to_binary(whistle_util:to_hex(crypto:rand_bytes(16))))/binary, ".wav">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the Universal Coordinated Time (UTC) reported by the 
%% underlying operating system (local time is used if universal 
%% time is not available) as number of gregorian seconds starting 
%% with year 0.
%% @end
%%--------------------------------------------------------------------
-spec(new_timestamp/0 :: () -> binary()).
new_timestamp() ->
    whistle_util:to_binary(calendar:datetime_to_gregorian_seconds(calendar:universal_time())).    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Accepts Universal Coordinated Time (UTC) and convert it to binary
%% encoded Unix epoch in the provided timezone
%% @end
%%--------------------------------------------------------------------
-spec(get_unix_epoch/2 :: (Epoch :: binary(), Timezone :: binary()) -> binary()).
get_unix_epoch(Epoch, Timezone) ->
    UtcDateTime = calendar:gregorian_seconds_to_datetime(whistle_util:to_integer(Epoch)),
    LocalDateTime = localtime:utc_to_local(UtcDateTime, whistle_util:to_list(Timezone)),
    whistle_util:to_binary(calendar:datetime_to_gregorian_seconds(LocalDateTime) - 62167219200).
