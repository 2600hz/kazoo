%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
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
-define(MAILBOX_DEFAULT_SIZE, 0).
-define(MAILBOX_DEFAULT_MSG_MAX_LENGTH, 0).

-record(keys, {
          %% Compose Voicemail
           operator = <<"0">>
          ,login = <<"*">>

           %% Record Review
          ,save = <<"1">>
          ,listen = <<"2">>
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
          ,keep = <<"1">>
          ,replay = <<"2">>
          ,delete = <<"7">>
         }).

-record(prompts, {
           person_at_exten = <<"/system_media/vm-person">>
          ,not_available = <<"/system_media/vm-not_available">>
          ,no_mailbox = <<"/system_media/vm-not_available_no_voicemail">>
          ,mailbox_full = <<"/system_media/vm-mailbox_full">>

          ,record_instructions = <<"/system_media/vm-record_message">>

          ,saved = <<"/system_media/vm-saved">>
          ,deleted = <<"/system_media/vm-deleted">>

          ,enter_mailbox = <<"/system_media/vm-enter_id">>
          ,enter_password = <<"/system_media/vm-enter_pass">>
          ,invalid_login = <<"/system_media/vm-fail_auth">>
          ,abort_login = <<"/system_media/vm-abort">>
          ,no_access = <<"/system_media/vm-no_access">>
          ,goodbye = <<"/system_media/vm-goodbye">>

          ,setup_intro = <<"/system_media/vm-setup_intro">>
          ,setup_rec_greet = <<"/system_media/vm-setup_rec_greeting">>
          ,setup_complete = <<"/system_media/vm-setup_complete">>

          ,review_recording = <<"/system_media/vm-review_recording">>
          ,main_menu = <<"/system_media/vm-main_menu">>
          ,message_menu = <<"/system_media/vm-message_menu">>
          ,settings_menu = <<"/system_media/vm-settings_menu">>

          ,message_number = <<"/system_media/vm-message_number">>
          ,received = <<"/system_media/vm-received">>
          ,no_messages = <<"/system_media/vm-no_messages">>
          ,you_have = <<"/system_media/vm-you_have">>
          ,new_message = <<"/system_media/vm-new_message">>
          ,new_messages = <<"/system_media/vm-new_messages">>
          ,new_and = <<"/system_media/vm-new_and">>
          ,saved_message = <<"/system_media/vm-saved_message">>
          ,saved_messages = <<"/system_media/vm-saved_messages">>

          ,record_name = <<"/system_media/vm-record_name">>
          ,record_unavail_greeting = <<"/system_media/vm-record_greeting">>

          ,enter_new_pin = <<"/system_media/vm-enter_new_pin">>
          ,reenter_new_pin = <<"/system_media/vm-enter_new_pin_confirm">>
          ,new_pin_saved = <<"/system_media/vm-pin_set">>
          ,new_pin_bad = <<"/system_media/vm-pin_invalid">>

          ,tone_spec = [wh_json:from_list([{<<"Frequencies">>, [<<"440">>]},{<<"Duration-ON">>, <<"500">>},{<<"Duration-OFF">>, <<"100">>}])]
         }).

-record(mailbox, {
           mailbox_id = undefined :: undefined | binary()
          ,mailbox_number = <<>> :: binary()
          ,exists = false :: boolean()
          ,skip_instructions = false :: boolean()
          ,skip_greeting = false :: boolean()
          ,unavailable_media_id = undefined :: undefined | binary()
          ,name_media_id = undefined :: undefined | binary()
          ,pin = <<>> :: binary()
          ,timezone = <<"America/Los_Angeles">> :: binary()
          ,max_login_attempts = 3 :: non_neg_integer()
          ,require_pin = false :: boolean()
          ,check_if_owner = true :: boolean()
          ,owner_id = <<>> :: binary()
          ,is_setup = false :: boolean()
          ,message_count = 0 :: non_neg_integer()
          ,max_message_count = 0 :: non_neg_integer()
          ,max_message_length = 'undefined' :: 'undefined' | pos_integer()
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
-spec handle/2 :: (wh_json:json_object(), #cf_call{}) -> ok.
handle(Data, #cf_call{owner_id=OwnerId, account_db=AccountDb}=Call) ->
    case wh_json:get_value(<<"action">>, Data, <<"compose">>) of
        <<"compose">> ->
            cf_call_command:answer(Call),
            compose_voicemail(get_mailbox_profile(Data, Call), Call),
            ?LOG("compose voicemail complete"),
            spawn(fun() -> cf_util:update_mwi(OwnerId, AccountDb) end),
            cf_exe:stop(Call);
        <<"check">> ->
            cf_call_command:answer(Call),
            check_mailbox(get_mailbox_profile(Data, Call), Call),
            ?LOG("check voicemail complete"),
            spawn(fun() -> cf_util:update_mwi(OwnerId, AccountDb) end),
            cf_exe:stop(Call);
        _ ->
            cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_mailbox/2 :: (#mailbox{}, #cf_call{}) -> ok.
-spec check_mailbox/3 :: (#mailbox{}, #cf_call{}, non_neg_integer()) -> ok.

check_mailbox(Box, Call) ->
    %% Wrapper to initalize the attempt counter
    check_mailbox(Box, Call, 1).

check_mailbox(#mailbox{max_login_attempts=MaxLoginAttempts, prompts=#prompts{abort_login=AbortLogin}}
              ,Call, Loop) when Loop > MaxLoginAttempts ->
    %% if we have exceeded the maximum loop attempts then terminate this call
    ?LOG("maximum number of invalid attempts to check mailbox"),
    cf_call_command:b_play(AbortLogin, Call),
    ok;
check_mailbox(#mailbox{exists=false}=Box, Call, Loop) ->
    %% if the callflow did not define the mailbox to check then request the mailbox ID from the user
    find_mailbox(Box, Call, Loop);
check_mailbox(#mailbox{require_pin=false, owner_id=OwnerId}=Box, #cf_call{owner_id=OwnerId}=Call, _) when
      is_binary(OwnerId), OwnerId =/= <<>> ->
    %% If this is the owner of the mailbox calling in and it doesn't require a pin then jump
    %% right to the main menu
    ?LOG("caller is the owner of this mailbox, and requires no pin"),
    main_menu(Box, Call);
check_mailbox(#mailbox{prompts=Prompts, pin = <<>>, exists=true}, Call, _) ->
    %% If the caller is not the owner or the mailbox requires a pin to access it but has none set
    %% then terminate this call.
    ?LOG("attempted to sign into a mailbox with no pin"),
    cf_call_command:b_play(Prompts#prompts.no_access, Call),
    ok;
check_mailbox(#mailbox{prompts=#prompts{enter_password=EnterPass, invalid_login=InvalidLogin}
                       ,pin=Pin}=Box, Call, Loop) ->
    ?LOG("requesting pin number to check mailbox"),
    case cf_call_command:b_play_and_collect_digits(<<"1">>, <<"6">>, EnterPass, <<"1">>, Call) of
        {ok, Pin} ->
            ?LOG("caller entered a valid pin"),
            main_menu(Box, Call);
        {ok, _} ->
            ?LOG("invalid mailbox login"),
            _ = cf_call_command:b_play(InvalidLogin, Call),
            check_mailbox(Box, Call, Loop + 1);
        _ ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_mailbox/3 :: (#mailbox{}, #cf_call{}, non_neg_integer()) -> ok.

find_mailbox(#mailbox{max_login_attempts=MaxLoginAttempts, prompts=#prompts{abort_login=AbortLogin}}
             ,Call, Loop) when Loop > MaxLoginAttempts ->
    %% if we have exceeded the maximum loop attempts then terminate this call
    ?LOG("maximum number of invalid attempts to find mailbox"),
    cf_call_command:b_play(AbortLogin, Call),
    ok;
find_mailbox(#mailbox{prompts=#prompts{enter_mailbox=EnterBox}}=Box, #cf_call{account_db=Db}=Call, Loop) ->
    ?LOG("requesting mailbox number to check"),
    case cf_call_command:b_play_and_collect_digits(<<"1">>, <<"6">>, EnterBox, <<"1">>, Call) of 
        {ok, <<>>} -> find_mailbox(Box, Call, Loop + 1);
        {ok, Mailbox} ->
            BoxNum = try wh_util:to_integer(Mailbox) catch _:_ -> 0 end,
            %% find the voicemail box, by making a fake 'callflow data payload' we look for it now because if the
            %% caller is the owner, and the pin is not required then we skip requesting the pin
            case couch_mgr:get_results(Db, {<<"vmboxes">>, <<"listing_by_mailbox">>}, [{<<"key">>, BoxNum}]) of
                {ok, []} ->
                    ?LOG("mailbox ~s doesnt exist", [Mailbox]),
                    find_mailbox(Box, Call, Loop + 1);
                {ok, [JObj]} ->
                    ReqBox = get_mailbox_profile(wh_json:from_list([{<<"id">>, wh_json:get_value(<<"id">>, JObj)}]), Call),
                    check_mailbox(ReqBox, Call, Loop);
                {ok, _} ->
                    ?LOG("mailbox ~s is ambiguous", [Mailbox]),
                    find_mailbox(Box, Call, Loop + 1);
                _E ->
                    ?LOG("failed to find mailbox ~s: ~p", [Mailbox, _E]),
                    find_mailbox(Box, Call, Loop + 1)
            end;
        _ -> ok       
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_voicemail/2 :: (#mailbox{}, #cf_call{}) -> ok.
compose_voicemail(#mailbox{check_if_owner=true, owner_id=OwnerId}=Box, #cf_call{owner_id=OwnerId}=Call) when
      is_binary(OwnerId), OwnerId =/= <<>> ->
    ?LOG("caller is the owner of this mailbox"),
    ?LOG("overriding action as check (instead of compose)"),
    check_mailbox(Box, Call);
compose_voicemail(#mailbox{exists=false, prompts=#prompts{no_mailbox=NoMailbox}}, Call) ->
    ?LOG("attempted to compose voicemail for missing mailbox"),
    cf_call_command:b_play(NoMailbox, Call),
    ok;
compose_voicemail(#mailbox{max_message_count=Count, message_count=Count,
                           prompts=#prompts{mailbox_full=MailboxFull}}, Call) when Count /= 0->
    ?LOG("voicemail box is full, cannot hold more messages"),
    cf_call_command:b_play(MailboxFull, Call),
    ok;
compose_voicemail(#mailbox{skip_greeting=SkipGreeting, skip_instructions=SkipInstructions
                           ,prompts=#prompts{record_instructions=RecordInstructions}
                           ,keys=#keys{login=Login}}=Box, Call) ->
    ?LOG("playing mailbox greeting to caller"),

    not SkipGreeting andalso play_greeting(Box, Call),
    not SkipInstructions andalso cf_call_command:play(RecordInstructions, Call),

    _NoopId = cf_call_command:noop(Call),

    %% timeout after 5 min for saftey, so this process cant hang around forever
    case cf_call_command:wait_for_application_or_dtmf(<<"noop">>, 300000) of
        {ok, _} -> 
            ?LOG("played greeting and instructions to caller, recording new message"),
            record_voicemail(tmp_file(), Box, Call);
        {dtmf, Digit} ->
            _ = cf_call_command:b_flush(Call),
            case Digit of
                Login ->
                    ?LOG("caller pressed '~s', redirecting to check voicemail", [Login]), 
                    check_mailbox(Box, Call);
                _Else -> 
                    ?LOG("caller pressed unbound '~s', skip to recording new message", [_Else]), 
                    record_voicemail(tmp_file(), Box, Call)
            end;
        {error, R} -> 
            ?LOG("error while playing voicemail greeting: ~p", [R]),
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_greeting/2 :: (#mailbox{}, #cf_call{}) -> ne_binary() | ok.
play_greeting(#mailbox{prompts=#prompts{person_at_exten=PersonAtExten, not_available=NotAvailable}
                       ,unavailable_media_id=undefined, mailbox_number=Mailbox}, Call) ->
    ?LOG("mailbox has no greeting, playing the generic"),
    cf_call_command:audio_macro([{play, PersonAtExten}
                                 ,{say,  Mailbox}
                                 ,{play, NotAvailable}
                                ], Call);
play_greeting(#mailbox{unavailable_media_id = <<"local_stream://", _/binary>> = Id}, Call) ->
    ?LOG("mailbox has a greeting file on the softswitch: ~s", Id),
    cf_call_command:play(Id, Call);
play_greeting(#mailbox{unavailable_media_id=Id}, #cf_call{account_db=Db}=Call) ->
    ?LOG("streaming mailbox greeting"),
    cf_call_command:play(<<$/, Db/binary, $/, Id/binary>>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_voicemail/3 :: (ne_binary(), #mailbox{}, #cf_call{}) -> ok.
record_voicemail(AttachmentName, #mailbox{prompts = #prompts{tone_spec=ToneSpec, saved=Saved}
                                          ,max_message_length=MaxMessageLength
                                         }=Box, Call) ->
    cf_call_command:tones(ToneSpec, Call),
    ?LOG("composing new voicemail"),
    case cf_call_command:b_record(AttachmentName, ?ANY_DIGIT, wh_util:to_binary(MaxMessageLength), Call) of
        {ok, Msg} ->
            Length = wh_json:get_integer_value(<<"Length">>, Msg, 0),
            case review_recording(AttachmentName, Box, Call) of
                {ok, record} ->
                    ?LOG("caller choose to re-record the message"),
                    record_voicemail(tmp_file(), Box, Call);
                {ok, save} ->
                    ?LOG("caller choose to save the message"),
                    new_message(AttachmentName, Length, Box, Call),
                    cf_call_command:b_play(Saved, Call);
                {ok, no_selection} ->
                    ?LOG("caller made no selection or hungup, saving the message"),
                    new_message(AttachmentName, Length, Box, Call),
                    cf_call_command:b_play(Saved, Call)
            end;
        {error, R} ->
            ?LOG("error while attempting to record a new message: ~p", [R]),
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec setup_mailbox/2 :: (#mailbox{}, #cf_call{}) -> #mailbox{}.
setup_mailbox(#mailbox{prompts=#prompts{setup_intro=SetupIntro
                                        ,setup_rec_greet=SetupRecGreet
                                        ,setup_complete=SetupComplete}}=Box, Call) ->
    ?LOG("starting voicemail configuration wizard"),
    {ok, _} = cf_call_command:b_play(SetupIntro, Call),
    ?LOG("prompting caller to set a pin"),
    change_pin(Box, Call),
    {ok, _} = cf_call_command:b_play(SetupRecGreet, Call),
    ?LOG("prompting caller to record an unavailable greeting"),
    Box1 = record_unavailable_greeting(tmp_file(), Box, Call),
    ok = update_doc(<<"is_setup">>, true, Box1, Call),
    ?LOG("voicemail configuration wizard is complete"),
    {ok, _} = cf_call_command:b_play(SetupComplete, Call),
    Box1#mailbox{is_setup=true}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec main_menu/2 :: (#mailbox{}, #cf_call{}) ->  ok.
-spec main_menu/3 :: (#mailbox{}, #cf_call{}, non_neg_integer()) -> ok.

main_menu(#mailbox{is_setup=false}=Box, Call) ->
    main_menu(setup_mailbox(Box, Call), Call, 1);
main_menu(Box, Call) ->
    main_menu(Box, Call, 1).

main_menu(#mailbox{prompts=#prompts{goodbye=Goodbye}}, Call, Loop) when Loop > 4 ->
    %% If there have been too may loops with no action from the caller this
    %% is likely a abandonded channel, terminate
    ?LOG("entered main menu with too many invalid entries"),
    cf_call_command:b_play(Goodbye, Call),
    ok;
main_menu(#mailbox{prompts=#prompts{main_menu=MainMenu}=Prompts
                   ,keys=#keys{hear_new=HearNew, hear_saved=HearSaved, configure=Configure, exit=Exit}}=Box, Call, Loop) ->
    ?LOG("playing mailbox main menu"),
    cf_call_command:b_flush(Call),
    Messages = get_messages(Box, Call),
    New = count_messages(Messages, ?FOLDER_NEW),
    Saved = count_messages(Messages, ?FOLDER_SAVED),
    ?LOG("mailbox has ~p new and ~p saved messages", [New, Saved]),
    NoopId = cf_call_command:audio_macro(message_count_prompts(New, Saved, Prompts) ++ [{play, MainMenu}], Call),
    case cf_call_command:collect_digits(1, 5000, 2000, NoopId, Call) of
        {error, _} -> 
            ?LOG("error during mailbox main menu"),
            ok;
        {ok, Exit} -> 
            ?LOG("user choose to exit voicemail menu"),
            ok;
        {ok, HearNew} ->
            ?LOG("playing all messages in folder: ~s", [?FOLDER_NEW]),
            Folder = get_folder(Messages, ?FOLDER_NEW),
            case play_messages(Folder, length(Folder), Box, Call) of
                ok -> ok;
                _Else -> main_menu(Box, Call)
            end;
        {ok, HearSaved} ->
            ?LOG("playing all messages in folder: ~s", [?FOLDER_SAVED]),
            Folder = get_folder(Messages, ?FOLDER_SAVED),
            case play_messages(Folder, length(Folder), Box, Call) of
                ok -> ok;
                _Else ->  main_menu(Box, Call)
            end;
        {ok, Configure} ->
            ?LOG("caller choose to change their mailbox configuration"),
            case config_menu(Box, Call) of
                ok -> ok;
                Else -> main_menu(Else, Call)
            end;
        _ ->
            main_menu(Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec message_count_prompts/3 :: (integer(), integer(), #prompts{}) -> proplist().
message_count_prompts(0, 0, #prompts{no_messages=NoMessages}) ->
    [{play, NoMessages}];
message_count_prompts(1, 0, #prompts{you_have=YouHave, new_message=NewMessage}) ->
    [{play, YouHave}
     ,{say, <<"1">>}
     ,{play, NewMessage}
    ];
message_count_prompts(0, 1, #prompts{you_have=YouHave, saved_message=SavedMessage}) ->
    [{play, YouHave}
     ,{say, <<"1">>}
     ,{play, SavedMessage}
    ];
message_count_prompts(1, 1, #prompts{you_have=YouHave, new_and=NewAnd, saved_message=SavedMessage}) ->
    [{play, YouHave}
     ,{say, <<"1">>}
     ,{play, NewAnd}
     ,{say, <<"1">>}
     ,{play, SavedMessage}
    ];
message_count_prompts(New, 0, #prompts{you_have=YouHave, new_messages=NewMessages}) ->
    [{play, YouHave}
     ,{say, wh_util:to_binary(New), <<"number">>}
     ,{play, NewMessages}
    ];
message_count_prompts(New, 1, #prompts{you_have=YouHave, new_and=NewAnd, saved_message=SavedMessage}) ->
    [{play, YouHave}
     ,{say, wh_util:to_binary(New), <<"number">>}
     ,{play, NewAnd}
     ,{say, <<"1">>}
     ,{play, SavedMessage}
    ];
message_count_prompts(0, Saved, #prompts{you_have=YouHave, saved_messages=SavedMessages}) ->
    [{play, YouHave}
     ,{say, wh_util:to_binary(Saved), <<"number">>}
     ,{play, SavedMessages}
    ];
message_count_prompts(1, Saved, #prompts{you_have=YouHave, new_and=NewAnd, saved_messages=SavedMessages}) ->
    [{play, YouHave}
     ,{say, <<"1">>}
     ,{play, NewAnd}
     ,{say, wh_util:to_binary(Saved), <<"number">>}
     ,{play, SavedMessages}
    ];
message_count_prompts(New, Saved, #prompts{you_have=YouHave, new_and=NewAnd, saved_messages=SavedMessages}) ->
    [{play, YouHave}
     ,{say, wh_util:to_binary(New), <<"number">>}
     ,{play, NewAnd}
     ,{say, wh_util:to_binary(Saved), <<"number">>}
     ,{play, SavedMessages}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays back a message then the menu, and continues to loop over the
%% menu utill
%% @end
%%--------------------------------------------------------------------
-spec play_messages/4 :: (wh_json:json_objects(), non_neg_integer(), #mailbox{}, #cf_call{}) -> 'ok' | 'complete'.
play_messages([H|T]=Messages, Count, #mailbox{timezone=Timezone
                                              ,prompts=#prompts{message_number=MsgNum,
                                                                received=Received,
                                                                message_menu=MessageMenu,
                                                                saved=Saved, deleted=Deleted}}=Box, Call) ->
    Message = get_message(H, Call),
    ?LOG("playing mailbox message ~p (~s)", [Count, Message]),
    Prompt = [{play, MsgNum}
              ,{say, wh_util:to_binary(Count - length(Messages) + 1), <<"number">>}
              ,{play, Message}
              ,{play, Received}
              ,{say,  get_unix_epoch(wh_json:get_value(<<"timestamp">>, H), Timezone), <<"current_date_time">>}
              ,{play, MessageMenu}
             ],
    case message_menu(Prompt, Box, Call) of
        {ok, keep} ->
            ?LOG("caller choose to save the message"),
            cf_call_command:b_play(Saved, Call),
            set_folder(?FOLDER_SAVED, H, Box, Call),
            play_messages(T, Count, Box, Call);
        {ok, delete} ->
            ?LOG("caller choose to delete the message"),
            cf_call_command:b_play(Deleted, Call),
            set_folder(?FOLDER_DELETED, H, Box, Call),
            play_messages(T, Count, Box, Call);
        {ok, return} ->
            ?LOG("caller choose to return to the main menu"),
            cf_call_command:b_play(Saved, Call),
            set_folder(?FOLDER_SAVED, H, Box, Call),
            complete;
        {ok, replay} ->
            ?LOG("caller choose to replay"),
            play_messages(Messages, Count, Box, Call);
        {error, _} ->
            ?LOG("error during message playback"),
            ok
    end;
play_messages([], _, _, _) ->
    ?LOG("all messages in folder played to caller"),
    complete.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loops over the message menu after the first play back util the
%% user provides a valid option
%% @end
%%--------------------------------------------------------------------
-type message_menu_returns() :: {'ok', 'keep' | 'delete' | 'return' | 'replay'}.

-spec message_menu/2 :: (#mailbox{}, #cf_call{}) ->
                                {'error', 'channel_hungup' | 'channel_unbridge' | wh_json:json_object()} | message_menu_returns().
-spec message_menu/3 :: ([cf_call_command:audio_macro_prompt(),...], #mailbox{}, #cf_call{}) ->
                                {'error', 'channel_hungup' | 'channel_unbridge' | wh_json:json_object()} | message_menu_returns().
message_menu(#mailbox{prompts=#prompts{message_menu=MessageMenu}}=Box, Call) ->
    message_menu([{play, MessageMenu}], Box, Call).
message_menu(Prompt, #mailbox{keys=#keys{replay=Replay, keep=Keep,
                                         delete=Delete, return_main=ReturnMain}}=Box, Call) ->
    ?LOG("playing message menu"),
    NoopId = cf_call_command:audio_macro(Prompt, Call),
    case cf_call_command:collect_digits(1, 5000, 2000, NoopId, Call)of
        {ok, Keep} -> {ok, keep};
        {ok, Delete} -> {ok, delete};
        {ok, ReturnMain} -> {ok, return};
        {ok, Replay} -> {ok, replay};
        {error, _}=E -> E;
        _ -> message_menu(Box, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec config_menu/2 :: (#mailbox{}, #cf_call{}) -> ok | #mailbox{}.
-spec config_menu/3 :: (#mailbox{}, #cf_call{}, pos_integer()) -> ok | #mailbox{}.

config_menu(Box, Call) ->
    config_menu(Box, Call, 1).

config_menu(#mailbox{keys=#keys{rec_unavailable=RecUnavailable, rec_name=RecName, set_pin=SetPin, return_main=ReturnMain}
                     ,prompts=#prompts{settings_menu=SettingsMenu}}=Box, Call, Loop) when Loop < 4 ->
    ?LOG("playing mailbox configuration menu"),
    {ok, _} = cf_call_command:b_flush(Call),
    case cf_call_command:b_play_and_collect_digit(SettingsMenu, Call) of
        {ok, RecUnavailable} ->
            ?LOG("caller choose to record their unavailable greeting"),
            case record_unavailable_greeting(tmp_file(), Box, Call) of
                ok -> ok;
                Else -> config_menu(Else, Call)
            end;
        {ok, RecName} ->
            ?LOG("caller choose to record their name"),
            case record_name(tmp_file(), Box, Call) of
                ok -> ok;
                Else -> config_menu(Else, Call)
            end;
        {ok, SetPin} ->
            ?LOG("caller choose to change their pin"),
            case change_pin(Box, Call) of
                ok -> ok;
                _Else -> config_menu(Box, Call)
            end;
        {ok, ReturnMain} ->
            ?LOG("caller choose to return to the main menu"),
            Box;
        %% Bulk delete -> delete all voicemails
        %% Reset -> delete all voicemails, greetings, name, and reset pin
        {ok, _} ->
            config_menu(Box, Call, Loop + 1);
        _ -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_unavailable_greeting/3 :: (ne_binary(), #mailbox{}, #cf_call{}) -> ok | #mailbox{}.
record_unavailable_greeting(AttachmentName, #mailbox{unavailable_media_id=undefined}=Box, Call) ->
    MediaId = recording_media_doc(<<"unavailable greeting">>, Box, Call),
    ok = update_doc([<<"media">>, <<"unavailable">>], MediaId, Box, Call),
    record_unavailable_greeting(AttachmentName, Box#mailbox{unavailable_media_id=MediaId}, Call);
record_unavailable_greeting(AttachmentName, #mailbox{prompts=#prompts{record_unavail_greeting=RecordUnavailGreeting
                                                                     ,tone_spec=ToneSpec, saved=Saved, deleted=Deleted}
                                                   ,unavailable_media_id=MediaId}=Box, Call) ->
    ?LOG("recording unavailable greeting  as ~s", [AttachmentName]),
    _NoopId = cf_call_command:audio_macro([{play, RecordUnavailGreeting}
                                           ,{tones, ToneSpec}
                                          ], Call),
    cf_call_command:b_record(AttachmentName, Call),
    case cf_call_command:review_recording(AttachmentName, Box, Call) of
        {ok, record} ->
            record_unavailable_greeting(tmp_file(), Box, Call);
        {ok, save} ->
            store_recording(AttachmentName, MediaId, Call),
            cf_call_command:b_play(Saved, Call),
            Box;
        {ok, no_selection} ->
            cf_call_command:b_play(Deleted, Call),
            ok;
        {error, _} -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_name/3 :: (ne_binary(), #mailbox{}, #cf_call{}) -> ok | #mailbox{}.
record_name(AttachmentName, #mailbox{name_media_id=undefined}=Box, Call) ->
    MediaId = recording_media_doc(<<"users name">>, Box, Call),
    ok = update_doc([<<"media">>, <<"name">>], MediaId, Box, Call),
    record_name(AttachmentName, Box#mailbox{name_media_id=MediaId}, Call);
record_name(AttachmentName, #mailbox{prompts=#prompts{record_name=RecordName, tone_spec=ToneSpec
                                                 ,saved=Saved, deleted=Deleted}
                                   ,name_media_id=MediaId}=Box, Call) ->
    ?LOG("recording name as ~s", [AttachmentName]),
    _NoopId = cf_call_command:audio_macro([{play,  RecordName}
                                           ,{tones, ToneSpec}
                                          ], Call),
    cf_call_command:b_record(AttachmentName, Call),
    case review_recording(AttachmentName, Box, Call) of
        {ok, record} ->
            record_name(tmp_file(), Box, Call);
        {ok, save} ->
            store_recording(AttachmentName, MediaId, Call),
            cf_call_command:b_play(Saved, Call),
            Box;
        {ok, no_selection} ->
            cf_call_command:b_play(Deleted, Call),
            ok;
        {error, _} -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec change_pin/2 :: (#mailbox{}, #cf_call{}) -> ok | #mailbox{}.
change_pin(#mailbox{prompts=#prompts{enter_new_pin=EnterNewPin, reenter_new_pin=ReenterNewPin
                                     ,new_pin_saved=SavedPin, new_pin_bad=BadPin}
                    ,mailbox_id=Id}=Box, #cf_call{account_db=Db}=Call) ->
    ?LOG("requesting new mailbox pin number"),
    try
        {ok, Pin} = cf_call_command:b_play_and_collect_digits(<<"1">>, <<"6">>, EnterNewPin, <<"1">>, Call),
        ?LOG("collected first pin"),
        {ok, Pin} = cf_call_command:b_play_and_collect_digits(<<"1">>, <<"6">>, ReenterNewPin, <<"1">>, Call),
        ?LOG("collected second pin"),
        if byte_size(Pin) == 0 -> throw(pin_empty); true -> ok end,
        ?LOG("entered pin is not empty"),
        {ok, JObj} = couch_mgr:open_doc(Db, Id),
        {ok, _} = couch_mgr:save_doc(Db, wh_json:set_value(<<"pin">>, Pin, JObj)),
        {ok, _} = cf_call_command:b_play(SavedPin, Call),
        ?LOG("updated mailbox pin number"),
        Box
    catch
        _:_ ->
            ?LOG("new pin was invalid, trying again"),
            case cf_call_command:b_play(BadPin, Call) of
                {ok, _} -> change_pin(Box, Call);
                _ -> ok
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new_message/4 :: (ne_binary(), pos_integer(), #mailbox{}, #cf_call{}) -> ok.
new_message(AttachmentName, Length, #mailbox{mailbox_id=Id}=Box, #cf_call{account_db=Db, cid_name=CIDName, cid_number=CIDNumber
                                                                          ,from=From, from_user=FromU, from_realm=FromR
                                                                          ,to=To, to_user=ToU, to_realm=ToR}=Call) ->
    ?LOG("saving new ~bms voicemail message and metadata", [Length]),
    CallID = cf_exe:callid(Call),
    MediaId = message_media_doc(Db, Box),
    {ok, StoreJObj} = store_recording(AttachmentName, MediaId, Call),

    Status = wh_json:get_value([<<"Application-Response">>, <<"Status-Code">>], StoreJObj),
    Loc = wh_json:get_value([<<"Application-Response">>, <<"Headers">>, <<"Location">>], StoreJObj),
    ?LOG("stored voicemail message (~s) ~s", [Status, Loc]),

    Tstamp = new_timestamp(),
    Metadata = wh_json:from_list([{<<"timestamp">>, Tstamp}
                                  ,{<<"from">>, From}
                                  ,{<<"to">>, To}
                                  ,{<<"caller_id_number">>, CIDNumber}
                                  ,{<<"caller_id_name">>, CIDName}
                                  ,{<<"call_id">>, CallID}
                                  ,{<<"folder">>, ?FOLDER_NEW}
                                  ,{<<"length">>, Length}
                                  ,{<<"media_id">>, MediaId}
                                 ]),
    {ok, _} = save_metadata(Metadata, Db, Id),
    ?LOG("stored voicemail metadata for ~s", [MediaId]),

    wapi_notifications:publish_voicemail([{<<"From-User">>, FromU}
                                          ,{<<"From-Realm">>, FromR}
                                          ,{<<"To-User">>, ToU}
                                          ,{<<"To-Realm">>, ToR}
                                          ,{<<"Account-DB">>, Db}
                                          ,{<<"Voicemail-Box">>, Id}
                                          ,{<<"Voicemail-Name">>, MediaId}
                                          ,{<<"Caller-ID-Name">>, CIDName}
                                          ,{<<"Caller-ID-Number">>, CIDNumber}
                                          ,{<<"Voicemail-Timestamp">>, Tstamp}
                                          ,{<<"Voicemail-Length">>, Length}
                                          ,{<<"Call-ID">>, CallID}
                                          | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                         ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_metadata/3 :: (wh_json:json_object(), ne_binary(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', atom()}.
save_metadata(NewMessage, Db, Id) ->
    {ok, JObj} = couch_mgr:open_doc(Db, Id),
    Messages = wh_json:get_value([<<"messages">>], JObj, []),
    case has_message_meta(wh_json:get_value(<<"call_id">>, NewMessage), Messages) of
        true ->
            ?LOG("message meta already exists in VM Messages"),
            {ok, JObj};
        false ->
            case couch_mgr:save_doc(Db, wh_json:set_value([<<"messages">>], [NewMessage | Messages], JObj)) of
                {error, conflict} ->
                    ?LOG("saving resulted in a conflict, trying again"),
                    save_metadata(NewMessage, Db, Id);
                {ok, _}=Ok -> Ok;
                {error, R}=E ->
                    ?LOG("error while storing voicemail metadata: ~p", [R]),
                    E
            end
    end.

-spec has_message_meta/2 :: (ne_binary(), wh_json:json_objects()) -> boolean().
has_message_meta(_, []) -> false;
has_message_meta(NewMsgCallId, Messages) ->
    lists:any(fun(Msg) -> wh_json:get_value(<<"call_id">>, Msg) =:= NewMsgCallId end, Messages).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the mailbox parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec get_mailbox_profile/2 :: (wh_json:json_object(), #cf_call{}) -> #mailbox{}.
get_mailbox_profile(Data, #cf_call{capture_group=CG, account_db=Db, request_user=ReqUser, last_action=LastAct}) ->
    Id = wh_json:get_value(<<"id">>, Data),
    case get_mailbox_doc(Db, Id, CG) of
        {ok, JObj} ->
            MailboxId = wh_json:get_value(<<"_id">>, JObj),
            ?LOG("loaded voicemail box ~s", [MailboxId]),
            Default = #mailbox{},
            %% dont check if the voicemail box belongs to the owner (by default) if the call was not
            %% specificly to him, IE: calling a ring group and going to voicemail should not check
            CheckIfOwner = ((undefined =:= LastAct) orelse (cf_device =:= LastAct)),
            #mailbox{mailbox_id = MailboxId
                     ,skip_instructions =
                         wh_json:is_true(<<"skip_instructions">>, JObj, Default#mailbox.skip_instructions)
                     ,skip_greeting =
                         wh_json:is_true(<<"skip_greeting">>, JObj, Default#mailbox.skip_greeting)
                     ,pin =
                         wh_json:get_binary_value(<<"pin">>, JObj, <<>>)
                     ,timezone =
                         wh_json:get_value(<<"timezone">>, JObj, Default#mailbox.timezone)
                     ,mailbox_number =
                         wh_json:get_binary_value(<<"mailbox">>, JObj, ReqUser)
                     ,require_pin =
                         wh_json:is_true(<<"require_pin">>, JObj)
                     ,check_if_owner =
                         wh_json:is_true(<<"check_if_owner">>, JObj, CheckIfOwner)
                     ,unavailable_media_id =
                         wh_json:get_ne_value([<<"media">>, <<"unavailable">>], JObj)
                     ,name_media_id =
                         wh_json:get_ne_value([<<"media">>, <<"name">>], JObj)
                     ,owner_id =
                         wh_json:get_ne_value(<<"owner_id">>, JObj)
                     ,is_setup =
                         wh_json:is_true(<<"is_setup">>, JObj, false)
                     ,max_message_count =
                         wh_json:get_integer_value(<<"max_message_count">>, JObj, ?MAILBOX_DEFAULT_SIZE)
                     ,max_message_length =
                         find_max_message_length([Data, JObj])
                     ,message_count =
                         length(wh_json:get_value(<<"messages">>, JObj, []))
                     ,exists = true
                    };
        {error, R} ->
            ?LOG("failed to load voicemail box ~s, ~p", [Id, R]),
            #mailbox{}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_mailbox_doc/3 :: (binary(), undefined | binary(), undefined | binary()) -> {ok, wh_json:json_object()} | {error, term()}.
get_mailbox_doc(Db, Id, CaptureGroup) ->
    CGIsEmpty = wh_util:is_empty(CaptureGroup),
    case wh_util:is_empty(Id) of 
        false -> couch_mgr:open_doc(Db, Id);
        true when not CGIsEmpty -> 
            Opts = [{<<"key">>, CaptureGroup}, {<<"include_docs">>, true}], 
            case couch_mgr:get_results(Db, {<<"cf_attributes">>, <<"mailbox_number">>}, Opts) of
                {ok, []} -> {error, not_found};
                {ok, [JObj|_]} -> {ok, wh_json:get_value(<<"doc">>, JObj, wh_json:new())};
                Else -> Else
            end;
        true ->
            {error, "ID and capture group is empty, what voicemail box do you want?"}
    end.
            
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec review_recording/3 :: (ne_binary(), #mailbox{}, #cf_call{}) -> {'ok', 'record' | 'save' | 'no_selection'}.
-spec review_recording/4 :: (ne_binary(), #mailbox{}, #cf_call{}, pos_integer()) -> {'ok', 'record' | 'save' | 'no_selection'}.

review_recording(AttachmentName, Box, Call) ->
    review_recording(AttachmentName, Box, Call, 1).

review_recording(_, _, _, Loop) when Loop > 4 ->
    {ok, no_selection};
review_recording(AttachmentName, #mailbox{keys=#keys{listen=Listen, save=Save, record=Record}
                                     ,prompts=#prompts{review_recording=ReviewRecording}}=Box, Call, Loop) ->
    ?LOG("playing recording review options"),
    case cf_call_command:b_play_and_collect_digit(ReviewRecording, Call) of
        {ok, Listen} ->
            ?LOG("caller choose to replay the recording"),
            _ = cf_call_command:b_play(AttachmentName, Call),
            review_recording(AttachmentName, Box, Call);
        {ok, Record} ->
            ?LOG("caller choose to re-record"),
            {ok, record};
        {ok, Save} ->
            ?LOG("caller choose to save the recording"),
            {ok, save};
        {error, _} ->
            ?LOG("error while waiting for review selection"),
            {ok, no_selection};
        _ ->
            review_recording(AttachmentName, Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording/3 :: (ne_binary(), ne_binary(), #cf_call{}) -> {'ok', wh_json:json_object()} | {'error', wh_json:json_object()}.
store_recording(AttachmentName, MediaId, Call) ->
    ?LOG("storing recording ~s as media ~s", [AttachmentName, MediaId]),
    ok = update_doc(<<"content_type">>, <<"audio/mpeg">>, MediaId, Call),
    cf_call_command:b_store(AttachmentName, get_new_attachment_url(AttachmentName, MediaId, Call), Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_new_attachment_url/3 :: (ne_binary(), ne_binary(), #cf_call{}) -> ne_binary().
get_new_attachment_url(AttachmentName, MediaId, #cf_call{account_db=Db}) ->
    _ = case couch_mgr:open_doc(Db, MediaId) of
            {ok, JObj} ->
                case wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())) of
                    [] -> ok;
                    Existing -> [couch_mgr:delete_attachment(Db, MediaId, Attach) || Attach <- Existing]
                end;
            {error, _} -> ok
        end,
    Rev = case couch_mgr:lookup_doc_rev(Db, MediaId) of
              {ok, R} -> <<"?rev=", R/binary>>;
              _ -> <<>>
          end,

    list_to_binary([couch_mgr:get_url(), Db, "/", MediaId, "/", AttachmentName, Rev]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec message_media_doc/2 :: (ne_binary(), #mailbox{}) -> ne_binary().
message_media_doc(Db, #mailbox{mailbox_number=BoxNum, mailbox_id=Id, timezone=Timezone}) ->
    UtcDateTime = calendar:gregorian_seconds_to_datetime(wh_util:current_tstamp()),
    {{Y,M,D},{H,I,S}} = localtime:utc_to_local(UtcDateTime, wh_util:to_list(Timezone)),
    Name = list_to_binary(["mailbox ", BoxNum, " message "
                           ,wh_util:to_binary(M), "-", wh_util:to_binary(D), "-", wh_util:to_binary(Y)
                           ," " , wh_util:to_binary(H), ":", wh_util:to_binary(I), ":", wh_util:to_binary(S)
                          ]),
    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<"voicemail message media">>}
             ,{<<"source_type">>, <<"voicemail">>}
             ,{<<"source_id">>, Id}
             ,{<<"content_type">>, <<"audio/mpeg">>}
             ,{<<"media_type">>, <<"mp3">>}
             ,{<<"streamable">>, true}],
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), Db, [{type, <<"private_media">>}]),
    {ok, JObj} = couch_mgr:save_doc(Db, Doc),
    wh_json:get_value(<<"_id">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec recording_media_doc/3 :: (ne_binary(), #mailbox{}, #cf_call{}) -> ne_binary().
recording_media_doc(Recording, #mailbox{mailbox_number=BoxNum, mailbox_id=Id}, #cf_call{account_db=Db}) ->
    Name = list_to_binary(["mailbox ", BoxNum, " ", Recording]),
    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<"voicemail recorded/prompt media">>}
             ,{<<"source_type">>, <<"voicemail">>}
             ,{<<"source_id">>, Id}
             ,{<<"content_type">>, <<"audio/mpeg">>}
             ,{<<"media_type">>, <<"mp3">>}
             ,{<<"streamable">>, true}],
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), Db, [{type, <<"media">>}]),
    {ok, JObj} = couch_mgr:save_doc(Db, Doc),
    wh_json:get_value(<<"_id">>, JObj).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_messages/2 :: (#mailbox{}, #cf_call{}) -> wh_json:json_objects().
get_messages(#mailbox{mailbox_id=Id}, #cf_call{account_db=Db}) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} -> wh_json:get_value(<<"messages">>, JObj, []);
        _ -> []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_message/2 :: (wh_json:json_object(), #cf_call{}) -> ne_binary().
get_message(Message, #cf_call{account_db=Db}) ->
    MediaId = wh_json:get_value(<<"media_id">>, Message),
    list_to_binary(["/", Db, "/", MediaId]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec count_messages/2 :: (wh_json:json_objects(), ne_binary()) -> non_neg_integer().
count_messages(Messages, Folder) ->
    lists:sum([1 || Message <- Messages, wh_json:get_value(<<"folder">>, Message) =:= Folder]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_folder/2 :: (wh_json:json_objects(), ne_binary()) -> wh_json:json_objects().
get_folder(Messages, Folder) ->
    [M || M <- Messages, wh_json:get_value(<<"folder">>, M) =:= Folder].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_folder/4 :: (ne_binary(), wh_json:json_object(), #mailbox{}, #cf_call{}) -> no_return().
set_folder(Folder, Message, Box, Call) ->
    ?LOG("setting folder for message to ~s", [Folder]),
    not (wh_json:get_value(<<"folder">>, Message) =:= Folder) andalso
        update_folder(Folder, wh_json:get_value(<<"media_id">>, Message), Box, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_folder/4 :: (ne_binary(), ne_binary(), #mailbox{}, #cf_call{}) -> {ok, wh_json:json_object()} | {error, term()}.
update_folder(_, undefined, _, _) ->
    {error, attachment_undefined};
update_folder(Folder, MediaId, #mailbox{mailbox_id=Id}=Mailbox, #cf_call{account_db=Db}=Call) ->
    Folder =:= ?FOLDER_DELETED andalso update_doc(<<"pvt_deleted">>, true, MediaId, Db),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            Messages = [ update_folder1(Message, Folder, MediaId, wh_json:get_value(<<"media_id">>, Message))
                         || Message <- wh_json:get_value(<<"messages">>, JObj, []) ],
            case couch_mgr:save_doc(Db, wh_json:set_value(<<"messages">>, Messages, JObj)) of
                {error, conflict} ->
                    update_folder(Folder, MediaId, Mailbox, Call);
                {ok, _}=OK ->
                    UpdatedMsgs = wh_json:get_value(<<"messages">>, JObj, []),
                    OK;
                {error, R}=E ->
                    ?LOG("error while updating folder ~s ~p", [Folder, R]),
                    E
            end;
        {error, R}=E ->
            ?LOG("failed ot open mailbox ~s: ~p", [Id, R]),
            E
    end.

update_folder1(Message, Folder, MediaId, MediaId) ->
    wh_json:set_value(<<"folder">>, Folder, Message);
update_folder1(Message, _, _, _) ->
    Message.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_doc/4 :: (list() | binary(), list() | binary(), #mailbox{}, #cf_call{}) -> 'ok' | {'error', atom()}.

update_doc(Key, Value, #mailbox{mailbox_id=Id}, Db) ->
    update_doc(Key, Value, Id, Db);
update_doc(Key, Value, Id, #cf_call{account_db=Db}) ->
    update_doc(Key, Value, Id, Db);
update_doc(Key, Value, Id, Db) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            case couch_mgr:save_doc(Db, wh_json:set_value(Key, Value, JObj)) of
                {error, conflict} ->
                    update_doc(Key, Value, Id, Db);
                {ok, _} ->
                    ok;
                {error, R}=E ->
                    ?LOG("unable to update ~s in ~s, ~p", [Id, Db, R]),
                    E
            end;
        {error, R}=E ->
            ?LOG("unable to update ~s in ~s, ~p", [Id, Db, R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tmp_file/0 :: () -> binary().
tmp_file() ->
     <<(list_to_binary(wh_util:to_hex(crypto:rand_bytes(16))))/binary, ".mp3">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the Universal Coordinated Time (UTC) reported by the
%% underlying operating system (local time is used if universal
%% time is not available) as number of gregorian seconds starting
%% with year 0.
%% @end
%%--------------------------------------------------------------------
-spec new_timestamp/0 :: () -> binary().
new_timestamp() ->
    wh_util:to_binary(wh_util:current_tstamp()).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Accepts Universal Coordinated Time (UTC) and convert it to binary
%% encoded Unix epoch in the provided timezone
%% @end
%%--------------------------------------------------------------------
-spec get_unix_epoch/2 :: (ne_binary(), ne_binary()) -> ne_binary().
get_unix_epoch(Epoch, Timezone) ->
    UtcDateTime = calendar:gregorian_seconds_to_datetime(wh_util:to_integer(Epoch)),
    LocalDateTime = localtime:utc_to_local(UtcDateTime, wh_util:to_list(Timezone)),
    wh_util:to_binary(calendar:datetime_to_gregorian_seconds(LocalDateTime) - ?UNIX_EPOCH_IN_GREGORIAN).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec find_max_message_length/1 :: (wh_json:json_objects()) -> pos_integer().
find_max_message_length([JObj | T]) ->
    case wh_json:get_integer_value(<<"max_message_length">>, JObj) of
        Len when is_integer(Len) andalso Len > 0 -> Len;
        _ -> find_max_message_length(T)
    end;
find_max_message_length([]) ->
    whapps_config:get_integer(<<"voicemail">>, <<"max_message_length">>, 120).
            
