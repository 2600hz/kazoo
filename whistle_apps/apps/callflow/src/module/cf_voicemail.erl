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

-import(cf_call_command, [answer/1, play/2, b_play/2, say/3, tones/2, b_record/2
                          ,b_store/3, b_play_and_collect_digits/5, b_play_and_collect_digit/2
                          ,noop/1, b_flush/1, wait_for_dtmf/1, wait_for_application_or_dtmf/2
                          ,audio_macro/2, flush_dtmf/1
                         ]).

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

          ,tone_spec = [wh_json:from_list([{<<"Frequencies">>, [440]},{<<"Duration-ON">>, 500},{<<"Duration-OFF">>, 100}])]
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
-spec handle/2 :: (Data, Call) -> no_return() when
      Data :: json_object(),
      Call :: #cf_call{}.
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    case wh_json:get_value(<<"action">>, Data, <<"compose">>) of
        <<"compose">> ->
            answer(Call),
            _ = compose_voicemail(get_mailbox_profile(Data, Call), Call),
            CFPid ! {stop};
        <<"check">> ->
            answer(Call),
            check_mailbox(get_mailbox_profile(Data, Call), Call),
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
-spec check_mailbox/2 :: (Box, Call) -> no_return() when
      Box :: #mailbox{},
      Call :: #cf_call{}.
-spec check_mailbox/3 :: (Box, Call, Loop) -> no_return() when
      Box :: #mailbox{},
      Call :: #cf_call{},
      Loop :: non_neg_integer().

check_mailbox(Box, Call) ->
    %% Wrapper to initalize the attempt counter
    check_mailbox(Box, Call, 1).

check_mailbox(#mailbox{max_login_attempts=MaxLoginAttempts, prompts=#prompts{abort_login=AbortLogin}}
              ,Call, Loop) when Loop > MaxLoginAttempts ->
    %% if we have exceeded the maximum loop attempts then terminate this call
    ?LOG("maximum number of invalid attempts to check mailbox"),
    b_play(AbortLogin, Call);
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
    b_play(Prompts#prompts.no_access, Call);
check_mailbox(#mailbox{prompts=#prompts{enter_password=EnterPass, invalid_login=InvalidLogin}
		       ,pin=Pin}=Box, Call, Loop) ->
    {ok, _} = try
		  %% Request the pin number from the caller but crash if it doesnt match the mailbox
		  ?LOG("requesting pin number to check mailbox"),
		  {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterPass, <<"1">>, Call)
	      catch
		  _:R ->
		      ?LOG("invalid mailbox login ~w", [R]),
		      {ok, _} = b_play(InvalidLogin, Call),
		      check_mailbox(Box#mailbox{exists=false}, Call, Loop+1)
	      end,
    main_menu(Box, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_mailbox/3 :: (Box, Call, Loop) -> no_return() when
      Box :: #mailbox{},
      Call :: #cf_call{},
      Loop :: pos_integer().
find_mailbox(#mailbox{prompts=#prompts{enter_mailbox=EnterBox, enter_password=EnterPwd, invalid_login=Invalid}}=Box
             ,#cf_call{account_db=Db}=Call, Loop) ->
    ?LOG("requesting mailbox number to check"),
    {ok, Mailbox} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterBox, <<"1">>, Call),
    BoxNum = try wh_util:to_integer(Mailbox) catch _:_ -> 0 end,

    %% find the voicemail box, by making a fake 'callflow data payload' we look for it now because if the
    %% caller is the owner, and the pin is not required then we skip requesting the pin
    case couch_mgr:get_results(Db, {<<"vmboxes">>, <<"listing_by_mailbox">>}, [{<<"key">>, BoxNum}]) of
        {ok, [JObj]} ->
            ReqBox = get_mailbox_profile(wh_json:from_list([{<<"id">>, wh_json:get_value(<<"id">>, JObj)}]), Call),
            check_mailbox(ReqBox, Call, Loop);
        _ ->
            %% we dont want to alert the caller that the mailbox number doesnt match or people could use
            %% that to determine the mailboxs on this system then try brute force to guess the pwd.
            ?LOG("invalid mailbox ~s, faking user out...", [Mailbox]),
            {ok, _} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterPwd, <<"1">>, Call),
            {ok, _} = b_play(Invalid, Call),
            check_mailbox(Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_voicemail/2 :: (Box, Call) -> no_return() when
      Box :: #mailbox{},
      Call :: #cf_call{}.
compose_voicemail(#mailbox{check_if_owner=true, owner_id=OwnerId}=Box, #cf_call{owner_id=OwnerId}=Call) when
      is_binary(OwnerId), OwnerId =/= <<>> ->
    ?LOG("caller is the owner of this mailbox"),
    ?LOG("overriding action as check (instead of compose)"),
    check_mailbox(Box, Call);
compose_voicemail(#mailbox{exists=false, prompts=#prompts{no_mailbox=NoMailbox}}, Call) ->
    ?LOG("attempted to compose voicemail for missing mailbox"),
    b_play(NoMailbox, Call);
compose_voicemail(#mailbox{skip_greeting=SkipGreeting, skip_instructions=SkipInstructions
			   ,prompts=#prompts{record_instructions=RecordInstructions}
			   ,keys=#keys{login=Login}}=Box, Call) ->
    ?LOG("playing mailbox greeting to caller"),

    not SkipGreeting andalso play_greeting(Box, Call),
    not SkipInstructions andalso play(RecordInstructions, Call),

    _NoopId = noop(Call),

    %% timeout after 5 min for saftey, so this process cant hang around forever
    case wait_for_application_or_dtmf(<<"noop">>, 300000) of
        {ok, _} ->
            record_voicemail(tmp_file(), Box, Call);
        {dtmf, Digit} ->
            _ = b_flush(Call),
            case Digit of
                Login ->
                    check_mailbox(Box, Call);
                _ ->
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
-spec play_greeting/2 :: (Box, Call) -> no_return() when
      Box :: #mailbox{},
      Call :: #cf_call{}.
play_greeting(#mailbox{prompts=#prompts{person_at_exten=PersonAtExten, not_available=NotAvailable}
		       ,unavailable_media_id=undefined, mailbox_number=Mailbox}, Call) ->
    audio_macro([{play, PersonAtExten}
                 ,{say,  Mailbox}
                 ,{play, NotAvailable}
                ], Call);
play_greeting(#mailbox{unavailable_media_id=Id}, #cf_call{account_db=Db}=Call) ->
    play(<<$/, Db/binary, $/, Id/binary>>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_voicemail/3 :: (RecordingName, Box, Call) -> no_return() when
      RecordingName :: binary(),
      Box :: #mailbox{},
      Call :: #cf_call{}.
record_voicemail(RecordingName, #mailbox{prompts=#prompts{tone_spec=ToneSpec, saved=Saved}}=Box, Call) ->
    tones(ToneSpec, Call),
    ?LOG("composing new voicemail"),
    case b_record(RecordingName, Call) of
        {ok, _Msg} ->
            case review_recording(RecordingName, Box, Call) of
                {ok, record} ->
                    record_voicemail(tmp_file(), Box, Call);
		{ok, save} ->
		    new_message(RecordingName, Box, Call),
                    b_play(Saved, Call);
                {ok, no_selection} ->
		    new_message(RecordingName, Box, Call),
                    b_play(Saved, Call)
            end;
        {error, channel_hungup} ->
            _ = cf_call_command:wait_for_application(<<"record">>, <<"RECORD_STOP">>),
            new_message(RecordingName, Box, Call);
	{error, execution_failure} ->
            ?LOG("media server exploded"),
	    ok %% something happened Whistle-side, nothing to do for now
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec setup_mailbox/2 :: (Box, Call) -> #mailbox{} when
      Box :: #mailbox{},
      Call :: #cf_call{}.
setup_mailbox(#mailbox{prompts=#prompts{setup_intro=SetupIntro
                                        ,setup_rec_greet=SetupRecGreet
                                        ,setup_complete=SetupComplete}}=Box, Call) ->
    {ok, _} = b_play(SetupIntro, Call),
    change_pin(Box, Call),
    {ok, _} = b_play(SetupRecGreet, Call),
    Box1 = record_unavailable_greeting(tmp_file(), Box, Call),
    ok = update_doc(<<"is_setup">>, true, Box1, Call),
    {ok, _} = b_play(SetupComplete, Call),
    Box1#mailbox{is_setup=true}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec main_menu/2 :: (Box, Call) -> no_return() when
      Box :: #mailbox{},
      Call :: #cf_call{}.
-spec main_menu/3 :: (Box, Call, Loop) -> no_return() when
      Box :: #mailbox{},
      Call :: #cf_call{},
      Loop :: non_neg_integer().

main_menu(#mailbox{is_setup=false}=Box, Call) ->
    main_menu(setup_mailbox(Box, Call), Call, 1);
main_menu(Box, Call) ->
    main_menu(Box, Call, 1).

main_menu(#mailbox{prompts=#prompts{goodbye=Goodbye}}, Call, Loop) when Loop > 4 ->
    %% If there have been too may loops with no action from the caller this
    %% is likely a abandonded channel, terminate
    ?LOG("entered main menu with too many invalid entries"),
    b_play(Goodbye, Call);
main_menu(#mailbox{prompts=#prompts{main_menu=MainMenu}=Prompts
		   ,keys=#keys{hear_new=HearNew, hear_saved=HearSaved, configure=Configure, exit=Exit}}=Box, Call, Loop) ->
    ?LOG("playing mailbox main menu"),
    {ok, _} = b_flush(Call),
    Messages = get_messages(Box, Call),
    New = count_messages(Messages, ?FOLDER_NEW),
    Saved = count_messages(Messages, ?FOLDER_SAVED),
    NoopId = audio_macro(message_count_prompts(New, Saved, Prompts) ++ [{play, MainMenu}], Call),
    case cf_call_command:collect_digits(1, 5000, 2000, NoopId, Call) of
	{ok, HearNew} ->
            Folder = get_folder(Messages, ?FOLDER_NEW),
	    play_messages(Folder, length(Folder), Box, Call),
	    main_menu(Box, Call);
	{ok, HearSaved} ->
            Folder = get_folder(Messages, ?FOLDER_SAVED),
	    play_messages(Folder, length(Folder), Box, Call),
	    main_menu(Box, Call);
	{ok, Configure} ->
	    B = config_menu(Box, Call),
	    main_menu(B, Call);
	{ok, Exit} ->
            update_mwi(New, Saved, Box, Call),
	    ok;
	{error, _} ->
            update_mwi(New, Saved, Box, Call),
	    ok;
	_ ->
	    main_menu(Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec message_count_prompts/3 :: (New, Saved, Prompts) -> proplist() when
      New :: integer(),
      Saved :: integer(),
      Prompts :: #prompts{}.
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
-spec play_messages/4 :: (Messages, Count, Box, Call) -> 'ok' when
      Messages :: json_objects(),
      Count :: non_neg_integer(),
      Box :: #mailbox{},
      Call :: #cf_call{}.
play_messages([H|T]=Messages, Count, #mailbox{timezone=Timezone
					      ,prompts=#prompts{message_number=MsgNum,
								received=Received,
								message_menu=MessageMenu,
								saved=Saved, deleted=Deleted}}=Box, Call) ->
    ?LOG("reviewing mailbox message"),
    Message = get_message(H, Call),
    Prompt = [{play, MsgNum}
              ,{say, wh_util:to_binary(Count - length(Messages) + 1), <<"number">>}
              ,{play, Message}
              ,{play, Received}
              ,{say,  get_unix_epoch(wh_json:get_value(<<"timestamp">>, H), Timezone), <<"current_date_time">>}
              ,{play, MessageMenu}
             ],
    case message_menu(Prompt, Box, Call) of
	{ok, keep} ->
	    {ok, _} = b_play(Saved, Call),
	    set_folder(?FOLDER_SAVED, H, Box, Call),
	    play_messages(T, Count, Box, Call);
	{ok, delete} ->
	    {ok, _} = b_play(Deleted, Call),
	    set_folder(?FOLDER_DELETED, H, Box, Call),
	    play_messages(T, Count, Box, Call);
	{ok, return} ->
	    {ok, _} = b_play(Saved, Call),
	    set_folder(?FOLDER_SAVED, H, Box, Call),
            ok;
	{ok, replay} ->
	    play_messages(Messages, Count, Box, Call);
        {error, _} ->
            ok
    end;
play_messages([], _, _, _) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loops over the message menu after the first play back util the
%% user provides a valid option
%% @end
%%--------------------------------------------------------------------
-spec message_menu/2 :: (Box, Call) -> {'error', 'channel_hungup' | 'channel_unbridge' | 'execution_failure'} | {'ok', 'keep' | 'delete' | 'return' | 'replay'} when
      Box :: #mailbox{},
      Call :: #cf_call{}.
-spec message_menu/3 :: (Prompt, Box, Call) -> {'error', 'channel_hungup' | 'channel_unbridge' | 'execution_failure'} | {'ok', 'keep' | 'delete' | 'return' | 'replay'} when
      Prompt :: proplist(),
      Box :: #mailbox{},
      Call :: #cf_call{}.

message_menu(#mailbox{prompts=#prompts{message_menu=MessageMenu}}=Box, Call) ->
    message_menu([{play, MessageMenu}], Box, Call).

message_menu(Prompt, #mailbox{keys=#keys{replay=Replay, keep=Keep,
                                         delete=Delete, return_main=ReturnMain}}=Box, Call) ->
    NoopId = audio_macro(Prompt, Call),
    case cf_call_command:collect_digits(1, 5000, 2000, NoopId, Call)of
	{ok, Keep} ->
            {ok, keep};
	{ok, Delete} ->
            {ok, delete};
	{ok, ReturnMain} ->
            {ok, return};
	{ok, Replay} ->
            {ok, replay};
        {error, _}=E ->
            E;
	_ ->
	    message_menu(Box, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec config_menu/2 :: (Box, Call) -> #mailbox{} when
      Box :: #mailbox{},
      Call :: #cf_call{}.
-spec config_menu/3 :: (Box, Call, Loop) -> #mailbox{} when
      Box :: #mailbox{},
      Call :: #cf_call{},
      Loop :: pos_integer().

config_menu(Box, Call) ->
    config_menu(Box, Call, 1).

config_menu(#mailbox{keys=#keys{rec_unavailable=RecUnavailable, rec_name=RecName, set_pin=SetPin, return_main=ReturnMain}
                     ,prompts=#prompts{settings_menu=SettingsMenu}}=Box, Call, Loop) when Loop < 4 ->
    ?LOG("playing mailbox configuration menu"),
    {ok, _} = b_flush(Call),
    case b_play_and_collect_digit(SettingsMenu, Call) of
	{ok, RecUnavailable} ->
	    B = record_unavailable_greeting(tmp_file(), Box, Call),
	    config_menu(B, Call);
	{ok, RecName} ->
	    B = record_name(tmp_file(), Box, Call),
	    config_menu(B, Call);
	{ok, SetPin} ->
	    change_pin(Box, Call),
	    config_menu(Box, Call);
	{ok, ReturnMain} ->
            Box;
	%% Bulk delete -> delete all voicemails
	%% Reset -> delete all voicemails, greetings, name, and reset pin
	{ok, _} ->
	    config_menu(Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_unavailable_greeting/3 :: (RecordingName, Box, Call) -> #mailbox{} when
      RecordingName :: binary(),
      Box :: #mailbox{},
      Call :: #cf_call{}.
record_unavailable_greeting(RecordingName, #mailbox{unavailable_media_id=undefined}=Box, Call) ->
    MediaId = recording_media_doc(<<"unavailable greeting">>, Box, Call),
    ok = update_doc([<<"media">>, <<"unavailable">>], MediaId, Box, Call),
    record_unavailable_greeting(RecordingName, Box#mailbox{unavailable_media_id=MediaId}, Call);
record_unavailable_greeting(RecordingName, #mailbox{prompts=#prompts{record_unavail_greeting=RecordUnavailGreeting
                                                                     ,tone_spec=ToneSpec, saved=Saved, deleted=Deleted}
                                                   ,unavailable_media_id=MediaId}=Box, Call) ->
    ?LOG("recording unavailable greeting"),
    _NoopId = audio_macro([{play, RecordUnavailGreeting}
			   ,{tones, ToneSpec}]
			  ,Call),
    {ok, _} = b_record(RecordingName, Call),
    case review_recording(RecordingName, Box, Call) of
	{ok, record} ->
	    record_unavailable_greeting(tmp_file(), Box, Call);
	{ok, save} ->
	    {ok, _} = store_recording(RecordingName, MediaId, Call),
            {ok, _} = b_play(Saved, Call),
            Box;
        {ok, no_selection} ->
            {ok, _} = b_play(Deleted, Call),
            Box
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_name/3 :: (RecordingName, Box, Call) -> #mailbox{} when
      RecordingName :: binary(),
      Box :: #mailbox{},
      Call :: #cf_call{}.
record_name(RecordingName, #mailbox{name_media_id=undefined}=Box, Call) ->
    MediaId = recording_media_doc(<<"users name">>, Box, Call),
    ok = update_doc([<<"media">>, <<"name">>], MediaId, Box, Call),
    record_name(RecordingName, Box#mailbox{name_media_id=MediaId}, Call);
record_name(RecordingName, #mailbox{prompts=#prompts{record_name=RecordName, tone_spec=ToneSpec
                                                 ,saved=Saved, deleted=Deleted}
                                   ,name_media_id=MediaId}=Box, Call) ->
    ?LOG("recording name"),
    _NoopId = audio_macro([{play,  RecordName}
			   ,{tones, ToneSpec}
			  ], Call),
    {ok, _} = b_record(RecordingName, Call),
    case review_recording(RecordingName, Box, Call) of
	{ok, record} ->
	    record_name(tmp_file(), Box, Call);
	{ok, save} ->
	    {ok, _} = store_recording(RecordingName, MediaId, Call),
            {ok, _} = b_play(Saved, Call),
            Box;
        {ok, no_selection} ->
            {ok, _} = b_play(Deleted, Call),
            Box
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec change_pin/2 :: (Box, Call) -> stop | continue when
      Box :: #mailbox{},
      Call :: #cf_call{}.
change_pin(#mailbox{prompts=#prompts{enter_new_pin=EnterNewPin, reenter_new_pin=ReenterNewPin
                                    ,new_pin_saved=SavedPin, new_pin_bad=BadPin}
		    ,mailbox_id=Id}=Box, #cf_call{account_db=Db}=Call) ->
    ?LOG("requesting new mailbox pin number"),
    try
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterNewPin, <<"1">>, Call),
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, ReenterNewPin, <<"1">>, Call),
        if byte_size(Pin) == 0 -> throw(pin_empty); true -> ok end,
        {ok, JObj} = couch_mgr:open_doc(Db, Id),
        {ok, _} = couch_mgr:save_doc(Db, wh_json:set_value(<<"pin">>, Pin, JObj)),
        {ok, _} = b_play(SavedPin, Call),
        ?LOG("updated mailbox pin number")
    catch
        _:_ ->
            ?LOG("new pin was invalid, trying again"),
            {ok, _} = b_play(BadPin, Call),
            change_pin(Box, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new_message/3 :: (RecordingName, Box, Call) -> no_return() when
      RecordingName :: binary(),
      Box :: #mailbox{},
      Call :: #cf_call{}.
new_message(RecordingName, #mailbox{mailbox_id=Id}=Box, #cf_call{account_db=Db, call_id=CallID
                                                             ,from=From, from_user=FromU, from_realm=FromR
                                                             ,to=To, to_user=ToU, to_realm=ToR
							     ,cid_name=CIDName, cid_number=CIDNumber}=Call) ->
    MediaId = message_media_doc(Db, Box),
    {ok, StoreJObj} = store_recording(RecordingName, MediaId, Call),

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
				  ,{<<"media_id">>, MediaId}
				 ]),
    {ok, _} = save_metadata(Metadata, Db, Id),
    ?LOG("stored voicemail metadata for ~s", [MediaId]),

    {ok, JSON} = cf_api:new_voicemail([{<<"From-User">>, FromU}
				       ,{<<"From-Realm">>, FromR}
				       ,{<<"To-User">>, ToU}
				       ,{<<"To-Realm">>, ToR}
				       ,{<<"Account-DB">>, Db}
				       ,{<<"Voicemail-Box">>, Id}
				       ,{<<"Voicemail-Name">>, MediaId}
				       ,{<<"Caller-ID-Name">>, CIDName}
				       ,{<<"Caller-ID-Number">>, CIDNumber}
				       ,{<<"Voicemail-Timestamp">>, Tstamp}
				       ,{<<"Call-ID">>, CallID}
				       | wh_api:default_headers(<<>>, <<"notification">>, <<"new_voicemail">>, ?APP_NAME, ?APP_VERSION)
				      ]),
    ?LOG("new voicemail message ~s whistle API broadcast", [MediaId]),
    amqp_util:callevt_publish(CallID, JSON, ?NOTIFY_VOICEMAIL_NEW),
    update_mwi(Box, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_metadata/3 :: (NewMessage, Db, Id) -> tuple(ok, json_object()) | tuple(error, atom()) when
      NewMessage :: json_object(),
      Db :: binary(),
      Id :: binary().
save_metadata(NewMessage, Db, Id) ->
    {ok, JObj} = couch_mgr:open_doc(Db, Id),
    NewMessages=[NewMessage | wh_json:get_value([<<"messages">>], JObj, [])],
    case couch_mgr:save_doc(Db, wh_json:set_value([<<"messages">>], NewMessages, JObj)) of
        {error, conflict} ->
            save_metadata(NewMessage, Db, Id);
        {ok, _}=Ok -> Ok;
        {error, R}=E ->
            ?LOG("error while storing voicemail metadata ~w", [R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the mailbox parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec get_mailbox_profile/2 :: (Data, Call) -> #mailbox{} when
      Data :: json_object(),
      Call :: #cf_call{}.
get_mailbox_profile(Data, #cf_call{account_db=Db, request_user=ReqUser, last_action=LastAct}) ->
    Id = wh_json:get_value(<<"id">>, Data, <<"undefined">>),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            ?LOG("loaded voicemail box ~s", [Id]),
            Default = #mailbox{},
            %% dont check if the voicemail box belongs to the owner (by default) if the call was not
            %% specificly to him, IE: calling a ring group and going to voicemail should not check
            CheckIfOwner = ((undefined =:= LastAct) orelse (cf_device =:= LastAct)),
            #mailbox{mailbox_id = Id
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
                         wh_json:get_value([<<"media">>, <<"unavailable">>], JObj)
                     ,name_media_id =
                         wh_json:get_value([<<"media">>, <<"name">>], JObj)
                     ,owner_id =
                         wh_json:get_value(<<"owner_id">>, JObj)
                     ,is_setup =
                         wh_json:is_true(<<"is_setup">>, JObj, false)
                     ,exists = true
                    };
        {error, R} ->
            ?LOG("failed to load voicemail box ~s, ~w", [Id, R]),
            #mailbox{}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec review_recording/3 :: (RecordingName, Box, Call) -> {'ok', 'record' | 'save' | 'no_selection'} when
      RecordingName :: binary(),
      Box :: #mailbox{},
      Call :: #cf_call{}.
-spec review_recording/4 :: (RecordingName, Box, Call, Loop) -> {'ok', 'record' | 'save' | 'no_selection'} when
      RecordingName :: binary(),
      Box :: #mailbox{},
      Call :: #cf_call{},
      Loop :: non_neg_integer().

review_recording(RecordingName, Box, Call) ->
    review_recording(RecordingName, Box, Call, 1).

review_recording(_, _, _, Loop) when Loop > 4 ->
    {ok, no_selection};
review_recording(RecordingName, #mailbox{keys=#keys{listen=Listen, save=Save, record=Record}
				     ,prompts=#prompts{review_recording=ReviewRecording}}=Box, Call, Loop) ->
    ?LOG("playing review options"),
    case b_play_and_collect_digit(ReviewRecording, Call) of
        {ok, Listen} ->
            _ = b_play(RecordingName, Call),
            review_recording(RecordingName, Box, Call);
        {ok, Record} ->
            {ok, record};
        {ok, Save} ->
            {ok, save};
	{error, _} ->
            ?LOG("channel hungup while waiting for dtmf"),
	    {ok, no_selection};
        _ ->
	    review_recording(RecordingName, Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording/3 :: (RecordingName, MediaId, Call) -> tuple(ok, json_object()) | tuple(error, execution_failure) when
      RecordingName :: binary(),
      MediaId :: binary(),
      Call :: #cf_call{}.
store_recording(RecordingName, MediaId, Call) ->
    ?LOG("storing recording ~s as media ~s", [RecordingName, MediaId]),
    b_store(RecordingName, get_attachment_path(RecordingName, MediaId, Call), Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_attachment_path/3 :: (MediaId, RecordingName, Call) -> binary() when
      RecordingName :: binary(),
      MediaId :: binary(),
      Call :: #cf_call{}.
get_attachment_path(RecordingName, MediaId, #cf_call{account_db=Db}) ->
    <<(couch_mgr:get_url())/binary
      ,Db/binary
      ,$/, MediaId/binary
      ,$/, RecordingName/binary
      ,(lookup_doc_rev(Db, MediaId))/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec lookup_doc_rev/2 :: (Db, Id) -> binary() when
      Db :: binary(),
      Id :: binary().
lookup_doc_rev(Db, Id) ->
    case couch_mgr:lookup_doc_rev(Db, Id) of
        {ok, Rev} ->
            <<"?rev=", Rev/binary>>;
        _ ->
            <<>>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec message_media_doc/2 :: (Db, Box) -> binary() when
      Db :: binary(),
      Box :: #mailbox{}.

message_media_doc(Db, #mailbox{mailbox_number=BoxNum, mailbox_id=Id, timezone=Timezone}) ->
    UtcDateTime = calendar:gregorian_seconds_to_datetime(wh_util:current_tstamp()),
    {{Y,M,D},{H,I,S}} = localtime:utc_to_local(UtcDateTime, wh_util:to_list(Timezone)),
    Name = <<"mailbox ", BoxNum/binary, " message "
             ,(wh_util:to_binary(M))/binary, $-, (wh_util:to_binary(D))/binary, $-, (wh_util:to_binary(Y))/binary
             ,$ , (wh_util:to_binary(H))/binary, $:, (wh_util:to_binary(I))/binary, $:, (wh_util:to_binary(S))/binary>>,
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
-spec recording_media_doc/3 :: (Recording, Box, Call) -> binary() when
      Recording :: binary(),
      Box :: #mailbox{},
      Call :: #cf_call{}.

recording_media_doc(Recording, #mailbox{mailbox_number=BoxNum, mailbox_id=Id}, #cf_call{account_db=Db}) ->
    Name = <<"mailbox ", BoxNum/binary, $ , Recording/binary>>,
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
-spec get_messages/2 :: (Mailbox, Call) -> json_objects() when
      Mailbox :: #mailbox{},
      Call :: #cf_call{}.
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
-spec get_message/2 :: (Message, Call) -> binary() when
      Message :: json_object(),
      Call :: #cf_call{}.
get_message(Message, #cf_call{account_db=Db}) ->
    MediaId = wh_json:get_value(<<"media_id">>, Message),
    <<$/, Db/binary, $/, MediaId/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec count_messages/2 :: (Messages, Folder) -> integer() when
      Messages :: json_objects(),
      Folder :: binary().
count_messages(Messages, Folder) ->
    lists:foldr(fun(Message, Count) ->
                       case wh_json:get_value(<<"folder">>, Message) of
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
-spec get_folder/2 :: (Messages, Folder) -> json_objects() when
      Messages :: json_objects(),
      Folder :: binary().
get_folder(Messages, Folder) ->
    lists:foldr(fun(Message, Acc) ->
			case wh_json:get_value(<<"folder">>, Message) of
			    Folder -> [Message|Acc];
			    _ -> Acc
			end
		end, [], Messages).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_folder/4 :: (Folder, Message, Box, Call) -> no_return() when
      Folder :: binary(),
      Message :: json_object(),
      Box :: #mailbox{},
      Call :: #cf_call{}.
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
-spec update_folder/4 :: (Folder, MediaId, Box, Call) -> no_return() when
      Folder :: binary(),
      MediaId :: binary(),
      Box :: #mailbox{},
      Call :: #cf_call{}.
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
                    New = count_messages(UpdatedMsgs, ?FOLDER_NEW),
                    Saved = count_messages(UpdatedMsgs, ?FOLDER_SAVED),
                    update_mwi(New, Saved, Mailbox, Call),
                    OK;
                {error, R}=E ->
                    ?LOG("error while updating folder ~s ~s", [Folder, R]),
                    E
            end;
        {error, _}=E ->
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
-spec update_doc/4 :: (Key, Value, Id, Db) -> ok | tuple(error, atom()) when
      Key :: list() | binary(),
      Value :: json_term(),
      Id :: #mailbox{} | binary(),
      Db :: #cf_call{} | binary().

update_doc(Key, Value, #mailbox{mailbox_id=Id}, #cf_call{account_db=Db}) ->
    update_doc(Key, Value, Id, Db);
update_doc(Key, Value, Id, Db) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            case couch_mgr:save_doc(Db, wh_json:set_value(Key, Value, JObj)) of
                {error, conflict} ->
                    update_doc(Key, Value, Id, Db);
                {ok, _} ->
                    ok;
                {error, _}=E ->
                    ?LOG("unable to update ~s in ~s, ~p", [Id, Db, E])
            end;
        {error, _}=E ->
            ?LOG("unable to update ~s in ~s, ~p", [Id, Db, E])
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
-spec get_unix_epoch/2 :: (Epoch, Timezone) -> binary() when
      Epoch :: binary(),
      Timezone :: binary().
get_unix_epoch(Epoch, Timezone) ->
    UtcDateTime = calendar:gregorian_seconds_to_datetime(wh_util:to_integer(Epoch)),
    LocalDateTime = localtime:utc_to_local(UtcDateTime, wh_util:to_list(Timezone)),
    wh_util:to_binary(calendar:datetime_to_gregorian_seconds(LocalDateTime) - 62167219200).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends SIP notify to all devices owned by the same owner as the
%% mailbox with new message counts.
%% @end
%%--------------------------------------------------------------------
-spec update_mwi/2 :: (Box, Call) -> 'ok' when
      Box :: #mailbox{},
      Call :: #cf_call{}.
-spec update_mwi/4 :: (New, Saved, Box, Call) -> 'ok' when
      New :: integer(),
      Saved :: integer(),
      Box :: #mailbox{},
      Call :: #cf_call{}.

update_mwi(Box, Call) ->
    Messages = get_messages(Box, Call),
    New = count_messages(Messages, ?FOLDER_NEW),
    Saved = count_messages(Messages, ?FOLDER_SAVED),
    update_mwi(New, Saved, Box, Call).

update_mwi(New, Saved, #mailbox{owner_id=OwnerId}, #cf_call{account_db=Db}) ->
    case couch_mgr:get_results(Db, <<"cf_attributes/owned_devices">>, [{<<"key">>, OwnerId}]) of
        {ok, []} ->
            ?LOG("mwi update found no devices owned by ~s", [OwnerId]),
            ok;
        {ok, Devices} ->
            lists:foreach(fun(Device) ->
				  User = wh_json:get_value([<<"value">>, <<"sip_username">>], Device),
				  Realm = wh_json:get_value([<<"value">>, <<"sip_realm">>], Device),
				  Command = wh_json:from_list([{<<"Notify-User">>, User}
							       ,{<<"Notify-Realm">>, Realm}
							       ,{<<"Messages-New">>, wh_util:to_binary(New)}
							       ,{<<"Messages-Saved">>, wh_util:to_binary(Saved)}
							       | wh_api:default_headers(<<>>, <<"notify">>, <<"mwi">>, ?APP_NAME, ?APP_VERSION)
							      ]),
				  ?LOG("sending mwi update to ~s@~s ~p:~p", [User, Realm, New, Saved]),
				  {ok, Payload} = wh_api:mwi_update(Command),
				  amqp_util:callmgr_publish(Payload, <<"application/json">>, ?KEY_SIP_NOTIFY)
			  end, Devices),
            ok;
        Error ->
            ?LOG("mwi update found ~s in ~s lookup error ~p", [OwnerId, Db, Error]),
            ok
    end.
