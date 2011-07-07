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

-define(UNAVAILABLE_GREETING, <<"unavailable_greeting.mp3">>).
-define(NAME_RECORDING, <<"name_recording.mp3">>).

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

          ,setup_intro = <<"/system_media/vm-setup_intro">>
          ,setup_rec_greet = <<"/system_media/vm-setup_rec_greeting">>
          ,setup_complete = <<"/system_media/vm-setup_complete">>

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

          ,enter_new_pin = <<"/system_media/vm-enter_new_pin">>
          ,reenter_new_pin = <<"/system_media/vm-enter_new_pin_confirm">>
          ,new_pin_saved = <<"/system_media/vm-pin_set">>
          ,new_pin_bad = <<"/system_media/vm-pin_invalid">>

          ,tone_spec = [{struct, [{<<"Frequencies">>, [440]},{<<"Duration-ON">>, 500},{<<"Duration-OFF">>, 100}]}]
         }).

-record(mailbox, {
           has_unavailable_greeting = false :: boolean()
          ,mailbox_id = undefined :: undefined | binary()
          ,mailbox_number = <<>> :: binary()
          ,exists = false :: boolean()
          ,skip_instructions = false :: boolean()
          ,skip_greeting = false :: boolean()
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
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
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
-spec(check_mailbox/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
-spec(check_mailbox/3 :: (Box :: #mailbox{}, Call :: #cf_call{}, Loop :: non_neg_integer()) -> no_return()).
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
check_mailbox(#mailbox{require_pin=false, owner_id=OwnerId}=Box, #cf_call{owner_id=OwnerId}=Call, _) ->
    %% If this is the owner of the mailbox calling in and it doesn't require a pin then jump
    %% right to the main menu
    main_menu(Box, Call);
check_mailbox(#mailbox{prompts=Prompts, pin = <<>>}, Call, _) ->
    %% If the caller is not the owner or the mailbox requires a pin to access it but has none set
    %% then terminate this call.
    ?LOG("attempted to sign into a mailbox with no pin"),
    b_play(Prompts#prompts.goodbye, Call);
check_mailbox(#mailbox{prompts=#prompts{enter_password=EnterPass, invalid_login=InvalidLogin}
		       ,pin=Pin}=Box, Call, Loop) ->
    try
        %% Request the pin number from the caller but crash if it doesnt match the mailbox
        ?LOG("requesting pin number to check mailbox"),
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterPass, <<"1">>, <<"8000">>, Call),
        main_menu(Box, Call)
    catch
        _:R ->
            ?LOG("invalid mailbox login ~w", [R]),
            _ = b_play(InvalidLogin, Call),
            check_mailbox(Box, Call, Loop+1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(find_mailbox/3 :: (Box :: #mailbox{}, Call :: #cf_call{}, Loop :: non_neg_integer()) -> no_return()).

find_mailbox(#mailbox{prompts=#prompts{enter_mailbox=EnterBox, enter_password=EnterPwd, invalid_login=Invalid}}=Box
             ,#cf_call{account_db=Db}=Call, Loop) ->
    ?LOG("requesting mailbox number to check"),
    {ok, Mailbox} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterBox, <<"1">>, <<"8000">>, Call),
    BoxNum = try whistle_util:to_integer(Mailbox) catch _:_ -> 0 end,

    %% find the voicemail box, by making a fake 'callflow data payload' we look for it now because if the
    %% caller is the owner, and the pin is not required then we skip requesting the pin
    case couch_mgr:get_results(Db, {<<"vmboxes">>, <<"listing_by_mailbox">>}, [{<<"key">>, BoxNum}]) of
        {ok, [JObj]} ->
            ReqBox = get_mailbox_profile({struct, [{<<"id">>, wh_json:get_value(<<"id">>, JObj)}]}, Call),
            check_mailbox(ReqBox, Call, Loop);
        _ ->
            %% we dont want to alert the caller that the mailbox number doesnt match or people could use
            %% that to determine the mailboxs on this system then try brute force to guess the pwd.
            ?LOG("invalid mailbox ~s, faking user out...", [Mailbox]),
            b_play_and_collect_digits(<<"1">>, <<"6">>, EnterPwd, <<"1">>, <<"8000">>, Call),
            _ = b_play(Invalid, Call),
            check_mailbox(Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(compose_voicemail/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
compose_voicemail(#mailbox{check_if_owner=true, owner_id=OwnerId}=Box, #cf_call{owner_id=OwnerId}=Call)
  when is_binary(OwnerId), OwnerId =/= <<>> ->
    ?LOG("caller is the owner of this mailbox, overriding action as check (instead of compose)"),
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

    noop(Call),

    case wait_for_application_or_dtmf(<<"noop">>, 25000) of
        {ok, _} ->
            record_voicemail(tmp_file(), Box, Call);
        {dtmf, Digit} ->
            _ = flush(Call),
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
-spec(play_greeting/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
play_greeting(#mailbox{prompts=#prompts{person_at_exten=PersonAtExten, not_available=NotAvailable}
		       ,has_unavailable_greeting=false, mailbox_number=Mailbox}, Call) ->
    audio_macro([
                  {play, PersonAtExten}
                 ,{say,  Mailbox}
                 ,{play, NotAvailable}
                ], Call);
play_greeting(#mailbox{mailbox_id=Id, has_unavailable_greeting=true}, #cf_call{account_db=Db}=Call) ->
    play(<<$/, Db/binary, $/, Id/binary, $/, "unavailable_greeting.mp3">>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(record_voicemail/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
record_voicemail(MediaName, #mailbox{prompts=#prompts{tone_spec=ToneSpec, message_saved=Saved}}=Box, Call) ->
    tones(ToneSpec, Call),
    ?LOG("composing new voicemail"),
    case b_record(MediaName, Call) of
        {ok, _Msg} ->
            case review_recording(MediaName, Box, Call) of
                {ok, record} ->
                    record_voicemail(MediaName, Box, Call);
		{ok, save} ->
		    new_message(MediaName, Box, Call),
                    b_play(Saved, Call);
                {ok, no_selection} ->
		    new_message(MediaName, Box, Call),
                    b_play(Saved, Call)
            end;
        {error, channel_hungup} ->
            _ = cf_call_command:wait_for_message(<<"record">>, <<"RECORD_STOP">>, <<"call_event">>, false),
            new_message(MediaName, Box, Call);
	{error, execution_failure} ->
            ?LOG("media server exploded"),
	    ok %% something happened Whistle-side, nothing to do for now
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(main_menu/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
-spec(main_menu/3 :: (Box :: #mailbox{}, Call :: #cf_call{}, Loop :: non_neg_integer()) -> no_return()).
main_menu(#mailbox{is_setup=false}=Box, Call) ->
    main_menu(setup_mailbox(Box, Call), Call, 1);
main_menu(Box, Call) ->
    main_menu(Box, Call, 1).

main_menu(#mailbox{prompts=#prompts{goodbye=Goodbye}}, Call, Loop) when Loop > 4 ->
    %% If there have been too may loops with no action from the caller this
    %% is likely a abandonded channel, terminate
    ?LOG("entered main menu with too many invalid entries"),
    b_play(Goodbye, Call);
main_menu(#mailbox{prompts=#prompts{you_have=YouHave, new=New, messages=PromptMessages, saved=Saved, to_hear_new=ToHearNew
				    ,press=Press, to_hear_saved=ToHearSaved, to_configure=ToConfigure, to_exit=ToExit}
		   ,keys=#keys{hear_new=HearNew, hear_saved=HearSaved, configure=Configure, exit=Exit}}=Box, Call, Loop) ->
    ?LOG("playing mailbox main menu"),
    Messages = get_messages(Box, Call),
    audio_macro([
                  {play, YouHave}
                 ,{say,  whistle_util:to_binary(count_messages(Messages, ?FOLDER_NEW))}
                 ,{play, New}
                 ,{play, PromptMessages}

                 ,{play, YouHave}
                 ,{say,  whistle_util:to_binary(count_messages(Messages, ?FOLDER_SAVED))}
                 ,{play, Saved}
                 ,{play, PromptMessages}

                 ,{play, ToHearNew}
                 ,{play, Press}
                 ,{say,  HearNew}

                 ,{play, ToHearSaved}
                 ,{play, Press}
                 ,{say,  HearSaved}

                 ,{play, ToConfigure}
                 ,{play, Press}
                 ,{say,  Configure}

                 ,{play, ToExit}
                 ,{play, Press}
                 ,{say,  Exit}
                ], Call),
    DTMF = wait_for_dtmf(30000),
    _ = flush(Call),
    case DTMF of
	{ok, HearNew} ->
	    play_messages(get_folder(Messages, ?FOLDER_NEW), Box, Call),
	    main_menu(Box, Call);
	{ok, HearSaved} ->
	    play_messages(get_folder(Messages, ?FOLDER_SAVED), Box, Call),
	    main_menu(Box, Call);
	{ok, Configure} ->
	    config_menu(Box, Call);
	{ok, Exit} ->
	    ok;
	{error, _} ->
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
-spec(play_messages/3 :: (Messages :: json_objects(), Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
play_messages([{struct, _}=H|T]=Messages, #mailbox{timezone=Timezone
			      ,prompts=#prompts{received=Received, to_replay=ToReplay, press=Press, to_keep=ToKeep
						,to_delete=ToDelete, to_return_main=ToReturnMain
						,message_saved=MessageSaved, message_deleted=MessageDeleted}
			      ,keys=#keys{replay=Replay, keep=Keep, delete=Delete, return_main=ReturnMain}}=Box, Call) ->
    ?LOG("reviewing mailbox message"),
    Message = get_message(H, Box, Call),
    audio_macro([
                  {play, Received}
                 ,{say,  get_unix_epoch(wh_json:get_value(<<"timestamp">>, H), Timezone), <<"current_date_time">>}
                 ,{play, Message}

                 ,{play, ToReplay}
                 ,{play, Press}
                 ,{say,  Replay}

                 ,{play, ToKeep}
                 ,{play, Press}
                 ,{say,  Keep}

                 ,{play, ToDelete}
                 ,{play, Press}
                 ,{say,  Delete}

                 ,{play, ToReturnMain}
                 ,{play, Press}
                 ,{say,  ReturnMain}
                ], Call),
    DTMF = wait_for_dtmf(30000),
    _ = flush(Call),
    case DTMF of
	{ok, Keep} ->
	    play(MessageSaved, Call),
	    set_folder(?FOLDER_SAVED, H, Box, Call),
	    play_messages(T, Box, Call);
	{ok, Delete} ->
	    play(MessageDeleted, Call),
	    set_folder(?FOLDER_DELETED, H, Box, Call),
	    play_messages(T, Box, Call);
	{ok, ReturnMain} ->
	    play(MessageSaved, Call),
	    set_folder(?FOLDER_SAVED, H, Box, Call);
	{ok, Replay} ->
	    play_messages(Messages, Box, Call);
        {error, _} ->
            ok;
	_ ->
	    play(MessageSaved, Call),
	    set_folder(?FOLDER_SAVED, H, Box, Call),
	    play_messages(T, Box, Call)
    end;
play_messages(_, Box, Call) ->
    main_menu(Box, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(config_menu/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
-spec(config_menu/3 :: (Box :: #mailbox{}, Call :: #cf_call{}, Loop :: non_neg_integer()) -> no_return()).
config_menu(Box, Call) ->
    config_menu(Box, Call, 1).

config_menu(Box, Call, Loop) when Loop > 4 ->
    main_menu(Box, Call);
config_menu(#mailbox{prompts=#prompts{to_rec_unavailable=ToRecUnavailable, press=Press, to_rec_name=ToRecName
				     ,to_change_pin=ToChangePin, to_return_main=ToReturnMain}
		     ,keys=#keys{rec_unavailable=RecUnavailable, rec_name=RecName, set_pin=SetPin, return_main=ReturnMain}}=Box, Call, Loop) ->
    ?LOG("playing mailbox configuration menu"),
    audio_macro([
                 {play, ToRecUnavailable}
                 ,{play, Press}
                 ,{say,  RecUnavailable}

                 ,{play, ToRecName}
                 ,{play, Press}
                 ,{say,  RecName}

                 ,{play, ToChangePin}
                 ,{play, Press}
                 ,{say,  SetPin}

                 ,{play, ToReturnMain}
                 ,{play, Press}
                 ,{say,  ReturnMain}
                ], Call),
    DTMF = wait_for_dtmf(20000),
    _ = flush(Call),
    case DTMF of
	{ok, RecUnavailable} ->
	    record_unavailable_greeting(tmp_file(), Box, Call),
	    config_menu(Box, Call);
	{ok, RecName} ->
	    _ = record_name(tmp_file(), Box, Call),
	    config_menu(Box, Call);
	{ok, SetPin} ->
	    change_pin(Box, Call),
	    config_menu(Box, Call);
	{ok, ReturnMain} ->
	    main_menu(Box, Call);
	%% Bulk delete -> delete all voicemails
	%% Reset -> delete all voicemails, greetings, name, and reset pin
        {error, _} ->
            ok;
	_ ->
	    config_menu(Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(record_unavailable_greeting/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
record_unavailable_greeting(MediaName, #mailbox{prompts=#prompts{record_unavail_greeting=RecordUnavailGreeting, tone_spec=ToneSpec
                                                                 ,message_saved=Saved, message_deleted=Deleted}}=Box, Call) ->
    ?LOG("recoding unavailable greeting"),
    audio_macro([
                  {play,  RecordUnavailGreeting}
                 ,{tones, ToneSpec}
                ], Call),
    {ok, _} = b_record(MediaName, Call),
    case review_recording(MediaName, Box, Call) of
	{ok, record} ->
	    record_unavailable_greeting(MediaName, Box, Call);
	{ok, save} ->
	    store_recording(MediaName, ?UNAVAILABLE_GREETING, Box, Call),
            b_play(Saved, Call);
        {ok, no_selection} ->
            b_play(Deleted, Call),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(setup_mailbox/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> #mailbox{}).
setup_mailbox(#mailbox{prompts=#prompts{setup_intro=SetupIntro
                                        ,setup_rec_greet=SetupRecGreet
                                        ,setup_complete=SetupComplete}}=Box, Call) ->
    b_play(SetupIntro, Call),
    change_pin(Box, Call),
    b_play(SetupRecGreet, Call),
    record_unavailable_greeting(tmp_file(), Box, Call),
    mark_mailbox_setup(Box, Call),
    b_play(SetupComplete, Call),
    Box#mailbox{is_setup=true}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(record_name/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> tuple(ok, json_object())).
record_name(MediaName, #mailbox{prompts=#prompts{record_name=RecordName, tone_spec=ToneSpec
                                                 ,message_saved=Saved, message_deleted=Deleted}}=Box, Call) ->
    ?LOG("recording name"),
    audio_macro([
                  {play,  RecordName}
                 ,{tones, ToneSpec}
                ], Call),
    {ok, _} = b_record(MediaName, Call),
    case review_recording(MediaName, Box, Call) of
	{ok, record} ->
	    record_name(MediaName, Box, Call);
	{ok, save} ->
	    store_recording(MediaName, ?NAME_RECORDING, Box, Call),
            b_play(Saved, Call);
        {ok, no_selection} ->
            b_play(Deleted, Call),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(change_pin/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> stop | continue).
change_pin(#mailbox{prompts=#prompts{enter_new_pin=EnterNewPin, reenter_new_pin=ReenterNewPin
                                    ,new_pin_saved=SavedPin, new_pin_bad=BadPin}
		    ,mailbox_id=Id}=Box, #cf_call{account_db=Db}=Call) ->
    ?LOG("requesting new mailbox pin number"),
    try
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterNewPin, <<"1">>, <<"8000">>, Call),
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, ReenterNewPin, <<"1">>, <<"8000">>, Call),
        if byte_size(Pin) == 0 -> throw(pin_empty); true -> ok end,
        {ok, JObj} = couch_mgr:open_doc(Db, Id),
        {ok, _} = couch_mgr:save_doc(Db, wh_json:set_value(<<"pin">>, Pin, JObj)),
        b_play(SavedPin, Call),
        ?LOG("updated mailbox pin number")
    catch
        _:_ ->
            ?LOG("new pin was invalid, trying again"),
            b_play(BadPin, Call),
            change_pin(Box, Call)
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(new_message/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
new_message(MediaName, #mailbox{mailbox_id=Id}=Box, #cf_call{account_db=Db, call_id=CallID
                                                             ,from=From, from_user=FromU, from_realm=FromR
                                                             ,to=To, to_user=ToU, to_realm=ToR
							     ,cid_name=CIDName, cid_number=CIDNumber}=Call) ->
    {ok, _StoreJObj} = store_recording(MediaName, Box, Call), %% store was successful
    ?LOG("stored voicemail message ~s", [MediaName]),

    Tstamp = new_timestamp(),
    NewMessage={struct, [
                          {<<"timestamp">>, Tstamp}
                         ,{<<"from">>, From}
                         ,{<<"to">>, To}
                         ,{<<"caller_id_number">>, CIDNumber}
                         ,{<<"caller_id_name">>, CIDName}
                         ,{<<"call_id">>, CallID}
                         ,{<<"folder">>, ?FOLDER_NEW}
                         ,{<<"attachment">>, MediaName}
                        ]},
    {ok, _} = save_metadata(NewMessage, Db, Id),
    ?LOG("stored voicemail metadata for ~s", [MediaName]),

    {ok, JSON} = cf_api:new_voicemail([{<<"From-User">>, FromU}
				       ,{<<"From-Realm">>, FromR}
				       ,{<<"To-User">>, ToU}
				       ,{<<"To-Realm">>, ToR}
				       ,{<<"Account-DB">>, Db}
				       ,{<<"Voicemail-Box">>, Id}
				       ,{<<"Voicemail-Name">>, MediaName}
				       ,{<<"Caller-ID-Name">>, CIDName}
				       ,{<<"Caller-ID-Number">>, CIDNumber}
				       ,{<<"Voicemail-Timestamp">>, Tstamp}
				       | whistle_api:default_headers(<<>>, <<"notification">>, <<"new_voicemail">>, ?APP_NAME, ?APP_VERSION)
				      ]),
    ?LOG("new voicemail message ~s whistle API broadcast", [MediaName]),
    amqp_util:callevt_publish(CallID, JSON, ?NOTIFY_VOICEMAIL_NEW).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(save_metadata/3 :: (NewMessage :: json_object(), Db :: binary(), Id :: binary()) -> no_return()).
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
-spec(get_mailbox_profile/2 :: (Data :: json_object(), Call :: #cf_call{}) -> #mailbox{}).
get_mailbox_profile(Data, #cf_call{account_db=Db, request_user=ReqUser, last_action=LastAct}) ->
    Id = wh_json:get_value(<<"id">>, Data),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            ?LOG("loaded voicemail box ~s", [Id]),
            Default = #mailbox{},
            %% dont check if the voicemail box belongs to the owner (by default) if the call was not
            %% specificly to him, IE: calling a ring group and going to voicemail should not check
            CheckIfOwner = ((undefined =:= LastAct) orelse (cf_device =:= LastAct)),
            #mailbox{mailbox_id = Id
                     ,skip_instructions =
                         whistle_util:is_true(wh_json:get_value(<<"skip_instructions">>, JObj, Default#mailbox.skip_instructions))
                     ,skip_greeting =
                         whistle_util:is_true(wh_json:get_value(<<"skip_greeting">>, JObj, Default#mailbox.skip_greeting))
                     ,has_unavailable_greeting =
                         wh_json:get_value([<<"_attachments">>, ?UNAVAILABLE_GREETING], JObj) =/= undefined
                     ,pin =
                         whistle_util:to_binary(wh_json:get_value(<<"pin">>, JObj, <<>>))
                     ,timezone =
                         wh_json:get_value(<<"timezone">>, JObj, Default#mailbox.timezone)
                     ,mailbox_number =
                         whistle_util:to_binary(wh_json:get_value(<<"mailbox">>, JObj, ReqUser))
                     ,require_pin =
                         whistle_util:is_true(wh_json:get_value(<<"require_pin">>, JObj, false))
                     ,check_if_owner =
                         whistle_util:is_true(wh_json:get_value(<<"check_if_owner">>, JObj, CheckIfOwner))
                     ,owner_id =
                         wh_json:get_value(<<"owner_id">>, JObj)
                     ,is_setup =
                         whistle_util:is_true(wh_json:get_value(<<"is_setup">>, JObj, false))
                     ,exists = true
                    };
        {error, R} ->
            ?LOG("failed to load voicemail box ~s, ~w", [Id, R]),
            #mailbox{}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(mark_mailbox_setup/2 :: (Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
mark_mailbox_setup(#mailbox{mailbox_id=Id}=Box, #cf_call{account_db=Db}=Call) ->
    {ok, JObj} = couch_mgr:open_doc(Db, Id),
    case couch_mgr:save_doc(Db, wh_json:set_value(<<"is_setup">>, true, JObj)) of
        {ok, _} -> ok;
        {error, conflict} -> mark_mailbox_setup(Box, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(review_recording/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> tuple(ok, record | save | no_selection)).
-spec(review_recording/4 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}, Loop :: non_neg_integer()) -> tuple(ok, record | save | no_selection)).
review_recording(MediaName, Box, Call) ->
    review_recording(MediaName, Box, Call, 1).

review_recording(_, _, _, Loop) when Loop > 4 ->
    {ok, no_selection};
review_recording(MediaName, #mailbox{prompts=#prompts{press=Press, to_listen=ToListen, to_save=ToSave, to_rerecord=ToRerecord}
				     ,keys=#keys{listen=Listen, save=Save, record=Record}}=Box, Call, Loop) ->
    ?LOG("playing review options"),
    audio_macro([
                  {play, Press}
                 ,{say,  Listen}
                 ,{play, ToListen}

                 ,{play, Press}
                 ,{say,  Save}
                 ,{play, ToSave}

                 ,{play, Press}
                 ,{say,  Record}
                 ,{play, ToRerecord}
                ], Call),
    DTMF = wait_for_dtmf(18000),
    _ = flush(Call),
    case DTMF of
        {ok, Listen} ->
            _ = b_play(MediaName, Call),
            review_recording(MediaName, Box, Call);
        {ok, Record} ->
            {ok, record};
        {ok, Save} ->
            {ok, save};
	{error, _} ->
            ?LOG("channel hungup while waiting for dtmf"),
	    {ok, no_selection};
        _ ->
	    review_recording(MediaName, Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(store_recording/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> tuple(ok, json_object()) | tuple(error, execution_failure)).
-spec(store_recording/4 :: (MediaName :: binary(), DestName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> tuple(ok, json_object()) | tuple(error, execution_failure)).
store_recording(MediaName, Box, Call) ->
    store_recording(MediaName, MediaName, Box, Call).
store_recording(MediaName, DestName, Box, Call) ->
    ?LOG("storing media ~s", [DestName]),
    store(MediaName, get_attachment_path(DestName, Box, Call), Call),
    cf_call_command:wait_for_store(Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_attachment_path/3 :: (MediaName :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> binary()).
get_attachment_path(MediaName, #mailbox{mailbox_id=Id}, #cf_call{account_db=Db}) ->
    <<(couch_mgr:get_url())/binary
      ,Db/binary
      ,$/, Id/binary
      ,$/, MediaName/binary
      ,"?rev=", (lookup_doc_rev(Db, Id))/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(lookup_doc_rev/2 :: (Db :: binary(), Id :: binary()) -> binary()).
lookup_doc_rev(Db, Id) ->
    {ok, Rev} = couch_mgr:lookup_doc_rev(Db, Id),
    Rev.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_messages/2 :: (Mailbox :: #mailbox{}, Call :: #cf_call{}) -> json_objects()).
get_messages(#mailbox{mailbox_id=Id}, #cf_call{account_db=Db}) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            wh_json:get_value(<<"messages">>, JObj, []);
        _ ->
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_message/3 :: (Message :: json_object(), Mailbox :: #mailbox{}, Call :: #cf_call{}) -> binary()).
get_message(Message, #mailbox{mailbox_id=Id}, #cf_call{account_db=Db}) ->
    <<$/, Db/binary, $/, Id/binary, $/, (wh_json:get_value(<<"attachment">>, Message))/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(count_messages/2 :: (Message :: json_objects(), Folder :: binary()) -> integer()).
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
-spec(get_folder/2 :: (Messages :: json_objects(), Folder :: binary()) -> json_objects()).
get_folder(Messages, Folder) ->
    lists:foldr(fun(Message, Acc) ->
                       case wh_json:get_value(<<"folder">>, Message) of
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
-spec(set_folder/4 :: (Folder :: binary(), Message :: json_object(), Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
set_folder(Folder, Message, Box, Call) ->
    ?LOG("setting folder for message to ~s", [Folder]),
    not (wh_json:get_value(<<"folder">>, Message) =:= Folder) andalso
	update_folder(Folder, wh_json:get_value(<<"attachment">>, Message), Box, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(update_folder/4 :: (Folder :: binary(), Attachment :: binary(), Box :: #mailbox{}, Call :: #cf_call{}) -> no_return()).
update_folder(_, undefined, _, _) ->
    {error, attachment_undefined};
update_folder(Folder, Attachment, #mailbox{mailbox_id=Id}=Mailbox, #cf_call{account_db=Db}=Call) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            Messages = [ update_folder1(Message, Folder, Attachment, wh_json:get_value(<<"attachment">>, Message))
			 || Message <- wh_json:get_value(<<"messages">>, JObj, []) ],
            case couch_mgr:save_doc(Db, wh_json:set_value(<<"messages">>, Messages, JObj)) of
                {error, conflict} -> update_folder(Folder, Attachment, Mailbox, Call);
                {ok, _}=OK -> OK;
                {error, R}=E ->
                    ?LOG("error while updating folder ~s ~s", [Folder, R]),
                    E
            end;
        {error, _}=E ->
            E
    end.

update_folder1(Message, Folder, Attachment, Attachment) ->
    wh_json:set_value(<<"folder">>, Folder, Message);
update_folder1(Message, _, _, _) ->
    Message.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(tmp_file/0 :: () -> binary()).
tmp_file() ->
     <<(list_to_binary(whistle_util:to_hex(crypto:rand_bytes(16))))/binary, ".mp3">>.

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
    whistle_util:to_binary(whistle_util:current_tstamp()).

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
