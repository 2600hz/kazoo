%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
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
-type vm_keys() :: #keys{}.

-record(mailbox, {
          mailbox_id :: api_binary()
          ,mailbox_number = <<>> :: binary()
          ,exists = false :: boolean()
          ,skip_instructions = 'false' :: boolean()
          ,skip_greeting = 'false' :: boolean()
          ,unavailable_media_id :: api_binary()
          ,name_media_id :: api_binary()
          ,pin = <<>> :: binary()
          ,timezone = <<"America/Los_Angeles">> :: binary()
          ,max_login_attempts = 3 :: non_neg_integer()
          ,require_pin = 'false' :: boolean()
          ,check_if_owner = 'true' :: boolean()
          ,owner_id :: api_binary()
          ,is_setup = 'false' :: boolean()
          ,message_count = 0 :: non_neg_integer()
          ,max_message_count = 0 :: non_neg_integer()
          ,max_message_length :: pos_integer()
          ,keys = #keys{} :: vm_keys()
          ,transcribe_voicemail = 'false' :: boolean()
          ,notifications :: wh_json:object()
          ,delete_after_notify = 'false' :: boolean()
         }).
-type mailbox() :: #mailbox{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, based on the payload will either
%% connect a caller to check_voicemail or compose_voicemail.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case wh_json:get_value(<<"action">>, Data, <<"compose">>) of
        <<"compose">> ->
            whapps_call_command:answer(Call),
            case compose_voicemail(get_mailbox_profile(Data, Call), Call) of
                ok ->
                    lager:debug("compose voicemail complete"),
                    cf_exe:continue(Call);
                {branch, Flow} ->
                    lager:debug("compose voicemail complete, branch to operator"),
                    cf_exe:branch(Flow, Call)
            end;
        <<"check">> ->
            whapps_call_command:answer(Call),
            check_mailbox(get_mailbox_profile(Data, Call), Call),
            lager:debug("check voicemail complete"),
            cf_exe:continue(Call);
        _ ->
            cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_mailbox/2 :: (mailbox(), whapps_call:call()) -> 'ok'.
-spec check_mailbox/3 :: (mailbox(), whapps_call:call(), non_neg_integer()) -> 'ok'.
-spec check_mailbox/4 :: (mailbox(), boolean(), whapps_call:call(), non_neg_integer()) -> 'ok'.

check_mailbox(Box, Call) ->
    %% Wrapper to initalize the attempt counter
    check_mailbox(Box, Call, 1),
    cf_util:update_mwi(Call).

check_mailbox(#mailbox{owner_id=OwnerId}=Box, Call, Loop) ->
    IsOwner = case whapps_call:kvs_fetch(owner_id, Call) of
                  <<>> -> false;
                  undefined -> false;
                  OwnerId -> true;
                  _Else -> false
              end,
    check_mailbox(Box, IsOwner, Call, Loop).

check_mailbox(#mailbox{max_login_attempts=MaxLoginAttempts}, _, Call, Loop) when Loop > MaxLoginAttempts ->
    %% if we have exceeded the maximum loop attempts then terminate this call
    lager:debug("maximum number of invalid attempts to check mailbox"),
    _ = whapps_call_command:b_prompt(<<"vm-abort">>, Call),
    ok;
check_mailbox(#mailbox{exists=false}=Box, _ , Call, Loop) ->
    %% if the callflow did not define the mailbox to check then request the mailbox ID from the user
    find_mailbox(Box, Call, Loop);
check_mailbox(#mailbox{require_pin=false}=Box, true, Call, _) ->
    %% If this is the owner of the mailbox calling in and it doesn't require a pin then jump
    %% right to the main menu
    lager:debug("caller is the owner of this mailbox, and requires no pin"),
    main_menu(Box, Call);
check_mailbox(#mailbox{pin = <<>>}=Box, true, Call, _) ->
    %% If this is the owner of the mailbox calling in and it doesn't require a pin then jump
    %% right to the main menu
    lager:debug("caller is the owner of this mailbox, and it has no pin"),
    main_menu(Box, Call);
check_mailbox(#mailbox{pin = <<>>, exists=true}, false, Call, _) ->
    %% If the caller is not the owner or the mailbox requires a pin to access it but has none set
    %% then terminate this call.
    lager:debug("attempted to sign into a mailbox with no pin"),
    _ = whapps_call_command:b_prompt(<<"vm-no_access">>, Call),
    ok;
check_mailbox(#mailbox{pin=Pin}=Box, IsOwner, Call, Loop) ->
    lager:debug("requesting pin number to check mailbox"),
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"vm-enter_pass">>, <<"1">>, Call) of
        {ok, Pin} ->
            lager:debug("caller entered a valid pin"),
            main_menu(Box, Call);
        {ok, _} ->
            lager:debug("invalid mailbox login"),
            _ = whapps_call_command:b_prompt(<<"vm-fail_auth">>, Call),
            check_mailbox(Box, IsOwner, Call, Loop + 1);
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
-spec find_mailbox/3 :: (mailbox(), whapps_call:call(), non_neg_integer()) -> 'ok'.

find_mailbox(#mailbox{max_login_attempts=MaxLoginAttempts}, Call, Loop) when Loop > MaxLoginAttempts ->
    %% if we have exceeded the maximum loop attempts then terminate this call
    lager:debug("maximum number of invalid attempts to find mailbox"),
    _ = whapps_call_command:b_prompt(<<"vm-abort">>, Call),
    ok;
find_mailbox(Box, Call, Loop) ->
    lager:debug("requesting mailbox number to check"),
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"vm-enter_id">>, <<"1">>, Call) of
        {ok, <<>>} ->
            find_mailbox(Box, Call, Loop + 1);
        {ok, Mailbox} ->
            BoxNum = try wh_util:to_integer(Mailbox) catch _:_ -> 0 end,
            %% find the voicemail box, by making a fake 'callflow data payload' we look for it now because if the
            %% caller is the owner, and the pin is not required then we skip requesting the pin
            ViewOptions = [{key, BoxNum}],
            AccountDb = whapps_call:account_db(Call),
            case couch_mgr:get_results(AccountDb, <<"vmboxes/listing_by_mailbox">>, ViewOptions) of
                {ok, []} ->
                    lager:debug("mailbox ~s doesnt exist", [Mailbox]),
                    find_mailbox(Box, Call, Loop + 1);
                {ok, [JObj]} ->
                    lager:debug("get profile of ~p", [JObj]),
                    ReqBox = get_mailbox_profile(wh_json:from_list([{<<"id">>, wh_json:get_value(<<"id">>, JObj)}]), Call),
                    check_mailbox(ReqBox, Call, Loop);
                {ok, _} ->
                    lager:debug("mailbox ~s is ambiguous", [Mailbox]),
                    find_mailbox(Box, Call, Loop + 1);
                _E ->
                    lager:debug("failed to find mailbox ~s: ~p", [Mailbox, _E]),
                    find_mailbox(Box, Call, Loop + 1)
            end;
        _E ->
            lager:debug("recv other: ~p", [_E]),
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_voicemail/2 :: (mailbox(), whapps_call:call()) ->
                                     'ok' | {'branch', _}.
-spec compose_voicemail/3 :: (mailbox(), boolean(), whapps_call:call()) ->
                                     'ok' | {'branch', _}.
compose_voicemail(#mailbox{owner_id=OwnerId}=Box, Call) ->
    IsOwner = case whapps_call:kvs_fetch(owner_id, Call) of
                  <<>> -> false;
                  undefined -> false;
                  OwnerId -> true;
                  _Else -> false
              end,
    compose_voicemail(Box, IsOwner, Call).

compose_voicemail(#mailbox{check_if_owner=true}=Box, true, Call) ->
    lager:debug("caller is the owner of this mailbox"),
    lager:debug("overriding action as check (instead of compose)"),
    check_mailbox(Box, Call);
compose_voicemail(#mailbox{exists=false}, _, Call) ->
    lager:debug("attempted to compose voicemail for missing mailbox"),
    _ = whapps_call_command:b_prompt(<<"vm-not_available_no_voicemail">>, Call),
    ok;
compose_voicemail(#mailbox{max_message_count=Count
                           ,message_count=Count
                          }, _, Call) when Count > 0 ->
    lager:debug("voicemail box is full, cannot hold more messages"),
    _ = whapps_call_command:b_prompt(<<"vm-mailbox_full">>, Call),
    ok;
compose_voicemail(#mailbox{keys=#keys{login=Login
                                      ,operator=Operator
                                     }
                          }=Box, _, Call) ->
    lager:debug("playing mailbox greeting to caller"),
    _ = play_greeting(Box, Call),
    _ = play_instructions(Box, Call),
    _NoopId = whapps_call_command:noop(Call),
    %% timeout after 5 min for saftey, so this process cant hang around forever
    case whapps_call_command:wait_for_application_or_dtmf(<<"noop">>, 300000) of
        {ok, _} ->
            lager:debug("played greeting and instructions to caller, recording new message"),
            record_voicemail(tmp_file(), Box, Call);
        {dtmf, Digit} ->
            _ = whapps_call_command:b_flush(Call),
            case Digit of
                Login ->
                    lager:debug("caller pressed '~s', redirecting to check voicemail", [Login]),
                    check_mailbox(Box, Call);
                Operator ->
                    lager:debug("caller choose to ring the operator"),
                    case cf_util:get_operator_callflow(whapps_call:account_id(Call)) of
                        {ok, Flow} -> {branch, Flow};
                        {error, _R} -> record_voicemail(tmp_file(), Box, Call)
                    end;
                _Else ->
                    lager:debug("caller pressed unbound '~s', skip to recording new message", [_Else]),
                    record_voicemail(tmp_file(), Box, Call)
            end;
        {error, R} ->
            lager:debug("error while playing voicemail greeting: ~p", [R]),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_greeting/2 :: (mailbox(), whapps_call:call()) -> ne_binary() | 'ok'.
play_greeting(#mailbox{skip_greeting=true}, _) -> ok;
play_greeting(#mailbox{unavailable_media_id=undefined
                       ,mailbox_number=Mailbox
                      }, Call) ->
    lager:debug("mailbox has no greeting, playing the generic"),
    whapps_call_command:audio_macro([{prompt, <<"vm-person">>}
                                     ,{say, Mailbox}
                                     ,{prompt, <<"vm-not_available">>}
                                    ], Call);
play_greeting(#mailbox{unavailable_media_id = <<"local_stream://", _/binary>> = Id}, Call) ->
    lager:debug("mailbox has a greeting file on the softswitch: ~s", Id),
    whapps_call_command:play(Id, Call);
play_greeting(#mailbox{unavailable_media_id=Id}, Call) ->
    lager:debug("streaming mailbox greeting"),
    whapps_call_command:play(<<$/, (whapps_call:account_db(Call))/binary, $/, Id/binary>>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_instructions/2 :: (mailbox(), whapps_call:call()) -> ne_binary() | 'ok'.
play_instructions(#mailbox{skip_instructions=true}, _) -> ok;
play_instructions(#mailbox{skip_instructions=false}, Call) ->
    whapps_call_command:prompt(<<"vm-record_message">>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_voicemail/3 :: (ne_binary(), mailbox(), whapps_call:call()) -> 'ok'.
record_voicemail(AttachmentName, #mailbox{max_message_length=MaxMessageLength}=Box, Call) ->
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    whapps_call_command:tones([Tone], Call),
    lager:debug("composing new voicemail"),
    case whapps_call_command:b_record(AttachmentName, ?ANY_DIGIT, wh_util:to_binary(MaxMessageLength), Call) of
        {ok, Msg} ->
            Length = wh_json:get_integer_value(<<"Length">>, Msg, 0),
            case review_recording(AttachmentName, true, Box, Call) of
                {ok, record} ->
                    record_voicemail(tmp_file(), Box, Call);
                {ok, _Selection} ->
                    _ = new_message(AttachmentName, Length, Box, Call),
                    _ = whapps_call_command:prompt(<<"vm-saved">>, Call),
                    _ = whapps_call_command:prompt(<<"vm-thank_you">>, Call),
                    _ = timer:sleep(8000),
                    cf_exe:continue(Call);
                {branch, Flow} ->
                    _ = new_message(AttachmentName, Length, Box, Call),
                    _ = whapps_call_command:prompt(<<"vm-saved">>, Call),
                    _ = timer:sleep(8000),
                    cf_exe:branch(Flow, Call)
            end;
        {error, _R} ->
            lager:debug("error while attempting to record a new message: ~p", [_R]),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec setup_mailbox/2 :: (mailbox(), whapps_call:call()) -> mailbox().
setup_mailbox(Box, Call) ->
    lager:debug("starting voicemail configuration wizard"),
    {ok, _} = whapps_call_command:b_prompt(<<"vm-setup_intro">>, Call),

    lager:debug("prompting caller to set a pin"),
    _ = change_pin(Box, Call),

    {ok, _} = whapps_call_command:b_prompt(<<"vm-setup_rec_greeting">>, Call),
    lager:debug("prompting caller to record an unavailable greeting"),

    Box1 = record_unavailable_greeting(tmp_file(), Box, Call),
    ok = update_doc(<<"is_setup">>, true, Box1, Call),
    lager:debug("voicemail configuration wizard is complete"),

    {ok, _} = whapps_call_command:b_prompt(<<"vm-setup_complete">>, Call),
    Box1#mailbox{is_setup=true}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec main_menu/2 :: (mailbox(), whapps_call:call()) -> 'ok'.
-spec main_menu/3 :: (mailbox(), whapps_call:call(), non_neg_integer()) -> 'ok'.
main_menu(#mailbox{is_setup=false}=Box, Call) ->
    main_menu(setup_mailbox(Box, Call), Call, 1);
main_menu(Box, Call) -> main_menu(Box, Call, 1).

main_menu(_, Call, Loop) when Loop > 4 ->
    %% If there have been too may loops with no action from the caller this
    %% is likely a abandonded channel, terminate
    lager:debug("entered main menu with too many invalid entries"),
    _ = whapps_call_command:b_prompt(<<"vm-goodbye">>, Call),
    ok;
main_menu(#mailbox{owner_id=OwnerId
                   ,keys=#keys{hear_new=HearNew
                               ,hear_saved=HearSaved
                               ,configure=Configure
                               ,exit=Exit
                              }
                  }=Box, Call, Loop) ->
    lager:debug("playing mailbox main menu"),
    _ = whapps_call_command:b_flush(Call),
    AccountDb = whapps_call:account_db(Call),

    Messages = get_messages(Box, Call),
    New = count_messages(Messages, ?FOLDER_NEW),
    Saved = count_messages(Messages, ?FOLDER_SAVED),

    lager:debug("mailbox has ~p new and ~p saved messages", [New, Saved]),
    NoopId = whapps_call_command:audio_macro(message_count_prompts(New, Saved)
                                             ++ [{prompt, <<"vm-main_menu">>}]
                                             ,Call),
    case whapps_call_command:collect_digits(1, 5000, 2000, NoopId, Call) of
        {error, _} ->
            lager:debug("error during mailbox main menu"),
            cf_util:update_mwi(OwnerId, AccountDb),
            ok;
        {ok, Exit} ->
            lager:debug("user choose to exit voicemail menu"),
            cf_util:update_mwi(OwnerId, AccountDb),
            ok;
        {ok, HearNew} ->
            lager:debug("playing all messages in folder: ~s", [?FOLDER_NEW]),
            Folder = get_folder(Messages, ?FOLDER_NEW),
            case play_messages(Folder, New, Box, Call) of
                ok -> ok;
                _Else -> main_menu(Box, Call)
            end;
        {ok, HearSaved} ->
            lager:debug("playing all messages in folder: ~s", [?FOLDER_SAVED]),
            Folder = get_folder(Messages, ?FOLDER_SAVED),
            case play_messages(Folder, Saved, Box, Call) of
                ok -> ok;
                _Else ->  main_menu(Box, Call)
            end;
        {ok, Configure} ->
            lager:debug("caller choose to change their mailbox configuration"),
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
-spec message_count_prompts/2 :: (integer(), integer()) -> wh_proplist().
message_count_prompts(0, 0) ->
    [{prompt, <<"vm-no_messages">>}];
message_count_prompts(1, 0) ->
    [{prompt, <<"vm-you_have">>}
     ,{say, <<"1">>}
     ,{prompt, <<"vm-new_message">>}
    ];
message_count_prompts(0, 1) ->
    [{prompt, <<"vm-you_have">>}
     ,{say, <<"1">>}
     ,{prompt, <<"vm-saved_message">>}
    ];
message_count_prompts(1, 1) ->
    [{prompt, <<"vm-you_have">>}
     ,{say, <<"1">>}
     ,{prompt, <<"vm-new_and">>}
     ,{say, <<"1">>}
     ,{prompt, <<"vm-saved_message">>}
    ];
message_count_prompts(New, 0) ->
    [{prompt, <<"vm-you_have">>}
     ,{say, wh_util:to_binary(New), <<"number">>}
     ,{prompt, <<"vm-new_messages">>}
    ];
message_count_prompts(New, 1) ->
    [{prompt, <<"vm-you_have">>}
     ,{say, wh_util:to_binary(New), <<"number">>}
     ,{prompt, <<"vm-new_and">>}
     ,{say, <<"1">>}
     ,{prompt, <<"vm-saved_message">>}
    ];
message_count_prompts(0, Saved) ->
    [{prompt, <<"vm-you_have">>}
     ,{say, wh_util:to_binary(Saved), <<"number">>}
     ,{prompt, <<"vm-saved_messages">>}
    ];
message_count_prompts(1, Saved) ->
    [{prompt, <<"vm-you_have">>}
     ,{say, <<"1">>}
     ,{prompt, <<"vm-new_and">>}
     ,{say, wh_util:to_binary(Saved), <<"number">>}
     ,{prompt, <<"vm-saved_messages">>}
    ];
message_count_prompts(New, Saved) ->
    [{prompt, <<"vm-you_have">>}
     ,{say, wh_util:to_binary(New), <<"number">>}
     ,{prompt, <<"vm-new_and">>}
     ,{say, wh_util:to_binary(Saved), <<"number">>}
     ,{prompt, <<"vm-saved_messages">>}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays back a message then the menu, and continues to loop over the
%% menu utill
%% @end
%%--------------------------------------------------------------------
-spec play_messages/4 :: (wh_json:objects(), non_neg_integer(), mailbox(), whapps_call:call()) ->
                                 'ok' | 'complete'.
play_messages([H|T]=Messages, Count, #mailbox{timezone=Timezone}=Box, Call) ->
    Message = get_message(H, Call),
    lager:debug("playing mailbox message ~p (~s)", [Count, Message]),
    Prompt = [{prompt, <<"vm-message_number">>}
              ,{say, wh_util:to_binary(Count - length(Messages) + 1), <<"number">>}
              ,{play, Message}
              ,{prompt, <<"vm-received">>}
              ,{say,  get_unix_epoch(wh_json:get_value(<<"timestamp">>, H), Timezone), <<"current_date_time">>}
              ,{prompt, <<"vm-message_menu">>}
             ],
    case message_menu(Prompt, Box, Call) of
        {ok, keep} ->
            lager:debug("caller choose to save the message"),
            _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
            set_folder(?FOLDER_SAVED, H, Box, Call),
            play_messages(T, Count, Box, Call);
        {ok, delete} ->
            lager:debug("caller choose to delete the message"),
            _ = whapps_call_command:b_prompt(<<"vm-deleted">>, Call),
            set_folder(?FOLDER_DELETED, H, Box, Call),
            play_messages(T, Count, Box, Call);
        {ok, return} ->
            lager:debug("caller choose to return to the main menu"),
            _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
            set_folder(?FOLDER_SAVED, H, Box, Call),
            complete;
        {ok, replay} ->
            lager:debug("caller choose to replay"),
            play_messages(Messages, Count, Box, Call);
        {error, _} ->
            lager:debug("error during message playback"),
            ok
    end;
play_messages([], _, _, _) ->
    lager:debug("all messages in folder played to caller"),
    complete.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loops over the message menu after the first play back util the
%% user provides a valid option
%% @end
%%--------------------------------------------------------------------
-type message_menu_returns() :: {'ok', 'keep' | 'delete' | 'return' | 'replay'}.

-spec message_menu/2 :: (mailbox(), whapps_call:call()) ->
                                {'error', 'channel_hungup' | 'channel_unbridge' | wh_json:object()} |
                                message_menu_returns().
-spec message_menu/3 :: ([whapps_call_command:audio_macro_prompt(),...], mailbox(), whapps_call:call()) ->
                                {'error', 'channel_hungup' | 'channel_unbridge' | wh_json:object()} |
                                message_menu_returns().
message_menu(Box, Call) ->
    message_menu([{prompt, <<"vm-message_menu">>}], Box, Call).
message_menu(Prompt, #mailbox{keys=#keys{replay=Replay, keep=Keep,
                                         delete=Delete, return_main=ReturnMain}}=Box, Call) ->
    lager:debug("playing message menu"),
    NoopId = whapps_call_command:audio_macro(Prompt, Call),
    case whapps_call_command:collect_digits(1, 5000, 2000, NoopId, Call)of
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
-spec config_menu/2 :: (mailbox(), whapps_call:call()) -> 'ok' | mailbox().
-spec config_menu/3 :: (mailbox(), whapps_call:call(), pos_integer()) -> 'ok' | mailbox().

config_menu(Box, Call) ->
    config_menu(Box, Call, 1).

config_menu(#mailbox{keys=#keys{rec_unavailable=RecUnavailable, rec_name=RecName
                                ,set_pin=SetPin, return_main=ReturnMain}}=Box, Call, Loop) when Loop < 4 ->
    lager:debug("playing mailbox configuration menu"),
    {ok, _} = whapps_call_command:b_flush(Call),
    case whapps_call_command:b_prompt_and_collect_digit(<<"vm-settings_menu">>, Call) of
        {ok, RecUnavailable} ->
            lager:debug("caller choose to record their unavailable greeting"),
            case record_unavailable_greeting(tmp_file(), Box, Call) of
                ok -> ok;
                Else -> config_menu(Else, Call)
            end;
        {ok, RecName} ->
            lager:debug("caller choose to record their name"),
            case record_name(tmp_file(), Box, Call) of
                ok -> ok;
                Else -> config_menu(Else, Call)
            end;
        {ok, SetPin} ->
            lager:debug("caller choose to change their pin"),
            case change_pin(Box, Call) of
                ok -> ok;
                _Else -> config_menu(Box, Call)
            end;
        {ok, ReturnMain} ->
            lager:debug("caller choose to return to the main menu"),
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
-spec record_unavailable_greeting/3 :: (ne_binary(), mailbox(), whapps_call:call()) -> 'ok' | mailbox().
record_unavailable_greeting(AttachmentName, #mailbox{unavailable_media_id=undefined}=Box, Call) ->
    MediaId = recording_media_doc(<<"unavailable greeting">>, Box, Call),
    record_unavailable_greeting(AttachmentName, Box#mailbox{unavailable_media_id=MediaId}, Call);
record_unavailable_greeting(AttachmentName, #mailbox{unavailable_media_id=MediaId}=Box, Call) ->
    lager:debug("recording unavailable greeting  as ~s", [AttachmentName]),
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    _NoopId = whapps_call_command:audio_macro([{prompt, <<"vm-record_greeting">>}
                                               ,{tones, [Tone]}
                                              ], Call),
    _ = whapps_call_command:b_record(AttachmentName, Call),
    case review_recording(AttachmentName, false, Box, Call) of
        {ok, record} ->
            record_unavailable_greeting(tmp_file(), Box, Call);
        {ok, save} ->
            _ = store_recording(AttachmentName, MediaId, Call),
            ok = update_doc([<<"media">>, <<"unavailable">>], MediaId, Box, Call),
            _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
            Box;
        {ok, no_selection} ->
            _ = whapps_call_command:b_prompt(<<"vm-deleted">>, Call),
            ok;
        {branch, _}=B -> B
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_name/3 :: (ne_binary(), mailbox(), whapps_call:call()) -> 'ok' | mailbox().
-spec record_name/4 :: (ne_binary(), mailbox(), whapps_call:call(), ne_binary()) -> 'ok' | mailbox().
record_name(AttachmentName, #mailbox{owner_id=undefined
                                     ,name_media_id=undefined
                                    }=Box, Call) ->
    lager:debug("no recorded name media id nor owner id"),
    MediaId = recording_media_doc(<<"users name">>, Box, Call),
    lager:debug("created recorded name media doc: ~s", [MediaId]),
    record_name(AttachmentName, Box#mailbox{name_media_id=MediaId}, Call);
record_name(AttachmentName, #mailbox{owner_id=undefined
                                     ,mailbox_id=BoxId
                                    }=Box, Call) ->
    lager:debug("no owner_id set on mailbox, saving recorded name id into mailbox"),
    record_name(AttachmentName, Box, Call, BoxId);
record_name(AttachmentName, #mailbox{owner_id=OwnerId
                                     ,name_media_id=undefined
                                    }=Box, Call) ->
    lager:debug("no recorded name media id for owner"),
    MediaId = recording_media_doc(<<"users name">>, Box, Call),
    lager:debug("created recorded name media doc: ~s", [MediaId]),
    record_name(AttachmentName, Box#mailbox{name_media_id=MediaId}, Call, OwnerId);
record_name(AttachmentName, #mailbox{owner_id=OwnerId}=Box, Call) ->
    lager:debug("owner_id (~s) set on mailbox, saving into owner's doc", [OwnerId]),
    record_name(AttachmentName, Box, Call, OwnerId).

record_name(AttachmentName, #mailbox{name_media_id=MediaId}=Box, Call, DocId) ->
    lager:debug("recording name as ~s in ~s", [AttachmentName, MediaId]),
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    _NoopId = whapps_call_command:audio_macro([{prompt,  <<"vm-record_name">>}
                                               ,{tones, [Tone]}
                                              ], Call),
    _ = whapps_call_command:b_record(AttachmentName, Call),
    case review_recording(AttachmentName, false, Box, Call) of
        {ok, record} ->
            record_name(tmp_file(), Box, Call);
        {ok, save} ->
            _ = store_recording(AttachmentName, MediaId, Call),
            ok = update_doc(?RECORDED_NAME_KEY, MediaId, DocId, Call),
            _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
            Box;
        {ok, no_selection} ->
            _ = whapps_call_command:b_prompt(<<"vm-deleted">>, Call),
            ok;
        {branch, _}=B -> B
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec change_pin/2 :: (mailbox(), whapps_call:call()) -> 'ok' | mailbox().
change_pin(#mailbox{mailbox_id=Id}=Box, Call) ->
    lager:debug("requesting new mailbox pin number"),
    try
        {ok, Pin} = whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"vm-enter_new_pin">>, <<"1">>, Call),
        lager:debug("collected first pin"),
        {ok, Pin} = whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"vm-enter_new_pin_confirm">>, <<"1">>, Call),
        lager:debug("collected second pin"),
        if byte_size(Pin) == 0 -> throw(pin_empty); true -> ok end,
        lager:debug("entered pin is not empty"),
        AccountDb = whapps_call:account_db(Call),
        {ok, JObj} = couch_mgr:open_doc(AccountDb, Id),
        {ok, _} = couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pin">>, Pin, JObj)),
        {ok, _} = whapps_call_command:b_prompt(<<"vm-pin_set">>, Call),
        lager:debug("updated mailbox pin number"),
        Box
    catch
        _:_ ->
            lager:debug("new pin was invalid, trying again"),
            case whapps_call_command:b_prompt(<<"vm-pin_invalid">>, Call) of
                {ok, _} -> change_pin(Box, Call);
                _ -> ok
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new_message/4 :: (ne_binary(), pos_integer(), mailbox(), whapps_call:call()) -> any().
new_message(AttachmentName, Length, Box, Call) ->
    lager:debug("saving new ~bms voicemail message and metadata", [Length]),
    MediaId = message_media_doc(whapps_call:account_db(Call), Box),

    case store_recording(AttachmentName, MediaId, Call) of
        {ok, StoreJObj} ->
            AR = wh_json:get_value(<<"Application-Response">>, StoreJObj),
            update_mailbox(Box, Call, MediaId, Length, AR);
        {error, FailJObj} ->
            lager:debug("failed to store media: ~p", [FailJObj])
    end.

update_mailbox(#mailbox{mailbox_id=Id
                        ,owner_id=OwnerId
                        ,transcribe_voicemail=MaybeTranscribe
                       }=Box, Call, MediaId, Length, ARJObj) ->
    Status = wh_json:get_value(<<"Status-Code">>, ARJObj),
    Loc = wh_json:get_value([<<"Headers">>, <<"Location">>], ARJObj),
    lager:debug("stored voicemail message (~s) ~s", [Status, Loc]),

    Transcription = maybe_transcribe(Call, MediaId, MaybeTranscribe),

    Prop = [{<<"From-User">>, whapps_call:from_user(Call)}
            ,{<<"From-Realm">>, whapps_call:from_realm(Call)}
            ,{<<"To-User">>, whapps_call:to_user(Call)}
            ,{<<"To-Realm">>, whapps_call:to_realm(Call)}
            ,{<<"Account-DB">>, whapps_call:account_db(Call)}
            ,{<<"Voicemail-Box">>, Id}
            ,{<<"Voicemail-Name">>, MediaId}
            ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
            ,{<<"Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
            ,{<<"Voicemail-Timestamp">>, new_timestamp()}
            ,{<<"Voicemail-Length">>, Length}
            ,{<<"Voicemail-Transcription">>, Transcription}
            ,{<<"Call-ID">>, whapps_call:call_id(Call)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],

    lager:debug("sending notification"),
    _ = case whapps_util:amqp_pool_request(Prop
                                           ,fun wapi_notifications:publish_voicemail/1
                                           ,fun wapi_notifications:notify_update_v/1
                                           ,5000
                                          )
        of
            {ok, UpdateJObj} ->
                lager:debug("notification update received: ~p", [UpdateJObj]),
                maybe_save_meta(Length, Box, Call, MediaId, UpdateJObj);
            {error, _E} ->
                lager:debug("notification error: ~p", [_E]),
                save_meta(Length, Box, Call, MediaId),
                timer:sleep(1000)
        end,
    cf_util:update_mwi(OwnerId, whapps_call:account_db(Call)).

maybe_save_meta(Length, #mailbox{delete_after_notify=false}=Box, Call, MediaId, _UpdateJObj) ->
    save_meta(Length, Box, Call, MediaId);
maybe_save_meta(Length, #mailbox{delete_after_notify=true}=Box, Call, MediaId, UpdateJObj) ->
    case wh_json:get_value(<<"Status">>, UpdateJObj) of
        <<"completed">> ->
            lager:debug("attachment was sent out via notification, deleting"),
            couch_mgr:del_doc(whapps_call:account_db(Call), MediaId);
        <<"failed">> ->
            lager:debug("attachment failed to send out via notification: ~s", [wh_json:get_value(<<"Failure-Message">>, UpdateJObj)]),
            save_meta(Length, Box, Call, MediaId)
    end.

save_meta(Length, #mailbox{mailbox_id=Id}, Call, MediaId) ->
    Metadata = wh_json:from_list(
                 [{<<"timestamp">>, new_timestamp()}
                  ,{<<"from">>, whapps_call:from(Call)}
                  ,{<<"to">>, whapps_call:to(Call)}
                  ,{<<"caller_id_number">>, whapps_call:caller_id_number(Call)}
                  ,{<<"caller_id_name">>, whapps_call:caller_id_name(Call)}
                  ,{<<"call_id">>, whapps_call:call_id(Call)}
                  ,{<<"folder">>, ?FOLDER_NEW}
                  ,{<<"length">>, Length}
                  ,{<<"media_id">>, MediaId}
                 ]),
    {ok, BoxJObj} = save_metadata(Metadata, whapps_call:account_db(Call), Id),
    lager:debug("stored voicemail metadata for ~s: ~p", [MediaId, BoxJObj]).

-spec maybe_transcribe/3 :: (whapps_call:call(), ne_binary(), boolean()) ->
                                    'undefined' | wh_json:object().
maybe_transcribe(Call, MediaId, true) ->
    Db = whapps_call:account_db(Call),
    {ok, MediaDoc} = couch_mgr:open_doc(Db, MediaId),

    case wh_json:get_value(<<"_attachments">>, MediaDoc, []) of
        [] ->
            lager:warning("no audio attachments on media doc ~s: ~p", [MediaId, MediaDoc]),
            undefined;
        Attachments ->
            {Attachment, MetaData} = hd(wh_json:to_proplist(Attachments)),
            case couch_mgr:fetch_attachment(Db, MediaId, Attachment) of
                {ok, Bin} ->
                    lager:debug("transcribing first attachment ~s: ~p", [Attachment, MetaData]),
                    maybe_transcribe(Db, MediaDoc, Bin, wh_json:get_value(<<"content_type">>, MetaData));
                {error, _E} ->
                    lager:debug("error fetching vm: ~p", [_E]),
                    undefined
            end
    end;
maybe_transcribe(_, _, false) ->
    undefined.

-spec maybe_transcribe/4 :: (ne_binary(), wh_json:object(), binary(), api_binary()) ->
                                    'undefined' | wh_json:object().
maybe_transcribe(_, _, _, undefined) -> undefined;
maybe_transcribe(_, _, <<>>, _) -> undefined;
maybe_transcribe(Db, MediaDoc, Bin, <<"audio/wav">>) ->
    case whapps_speech:asr_freeform(Bin) of
        {ok, Resp} ->
            lager:debug("transcription resp: ~p", [Resp]),
            MediaDoc1 = wh_json:set_value(<<"transcription">>, Resp, MediaDoc),
            _ = couch_mgr:ensure_saved(Db, MediaDoc1),
            is_valid_transcription(wh_json:get_value(<<"result">>, Resp)
                                   ,wh_json:get_value(<<"text">>, Resp)
                                   ,Resp
                                  );
        {error, _E} ->
            lager:debug("error getting ASR: ~p", [_E]),
            undefined
    end;
maybe_transcribe(_, _, _, _ContentType) ->
    lager:debug("un-ASR-able content type: ~s", [_ContentType]),
    undefined.

-spec is_valid_transcription/3 :: (api_binary(), binary(), wh_json:object()) ->
                                          wh_json:object() | 'undefined'.
is_valid_transcription(<<"success">>, ?NE_BINARY, Resp) -> Resp;
is_valid_transcription(_Res, _Txt, _) ->
    lager:debug("not valid transcription: ~s: '~s'", [_Res, _Txt]),
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_metadata/3 :: (wh_json:object(), ne_binary(), ne_binary()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', atom()}.
save_metadata(NewMessage, Db, Id) ->
    {ok, JObj} = couch_mgr:open_doc(Db, Id),
    Messages = wh_json:get_value([<<"messages">>], JObj, []),
    case has_message_meta(wh_json:get_value(<<"call_id">>, NewMessage), Messages) of
        true ->
            lager:debug("message meta already exists in VM Messages"),
            {ok, JObj};
        false ->
            case couch_mgr:save_doc(Db, wh_json:set_value([<<"messages">>], [NewMessage | Messages], JObj)) of
                {error, conflict} ->
                    lager:debug("saving resulted in a conflict, trying again"),
                    save_metadata(NewMessage, Db, Id);
                {ok, _}=Ok -> Ok;
                {error, R}=E ->
                    lager:debug("error while storing voicemail metadata: ~p", [R]),
                    E
            end
    end.

-spec has_message_meta/2 :: (ne_binary(), wh_json:objects()) -> boolean().
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
-spec get_mailbox_profile/2 :: (wh_json:object(), whapps_call:call()) -> mailbox().
get_mailbox_profile(Data, Call) ->
    Id = wh_json:get_value(<<"id">>, Data),
    AccountDb = whapps_call:account_db(Call),

    case get_mailbox_doc(AccountDb, Id, whapps_call:kvs_fetch(cf_capture_group, Call)) of
        {ok, JObj} ->
            MailboxId = wh_json:get_value(<<"_id">>, JObj),
            lager:debug("loaded voicemail box ~s", [MailboxId]),
            Default = #mailbox{},

            %% dont check if the voicemail box belongs to the owner (by default) if the call was not
            %% specificly to him, IE: calling a ring group and going to voicemail should not check
            LastAct = whapps_call:kvs_fetch(cf_last_action, Call),
            CheckIfOwner = ((undefined =:= LastAct) orelse (cf_device =:= LastAct)),

            {NameMediaId, OwnerId}
                = case wh_json:get_ne_value(<<"owner_id">>, JObj) of
                      undefined -> {wh_json:get_ne_value(?RECORDED_NAME_KEY, JObj), undefined};
                      OId ->
                          case couch_mgr:open_cache_doc(AccountDb, OId) of
                              {ok, Owner} ->
                                  {wh_json:find(?RECORDED_NAME_KEY, [Owner, JObj]), OId};
                              {error, not_found} ->
                                  lager:debug("owner ~s no longer exists", [OId]),
                                  {wh_json:get_ne_value(?RECORDED_NAME_KEY, JObj), undefined}
                          end
                  end,

            MaxMessageCount =
                case whapps_account_config:get(whapps_call:account_id(Call)
                                               ,?CF_CONFIG_CAT
                                               ,[<<"voicemail">>, <<"max_message_count">>]
                                              ) of
                    undefined ->
                        whapps_config:get(?CF_CONFIG_CAT
                                          ,[<<"voicemail">>, <<"max_message_count">>]
                                          ,?MAILBOX_DEFAULT_SIZE
                                         );
                    MMC -> MMC
                end,
            MsgCount = count_non_deleted_messages(wh_json:get_value(<<"messages">>, JObj, [])),

            lager:debug("mailbox limited to ~p voicemail messages (has ~b currently)", [MaxMessageCount, MsgCount]),

            #mailbox{mailbox_id = MailboxId
                     ,exists = true
                     ,keys = populate_keys(Call)
                     ,skip_instructions =
                         wh_json:is_true(<<"skip_instructions">>, JObj, Default#mailbox.skip_instructions)
                     ,skip_greeting =
                         wh_json:is_true(<<"skip_greeting">>, JObj, Default#mailbox.skip_greeting)
                     ,pin =
                         wh_json:get_binary_value(<<"pin">>, JObj, <<>>)
                     ,timezone =
                         wh_json:get_value(<<"timezone">>, JObj, Default#mailbox.timezone)
                     ,mailbox_number =
                         wh_json:get_binary_value(<<"mailbox">>, JObj, whapps_call:request_user(Call))
                     ,require_pin =
                         wh_json:is_true(<<"require_pin">>, JObj)
                     ,check_if_owner =
                         wh_json:is_true(<<"check_if_owner">>, JObj, CheckIfOwner)
                     ,unavailable_media_id =
                         wh_json:get_ne_value([<<"media">>, <<"unavailable">>], JObj)
                     ,name_media_id =
                         NameMediaId
                     ,owner_id =
                         OwnerId
                     ,is_setup =
                         wh_json:is_true(<<"is_setup">>, JObj, false)
                     ,max_message_count =
                         wh_util:to_integer(MaxMessageCount)
                     ,max_message_length =
                         find_max_message_length([Data, JObj])
                     ,message_count =
                         MsgCount
                     ,transcribe_voicemail =
                         wh_json:is_true(<<"transcribe">>, JObj, false)
                     ,notifications =
                         wh_json:get_value(<<"notifications">>, JObj)
                     ,delete_after_notify =
                         wh_json:is_true(<<"delete_after_notify">>, JObj, false)
                    };
        {error, R} ->
            lager:debug("failed to load voicemail box ~s, ~p", [Id, R]),
            #mailbox{}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec populate_keys/1 :: (whapps_call:call()) -> vm_keys().
populate_keys(Call) ->
    Default = #keys{},
    JObj = whapps_account_config:get(whapps_call:account_id(Call), <<"keys">>),
    #keys{operator = wh_json:get_binary_value([<<"voicemail">>, <<"operator">>], JObj, Default#keys.operator)
          ,login = wh_json:get_binary_value([<<"voicemail">>, <<"login">>], JObj, Default#keys.login)
          ,save = wh_json:get_binary_value([<<"voicemail">>, <<"save">>], JObj, Default#keys.save)
          ,listen = wh_json:get_binary_value([<<"voicemail">>, <<"listen">>], JObj, Default#keys.listen)
          ,record = wh_json:get_binary_value([<<"voicemail">>, <<"record">>], JObj, Default#keys.record)
          ,hear_new = wh_json:get_binary_value([<<"voicemail">>, <<"hear_new">>], JObj, Default#keys.hear_new)
          ,hear_saved = wh_json:get_binary_value([<<"voicemail">>, <<"hear_saved">>], JObj, Default#keys.hear_saved)
          ,configure = wh_json:get_binary_value([<<"voicemail">>, <<"configure">>], JObj, Default#keys.configure)
          ,exit = wh_json:get_binary_value([<<"voicemail">>, <<"exit">>], JObj, Default#keys.exit)
          ,rec_unavailable = wh_json:get_binary_value([<<"voicemail">>, <<"record_unavailable">>], JObj, Default#keys.rec_unavailable)
          ,rec_name = wh_json:get_binary_value([<<"voicemail">>, <<"record_name">>], JObj, Default#keys.rec_name)
          ,set_pin = wh_json:get_binary_value([<<"voicemail">>, <<"set_pin">>], JObj, Default#keys.set_pin)
          ,return_main = wh_json:get_binary_value([<<"voicemail">>, <<"return_main_menu">>], JObj, Default#keys.return_main)
          ,keep = wh_json:get_binary_value([<<"voicemail">>, <<"keep">>], JObj, Default#keys.keep)
          ,replay = wh_json:get_binary_value([<<"voicemail">>, <<"replay">>], JObj, Default#keys.replay)
          ,delete = wh_json:get_binary_value([<<"voicemail">>, <<"delete">>], JObj, Default#keys.delete)
         }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_mailbox_doc/3 :: (ne_binary(), api_binary(), api_binary()) ->
                                   {'ok', wh_json:object()} |
                                   {'error', term()}.
get_mailbox_doc(Db, Id, CaptureGroup) ->
    CGIsEmpty = wh_util:is_empty(CaptureGroup),
    case wh_util:is_empty(Id) of
        false ->
            lager:debug("opening ~s", [Id]),
            couch_mgr:open_doc(Db, Id);
        true when not CGIsEmpty ->
            lager:debug("capture group not empty: ~s", [CaptureGroup]),
            Opts = [{key, CaptureGroup}, include_docs],
            case couch_mgr:get_results(Db, <<"cf_attributes/mailbox_number">>, Opts) of
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
-spec review_recording/4 :: (ne_binary(), boolean(), mailbox(), whapps_call:call()) ->
                                    {'ok', 'record' | 'save' | 'no_selection'} |
                                    {'branch', wh_json:object()}.
-spec review_recording/5 :: (ne_binary(), boolean(), mailbox(), whapps_call:call(), integer()) ->
                                    {'ok', 'record' | 'save' | 'no_selection'} |
                                    {'branch', wh_json:object()}.

review_recording(AttachmentName, AllowOperator, Box, Call) ->
    review_recording(AttachmentName, AllowOperator, Box, Call, 1).

review_recording(_, _, _, _, Loop) when Loop > 4 ->
    {ok, no_selection};
review_recording(AttachmentName, AllowOperator
                 ,#mailbox{keys=#keys{listen=Listen
                                      ,save=Save
                                      ,record=Record
                                      ,operator=Operator
                                     }}=Box
                 ,Call, Loop) ->
    lager:debug("playing recording review options"),
    case whapps_call_command:b_prompt_and_collect_digit(<<"vm-review_recording">>, Call) of
        {ok, Listen} ->
            lager:debug("caller choose to replay the recording"),
            _ = whapps_call_command:b_play(AttachmentName, Call),
            review_recording(AttachmentName, AllowOperator, Box, Call);
        {ok, Record} ->
            lager:debug("caller choose to re-record"),
            {ok, record};
        {ok, Save} ->
            lager:debug("caller choose to save the recording"),
            {ok, save};
        {ok, Operator} when AllowOperator ->
            lager:debug("caller choose to ring the operator"),
            case cf_util:get_operator_callflow(whapps_call:account_id(Call)) of
                {ok, Flow} -> {branch, Flow};
                {error,_R} -> review_recording(AttachmentName, AllowOperator, Box, Call, Loop + 1)
            end;
        {error, _} ->
            lager:debug("error while waiting for review selection"),
            {ok, no_selection};
        _ ->
            review_recording(AttachmentName, AllowOperator, Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording/3 :: (ne_binary(), ne_binary(), whapps_call:call()) ->
                                   {'ok', wh_json:object()} |
                                   {'error', wh_json:object()}.
store_recording(AttachmentName, DocId, Call) ->
    lager:debug("storing recording ~s in doc ~s", [AttachmentName, DocId]),
    {ok, StoreJObj} = whapps_call_command:b_store(AttachmentName
                                                  ,get_new_attachment_url(AttachmentName, DocId, Call)
                                                  ,Call),
    case wh_json:get_value(<<"Application-Response">>, StoreJObj) of
        <<"failure">> ->
            lager:debug("seems store failed"),
            {error, StoreJObj};
        _ -> {ok, StoreJObj}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_new_attachment_url/3 :: (ne_binary(), ne_binary(), whapps_call:call()) -> ne_binary().
get_new_attachment_url(AttachmentName, MediaId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    _ = case couch_mgr:open_doc(AccountDb, MediaId) of
            {ok, JObj} ->
                case wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())) of
                    [] -> ok;
                    Existing ->
                        [begin
                             lager:debug("need to remove ~s/~s/~s first", [AccountDb, MediaId, Attach]),
                             couch_mgr:delete_attachment(AccountDb, MediaId, Attach)
                         end
                         || Attach <- Existing
                        ]
                end;
            {error, _} -> ok
        end,
    {ok, URL} = wh_media_url:store(AccountDb, MediaId, AttachmentName),
    URL.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec message_media_doc/2 :: (ne_binary(), mailbox()) -> ne_binary().
message_media_doc(Db, #mailbox{mailbox_number=BoxNum, mailbox_id=Id, timezone=Timezone}) ->
    UtcSeconds = wh_util:current_tstamp(),
    UtcDateTime = calendar:gregorian_seconds_to_datetime(UtcSeconds),
    Name = case localtime:utc_to_local(UtcDateTime, wh_util:to_list(Timezone)) of
               {{Y,M,D},{H,I,S}} ->
                   list_to_binary(["mailbox ", BoxNum, " message "
                                   ,wh_util:to_binary(M), "-"
                                   ,wh_util:to_binary(D), "-"
                                   ,wh_util:to_binary(Y), " "
                                   ,wh_util:to_binary(H), ":"
                                   ,wh_util:to_binary(I), ":"
                                   ,wh_util:to_binary(S)
                                  ]);
               {error, unknown_tz} ->
                   lager:debug("unknown timezone: ~s", [Timezone]),
                   {{Y,M,D},{H,I,S}} = UtcDateTime,
                   list_to_binary(["mailbox ", BoxNum, " message "
                                   ,wh_util:to_binary(M), "-"
                                   ,wh_util:to_binary(D), "-"
                                   ,wh_util:to_binary(Y), " "
                                   ,wh_util:to_binary(H), ":"
                                   ,wh_util:to_binary(I), ":"
                                   ,wh_util:to_binary(S), " UTC"
                                  ])
           end,

    Ext = whapps_config:get(?CF_CONFIG_CAT, [<<"voicemail">>, <<"extension">>], <<"mp3">>),
    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<"voicemail message media">>}
             ,{<<"source_type">>, <<"voicemail">>}
             ,{<<"source_id">>, Id}
             ,{<<"media_source">>, <<"recording">>}
             ,{<<"media_type">>, Ext}
             ,{<<"streamable">>, true}
             ,{<<"utc_seconds">>, UtcSeconds}
            ],
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), Db, [{type, <<"private_media">>}]),
    {ok, JObj} = couch_mgr:save_doc(Db, Doc),
    wh_json:get_value(<<"_id">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec recording_media_doc/3 :: (ne_binary(), mailbox(), whapps_call:call()) -> ne_binary().
recording_media_doc(Recording, #mailbox{mailbox_number=BoxNum
                                        ,mailbox_id=Id
                                        ,owner_id=OwnerId
                                       }, Call) ->
    AccountDb = whapps_call:account_db(Call),
    Name = list_to_binary(["mailbox ", BoxNum, " ", Recording]),
    Props = props:filter_undefined(
              [{<<"name">>, Name}
               ,{<<"description">>, <<"voicemail recorded/prompt media">>}
               ,{<<"source_type">>, <<"voicemail">>}
               ,{<<"source_id">>, Id}
               ,{<<"owner_id">>, OwnerId}
               ,{<<"media_source">>, <<"recording">>}
               ,{<<"streamable">>, true}
              ]),
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), AccountDb, [{type, <<"media">>}]),
    {ok, JObj} = couch_mgr:save_doc(AccountDb, Doc),
    wh_json:get_value(<<"_id">>, JObj).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_messages/2 :: (mailbox(), whapps_call:call()) -> wh_json:objects().
get_messages(#mailbox{mailbox_id=Id}, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, Id) of
        {ok, JObj} -> wh_json:get_value(<<"messages">>, JObj, []);
        _ -> []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_message/2 :: (wh_json:object(), whapps_call:call()) -> ne_binary().
get_message(Message, Call) ->
    MediaId = wh_json:get_value(<<"media_id">>, Message),
    list_to_binary(["/", whapps_call:account_db(Call), "/", MediaId]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec count_messages/2 :: (wh_json:objects(), ne_binary()) -> non_neg_integer().
count_messages(Messages, Folder) ->
    lists:sum([1 || Message <- Messages, wh_json:get_value(<<"folder">>, Message) =:= Folder]).

-spec count_non_deleted_messages/1 :: (wh_json:objects()) -> non_neg_integer().
-spec count_non_deleted_messages/2 :: (wh_json:objects(), non_neg_integer()) -> non_neg_integer().
count_non_deleted_messages(L) ->
    count_non_deleted_messages(L, 0).
count_non_deleted_messages([], Count) ->
    Count;
count_non_deleted_messages([MsgMeta|Messages], Count) ->
    case wh_json:get_value(<<"folder">>, MsgMeta) of
        ?FOLDER_DELETED -> count_non_deleted_messages(Messages, Count);
        _ -> count_non_deleted_messages(Messages, Count+1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_folder/2 :: (wh_json:objects(), ne_binary()) -> wh_json:objects().
get_folder(Messages, Folder) ->
    [M || M <- Messages, wh_json:get_value(<<"folder">>, M) =:= Folder].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_folder/4 :: (ne_binary(), wh_json:object(), mailbox(), whapps_call:call()) -> no_return().
set_folder(Folder, Message, Box, Call) ->
    lager:debug("setting folder for message to ~s", [Folder]),
    not (wh_json:get_value(<<"folder">>, Message) =:= Folder) andalso
        update_folder(Folder, wh_json:get_value(<<"media_id">>, Message), Box, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_folder/4 :: (ne_binary(), ne_binary(), mailbox(), whapps_call:call()) -> {'ok', wh_json:object()} |
                                                                                       {'error', term()}.
update_folder(_, undefined, _, _) ->
    {error, attachment_undefined};
update_folder(Folder, MediaId, #mailbox{mailbox_id=Id}=Mailbox, Call) ->
    AccountDb = whapps_call:account_db(Call),
    Folder =:= ?FOLDER_DELETED andalso update_doc(<<"pvt_deleted">>, true, MediaId, AccountDb),
    case couch_mgr:open_doc(AccountDb, Id) of
        {ok, JObj} ->
            Messages = [ update_folder1(Message, Folder, MediaId, wh_json:get_value(<<"media_id">>, Message))
                         || Message <- wh_json:get_value(<<"messages">>, JObj, []) ],
            case couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"messages">>, Messages, JObj)) of
                {error, conflict} ->
                    update_folder(Folder, MediaId, Mailbox, Call);
                {ok, _}=OK ->
                    OK;
                {error, R}=E ->
                    lager:debug("error while updating folder ~s ~p", [Folder, R]),
                    E
            end;
        {error, R}=E ->
            lager:debug("failed ot open mailbox ~s: ~p", [Id, R]),
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
-spec update_doc/4 :: (wh_json:json_string() | wh_json:json_strings()
                       ,wh_json:json_term()
                       ,mailbox() | ne_binary()
                       ,whapps_call:call() | ne_binary()
                      ) ->
                              'ok' |
                              {'error', atom()}.
update_doc(Key, Value, #mailbox{mailbox_id=Id}, Db) ->
    update_doc(Key, Value, Id, Db);
update_doc(Key, Value, Id, ?NE_BINARY = Db) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            case couch_mgr:save_doc(Db, wh_json:set_value(Key, Value, JObj)) of
                {error, conflict} ->
                    update_doc(Key, Value, Id, Db);
                {ok, _} ->
                    ok;
                {error, R}=E ->
                    lager:debug("unable to update ~s in ~s, ~p", [Id, Db, R]),
                    E
            end;
        {error, R}=E ->
            lager:debug("unable to update ~s in ~s, ~p", [Id, Db, R]),
            E
    end;
update_doc(Key, Value, Id, Call) ->
    update_doc(Key, Value, Id, whapps_call:account_db(Call)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tmp_file/0 :: () -> ne_binary().
tmp_file() ->
    Ext = whapps_config:get(?CF_CONFIG_CAT, [<<"voicemail">>, <<"extension">>], <<"mp3">>),
    <<(wh_util:to_hex_binary(crypto:rand_bytes(16)))/binary, ".", Ext/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the Universal Coordinated Time (UTC) reported by the
%% underlying operating system (local time is used if universal
%% time is not available) as number of gregorian seconds starting
%% with year 0.
%% @end
%%--------------------------------------------------------------------
-spec new_timestamp/0 :: () -> pos_integer().
new_timestamp() -> wh_util:current_tstamp().

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
-spec find_max_message_length/1 :: (wh_json:objects()) -> pos_integer().
find_max_message_length([JObj | T]) ->
    case wh_json:get_integer_value(<<"max_message_length">>, JObj) of
        Len when is_integer(Len) andalso Len > 0 -> Len;
        _ -> find_max_message_length(T)
    end;
find_max_message_length([]) ->
    whapps_config:get_integer(<<"voicemail">>, <<"max_message_length">>, 120).
