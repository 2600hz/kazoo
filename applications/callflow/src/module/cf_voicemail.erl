%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "action":"compose"|"check"
%%%   // optional
%%%   ,"id":"vmbox_id"
%%%   ,"max_message_length":500
%%%   ,"interdigit_timeout":2000 // in milliseconds
%%% }
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_voicemail).

-include("callflow.hrl").
-include_lib("whistle/src/wh_json.hrl").

-export([handle/2]).
-export([new_message/4]).

-define(KEY_MEDIA_ID, <<"media_id">>).
-define(KEY_VOICEMAIL, <<"voicemail">>).
-define(KEY_MAX_MESSAGE_COUNT, <<"max_message_count">>).
-define(KEY_MAX_MESSAGE_LENGTH, <<"max_message_length">>).
-define(KEY_MIN_MESSAGE_SIZE, <<"min_message_size">>).
-define(KEY_MAX_BOX_NUMBER_LENGTH, <<"max_box_number_length">>).
-define(KEY_EXTERNAL_STORAGE, <<"external_storage">>).
-define(KEY_EXTENSION, <<"extension">>).
-define(KEY_MAX_PIN_LENGTH, <<"max_pin_length">>).
-define(KEY_DELETE_AFTER_NOTIFY, <<"delete_after_notify">>).
-define(KEY_SAVE_AFTER_NOTIFY, <<"save_after_notify">>).

-define(MAILBOX_DEFAULT_SIZE
        ,whapps_config:get_integer(?CF_CONFIG_CAT
                                   ,[?KEY_VOICEMAIL, ?KEY_MAX_MESSAGE_COUNT]
                                   ,100
                                  )).
-define(MAILBOX_DEFAULT_MSG_MAX_LENGTH
        ,whapps_config:get_integer(?CF_CONFIG_CAT
                                   ,[?KEY_VOICEMAIL, ?KEY_MAX_MESSAGE_LENGTH]
                                   ,500
                                  )).
-define(MAILBOX_DEFAULT_MSG_MIN_LENGTH
        ,whapps_config:get_integer(?CF_CONFIG_CAT
                                   ,[?KEY_VOICEMAIL, ?KEY_MIN_MESSAGE_SIZE]
                                   ,500
                                  )).
-define(MAILBOX_DEFAULT_BOX_NUMBER_LENGTH
        ,whapps_config:get_integer(?CF_CONFIG_CAT
                                   ,[?KEY_VOICEMAIL, ?KEY_MAX_BOX_NUMBER_LENGTH]
                                   ,15
                                  )).
-define(MAILBOX_DEFAULT_STORAGE
        ,whapps_config:get(?CF_CONFIG_CAT
                           ,[?KEY_VOICEMAIL, ?KEY_EXTERNAL_STORAGE]
                          )).
-define(DEFAULT_VM_EXTENSION
        ,whapps_config:get(?CF_CONFIG_CAT
                           ,[?KEY_VOICEMAIL, ?KEY_EXTENSION]
                           ,<<"mp3">>
                          )
       ).

-define(DEFAULT_MAX_PIN_LENGTH
        ,whapps_config:get_integer(?CF_CONFIG_CAT
                                   ,[?KEY_VOICEMAIL, ?KEY_MAX_PIN_LENGTH]
                                   ,6
                                  )
       ).

-define(DEFAULT_SAVE_AFTER_NOTIFY
        ,whapps_config:get(?CF_CONFIG_CAT
                           ,[?KEY_VOICEMAIL, ?KEY_SAVE_AFTER_NOTIFY]
                           ,'false'
                          )
       ).

-define(DEFAULT_DELETE_AFTER_NOTIFY
        ,whapps_config:get(?CF_CONFIG_CAT
                           ,[?KEY_VOICEMAIL, ?KEY_DELETE_AFTER_NOTIFY]
                           ,'false'
                          )
       ).
-define(MAILBOX_RETRY_STORAGE_TIMES(AccountId)
        ,whapps_account_config:get_global(AccountId, ?CF_CONFIG_CAT
                                          ,[?KEY_VOICEMAIL, <<"storage_retry_times">>]
                                          ,5
                                         )).
-define(MAILBOX_RETRY_LOCAL_STORAGE_REMOTE_FAILS(AccountId)
        ,whapps_account_config:get_global(AccountId, ?CF_CONFIG_CAT
                                          ,[?KEY_VOICEMAIL, <<"storage_retry_local_on_remote_failure">>]
                                          ,'true'
                                         )).

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
          ,rec_temporary_unavailable  = <<"4">>
          ,del_temporary_unavailable  = <<"5">>
          ,return_main = <<"0">>

          %% Post playbak
          ,keep = <<"1">>
          ,replay = <<"2">>
          ,prev = <<"4">>
          ,next = <<"6">>
          ,delete = <<"7">>
         }).
-type vm_keys() :: #keys{}.

-define(KEY_LENGTH, 1).

-record(mailbox, {
          mailbox_id :: api_binary()
          ,mailbox_number = <<>> :: binary()
          ,exists = 'false' :: boolean()
          ,skip_instructions = 'false' :: boolean()
          ,skip_greeting = 'false' :: boolean()
          ,unavailable_media_id :: api_binary()
          ,temporary_unavailable_media_id :: api_binary()
          ,name_media_id :: api_binary()
          ,pin = <<>> :: binary()
          ,timezone :: ne_binary()
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
          ,after_notify_action = 'nothing' :: 'nothing' | 'delete' | 'save'
          ,interdigit_timeout = whapps_call_command:default_interdigit_timeout() :: pos_integer()
          ,play_greeting_intro = 'false' :: boolean()
          ,use_person_not_available = 'false' :: boolean()
          ,not_configurable = 'false' :: boolean()
          ,account_db :: api_binary()
         }).
-type mailbox() :: #mailbox{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, based on the payload will either
%% connect a caller to check_voicemail or compose_voicemail.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case wh_json:get_value(<<"action">>, Data, <<"compose">>) of
        <<"compose">> ->
            whapps_call_command:answer(Call),
            lager:debug("answered the call and composing the voicemail"),
            case compose_voicemail(get_mailbox_profile(Data, Call), Call) of
                'ok' ->
                    lager:info("compose voicemail complete"),
                    cf_exe:continue(Call);
                {'branch', Flow} ->
                    lager:info("compose voicemail complete, branch to operator"),
                    cf_exe:branch(Flow, Call);
                {'error', 'channel_hungup'} ->
                    lager:info("channel has hungup, stopping the compose"),
                    cf_exe:stop(Call)
            end;
        <<"check">> ->
            whapps_call_command:answer(Call),
            case check_mailbox(get_mailbox_profile(Data, Call), Call) of
                'ok' ->  cf_exe:continue(Call);
                {'error', 'channel_hungup'} -> cf_exe:stop(Call)
            end;
        _ ->
            cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_mailbox(mailbox(), whapps_call:call()) ->
                           'ok' | {'error', 'channel_hungup'}.
-spec check_mailbox(mailbox(), whapps_call:call(), non_neg_integer()) ->
                           'ok' | {'error', 'channel_hungup'}.
-spec check_mailbox(mailbox(), boolean(), whapps_call:call(), non_neg_integer()) ->
                           'ok' | {'error', 'channel_hungup'}.

check_mailbox(Box, Call) ->
    %% Wrapper to initalize the attempt counter
    Resp = check_mailbox(Box, Call, 1),
    _ = send_mwi_update(Box, Call),
    Resp.

check_mailbox(#mailbox{owner_id=OwnerId}=Box, Call, Loop) ->
    IsOwner = is_owner(Call, OwnerId),
    check_mailbox(Box, IsOwner, Call, Loop).

check_mailbox(#mailbox{max_login_attempts=MaxLoginAttempts}, _, Call, Loop) when Loop > MaxLoginAttempts ->
    %% if we have exceeded the maximum loop attempts then terminate this call
    lager:info("maximum number of invalid attempts to check mailbox"),
    _ = whapps_call_command:b_prompt(<<"vm-abort">>, Call),
    'ok';
check_mailbox(#mailbox{exists='false'}=Box, _ , Call, Loop) ->
    %% if the callflow did not define the mailbox to check then request the mailbox ID from the user
    find_mailbox(Box, Call, Loop);
check_mailbox(#mailbox{require_pin='false'}=Box, 'true', Call, _) ->
    %% If this is the owner of the mailbox calling in and it doesn't require a pin then jump
    %% right to the main menu
    lager:info("caller is the owner of this mailbox, and requires no pin"),
    main_menu(Box, Call);
check_mailbox(#mailbox{pin = <<>>}=Box, 'true', Call, _) ->
    %% If this is the owner of the mailbox calling in and it doesn't require a pin then jump
    %% right to the main menu
    lager:info("caller is the owner of this mailbox, and it has no pin"),
    main_menu(Box, Call);
check_mailbox(#mailbox{pin = <<>>, require_pin='true'}, 'false', Call, _) ->
    %% If the caller is not the owner or the mailbox requires a pin to access it but has none set
    %% then terminate this call.
    lager:info("attempted to sign into a mailbox with no pin"),
    _ = whapps_call_command:b_prompt(<<"vm-no_access">>, Call),
    'ok';
check_mailbox(#mailbox{pin=Pin
                       ,interdigit_timeout=Interdigit
                      }=Box, IsOwner, Call, Loop) ->
    lager:info("requesting pin number to check mailbox"),

    NoopId = whapps_call_command:prompt(<<"vm-enter_pass">>, Call),

    case whapps_call_command:collect_digits(?DEFAULT_MAX_PIN_LENGTH
                                            ,whapps_call_command:default_collect_timeout()
                                            ,Interdigit
                                            ,NoopId
                                            ,Call
                                           )
    of
        {'ok', Pin} ->
            lager:info("caller entered a valid pin"),
            main_menu(Box, Call);
        {'ok', _} ->
            lager:info("invalid mailbox login"),
            _ = whapps_call_command:b_prompt(<<"vm-fail_auth">>, Call),
            check_mailbox(Box, IsOwner, Call, Loop + 1);
        _ ->
            'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_mailbox(mailbox(), whapps_call:call(), non_neg_integer()) -> 'ok'.

find_mailbox(#mailbox{max_login_attempts=MaxLoginAttempts}, Call, Loop) when Loop > MaxLoginAttempts ->
    %% if we have exceeded the maximum loop attempts then terminate this call
    lager:info("maximum number of invalid attempts to find mailbox"),
    _ = whapps_call_command:b_prompt(<<"vm-abort">>, Call),
    'ok';
find_mailbox(#mailbox{interdigit_timeout=Interdigit}=Box, Call, Loop) ->
    lager:info("requesting mailbox number to check"),

    NoopId = whapps_call_command:prompt(<<"vm-enter_id">>, Call),

    case whapps_call_command:collect_digits(?MAILBOX_DEFAULT_BOX_NUMBER_LENGTH
                                            ,whapps_call_command:default_collect_timeout()
                                            ,Interdigit
                                            ,NoopId
                                            ,Call
                                           )
    of
        {'ok', <<>>} ->
            find_mailbox(Box, Call, Loop + 1);
        {'ok', Mailbox} ->
            BoxNum = try wh_util:to_integer(Mailbox) catch _:_ -> 0 end,
            %% find the voicemail box, by making a fake 'callflow data payload' we look for it now because if the
            %% caller is the owner, and the pin is not required then we skip requesting the pin
            ViewOptions = [{'key', BoxNum}],
            AccountDb = whapps_call:account_db(Call),
            case kz_datamgr:get_results(AccountDb, <<"vmboxes/listing_by_mailbox">>, ViewOptions) of
                {'ok', []} ->
                    lager:info("mailbox ~s doesnt exist", [Mailbox]),
                    find_mailbox(Box, Call, Loop + 1);
                {'ok', [JObj]} ->
                    lager:info("get profile of ~p", [JObj]),
                    ReqBox = get_mailbox_profile(
                               wh_json:from_list([{<<"id">>, wh_doc:id(JObj)}])
                               ,Call
                              ),
                    check_mailbox(ReqBox, Call, Loop);
                {'ok', _} ->
                    lager:info("mailbox ~s is ambiguous", [Mailbox]),
                    find_mailbox(Box, Call, Loop + 1);
                _E ->
                    lager:info("failed to find mailbox ~s: ~p", [Mailbox, _E]),
                    find_mailbox(Box, Call, Loop + 1)
            end;
        _E -> lager:info("recv other: ~p", [_E])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_voicemail(mailbox(), whapps_call:call()) ->
                               'ok' | {'branch', _} |
                               {'error', 'channel_hungup'}.
-spec compose_voicemail(mailbox(), boolean(), whapps_call:call()) ->
                               'ok' | {'branch', _} |
                               {'error', 'channel_hungup'}.
compose_voicemail(#mailbox{owner_id=OwnerId}=Box, Call) ->
    IsOwner = is_owner(Call, OwnerId),
    compose_voicemail(Box, IsOwner, Call).

compose_voicemail(#mailbox{check_if_owner='true'}=Box, 'true', Call) ->
    lager:info("caller is the owner of this mailbox"),
    lager:info("overriding action as check (instead of compose)"),
    check_mailbox(Box, Call);
compose_voicemail(#mailbox{exists='false'}, _, Call) ->
    lager:info("attempted to compose voicemail for missing mailbox"),
    _ = whapps_call_command:b_prompt(<<"vm-not_available_no_voicemail">>, Call),
    'ok';
compose_voicemail(#mailbox{max_message_count=MaxCount
                           ,message_count=Count
                           ,mailbox_id=VMBId
                           ,mailbox_number=VMBN
                           ,keys=#keys{login=Login}
                          }=Box, _, Call) when Count >= MaxCount
                                               andalso MaxCount > 0 ->
    lager:debug("voicemail box is full, cannot hold more messages, sending notification"),
    Props = [{<<"Account-DB">>, whapps_call:account_db(Call)}
             ,{<<"Account-ID">>, whapps_call:account_id(Call)}
             ,{<<"Voicemail-Box">>, VMBId}
             ,{<<"Voicemail-Number">>, VMBN}
             ,{<<"Max-Message-Count">>, MaxCount}
             ,{<<"Message-Count">>, Count}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    _ = wh_amqp_worker:call(Props
                            ,fun wapi_notifications:publish_voicemail_full/1
                            ,fun wapi_notifications:voicemail_full_v/1
                           ),
    _ = whapps_call_command:prompt(<<"vm-mailbox_full">>, Call),
    _NoopId = whapps_call_command:noop(Call),

    case whapps_call_command:wait_for_application_or_dtmf(<<"noop">>, 5 * ?MILLISECONDS_IN_MINUTE) of
        {'dtmf', Login} ->
            lager:info("caller wishes to login to mailbox"),
            check_mailbox(Box, Call);
        _Else ->
            lager:debug("finished with call")
    end;
compose_voicemail(#mailbox{keys=#keys{login=Login
                                      ,operator=Operator
                                     }
                          }=Box, _, Call) ->
    lager:debug("playing mailbox greeting to caller"),
    _ = play_greeting_intro(Box, Call),
    _ = play_greeting(Box, Call),
    _ = play_instructions(Box, Call),
    _NoopId = whapps_call_command:noop(Call),
    %% timeout after 5 min for saftey, so this process cant hang around forever
    case whapps_call_command:wait_for_application_or_dtmf(<<"noop">>, 300000) of
        {'ok', _} ->
            lager:info("played greeting and instructions to caller, recording new message"),
            record_voicemail(tmp_file(), Box, Call);
        {'dtmf', Digit} ->
            _ = whapps_call_command:b_flush(Call),
            case Digit of
                Login ->
                    lager:info("caller pressed '~s', redirecting to check voicemail", [Login]),
                    check_mailbox(Box, Call);
                Operator ->
                    lager:info("caller chose to ring the operator"),
                    case cf_util:get_operator_callflow(whapps_call:account_id(Call)) of
                        {'ok', Flow} -> {'branch', Flow};
                        {'error', _R} -> record_voicemail(tmp_file(), Box, Call)
                    end;
                _Else ->
                    lager:info("caller pressed unbound '~s', skip to recording new message", [_Else]),
                    record_voicemail(tmp_file(), Box, Call)
            end;
        {'error', R} ->
            lager:info("error while playing voicemail greeting: ~p", [R])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_greeting_intro(mailbox(), whapps_call:call()) -> ne_binary() | 'ok'.
play_greeting_intro(#mailbox{play_greeting_intro='true'}, Call) ->
    whapps_call_command:audio_macro([{'prompt', <<"vm-greeting_intro">>}], Call);
play_greeting_intro(_, _) -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_greeting(mailbox(), whapps_call:call()) -> ne_binary() | 'ok'.
play_greeting(#mailbox{skip_greeting='true'}, _Call) -> 'ok';
play_greeting(#mailbox{temporary_unavailable_media_id= <<_/binary>> = MediaId}
              ,Call
             ) ->
    Corrected = wh_media_util:media_path(MediaId, Call),
    lager:info("mailbox has a temporary greeting which always overrides standard greeting: '~s', corrected to '~s'",
               [MediaId, Corrected]
              ),
    whapps_call_command:play(Corrected, Call);
play_greeting(#mailbox{use_person_not_available='true'
                       ,unavailable_media_id='undefined'
                      }, Call) ->
    lager:debug("mailbox has no greeting, playing the customized generic"),
    whapps_call_command:audio_macro([{'prompt', <<"vm-person_not_available">>}], Call);
play_greeting(#mailbox{unavailable_media_id='undefined'
                       ,mailbox_number=Mailbox
                      }, Call) ->
    lager:debug("mailbox has no greeting, playing the generic"),
    whapps_call_command:audio_macro([{'prompt', <<"vm-person">>}
                                     ,{'say', Mailbox}
                                     ,{'prompt', <<"vm-not_available">>}
                                    ], Call);
play_greeting(#mailbox{unavailable_media_id=MediaId}, Call) ->
    Corrected = wh_media_util:media_path(MediaId, Call),
    lager:info("mailbox has a greeting: '~s', corrected to '~s'", [MediaId, Corrected]),
    whapps_call_command:play(Corrected, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_instructions(mailbox(), whapps_call:call()) -> ne_binary() | 'ok'.
play_instructions(#mailbox{skip_instructions='true'}, _) -> 'ok';
play_instructions(#mailbox{skip_instructions='false'}, Call) ->
    whapps_call_command:prompt(<<"vm-record_message">>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_voicemail(ne_binary(), mailbox(), whapps_call:call()) -> 'ok'.
record_voicemail(AttachmentName, #mailbox{max_message_length=MaxMessageLength}=Box, Call) ->
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    whapps_call_command:tones([Tone], Call),
    lager:info("composing new voicemail to ~s", [AttachmentName]),
    case whapps_call_command:b_record(AttachmentName, ?ANY_DIGIT, wh_util:to_binary(MaxMessageLength), Call) of
        {'ok', Msg} ->
            Length = wh_json:get_integer_value(<<"Length">>, Msg, 0),
            IsCallUp = wh_json:get_value(<<"Hangup-Cause">>, Msg) =:= 'undefined',
            case IsCallUp
                andalso review_recording(AttachmentName, 'true', Box, Call)
            of
                'false' ->
                    cf_util:start_task(fun new_message/4, [AttachmentName, Length, Box], Call);
                {'ok', 'record'} ->
                    record_voicemail(tmp_file(), Box, Call);
                {'ok', _Selection} ->
                    cf_util:start_task(fun new_message/4, [AttachmentName, Length, Box], Call),
                    _ = whapps_call_command:prompt(<<"vm-saved">>, Call),
                    _ = whapps_call_command:prompt(<<"vm-thank_you">>, Call),
                    'ok';
                {'branch', Flow} ->
                    _ = new_message(AttachmentName, Length, Box, Call),
                    _ = whapps_call_command:prompt(<<"vm-saved">>, Call),
                    {'branch', Flow}
            end;
        {'error', _R} ->
            lager:info("error while attempting to record a new message: ~p", [_R])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec setup_mailbox(mailbox(), whapps_call:call()) -> mailbox().
setup_mailbox(Box, Call) ->
    lager:debug("starting voicemail configuration wizard"),
    {'ok', _} = whapps_call_command:b_prompt(<<"vm-setup_intro">>, Call),

    lager:info("prompting caller to set a pin"),
    #mailbox{} = change_pin(Box, Call),

    {'ok', _} = whapps_call_command:b_prompt(<<"vm-setup_rec_greeting">>, Call),
    lager:info("prompting caller to record an unavailable greeting"),

    #mailbox{}=Box1 = record_unavailable_greeting(tmp_file(), Box, Call),
    'ok' = update_doc(<<"is_setup">>, 'true', Box1, Call),
    lager:info("voicemail configuration wizard is complete"),

    {'ok', _} = whapps_call_command:b_prompt(<<"vm-setup_complete">>, Call),
    Box1#mailbox{is_setup='true'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec main_menu(mailbox(), whapps_call:call()) ->
                       'ok' | {'error', 'channel_hungup'}.
-spec main_menu(mailbox(), whapps_call:call(), non_neg_integer()) ->
                       'ok' | {'error', 'channel_hungup'}.
main_menu(#mailbox{is_setup='false'}=Box, Call) ->
    try setup_mailbox(Box, Call) of
        #mailbox{}=Box1 -> main_menu(Box1, Call, 1)
    catch
        'error':{'badmatch',{'error','channel_hungup'}} ->
            lager:debug("channel has hungup while setting up mailbox"),
            {'error', 'channel_hungup'};
        _E:_R ->
            lager:debug("failed to setup mailbox: ~s: ~p", [_E, _R])
    end;
main_menu(Box, Call) -> main_menu(Box, Call, 1).

main_menu(Box, Call, Loop) when Loop > 4 ->
    %% If there have been too may loops with no action from the caller this
    %% is likely a abandonded channel, terminate
    lager:info("entered main menu with too many invalid entries"),
    _ = whapps_call_command:b_prompt(<<"vm-goodbye">>, Call),
    send_mwi_update(Box, Call);
main_menu(#mailbox{keys=#keys{hear_new=HearNew
                               ,hear_saved=HearSaved
                               ,exit=Exit
                              }
                   ,interdigit_timeout=Interdigit
                   ,not_configurable='true'
                   ,mailbox_id=BoxId
                  }=Box, Call, Loop) ->

    lager:debug("playing mailbox main menu"),
    _ = whapps_call_command:b_flush(Call),

    Messages = kz_vm_message:messages(whapps_call:account_id(Call), BoxId),
    New = kzd_voice_message:count_messages(Messages, ?VM_FOLDER_NEW),
    Saved = kzd_voice_message:count_messages(Messages, ?VM_FOLDER_SAVED),

    lager:debug("mailbox has ~p new and ~p saved messages", [New, Saved]),
    NoopId = whapps_call_command:audio_macro(message_count_prompts(New, Saved)
                                             ++ [{'prompt', <<"vm-main_menu_not_configurable">>}]
                                             ,Call
                                            ),

    case whapps_call_command:collect_digits(?KEY_LENGTH
                                            ,whapps_call_command:default_collect_timeout()
                                            ,Interdigit
                                            ,NoopId
                                            ,Call
                                           )
    of
        {'error', _} ->
            lager:info("error during mailbox main menu"),
            send_mwi_update(Box, Call);
        {'ok', Exit} ->
            lager:info("user choose to exit voicemail menu"),
            send_mwi_update(Box, Call);
        {'ok', HearNew} ->
            lager:info("playing all messages in folder: ~s", [?VM_FOLDER_NEW]),
            Folder = kzd_voice_message:filter_folder(Messages, ?VM_FOLDER_NEW),
            case play_messages(Folder, New, Box, Call) of
                'ok' -> send_mwi_update(Box, Call);
                _Else -> main_menu(Box, Call)
            end;
        {'ok', HearSaved} ->
            lager:info("playing all messages in folder: ~s", [?VM_FOLDER_SAVED]),
            Folder = kzd_voice_message:filter_folder(Messages, ?VM_FOLDER_SAVED),
            case play_messages(Folder, Saved, Box, Call) of
                'ok' -> send_mwi_update(Box, Call);
                _Else ->  main_menu(Box, Call)
            end;
        _ ->
            main_menu(Box, Call, Loop + 1)
    end;
main_menu(#mailbox{keys=#keys{hear_new=HearNew
                               ,hear_saved=HearSaved
                               ,configure=Configure
                               ,exit=Exit
                              }
                   ,interdigit_timeout=Interdigit
                   ,not_configurable='false'
                   ,mailbox_id=BoxId
                  }=Box, Call, Loop) ->
    lager:debug("playing mailbox main menu"),
    _ = whapps_call_command:b_flush(Call),

    Messages = kz_vm_message:messages(whapps_call:account_id(Call), BoxId),
    New = kzd_voice_message:count_messages(Messages, ?VM_FOLDER_NEW),
    Saved = kzd_voice_message:count_messages(Messages, ?VM_FOLDER_SAVED),

    lager:debug("mailbox has ~p new and ~p saved messages", [New, Saved]),
    NoopId = whapps_call_command:audio_macro(message_count_prompts(New, Saved)
                                             ++ [{'prompt', <<"vm-main_menu">>}]
                                             ,Call),

    case whapps_call_command:collect_digits(?KEY_LENGTH
                                            ,whapps_call_command:default_collect_timeout()
                                            ,Interdigit
                                            ,NoopId
                                            ,Call
                                           )
    of
        {'error', _} ->
            lager:info("error during mailbox main menu"),
                send_mwi_update(Box, Call);
        {'ok', Exit} ->
            lager:info("user choose to exit voicemail menu"),
            send_mwi_update(Box, Call);
        {'ok', HearNew} ->
            lager:info("playing all messages in folder: ~s", [?VM_FOLDER_NEW]),
            Folder = kzd_voice_message:filter_folder(Messages, ?VM_FOLDER_NEW),
            case play_messages(Folder, New, Box, Call) of
                'ok' -> send_mwi_update(Box, Call);
                _Else -> main_menu(Box, Call)
            end;
        {'ok', HearSaved} ->
            lager:info("playing all messages in folder: ~s", [?VM_FOLDER_SAVED]),
            Folder = kzd_voice_message:filter_folder(Messages, ?VM_FOLDER_SAVED),
            case play_messages(Folder, Saved, Box, Call) of
                'ok' -> send_mwi_update(Box, Call);
                _Else ->  main_menu(Box, Call)
            end;
        {'ok', Configure} ->
            lager:info("caller chose to change their mailbox configuration"),
            case config_menu(Box, Call) of
                'ok' -> 'ok';
                {'error', 'channel_hungup'}=E ->
                    lager:debug("channel has hungup, done trying to setup mailbox"),
                    E;
                #mailbox{}=Box1 -> main_menu(Box1, Call)
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
-spec message_count_prompts(integer(), integer()) -> wh_proplist().
message_count_prompts(0, 0) ->
    [{'prompt', <<"vm-no_messages">>}];
message_count_prompts(1, 0) ->
    [{'prompt', <<"vm-you_have">>}
     ,{'say', <<"1">>, ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-new_message">>}
    ];
message_count_prompts(0, 1) ->
    [{'prompt', <<"vm-you_have">>}
     ,{'say', <<"1">>, ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-saved_message">>}
    ];
message_count_prompts(1, 1) ->
    [{'prompt', <<"vm-you_have">>}
     ,{'say', <<"1">>, ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-new_and">>}
     ,{'say', <<"1">>, ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-saved_message">>}
    ];
message_count_prompts(New, 0) ->
    [{'prompt', <<"vm-you_have">>}
     ,{'say', wh_util:to_binary(New), ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-new_messages">>}
    ];
message_count_prompts(New, 1) ->
    [{'prompt', <<"vm-you_have">>}
     ,{'say', wh_util:to_binary(New), ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-new_and">>}
     ,{'say', <<"1">>, ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-saved_message">>}
    ];
message_count_prompts(0, Saved) ->
    [{'prompt', <<"vm-you_have">>}
     ,{'say', wh_util:to_binary(Saved), ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-saved_messages">>}
    ];
message_count_prompts(1, Saved) ->
    [{'prompt', <<"vm-you_have">>}
     ,{'say', <<"1">>, ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-new_and">>}
     ,{'say', wh_util:to_binary(Saved), ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-saved_messages">>}
    ];
message_count_prompts(New, Saved) ->
    [{'prompt', <<"vm-you_have">>}
     ,{'say', wh_util:to_binary(New), ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-new_and">>}
     ,{'say', wh_util:to_binary(Saved), ?VM_KEY_MESSAGES}
     ,{'prompt', <<"vm-saved_messages">>}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays back a message then the menu, and continues to loop over the
%% menu utill
%% @end
%%--------------------------------------------------------------------
-spec play_messages(wh_json:objects(), non_neg_integer(), mailbox(), whapps_call:call()) ->
                           'ok' | 'complete'.
play_messages(Messages, Count, Box, Call) ->
    play_messages(Messages, [], Count, Box, Call).

-spec play_messages(wh_json:objects(), wh_json:objects(), non_neg_integer(), mailbox(), whapps_call:call()) ->
                           'ok' | 'complete'.
play_messages([H|T]=Messages, PrevMessages, Count, #mailbox{timezone=Timezone
                                                           }=Box, Call) ->
    AccountId = whapps_call:account_id(Call),
    Message = kz_vm_message:media_url(AccountId, H),
    lager:info("playing mailbox message ~p (~s)", [Count, Message]),
    Prompt = [{'prompt', <<"vm-message_number">>}
              ,{'say', wh_util:to_binary(Count - length(Messages) + 1), <<"number">>}
              ,{'play', Message}
              ,{'prompt', <<"vm-received">>}
              ,{'say',  get_unix_epoch(wh_json:get_value(<<"timestamp">>, H), Timezone), <<"current_date_time">>}
              ,{'prompt', <<"vm-message_menu">>}
             ],
    case message_menu(Prompt, Box, Call) of
        {'ok', 'keep'} ->
            lager:info("caller chose to save the message"),
            _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
            _ = kz_vm_message:set_folder(?VM_FOLDER_SAVED, H, AccountId),
            play_messages(T, [H|PrevMessages], Count, Box, Call);
        {'ok', 'prev'} ->
            lager:info("caller chose to listen to previous message"),
            play_prev_message(Messages, PrevMessages, Count, Box, Call);
        {'ok', 'next'} ->
            lager:info("caller chose to listen to next message"),
            play_next_message(Messages, PrevMessages, Count, Box, Call);
        {'ok', 'delete'} ->
            lager:info("caller chose to delete the message"),
            _ = whapps_call_command:b_prompt(<<"vm-deleted">>, Call),
            _ = kz_vm_message:set_folder(?VM_FOLDER_DELETED, H, AccountId),
            play_messages(T, PrevMessages, Count, Box, Call);
        {'ok', 'return'} ->
            lager:info("caller chose to return to the main menu"),
            _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
            _ = kz_vm_message:set_folder(?VM_FOLDER_SAVED, H, AccountId),
            'complete';
        {'ok', 'replay'} ->
            lager:info("caller chose to replay"),
            play_messages(Messages, PrevMessages, Count, Box, Call);
        {'error', _} ->
            lager:info("error during message playback")
    end;
play_messages([], _, _, _, _) ->
    lager:info("all messages in folder played to caller"),
    'complete'.

-spec play_next_message(wh_json:objects(), wh_json:objects(), non_neg_integer(), mailbox(), whapps_call:call()) ->
                               'ok' | 'complete'.
play_next_message([_] = Messages, PrevMessages, Count, Box, Call) ->
    play_messages(Messages, PrevMessages, Count, Box, Call);
play_next_message([H|T], PrevMessages, Count, Box, Call) ->
    play_messages(T, [H|PrevMessages], Count, Box, Call).

-spec play_prev_message(wh_json:objects(), wh_json:objects(), non_neg_integer(), mailbox(), whapps_call:call()) ->
                               'ok' | 'complete'.
play_prev_message(Messages, [] = PrevMessages, Count, Box, Call) ->
    play_messages(Messages, PrevMessages, Count, Box, Call);
play_prev_message(Messages, [H|T], Count, Box, Call) ->
    play_messages([H|Messages], T, Count, Box, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loops over the message menu after the first play back util the
%% user provides a valid option
%% @end
%%--------------------------------------------------------------------
-type message_menu_returns() :: {'ok', 'keep' | 'delete' | 'return' | 'replay' | 'prev' | 'next'}.

-spec message_menu(mailbox(), whapps_call:call()) ->
                          {'error', 'channel_hungup' | 'channel_unbridge' | wh_json:object()} |
                          message_menu_returns().
-spec message_menu(whapps_call_command:audio_macro_prompts(), mailbox(), whapps_call:call()) ->
                          {'error', 'channel_hungup' | 'channel_unbridge' | wh_json:object()} |
                          message_menu_returns().
message_menu(Box, Call) ->
    message_menu([{'prompt', <<"vm-message_menu">>}], Box, Call).
message_menu(Prompt, #mailbox{keys=#keys{replay=Replay
                                         ,keep=Keep
                                         ,delete=Delete
                                         ,prev=Prev
                                         ,next=Next
                                         ,return_main=ReturnMain
                                        }
                              ,interdigit_timeout=Interdigit
                             }=Box, Call) ->
    lager:info("playing message menu"),
    NoopId = whapps_call_command:audio_macro(Prompt, Call),

    case whapps_call_command:collect_digits(?KEY_LENGTH
                                            ,whapps_call_command:default_collect_timeout()
                                            ,Interdigit
                                            ,NoopId
                                            ,Call
                                           )
    of
        {'ok', Keep} -> {'ok', 'keep'};
        {'ok', Delete} -> {'ok', 'delete'};
        {'ok', ReturnMain} -> {'ok', 'return'};
        {'ok', Replay} -> {'ok', 'replay'};
        {'ok', Prev} -> {'ok', 'prev'};
        {'ok', Next} -> {'ok', 'next'};
        {'error', _}=E -> E;
        _ -> message_menu(Box, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec config_menu(mailbox(), whapps_call:call()) ->
                         'ok' | mailbox() |
                         {'error', 'channel_hungup'}.
-spec config_menu(mailbox(), whapps_call:call(), pos_integer()) ->
                         'ok' | mailbox() |
                         {'error', 'channel_hungup'}.
config_menu(Box, Call) ->
    config_menu(Box, Call, 1).

config_menu(#mailbox{interdigit_timeout=Interdigit}=Box
            ,Call
            ,Loop
           ) when Loop < 4 ->
    lager:info("playing mailbox configuration menu"),
    {'ok', _} = whapps_call_command:b_flush(Call),

    NoopId = whapps_call_command:prompt(<<"vm-settings_menu">>, Call),

    case whapps_call_command:collect_digits(?KEY_LENGTH
                                            ,whapps_call_command:default_collect_timeout()
                                            ,Interdigit
                                            ,NoopId
                                            ,Call
                                           )
    of
        {'ok', Selection} ->
            handle_config_selection(Box, Call, Loop, Selection);
        {'error', _E} ->
            lager:info("failed to collect config menu selection: ~p", [_E])
    end.

-spec handle_config_selection(mailbox(), whapps_call:call(), pos_integer(), binary()) ->
                                     'ok' | mailbox() |
                                     {'error', 'channel_hungup'}.
handle_config_selection(#mailbox{keys=#keys{rec_unavailable=Selection}}=Box
                        ,Call
                        ,_Loop
                        ,Selection
                       ) ->
    lager:info("caller chose to record their unavailable greeting"),
    case record_unavailable_greeting(tmp_file(), Box, Call) of
        'ok' -> 'ok';
        Else -> config_menu(Else, Call)
    end;
handle_config_selection(#mailbox{keys=#keys{rec_name=Selection}}=Box
                        ,Call
                        ,_Loop
                        ,Selection
                       ) ->
    lager:info("caller chose to record their name"),
    case record_name(tmp_file(), Box, Call) of
        'ok' -> 'ok';
        Else -> config_menu(Else, Call)
    end;
handle_config_selection(#mailbox{keys=#keys{set_pin=Selection}}=Box
                        ,Call
                        ,_Loop
                        ,Selection
                       ) ->
    lager:info("caller chose to change their pin"),
    case change_pin(Box, Call) of
        {'error', 'channel_hungup'}=E ->
            lager:debug("channel has hungup, done trying to setup mailbox"),
            E;
        {'error', _E} ->
            lager:debug("changing pin failed: ~p", [_E]),
            config_menu(Box, Call);
        #mailbox{}=Box1 ->
            config_menu(Box1, Call)
    end;
handle_config_selection(#mailbox{keys=#keys{rec_temporary_unavailable=Selection}}=Box
                        ,Call
                        ,_Loop
                        ,Selection
                       ) ->
    lager:info("caller chose to record their temporary unavailable greeting"),
    case record_temporary_unavailable_greeting(tmp_file(), Box, Call) of
        'ok' -> 'ok';
        Box1 -> config_menu(Box1, Call)
    end;
handle_config_selection(#mailbox{keys=#keys{del_temporary_unavailable=Selection}}=Box
                        ,Call
                        ,_Loop
                        ,Selection
                       ) ->
    lager:info("caller chose to delete their temporary unavailable greeting"),
    case delete_temporary_unavailable_greeting(Box, Call) of
        'ok' -> 'ok';
        Box1 -> config_menu(Box1, Call)
    end;
handle_config_selection(#mailbox{keys=#keys{return_main=Selection}}=Box
                        ,_Call
                        ,_Loop
                        ,Selection
                       ) ->
    lager:info("caller chose to return to the main menu"),
    Box;
%% Bulk delete -> delete all voicemails
%% Reset -> delete all voicemails, greetings, name, and reset pin
handle_config_selection(#mailbox{}=Box
                        ,Call
                        ,Loop
                        ,_Selection
                       ) ->
    lager:info("undefined config menu option '~s'", [_Selection]),
    config_menu(Box, Call, Loop + 1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Recording the temporary greeting to override the common greeting
%% @end
%%--------------------------------------------------------------------
-spec record_temporary_unavailable_greeting(ne_binary(), mailbox(), whapps_call:call()) ->
                                                   'ok' | mailbox().
record_temporary_unavailable_greeting(AttachmentName
                                      ,#mailbox{temporary_unavailable_media_id='undefined'}=Box
                                      ,Call
                                     ) ->
    lager:info("no temporary greetings was recorded before so new media document should be created"),
    MediaId = recording_media_doc(<<"temporary unavailable greeting">>, Box, Call),
    record_temporary_unavailable_greeting(AttachmentName
                                          ,Box#mailbox{temporary_unavailable_media_id=MediaId}
                                          ,Call
                                         );
record_temporary_unavailable_greeting(AttachmentName, Box, Call) ->
    lager:info("record new temporary greetings use existing media document"),
    overwrite_temporary_unavailable_greeting(AttachmentName, Box, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Overwrites current media document of the temporary greeting
%% by a new recorded version.
%% @end
%%--------------------------------------------------------------------
-spec overwrite_temporary_unavailable_greeting(ne_binary(), mailbox(), whapps_call:call()) ->
                                                      'ok' | mailbox().
overwrite_temporary_unavailable_greeting(AttachmentName
                                         ,#mailbox{temporary_unavailable_media_id=MediaId}=Box
                                         ,Call
                                        ) ->
    lager:info("overwriting temporary unavailable greeting  as ~s", [AttachmentName]),
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    _NoopId = whapps_call_command:audio_macro(
                [{'prompt', <<"vm-record_temp_greeting">>}
                 ,{'tones', [Tone]}
                ]
                ,Call
               ),
    _ = whapps_call_command:b_record(AttachmentName, Call),
    case review_recording(AttachmentName, 'false', Box, Call) of
        {'ok', 'record'} ->
            lager:info("selected item: record new temporary greetings"),
            record_temporary_unavailable_greeting(tmp_file(), Box, Call);
        {'ok', 'save'} ->
            lager:info("selected item: store recorded temporary greetings"),
            _ = store_recording(AttachmentName, MediaId, Call),
            'ok' = update_doc([<<"media">>, <<"temporary_unavailable">>], MediaId, Box, Call),
            _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
            Box;
        {'ok', 'no_selection'} ->
            lager:info("selected item: no selection"),
            _ = whapps_call_command:b_prompt(<<"vm-deleted">>, Call),
            'ok';
        {'branch', _}=B -> B
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes current temporary greeting.
%% @end
%%--------------------------------------------------------------------
-spec delete_temporary_unavailable_greeting(mailbox(), whapps_call:call()) ->
                                                   'ok' | mailbox().
delete_temporary_unavailable_greeting(#mailbox{temporary_unavailable_media_id='undefined'}=_Box, _Call) ->
    'ok';
delete_temporary_unavailable_greeting(Box, Call) ->
    'ok' = update_doc([<<"media">>, <<"temporary_unavailable">>], 'undefined', Box, Call),
    Box#mailbox{temporary_unavailable_media_id='undefined'}.

-spec record_unavailable_greeting(ne_binary(), mailbox(), whapps_call:call()) ->
                                         'ok' | mailbox().
record_unavailable_greeting(AttachmentName, #mailbox{unavailable_media_id='undefined'}=Box, Call) ->
    MediaId = recording_media_doc(<<"unavailable greeting">>, Box, Call),
    record_unavailable_greeting(AttachmentName, Box#mailbox{unavailable_media_id=MediaId}, Call);
record_unavailable_greeting(AttachmentName, #mailbox{unavailable_media_id=MediaId}=Box, Call) ->
    case kz_datamgr:open_cache_doc(whapps_call:account_db(Call), MediaId) of
        {'ok', JObj} -> check_media_source(AttachmentName, Box, Call, JObj);
        _ -> overwrite_unavailable_greeting(AttachmentName, Box, Call)
    end.

-spec check_media_source(ne_binary(), mailbox(), whapps_call:call(), wh_json:object()) ->
                                'ok' | mailbox().
check_media_source(AttachmentName, Box, Call, JObj) ->
    case wh_json:get_value(<<"media_source">>, JObj) of
        <<"upload">> ->
            lager:debug("The voicemail greeting media is a web upload, let's not touch it,"
                        ++ " it may be in use in some other maibox. We create new media document."
                       ),
            record_unavailable_greeting(AttachmentName, Box#mailbox{unavailable_media_id='undefined'}, Call);
        _ ->
            overwrite_unavailable_greeting(AttachmentName, Box, Call)
    end.

-spec overwrite_unavailable_greeting(ne_binary(), mailbox(), whapps_call:call()) ->
                                            'ok' | mailbox().
overwrite_unavailable_greeting(AttachmentName, #mailbox{unavailable_media_id=MediaId}=Box, Call) ->
    lager:info("overwriting unavailable greeting  as ~s", [AttachmentName]),
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    _NoopId = whapps_call_command:audio_macro([{'prompt', <<"vm-record_greeting">>}
                                               ,{'tones', [Tone]}
                                              ]
                                              ,Call
                                             ),
    _ = whapps_call_command:b_record(AttachmentName, Call),
    case review_recording(AttachmentName, 'false', Box, Call) of
        {'ok', 'record'} ->
            record_unavailable_greeting(tmp_file(), Box, Call);
        {'ok', 'save'} ->
            _ = store_recording(AttachmentName, MediaId, Call),
            'ok' = update_doc([<<"media">>, <<"unavailable">>], MediaId, Box, Call),
            _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
            Box;
        {'ok', 'no_selection'} ->
            _ = whapps_call_command:b_prompt(<<"vm-deleted">>, Call),
            'ok';
        {'branch', _}=B -> B
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_name(ne_binary(), mailbox(), whapps_call:call()) ->
                         'ok' | mailbox().
-spec record_name(ne_binary(), mailbox(), whapps_call:call(), ne_binary()) ->
                         'ok' | mailbox().
record_name(AttachmentName, #mailbox{owner_id='undefined'
                                     ,name_media_id='undefined'
                                    }=Box, Call) ->
    lager:info("no recorded name media id nor owner id"),
    MediaId = recording_media_doc(<<"users name">>, Box, Call),
    lager:info("created recorded name media doc: ~s", [MediaId]),
    record_name(AttachmentName, Box#mailbox{name_media_id=MediaId}, Call);
record_name(AttachmentName, #mailbox{owner_id='undefined'
                                     ,mailbox_id=BoxId
                                    }=Box, Call) ->
    lager:info("no owner_id set on mailbox, saving recorded name id into mailbox"),
    record_name(AttachmentName, Box, Call, BoxId);
record_name(AttachmentName, #mailbox{owner_id=OwnerId
                                     ,name_media_id='undefined'
                                    }=Box, Call) ->
    lager:info("no recorded name media id for owner"),
    MediaId = recording_media_doc(<<"users name">>, Box, Call),
    lager:info("created recorded name media doc: ~s", [MediaId]),
    record_name(AttachmentName, Box#mailbox{name_media_id=MediaId}, Call, OwnerId);
record_name(AttachmentName, #mailbox{owner_id=OwnerId}=Box, Call) ->
    lager:info("owner_id (~s) set on mailbox, saving into owner's doc", [OwnerId]),
    record_name(AttachmentName, Box, Call, OwnerId).

record_name(AttachmentName, #mailbox{name_media_id=MediaId}=Box, Call, DocId) ->
    lager:info("recording name as ~s in ~s", [AttachmentName, MediaId]),
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                              ,{<<"Duration-ON">>, <<"500">>}
                              ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    _NoopId = whapps_call_command:audio_macro([{'prompt',  <<"vm-record_name">>}
                                               ,{'tones', [Tone]}
                                              ], Call),
    _ = whapps_call_command:b_record(AttachmentName, Call),
    case review_recording(AttachmentName, 'false', Box, Call) of
        {'ok', 'record'} ->
            record_name(tmp_file(), Box, Call);
        {'ok', 'save'} ->
            _ = store_recording(AttachmentName, MediaId, Call),
            'ok' = update_doc(?RECORDED_NAME_KEY, MediaId, DocId, Call),
            _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
            Box;
        {'ok', 'no_selection'} ->
            _ = whapps_call_command:b_prompt(<<"vm-deleted">>, Call),
            'ok';
        {'branch', _}=B -> B
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec change_pin(mailbox(), whapps_call:call()) ->
                        mailbox() | {'error', any()}.
change_pin(#mailbox{mailbox_id=Id
                    ,interdigit_timeout=Interdigit
                   }=Box
           ,Call
          ) ->
    lager:info("requesting new mailbox pin number"),
    try
        {'ok', Pin} = get_new_pin(Interdigit, Call),
        lager:info("collected first pin"),

        {'ok', Pin} = confirm_new_pin(Interdigit, Call),
        lager:info("collected second pin"),

        if byte_size(Pin) == 0 -> throw('pin_empty'); 'true' -> 'ok' end,
        lager:info("entered pin is not empty"),

        AccountDb = whapps_call:account_db(Call),

        {'ok', JObj} = kz_datamgr:open_cache_doc(AccountDb, Id),

        case validate_box_schema(wh_json:set_value(<<"pin">>, Pin, JObj)) of
            {'ok', PublicJObj} ->
                PrivJObj = wh_json:private_fields(JObj),

                JObj1 = wh_json:merge_jobjs(PrivJObj, PublicJObj),

                {'ok', _} = kz_datamgr:save_doc(AccountDb, JObj1),
                {'ok', _} = whapps_call_command:b_prompt(<<"vm-pin_set">>, Call),
                lager:info("updated mailbox pin number"),
                Box;
            {'error', _Reason} ->
                lager:debug("box failed validation: ~p", [_Reason]),
                invalid_pin(Box, Call)
        end
    catch
        'error':{'badmatch',{'error','channel_hungup'}} ->
            lager:debug("channel hungup while configuring pin"),
            {'error', 'channel_hungup'};
        'error':{'badmatch',{'ok',_ConfirmPin}} ->
            lager:debug("new pin was invalid, try again"),
            invalid_pin(Box, Call);
        _E:_R ->
            lager:debug("failed to get new pin: ~s: ~p", [_E, _R]),
            invalid_pin(Box, Call)
    end.

-spec invalid_pin(mailbox(), whapps_call:call()) ->
                         mailbox() |
                         {'error', any()}.
invalid_pin(Box, Call) ->
    case whapps_call_command:b_prompt(<<"vm-pin_invalid">>, Call) of
        {'ok', _} -> change_pin(Box, Call);
        {'error', 'channel_hungup'}=E ->
            lager:debug("channel hungup after bad pin"),
            E;
        {'error', _E}=E ->
            lager:debug("invalid pin prompt interrupted: ~p", [_E]),
            E
    end.

-spec validate_box_schema(wh_json:object()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', any()}.
validate_box_schema(JObj) ->
    {'ok', Schema} = wh_json_schema:load(<<"vmboxes">>),
    case jesse:validate_with_schema(Schema, wh_json:public_fields(JObj)) of
        {'ok', _}=OK -> OK;
        {'error', _Errors} ->
            lager:debug("failed to validate vmbox schema: ~p", [_Errors]),
            {'error', 'invalid_pin'}
    end.

-spec get_new_pin(pos_integer(), whapps_call:call()) ->
                         {'ok', binary()} |
                         {'error', any()}.
get_new_pin(Interdigit, Call) ->
    NoopId = whapps_call_command:prompt(<<"vm-enter_new_pin">>, Call),
    collect_pin(Interdigit, Call, NoopId).

-spec confirm_new_pin(pos_integer(), whapps_call:call()) ->
                             {'ok', binary()} |
                             {'error', any()}.
confirm_new_pin(Interdigit, Call) ->
    NoopId = whapps_call_command:prompt(<<"vm-enter_new_pin_confirm">>, Call),
    collect_pin(Interdigit, Call, NoopId).

-spec collect_pin(pos_integer(), whapps_call:call(), ne_binary()) ->
                         {'ok', binary()} |
                         {'error', any()}.
collect_pin(Interdigit, Call, NoopId) ->
    whapps_call_command:collect_digits(?DEFAULT_MAX_PIN_LENGTH
                                       ,whapps_call_command:default_collect_timeout()
                                       ,Interdigit
                                       ,NoopId
                                       ,Call
                                      ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new_message(ne_binary(), pos_integer(), mailbox(), whapps_call:call()) -> any().
new_message(AttachmentName, Length, #mailbox{mailbox_number=BoxNum
                                             ,mailbox_id=BoxId
                                             ,timezone=Timezone
                                             ,owner_id=OwnerId
                                             ,transcribe_voicemail=MaybeTranscribe
                                             ,after_notify_action=Action
                                            }=Box, Call) ->
    AccountId = whapps_call:account_id(Call),
    NewMsgProps = [{{<<"Box-Id">>, BoxId}
                   ,{<<"OwnerId">>, OwnerId}
                   ,{<<"Length">>, Length}
                   ,{<<"Transcribe-Voicemail">>, MaybeTranscribe}
                   ,{<<"After-Notify-Action">>, Action}
                   ,{<<"Default-External-Storage">>, ?MAILBOX_DEFAULT_STORAGE}
                   ,{<<"Default-Extension">>, ?DEFAULT_VM_EXTENSION}
                   ,{<<"Retry-Storage-Times">>, ?MAILBOX_RETRY_STORAGE_TIMES(AccountId)}
                   ,{<<"Retry-Local-Storage">>, ?MAILBOX_RETRY_LOCAL_STORAGE_REMOTE_FAILS(AccountId)}
                   ,{<<"Defualt-Min-MSG-Length">>, min_recording_length(Call)}
                  ],
    % case store_recording(AttachmentName, MediaId, Call, Box, ?MAILBOX_DEFAULT_STORAGE) of
    case kz_vm_message:new_message(AttachmentName, BoxNum, Timezone, Call, NewMsgProps) of
        'ok' -> send_mwi_update(Box, Call);
                % update_mailbox(Box, Call, MediaId, Length);
        {'error', Call1, Msg} ->
            system_report(Msg, Call1)
    end.

-spec system_report(text(), whapps_call:call()) -> 'ok'.
system_report(Msg, Call) ->
    Notify = props:filter_undefined(
               [{<<"Subject">>, <<"failed to store voicemail recorded media">>}
                ,{<<"Message">>, iolist_to_binary(Msg)}
                ,{<<"Details">>, whapps_call:to_json(Call)}
                ,{<<"Account-ID">>, whapps_call:account_id(Call)}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    wh_amqp_worker:cast(Notify, fun wapi_notifications:publish_system_alert/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the mailbox parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec get_mailbox_profile(wh_json:object(), whapps_call:call()) -> mailbox().
get_mailbox_profile(Data, Call) ->
    Id = wh_doc:id(Data),
    AccountDb = whapps_call:account_db(Call),

    case get_mailbox_doc(AccountDb, Id, Data, Call) of
        {'ok', MailboxJObj} ->
            MailboxId = wh_doc:id(MailboxJObj),
            lager:info("loaded voicemail box ~s", [MailboxId]),
            Default = #mailbox{},

            %% dont check if the voicemail box belongs to the owner (by default) if the call was not
            %% specificly to him, IE: calling a ring group and going to voicemail should not check
            LastAct = whapps_call:kvs_fetch('cf_last_action', Call),
            CheckIfOwner = (('undefined' =:= LastAct)
                            orelse ('cf_device' =:= LastAct)
                           ),

            {NameMediaId, OwnerId} = owner_info(AccountDb, MailboxJObj),

            MaxMessageCount = max_message_count(Call),
            MsgCount = kz_vm_message:count_all(whapps_call:account_id(Call), Id),

            lager:info("mailbox limited to ~p voicemail messages (has ~b currently)"
                       ,[MaxMessageCount, MsgCount]
                      ),

            AfterNotifyAction = after_notify_action(MailboxJObj),

            #mailbox{mailbox_id = MailboxId
                     ,exists = 'true'
                     ,keys = populate_keys(Call)
                     ,skip_instructions =
                         kzd_voicemail_box:skip_instructions(MailboxJObj, Default#mailbox.skip_instructions)
                     ,skip_greeting =
                         kzd_voicemail_box:skip_greeting(MailboxJObj, Default#mailbox.skip_greeting)
                     ,pin =
                         kzd_voicemail_box:pin(MailboxJObj, <<>>)
                     ,timezone =
                         kzd_voicemail_box:timezone(MailboxJObj, ?DEFAULT_TIMEZONE)
                     ,mailbox_number =
                         kzd_voicemail_box:mailbox_number(MailboxJObj, whapps_call:request_user(Call))
                     ,require_pin =
                         kzd_voicemail_box:pin_required(MailboxJObj)
                     ,check_if_owner =
                         kzd_voicemail_box:check_if_owner(MailboxJObj, CheckIfOwner)
                     ,unavailable_media_id =
                         wh_json:get_ne_value([<<"media">>, <<"unavailable">>], MailboxJObj)
                     ,temporary_unavailable_media_id =
                         wh_json:get_ne_value([<<"media">>, <<"temporary_unavailable">>], MailboxJObj)
                     ,name_media_id =
                         NameMediaId
                     ,owner_id =
                         OwnerId
                     ,is_setup =
                         kzd_voicemail_box:is_setup(MailboxJObj, 'false')
                     ,max_message_count =
                         wh_util:to_integer(MaxMessageCount)
                     ,max_message_length =
                         find_max_message_length([Data, MailboxJObj])
                     ,message_count =
                         MsgCount
                     ,transcribe_voicemail =
                         wh_json:is_true(<<"transcribe">>, MailboxJObj, 'false')
                     ,notifications =
                         wh_json:get_value(<<"notifications">>, MailboxJObj)
                     ,after_notify_action = AfterNotifyAction
                     ,interdigit_timeout =
                         wh_json:find(<<"interdigit_timeout">>, [MailboxJObj, Data], whapps_call_command:default_interdigit_timeout())
                     ,play_greeting_intro =
                         wh_json:is_true(<<"play_greeting_intro">>, MailboxJObj, Default#mailbox.play_greeting_intro)
                     ,use_person_not_available =
                         wh_json:is_true(<<"use_person_not_available">>, MailboxJObj, Default#mailbox.use_person_not_available)
                     ,not_configurable=
                         wh_json:is_true(<<"not_configurable">>, MailboxJObj, 'false')
                     ,account_db = AccountDb
                    };
        {'error', R} ->
            lager:info("failed to load voicemail box ~s, ~p", [Id, R]),
            #mailbox{}
    end.

-spec after_notify_action(wh_json:object()) -> atom().
after_notify_action(MailboxJObj) ->
    Delete = wh_json:is_true(?KEY_DELETE_AFTER_NOTIFY, MailboxJObj, ?DEFAULT_DELETE_AFTER_NOTIFY),
    Save   = wh_json:is_true(?KEY_SAVE_AFTER_NOTIFY, MailboxJObj, ?DEFAULT_SAVE_AFTER_NOTIFY),

    case {Delete, Save} of
        {'false', 'false'} -> 'nothing';
        {'false', 'true'}  -> 'save';
        {'true', 'false'}  -> 'delete';
        {'true', 'true'}   -> 'save'
    end.

-spec max_message_count(whapps_call:call()) -> non_neg_integer().
max_message_count(Call) ->
    case whapps_account_config:get(whapps_call:account_id(Call)
                                   ,?CF_CONFIG_CAT
                                   ,[?KEY_VOICEMAIL, ?KEY_MAX_MESSAGE_COUNT]
                                  )
    of
        'undefined' -> ?MAILBOX_DEFAULT_SIZE;
        MMC -> MMC
    end.

-spec owner_info(ne_binary(), wh_json:object()) ->
                        {api_binary(), api_binary()}.
-spec owner_info(ne_binary(), wh_json:object(), api_binary()) ->
                        {api_binary(), api_binary()}.
owner_info(AccountDb, MailboxJObj) ->
    owner_info(AccountDb, MailboxJObj, wh_json:get_ne_value(<<"owner_id">>, MailboxJObj)).

owner_info(_AccountDb, MailboxJObj, 'undefined') ->
    {wh_json:get_ne_value(?RECORDED_NAME_KEY, MailboxJObj)
     ,'undefined'
    };
owner_info(AccountDb, MailboxJObj, OwnerId) ->
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', OwnerJObj} ->
            {wh_json:find(?RECORDED_NAME_KEY, [OwnerJObj, MailboxJObj]), OwnerId};
        {'error', 'not_found'} ->
            lager:info("owner ~s no longer exists", [OwnerId]),
            {wh_json:get_ne_value(?RECORDED_NAME_KEY, MailboxJObj), 'undefined'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec populate_keys(whapps_call:call()) -> vm_keys().
populate_keys(Call) ->
    Default = #keys{},
    JObj = whapps_account_config:get(whapps_call:account_id(Call), <<"keys">>),
    #keys{operator = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"operator">>], JObj, Default#keys.operator)
          ,login = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"login">>], JObj, Default#keys.login)
          ,save = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"save">>], JObj, Default#keys.save)
          ,listen = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"listen">>], JObj, Default#keys.listen)
          ,record = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"record">>], JObj, Default#keys.record)
          ,hear_new = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"hear_new">>], JObj, Default#keys.hear_new)
          ,hear_saved = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"hear_saved">>], JObj, Default#keys.hear_saved)
          ,configure = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"configure">>], JObj, Default#keys.configure)
          ,exit = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"exit">>], JObj, Default#keys.exit)
          ,rec_unavailable = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"record_unavailable">>], JObj, Default#keys.rec_unavailable)
          ,rec_name = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"record_name">>], JObj, Default#keys.rec_name)
          ,set_pin = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"set_pin">>], JObj, Default#keys.set_pin)
          ,return_main = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"return_main_menu">>], JObj, Default#keys.return_main)
          ,keep = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"keep">>], JObj, Default#keys.keep)
          ,replay = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"replay">>], JObj, Default#keys.replay)
          ,prev = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"prev">>], JObj, Default#keys.prev)
          ,next = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"next">>], JObj, Default#keys.next)
          ,delete = wh_json:get_binary_value([?KEY_VOICEMAIL, <<"delete">>], JObj, Default#keys.delete)
         }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_mailbox_doc(ne_binary(), api_binary(), wh_json:object(), whapps_call:call()) ->
                             {'ok', wh_json:object()} |
                             {'error', any()}.
get_mailbox_doc(Db, Id, Data, Call) ->
    CaptureGroup = whapps_call:kvs_fetch('cf_capture_group', Call),
    CGIsEmpty = wh_util:is_empty(CaptureGroup),
    case wh_util:is_empty(Id) of
        'false' ->
            lager:info("opening ~s", [Id]),
            kz_datamgr:open_doc(Db, Id);
        'true' when not CGIsEmpty ->
            lager:info("capture group not empty: ~s", [CaptureGroup]),
            Opts = [{'key', CaptureGroup}, 'include_docs'],
            case kz_datamgr:get_results(Db, <<"cf_attributes/mailbox_number">>, Opts) of
                {'ok', []} -> {'error', 'not_found'};
                {'ok', [JObj|_]} -> {'ok', wh_json:get_value(<<"doc">>, JObj, wh_json:new())};
                Else -> Else
            end;
        'true' ->
            get_user_mailbox_doc(Data, Call)
    end.

-spec get_user_mailbox_doc(wh_json:object(), whapps_call:call()) ->
                                  {'ok', wh_json:object()} |
                                  {'error', any()}.
-spec get_user_mailbox_doc(wh_json:object(), whapps_call:call(), api_binary()) ->
                                  {'ok', wh_json:object()} |
                                  {'error', any()}.
get_user_mailbox_doc(Data, Call) ->
    get_user_mailbox_doc(Data, Call, whapps_call:owner_id(Call)).

get_user_mailbox_doc(Data, Call, 'undefined') ->
    DeviceId = whapps_call:authorizing_id(Call),
    case kz_datamgr:open_cache_doc(whapps_call:account_db(Call), DeviceId) of
        {'ok', DeviceJObj} ->
            case wh_json:get_value(<<"owner_id">>, DeviceJObj) of
                'undefined' ->
                    lager:debug("device used to check voicemail has no owner assigned", []),
                    {'error', "request voicemail box number"};
                OwnerId ->
                    get_user_mailbox_doc(Data, Call, OwnerId)
            end;
        {'error', _} ->
            lager:debug("unknown device used to check voicemail", []),
            {'error', "request voicemail box number"}
    end;
get_user_mailbox_doc(Data, Call, OwnerId) ->
    SingleMailboxLogin = wh_json:is_true(<<"single_mailbox_login">>, Data, 'false'),
    case cf_attributes:owned_by_docs(OwnerId, <<"vmbox">>, Call) of
        [] ->
            lager:debug("owner ~s has no vmboxes", [OwnerId]),
            {'error', "request voicemail box number"};
        [Box] when SingleMailboxLogin ->
            lager:debug("owner ~s has one vmbox ~s, and single mailbox login is enabled"
                       ,[OwnerId, wh_doc:id(Box)]
                       ),
            {'ok', Box};
        Boxes ->
            lager:debug("found ~p vmboxes assigned to owner ~s",
                        [length(Boxes), OwnerId]),
            maybe_match_callerid(Boxes,Data, Call)
    end.

-spec maybe_match_callerid(wh_json:objects(), wh_json:object(), whapps_call:call()) ->
                                  {'ok', wh_json:object()} |
                                  {'error', any()}.
maybe_match_callerid(Boxes, Data, Call) ->
    case wh_json:is_true(<<"callerid_match_login">>, Data, 'false') of
        'false' ->
            lager:debug("found voicemail boxes but caller-id match disabled", []),
            {'error', "request voicemail box number"};
        'true' ->
            CallerId = whapps_call:caller_id_number(Call),
            try_match_callerid(Boxes, CallerId)
    end.

-spec try_match_callerid(wh_json:objects(), ne_binary()) ->
                                {'ok', wh_json:object()} |
                                {'error', any()}.
try_match_callerid([], _CallerId) ->
    lager:debug("no voicemail box found for owner with matching caller id ~s", [_CallerId]),
    {'error', "request voicemail box number"};
try_match_callerid([Box|Boxes], CallerId) ->
    case wh_json:get_value(<<"mailbox">>, Box) of
        CallerId ->
            lager:debug("found mailbox from caller id ~s", [CallerId]),
            {'ok', Box};
        _Mailbox ->
            try_match_callerid(Boxes, CallerId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec review_recording(ne_binary(), boolean(), mailbox(), whapps_call:call()) ->
                              {'ok', 'record' | 'save' | 'no_selection'} |
                              {'branch', wh_json:object()}.
-spec review_recording(ne_binary(), boolean(), mailbox(), whapps_call:call(), integer()) ->
                              {'ok', 'record' | 'save' | 'no_selection'} |
                              {'branch', wh_json:object()}.

review_recording(AttachmentName, AllowOperator, Box, Call) ->
    review_recording(AttachmentName, AllowOperator, Box, Call, 1).

review_recording(_, _, _, _, Loop) when Loop > 4 ->
    {'ok', 'no_selection'};
review_recording(AttachmentName, AllowOperator
                 ,#mailbox{keys=#keys{listen=Listen
                                      ,save=Save
                                      ,record=Record
                                      ,operator=Operator
                                     }
                           ,interdigit_timeout=Interdigit
                          }=Box
                 ,Call, Loop) ->
    lager:info("playing recording review options"),

    NoopId = whapps_call_command:prompt(<<"vm-review_recording">>, Call),
    case whapps_call_command:collect_digits(?KEY_LENGTH
                                            ,whapps_call_command:default_collect_timeout()
                                            ,Interdigit
                                            ,NoopId
                                            ,Call
                                           )
    of
        {'ok', Listen} ->
            lager:info("caller chose to replay the recording"),
            _ = whapps_call_command:b_play(AttachmentName, Call),
            review_recording(AttachmentName, AllowOperator, Box, Call);
        {'ok', Record} ->
            lager:info("caller chose to re-record"),
            {'ok', 'record'};
        {'ok', Save} ->
            lager:info("caller chose to save the recording"),
            {'ok', 'save'};
        {'ok', Operator} when AllowOperator ->
            lager:info("caller chose to ring the operator"),
            case cf_util:get_operator_callflow(whapps_call:account_id(Call)) of
                {'ok', Flow} -> {'branch', Flow};
                {'error',_R} -> review_recording(AttachmentName, AllowOperator, Box, Call, Loop + 1)
            end;
        {'error', 'channel_hungup'} ->
            {'ok', 'no_selection'};
        {'error', _E} ->
            lager:info("error while waiting for review selection ~p", [_E]),
            {'ok', 'no_selection'};
        _ ->
            review_recording(AttachmentName, AllowOperator, Box, Call, Loop + 1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording(ne_binary(), ne_binary(), whapps_call:call()) -> boolean() | 'error'.
store_recording(AttachmentName, DocId, Call) ->
    lager:debug("storing recording ~s in doc ~s", [AttachmentName, DocId]),
    Fun = fun() -> get_new_attachment_url(AttachmentName, DocId, Call) end,
    case try_store_recording(AttachmentName, DocId, Fun, Call) of
        'ok' ->
            check_attachment_length(AttachmentName, DocId, Call);
        {'error', _}=Err -> Err
    end.

-spec check_attachment_length(ne_binary(), ne_binary(), whapps_call:call()) ->
                                     boolean() |
                                     {'error', whapps_call:call()}.
check_attachment_length(AttachmentName, DocId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    MinLength = min_recording_length(Call),

    case kz_datamgr:open_doc(AccountDb, DocId) of
        {'ok', JObj} ->
            case wh_doc:attachment_length(JObj, AttachmentName) of
                'undefined' ->
                    Err = io_lib:format("attachment ~s is missing from doc ~s", [AttachmentName, DocId]),
                    lager:debug(Err),
                    {'error', whapps_call:kvs_store('error_details', {'error', Err}, Call)};
                AttachmentLength ->
                    lager:info("attachment length is ~B and must be larger than ~B to be stored", [AttachmentLength, MinLength]),
                    is_integer(AttachmentLength) andalso AttachmentLength > MinLength
            end;
        {'error', _}=Err ->
            {'error', whapps_call:kvs_store('error_details', Err, Call) }
    end.

-spec try_store_recording(ne_binary(), ne_binary(), ne_binary(), whapps_call:call()) ->
                                 'ok' | {'error', whapps_call:call()}.
-spec try_store_recording(ne_binary(), ne_binary(), ne_binary(), integer(), whapps_call:call()) ->
                                 'ok' | {'error', whapps_call:call()}.
try_store_recording(AttachmentName, DocId, Url, Call) ->
    Tries = ?MAILBOX_RETRY_STORAGE_TIMES(whapps_call:account_id(Call)),
    Funs = [{fun whapps_call:kvs_store/3, 'media_url', Url}],
    try_store_recording(AttachmentName, DocId, Url, Tries, whapps_call:exec(Funs, Call)).

try_store_recording(_, _, _, 0, Call) -> {'error', Call};
try_store_recording(AttachmentName, DocId, Url, Tries, Call) ->
    case whapps_call_command:b_store_vm(AttachmentName, Url, <<"put">>, [wh_json:new()], 'true', Call) of
        {'ok', JObj} ->
            verify_stored_recording(AttachmentName, DocId, Url, Tries, Call, JObj);
        Other ->
            lager:error("error trying to store voicemail media, retrying ~B more times", [Tries - 1]),
            retry_store(AttachmentName, DocId, Url, Tries, Call, Other)
    end.

-spec retry_store(ne_binary(), ne_binary(), ne_binary(), pos_integer(), whapps_call:call(), any()) ->
                         'ok' | {'error', whapps_call:call()}.
retry_store(AttachmentName, DocId, Url, Tries, Call, Error) ->
    timer:sleep(2000),
    Call1 = whapps_call:kvs_store('error_details', Error, Call),
    try_store_recording(AttachmentName, DocId, Url, Tries - 1, Call1).

-spec verify_stored_recording(ne_binary(), ne_binary(), ne_binary(), pos_integer(), whapps_call:call(), wh_json:object()) ->
                                     'ok' |
                                     {'error', whapps_call:call()}.
verify_stored_recording(AttachmentName, DocId, Url, Tries, Call, JObj) ->
    case wh_json:get_value(<<"Application-Response">>, JObj) of
        <<"success">> ->
            lager:debug("storing ~s into ~s was successful", [AttachmentName, DocId]);
        _Response ->
            case check_attachment_length(AttachmentName, DocId, Call) of
                'true' ->
                    lager:debug("attachment ~s exists on ~s, saved!", [AttachmentName, DocId]);
                'false' ->
                    lager:debug("attachment ~s isn't on ~s, retry necessary", [AttachmentName, DocId]),
                    retry_store(AttachmentName, DocId, Url, Tries, Call, JObj);
                {'error', Call1} ->
                    lager:debug("error fetching ~s, will retry store", [DocId]),
                    retry_store(AttachmentName, DocId, Url, Tries, Call1, JObj)
            end
    end.

-spec min_recording_length(whapps_call:call()) -> integer().
min_recording_length(Call) ->
    case whapps_account_config:get(whapps_call:account_id(Call)
                                   ,?CF_CONFIG_CAT
                                   ,[?KEY_VOICEMAIL, ?KEY_MIN_MESSAGE_SIZE]
                                  )
    of
        'undefined' -> ?MAILBOX_DEFAULT_MSG_MIN_LENGTH;
        MML -> wh_util:to_integer(MML)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_new_attachment_url(ne_binary(), ne_binary(), whapps_call:call()) -> ne_binary().
get_new_attachment_url(AttachmentName, MediaId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    _ = case kz_datamgr:open_doc(AccountDb, MediaId) of
            {'ok', JObj} ->
                maybe_remove_attachments(AccountDb, MediaId, JObj);
            {'error', _} -> 'ok'
        end,
%%    {'ok', URL} = wh_media_url:store(AccountDb, MediaId, AttachmentName),
%%    URL.
    kz_datamgr:attachment_url(AccountDb, MediaId, AttachmentName, [{'doc_type', <<"voicemail">>}]).

-spec maybe_remove_attachments(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_remove_attachments(AccountDb, MediaId, JObj) ->
    case wh_doc:maybe_remove_attachments(JObj) of
        {'false', _} -> 'ok';
        {'true', Removed} ->
            kz_datamgr:save_doc(AccountDb, Removed),
            lager:debug("doc ~s has existing attachments, removing", [MediaId])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec recording_media_doc(ne_binary(), mailbox(), whapps_call:call()) -> ne_binary().
recording_media_doc(Recording, #mailbox{mailbox_number=BoxNum
                                        ,mailbox_id=Id
                                        ,owner_id=OwnerId
                                       }, Call) ->
    AccountDb = whapps_call:account_db(Call),
    Name = list_to_binary(["mailbox ", BoxNum, " ", Recording]),
    Props = props:filter_undefined(
              [{<<"name">>, Name}
               ,{<<"description">>, <<"voicemail recorded/prompt media">>}
               ,{<<"source_type">>, ?KEY_VOICEMAIL}
               ,{<<"source_id">>, Id}
               ,{<<"owner_id">>, OwnerId}
               ,{<<"media_source">>, <<"recording">>}
               ,{<<"streamable">>, 'true'}
              ]),
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), AccountDb, [{'type', <<"media">>}]),
    {'ok', JObj} = kz_datamgr:save_doc(AccountDb, Doc),
    wh_doc:id(JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_doc(wh_json:key() | wh_json:keys()
                 ,wh_json:json_term()
                 ,mailbox() | ne_binary()
                 ,whapps_call:call() | ne_binary()
                ) ->
                        'ok' |
                        {'error', atom()}.
update_doc(Key, Value, #mailbox{mailbox_id=Id}, Db) ->
    update_doc(Key, Value, Id, Db);
update_doc(Key, Value, Id, ?NE_BINARY = Db) ->
    case kz_datamgr:open_doc(Db, Id) of
        {'ok', JObj} ->
            case kz_datamgr:save_doc(Db, wh_json:set_value(Key, Value, JObj)) of
                {'error', 'conflict'} ->
                    update_doc(Key, Value, Id, Db);
                {'ok', _} -> 'ok';
                {'error', R}=E ->
                    lager:info("unable to update ~s in ~s, ~p", [Id, Db, R]),
                    E
            end;
        {'error', R}=E ->
            lager:info("unable to update ~s in ~s, ~p", [Id, Db, R]),
            E
    end;
update_doc(Key, Value, Id, Call) ->
    update_doc(Key, Value, Id, whapps_call:account_db(Call)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tmp_file() -> ne_binary().
tmp_file() ->
    Ext = ?DEFAULT_VM_EXTENSION,
    <<(wh_util:to_hex_binary(crypto:rand_bytes(16)))/binary, ".", Ext/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Accepts Universal Coordinated Time (UTC) and convert it to binary
%% encoded Unix epoch in the provided timezone
%% @end
%%--------------------------------------------------------------------
-spec get_unix_epoch(ne_binary(), ne_binary()) -> ne_binary().
get_unix_epoch(Epoch, Timezone) ->
    UtcDateTime = calendar:gregorian_seconds_to_datetime(wh_util:to_integer(Epoch)),
    LocalDateTime = localtime:utc_to_local(UtcDateTime, Timezone),
    wh_util:to_binary(calendar:datetime_to_gregorian_seconds(LocalDateTime) - ?UNIX_EPOCH_IN_GREGORIAN).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_max_message_length(wh_json:objects()) -> pos_integer().
find_max_message_length([]) -> ?MAILBOX_DEFAULT_MSG_MAX_LENGTH;
find_max_message_length([JObj | T]) ->
    case wh_json:get_integer_value(?KEY_MAX_MESSAGE_LENGTH, JObj) of
        Len when is_integer(Len) andalso Len > 0 -> Len;
        _ -> find_max_message_length(T)
    end.

-spec is_owner(whapps_call:call(), ne_binary()) -> boolean().
is_owner(Call, OwnerId) ->
    case whapps_call:owner_id(Call) of
        <<>> -> 'false';
        'undefined' -> 'false';
        OwnerId -> 'true';
        _Else -> 'false'
    end.

-spec send_mwi_update(mailbox(), whapps_call:call()) -> 'ok'.
send_mwi_update(#mailbox{owner_id=OwnerId
                         ,mailbox_number=BoxNumber
                         ,account_db=AccountDb
                         ,mailbox_id=BoxId}, Call) ->
    _ = wh_util:spawn(fun cf_util:unsolicited_owner_mwi_update/2, [AccountDb, OwnerId]),
    Messages = kz_vm_message:messages(whapps_call:account_id(Call), BoxId),
    New = kzd_voice_message:count_messages(Messages, ?VM_FOLDER_NEW),
    Saved = kzd_voice_message:count_messages(Messages, ?VM_FOLDER_SAVED),
    _ = wh_util:spawn(fun send_mwi_update/4, [New, Saved, BoxNumber, Call]),
    lager:debug("sent MWI updates for vmbox ~s in account ~s (~b/~b)", [BoxNumber, whapps_call:account_id(Call), New, Saved]).

-spec send_mwi_update(non_neg_integer(), non_neg_integer(), ne_binary(), whapps_call:call()) -> 'ok'.
send_mwi_update(New, Saved, BoxNumber, Call) ->
    Realm = whapps_call:account_realm(Call),
    Command = [{<<"To">>, <<BoxNumber/binary, "@", Realm/binary>>}
               ,{<<"Messages-New">>, New}
               ,{<<"Messages-Saved">>, Saved}
               ,{<<"Call-ID">>, whapps_call:call_id(Call)}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("updating MWI for vmbox ~s@~s (~b/~b)", [BoxNumber, Realm, New, Saved]),
    wh_amqp_worker:cast(Command, fun wapi_presence:publish_mwi_update/1).
