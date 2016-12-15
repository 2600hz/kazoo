%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
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

-behaviour(gen_cf_action).

-include("callflow.hrl").
-include_lib("kazoo_json/include/kazoo_json.hrl").

-export([handle/2]).
-export([new_message/4]).

-define(KEY_VOICEMAIL, <<"voicemail">>).
-define(KEY_MAX_MESSAGE_COUNT, <<"max_message_count">>).
-define(KEY_MAX_MESSAGE_LENGTH, <<"max_message_length">>).
-define(KEY_MIN_MESSAGE_SIZE, <<"min_message_size">>).
-define(KEY_MAX_BOX_NUMBER_LENGTH, <<"max_box_number_length">>).
-define(KEY_EXTENSION, <<"extension">>).
-define(KEY_MAX_PIN_LENGTH, <<"max_pin_length">>).
-define(KEY_DELETE_AFTER_NOTIFY, <<"delete_after_notify">>).
-define(KEY_SAVE_AFTER_NOTIFY, <<"save_after_notify">>).

-define(MAILBOX_DEFAULT_SIZE
       ,kapps_config:get_integer(?CF_CONFIG_CAT
                                ,[?KEY_VOICEMAIL, ?KEY_MAX_MESSAGE_COUNT]
                                ,100
                                )).
-define(MAILBOX_DEFAULT_MSG_MAX_LENGTH
       ,kapps_config:get_integer(?CF_CONFIG_CAT
                                ,[?KEY_VOICEMAIL, ?KEY_MAX_MESSAGE_LENGTH]
                                ,500
                                )).
-define(MAILBOX_DEFAULT_MSG_MIN_LENGTH
       ,kapps_config:get_integer(?CF_CONFIG_CAT
                                ,[?KEY_VOICEMAIL, ?KEY_MIN_MESSAGE_SIZE]
                                ,500
                                )).
-define(MAILBOX_DEFAULT_BOX_NUMBER_LENGTH
       ,kapps_config:get_integer(?CF_CONFIG_CAT
                                ,[?KEY_VOICEMAIL, ?KEY_MAX_BOX_NUMBER_LENGTH]
                                ,15
                                )).
-define(DEFAULT_VM_EXTENSION
       ,kapps_config:get(?CF_CONFIG_CAT
                        ,[?KEY_VOICEMAIL, ?KEY_EXTENSION]
                        ,<<"mp3">>
                        )
       ).

-define(ACCOUNT_VM_EXTENSION(AccountId)
       ,kapps_account_config:get_global(AccountId
                                       ,?CF_CONFIG_CAT
                                       ,[?KEY_VOICEMAIL, ?KEY_EXTENSION]
                                       ,<<"mp3">>
                                       )
       ).

-define(DEFAULT_MAX_PIN_LENGTH
       ,kapps_config:get_integer(?CF_CONFIG_CAT
                                ,[?KEY_VOICEMAIL, ?KEY_MAX_PIN_LENGTH]
                                ,6
                                )
       ).

-define(DEFAULT_SAVE_AFTER_NOTIFY
       ,kapps_config:get(?CF_CONFIG_CAT
                        ,[?KEY_VOICEMAIL, ?KEY_SAVE_AFTER_NOTIFY]
                        ,'false'
                        )
       ).

-define(DEFAULT_DELETE_AFTER_NOTIFY
       ,kapps_config:get(?CF_CONFIG_CAT
                        ,[?KEY_VOICEMAIL, ?KEY_DELETE_AFTER_NOTIFY]
                        ,'false'
                        )
       ).

-define(DEFAULT_FIND_BOX_PROMPT, <<"vm-enter_id">>).
-define(MAX_LOGIN_ATTEMPTS, 3).

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
              ,forward = <<"3">>
              ,prev = <<"4">>
              ,next = <<"6">>
              ,delete = <<"7">>
         }).
-type vm_keys() :: #keys{}.

-define(KEY_LENGTH, 1).

-record(mailbox, {mailbox_id :: api_binary()
                 ,mailbox_number = <<>> :: binary()
                 ,exists = 'false' :: boolean()
                 ,skip_instructions = 'false' :: boolean()
                 ,skip_greeting = 'false' :: boolean()
                 ,unavailable_media_id :: api_binary()
                 ,temporary_unavailable_media_id :: api_binary()
                 ,name_media_id :: api_binary()
                 ,pin = <<>> :: binary()
                 ,timezone :: ne_binary()
                 ,max_login_attempts = ?MAX_LOGIN_ATTEMPTS :: non_neg_integer()
                 ,require_pin = 'false' :: boolean()
                 ,check_if_owner = 'true' :: boolean()
                 ,owner_id :: api_binary()
                 ,is_setup = 'false' :: boolean()
                 ,message_count = 0 :: non_neg_integer()
                 ,max_message_count = 0 :: non_neg_integer()
                 ,max_message_length :: pos_integer()
                 ,min_message_length = ?MAILBOX_DEFAULT_MSG_MIN_LENGTH :: pos_integer()
                 ,keys = #keys{} :: vm_keys()
                 ,transcribe_voicemail = 'false' :: boolean()
                 ,notifications :: kz_json:object()
                 ,after_notify_action = 'nothing' :: 'nothing' | 'delete' | 'save'
                 ,interdigit_timeout = kapps_call_command:default_interdigit_timeout() :: pos_integer()
                 ,play_greeting_intro = 'false' :: boolean()
                 ,use_person_not_available = 'false' :: boolean()
                 ,not_configurable = 'false' :: boolean()
                 ,account_db :: api_binary()
                 ,media_extension :: api_binary()
                 }).
-type mailbox() :: #mailbox{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, based on the payload will either
%% connect a caller to check_voicemail or compose_voicemail.
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case kz_json:get_value(<<"action">>, Data, <<"compose">>) of
        <<"compose">> ->
            kapps_call_command:answer(Call),
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
            kapps_call_command:answer(Call),
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
-spec check_mailbox(mailbox(), kapps_call:call()) ->
                           'ok' | {'error', 'channel_hungup'}.
-spec check_mailbox(mailbox(), kapps_call:call(), non_neg_integer()) ->
                           'ok' | {'error', 'channel_hungup'}.
-spec check_mailbox(mailbox(), boolean(), kapps_call:call(), non_neg_integer()) ->
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
    _ = kapps_call_command:b_prompt(<<"vm-abort">>, Call),
    'ok';
check_mailbox(#mailbox{exists='false'}=Box, _ , Call, Loop) ->
    %% if the callflow did not define the mailbox to check then request the mailbox ID from the user
    {PossibleBox, NewLoop} = find_mailbox(Box, Call, ?DEFAULT_FIND_BOX_PROMPT, Loop),
    check_mailbox(PossibleBox, Call, NewLoop);
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
    _ = kapps_call_command:b_prompt(<<"vm-no_access">>, Call),
    'ok';
check_mailbox(#mailbox{pin=Pin
                      ,interdigit_timeout=Interdigit
                      }=Box, IsOwner, Call, Loop) ->
    lager:info("requesting pin number to check mailbox"),

    NoopId = kapps_call_command:prompt(<<"vm-enter_pass">>, Call),

    case kapps_call_command:collect_digits(?DEFAULT_MAX_PIN_LENGTH
                                          ,kapps_call_command:default_collect_timeout()
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
            _ = kapps_call_command:b_prompt(<<"vm-fail_auth">>, Call),
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
-spec find_mailbox(mailbox(), kapps_call:call(), ne_binary(), non_neg_integer()) -> {mailbox(), non_neg_integer()}.

find_mailbox(#mailbox{max_login_attempts=MaxLoginAttempts}=Box, _Call, _VmEntryIdMedia, Loop)
  when Loop > MaxLoginAttempts ->
    %% if we have exceeded the maximum loop attempts then terminate this call
    %% Note: check_mailbox will play vm-abort
    lager:info("maximum number of invalid attempts to find mailbox"),
    {Box, Loop};
find_mailbox(#mailbox{interdigit_timeout=Interdigit}=Box, Call, VmEntryIdMedia, Loop) ->
    lager:info("requesting mailbox number to check"),

    NoopId = kapps_call_command:prompt(VmEntryIdMedia, Call),

    case kapps_call_command:collect_digits(?MAILBOX_DEFAULT_BOX_NUMBER_LENGTH
                                          ,kapps_call_command:default_collect_timeout()
                                          ,Interdigit
                                          ,NoopId
                                          ,Call
                                          )
    of
        {'ok', <<>>} ->
            _ = kapps_call_command:b_prompt(<<"menu-invalid_entry">>, Call),
            find_mailbox(Box, Call, VmEntryIdMedia, Loop + 1);
        {'ok', Mailbox} ->
            BoxNum = try kz_util:to_integer(Mailbox) catch _:_ -> 0 end,
            case find_mailbox_by_number(BoxNum, Call) of
                {'ok', FoundBox} -> {FoundBox, Loop};
                {'error', 'not_found'} ->
                    _ = kapps_call_command:b_prompt(<<"menu-invalid_entry">>, Call),
                    find_mailbox(Box, Call, VmEntryIdMedia, Loop + 1);
                {'error', _R} ->
                    lager:info("mailbox ~s lookup failed: ~p", [Mailbox, _R]),
                    _ = kapps_call_command:b_prompt(<<"menu-invalid_entry">>, Call),
                    find_mailbox(Box, Call, VmEntryIdMedia, Loop + 1)
            end;
        _E ->
            lager:info("recv other: ~p", [_E]),
            {Box, Loop + 1}
    end.

%% find the voicemail box, by making a fake 'callflow data payload' we look for it now because if the
%% caller is the owner, and the pin is not required then we skip requesting the pin
%%
%% Note: Check mailbox existence here to properly updating Loop in find_mailbox/4
-spec find_mailbox_by_number(non_neg_integer(), kapps_call:call()) ->
                                    {'ok', mailbox()} |
                                    {'error', any()}.
find_mailbox_by_number(BoxNum, Call) ->
    ViewOptions = [{'key', BoxNum}],
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_single_result(AccountDb, <<"vmboxes/listing_by_mailbox">>, ViewOptions) of
        {'ok', JObj} ->
            lager:info("get profile of ~p", [JObj]),
            case get_mailbox_profile(kz_json:from_list([{<<"id">>, kz_doc:id(JObj)}]), Call) of
                #mailbox{exists='false'} -> {'error', 'not_found'};
                Found -> {'ok', Found}
            end;
        Error -> Error
    end.

find_destination_mailbox(Call, _SrcBoxId, Loop) when Loop > ?MAX_LOGIN_ATTEMPTS ->
    lager:info("maximum number of invalid attempts to find destination mailbox"),
    _ = kapps_call_command:b_prompt(<<"vm-abort">>, Call),
    #mailbox{};
find_destination_mailbox(Call, SrcBoxId, Loop) ->
    case find_mailbox(#mailbox{}, <<"vm-enter_forward_id">>, Call, Loop) of
        {#mailbox{exists='false'}, NewLoop} ->
            _ = kapps_call_command:b_prompt(<<"menu-invalid_entry">>, Call),
            find_destination_mailbox(Call, SrcBoxId, NewLoop);
        {#mailbox{mailbox_id=SrcBoxId}, NewLoop} ->
            lager:info("source mailbox can't be a destination mailbox"),
            _ = kapps_call_command:b_prompt(<<"menu-invalid_entry">>, Call),
            find_destination_mailbox(Call, SrcBoxId, NewLoop + 1);
        {DestBox, _NewLoop} -> DestBox
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_voicemail(mailbox(), kapps_call:call()) ->
                               'ok' | {'branch', _} |
                               {'error', 'channel_hungup'}.
-spec compose_voicemail(mailbox(), boolean(), kapps_call:call()) ->
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
    _ = kapps_call_command:b_prompt(<<"vm-not_available_no_voicemail">>, Call),
    'ok';
compose_voicemail(#mailbox{max_message_count=MaxCount
                          ,message_count=Count
                          ,mailbox_id=VMBId
                          ,mailbox_number=VMBN
                          ,keys=#keys{login=Login}
                          }=Box, _, Call) when Count >= MaxCount
                                               andalso MaxCount > 0 ->
    lager:debug("voicemail box is full, cannot hold more messages, sending notification"),
    Props = [{<<"Account-DB">>, kapps_call:account_db(Call)}
            ,{<<"Account-ID">>, kapps_call:account_id(Call)}
            ,{<<"Voicemail-Box">>, VMBId}
            ,{<<"Voicemail-Number">>, VMBN}
            ,{<<"Max-Message-Count">>, MaxCount}
            ,{<<"Message-Count">>, Count}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    _ = kz_amqp_worker:call(Props
                           ,fun kapi_notifications:publish_voicemail_full/1
                           ,fun kapi_notifications:voicemail_full_v/1
                           ),
    lager:debug("playing mailbox greeting to caller"),
    _ = play_greeting_intro(Box, Call),
    _ = play_greeting(Box, Call),
    _ = kapps_call_command:prompt(<<"vm-mailbox_full">>, Call),
    _NoopId = kapps_call_command:noop(Call),

    case kapps_call_command:wait_for_application_or_dtmf(<<"noop">>, 5 * ?MILLISECONDS_IN_MINUTE) of
        {'dtmf', Login} ->
            lager:info("caller wishes to login to mailbox"),
            check_mailbox(Box, Call);
        _Else ->
            lager:debug("finished with call")
    end;
compose_voicemail(#mailbox{keys=#keys{login=Login
                                     ,operator=Operator
                                     }
                          ,media_extension=Ext
                          }=Box, _, Call) ->
    lager:debug("playing mailbox greeting to caller"),
    _ = play_greeting_intro(Box, Call),
    _ = play_greeting(Box, Call),
    _ = play_instructions(Box, Call),
    _NoopId = kapps_call_command:noop(Call),
    %% timeout after 5 min for saftey, so this process cant hang around forever
    case kapps_call_command:wait_for_application_or_dtmf(<<"noop">>, 300000) of
        {'ok', _} ->
            lager:info("played greeting and instructions to caller, recording new message"),
            record_voicemail(tmp_file(Ext), Box, Call);
        {'dtmf', Digit} ->
            _ = kapps_call_command:b_flush(Call),
            case Digit of
                Login ->
                    lager:info("caller pressed '~s', redirecting to check voicemail", [Login]),
                    check_mailbox(Box, Call);
                Operator ->
                    lager:info("caller chose to ring the operator"),
                    case cf_util:get_operator_callflow(kapps_call:account_id(Call)) of
                        {'ok', Flow} -> {'branch', Flow};
                        {'error', _R} -> record_voicemail(tmp_file(Ext), Box, Call)
                    end;
                _Else ->
                    lager:info("caller pressed unbound '~s', skip to recording new message", [_Else]),
                    record_voicemail(tmp_file(Ext), Box, Call)
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
-spec play_greeting_intro(mailbox(), kapps_call:call()) -> ne_binary() | 'ok'.
play_greeting_intro(#mailbox{play_greeting_intro='true'}, Call) ->
    kapps_call_command:audio_macro([{'prompt', <<"vm-greeting_intro">>}], Call);
play_greeting_intro(_, _) -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_greeting(mailbox(), kapps_call:call()) -> ne_binary() | 'ok'.
play_greeting(#mailbox{skip_greeting='true'}, _Call) -> 'ok';
play_greeting(#mailbox{temporary_unavailable_media_id= <<_/binary>> = MediaId}
             ,Call
             ) ->
    Corrected = kz_media_util:media_path(MediaId, Call),
    lager:info("mailbox has a temporary greeting which always overrides standard greeting: '~s', corrected to '~s'",
               [MediaId, Corrected]
              ),
    kapps_call_command:play(Corrected, Call);
play_greeting(#mailbox{use_person_not_available='true'
                      ,unavailable_media_id='undefined'
                      }, Call) ->
    lager:debug("mailbox has no greeting, playing the customized generic"),
    kapps_call_command:audio_macro([{'prompt', <<"vm-person_not_available">>}], Call);
play_greeting(#mailbox{unavailable_media_id='undefined'
                      ,mailbox_number=Mailbox
                      }, Call) ->
    lager:debug("mailbox has no greeting, playing the generic"),
    kapps_call_command:audio_macro([{'prompt', <<"vm-person">>}
                                   ,{'say', Mailbox}
                                   ,{'prompt', <<"vm-not_available">>}
                                   ], Call);
play_greeting(#mailbox{unavailable_media_id=MediaId}, Call) ->
    Corrected = kz_media_util:media_path(MediaId, Call),
    lager:info("mailbox has a greeting: '~s', corrected to '~s'", [MediaId, Corrected]),
    kapps_call_command:play(Corrected, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec play_instructions(mailbox(), kapps_call:call()) -> ne_binary() | 'ok'.
play_instructions(#mailbox{skip_instructions='true'}, _) -> 'ok';
play_instructions(#mailbox{skip_instructions='false'}, Call) ->
    kapps_call_command:prompt(<<"vm-record_message">>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_voicemail(ne_binary(), mailbox(), kapps_call:call()) -> 'ok'.
record_voicemail(AttachmentName, #mailbox{max_message_length=MaxMessageLength
                                         ,media_extension=Ext
                                         }=Box, Call) ->
    Tone = kz_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                             ,{<<"Duration-ON">>, <<"500">>}
                             ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    kapps_call_command:tones([Tone], Call),
    lager:info("composing new voicemail to ~s", [AttachmentName]),
    case kapps_call_command:b_record(AttachmentName, ?ANY_DIGIT, kz_util:to_binary(MaxMessageLength), Call) of
        {'ok', Msg} ->
            Length = kz_json:get_integer_value(<<"Length">>, Msg, 0),
            IsCallUp = kz_json:get_value(<<"Hangup-Cause">>, Msg) =:= 'undefined',
            case IsCallUp
                andalso review_recording(AttachmentName, 'true', Box, Call)
            of
                'false' ->
                    new_message(AttachmentName, Length, Box, Call);
                {'ok', 'record'} ->
                    record_voicemail(tmp_file(Ext), Box, Call);
                {'ok', _Selection} ->
                    cf_util:start_task(fun new_message/4, [AttachmentName, Length, Box], Call),
                    _ = kapps_call_command:prompt(<<"vm-saved">>, Call),
                    _ = kapps_call_command:prompt(<<"vm-thank_you">>, Call),
                    'ok';
                {'branch', Flow} ->
                    _ = new_message(AttachmentName, Length, Box, Call),
                    _ = kapps_call_command:prompt(<<"vm-saved">>, Call),
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
-spec setup_mailbox(mailbox(), kapps_call:call()) -> mailbox().
setup_mailbox(#mailbox{media_extension=Ext}=Box, Call) ->
    lager:debug("starting voicemail configuration wizard"),
    {'ok', _} = kapps_call_command:b_prompt(<<"vm-setup_intro">>, Call),

    lager:info("prompting caller to set a pin"),
    #mailbox{} = change_pin(Box, Call),

    {'ok', _} = kapps_call_command:b_prompt(<<"vm-setup_rec_greeting">>, Call),
    lager:info("prompting caller to record an unavailable greeting"),

    #mailbox{}=Box1 = record_unavailable_greeting(tmp_file(Ext), Box, Call),
    'ok' = update_doc(<<"is_setup">>, 'true', Box1, Call),
    lager:info("voicemail configuration wizard is complete"),

    {'ok', _} = kapps_call_command:b_prompt(<<"vm-setup_complete">>, Call),
    Box1#mailbox{is_setup='true'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec main_menu(mailbox(), kapps_call:call()) ->
                       'ok' | {'error', 'channel_hungup'}.
-spec main_menu(mailbox(), kapps_call:call(), non_neg_integer()) ->
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
    _ = kapps_call_command:b_prompt(<<"vm-goodbye">>, Call),
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
    _ = kapps_call_command:b_flush(Call),

    Messages = kvm_messages:get(kapps_call:account_id(Call), BoxId),
    New = kzd_box_message:count_folder(Messages, ?VM_FOLDER_NEW),
    Saved = kzd_box_message:count_folder(Messages, ?VM_FOLDER_SAVED),

    lager:debug("mailbox has ~p new and ~p saved messages", [New, Saved]),
    NoopId = kapps_call_command:audio_macro(message_count_prompts(New, Saved)
                                            ++ [{'prompt', <<"vm-main_menu_not_configurable">>}]
                                           ,Call
                                           ),

    case kapps_call_command:collect_digits(?KEY_LENGTH
                                          ,kapps_call_command:default_collect_timeout()
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
            Folder = kzd_box_message:filter_folder(Messages, ?VM_FOLDER_NEW),
            case play_messages(Folder, New, Box, Call) of
                'ok' -> send_mwi_update(Box, Call);
                _Else -> main_menu(Box, Call)
            end;
        {'ok', HearSaved} ->
            lager:info("playing all messages in folder: ~s", [?VM_FOLDER_SAVED]),
            Folder = kzd_box_message:filter_folder(Messages, ?VM_FOLDER_SAVED),
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
    _ = kapps_call_command:b_flush(Call),

    Messages = kvm_messages:get(kapps_call:account_id(Call), BoxId),
    New = kzd_box_message:count_folder(Messages, ?VM_FOLDER_NEW),
    Saved = kzd_box_message:count_folder(Messages, ?VM_FOLDER_SAVED),

    lager:debug("mailbox has ~p new and ~p saved messages", [New, Saved]),
    NoopId = kapps_call_command:audio_macro(message_count_prompts(New, Saved)
                                            ++ [{'prompt', <<"vm-main_menu">>}]
                                           ,Call),

    case kapps_call_command:collect_digits(?KEY_LENGTH
                                          ,kapps_call_command:default_collect_timeout()
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
            Folder = kzd_box_message:filter_folder(Messages, ?VM_FOLDER_NEW),
            case play_messages(Folder, New, Box, Call) of
                'ok' -> send_mwi_update(Box, Call);
                _Else -> main_menu(Box, Call)
            end;
        {'ok', HearSaved} ->
            lager:info("playing all messages in folder: ~s", [?VM_FOLDER_SAVED]),
            Folder = kzd_box_message:filter_folder(Messages, ?VM_FOLDER_SAVED),
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
-spec message_count_prompts(integer(), integer()) -> kz_proplist().
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
    ,{'say', kz_util:to_binary(New), ?VM_KEY_MESSAGES}
    ,{'prompt', <<"vm-new_messages">>}
    ];
message_count_prompts(New, 1) ->
    [{'prompt', <<"vm-you_have">>}
    ,{'say', kz_util:to_binary(New), ?VM_KEY_MESSAGES}
    ,{'prompt', <<"vm-new_and">>}
    ,{'say', <<"1">>, ?VM_KEY_MESSAGES}
    ,{'prompt', <<"vm-saved_message">>}
    ];
message_count_prompts(0, Saved) ->
    [{'prompt', <<"vm-you_have">>}
    ,{'say', kz_util:to_binary(Saved), ?VM_KEY_MESSAGES}
    ,{'prompt', <<"vm-saved_messages">>}
    ];
message_count_prompts(1, Saved) ->
    [{'prompt', <<"vm-you_have">>}
    ,{'say', <<"1">>, ?VM_KEY_MESSAGES}
    ,{'prompt', <<"vm-new_and">>}
    ,{'say', kz_util:to_binary(Saved), ?VM_KEY_MESSAGES}
    ,{'prompt', <<"vm-saved_messages">>}
    ];
message_count_prompts(New, Saved) ->
    [{'prompt', <<"vm-you_have">>}
    ,{'say', kz_util:to_binary(New), ?VM_KEY_MESSAGES}
    ,{'prompt', <<"vm-new_and">>}
    ,{'say', kz_util:to_binary(Saved), ?VM_KEY_MESSAGES}
    ,{'prompt', <<"vm-saved_messages">>}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays back a message then the menu, and continues to loop over the
%% menu utill
%% @end
%%--------------------------------------------------------------------
-spec play_messages(kz_json:objects(), non_neg_integer(), mailbox(), kapps_call:call()) ->
                           'ok' | 'complete'.
play_messages(Messages, Count, Box, Call) ->
    play_messages(Messages, [], Count, Box, Call).

-spec play_messages(kz_json:objects(), kz_json:objects(), non_neg_integer(), mailbox(), kapps_call:call()) ->
                           'ok' | 'complete'.
play_messages([H|T]=Messages, PrevMessages, Count, #mailbox{timezone=Timezone
                                                           }=Box, Call) ->
    AccountId = kapps_call:account_id(Call),
    Message = kvm_message:media_url(AccountId, H),
    lager:info("playing mailbox message ~p (~s)", [Count, Message]),
    Prompt = [{'prompt', <<"vm-message_number">>}
             ,{'say', kz_util:to_binary(Count - length(Messages) + 1), <<"number">>}
             ,{'play', Message}
             ,{'prompt', <<"vm-received">>}
             ,{'say',  get_unix_epoch(kz_json:get_value(<<"timestamp">>, H), Timezone), <<"current_date_time">>}
             ,{'prompt', <<"vm-message_menu">>}
             ],
    case message_menu(Prompt, Box, Call) of
        {'ok', 'keep'} ->
            lager:info("caller chose to save the message"),
            _ = kapps_call_command:b_prompt(<<"vm-saved">>, Call),
            {_, NMessage} = kvm_message:set_folder(?VM_FOLDER_SAVED, H, AccountId),
            play_messages(T, [NMessage|PrevMessages], Count, Box, Call);
        {'ok', 'prev'} ->
            lager:info("caller chose to listen to previous message"),
            play_prev_message(Messages, PrevMessages, Count, Box, Call);
        {'ok', 'next'} ->
            lager:info("caller chose to listen to next message"),
            play_next_message(Messages, PrevMessages, Count, Box, Call);
        {'ok', 'delete'} ->
            lager:info("caller chose to delete the message"),
            _ = kapps_call_command:b_prompt(<<"vm-deleted">>, Call),
            _ = kvm_message:set_folder({?VM_FOLDER_DELETED, 'false'}, H, AccountId),
            play_messages(T, PrevMessages, Count, Box, Call);
        {'ok', 'return'} ->
            lager:info("caller chose to return to the main menu"),
            _ = kapps_call_command:b_prompt(<<"vm-saved">>, Call),
            _ = kvm_message:set_folder(?VM_FOLDER_SAVED, H, AccountId),
            'complete';
        {'ok', 'replay'} ->
            lager:info("caller chose to replay"),
            play_messages(Messages, PrevMessages, Count, Box, Call);
        {'ok', 'forward'} ->
            lager:info("caller chose to forward the message"),
            forward_message(H, Box, Call),
            _ = kvm_message:set_folder(?VM_FOLDER_SAVED, H, AccountId),
            _ = kapps_call_command:prompt(<<"vm-saved">>, Call),
            play_messages(T, PrevMessages, Count, Box, Call);
        {'error', _} ->
            lager:info("error during message playback")
    end;
play_messages([], _, _, _, _) ->
    lager:info("all messages in folder played to caller"),
    'complete'.

-spec play_next_message(kz_json:objects(), kz_json:objects(), non_neg_integer(), mailbox(), kapps_call:call()) ->
                               'ok' | 'complete'.
play_next_message([_] = Messages, PrevMessages, Count, Box, Call) ->
    play_messages(Messages, PrevMessages, Count, Box, Call);
play_next_message([H|T], PrevMessages, Count, Box, Call) ->
    play_messages(T, [H|PrevMessages], Count, Box, Call).

-spec play_prev_message(kz_json:objects(), kz_json:objects(), non_neg_integer(), mailbox(), kapps_call:call()) ->
                               'ok' | 'complete'.
play_prev_message(Messages, [] = PrevMessages, Count, Box, Call) ->
    play_messages(Messages, PrevMessages, Count, Box, Call);
play_prev_message(Messages, [H|T], Count, Box, Call) ->
    play_messages([H|Messages], T, Count, Box, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Main function for forwarding a message to another vmbox of this account
%% @end
%%--------------------------------------------------------------------
-spec forward_message(kz_json:object(), mailbox(), kapps_call:call()) -> 'ok'.
forward_message(Message, #mailbox{mailbox_id=SrcBoxId}, Call) ->
    lager:info("enter destination mailbox number"),
    _ = kapps_call_command:b_flush(Call),
    PossibleBox = find_destination_mailbox(Call, SrcBoxId, 1),
    forward_message(Message, SrcBoxId, PossibleBox, Call).

forward_message(_Message, _SrcBoxId, #mailbox{exists='false'}, Call) ->
    _ = kapps_call_command:b_prompt(<<"vm-abort">>, Call),
    lager:info("unable to find destination mailbox, returning to message menu...");
forward_message(Message, SrcBoxId, DestBox, Call) ->
    case forward_message_menu(DestBox, Call) of
        {'ok', 'append'} ->
            compose_forward_message(Message, SrcBoxId, DestBox, Call);
        {'ok', 'forward'} ->
            forward_message('undefined', 'undefined', Message, SrcBoxId, DestBox, Call);
        {'error', _R} ->
            lager:info("error during forward message playback: ~p", [_R])
    end.

forward_message_menu(#mailbox{interdigit_timeout=Interdigit}=DestBox, Call) ->
    lager:info("playing forward message menu"),

    Prompt = [{'prompt', <<"vm-message_forwarding">>}],
    NoopId = kapps_call_command:audio_macro(Prompt, Call),

    case kapps_call_command:collect_digits(?KEY_LENGTH
                                          ,kapps_call_command:default_collect_timeout()
                                          ,Interdigit
                                          ,NoopId
                                          ,Call
                                          )
    of
        {'ok', <<"1">>} -> {'ok', 'append'};
        {'ok', <<"2">>} -> {'ok', 'forward'};
        {'ok', <<"0">>} -> {'ok', 'return'};
        {'error', _}=Error -> Error;
        _ -> forward_message_menu(DestBox, Call)
    end.

compose_forward_message(Message, SrcBoxId, #mailbox{media_extension=Ext}=DestBox, Call) ->
    lager:debug("playing forwarding instructions to caller"),
    _ = play_instructions(DestBox, Call),
    _NoopId = kapps_call_command:noop(Call),
    %% timeout after 5 min for saftey, so this process cant hang around forever
    case kapps_call_command:wait_for_application_or_dtmf(<<"noop">>, 300000) of
        {'ok', _} ->
            lager:info("played fowarding instructions to caller, recording new message"),
            record_forward(tmp_file(Ext), Message, SrcBoxId, DestBox, Call);
        {'dtmf', _Digits} ->
            _ = kapps_call_command:b_flush(Call),
            lager:info("recording forwarding message"),
            record_forward(tmp_file(Ext), Message, SrcBoxId, DestBox, Call);
        {'error', _R} ->
            lager:info("error while playing voicemail greeting: ~p", [_R])
    end.

record_forward(AttachmentName, Message, SrcBoxId, #mailbox{media_extension=Ext
                                                          ,max_message_length=MaxMessageLength
                                                          }=DestBox, Call) ->
    lager:info("composing new forward voicemail to ~s", [AttachmentName]),
    Tone = kz_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                             ,{<<"Duration-ON">>, <<"500">>}
                             ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    kapps_call_command:tones([Tone], Call),
    lager:info("composing a voicemail forward to ~s", [AttachmentName]),
    _NoopId = kapps_call_command:audio_macro([{'prompt',  <<"vm-record_name">>}
                                             ,{'tones', [Tone]}
                                             ], Call),
    case kapps_call_command:b_record(AttachmentName, ?ANY_DIGIT, kz_util:to_binary(MaxMessageLength), Call) of
        {'ok', Msg} ->
            Length = kz_json:get_integer_value(<<"Length">>, Msg, 0),
            IsCallUp = kz_json:get_value(<<"Hangup-Cause">>, Msg) =:= 'undefined',
            case IsCallUp
                andalso review_recording(AttachmentName, 'false', DestBox, Call)
            of
                'false' ->
                    forward_message(AttachmentName, Length, Message, SrcBoxId, DestBox, Call);
                {'ok', 'record'} ->
                    record_forward(tmp_file(Ext), Message, SrcBoxId, DestBox, Call);
                {'ok', _Selection} ->
                    cf_util:start_task(fun forward_message/6
                                      ,[AttachmentName, Length, Message, SrcBoxId, DestBox]
                                      , Call
                                      )
            end;
        {'error', _R} ->
            lager:info("error while attempting to record a foward message: ~p", [_R])
    end.

forward_message(AttachmentName, Length, Message, SrcBoxId, #mailbox{mailbox_number=BoxNum
                                                                   ,mailbox_id=BoxId
                                                                   ,timezone=Timezone
                                                                   ,owner_id=OwnerId
                                                                   ,transcribe_voicemail=MaybeTranscribe
                                                                   ,after_notify_action=Action
                                                                   ,media_extension=Extension
                                                                   }=DestBox, Call) ->
    NewMsgProps = props:filter_empty(
                    [{<<"Box-Id">>, BoxId}
                    ,{<<"Owner-Id">>, OwnerId}
                    ,{<<"Length">>, Length}
                    ,{<<"Transcribe-Voicemail">>, MaybeTranscribe}
                    ,{<<"After-Notify-Action">>, Action}
                    ,{<<"Attachment-Name">>, AttachmentName}
                    ,{<<"Box-Num">>, BoxNum}
                    ,{<<"Timezone">>, Timezone}
                    ,{<<"Description">>, <<"forwarded voicemail message with media">>}
                    ,{<<"Media-Extension">>, Extension}
                    ]
                   ),
    case kvm_message:forward_message(Call, Message, SrcBoxId, NewMsgProps) of
        'ok' -> send_mwi_update(DestBox, Call);
        {'error', _, _Msg} ->
            lager:warning("failed to save forwarded voice mail message recorded media : ~p", [_Msg])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loops over the message menu after the first play back util the
%% user provides a valid option
%% @end
%%--------------------------------------------------------------------
-type message_menu_returns() :: {'ok', 'keep' | 'delete' | 'return' | 'replay' | 'prev' | 'next' | 'forward'}.

-spec message_menu(mailbox(), kapps_call:call()) ->
                          {'error', 'channel_hungup' | 'channel_unbridge' | kz_json:object()} |
                          message_menu_returns().
-spec message_menu(kapps_call_command:audio_macro_prompts(), mailbox(), kapps_call:call()) ->
                          {'error', 'channel_hungup' | 'channel_unbridge' | kz_json:object()} |
                          message_menu_returns().
message_menu(Box, Call) ->
    message_menu([{'prompt', <<"vm-message_menu">>}], Box, Call).
message_menu(Prompt, #mailbox{keys=#keys{replay=Replay
                                        ,keep=Keep
                                        ,forward=Forward
                                        ,delete=Delete
                                        ,prev=Prev
                                        ,next=Next
                                        ,return_main=ReturnMain
                                        }
                             ,interdigit_timeout=Interdigit
                             }=Box, Call) ->
    lager:info("playing message menu"),
    NoopId = kapps_call_command:audio_macro(Prompt, Call),

    case kapps_call_command:collect_digits(?KEY_LENGTH
                                          ,kapps_call_command:default_collect_timeout()
                                          ,Interdigit
                                          ,NoopId
                                          ,Call
                                          )
    of
        {'ok', Keep} -> {'ok', 'keep'};
        {'ok', Forward} -> {'ok', 'forward'};
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
-spec config_menu(mailbox(), kapps_call:call()) ->
                         'ok' | mailbox() |
                         {'error', 'channel_hungup'}.
-spec config_menu(mailbox(), kapps_call:call(), pos_integer()) ->
                         'ok' | mailbox() |
                         {'error', 'channel_hungup'}.
config_menu(Box, Call) ->
    config_menu(Box, Call, 1).

config_menu(#mailbox{interdigit_timeout=Interdigit}=Box
           ,Call
           ,Loop
           ) when Loop < 4 ->
    lager:info("playing mailbox configuration menu"),
    {'ok', _} = kapps_call_command:b_flush(Call),

    NoopId = kapps_call_command:prompt(<<"vm-settings_menu">>, Call),

    case kapps_call_command:collect_digits(?KEY_LENGTH
                                          ,kapps_call_command:default_collect_timeout()
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

-spec handle_config_selection(mailbox(), kapps_call:call(), pos_integer(), binary()) ->
                                     'ok' | mailbox() |
                                     {'error', 'channel_hungup'}.
handle_config_selection(#mailbox{keys=#keys{rec_unavailable=Selection}
                                ,media_extension=Ext
                                }=Box
                       ,Call
                       ,_Loop
                       ,Selection
                       ) ->
    lager:info("caller chose to record their unavailable greeting"),
    case record_unavailable_greeting(tmp_file(Ext), Box, Call) of
        'ok' -> 'ok';
        Else -> config_menu(Else, Call)
    end;
handle_config_selection(#mailbox{keys=#keys{rec_name=Selection}
                                ,media_extension=Ext
                                }=Box
                       ,Call
                       ,_Loop
                       ,Selection
                       ) ->
    lager:info("caller chose to record their name"),
    case record_name(tmp_file(Ext), Box, Call) of
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
handle_config_selection(#mailbox{keys=#keys{rec_temporary_unavailable=Selection}
                                ,media_extension=Ext
                                }=Box
                       ,Call
                       ,_Loop
                       ,Selection
                       ) ->
    lager:info("caller chose to record their temporary unavailable greeting"),
    case record_temporary_unavailable_greeting(tmp_file(Ext), Box, Call) of
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
-spec record_temporary_unavailable_greeting(ne_binary(), mailbox(), kapps_call:call()) ->
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
-spec overwrite_temporary_unavailable_greeting(ne_binary(), mailbox(), kapps_call:call()) ->
                                                      'ok' | mailbox().
overwrite_temporary_unavailable_greeting(AttachmentName
                                        ,#mailbox{temporary_unavailable_media_id=MediaId
                                                 ,media_extension=Ext
                                                 }=Box
                                        ,Call
                                        ) ->
    lager:info("overwriting temporary unavailable greeting  as ~s", [AttachmentName]),
    Tone = kz_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                             ,{<<"Duration-ON">>, <<"500">>}
                             ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    _NoopId = kapps_call_command:audio_macro(
                [{'prompt', <<"vm-record_temp_greeting">>}
                ,{'tones', [Tone]}
                ]
                                            ,Call
               ),
    case kapps_call_command:b_record(AttachmentName, Call) of
        {'ok', Msg} ->
            case review_recording(AttachmentName, 'false', Box, Call) of
                {'ok', 'record'} ->
                    lager:info("selected item: record new temporary greetings"),
                    record_temporary_unavailable_greeting(tmp_file(Ext), Box, Call);
                {'ok', 'save'} ->
                    lager:info("selected item: store recorded temporary greetings"),
                    Length = kz_json:get_integer_value(<<"Length">>, Msg, 0),
                    _ = store_recording(AttachmentName, Length, MediaId, Box, Call),
                    'ok' = update_doc([<<"media">>, <<"temporary_unavailable">>], MediaId, Box, Call),
                    _ = kapps_call_command:b_prompt(<<"vm-saved">>, Call),
                    Box;
                {'ok', 'no_selection'} ->
                    lager:info("selected item: no selection"),
                    _ = kapps_call_command:b_prompt(<<"vm-deleted">>, Call),
                    'ok';
                {'branch', _}=B -> B
            end;
        {'error', _R} ->
            lager:info("error while attempting to record temporary unavailable recording: ~p", [_R])

    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes current temporary greeting.
%% @end
%%--------------------------------------------------------------------
-spec delete_temporary_unavailable_greeting(mailbox(), kapps_call:call()) ->
                                                   'ok' | mailbox().
delete_temporary_unavailable_greeting(#mailbox{temporary_unavailable_media_id='undefined'}=_Box, _Call) ->
    'ok';
delete_temporary_unavailable_greeting(Box, Call) ->
    'ok' = update_doc([<<"media">>, <<"temporary_unavailable">>], 'undefined', Box, Call),
    Box#mailbox{temporary_unavailable_media_id='undefined'}.

-spec record_unavailable_greeting(ne_binary(), mailbox(), kapps_call:call()) ->
                                         'ok' | mailbox().
record_unavailable_greeting(AttachmentName, #mailbox{unavailable_media_id='undefined'}=Box, Call) ->
    MediaId = recording_media_doc(<<"unavailable greeting">>, Box, Call),
    record_unavailable_greeting(AttachmentName, Box#mailbox{unavailable_media_id=MediaId}, Call);
record_unavailable_greeting(AttachmentName, #mailbox{unavailable_media_id=MediaId}=Box, Call) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), MediaId) of
        {'ok', JObj} -> check_media_source(AttachmentName, Box, Call, JObj);
        _ -> overwrite_unavailable_greeting(AttachmentName, Box, Call)
    end.

-spec check_media_source(ne_binary(), mailbox(), kapps_call:call(), kz_json:object()) ->
                                'ok' | mailbox().
check_media_source(AttachmentName, Box, Call, JObj) ->
    case kz_json:get_value(<<"media_source">>, JObj) of
        <<"upload">> ->
            lager:debug("The voicemail greeting media is a web upload, let's not touch it,"
                        ++ " it may be in use in some other maibox. We create new media document."
                       ),
            record_unavailable_greeting(AttachmentName, Box#mailbox{unavailable_media_id='undefined'}, Call);
        _ ->
            overwrite_unavailable_greeting(AttachmentName, Box, Call)
    end.

-spec overwrite_unavailable_greeting(ne_binary(), mailbox(), kapps_call:call()) ->
                                            'ok' | mailbox().
overwrite_unavailable_greeting(AttachmentName, #mailbox{unavailable_media_id=MediaId
                                                       ,media_extension=Ext
                                                       }=Box, Call) ->
    lager:info("overwriting unavailable greeting  as ~s", [AttachmentName]),
    Tone = kz_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                             ,{<<"Duration-ON">>, <<"500">>}
                             ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    _NoopId = kapps_call_command:audio_macro([{'prompt', <<"vm-record_greeting">>}
                                             ,{'tones', [Tone]}
                                             ]
                                            ,Call
                                            ),
    case kapps_call_command:b_record(AttachmentName, Call) of
        {'ok', Msg} ->
            case review_recording(AttachmentName, 'false', Box, Call) of
                {'ok', 'record'} ->
                    record_unavailable_greeting(tmp_file(Ext), Box, Call);
                {'ok', 'save'} ->
                    Length = kz_json:get_integer_value(<<"Length">>, Msg, 0),
                    _ = store_recording(AttachmentName, Length, MediaId, Box, Call),
                    'ok' = update_doc([<<"media">>, <<"unavailable">>], MediaId, Box, Call),
                    _ = kapps_call_command:b_prompt(<<"vm-saved">>, Call),
                    Box;
                {'ok', 'no_selection'} ->
                    _ = kapps_call_command:b_prompt(<<"vm-deleted">>, Call),
                    'ok';
                {'branch', _}=B -> B
            end;
        {'error', _R} ->
            lager:info("error while attempting to record unavailable recording: ~p", [_R])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec record_name(ne_binary(), mailbox(), kapps_call:call()) ->
                         'ok' | mailbox().
-spec record_name(ne_binary(), mailbox(), kapps_call:call(), ne_binary()) ->
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

record_name(AttachmentName, #mailbox{name_media_id=MediaId
                                    ,media_extension=Ext
                                    }=Box, Call, DocId) ->
    lager:info("recording name as ~s in ~s", [AttachmentName, MediaId]),
    Tone = kz_json:from_list([{<<"Frequencies">>, [<<"440">>]}
                             ,{<<"Duration-ON">>, <<"500">>}
                             ,{<<"Duration-OFF">>, <<"100">>}
                             ]),
    _NoopId = kapps_call_command:audio_macro([{'prompt',  <<"vm-record_name">>}
                                             ,{'tones', [Tone]}
                                             ], Call),
    case kapps_call_command:b_record(AttachmentName, Call) of
        {'ok', Msg} ->
            case review_recording(AttachmentName, 'false', Box, Call) of
                {'ok', 'record'} ->
                    record_name(tmp_file(Ext), Box, Call);
                {'ok', 'save'} ->
                    Length = kz_json:get_integer_value(<<"Length">>, Msg, 0),
                    _ = store_recording(AttachmentName, Length, MediaId, Box, Call),
                    'ok' = update_doc(?RECORDED_NAME_KEY, MediaId, DocId, Call),
                    _ = kapps_call_command:b_prompt(<<"vm-saved">>, Call),
                    Box;
                {'ok', 'no_selection'} ->
                    _ = kapps_call_command:b_prompt(<<"vm-deleted">>, Call),
                    'ok';
                {'branch', _}=B -> B
            end;
        {'error', _R} ->
            lager:info("error while attempting to record recording name: ~p", [_R])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec change_pin(mailbox(), kapps_call:call()) ->
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

        Pin =:= <<>>
            andalso throw('pin_empty'),
        lager:info("entered pin is not empty"),

        AccountDb = kapps_call:account_db(Call),

        {'ok', JObj} = kz_datamgr:open_cache_doc(AccountDb, Id),

        case validate_box_schema(kz_json:set_value(<<"pin">>, Pin, JObj)) of
            {'ok', PublicJObj} ->
                PrivJObj = kz_json:private_fields(JObj),

                JObj1 = kz_json:merge_jobjs(PrivJObj, PublicJObj),

                {'ok', _} = kz_datamgr:save_doc(AccountDb, JObj1),
                {'ok', _} = kapps_call_command:b_prompt(<<"vm-pin_set">>, Call),
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

-spec invalid_pin(mailbox(), kapps_call:call()) ->
                         mailbox() |
                         {'error', any()}.
invalid_pin(Box, Call) ->
    case kapps_call_command:b_prompt(<<"vm-pin_invalid">>, Call) of
        {'ok', _} -> change_pin(Box, Call);
        {'error', 'channel_hungup'}=E ->
            lager:debug("channel hungup after bad pin"),
            E;
        {'error', _E}=E ->
            lager:debug("invalid pin prompt interrupted: ~p", [_E]),
            E
    end.

-spec validate_box_schema(kz_json:object()) ->
                                 {'ok', kz_json:object()} |
                                 {'error', any()}.
validate_box_schema(JObj) ->
    {'ok', Schema} = kz_json_schema:load(<<"vmboxes">>),
    case jesse:validate_with_schema(Schema, kz_json:public_fields(JObj)) of
        {'ok', _}=OK -> OK;
        {'error', _Errors} ->
            lager:debug("failed to validate vmbox schema: ~p", [_Errors]),
            {'error', 'invalid_pin'}
    end.

-spec get_new_pin(pos_integer(), kapps_call:call()) ->
                         {'ok', binary()} |
                         {'error', any()}.
get_new_pin(Interdigit, Call) ->
    NoopId = kapps_call_command:prompt(<<"vm-enter_new_pin">>, Call),
    collect_pin(Interdigit, Call, NoopId).

-spec confirm_new_pin(pos_integer(), kapps_call:call()) ->
                             {'ok', binary()} |
                             {'error', any()}.
confirm_new_pin(Interdigit, Call) ->
    NoopId = kapps_call_command:prompt(<<"vm-enter_new_pin_confirm">>, Call),
    collect_pin(Interdigit, Call, NoopId).

-spec collect_pin(pos_integer(), kapps_call:call(), ne_binary()) ->
                         {'ok', binary()} |
                         {'error', any()}.
collect_pin(Interdigit, Call, NoopId) ->
    kapps_call_command:collect_digits(?DEFAULT_MAX_PIN_LENGTH
                                     ,kapps_call_command:default_collect_timeout()
                                     ,Interdigit
                                     ,NoopId
                                     ,Call
                                     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new_message(ne_binary(), pos_integer(), mailbox(), kapps_call:call()) -> any().
new_message(_, Length, #mailbox{min_message_length=MinLength}, _)
  when Length < MinLength ->
    lager:info("attachment length is ~B and must be larger than ~B to be stored", [Length, MinLength]);
new_message(AttachmentName, Length, #mailbox{mailbox_number=BoxNum
                                            ,mailbox_id=BoxId
                                            ,timezone=Timezone
                                            ,owner_id=OwnerId
                                            ,transcribe_voicemail=MaybeTranscribe
                                            ,after_notify_action=Action
                                            }=Box, Call) ->
    NewMsgProps = [{<<"Box-Id">>, BoxId}
                  ,{<<"Owner-Id">>, OwnerId}
                  ,{<<"Length">>, Length}
                  ,{<<"Transcribe-Voicemail">>, MaybeTranscribe}
                  ,{<<"After-Notify-Action">>, Action}
                  ,{<<"Attachment-Name">>, AttachmentName}
                  ,{<<"Box-Num">>, BoxNum}
                  ,{<<"Timezone">>, Timezone}
                  ],
    case kvm_message:new(Call, NewMsgProps) of
        'ok' -> send_mwi_update(Box, Call);
        {'error', _, _Msg} -> lager:warning("failed to save voice mail message recorded media : ~p", [_Msg])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the mailbox parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec get_mailbox_profile(kz_json:object(), kapps_call:call()) -> mailbox().
get_mailbox_profile(Data, Call) ->
    Id = kz_doc:id(Data),
    AccountDb = kapps_call:account_db(Call),
    AccountId = kapps_call:account_id(Call),

    case get_mailbox_doc(AccountDb, Id, Data, Call) of
        {'ok', MailboxJObj} ->
            MailboxId = kz_doc:id(MailboxJObj),
            lager:info("loaded voicemail box ~s", [MailboxId]),
            Default = #mailbox{},

            %% dont check if the voicemail box belongs to the owner (by default) if the call was not
            %% specificly to him, IE: calling a ring group and going to voicemail should not check
            LastAct = kapps_call:kvs_fetch('cf_last_action', Call),
            CheckIfOwner = (('undefined' =:= LastAct)
                            orelse ('cf_device' =:= LastAct)
                           ),

            {NameMediaId, OwnerId} = owner_info(AccountDb, MailboxJObj),

            MaxMessageCount = max_message_count(Call),
            MsgCount = kvm_messages:count(kapps_call:account_id(Call), MailboxId),

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
                         kzd_voicemail_box:mailbox_number(MailboxJObj, kapps_call:request_user(Call))
                    ,require_pin =
                         kzd_voicemail_box:pin_required(MailboxJObj)
                    ,check_if_owner =
                         kzd_voicemail_box:check_if_owner(MailboxJObj, CheckIfOwner)
                    ,unavailable_media_id =
                         kz_json:get_ne_value([<<"media">>, <<"unavailable">>], MailboxJObj)
                    ,temporary_unavailable_media_id =
                         kz_json:get_ne_value([<<"media">>, <<"temporary_unavailable">>], MailboxJObj)
                    ,name_media_id =
                         NameMediaId
                    ,owner_id =
                         OwnerId
                    ,is_setup =
                         kzd_voicemail_box:is_setup(MailboxJObj, 'false')
                    ,max_message_count =
                         kz_util:to_integer(MaxMessageCount)
                    ,max_message_length = find_max_message_length([Data, MailboxJObj])
                    ,min_message_length = min_recording_length(Call)
                    ,message_count =
                         MsgCount
                    ,transcribe_voicemail =
                         kz_json:is_true(<<"transcribe">>, MailboxJObj, 'false')
                    ,notifications =
                         kz_json:get_value(<<"notifications">>, MailboxJObj)
                    ,after_notify_action = AfterNotifyAction
                    ,interdigit_timeout =
                         kz_json:find(<<"interdigit_timeout">>, [MailboxJObj, Data], kapps_call_command:default_interdigit_timeout())
                    ,play_greeting_intro =
                         kz_json:is_true(<<"play_greeting_intro">>, MailboxJObj, Default#mailbox.play_greeting_intro)
                    ,use_person_not_available =
                         kz_json:is_true(<<"use_person_not_available">>, MailboxJObj, Default#mailbox.use_person_not_available)
                    ,not_configurable=
                         kz_json:is_true(<<"not_configurable">>, MailboxJObj, 'false')
                    ,account_db = AccountDb
                    ,media_extension = kz_json:get_value(<<"media_extension">>, MailboxJObj, ?ACCOUNT_VM_EXTENSION(AccountId))
                    };
        {'error', R} ->
            lager:info("failed to load voicemail box ~s, ~p", [Id, R]),
            #mailbox{}
    end.

-spec after_notify_action(kz_json:object()) -> atom().
after_notify_action(MailboxJObj) ->
    Delete = kz_json:is_true(?KEY_DELETE_AFTER_NOTIFY, MailboxJObj, ?DEFAULT_DELETE_AFTER_NOTIFY),
    Save   = kz_json:is_true(?KEY_SAVE_AFTER_NOTIFY, MailboxJObj, ?DEFAULT_SAVE_AFTER_NOTIFY),

    case {Delete, Save} of
        {'false', 'false'} -> 'nothing';
        {'false', 'true'}  -> 'save';
        {'true', 'false'}  -> 'delete';
        {'true', 'true'}   -> 'save'
    end.

-spec max_message_count(kapps_call:call()) -> non_neg_integer().
max_message_count(Call) ->
    case kapps_account_config:get(kapps_call:account_id(Call)
                                 ,?CF_CONFIG_CAT
                                 ,[?KEY_VOICEMAIL, ?KEY_MAX_MESSAGE_COUNT]
                                 )
    of
        'undefined' -> ?MAILBOX_DEFAULT_SIZE;
        MMC -> MMC
    end.

-spec owner_info(ne_binary(), kz_json:object()) ->
                        {api_binary(), api_binary()}.
-spec owner_info(ne_binary(), kz_json:object(), api_binary()) ->
                        {api_binary(), api_binary()}.
owner_info(AccountDb, MailboxJObj) ->
    owner_info(AccountDb, MailboxJObj, kz_json:get_ne_value(<<"owner_id">>, MailboxJObj)).

owner_info(_AccountDb, MailboxJObj, 'undefined') ->
    {kz_json:get_ne_value(?RECORDED_NAME_KEY, MailboxJObj)
    ,'undefined'
    };
owner_info(AccountDb, MailboxJObj, OwnerId) ->
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', OwnerJObj} ->
            {kz_json:find(?RECORDED_NAME_KEY, [OwnerJObj, MailboxJObj]), OwnerId};
        {'error', 'not_found'} ->
            lager:info("owner ~s no longer exists", [OwnerId]),
            {kz_json:get_ne_value(?RECORDED_NAME_KEY, MailboxJObj), 'undefined'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec populate_keys(kapps_call:call()) -> vm_keys().
populate_keys(Call) ->
    Default = #keys{},
    JObj = kapps_account_config:get(kapps_call:account_id(Call), <<"keys">>),
    #keys{operator = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"operator">>], JObj, Default#keys.operator)
         ,login = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"login">>], JObj, Default#keys.login)
         ,save = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"save">>], JObj, Default#keys.save)
         ,listen = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"listen">>], JObj, Default#keys.listen)
         ,record = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"record">>], JObj, Default#keys.record)
         ,hear_new = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"hear_new">>], JObj, Default#keys.hear_new)
         ,hear_saved = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"hear_saved">>], JObj, Default#keys.hear_saved)
         ,configure = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"configure">>], JObj, Default#keys.configure)
         ,exit = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"exit">>], JObj, Default#keys.exit)
         ,rec_unavailable = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"record_unavailable">>], JObj, Default#keys.rec_unavailable)
         ,rec_name = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"record_name">>], JObj, Default#keys.rec_name)
         ,set_pin = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"set_pin">>], JObj, Default#keys.set_pin)
         ,return_main = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"return_main_menu">>], JObj, Default#keys.return_main)
         ,keep = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"keep">>], JObj, Default#keys.keep)
         ,replay = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"replay">>], JObj, Default#keys.replay)
         ,prev = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"prev">>], JObj, Default#keys.prev)
         ,next = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"next">>], JObj, Default#keys.next)
         ,delete = kz_json:get_binary_value([?KEY_VOICEMAIL, <<"delete">>], JObj, Default#keys.delete)
         }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_mailbox_doc(ne_binary(), api_binary(), kz_json:object(), kapps_call:call()) ->
                             {'ok', kz_json:object()} |
                             {'error', any()}.
get_mailbox_doc(Db, Id, Data, Call) ->
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    CGIsEmpty = kz_util:is_empty(CaptureGroup),
    case kz_util:is_empty(Id) of
        'false' ->
            lager:info("opening ~s", [Id]),
            kz_datamgr:open_doc(Db, Id);
        'true' when not CGIsEmpty ->
            lager:info("capture group not empty: ~s", [CaptureGroup]),
            Opts = [{'key', CaptureGroup}
                   ,'include_docs'
                   ,'first_when_multiple'
                   ],
            case kz_datamgr:get_single_result(Db, <<"attributes/mailbox_number">>, Opts) of
                {'ok', JObj} ->
                    {'ok', kz_json:get_value(<<"doc">>, JObj, kz_json:new())};
                E -> E
            end;
        'true' ->
            get_user_mailbox_doc(Data, Call)
    end.

-spec get_user_mailbox_doc(kz_json:object(), kapps_call:call()) ->
                                  {'ok', kz_json:object()} |
                                  {'error', any()}.
-spec get_user_mailbox_doc(kz_json:object(), kapps_call:call(), api_binary()) ->
                                  {'ok', kz_json:object()} |
                                  {'error', any()}.
get_user_mailbox_doc(Data, Call) ->
    get_user_mailbox_doc(Data, Call, kapps_call:owner_id(Call)).

get_user_mailbox_doc(Data, Call, 'undefined') ->
    DeviceId = kapps_call:authorizing_id(Call),
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), DeviceId) of
        {'ok', DeviceJObj} ->
            case kz_json:get_value(<<"owner_id">>, DeviceJObj) of
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
    SingleMailboxLogin = kz_json:is_true(<<"single_mailbox_login">>, Data, 'false'),
    case kz_attributes:owned_by_docs(OwnerId, <<"vmbox">>, Call) of
        [] ->
            lager:debug("owner ~s has no vmboxes", [OwnerId]),
            {'error', "request voicemail box number"};
        [Box] when SingleMailboxLogin ->
            lager:debug("owner ~s has one vmbox ~s, and single mailbox login is enabled"
                       ,[OwnerId, kz_doc:id(Box)]
                       ),
            {'ok', Box};
        Boxes ->
            lager:debug("found ~p vmboxes assigned to owner ~s",
                        [length(Boxes), OwnerId]),
            maybe_match_callerid(Boxes,Data, Call)
    end.

-spec maybe_match_callerid(kz_json:objects(), kz_json:object(), kapps_call:call()) ->
                                  {'ok', kz_json:object()} |
                                  {'error', any()}.
maybe_match_callerid(Boxes, Data, Call) ->
    case kz_json:is_true(<<"callerid_match_login">>, Data, 'false') of
        'false' ->
            lager:debug("found voicemail boxes but caller-id match disabled", []),
            {'error', "request voicemail box number"};
        'true' ->
            CallerId = kapps_call:caller_id_number(Call),
            try_match_callerid(Boxes, CallerId)
    end.

-spec try_match_callerid(kz_json:objects(), ne_binary()) ->
                                {'ok', kz_json:object()} |
                                {'error', any()}.
try_match_callerid([], _CallerId) ->
    lager:debug("no voicemail box found for owner with matching caller id ~s", [_CallerId]),
    {'error', "request voicemail box number"};
try_match_callerid([Box|Boxes], CallerId) ->
    case kz_json:get_value(<<"mailbox">>, Box) of
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
-spec review_recording(ne_binary(), boolean(), mailbox(), kapps_call:call()) ->
                              {'ok', 'record' | 'save' | 'no_selection'} |
                              {'branch', kz_json:object()}.
-spec review_recording(ne_binary(), boolean(), mailbox(), kapps_call:call(), integer()) ->
                              {'ok', 'record' | 'save' | 'no_selection'} |
                              {'branch', kz_json:object()}.

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

    NoopId = kapps_call_command:prompt(<<"vm-review_recording">>, Call),
    case kapps_call_command:collect_digits(?KEY_LENGTH
                                          ,kapps_call_command:default_collect_timeout()
                                          ,Interdigit
                                          ,NoopId
                                          ,Call
                                          )
    of
        {'ok', Listen} ->
            lager:info("caller chose to replay the recording"),
            _ = kapps_call_command:b_play(AttachmentName, Call),
            review_recording(AttachmentName, AllowOperator, Box, Call);
        {'ok', Record} ->
            lager:info("caller chose to re-record"),
            {'ok', 'record'};
        {'ok', Save} ->
            lager:info("caller chose to save the recording"),
            {'ok', 'save'};
        {'ok', Operator} when AllowOperator ->
            lager:info("caller chose to ring the operator"),
            case cf_util:get_operator_callflow(kapps_call:account_id(Call)) of
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
-spec store_recording(ne_binary(), non_neg_integer(), ne_binary(), mailbox(), kapps_call:call()) -> 'ok' | {'error', kapps_call:call()}.
store_recording(_, Length, _, #mailbox{min_message_length=MinLength}, _)
  when Length < MinLength ->
    lager:info("attachment length is ~B and must be larger than ~B to be stored", [Length, MinLength]);
store_recording(AttachmentName, _Length, DocId, Box, Call) ->
    Url = get_new_attachment_url(AttachmentName, DocId, Box, Call),
    Call1 = kapps_call:kvs_store('media_url', Url, Call),

    lager:debug("storing recording ~s in doc ~s with url ~s", [AttachmentName, DocId, Url]),
    case kapps_call_command:store_file(<<"/tmp/", AttachmentName/binary>>, Url, Call1) of
        'ok' -> 'ok';
        {'error', _R}=E ->
            lager:error("error trying to store media recording: ~p", [_R]),
            {'error', kapps_call:kvs_store('error_details', {'error', E}, Call1)}
    end.

-spec get_new_attachment_url(ne_binary(), ne_binary(), mailbox(), kapps_call:call()) ->
                                    ne_binary().
get_new_attachment_url(AttachmentName, MediaId, #mailbox{owner_id=OwnerId}, Call) ->
    AccountDb = kapps_call:account_db(Call),
    _ = case kz_datamgr:open_doc(AccountDb, MediaId) of
            {'ok', JObj} ->
                maybe_remove_attachments(AccountDb, MediaId, JObj);
            {'error', _} -> 'ok'
        end,
    Opts = props:filter_undefined([{'doc_owner', OwnerId}]),
    kz_media_url:store(AccountDb, {<<"media">>, MediaId}, AttachmentName, Opts).

-spec maybe_remove_attachments(ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
maybe_remove_attachments(AccountDb, MediaId, JObj) ->
    case kz_doc:maybe_remove_attachments(JObj) of
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
-spec min_recording_length(kapps_call:call()) -> integer().
min_recording_length(Call) ->
    case kapps_account_config:get(kapps_call:account_id(Call)
                                 ,?CF_CONFIG_CAT
                                 ,[?KEY_VOICEMAIL, ?KEY_MIN_MESSAGE_SIZE]
                                 )
    of
        'undefined' -> ?MAILBOX_DEFAULT_MSG_MIN_LENGTH;
        MML -> kz_util:to_integer(MML)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec recording_media_doc(ne_binary(), mailbox(), kapps_call:call()) -> ne_binary().
recording_media_doc(Recording, #mailbox{mailbox_number=BoxNum
                                       ,mailbox_id=Id
                                       ,owner_id=OwnerId
                                       }, Call) ->
    AccountDb = kapps_call:account_db(Call),
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
    Doc = kz_doc:update_pvt_parameters(kz_json:from_list(Props), AccountDb, [{'type', <<"media">>}]),
    {'ok', JObj} = kz_datamgr:save_doc(AccountDb, Doc),
    kz_doc:id(JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_doc(kz_json:path()
                ,kz_json:api_json_term()
                ,mailbox() | ne_binary()
                ,kapps_call:call() | ne_binary()
                ) ->
                        'ok' |
                        {'error', atom()}.
update_doc(Key, Value, #mailbox{mailbox_id=Id}, Db) ->
    update_doc(Key, Value, Id, Db);
update_doc(Key, Value, Id, ?NE_BINARY = Db) ->
    case kz_datamgr:open_doc(Db, Id) of
        {'ok', JObj} ->
            case kz_datamgr:save_doc(Db, kz_json:set_value(Key, Value, JObj)) of
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
    update_doc(Key, Value, Id, kapps_call:account_db(Call)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tmp_file(ne_binary()) -> ne_binary().
tmp_file(Ext) ->
    <<(kz_util:rand_hex_binary(16))/binary, ".", Ext/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Accepts Universal Coordinated Time (UTC) and convert it to binary
%% encoded Unix epoch in the provided timezone
%% @end
%%--------------------------------------------------------------------
-spec get_unix_epoch(ne_binary(), ne_binary()) -> ne_binary().
get_unix_epoch(Epoch, Timezone) ->
    UtcDateTime = calendar:gregorian_seconds_to_datetime(kz_util:to_integer(Epoch)),
    LocalDateTime = localtime:utc_to_local(UtcDateTime, Timezone),
    kz_util:to_binary(calendar:datetime_to_gregorian_seconds(LocalDateTime) - ?UNIX_EPOCH_IN_GREGORIAN).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_max_message_length(kz_json:objects()) -> pos_integer().
find_max_message_length([]) -> ?MAILBOX_DEFAULT_MSG_MAX_LENGTH;
find_max_message_length([JObj | T]) ->
    case kz_json:get_integer_value(?KEY_MAX_MESSAGE_LENGTH, JObj) of
        Len when is_integer(Len)
                 andalso Len > 0 -> Len;
        _ -> find_max_message_length(T)
    end.

-spec is_owner(kapps_call:call(), ne_binary()) -> boolean().
is_owner(Call, OwnerId) ->
    case kapps_call:owner_id(Call) of
        <<>> -> 'false';
        'undefined' -> 'false';
        OwnerId -> 'true';
        _Else -> 'false'
    end.

-spec send_mwi_update(mailbox(), kapps_call:call()) -> 'ok'.
send_mwi_update(#mailbox{mailbox_id='undefined'
                        }
               ,_Call) ->
    'ok';
send_mwi_update(#mailbox{owner_id=OwnerId
                        ,mailbox_number=BoxNumber
                        ,account_db=AccountDb
                        ,mailbox_id=BoxId
                        }
               ,Call) ->
    _ = kz_util:spawn(fun cf_util:unsolicited_owner_mwi_update/2, [AccountDb, OwnerId]),
    {New, Saved} = kvm_messages:count_none_deleted(kapps_call:account_id(Call), BoxId),
    _ = kz_util:spawn(fun send_mwi_update/4, [New, Saved, BoxNumber, Call]),
    lager:debug("sent MWI updates for vmbox ~s in account ~s (~b/~b)", [BoxNumber, kapps_call:account_id(Call), New, Saved]).

-spec send_mwi_update(non_neg_integer(), non_neg_integer(), ne_binary(), kapps_call:call()) -> 'ok'.
send_mwi_update(New, Saved, BoxNumber, Call) ->
    Realm = kapps_call:account_realm(Call),
    Command = [{<<"To">>, <<BoxNumber/binary, "@", Realm/binary>>}
              ,{<<"Messages-New">>, New}
              ,{<<"Messages-Saved">>, Saved}
              ,{<<"Call-ID">>, kapps_call:call_id(Call)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("updating MWI for vmbox ~s@~s (~b/~b)", [BoxNumber, Realm, New, Saved]),
    kz_amqp_worker:cast(Command, fun kapi_presence:publish_mwi_update/1).
