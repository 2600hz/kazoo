%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti <james@2600hz.org>
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_voicemail_to_email).

-export([init/0
        ,handle_req/2
        ]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_voicemail_to_email_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_voicemail_to_email_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_voicemail_to_email_subj_tmpl').

-define(EMAIL_TXT_TEMPLATE_KEY, [<<"notifications">>, <<"voicemail_to_email">>, <<"email_text_template">>]).
-define(EMAIL_HTML_TEMPLATE_KEY, [<<"notifications">>, <<"voicemail_to_email">>, <<"email_html_template">>]).
-define(EMAIL_SUBJECT_TEMPLATE_KEY, [<<"notifications">>, <<"voicemail_to_email">>, <<"email_subject_template">>]).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".voicemail_to_email">>).

-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:voicemail_new_v(JObj),
    _ = kz_util:put_callid(JObj),

    lager:debug("new voicemail left, sending to email if enabled"),

    RespQ = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    notify_util:send_update(RespQ, MsgId, <<"pending">>),

    AccountDb = kz_util:format_account_db(kz_json:get_value(<<"Account-ID">>, JObj)),

    VMBoxId = kz_json:get_value(<<"Voicemail-Box">>, JObj),
    lager:debug("loading vm box ~s", [VMBoxId]),
    {'ok', VMBox} = kz_datamgr:open_cache_doc(AccountDb, VMBoxId),
    {'ok', UserJObj} = get_owner(AccountDb, VMBox),

    BoxEmails = kzd_voicemail_box:notification_emails(VMBox),
    Emails = maybe_add_user_email(BoxEmails, kzd_user:email(UserJObj), kzd_user:voicemail_notification_enabled(UserJObj)),

    %% If the box has emails, continue processing
    %% otherwise stop processing
    SendResult =
        case Emails =/= [] of
            'false' -> lager:debug("box ~s has no emails or owner doesn't want emails", [VMBoxId]);
            'true' -> continue_processing(JObj, AccountDb, VMBox, Emails)
        end,
    notify_util:maybe_send_update(SendResult, RespQ, MsgId).

-spec continue_processing(kz_json:object(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binaries()) -> send_email_return().
continue_processing(JObj, AccountDb, VMBox, Emails) ->
    AccountDb = kz_util:format_account_db(kz_json:get_value(<<"Account-ID">>, JObj)),

    lager:debug("VM->Email enabled for user, sending to ~p", [Emails]),
    {'ok', AccountJObj} = kz_account:fetch(AccountDb),
    Timezone = kzd_voicemail_box:timezone(VMBox, <<"UTC">>),

    Props = [{<<"email_address">>, Emails}
             | create_template_props(JObj, Timezone, AccountJObj)
            ],

    CustomTxtTemplate = kz_json:get_value(?EMAIL_TXT_TEMPLATE_KEY, AccountJObj),
    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = kz_json:get_value(?EMAIL_HTML_TEMPLATE_KEY, AccountJObj),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = kz_json:get_value(?EMAIL_SUBJECT_TEMPLATE_KEY, AccountJObj),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    build_and_send_email(TxtBody, HTMLBody, Subject, Emails, props:filter_undefined(Props)).

-spec maybe_add_user_email(kz_term:ne_binaries(), kz_term:api_binary(), boolean()) -> kz_term:ne_binaries().
maybe_add_user_email(BoxEmails, 'undefined', _) -> BoxEmails;
maybe_add_user_email(BoxEmails, _UserEmail, 'false') -> BoxEmails;
maybe_add_user_email(BoxEmails, UserEmail, 'true') -> [UserEmail | BoxEmails].

-spec get_owner(kz_term:ne_binary(), kzd_voicemail_box:doc(), kz_term:api_binary()) ->
                       {'ok', kzd_user:doc()}.
get_owner(AccountDb, VMBox) ->
    get_owner(AccountDb, VMBox, kzd_voicemail_box:owner_id(VMBox)).
get_owner(_AccountDb, _VMBox, 'undefined') ->
    lager:debug("no owner of voicemail box ~s, using empty owner", [kz_doc:id(_VMBox)]),
    {'ok', kz_json:new()};
get_owner(AccountDb, _VMBox, OwnerId) ->
    lager:debug("attempting to load owner: ~s", [OwnerId]),
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', _}=OK -> OK;
        {'error', _} -> {'ok', kz_json:new()}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
create_template_props(Event, Timezone, Account) ->
    CIDName = kz_json:get_value(<<"Caller-ID-Name">>, Event),
    CIDNum = kz_json:get_value(<<"Caller-ID-Number">>, Event),
    ToE164 = kz_json:get_value(<<"To-User">>, Event),
    FromE164 = kz_json:get_value(<<"From-User">>, Event),
    DateCalled = kz_json:get_integer_value(<<"Voicemail-Timestamp">>, Event),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    ClockTimezone = kapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    [{<<"account">>, notify_util:json_to_template_props(Account)}
    ,{<<"service">>, notify_util:get_service_props(Event, Account, ?MOD_CONFIG_CAT)}
    ,{<<"voicemail">>, props:filter_undefined(
                         [{<<"caller_id_number">>, knm_util:pretty_print(CIDNum)}
                          %% sometimes the name is a number...
                         ,{<<"caller_id_name">>, knm_util:pretty_print(CIDName)}
                         ,{<<"date_called_utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
                         ,{<<"date_called">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
                         ,{<<"from_user">>, knm_util:pretty_print(FromE164)}
                         ,{<<"from_realm">>, kz_json:get_value(<<"From-Realm">>, Event)}
                         ,{<<"to_user">>, knm_util:pretty_print(ToE164)}
                         ,{<<"to_realm">>, kz_json:get_value(<<"To-Realm">>, Event)}
                         ,{<<"box">>, kz_json:get_value(<<"Voicemail-Box">>, Event)}
                         ,{<<"media">>, kz_json:get_value(<<"Voicemail-ID">>, Event)}
                         ,{<<"length">>, preaty_print_length(Event)}
                         ,{<<"transcription">>, kz_json:get_value([<<"Voicemail-Transcription">>, <<"text">>], Event)}
                         ,{<<"call_id">>, kz_json:get_value(<<"Call-ID">>, Event)}
                         ,{<<"magic_hash">>, magic_hash(Event)}
                         ])}
    ,{<<"account_id">>, kz_doc:account_id(Account)}
    ].

-spec magic_hash(kz_json:object()) -> kz_term:api_binary().
magic_hash(Event) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, Event),
    VMBoxId = kz_json:get_value(<<"Voicemail-Box">>, Event),
    MessageId = kz_json:get_value(<<"Voicemail-ID">>, Event),

    try list_to_binary([<<"/v1/accounts/">>, AccountId, <<"/vmboxes/">>, VMBoxId
                       ,<<"/messages/">>, MessageId, <<"/raw">>
                       ])
    of
        URL -> kapps_util:to_magic_hash(URL)
    catch
        _:_ -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), kz_term:ne_binaries(), kz_term:proplist()) -> send_email_return().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Voicemail = props:get_value(<<"voicemail">>, Props),
    Service = props:get_value(<<"service">>, Props),
    AccountId = props:get_value(<<"account_id">>, Props),
    DocId = props:get_value(<<"media">>, Voicemail),
    DB = kvm_util:get_db(AccountId, DocId),

    From = props:get_value(<<"send_from">>, Service),

    {ContentTypeParams, CharsetString} = notify_util:get_charset_params(Service),

    lager:debug("attempting to attach media ~s in ~s", [DocId, DB]),
    {'ok', VMJObj} = kvm_message:fetch(AccountId, DocId),

    [AttachmentId] = kz_doc:attachment_names(VMJObj),
    lager:debug("attachment id ~s", [AttachmentId]),
    {'ok', AttachmentBin} = kz_datamgr:fetch_attachment(DB, DocId, AttachmentId),

    AttachmentFileName = get_file_name(VMJObj, Props),
    lager:debug("attachment renamed to ~s", [AttachmentFileName]),

    PlainTransferEncoding = kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"text_content_transfer_encoding">>, <<"7BIT">>),
    HTMLTransferEncoding = kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"html_content_transfer_encoding">>, <<"7BIT">>),

    %% Content Type, Subtype, Headers, Parameters, Body
    Emails = [{T
              ,{<<"multipart">>, <<"mixed">>
               ,[{<<"From">>, From}
                ,{<<"To">>, T}
                ,{<<"Subject">>, Subject}
                ,{<<"X-Call-ID">>, props:get_value(<<"call_id">>, Voicemail)}
                ]
               ,ContentTypeParams
               ,[{<<"multipart">>, <<"alternative">>, [], []
                 ,[{<<"text">>, <<"plain">>
                   ,props:filter_undefined(
                      [{<<"Content-Type">>, iolist_to_binary([<<"text/plain">>, CharsetString])}
                      ,{<<"Content-Transfer-Encoding">>, PlainTransferEncoding}
                      ])
                   ,[], iolist_to_binary(TxtBody)}
                  ,{<<"text">>, <<"html">>
                   ,props:filter_undefined(
                      [{<<"Content-Type">>, iolist_to_binary([<<"text/html">>, CharsetString])}
                      ,{<<"Content-Transfer-Encoding">>, HTMLTransferEncoding}
                      ])
                   ,[], iolist_to_binary(HTMLBody)}
                  ]
                 }
                ,{<<"audio">>, <<"mpeg">>
                 ,[{<<"Content-Disposition">>, list_to_binary([<<"attachment; filename=\"">>, AttachmentFileName, "\""])}
                  ,{<<"Content-Type">>, list_to_binary([<<"audio/mpeg; name=\"">>, AttachmentFileName, "\""])}
                  ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
                  ]
                 ,[], AttachmentBin
                 }
                ]
               }
              }
              || T <- To
             ],
    [notify_util:send_email(From, T, Email) || {T, Email} <- Emails].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a friendly file name
%% @end
%%--------------------------------------------------------------------
-spec get_file_name(kz_json:object(), kz_term:proplist()) -> kz_term:ne_binary().
get_file_name(MediaJObj, Props) ->
    %% CallerID_Date_Time.mp3
    Voicemail = props:get_value(<<"voicemail">>, Props),
    CallerID =
        case {props:get_value(<<"caller_id_name">>, Voicemail)
             ,props:get_value(<<"caller_id_number">>, Voicemail)
             }
        of
            {'undefined', 'undefined'} -> <<"Unknown">>;
            {'undefined', Num} -> knm_util:pretty_print(kz_term:to_binary(Num));
            {Name, _} -> knm_util:pretty_print(kz_term:to_binary(Name))
        end,

    LocalDateTime = props:get_value(<<"date_called">>, Voicemail, <<"0000-00-00_00-00-00">>),
    Extension = get_extension(MediaJObj),
    FName = list_to_binary([CallerID, "_", kz_time:pretty_print_datetime(LocalDateTime), ".", Extension]),

    binary:replace(kz_term:to_lower_binary(FName), <<" ">>, <<"_">>).

-spec get_extension(kz_json:object()) -> kz_term:ne_binary().
get_extension(MediaJObj) ->
    case kz_json:get_value(<<"media_type">>, MediaJObj) of
        'undefined' ->
            lager:debug("getting extension from attachment mime"),
            attachment_to_extension(kz_doc:attachments(MediaJObj));
        MediaType -> MediaType
    end.

-spec attachment_to_extension(kz_json:object()) -> kz_term:ne_binary().
attachment_to_extension(AttachmentsJObj) ->
    kz_json:get_value(<<"extension">>
                     ,kz_json:map(fun attachment_to_extension/2, AttachmentsJObj)
                     ,kapps_config:get_ne_binary(<<"callflow">>, [<<"voicemail">>, <<"extension">>], <<"mp3">>)
                     ).

-spec attachment_to_extension(kz_term:ne_binary(), kz_json:object()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
attachment_to_extension(_Id, Meta) ->
    CT = kz_json:get_value(<<"content_type">>, Meta),
    Ext = mime_to_extension(CT),
    {<<"extension">>, Ext}.

-spec mime_to_extension(kz_term:ne_binary()) -> kz_term:ne_binary().
mime_to_extension(<<"audio/mpeg">>) -> <<"mp3">>;
mime_to_extension(_) -> <<"wav">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec preaty_print_length(integer() | kz_term:api_object()) -> kz_term:ne_binary().
preaty_print_length('undefined') ->
    <<"00:00">>;
preaty_print_length(Milliseconds) when is_integer(Milliseconds) ->
    Seconds = round(Milliseconds / ?MILLISECONDS_IN_SECOND) rem 60,
    Minutes = trunc(Milliseconds / ?MILLISECONDS_IN_MINUTE) rem 60,
    kz_term:to_binary(io_lib:format("~2..0w:~2..0w", [Minutes, Seconds]));
preaty_print_length(Event) ->
    preaty_print_length(kz_json:get_integer_value(<<"Voicemail-Length">>, Event)).
