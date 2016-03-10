%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
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

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_v(JObj),
    _ = wh_util:put_callid(JObj),

    lager:debug("new voicemail left, sending to email if enabled"),

    AccountDb = wh_json:get_value(<<"Account-DB">>, JObj),

    VMBoxId = wh_json:get_value(<<"Voicemail-Box">>, JObj),
    lager:debug("loading vm box ~s", [VMBoxId]),
    {'ok', VMBox} = kz_datamgr:open_cache_doc(AccountDb, VMBoxId),
    {'ok', UserJObj} = get_owner(AccountDb, VMBox),

    BoxEmails = kzd_voicemail_box:notification_emails(VMBox),
    Emails = maybe_add_user_email(BoxEmails, kzd_user:email(UserJObj)),

    %% If the box has emails, continue processing
    %% andalso the voicemail notification is enabled on the user, continue processing
    %% otherwise stop processing
    case Emails =/= [] andalso
        (kzd_user:voicemail_notification_enabled(UserJObj) orelse wh_json:is_empty(UserJObj))
    of
        'false' -> lager:debug("box ~s has no emails or owner doesn't want emails", [VMBoxId]);
        'true' -> continue_processing(JObj, AccountDb, VMBox, Emails)
    end.

-spec continue_processing(wh_json:object(), ne_binary(), wh_json:object(), ne_binaries()) -> 'ok'.
continue_processing(JObj, AccountDb, VMBox, Emails) ->
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    AccountDb = wh_json:get_value(<<"Account-DB">>, JObj),


    'ok' = notify_util:send_update(RespQ, MsgId, <<"pending">>),
    lager:debug("VM->Email enabled for user, sending to ~p", [Emails]),
    {'ok', AccountJObj} = kz_account:fetch(AccountDb),
    Timezone = kzd_voicemail_box:timezone(VMBox, <<"UTC">>),

    Props = [{<<"email_address">>, Emails}
             | create_template_props(JObj, Timezone, AccountJObj)
            ],

    CustomTxtTemplate = wh_json:get_value(?EMAIL_TXT_TEMPLATE_KEY, AccountJObj),
    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = wh_json:get_value(?EMAIL_HTML_TEMPLATE_KEY, AccountJObj),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = wh_json:get_value(?EMAIL_SUBJECT_TEMPLATE_KEY, AccountJObj),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    build_and_send_email(TxtBody, HTMLBody, Subject, Emails
                         ,props:filter_undefined(Props)
                         ,{RespQ, MsgId}
                        ).

-spec maybe_add_user_email(ne_binaries(), api_binary()) -> ne_binaries().
maybe_add_user_email(BoxEmails, 'undefined') -> BoxEmails;
maybe_add_user_email(BoxEmails, UserEmail) -> [UserEmail | BoxEmails].

-spec get_owner(ne_binary(), kzd_voicemail_box:doc(), api_binary()) ->
                       {'ok', kzd_user:doc()}.
get_owner(AccountDb, VMBox) ->
    get_owner(AccountDb, VMBox, kzd_voicemail_box:owner_id(VMBox)).
get_owner(_AccountDb, _VMBox, 'undefined') ->
    lager:debug("no owner of voicemail box ~s, using empty owner", [wh_doc:id(_VMBox)]),
    {'ok', wh_json:new()};
get_owner(AccountDb, _VMBox, OwnerId) ->
    lager:debug("attempting to load owner: ~s", [OwnerId]),
    {'ok', _} = kz_datamgr:open_cache_doc(AccountDb, OwnerId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object(), ne_binary(), wh_json:object()) -> wh_proplist().
create_template_props(Event, Timezone, Account) ->
    CIDName = wh_json:get_value(<<"Caller-ID-Name">>, Event),
    CIDNum = wh_json:get_value(<<"Caller-ID-Number">>, Event),
    ToE164 = wh_json:get_value(<<"To-User">>, Event),
    FromE164 = wh_json:get_value(<<"From-User">>, Event),
    DateCalled = wh_json:get_integer_value(<<"Voicemail-Timestamp">>, Event),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    ClockTimezone = whapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    [{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"service">>, notify_util:get_service_props(Event, Account, ?MOD_CONFIG_CAT)}
     ,{<<"voicemail">>, props:filter_undefined(
                          [{<<"caller_id_number">>, knm_util:pretty_print(CIDNum)}
                           %% sometimes the name is a number...
                           ,{<<"caller_id_name">>, knm_util:pretty_print(CIDName)}
                           ,{<<"date_called_utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
                           ,{<<"date_called">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
                           ,{<<"from_user">>, knm_util:pretty_print(FromE164)}
                           ,{<<"from_realm">>, wh_json:get_value(<<"From-Realm">>, Event)}
                           ,{<<"to_user">>, knm_util:pretty_print(ToE164)}
                           ,{<<"to_realm">>, wh_json:get_value(<<"To-Realm">>, Event)}
                           ,{<<"box">>, wh_json:get_value(<<"Voicemail-Box">>, Event)}
                           ,{<<"media">>, wh_json:get_value(<<"Voicemail-Name">>, Event)}
                           ,{<<"length">>, preaty_print_length(Event)}
                           ,{<<"transcription">>, wh_json:get_value([<<"Voicemail-Transcription">>, <<"text">>], Event)}
                           ,{<<"call_id">>, wh_json:get_value(<<"Call-ID">>, Event)}
                           ,{<<"magic_hash">>, magic_hash(Event)}
                          ])}
     ,{<<"account_db">>, wh_doc:account_db(Account)}
    ].

-spec magic_hash(wh_json:object()) -> api_binary().
magic_hash(Event) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, Event),
    VMBoxId = wh_json:get_value(<<"Voicemail-Box">>, Event),
    MessageId = wh_json:get_value(<<"Voicemail-Name">>, Event),

    try list_to_binary([<<"/v1/accounts/">>, AccountId, <<"/vmboxes/">>, VMBoxId
                        ,<<"/messages/">>, MessageId, <<"/raw">>
                       ])
    of
        URL -> whapps_util:to_magic_hash(URL)
    catch
        _:_ -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-type respond_to() :: {api_binary(), ne_binary()}.
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binaries(), wh_proplist(), respond_to()) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props, {RespQ, MsgId}) ->
    Voicemail = props:get_value(<<"voicemail">>, Props),
    Service = props:get_value(<<"service">>, Props),
    DB = props:get_value(<<"account_db">>, Props),
    DocId = props:get_value(<<"media">>, Voicemail),

    From = props:get_value(<<"send_from">>, Service),

    {ContentTypeParams, CharsetString} = notify_util:get_charset_params(Service),

    lager:debug("attempting to attach media ~s in ~s", [DocId, DB]),
    {'ok', VMJObj} = kz_datamgr:open_doc(DB, DocId),

    [AttachmentId] = wh_doc:attachment_names(VMJObj),
    lager:debug("attachment id ~s", [AttachmentId]),
    {'ok', AttachmentBin} = kz_datamgr:fetch_attachment(DB, DocId, AttachmentId),

    AttachmentFileName = get_file_name(VMJObj, Props),
    lager:debug("attachment renamed to ~s", [AttachmentFileName]),

    PlainTransferEncoding = whapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"text_content_transfer_encoding">>, <<"7BIT">>),
    HTMLTransferEncoding = whapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"html_content_transfer_encoding">>, <<"7BIT">>),

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
    case [notify_util:send_email(From, T, Email) || {T, Email} <- Emails] of
        ['ok'|_] -> notify_util:send_update(RespQ, MsgId, <<"completed">>);
        [{'error', Reason}|_] -> notify_util:send_update(RespQ, MsgId, <<"failed">>, Reason)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a friendly file name
%% @end
%%--------------------------------------------------------------------
-spec get_file_name(wh_json:object(), wh_proplist()) -> ne_binary().
get_file_name(MediaJObj, Props) ->
    %% CallerID_Date_Time.mp3
    Voicemail = props:get_value(<<"voicemail">>, Props),
    CallerID =
        case {props:get_value(<<"caller_id_name">>, Voicemail)
              ,props:get_value(<<"caller_id_number">>, Voicemail)
             }
        of
            {'undefined', 'undefined'} -> <<"Unknown">>;
            {'undefined', Num} -> knm_util:pretty_print(wh_util:to_binary(Num));
            {Name, _} -> knm_util:pretty_print(wh_util:to_binary(Name))
        end,

    LocalDateTime = props:get_value(<<"date_called">>, Voicemail, <<"0000-00-00_00-00-00">>),
    Extension = get_extension(MediaJObj),
    FName = list_to_binary([CallerID, "_", wh_util:pretty_print_datetime(LocalDateTime), ".", Extension]),

    binary:replace(wh_util:to_lower_binary(FName), <<" ">>, <<"_">>).

-spec get_extension(wh_json:object()) -> ne_binary().
get_extension(MediaJObj) ->
    case wh_json:get_value(<<"media_type">>, MediaJObj) of
        'undefined' ->
            lager:debug("getting extension from attachment mime"),
            attachment_to_extension(wh_doc:attachments(MediaJObj));
        MediaType -> MediaType
    end.

-spec attachment_to_extension(wh_json:object()) -> ne_binary().
attachment_to_extension(AttachmentsJObj) ->
    wh_json:get_value(<<"extension">>
                      ,wh_json:map(fun attachment_to_extension/2, AttachmentsJObj)
                      ,whapps_config:get(<<"callflow">>, [<<"voicemail">>, <<"extension">>], <<"mp3">>)
                     ).

-spec attachment_to_extension(ne_binary(), wh_json:object()) -> {ne_binary(), ne_binary()}.
attachment_to_extension(_Id, Meta) ->
    CT = wh_json:get_value(<<"content_type">>, Meta),
    Ext = mime_to_extension(CT),
    {<<"extension">>, Ext}.

-spec mime_to_extension(ne_binary()) -> ne_binary().
mime_to_extension(<<"audio/mpeg">>) -> <<"mp3">>;
mime_to_extension(_) -> <<"wav">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec preaty_print_length(integer() | api_object()) -> ne_binary().
preaty_print_length('undefined') ->
    <<"00:00">>;
preaty_print_length(Milliseconds) when is_integer(Milliseconds) ->
    Seconds = round(Milliseconds / ?MILLISECONDS_IN_SECOND) rem 60,
    Minutes = trunc(Milliseconds / ?MILLISECONDS_IN_MINUTE) rem 60,
    wh_util:to_binary(io_lib:format("~2..0w:~2..0w", [Minutes, Seconds]));
preaty_print_length(Event) ->
    preaty_print_length(wh_json:get_integer_value(<<"Voicemail-Length">>, Event)).
