%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_voicemail_to_email).
-behaviour(teletype_gen_email_template).

-export([id/0
        ,init/0
        ,macros/0, macros/1
        ,subject/0
        ,category/0
        ,friendly_name/0
        ]).
-export([handle_new_voicemail/1]).

-include("teletype.hrl").

-spec id() -> ne_binary().
id() ->
    <<"voicemail_to_email">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"voicemail.vmbox_id">>, <<"voicemail_vmbox_id">>, <<"Voicemail Box Id">>, <<"Which voicemail box was the message left in">>)
      ,?MACRO_VALUE(<<"voicemail.msg_id">>, <<"voicemail_msg_id">>, <<"Voicemail Message ID">>, <<"Message Id of the voicemail">>)
      ,?MACRO_VALUE(<<"voicemail.transcription">>, <<"voicemail_transcription">>, <<"Voicemail Message Transcription">>, <<"Voicemail Message Transcription">>)
      ,?MACRO_VALUE(<<"voicemail.length">>, <<"voicemail_length">>, <<"Voicemail Length">>, <<"Length of the voicemail file (formated in HH:MM:SS)">>)
      ,?MACRO_VALUE(<<"voicemail.file_name">>, <<"voicemail_file_name">>, <<"Voicemail File Name">>, <<"Name of the voicemail file">>)
      ,?MACRO_VALUE(<<"voicemail.file_type">>, <<"voicemail_file_type">>, <<"Voicemail File Type">>, <<"Type of the voicemail file">>)
      ,?MACRO_VALUE(<<"voicemail.file_size">>, <<"voicemail_file_size">>, <<"Voicemail File Size">>, <<"Size of the voicemail file in bytes">>)
       | ?DEFAULT_CALL_MACROS
       ++ ?USER_MACROS
       ++ ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> ne_binary().
subject() ->
    <<"New voicemail from {{caller_id.name}} ({{caller_id.number}})">>.

-spec category() -> ne_binary().
category() ->
    <<"voicemail">>.

-spec friendly_name() -> ne_binary().
friendly_name() ->
    <<"Voicemail To Email">>.

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?MODULE),
    teletype_bindings:bind(id(), ?MODULE, 'handle_new_voicemail').

-spec handle_new_voicemail(kz_json:object()) -> 'ok'.
handle_new_voicemail(JObj) ->
    'true' = kapi_notifications:voicemail_new_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, id()) of
        'false' -> teletype_util:notification_disabled(DataJObj, id());
        'true' -> handle_req(DataJObj, AccountId)
    end.

-spec handle_req(kz_json:object(), ne_binary()) -> 'ok'.
handle_req(DataJObj, AccountId) ->
    VMBoxId = kz_json:get_value(<<"voicemail_box">>, DataJObj),
    {'ok', VMBox} = teletype_util:open_doc(<<"vmbox">>, VMBoxId, DataJObj),
    {'ok', UserJObj} = get_owner(VMBox, DataJObj),
    BoxEmails = kzd_voicemail_box:notification_emails(VMBox),
    Emails = maybe_add_user_email(BoxEmails, kzd_user:email(UserJObj), kzd_user:voicemail_notification_enabled(UserJObj)),
    {'ok', AccountJObj} = teletype_util:open_doc(<<"account">>, AccountId, DataJObj),

    Values = [{<<"vmbox">>, VMBox}
             ,{<<"user">>, UserJObj}
             ,{<<"account">>, AccountJObj}
             ,{<<"to">>, Emails}
             ],
    ReqData = kz_json:set_values(Values, DataJObj),

    HasEmails = Emails =/= [],
    case teletype_util:is_preview(DataJObj) of
        'false' -> maybe_process_req(ReqData, HasEmails);
        'true' ->
            maybe_process_req(kz_json:merge_jobjs(DataJObj, ReqData), HasEmails)
    end.

-spec maybe_add_user_email(ne_binaries(), api_binary(), boolean()) -> ne_binaries().
maybe_add_user_email(BoxEmails, 'undefined', _) -> BoxEmails;
maybe_add_user_email(BoxEmails, UserEmail, 'false') -> lists:delete(UserEmail, BoxEmails);
maybe_add_user_email(BoxEmails, UserEmail, 'true') -> [UserEmail | BoxEmails].

-spec get_owner(kzd_voicemail_box:doc(), kz_json:object()) -> {'ok', kz_json:object()}.
get_owner(VMBox, DataJObj) ->
    case teletype_util:open_doc(<<"user">>, kzd_voicemail_box:owner_id(VMBox), DataJObj) of
        {'ok', _}=OK -> OK;
        {'error', 'empty_doc_id'} -> {'ok', kz_json:new()}
    end.

-spec maybe_process_req(kz_json:object(), boolean()) -> 'ok'.
maybe_process_req(DataJObj, false) ->
    lager:debug("box ~s has no emails or owner doesn't want emails"
               ,[kz_json:get_value(<<"voicemail_box">>, DataJObj)]),
    teletype_util:send_update(DataJObj, <<"completed">>);
maybe_process_req(DataJObj, true) ->
    process_req(DataJObj).

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    teletype_util:send_update(DataJObj, <<"pending">>),
    Macros = macros(DataJObj),

    %% Load templates
    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], subject()),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, teletype_util:mod_config_cat(id())),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates, email_attachments(DataJObj)) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec macros(kz_json:object()) -> kz_proplist().
macros(DataJObj) ->
    TemplateData = template_data(DataJObj),
    EmailAttachements = email_attachments(DataJObj, TemplateData),
    maybe_add_file_data(TemplateData, EmailAttachements).

-spec template_data(kz_json:object()) -> kz_proplist().
template_data(DataJObj) ->
    [{<<"system">>, teletype_util:system_params()}
     | build_template_data(DataJObj)
    ].

-spec email_attachments(kz_json:object()) -> attachments().
-spec email_attachments(kz_json:object(), kz_proplist()) -> attachments().
-spec email_attachments(kz_json:object(), kz_proplist(), boolean()) -> attachments().
email_attachments(DataJObj) ->
    email_attachments(DataJObj, template_data(DataJObj)).

email_attachments(DataJObj, Macros) ->
    email_attachments(DataJObj, Macros, teletype_util:is_preview(DataJObj)).

email_attachments(_DataJObj, _Macros, 'true') -> [];
email_attachments(DataJObj, Macros, 'false') ->
    VMId = kz_json:get_value(<<"voicemail_id">>, DataJObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    DB = kvm_util:get_db(AccountId, VMId),
    {'ok', VMJObj} = kvm_message:fetch(AccountId, VMId),

    {[AttachmentMeta], [AttachmentId]} = kz_json:get_values(kz_doc:attachments(VMJObj)),
    {'ok', AttachmentBin} = kz_datamgr:fetch_attachment(DB, VMId, AttachmentId),

    [{kz_json:get_value(<<"content_type">>, AttachmentMeta)
     ,get_file_name(VMJObj, Macros)
     ,AttachmentBin
     }].

-spec get_file_name(kz_json:object(), kz_proplist()) -> ne_binary().
get_file_name(MediaJObj, Macros) ->
    %% CallerID_Date_Time.mp3
    CallerID =
        case {props:get_value([<<"caller_id">>, <<"name">>], Macros)
             ,props:get_value([<<"caller_id">>, <<"number">>], Macros)
             }
        of
            {'undefined', 'undefined'} -> <<"Unknown">>;
            {'undefined', Num} -> knm_util:pretty_print(kz_term:to_binary(Num));
            {Name, _} -> knm_util:pretty_print(kz_term:to_binary(Name))
        end,

    LocalDateTime = props:get_value([<<"date_called">>, <<"local">>], Macros),

    Extension = get_extension(MediaJObj),
    FileName = iolist_to_binary([CallerID, $_, kz_time:pretty_print_datetime(LocalDateTime), $., Extension]),

    kz_http_util:urlencode(
      binary:replace(kz_term:to_lower_binary(FileName), <<$\s>>, <<$_>>)
     ).

-spec get_extension(kz_json:object()) -> ne_binary().
get_extension(MediaJObj) ->
    kz_mime:to_extension(kz_doc:attachment_content_type(MediaJObj)).

-spec build_template_data(kz_json:object()) -> kz_proplist().
build_template_data(DataJObj) ->
    Timezone = kzd_voicemail_box:timezone(kz_json:get_value(<<"vmbox">>, DataJObj)),
    [{<<"voicemail">>, build_voicemail_data(DataJObj)}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"user">>, teletype_util:user_params(kz_json:get_value(<<"user">>, DataJObj))}
    ,{<<"owner">>, teletype_util:user_params(kz_json:get_value(<<"user">>, DataJObj))}
     | teletype_util:build_call_data(DataJObj, Timezone)
    ].

-spec build_voicemail_data(kz_json:object()) -> kz_proplist().
build_voicemail_data(DataJObj) ->
    props:filter_undefined(
      [{<<"vmbox_id">>, kz_json:get_value(<<"voicemail_box">>, DataJObj)}
      ,{<<"box">>, kz_json:get_value(<<"voicemail_box">>, DataJObj)} %% backward compatibility
      ,{<<"vmbox_name">>, kz_json:get_value([<<"vmbox">>, <<"name">>], DataJObj)}
      ,{<<"vmbox_number">>, kz_json:get_value([<<"vmbox">>, <<"mailbox">>], DataJObj)}
      ,{<<"msg_id">>, kz_json:get_value(<<"voicemail_id">>, DataJObj)}
      ,{<<"name">>, kz_json:get_value(<<"voicemail_id">>, DataJObj)} %% backward compatibility
      ,{<<"transcription">>, kz_json:get_value([<<"voicemail_transcription">>, <<"text">>], DataJObj)}
      ,{<<"length">>, pretty_print_length(DataJObj)}
      ]).

-spec pretty_print_length(api_object() | pos_integer()) -> ne_binary().
pretty_print_length('undefined') -> <<"00:00:00">>;
pretty_print_length(Ms) when is_integer(Ms) ->
    MilliSeconds = kz_time:milliseconds_to_seconds(Ms),
    Us = kz_time:unix_seconds_to_gregorian_seconds(MilliSeconds),
    {_, {H, M, S}} = calendar:gregorian_seconds_to_datetime(Us),
    kz_term:to_binary(io_lib:format("~2..0w:~2..0w:~2..0w", [H, M, S]));
pretty_print_length(JObj) ->
    pretty_print_length(kz_json:get_integer_value(<<"voicemail_length">>, JObj)).

-spec maybe_add_file_data(kz_proplist(), attachments()) -> kz_proplist().
maybe_add_file_data(Macros, []) -> Macros;
maybe_add_file_data(Macros, [{ContentType, Filename, Bin}]) ->
    Props = props:filter_undefined(
              [{<<"file_name">>, Filename}
              ,{<<"file_type">>, kz_mime:to_extension(ContentType)}
              ,{<<"file_size">>, erlang:size(Bin)}
              ]),
    VMF = props:set_values(Props, props:get_value(<<"voicemail">>, Macros, [])),
    props:set_value(<<"voicemail">>, VMF, Macros).
