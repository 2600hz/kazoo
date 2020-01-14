%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_voicemail_deleted).
-behaviour(teletype_gen_email_template).

-export([id/0
        ,init/0
        ,macros/0, macros/1
        ,subject/0
        ,category/0
        ,friendly_name/0
        ,to/0, from/0, cc/0, bcc/0, reply_to/0
        ]).
-export([handle_req/1]).

-include("teletype.hrl").

-spec id() -> kz_term:ne_binary().
id() ->
    <<"voicemail_deleted">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"voicemail.vmbox_id">>, <<"voicemail_vmbox_id">>, <<"Voicemail Box Id">>, <<"Which voicemail box was the message left in">>)
      ,?MACRO_VALUE(<<"voicemail.msg_id">>, <<"voicemail_msg_id">>, <<"Voicemail Message ID">>, <<"Message Id of the voicemail">>)
      ,?MACRO_VALUE(<<"voicemail.transcription">>, <<"voicemail_transcription">>, <<"Voicemail Message Transcription">>, <<"Voicemail Message Transcription">>)
      ,?MACRO_VALUE(<<"voicemail.length">>, <<"voicemail_length">>, <<"Voicemail Length">>, <<"Length of the voicemail file (formatted in HH:MM:SS)">>)
      ,?MACRO_VALUE(<<"voicemail.file_name">>, <<"voicemail_file_name">>, <<"Voicemail File Name">>, <<"Name of the voicemail file">>)
      ,?MACRO_VALUE(<<"voicemail.file_type">>, <<"voicemail_file_type">>, <<"Voicemail File Type">>, <<"Type of the voicemail file">>)
      ,?MACRO_VALUE(<<"voicemail.file_size">>, <<"voicemail_file_size">>, <<"Voicemail File Size">>, <<"Size of the voicemail file in bytes">>)
      ,?MACRO_VALUE(<<"reason">>, <<"voicemail_delete_reason">>, <<"Voicemail Delete Reason">>, <<"Why the voicemail was deleted">>)
       | ?DEFAULT_CALL_MACROS
       ++ ?USER_MACROS
       ++ ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> kz_term:ne_binary().
subject() -> <<"Voicemail from {{caller_id.name_number}} was deleted">>.

-spec category() -> kz_term:ne_binary().
category() -> <<"voicemail">>.

-spec friendly_name() -> kz_term:ne_binary().
friendly_name() -> <<"Deleted Voicemail To Email">>.

-spec to() -> kz_json:object().
to() -> ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL).

-spec from() -> kz_term:api_ne_binary().
from() -> teletype_util:default_from_address().

-spec cc() -> kz_json:object().
cc() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec bcc() -> kz_json:object().
bcc() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec reply_to() -> kz_term:api_ne_binary().
reply_to() -> teletype_util:default_reply_to().

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?MODULE),
    teletype_bindings:bind(<<"voicemail_deleted">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:voicemail_deleted_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [id()]),
    teletype_util:notification_failed(id(), <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [id()]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case is_notice_enabled(AccountId, JObj) of
        'false' -> teletype_util:notification_disabled(DataJObj, id());
        'true' -> process_req(DataJObj)
    end.

-spec is_notice_enabled(kz_term:ne_binary(), kz_json:object()) -> boolean().
is_notice_enabled(AccountId, JObj) ->
    case teletype_util:is_notice_enabled(AccountId, JObj, id()) of
        'false' -> 'false';
        'true' ->
            teletype_util:template_system_value(id(), <<"is_enabled">>, 'false')
    end.

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    VMBoxJObj = get_vmbox(DataJObj),
    UserJObj = get_owner(VMBoxJObj, DataJObj),
    BoxEmails = kzd_voicemail_box:notification_emails(VMBoxJObj),
    Emails = maybe_add_user_email(BoxEmails, kzd_user:email(UserJObj), kzd_user:voicemail_notification_enabled(UserJObj)),
    Values = [{<<"vmbox_doc">>, VMBoxJObj}
             ,{<<"user">>, UserJObj}
             ,{<<"to">>, Emails}
             ],
    ReqData = kz_json:set_values(Values, DataJObj),

    case teletype_util:is_preview(DataJObj) of
        'false' -> maybe_process_req(ReqData);
        'true' ->
            maybe_process_req(kz_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec get_vmbox(kz_json:object()) -> kz_json:object().
get_vmbox(DataJObj) ->
    VMBoxId = kz_json:get_value(<<"voicemail_box">>, DataJObj),
    case teletype_util:open_doc(<<"vmbox">>, VMBoxId, DataJObj) of
        {'ok', JObj} -> JObj;
        {'error', _Reason} ->
            lager:debug("failed to open vmbox ~s: ~p", [VMBoxId, _Reason]),
            kz_json:new()
    end.

-spec get_owner(kzd_voicemail_box:doc(), kz_json:object()) -> kz_json:object().
get_owner(VMBoxJObj, DataJObj) ->
    OwnerId = kzd_voicemail_box:owner_id(VMBoxJObj),
    case teletype_util:open_doc(<<"user">>, OwnerId, DataJObj) of
        {'ok', JObj} -> JObj;
        {'error', _Reason} ->
            lager:debug("failed to open user ~s: ~p", [OwnerId, _Reason]),
            kz_json:new()
    end.

-spec maybe_add_user_email(kz_term:ne_binaries(), kz_term:api_binary(), boolean()) -> kz_term:ne_binaries().
maybe_add_user_email(BoxEmails, 'undefined', _) -> BoxEmails;
maybe_add_user_email(BoxEmails, UserEmail, 'false') -> lists:delete(UserEmail, BoxEmails);
maybe_add_user_email(BoxEmails, UserEmail, 'true') -> [UserEmail | BoxEmails].


-spec maybe_process_req(kz_json:object()) -> template_response().
maybe_process_req(DataJObj) ->
    HasEmail = kz_term:is_not_empty(kz_json:get_value(<<"to">>, DataJObj)),
    maybe_process_req(DataJObj, HasEmail).

-spec maybe_process_req(kz_json:object(), boolean()) -> template_response().
maybe_process_req(DataJObj, false) ->
    Msg = io_lib:format("requestor or box ~s has no emails or owner doesn't want emails"
                       ,[kz_json:get_value(<<"voicemail_box">>, DataJObj)]
                       ),
    lager:debug(Msg),
    teletype_util:notification_ignored(id());
maybe_process_req(DataJObj, true) ->
    do_process_req(DataJObj).

-spec do_process_req(kz_json:object()) -> template_response().
do_process_req(DataJObj) ->
    teletype_util:send_update(DataJObj, <<"pending">>),
    Macros0 = macros(DataJObj),
    Macros = props:delete(<<"attachments">>, Macros0),

    %% Load templates
    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], subject()),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, id()),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates, props:get_value(<<"attachments">>, Macros0)) of
        'ok' -> teletype_util:notification_completed(id());
        {'error', Reason} -> teletype_util:notification_failed(id(), Reason)
    end.

-spec macros(kz_json:object()) -> kz_term:proplist().
macros(DataJObj) ->
    TemplateData = template_data(DataJObj),
    EmailAttachements = email_attachments(DataJObj, TemplateData),
    Macros = maybe_add_file_data(TemplateData, EmailAttachements),
    props:set_value(<<"attachments">>, EmailAttachements, Macros).

-spec template_data(kz_json:object()) -> kz_term:proplist().
template_data(DataJObj) ->
    [{<<"system">>, teletype_util:system_params()}
     | build_template_data(DataJObj)
    ].

-spec email_attachments(kz_json:object(), kz_term:proplist()) -> attachments().
email_attachments(DataJObj, Macros) ->
    email_attachments(DataJObj, Macros, teletype_util:is_preview(DataJObj)).

-spec email_attachments(kz_json:object(), kz_term:proplist(), boolean()) -> attachments().
email_attachments(_DataJObj, _Macros, 'true') -> [];
email_attachments(DataJObj, Macros, 'false') ->
    VMId = kz_json:get_value(<<"voicemail_id">>, DataJObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    Db = kvm_util:get_db(AccountId, VMId),
    VMJObj = case kvm_message:fetch(AccountId, VMId) of
                 {'ok', JObj} -> JObj;
                 {'error', _} ->
                     throw({'error', 'no_attachment'})
             end,

    maybe_fetch_attachments(DataJObj, Db, VMId, get_file_name(VMJObj, Macros), kz_doc:attachments(VMJObj)).

-spec maybe_fetch_attachments(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_object()) -> attachments().
maybe_fetch_attachments(_, _, _, _, 'undefined') ->
    throw({'error', 'no_attachment'});
maybe_fetch_attachments(DataJObj, Db, VMId, FileName, Attachments) ->
    case kz_json:is_empty(Attachments) of
        'true' -> throw({'error', 'no_attachment'});
        'false' ->
            teletype_util:send_update(DataJObj, <<"pending">>),
            fetch_attachments(Db, VMId, FileName, Attachments)
    end.

-spec fetch_attachments(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> attachments().
fetch_attachments(Db, VMId, FileName, Attachments) ->
    {[AttachmentMeta], [AttachmentId]} = kz_json:get_values(Attachments),
    ContentType = kz_json:get_value(<<"content_type">>, AttachmentMeta),

    lager:debug("accessing voicemail attachment ~s at ~s / ~s", [AttachmentId, Db, VMId]),

    case kz_datamgr:fetch_attachment(Db, {kzd_box_message:type(), VMId}, AttachmentId) of
        {'ok', Bin} -> [{ContentType, FileName, Bin}];
        {'error', _E} ->
            lager:debug("failed to fetch attachment ~s: ~p", [AttachmentId, _E]),
            throw({'error', 'no_attachment'})
    end.

-spec get_file_name(kz_json:object(), kz_term:proplist()) -> kz_term:ne_binary().
get_file_name(MediaJObj, Macros) ->
    %% CallerID_Date_Time.mp3
    CallerID =
        case {props:get_value([<<"caller_id">>, <<"name">>], Macros)
             ,props:get_value([<<"caller_id">>, <<"number">>], Macros)
             }
        of
            {'undefined', 'undefined'} -> props:get_value([<<"from">>, <<"user">>], Macros, <<"Unknown">>);
            {'undefined', Num} -> Num;
            {Name, _} -> Name
        end,

    LocalDateTime = props:get_value([<<"date_called">>, <<"local">>], Macros),

    Extension = get_extension(MediaJObj),
    FileName = iolist_to_binary([CallerID, $_, kz_time:pretty_print_datetime(LocalDateTime), $., Extension]),

    kz_http_util:urlencode(
      binary:replace(kz_term:to_lower_binary(FileName), <<$\s>>, <<$_>>)
     ).

-spec get_extension(kz_json:object()) -> kz_term:ne_binary().
get_extension(MediaJObj) ->
    kz_mime:to_extension(kz_doc:attachment_content_type(MediaJObj)).

-spec build_template_data(kz_json:object()) -> kz_term:proplist().
build_template_data(DataJObj) ->
    Timezone = kzd_voicemail_box:timezone(kz_json:get_value(<<"vmbox_doc">>, DataJObj)),
    [{<<"voicemail">>, build_voicemail_data(DataJObj)}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"user">>, teletype_util:user_params(kz_json:get_value(<<"user">>, DataJObj))}
    ,{<<"owner">>, teletype_util:user_params(kz_json:get_value(<<"user">>, DataJObj))}
    ,{<<"reason">>, render_vm_delete_reason(DataJObj)}
     | teletype_util:build_call_data(DataJObj, Timezone)
    ].

-spec build_voicemail_data(kz_json:object()) -> kz_term:proplist().
build_voicemail_data(DataJObj) ->
    props:filter_undefined(
      [{<<"vmbox_id">>, kz_json:get_value(<<"voicemail_box">>, DataJObj)}
      ,{<<"box">>, kz_json:get_value(<<"voicemail_box">>, DataJObj)} %% backward compatibility
      ,{<<"vmbox_name">>, kz_json:get_value([<<"vmbox_doc">>, <<"name">>], DataJObj)}
      ,{<<"vmbox_number">>, kz_json:get_value([<<"vmbox_doc">>, <<"mailbox">>], DataJObj)}
      ,{<<"msg_id">>, kz_json:get_value(<<"voicemail_id">>, DataJObj)}
      ,{<<"name">>, kz_json:get_value(<<"voicemail_id">>, DataJObj)} %% backward compatibility
      ,{<<"transcription">>, get_transcription(DataJObj)}
      ,{<<"length">>, pretty_print_length(DataJObj)}
      ]).

-spec render_vm_delete_reason(kz_json:object()) -> kz_term:api_ne_binary().
render_vm_delete_reason(DataJObj) ->
    case kz_json:get_atom_value(<<"reason">>, DataJObj) of
        'dtmf' -> <<"user pressed DTMF key">>;
        'delete_after_notify' -> <<"due to 'Deleted after notify'">>;
        'crossbar_action' -> <<"due to crossbar action (webhook)">>
    end.

-spec get_transcription(kz_json:object()) -> kz_term:api_ne_binary().
get_transcription(DataJObj) ->
    case kz_json:get_value(<<"voicemail_transcription">>, DataJObj) of
        'undefined' -> 'undefined';
        ?NE_BINARY=Bin -> Bin;
        JObj -> kz_json:get_ne_binary_value(<<"text">>, JObj)
    end.

-spec pretty_print_length(kz_term:api_object() | pos_integer()) -> kz_term:ne_binary().
pretty_print_length('undefined') -> <<"00:00:00">>;
pretty_print_length(Ms) when is_integer(Ms) ->
    MilliSeconds = kz_time:milliseconds_to_seconds(Ms),
    Us = kz_time:unix_seconds_to_gregorian_seconds(MilliSeconds),
    {_, {H, M, S}} = calendar:gregorian_seconds_to_datetime(Us),
    kz_term:to_binary(io_lib:format("~2..0w:~2..0w:~2..0w", [H, M, S]));
pretty_print_length(JObj) ->
    pretty_print_length(kz_json:get_integer_value(<<"voicemail_length">>, JObj)).

-spec maybe_add_file_data(kz_term:proplist(), attachments()) -> kz_term:proplist().
maybe_add_file_data(Macros, []) -> Macros;
maybe_add_file_data(Macros, [{ContentType, FileName, Bin}]) ->
    Props = props:filter_undefined(
              [{<<"file_name">>, FileName}
              ,{<<"file_type">>, kz_mime:to_extension(ContentType)}
              ,{<<"file_size">>, erlang:size(Bin)}
              ]),
    VMF = props:set_values(Props, props:get_value(<<"voicemail">>, Macros, [])),
    props:set_value(<<"voicemail">>, VMF, Macros).
