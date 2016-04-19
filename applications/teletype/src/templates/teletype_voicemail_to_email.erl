%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_voicemail_to_email).

-export([init/0
         ,handle_new_voicemail/2
        ]).

-include("../teletype.hrl").

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".voicemail_to_email">>).

-define(TEMPLATE_ID, <<"voicemail_to_email">>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"voicemail.box">>, <<"voicemail_box">>, <<"Voicemail Box">>, <<"Which voicemail box was the message left in">>)
            ,?MACRO_VALUE(<<"voicemail.name">>, <<"voicemail_name">>, <<"Voicemail Name">>, <<"Name of the voicemail file">>)
            ,?MACRO_VALUE(<<"voicemail.length">>, <<"voicemail_length">>, <<"Voicemail Length">>, <<"Length of the voicemail file">>)
            ,?MACRO_VALUE(<<"call_id">>, <<"call_id">>, <<"Call ID">>, <<"Call ID of the caller">>)
            ,?MACRO_VALUE(<<"owner.first_name">>, <<"first_name">>, <<"First Name">>, <<"First name of the owner of the voicemail box">>)
            ,?MACRO_VALUE(<<"owner.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last name of the owner of the voicemail box">>)
            | ?DEFAULT_CALL_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"New Voicemail Message\n\nCaller ID: {{caller_id.number}}\nCaller Name: {{caller_id.name}}\n\nCalled To: {{to.user}}   (Originally dialed number)\nCalled On: {{date_called.local|date:\"l, F j, Y \\\\a\\\\t H:i\"}}\n\nTranscription: {{voicemail.transcription|default:\"Not Enabled\"}}">>).
-define(TEMPLATE_HTML, <<"<html><body><h3>New Voicemail Message</h3><table><tr><td>Caller ID</td><td>{{caller_id.name}} ({{caller_id.number}})</td></tr><tr><td>Callee ID</td><td>{{to.user}} (originally dialed number)</td></tr><tr><td>Call received</td><td>{{date_called.local|date:\"l, F j, Y \\\\a\\\\t H:i\"}}</td></tr></table><p style=\"font-size: 9px;color:#C0C0C0\">{{call_id}}</p><p>Transcription: {{voicemail.transcription|default:\"Not Enabled\"}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"New voicemail from {{caller_id.name}} ({{caller_id.number}})">>).
-define(TEMPLATE_CATEGORY, <<"voicemail">>).
-define(TEMPLATE_NAME, <<"Voicemail To Email">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
                                           ,{'text', ?TEMPLATE_TEXT}
                                           ,{'html', ?TEMPLATE_HTML}
                                           ,{'subject', ?TEMPLATE_SUBJECT}
                                           ,{'category', ?TEMPLATE_CATEGORY}
                                           ,{'friendly_name', ?TEMPLATE_NAME}
                                           ,{'to', ?TEMPLATE_TO}
                                           ,{'from', ?TEMPLATE_FROM}
                                           ,{'cc', ?TEMPLATE_CC}
                                           ,{'bcc', ?TEMPLATE_BCC}
                                           ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                          ]).

-spec handle_new_voicemail(wh_json:object(), wh_proplist()) -> 'ok'.
handle_new_voicemail(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),

    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),

    teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID)
        orelse teletype_util:stop_processing("template ~s not enabled for account ~s", [?TEMPLATE_ID, AccountId]),

    {'ok', AccountJObj} = teletype_util:open_doc(<<"account">>, AccountId, DataJObj),

    VMBoxId = wh_json:get_value(<<"voicemail_box">>, DataJObj),
    {'ok', VMBox} = teletype_util:open_doc(<<"voicemail">>, VMBoxId, DataJObj),

    {'ok', UserJObj} = get_owner(VMBox, DataJObj),

    BoxEmails = kzd_voicemail_box:notification_emails(VMBox),
    Emails = maybe_add_user_email(BoxEmails, kzd_user:email(UserJObj)),

    %% If the box has emails, continue processing
    %% or If the voicemail notification is enabled on the user, continue processing
    %% otherwise stop processing
    (Emails =/= [] andalso
     (kzd_user:voicemail_notification_enabled(UserJObj) orelse wh_json:is_empty(UserJObj)))
        orelse teletype_util:stop_processing("box ~s has no emails or owner doesn't want emails", [VMBoxId]),

    ReqData =
        wh_json:set_values(
          [{<<"voicemail">>, VMBox}
           ,{<<"owner">>, UserJObj}
           ,{<<"account">>, AccountJObj}
           ,{<<"to">>, Emails}
          ]
          ,DataJObj
         ),

    case teletype_util:is_preview(DataJObj) of
        'false' -> process_req(ReqData);
        'true' ->
            process_req(wh_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec maybe_add_user_email(ne_binaries(), api_binary()) -> ne_binaries().
maybe_add_user_email(BoxEmails, 'undefined') -> BoxEmails;
maybe_add_user_email(BoxEmails, UserEmail) -> [UserEmail | BoxEmails].

-spec get_owner(kzd_voicemail_box:doc(), wh_json:object()) ->
                       {'ok', wh_json:object()}.
get_owner(VMBox, DataJObj) ->
    case teletype_util:open_doc(<<"user">>, kzd_voicemail_box:owner_id(VMBox), DataJObj) of
        {'ok', _}=OK -> OK;
        {'error', 'empty_doc_id'} -> {'ok', wh_json:new()}
    end.

-spec process_req(wh_json:object()) -> 'ok'.
process_req(DataJObj) ->
    teletype_util:send_update(DataJObj, <<"pending">>),

    Macros = [{<<"system">>, teletype_util:system_params()}
              | build_template_data(DataJObj)
             ],

    %% Load templates
    Templates = teletype_templates:fetch(?TEMPLATE_ID, DataJObj),

    %% Populate templates
    RenderedTemplates =
        props:filter_undefined(
          [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
           || {ContentType, Template} <- Templates
          ]),

    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_meta(?TEMPLATE_ID, AccountId),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                ,Macros
               ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    EmailAttachements = email_attachments(DataJObj, Macros),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates, EmailAttachements) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec email_attachments(wh_json:object(), wh_proplist()) -> attachments().
-spec email_attachments(wh_json:object(), wh_proplist(), boolean()) -> attachments().
email_attachments(DataJObj, Macros) ->
    email_attachments(DataJObj, Macros, teletype_util:is_preview(DataJObj)).

email_attachments(_DataJObj, _Macros, 'true') -> [];
email_attachments(DataJObj, Macros, 'false') ->
    VMId = wh_json:get_value(<<"voicemail_name">>, DataJObj),
    AccountDb = wh_json:get_value(<<"account_db">>, DataJObj),
    {'ok', VMJObj} = couch_mgr:open_doc(AccountDb, VMId),

    {[AttachmentMeta], [AttachmentId]} = wh_json:get_values(wh_doc:attachments(VMJObj)),
    {'ok', AttachmentBin} = couch_mgr:fetch_attachment(AccountDb, VMId, AttachmentId),

    [{wh_json:get_value(<<"content_type">>, AttachmentMeta)
      ,get_file_name(VMJObj, Macros)
      ,AttachmentBin
     }].

-spec get_file_name(wh_json:object(), wh_proplist()) -> ne_binary().
get_file_name(MediaJObj, Macros) ->
    %% CallerID_Date_Time.mp3
    CallerID =
        case {props:get_value([<<"caller_id">>, <<"name">>], Macros)
              ,props:get_value([<<"caller_id">>, <<"number">>], Macros)
             }
        of
            {'undefined', 'undefined'} -> <<"Unknown">>;
            {'undefined', Num} -> wnm_util:pretty_print(wh_util:to_binary(Num));
            {Name, _} -> wnm_util:pretty_print(wh_util:to_binary(Name))
        end,

    LocalDateTime = props:get_value([<<"date_called">>, <<"local">>], Macros),

    Extension = get_extension(MediaJObj),
    FileName = list_to_binary([CallerID, "_", wh_util:pretty_print_datetime(LocalDateTime), ".", Extension]),

    cow_qs:urlencode(
      binary:replace(wh_util:to_lower_binary(FileName), <<" ">>, <<"_">>)
     ).

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

-spec build_template_data(wh_json:object()) -> wh_proplist().
build_template_data(DataJObj) ->
    [{<<"caller_id">>, build_caller_id_data(DataJObj)}
     ,{<<"callee_id">>, build_callee_id_data(DataJObj)}
     ,{<<"date_called">>, build_date_called_data(DataJObj)}
     ,{<<"voicemail">>, build_voicemail_data(DataJObj)}
     ,{<<"call_id">>, wh_json:get_value(<<"call_id">>, DataJObj)}
     ,{<<"from">>, build_from_data(DataJObj)}
     ,{<<"to">>, build_to_data(DataJObj)}
     ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ].

-spec build_from_data(wh_json:object()) -> wh_proplist().
build_from_data(DataJObj) ->
    props:filter_undefined(
      [{<<"user">>, wh_json:get_value(<<"from_user">>, DataJObj)}
       ,{<<"realm">>, wh_json:get_value(<<"from_realm">>, DataJObj)}
      ]).

-spec build_to_data(wh_json:object()) -> wh_proplist().
build_to_data(DataJObj) ->
    props:filter_undefined(
      [{<<"user">>, wh_json:get_value(<<"to_user">>, DataJObj)}
       ,{<<"realm">>, wh_json:get_value(<<"to_realm">>, DataJObj)}
      ]).

-spec build_caller_id_data(wh_json:object()) -> wh_proplist().
build_caller_id_data(DataJObj) ->
    props:filter_undefined(
      [{<<"number">>, wnm_util:pretty_print(wh_json:get_value(<<"caller_id_number">>, DataJObj))}
       ,{<<"name">>, wnm_util:pretty_print(wh_json:get_value(<<"caller_id_name">>, DataJObj))}
      ]).

-spec build_callee_id_data(wh_json:object()) -> wh_proplist().
build_callee_id_data(DataJObj) ->
    props:filter_undefined(
      [{<<"number">>, wnm_util:pretty_print(wh_json:get_value(<<"callee_id_number">>, DataJObj))}
       ,{<<"name">>, wnm_util:pretty_print(wh_json:get_value(<<"callee_id_name">>, DataJObj))}
      ]).

-spec build_date_called_data(wh_json:object()) -> wh_proplist().
build_date_called_data(DataJObj) ->
    DateCalled = date_called(DataJObj),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    VMBox = wh_json:get_value(<<"voicemail">>, DataJObj),
    Timezone = kzd_voicemail_box:timezone(VMBox, <<"UTC">>),
    ClockTimezone = whapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    lager:debug("using tz ~s (system ~s) for ~p", [Timezone, ClockTimezone, DateTime]),

    props:filter_undefined(
      [{<<"utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
       ,{<<"local">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
      ]).

-spec date_called(api_object() | gregorian_seconds()) -> gregorian_seconds().
date_called(Timestamp) when is_integer(Timestamp) -> Timestamp;
date_called('undefined') -> wh_util:current_tstamp();
date_called(DataJObj) ->
    date_called(wh_json:get_integer_value(<<"voicemail_timestamp">>, DataJObj)).

-spec build_voicemail_data(wh_json:object()) -> wh_proplist().
build_voicemail_data(DataJObj) ->
    props:filter_undefined(
      [{<<"box">>, wh_json:get_value(<<"voicemail_box">>, DataJObj)}
       ,{<<"name">>, wh_json:get_value(<<"voicemail_name">>, DataJObj)}
       ,{<<"length">>, pretty_print_length(DataJObj)}
      ]).

-spec pretty_print_length(api_object() | pos_integer()) -> ne_binary().
pretty_print_length('undefined') -> <<"00:00">>;
pretty_print_length(Ms) when is_integer(Ms) ->
    Seconds = round(Ms / ?MILLISECONDS_IN_SECOND) rem 60,
    Minutes = trunc(Ms / (?MILLISECONDS_IN_MINUTE)) rem 60,
    wh_util:to_binary(io_lib:format("~2..0w:~2..0w", [Minutes, Seconds]));
pretty_print_length(JObj) ->
    pretty_print_length(wh_json:get_integer_value(<<"voicemail_length">>, JObj)).
