%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz Inc
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

-define(TEMPLATE_MACROS, wh_json:from_list([{<<"caller_id.number">>
                                             ,wh_json:from_list([{<<"i18n_label">>, <<"caller_id_number">>}
                                                                 ,{<<"friendly_name">>, <<"Caller ID Number">>}
                                                                 ,{<<"description">>, <<"Caller ID Number">>}
                                                                ])
                                            }
                                            ,{<<"caller_id.name">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"caller_id_name">>}
                                                                  ,{<<"friendly_name">>, <<"Caller ID Name">>}
                                                                  ,{<<"description">>, <<"Caller ID Name">>}
                                                                 ])
                                             }
                                            ,{<<"date_called.utc">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"date_called_utc">>}
                                                                  ,{<<"friendly_name">>, <<"Date (UTC)">>}
                                                                  ,{<<"description">>, <<"When was the voicemail left (UTC)">>}
                                                                 ])
                                             }
                                            ,{<<"date_called.local">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"date_called_local">>}
                                                                  ,{<<"friendly_name">>, <<"Date">>}
                                                                  ,{<<"description">>, <<"When was the voicemail left (Local time)">>}
                                                                 ])
                                             }
                                            ,{<<"from_user">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"from_user">>}
                                                                  ,{<<"friendly_name">>, <<"From">>}
                                                                  ,{<<"description">>, <<"SIP From address">>}
                                                                 ])
                                             }
                                            ,{<<"to_user">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"to_user">>}
                                                                  ,{<<"friendly_name">>, <<"To">>}
                                                                  ,{<<"description">>, <<"SIP To address">>}
                                                                 ])
                                             }
                                            ,{<<"voicemail.box">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"voicemail_box">>}
                                                                  ,{<<"friendly_name">>, <<"Voicemail Box">>}
                                                                  ,{<<"description">>, <<"Which voicemail box was the message left in">>}
                                                                 ])
                                             }
                                            ,{<<"voicemail.name">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"voicemail_name">>}
                                                                  ,{<<"friendly_name">>, <<"Voicemail Name">>}
                                                                  ,{<<"description">>, <<"Name of the voicemail file">>}
                                                                 ])
                                             }
                                            ,{<<"voicemail.length">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"voicemail_name">>}
                                                                  ,{<<"friendly_name">>, <<"Voicemail Name">>}
                                                                  ,{<<"description">>, <<"Name of the voicemail file">>}
                                                                 ])
                                             }
                                            ,{<<"call_id">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"call_id">>}
                                                                  ,{<<"friendly_name">>, <<"Call ID">>}
                                                                  ,{<<"description">>, <<"Call ID of the caller">>}
                                                                 ])
                                             }
                                            ,{<<"owner.first_name">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"first_name">>}
                                                                  ,{<<"friendly_name">>, <<"First Name">>}
                                                                  ,{<<"description">>, <<"First name of the owner of the voicemail box">>}
                                                                 ])
                                             }
                                            ,{<<"owner.last_name">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"last_name">>}
                                                                  ,{<<"friendly_name">>, <<"Last Name">>}
                                                                  ,{<<"description">>, <<"Last name of the owner of the voicemail box">>}
                                                                 ])
                                             }
                                           ])).

-define(TEMPLATE_TEXT, <<"New Voicemail Message\n\nCaller ID: {{caller_id.number}}\nCaller Name: {{caller_id.name}}\n\nCalled To: {{to_user}}   (Originally dialed number)\nCalled On: {{date_called.local|date:\"l, F j, Y \\a\\t H:i\"}}\n\nTranscription: {{voicemail.transcription|default:\"Not Enabled\"}}\n\n\nFor help or questions using your phone or voicemail, please contact support at {{service.support_number}} or email {{service.support_email}}.">>).
-define(TEMPLATE_HTML, <<"<html><body><h3>New Voicemail Message</h3><table><tr><td>Caller ID</td><td>{{caller_id.name}} ({{caller_id.number}})</td></tr><tr><td>Callee ID</td><td>{{to_user}} (originally dialed number)</td></tr><tr><td>Call received</td><td>{{date_called.local|date:\"l, F j, Y \\a\\t H:i\"}}</td></tr></table><p>For help or questions using your phone or voicemail, please contact {{service.support_number}} or email <a href=\"mailto:{{service.support_email}}\">Support</a></p><p style=\"font-size: 9px;color:#C0C0C0\">{{call_id}}</p><p>Transcription: {{voicemail.transcription|default:\"Not Enabled\"}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"New voicemail from {{caller_id.name}} ({{caller_id.number}})">>).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_util:init_template(?TEMPLATE_ID, ?TEMPLATE_MACROS, ?TEMPLATE_TEXT, ?TEMPLATE_HTML).

-spec handle_new_voicemail(wh_json:object(), wh_proplist()) -> 'ok'.
handle_new_voicemail(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(wh_api:remove_defaults(JObj)),

    AccountDb = wh_json:get_value(<<"account_db">>, DataJObj),

    {'ok', VMBox} = couch_mgr:open_cache_doc(AccountDb, wh_json:get_value(<<"voicemail_box">>, DataJObj)),

    {'ok', UserJObj} = couch_mgr:open_cache_doc(AccountDb, wh_json:get_value(<<"owner_id">>, VMBox)),

    case (Email = wh_json:get_ne_value(<<"email">>, UserJObj)) =/= 'undefined'
        andalso wh_json:is_true(<<"vm_to_email_enabled">>, UserJObj)
    of
        'false' ->
            lager:debug("sending voicemail to email not configured for owner ~s"
                        ,[wh_json:get_value(<<"owner_id">>, VMBox)]
                       );
        'true' ->
            lager:debug("voicemail->email enabled for owner ~s"
                        ,[wh_json:get_value(<<"owner_id">>, VMBox)]
                       ),

            {'ok', AccountJObj} = couch_mgr:open_cache_doc(AccountDb
                                                           ,wh_util:format_account_id(AccountDb, 'raw')
                                                          ),
            process_req(
              wh_json:set_values([{<<"voicemail">>, VMBox}
                                  ,{<<"owner">>, UserJObj}
                                  ,{<<"account">>, AccountJObj}
                                  ,{[<<"to">>, <<"email_addresses">>], [Email]}
                                 ]
                                 ,DataJObj
                                )
             )
    end.

-spec process_req(wh_json:object()) -> 'ok'.
process_req(DataJObj) ->
    teletype_util:send_update(DataJObj, <<"pending">>),

    ServiceData = teletype_util:service_params(DataJObj, ?MOD_CONFIG_CAT),
    Macros = [{<<"service">>, ServiceData}
              ,{<<"account">>, wh_json:to_proplist(wh_json:get_value(<<"account">>, DataJObj))}
              ,{<<"owner">>, wh_json:to_proplist(wh_json:get_value(<<"owner">>, DataJObj))}
              | build_template_data(DataJObj)
             ],

    %% Load templates
    Templates = teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj),

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} = teletype_util:fetch_template_meta(?TEMPLATE_ID, wh_json:get_value(<<"account_Id">>, DataJObj)),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                ,Macros
               ),

    %% Send email
    case teletype_util:send_email(?TEMPLATE_ID
                                  ,DataJObj
                                  ,ServiceData
                                  ,Subject
                                  ,RenderedTemplates
                                  ,email_attachments(DataJObj, Macros)
                                 )
    of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec email_attachments(wh_json:object(), wh_proplist()) -> attachments().
email_attachments(DataJObj, Macros) ->
    VMId = wh_json:get_value(<<"voicemail_name">>, DataJObj),
    AccountDb = wh_json:get_value(<<"account_db">>, DataJObj),
    {'ok', VMJObj} = couch_mgr:open_cache_doc(AccountDb, VMId),
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
            attachment_to_extension(wh_json:get_value(<<"_attachments">>, MediaJObj));
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
    props:filter_empty(
      [{<<"caller_id">>, build_caller_id_data(DataJObj)}
       ,{<<"date_called">>, build_date_called_data(DataJObj)}
       ,{<<"voicemail">>, build_voicemail_data(DataJObj)}
       ,{<<"call_id">>, wh_json:get_value(<<"call_id">>, DataJObj)}
       ,{<<"from_user">>, wh_json:get_value(<<"from_user">>, DataJObj)}
       ,{<<"to_user">>, wh_json:get_value(<<"to_user">>, DataJObj)}
      ]).

-spec build_caller_id_data(wh_json:object()) -> wh_proplist().
build_caller_id_data(DataJObj) ->
    props:filter_undefined(
      [{<<"number">>, wnm_util:pretty_print(wh_json:get_value(<<"caller_id_number">>, DataJObj))}
       ,{<<"name">>, wnm_util:pretty_print(wh_json:get_value(<<"caller_id_name">>, DataJObj))}
      ]).

-spec build_date_called_data(wh_json:object()) -> wh_proplist().
build_date_called_data(DataJObj) ->
    DateCalled = wh_json:get_integer_value(<<"voicemail_timestamp">>, DataJObj),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    Timezone = wh_json:get_first_defined([[<<"voicemail">>, <<"timezone">>]
                                          ,[<<"owner">>, <<"timezone">>]
                                          ,[<<"account">>, <<"timezone">>]
                                         ]
                                         ,DataJObj
                                         ,<<"UTC">>
                                        ),
    ClockTimezone = whapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    lager:debug("using tz ~s (system ~s) for ~p", [Timezone, ClockTimezone, DateTime]),

    props:filter_undefined(
      [{<<"utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
       ,{<<"local">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
      ]).

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
    Seconds = round(Ms / 1000) rem 60,
    Minutes = trunc(Ms / (1000*60)) rem 60,
    wh_util:to_binary(io_lib:format("~2..0w:~2..0w", [Minutes, Seconds]));
pretty_print_length(JObj) ->
    pretty_print_length(wh_json:get_integer_value(<<"voicemail_length">>, JObj)).
