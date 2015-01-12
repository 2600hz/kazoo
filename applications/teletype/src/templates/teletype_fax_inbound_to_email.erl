%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_fax_inbound_to_email).

-export([init/0
         ,handle_fax_inbound/2
        ]).

-include("../teletype.hrl").

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax_inbound_to_email">>).
-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).

-define(TEMPLATE_ID, <<"fax_inbound_to_email">>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"call_id">>, <<"call_id">>, <<"Call ID">>, <<"Call ID of the fax transmission">>)
            ,?MACRO_VALUE(<<"fax.total_pages">>, <<"fax_total_pages">>, <<"Total Pages">>, <<"Total number of pages received">>)
            ,?MACRO_VALUE(<<"fax.id">>, <<"fax_id">>, <<"Fax ID">>, <<"Crossbar ID of the fax transmission">>)
            ,?MACRO_VALUE(<<"fax.media">>, <<"fax_media">>, <<"Fax Name">>, <<"Name of the fax transmission">>)
            ,?MACRO_VALUE(<<"fax.success">>, <<"fax_success">>, <<"Fax Success">>, <<"Was the fax successful">>)
            ,?MACRO_VALUE(<<"fax.ecm_used">>, <<"fax_ecm_used">>, <<"ECM Used">>, <<"Was ECM used">>)
            ,?MACRO_VALUE(<<"fax.result_text">>, <<"fax_result_text">>, <<"Fax Result Text">>, <<"Result text from transmission">>)
            ,?MACRO_VALUE(<<"fax.transferred_pages">>, <<"fax_transferred_pages">>, <<"Transferred Pages">>, <<"How many pages were transferred">>)
            ,?MACRO_VALUE(<<"fax.bad_rows">>, <<"fax_bad_rows">>, <<"Bad Rows">>, <<"How many bad rows">>)
            ,?MACRO_VALUE(<<"fax.transfer_rate">>, <<"fax_transfer_rate">>, <<"Transfer Rate">>, <<"Transfer Rate">>)
            ,?MACRO_VALUE(<<"fax.encoding">>, <<"fax_encoding">>, <<"Fax Encoding">>, <<"Encoding of the fax">>)
            ,?MACRO_VALUE(<<"fax.doc_id">>, <<"fax_doc_id">>, <<"Document ID">>, <<"Crossbar ID of the Fax document">>)
            | ?DEFAULT_CALL_MACROS
            ++ ?SERVICE_MACROS
           ]
          )).

-define(TEMPLATE_TEXT, <<"New Fax ({{fax.total_pages}} Pages)\n\nCaller ID: {{caller_id.number}}\nCaller Name: {{caller_id.name}}\n\nCalled To: {{to.user}}   (Originally dialed number)\nCalled On: {{date_called.local|date:\"l, F j, Y \\a\\t H:i\"}}\n\n\nFor help or questions about receiving faxes, please contact support at {{service.support_number}} or email {{service.support_email}}.">>).
-define(TEMPLATE_HTML, <<"<html><body><h3>New Fax ({{fax.total_pages}} Pages)</h3><table><tr><td>Caller ID</td><td>{{caller_id.name}} ({{caller_id.number}})</td></tr><tr><td>Callee ID</td><td>{{to.user}} (originally dialed number)</td></tr><tr><td>Call received</td><td>{{date_called.local|date:\"l, F j, Y \\a\\t H:i\"}}</td></tr></table><p>For help or questions about receiving faxes, please contact {{service.support_number}} or email <a href=\"mailto:{{service.support_email}}\">Support</a></p><p style=\"font-size: 9px;color:#C0C0C0\">{{fax.call_id}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"New fax from {{caller_id.name}} ({{caller_id.number}})">>).
-define(TEMPLATE_CATEGORY, <<"fax">>).
-define(TEMPLATE_NAME, <<"Inbound Fax to Email">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),

    teletype_util:init_template(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
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

-spec get_fax_doc(wh_json:object()) -> wh_json:object().
get_fax_doc(DataJObj) ->
    AccountId = teletype_util:find_account_id(DataJObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    FaxId = wh_json:get_value(<<"fax_id">>, DataJObj),

    case couch_mgr:open_doc(AccountDb, FaxId) of
        {'ok', FaxJObj} -> FaxJObj;
        {'error', 'not_found'} ->
            get_fax_doc_from_modb(DataJObj, AccountId, FaxId);
        {'error', _E} ->
            lager:debug("failed to find fax ~s: ~p", [FaxId, _E]),
            maybe_send_failure(DataJObj, <<"Fax-ID was invalid">>)
    end.

-spec get_fax_doc_from_modb(wh_json:object(), ne_binary(), ne_binary()) -> wh_json:object().
get_fax_doc_from_modb(DataJObj, AccountId, FaxId) ->
    case kazoo_modb:open_doc(AccountId, FaxId) of
        {'ok', FaxJObj} -> FaxJObj;
        {'error', _E} ->
            lager:debug("failed to find fax ~s: ~p", [FaxId, _E]),
            maybe_send_failure(DataJObj, <<"Fax-ID was invalid">>)
    end.

-spec maybe_send_failure(wh_json:object(), ne_binary()) -> wh_json:object().
maybe_send_failure(DataJObj, Msg) ->
    case wh_json:is_true(<<"preview">>, DataJObj) of
        'true' ->
            lager:debug("not sending failure as this is a preview"),
            wh_json:new();
        'false' ->
            teletype_util:send_update(DataJObj, <<"failed">>, Msg),
            throw({'error', 'no_fax_id'})
    end.

-spec handle_fax_inbound(wh_json:object(), wh_proplist()) -> 'ok'.
handle_fax_inbound(JObj, _Props) ->
    'true' = wapi_notifications:fax_inbound_v(JObj),
    wh_util:put_callid(JObj),

    lager:debug("processing fax inbound to email"),

    %% Gather data for template
    DataJObj =
        wh_json:set_values([{<<"server_id">>, wh_json:get_value(<<"Server-ID">>, JObj)}
                            ,{<<"msg_id">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                           ]
                           ,wh_json:normalize(
                              wh_api:remove_defaults(JObj)
                             )
                          ),

    FaxJObj = get_fax_doc(DataJObj),

    AccountId = teletype_util:find_account_id(DataJObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AccountJObj} = couch_mgr:open_cache_doc(AccountDb, AccountId),

    OwnerJObj = get_owner_doc(FaxJObj),

    Macros = build_template_data(
               wh_json:set_values([{<<"account">>, wh_doc:public_fields(AccountJObj)}
                                   ,{<<"fax">>, wh_doc:public_fields(FaxJObj)}
                                   ,{<<"owner">>, wh_doc:public_fields(OwnerJObj)}
                                  ]
                                  ,DataJObj
                                 )),

    %% Load templates
    Templates = teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj),

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates,
                            Template =/= 'undefined'
                        ],
    lager:debug("rendered templates"),

    {'ok', TemplateMetaJObj} = teletype_util:fetch_template_meta(?TEMPLATE_ID, wh_json:get_value(<<"Account-ID">>, JObj)),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                ,Macros
               ),
    lager:debug("rendered subject: ~s", [Subject]),

    Emails = teletype_util:find_addresses(wh_json:set_value(<<"to">>
                                                            ,to_email_addresses(DataJObj)
                                                            ,DataJObj
                                                           )
                                          ,TemplateMetaJObj
                                          ,?MOD_CONFIG_CAT
                                         ),

    %% Send email
    case teletype_util:send_email(Emails
                                  ,Subject
                                  ,props:get_value(<<"service">>, Macros)
                                  ,RenderedTemplates
                                  ,[get_attachment(DataJObj, Macros)]
                                 )
    of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec get_owner_doc(wh_json:object()) -> wh_json:object().
get_owner_doc(FaxJObj) ->
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, FaxJObj),
    OwnerId = wh_json:get_value(<<"owner_id">>, FaxJObj),

    lager:debug("trying owner ~s ", [OwnerId]),

    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', OwnerJObj} -> OwnerJObj;
        {'error', 'not_found'} -> wh_json:new()
    end.

-spec get_attachment(wh_json:object(), wh_proplist()) -> attachment().
get_attachment(DataJObj, Macros) ->
    FaxMacros = props:get_value(<<"fax">>, Macros),
    FaxId = props:get_first_defined([<<"id">>, <<"fax_jobid">>, <<"fax_id">>], FaxMacros),
    Db = fax_db(DataJObj),
    lager:debug("accessing fax at ~s / ~s", [Db, FaxId]),
    {'ok', ContentType, Bin} = get_attachment_binary(Db, FaxId),

    ToFormat = whapps_config:get(?FAX_CONFIG_CAT, <<"attachment_format">>, <<"pdf">>),
    FromFormat = from_format_from_content_type(ContentType),

    lager:debug("converting from ~s to ~s", [FromFormat, ToFormat]),

    {'ok', Converted} = teletype_fax_util:convert(FromFormat, ToFormat, Bin),

    Filename = get_file_name(Macros, ToFormat),
    lager:debug("adding attachment as ~s", [Filename]),
    {content_type_from_extension(Filename)
     ,Filename
     ,Converted
    }.

-spec from_format_from_content_type(ne_binary()) -> ne_binary().
from_format_from_content_type(<<"application/pdf">>) ->
    <<"pdf">>;
from_format_from_content_type(<<"image/tiff">>) ->
    <<"tif">>;
from_format_from_content_type(ContentType) ->
    [_Type, SubType] = binary:split(ContentType, <<"/">>),
    SubType.

-spec content_type_from_extension(ne_binary()) -> ne_binary().
content_type_from_extension(Ext) ->
    {Type, SubType, _} = cow_mimetypes:all(Ext),
    <<Type/binary, "/", SubType/binary>>.

-spec get_file_name(wh_proplist(), ne_binary()) -> ne_binary().
get_file_name(Macros, Ext) ->
    CallerIdMacros = props:get_value(<<"caller_id">>, Macros),
    CallerID =
        case {props:get_value(<<"name">>, CallerIdMacros)
              ,props:get_value(<<"number">>, CallerIdMacros)
             }
        of
            {'undefined', 'undefined'} -> <<"Unknown">>;
            {'undefined', Num} -> wh_util:to_binary(Num);
            {Name, _} -> wh_util:to_binary(Name)
        end,
    LocalDateTime = props:get_value([<<"date_called">>, <<"local">>], Macros, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", wh_util:pretty_print_datetime(LocalDateTime), ".", Ext]),
    re:replace(wh_util:to_lower_binary(FName), <<"\\s+">>, <<"_">>, [{'return', 'binary'}, 'global']).

-spec get_attachment_binary(ne_binary(), ne_binary()) ->
                                   {'ok', ne_binary(), binary()}.
get_attachment_binary(Db, Id) ->
    get_attachment_binary(Db, Id, 2).
get_attachment_binary(_Db, _Id, 0) ->
    lager:debug("failed to find ~s in ~s, retries expired", [_Id, _Db]),
    throw({'error', 'no_attachment'});
get_attachment_binary(Db, Id, Retries) ->
    case couch_mgr:open_cache_doc(Db, Id) of
        {'error', 'not_found'} when Db =/= ?WH_FAXES ->
            get_attachment_binary(?WH_FAXES, Id, Retries);
        {'ok', FaxJObj} ->
            case wh_doc:attachment(FaxJObj) of
                'undefined' -> delayed_retry(Db, Id, Retries);
                AttachmentJObj ->
                    get_attachment_binary(Db, Id, Retries, AttachmentJObj)
            end
    end.

-spec delayed_retry(ne_binary(), ne_binary(), 1..2) -> {'ok', ne_binary(), binary()}.
delayed_retry(Db, Id, Retries) ->
    lager:debug("waiting to query fax doc ~s for attachment", [Id]),
    timer:sleep(?MILLISECONDS_IN_MINUTE * 5),
    get_attachment_binary(Db, Id, Retries-1).

-spec get_attachment_binary(ne_binary(), ne_binary(), 1..2) -> {'ok', ne_binary(), binary()}.
get_attachment_binary(Db, Id, Retries, AttachmentJObj) ->
    [AttachmentName] = wh_json:get_keys(AttachmentJObj),
    ContentType = wh_json:get_value([AttachmentName, <<"content_type">>], AttachmentJObj, <<"image/tiff">>),

    case couch_mgr:fetch_attachment(Db, Id, AttachmentName) of
        {'ok', Bin} -> {'ok', ContentType, Bin};
        {'error', _E} ->
            lager:debug("failed to fetch attachment ~s: ~p", [AttachmentName, _E]),
            delayed_retry(Db, Id, Retries-1)
    end.

-spec fax_db(wh_json:object()) -> ne_binary().
fax_db(DataJObj) ->
    case teletype_util:find_account_db(DataJObj) of
        'undefined' -> ?WH_FAXES;
        Db -> Db
    end.

-spec build_template_data(wh_json:object()) -> wh_proplist().
build_template_data(DataJObj) ->
    [{<<"account">>, wh_json:to_proplist(wh_json:get_value(<<"account">>, DataJObj))}
     ,{<<"fax">>, build_fax_template_data(DataJObj)}
     ,{<<"service">>, teletype_util:service_params(DataJObj, ?MOD_CONFIG_CAT)}
     ,{<<"caller_id">>, caller_id_data(DataJObj)}
     ,{<<"callee_id">>, callee_id_data(DataJObj)}
     ,{<<"date_called">>, date_called_data(DataJObj)}
     ,{<<"from">>, from_data(DataJObj)}
     ,{<<"to">>, to_data(DataJObj)}
     ,{<<"call_id">>, wh_json:get_value(<<"call_id">>, DataJObj)}
    ].

-spec caller_id_data(wh_json:object()) -> wh_proplist().
caller_id_data(DataJObj) ->
    props:filter_undefined(
      [{<<"name">>, wh_json:get_value(<<"caller_id_name">>, DataJObj)}
       ,{<<"number">>, wh_json:get_value(<<"caller_id_number">>, DataJObj)}
      ]).

-spec callee_id_data(wh_json:object()) -> wh_proplist().
callee_id_data(DataJObj) ->
    props:filter_undefined(
      [{<<"name">>, wh_json:get_value(<<"callee_id_name">>, DataJObj)}
       ,{<<"number">>, wh_json:get_value(<<"callee_id_number">>, DataJObj)}
      ]).

-spec date_called_data(wh_json:object()) -> wh_proplist().
date_called_data(DataJObj) ->
    DateCalled = wh_json:get_integer_value(<<"fax_timestamp">>, DataJObj, wh_util:current_tstamp()),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),
    Timezone = wh_json:get_value([<<"fax">>, <<"rx_result">>, <<"timezone">>], DataJObj, <<"UTC">>),
    ClockTimezone = whapps_config:get(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    props:filter_undefined(
      [{<<"utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
       ,{<<"local">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
      ]).

-spec from_data(wh_json:object()) -> wh_proplist().
from_data(DataJObj) ->
    FromE164 = wh_json:get_value(<<"from_user">>, DataJObj),

    props:filter_undefined(
      [{<<"from_user">>, wnm_util:pretty_print(FromE164)}
       ,{<<"from_realm">>, wh_json:get_value(<<"from_realm">>, DataJObj)}
      ]).

-spec to_data(wh_json:object()) -> wh_proplist().
to_data(DataJObj) ->
    ToE164 = wh_json:get_value(<<"to_user">>, DataJObj),
    props:filter_undefined(
      [{<<"user">>, wnm_util:pretty_print(ToE164)}
       ,{<<"realm">>, wh_json:get_value(<<"to_realm">>, DataJObj)}
      ]).

-spec to_email_addresses(wh_json:object()) -> api_binaries().
to_email_addresses(DataJObj) ->
    to_email_addresses(DataJObj
                       ,wh_json:get_first_defined([[<<"to">>, <<"email_addresses">>]
                                                   ,[<<"fax">>, <<"email">>, <<"send_to">>]
                                                   ,[<<"owner">>, <<"email">>]
                                                   ,[<<"owner">>, <<"username">>]
                                                  ]
                                                  ,DataJObj
                                                 )
                      ).

-spec to_email_addresses(wh_json:object(), ne_binary() | api_binaries()) -> api_binaries().
to_email_addresses(_DataJObj, <<_/binary>> = Email) ->
    [Email];
to_email_addresses(_DataJObj, [_|_] = Emails) ->
    Emails;
to_email_addresses(DataJObj, _) ->
    case teletype_util:find_account_rep_email(wh_json:get_value(<<"account">>, DataJObj)) of
        'undefined' ->
            lager:debug("failed to find account rep email, using defaults"),
            default_to_addresses();
        Emails ->
            lager:debug("using ~p for To", [Emails]),
            Emails
    end.

-spec default_to_addresses() -> api_binaries().
default_to_addresses() ->
    case whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>) of
        'undefined' -> 'undefined';
        <<_/binary>> = Email -> [Email];
        [_|_]=Emails -> Emails
    end.

-spec build_fax_template_data(wh_json:object()) -> wh_proplist().
build_fax_template_data(DataJObj) ->
    FaxJObj = wh_json:get_value(<<"fax">>, DataJObj),
    props:filter_undefined(
      [{<<"id">>, wh_json:get_value(<<"fax_id">>, DataJObj)}
       ,{<<"media">>, wh_json:get_value(<<"fax_name">>, DataJObj)}
       | wh_json:to_proplist(wh_json:get_value(<<"rx_results">>, FaxJObj, wh_json:new()))
      ]).
