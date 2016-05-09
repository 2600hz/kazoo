%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_fax_outbound_to_email).

-export([init/0
         ,handle_fax_outbound/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"fax_outbound_to_email">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).
-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).

-define(TEMPLATE_MACROS
        ,kz_json:from_list(
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
           ]
          )).

-define(TEMPLATE_TEXT, <<"Sent New Fax ({{fax.total_pages}} Pages)\n\nCaller ID: {{caller_id.number}}\nCaller Name: {{caller_id.name}}\n\nCalled To: {{to.user}}   (Originally dialed number)\nCalled On: {{date_called.local|date:\"l, F j, Y \\\\a\\\\t H:i\"}}">>).
-define(TEMPLATE_HTML, <<"<html><body><h3>Sent New Fax ({{fax.total_pages}} Pages)</h3><table><tr><td>Caller ID</td><td>{{caller_id.name}} ({{caller_id.number}})</td></tr><tr><td>Callee ID</td><td>{{to.user}} (originally dialed number)</td></tr><tr><td>Call received</td><td>{{date_called.local|date:\"l, F j, Y \\\\a\\\\t H:i\"}}</td></tr></table><p style=\"font-size: 9px;color:#C0C0C0\">{{call_id}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Successfully sent fax to {{callee_id.name}} ({{callee_id.number}})">>).
-define(TEMPLATE_CATEGORY, <<"fax">>).
-define(TEMPLATE_NAME, <<"Outbound Fax to Email">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
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

-spec handle_fax_outbound(kz_json:object(), kz_proplist()) -> 'ok'.
handle_fax_outbound(JObj, _Props) ->
    'true' = kapi_notifications:fax_outbound_v(JObj),
    kz_util:put_callid(JObj),

    lager:debug("processing fax outbound to email ~p", [JObj]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    FaxJObj = teletype_fax_util:get_fax_doc(DataJObj),
    OwnerJObj = get_owner_doc(FaxJObj, DataJObj),

    Macros = build_template_data(
               kz_json:set_values([{<<"account">>, teletype_util:account_params(DataJObj)}
                                   ,{<<"fax">>, kz_doc:public_fields(FaxJObj)}
                                   ,{<<"owner">>, kz_doc:public_fields(OwnerJObj)}
                                  ]
                                  ,DataJObj
                                 )),

    %% Load templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),
    lager:debug("rendered templates"),

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, teletype_util:find_account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                ,Macros
               ),
    lager:debug("rendered subject: ~s", [Subject]),

    EmailsJObj =
        case teletype_util:is_preview(DataJObj) of
            'true' -> DataJObj;
            'false' ->
                kz_json:set_value(<<"to">>, to_email_addresses(FaxJObj), DataJObj)
        end,

    Emails = teletype_util:find_addresses(EmailsJObj
                                          ,TemplateMetaJObj
                                          ,?MOD_CONFIG_CAT
                                         ),

    EmailAttachements = teletype_fax_util:get_attachments(DataJObj, Macros),
    case teletype_util:send_email(Emails, Subject, RenderedTemplates, EmailAttachements) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec get_owner_doc(kz_json:object(), kz_json:object()) -> kz_json:object().
get_owner_doc(FaxJObj, DataJObj) ->
    OwnerId = kz_json:get_value(<<"owner_id">>, FaxJObj),
    case teletype_util:open_doc(<<"user">>, OwnerId, DataJObj) of
        {'ok', OwnerJObj} -> OwnerJObj;
        {'error', _} -> kz_json:new()
    end.

-spec build_template_data(kz_json:object()) -> kz_proplist().
build_template_data(DataJObj) ->
    [{<<"account">>, kz_json:get_value(<<"account">>, DataJObj)}
     ,{<<"fax">>, build_fax_template_data(DataJObj)}
     ,{<<"system">>, teletype_util:system_params()}
     ,{<<"caller_id">>, caller_id_data(DataJObj)}
     ,{<<"callee_id">>, callee_id_data(DataJObj)}
     ,{<<"date_called">>, date_called_data(DataJObj)}
     ,{<<"from">>, from_data(DataJObj)}
     ,{<<"to">>, to_data(DataJObj)}
     ,{<<"call_id">>, kz_json:get_value(<<"call_id">>, DataJObj)}
    ].

-spec caller_id_data(kz_json:object()) -> kz_proplist().
caller_id_data(DataJObj) ->
    props:filter_undefined(
      [{<<"name">>, kz_json:get_value(<<"caller_id_name">>, DataJObj)}
       ,{<<"number">>, kz_json:get_value(<<"caller_id_number">>, DataJObj)}
      ]).

-spec callee_id_data(kz_json:object()) -> kz_proplist().
callee_id_data(DataJObj) ->
    props:filter_undefined(
      [{<<"name">>, kz_json:get_value(<<"callee_id_name">>, DataJObj)}
       ,{<<"number">>, kz_json:get_value(<<"callee_id_number">>, DataJObj)}
      ]).

-spec date_called_data(kz_json:object()) -> kz_proplist().
date_called_data(DataJObj) ->
    DateCalled = kz_json:get_integer_value(<<"fax_timestamp">>, DataJObj, kz_time:current_tstamp()),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),
    Timezone = kz_json:get_value([<<"fax">>, <<"tx_result">>, <<"timezone">>], DataJObj, <<"UTC">>),
    ClockTimezone = kapps_config:get(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    props:filter_undefined(
      [{<<"utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
       ,{<<"local">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
      ]).

-spec from_data(kz_json:object()) -> kz_proplist().
from_data(DataJObj) ->
    FromE164 = kz_json:get_value(<<"caller_id_number">>, DataJObj),

    props:filter_undefined(
      [{<<"user">>, knm_util:pretty_print(FromE164)}
       ,{<<"realm">>, kz_json:get_value(<<"from_realm">>, DataJObj)}
      ]).

-spec to_data(kz_json:object()) -> kz_proplist().
to_data(DataJObj) ->
    ToE164 = kz_json:get_value(<<"callee_id_number">>, DataJObj),
    props:filter_undefined(
      [{<<"user">>, knm_util:pretty_print(ToE164)}
       ,{<<"realm">>, kz_json:get_value(<<"to_realm">>, DataJObj)}
      ]).

-spec to_email_addresses(kz_json:object()) -> api_binaries().
to_email_addresses(DataJObj) ->
    to_email_addresses(DataJObj
                       ,kz_json:get_first_defined([[<<"to">>, <<"email_addresses">>]
                                                   ,[<<"fax">>, <<"email">>, <<"send_to">>]
                                                   ,[<<"fax_notifications">>, <<"email">>, <<"send_to">>]
                                                   ,[<<"notifications">>, <<"email">>, <<"send_to">>]
                                                   ,[<<"owner">>, <<"email">>]
                                                   ,[<<"owner">>, <<"username">>]
                                                  ]
                                                  ,DataJObj
                                                 )
                      ).

-spec to_email_addresses(kz_json:object(), ne_binary() | api_binaries()) -> api_binaries().
to_email_addresses(_DataJObj, <<_/binary>> = Email) ->
    [Email];
to_email_addresses(_DataJObj, [_|_] = Emails) ->
    Emails;
to_email_addresses(DataJObj, _) ->
    case teletype_util:find_account_rep_email(kz_json:get_value(<<"account">>, DataJObj)) of
        'undefined' ->
            lager:debug("failed to find account rep email, using defaults"),
            default_to_addresses();
        Emails ->
            lager:debug("using ~p for To", [Emails]),
            Emails
    end.

-spec default_to_addresses() -> api_binaries().
default_to_addresses() ->
    case kapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>) of
        'undefined' -> 'undefined';
        <<_/binary>> = Email -> [Email];
        [_|_]=Emails -> Emails
    end.

-spec build_fax_template_data(kz_json:object()) -> kz_proplist().
build_fax_template_data(DataJObj) ->
    FaxJObj = kz_json:get_value(<<"fax">>, DataJObj),
    props:filter_undefined(
      [{<<"id">>, kz_json:get_value(<<"fax_id">>, DataJObj)}
       ,{<<"media">>, kz_json:get_value(<<"fax_name">>, DataJObj)}
       | kz_json:to_proplist(kz_json:get_value(<<"tx_result">>, FaxJObj, kz_json:new()))
      ]).
