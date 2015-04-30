%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_fax_inbound_error_to_email).

-export([init/0
         ,handle_fax_inbound_error/2
        ]).

-include("../teletype.hrl").

-define(TEMPLATE_ID, <<"fax_inbound_error_to_email">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).
-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"call_id">>, <<"call_id">>, <<"Call ID">>, <<"Call ID of the fax transmission">>)
            ,?MACRO_VALUE(<<"fax.info">>, <<"fax_info">>, <<"Fax Info">>, <<"Fax Info">>)
            ,?MACRO_VALUE(<<"fax.id">>, <<"fax_id">>, <<"Fax ID">>, <<"Fax ID">>)
            ,?MACRO_VALUE(<<"fax.box_id">>, <<"fax_box_id">>, <<"FaxBox ID">>, <<"FaxBox ID">>)
            ,?MACRO_VALUE(<<"fax.error">>, <<"fax_error">>, <<"Fax Error">>, <<"Fax Error">>)
            ,?MACRO_VALUE(<<"fax.timestamp">>, <<"fax_timestamp">>, <<"Fax Timestamp">>, <<"Fax Timestamp">>)
            ,?MACRO_VALUE(<<"fax.remote_station_id">>, <<"fax_remote_station_id">>, <<"Fax Remote Station ID">>, <<"Fax Remote Station ID">>)
            | ?DEFAULT_CALL_MACROS
           ]
          )).

-define(TEMPLATE_TEXT, <<"Error : {% firstof error.fax_info error.call_info \"unknown error\" %} \n\nCaller ID: {% firstof fax.remote_station_id caller_id.number \"unknown number\" %}\nCaller Name: {% firstof caller_id.name fax.remote_station_id caller_id.number \"unknown number\" %}\n\nCalled To: {{to.user}}   (Originally dialed number)\nCalled On: {{fax.timestamp|date:\"l, F j, Y \\a\\t H:i\"}}\n\n\nFor help or questions about receiving faxes, please contact support at (415) 886-7900 or email support@2600hz.com.">>).
-define(TEMPLATE_HTML, <<"<html><body><h3>Error : {% firstof error.fax_info error.call_info \"unknown error\" %} </h3><table><tr><td>Caller ID</td><td>{% firstof caller_id.name fax.remote_station_id caller_id.number \"unknown number\" %} ({% firstof fax.remote_station_id caller_id.number \"unknown number\" %})</td></tr><tr><td>Callee ID</td><td>{{to.user}} (originally dialed number)</td></tr><tr><td>Call received</td><td>{{fax.timestamp|date:\"l, F j, Y \\a\\t H:i\"}}</td></tr></table><p>For help or questions about receiving faxes, please contact (415) 886-7900 or email <a href=\"mailto:support@2600hz.com\">Support</a></p><p style=\"font-size: 9px;color:#C0C0C0\">{{call_id}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Error Receiving Fax from {% firstof caller_id.name fax.remote_station_id caller_id.number \"unknown number\" %} ({% firstof fax.remote_station_id caller_id.number \"unknown number\" %})">>).
-define(TEMPLATE_CATEGORY, <<"fax">>).
-define(TEMPLATE_NAME, <<"Inbound Fax Error to Email">>).

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

-spec handle_fax_inbound_error(wh_json:object(), wh_proplist()) -> 'ok'.
handle_fax_inbound_error(JObj, _Props) ->
    'true' = wapi_notifications:fax_inbound_error_v(JObj),
    wh_util:put_callid(JObj),

    lager:debug("processing fax inbound error to email"),

    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:should_handle_notification(DataJObj)
        andalso teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID)
    of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_fax_inbound(DataJObj)
    end.

-spec handle_fax_inbound(wh_json:object()) -> 'ok'.
handle_fax_inbound(DataJObj) ->
    FaxJObj = teletype_fax_util:get_fax_doc(DataJObj),
    OwnerJObj = get_owner_doc(FaxJObj, DataJObj),

    Macros = build_template_data(
               wh_json:set_values([{<<"account">>, teletype_util:account_params(DataJObj)}
                                   ,{<<"fax">>, wh_doc:public_fields(FaxJObj)}
                                   ,{<<"owner">>, wh_doc:public_fields(OwnerJObj)}
                                   ,{<<"error">>, error_data(DataJObj)}
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

    {'ok', TemplateMetaJObj} = teletype_util:fetch_template_meta(?TEMPLATE_ID, teletype_util:find_account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                ,Macros
               ),
    lager:debug("rendered subject: ~s", [Subject]),

    EmailsJObj =
        case teletype_util:is_preview(DataJObj) of
            'true' -> DataJObj;
            'false' ->
                wh_json:set_value(<<"to">>, to_email_addresses(FaxJObj), DataJObj)
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

-spec error_data(wh_json:object()) -> wh_json:object().
error_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'false' ->
            wh_json:from_list(
              [{<<"call_info">>, wh_json:get_value(<<"fax_error">>, DataJObj)}
               ,{<<"fax_info">>, wh_json:get_value([<<"fax_info">>, <<"fax_result_text">>], DataJObj)}
              ]);
        'true'->
            wh_json:from_list(
              [{<<"call_info">>, <<"CALL_INFO">>}
               ,{<<"fax_info">>, <<"FAX_INFO">>}
              ])
    end.

-spec get_owner_doc(wh_json:object(), wh_json:object()) -> wh_json:object().
get_owner_doc(FaxJObj, DataJObj) ->
    OwnerId = wh_json:get_value(<<"owner_id">>, FaxJObj),
    case teletype_util:open_doc(<<"user">>, OwnerId, DataJObj) of
        {'ok', OwnerJObj} -> OwnerJObj;
        {'error', _} -> wh_json:new()
    end.

-spec build_template_data(wh_json:object()) -> wh_proplist().
build_template_data(DataJObj) ->
    [{<<"account">>, teletype_util:public_proplist(<<"account">>, DataJObj)}
     ,{<<"fax">>, build_fax_template_data(DataJObj)}
     ,{<<"system">>, teletype_util:system_params()}
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
                                                   ,[<<"fax_notifications">>, <<"email">>, <<"send_to">>]
                                                   ,[<<"notifications">>, <<"email">>, <<"send_to">>]
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
    props:filter_undefined(
      [{<<"id">>, wh_json:get_value(<<"fax_id">>, DataJObj)}
       ,{<<"info">>, wh_json:get_value(<<"fax_info">>, DataJObj)}
       ,{<<"box_id">>, wh_json:get_value(<<"faxbox_id">>, DataJObj)}
       ,{<<"timestamp">>, wh_json:get_value(<<"fax_timestamp">>, DataJObj)}
       ,{<<"notifications">>, wh_json:get_value(<<"fax_notifications">>, DataJObj)}
       ,{<<"remote_station_id">>, wh_json:get_value(<<"fax_remote_station_id">>, DataJObj)}
      ]).
