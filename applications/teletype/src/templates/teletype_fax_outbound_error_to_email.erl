%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_fax_outbound_error_to_email).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"fax_outbound_error_to_email">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).
-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          ?FAX_ERROR_MACROS
          ++ ?FAX_MACROS
          ++ ?DEFAULT_CALL_MACROS
          ++ ?USER_MACROS
          ++ ?COMMON_TEMPLATE_MACROS
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Error sending Fax to {{callee_id.name}} ({% firstof fax.remote_station_id callee_id.number \"Unknown Number\" %})">>).
-define(TEMPLATE_CATEGORY, <<"fax">>).
-define(TEMPLATE_NAME, <<"Outbound Fax Error to Email">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
                                          ,{'subject', ?TEMPLATE_SUBJECT}
                                          ,{'category', ?TEMPLATE_CATEGORY}
                                          ,{'friendly_name', ?TEMPLATE_NAME}
                                          ,{'to', ?TEMPLATE_TO}
                                          ,{'from', ?TEMPLATE_FROM}
                                          ,{'cc', ?TEMPLATE_CC}
                                          ,{'bcc', ?TEMPLATE_BCC}
                                          ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                          ]),
    teletype_bindings:bind(<<"outbound_fax_error">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:fax_outbound_error_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> 'ok'.
handle_req(JObj, 'false') ->
    lager:debug("invalid data for ~s", [?TEMPLATE_ID]),
    teletype_util:send_update(JObj, <<"failed">>, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?TEMPLATE_ID]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> teletype_util:notification_disabled(DataJObj, ?TEMPLATE_ID);
        'true' -> process_req(teletype_fax_util:add_data(DataJObj))
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    TemplateData = build_template_data(DataJObj),
    {Macros, EmailAttachements} = teletype_fax_util:add_attachments(DataJObj, TemplateData, 'false'),

    %% Load templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),
    lager:debug("rendered templates"),

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, kapi_notifications:account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT), Macros
               ),
    lager:debug("rendered subject: ~s", [Subject]),

    EmailsJObj =
        case teletype_util:is_preview(DataJObj) of
            'true' -> DataJObj;
            'false' ->
                kz_json:set_value(<<"to">>, teletype_fax_util:to_email_addresses(DataJObj, ?MOD_CONFIG_CAT), DataJObj)
        end,

    Emails = teletype_util:find_addresses(EmailsJObj
                                         ,TemplateMetaJObj
                                         ,?MOD_CONFIG_CAT
                                         ),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates, EmailAttachements) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec build_template_data(kz_json:object()) -> kz_proplist().
build_template_data(DataJObj) ->
    Timezone = kz_json:get_value(<<"timezone">>, DataJObj),
    props:filter_undefined(
      [{<<"account">>, teletype_util:account_params(DataJObj)}
      ,{<<"fax">>, build_fax_template_data(DataJObj)}
      ,{<<"system">>, teletype_util:system_params()}
      ,{<<"error">>, kz_json:to_proplist(<<"error">>, DataJObj)}
      ,{<<"user">>, teletype_util:user_params(kz_json:get_value(<<"owner">>, DataJObj))}
       | teletype_util:build_call_data(DataJObj, Timezone)
      ]).

-spec build_fax_template_data(kz_json:object()) -> kz_proplist().
build_fax_template_data(DataJObj) ->
    FaxJObj = kz_json:get_value(<<"fax_doc">>, DataJObj),
    FaxBoxJObj = kz_json:get_value(<<"faxbox">>, DataJObj),
    props:filter_undefined(
      [{<<"info">>, kz_json:to_proplist(<<"fax_info">>, DataJObj)}
      ,{<<"remote_station_id">>, kz_json:get_value(<<"fax_remote_station_id">>, DataJObj)}
      ,{<<"id">>, kz_json:get_value(<<"fax_id">>, DataJObj)}
      ,{<<"box_id">>, kz_json:get_value(<<"faxbox_id">>, DataJObj, kz_doc:id(FaxBoxJObj))}
      ,{<<"box_name">>, kz_json:get_value(<<"name">>, FaxBoxJObj)}
      ,{<<"timestamp">>, kz_json:get_value(<<"fax_timestamp">>, DataJObj, kz_time:current_tstamp())}
       | kz_json:to_proplist(kz_json:get_value(<<"tx_result">>, FaxJObj, kz_json:new()))
      ]).
