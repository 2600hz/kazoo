%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_fax_inbound_error_to_email).

-export([init/0
        ,handle_fax_inbound_error/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"fax_inbound_error_to_email">>).
-define(TEMPLATE_ID_FILTERED, <<"fax_inbound_error_to_email_filtered">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).
-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          ?FAX_ERROR_MACROS
          ++ ?FAX_MACROS
          ++ ?DEFAULT_CALL_MACROS
          ++ ?USER_MACROS
         )).

-define(TEMPLATE_SUBJECT, <<"Error receiving fax from {% firstof caller_id.name fax.remote_station_id %} ({% firstof fax.remote_station_id caller_id.number \"Unknown Number\" %})">>).
-define(TEMPLATE_CATEGORY, <<"fax">>).
-define(TEMPLATE_NAME, <<"Inbound Fax Negotiation Error to Email">>).
-define(FILTERED_TEMPLATE_NAME, <<"Inbound Fax Receive Error to Email">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_FILTERED_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    Fields = [{'macros', ?TEMPLATE_MACROS}
             ,{'subject', ?TEMPLATE_SUBJECT}
             ,{'category', ?TEMPLATE_CATEGORY}
             ,{'from', ?TEMPLATE_FROM}
             ,{'cc', ?TEMPLATE_CC}
             ,{'bcc', ?TEMPLATE_BCC}
             ,{'reply_to', ?TEMPLATE_REPLY_TO}],
    FilteredParams = [{'friendly_name', ?FILTERED_TEMPLATE_NAME}
                     ,{'to', ?TEMPLATE_FILTERED_TO}
                      | Fields
                     ],
    UnfilteredParams = [{'friendly_name', ?TEMPLATE_NAME}
                       ,{'to', ?TEMPLATE_TO}
                        | Fields
                       ],
    teletype_templates:init(?TEMPLATE_ID_FILTERED, FilteredParams),
    teletype_templates:init(?TEMPLATE_ID, UnfilteredParams),
    teletype_bindings:bind(<<"inbound_fax_error">>, ?MODULE, 'handle_fax_inbound_error').

-spec handle_fax_inbound_error(kz_json:object()) -> 'ok'.
handle_fax_inbound_error(JObj) ->
    'true' = kapi_notifications:fax_inbound_error_v(JObj),
    kz_util:put_callid(JObj),

    lager:debug("processing fax inbound error to email"),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_fax_inbound(teletype_fax_util:add_data(DataJObj), ?TEMPLATE_ID)
    end,
    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID_FILTERED)
        andalso is_true_fax_error(AccountId, JObj)
    of
        'false' -> lager:debug("filtered notification handling not configured for this account");
        'true' -> handle_fax_inbound(teletype_fax_util:add_data(DataJObj), ?TEMPLATE_ID_FILTERED)
    end.

-spec is_true_fax_error(ne_binary(), kz_json:object()) -> boolean().
is_true_fax_error(AccountId, JObj) ->
    Code = kz_json:get_value(<<"Fax-Result-Code">>, JObj),
    %% see: https://wiki.freeswitch.org/wiki/Variable_fax_result_code
    DefaultCodes = kapps_config:get(?MOD_CONFIG_CAT, <<"filter_error_codes">>, [<<"0">>, <<"49">>]),
    Codes = kapps_account_config:get(AccountId, ?MOD_CONFIG_CAT, <<"filter_error_codes">>, DefaultCodes),
    not lists:member(Code, Codes).

-spec handle_fax_inbound(kz_json:object(), ne_binary()) -> 'ok'.
handle_fax_inbound(DataJObj, TemplateId) ->
    TemplateData = build_template_data(DataJObj),
    EmailAttachements = teletype_fax_util:get_attachments(DataJObj, TemplateData),
    Macros = teletype_fax_util:maybe_add_document_data(TemplateData, EmailAttachements),

    %% Populate templates
    RenderedTemplates = teletype_templates:render(TemplateId, Macros, DataJObj),
    lager:debug("rendered templates"),

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(TemplateId, teletype_util:find_account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                          ,Macros
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
    FaxBoxJObj = kz_json:get_value(<<"faxbox">>, DataJObj),
    Timezone = kz_json:get_value([<<"fax">>, <<"rx_result">>, <<"timezone">>], DataJObj, kzd_fax_box:timezone(FaxBoxJObj)),
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
    FaxJObj = kz_json:get_value(<<"fax">>, DataJObj),
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
