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
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"fax_inbound_error_to_email">>).
-define(TEMPLATE_ID_FILTERED, <<"fax_inbound_error_to_email_filtered">>).
-define(MOD_CONFIG_CAT(TemplateId), ?TEMPLATE_CONFIG_CAT(TemplateId)).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          ?FAX_ERROR_MACROS
          ++ ?FAX_MACROS
          ++ ?DEFAULT_CALL_MACROS
          ++ ?USER_MACROS
          ++ ?COMMON_TEMPLATE_MACROS
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Error receiving fax from {% firstof caller_id.name fax.remote_station_id %} ({% firstof fax.remote_station_id caller_id.number \"Unknown Number\" %})">>).
-define(TEMPLATE_CATEGORY, <<"fax">>).
-define(TEMPLATE_NAME, <<"Inbound Fax Negotiation Error to Email">>).
-define(FILTERED_TEMPLATE_NAME, <<"Inbound Fax Receive Error to Email">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_FILTERED_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM(Id), teletype_util:default_from_address(?MOD_CONFIG_CAT(Id))).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO(Id), teletype_util:default_reply_to(?MOD_CONFIG_CAT(Id))).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    Fields = [{'macros', ?TEMPLATE_MACROS}
             ,{'subject', ?TEMPLATE_SUBJECT}
             ,{'category', ?TEMPLATE_CATEGORY}
             ,{'cc', ?TEMPLATE_CC}
             ,{'bcc', ?TEMPLATE_BCC}
             ],
    FilteredParams = [{'friendly_name', ?FILTERED_TEMPLATE_NAME}
                     ,{'to', ?TEMPLATE_FILTERED_TO}
                     ,{'from', ?TEMPLATE_FROM(?TEMPLATE_ID_FILTERED)}
                     ,{'reply_to', ?TEMPLATE_REPLY_TO(?TEMPLATE_ID_FILTERED)}
                      | Fields
                     ],
    UnfilteredParams = [{'friendly_name', ?TEMPLATE_NAME}
                       ,{'to', ?TEMPLATE_TO}
                       ,{'from', ?TEMPLATE_FROM(?TEMPLATE_ID)}
                       ,{'reply_to', ?TEMPLATE_REPLY_TO(?TEMPLATE_ID)}
                        | Fields
                       ],
    teletype_templates:init(?TEMPLATE_ID_FILTERED, FilteredParams),
    teletype_templates:init(?TEMPLATE_ID, UnfilteredParams),
    teletype_bindings:bind(<<"inbound_fax_error">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:fax_inbound_error_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> 'ok'.
handle_req(JObj, 'false') ->
    lager:debug("invalid data for ~s", [?TEMPLATE_ID]),
    teletype_util:send_update(JObj, <<"failed">>, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?TEMPLATE_ID]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) =/= ?TEMPLATE_ID
        andalso is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID_FILTERED)
    of
        'disabled' ->
            teletype_util:notification_disabled(DataJObj, kz_binary:join(["both ", ?TEMPLATE_ID, " and ", ?TEMPLATE_ID_FILTERED]));
        'false' ->
            lager:debug("handling ~s", [?TEMPLATE_ID]),
            handle_fax_inbound(teletype_fax_util:add_data(DataJObj), ?TEMPLATE_ID);
        ?TEMPLATE_ID_FILTERED ->
            lager:debug("handling ~s", [?TEMPLATE_ID_FILTERED]),
            handle_fax_inbound(teletype_fax_util:add_data(DataJObj), ?TEMPLATE_ID_FILTERED)
    end.

-spec is_notice_enabled(ne_binary(), kz_json:object(), ne_binary()) -> ne_binary() | 'disabled'.
is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) ->
    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'true' -> ?TEMPLATE_ID;
        'false' -> 'disabled'
    end;
is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID_FILTERED) ->
    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID_FILTERED)
        andalso is_true_fax_error(AccountId, JObj)
    of
        'false' -> 'disabled';
        'true' -> ?TEMPLATE_ID_FILTERED
    end.

-spec is_true_fax_error(ne_binary(), kz_json:object()) -> boolean().
is_true_fax_error(AccountId, JObj) ->
    Code = kz_json:get_value(<<"Fax-Result-Code">>, JObj),
    %% see: https://wiki.freeswitch.org/wiki/Variable_fax_result_code
    DefaultCodes = kapps_config:get(?MOD_CONFIG_CAT(?TEMPLATE_ID_FILTERED), <<"filter_error_codes">>, [<<"0">>, <<"49">>]),
    Codes = kapps_account_config:get(AccountId, ?MOD_CONFIG_CAT(?TEMPLATE_ID_FILTERED), <<"filter_error_codes">>, DefaultCodes),
    not lists:member(Code, Codes).

-spec handle_fax_inbound(kz_json:object(), ne_binary()) -> 'ok'.
handle_fax_inbound(DataJObj, TemplateId) ->
    TemplateData = build_template_data(DataJObj),
    {Macros, EmailAttachements} = teletype_fax_util:add_attachments(DataJObj, TemplateData, 'false'),

    %% Populate templates
    RenderedTemplates = teletype_templates:render(TemplateId, Macros, DataJObj),
    lager:debug("rendered templates"),

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(TemplateId, kapi_notifications:account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                          ,Macros
               ),
    lager:debug("rendered subject: ~s", [Subject]),

    EmailsJObj =
        case teletype_util:is_preview(DataJObj) of
            'true' -> DataJObj;
            'false' ->
                kz_json:set_value(<<"to">>, teletype_fax_util:to_email_addresses(DataJObj, ?MOD_CONFIG_CAT(TemplateId)), DataJObj)
        end,

    Emails = teletype_util:find_addresses(EmailsJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT(TemplateId)),

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
      ,{<<"timestamp">>, kz_json:get_value(<<"fax_timestamp">>, DataJObj, kz_time:now_s())}
       | kz_json:to_proplist(kz_json:get_value(<<"tx_result">>, FaxJObj, kz_json:new()))
      ]).
