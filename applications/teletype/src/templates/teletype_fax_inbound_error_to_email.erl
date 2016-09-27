%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz Inc
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

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"fax_inbound_error_to_email">>).
-define(TEMPLATE_ID_FILTERED, <<"fax_inbound_error_to_email_filtered">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).
-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"call_id">>, <<"call_id">>, <<"Call ID">>, <<"Call ID of the fax transmission">>)
          ,?MACRO_VALUE(<<"fax.info">>, <<"fax_info">>, <<"Fax Info">>, <<"Fax Info">>)
          ,?MACRO_VALUE(<<"fax.id">>, <<"fax_id">>, <<"Fax ID">>, <<"Fax ID">>)
          ,?MACRO_VALUE(<<"fax.box_id">>, <<"fax_box_id">>, <<"FaxBox ID">>, <<"FaxBox ID">>)
          ,?MACRO_VALUE(<<"fax.timestamp">>, <<"fax_timestamp">>, <<"Fax Timestamp">>, <<"Fax Timestamp">>)
          ,?MACRO_VALUE(<<"fax.remote_station_id">>, <<"fax_remote_station_id">>, <<"Fax Remote Station ID">>, <<"Fax Remote Station ID">>)
          ,?MACRO_VALUE(<<"error.call_info">>, <<"error_call_info">>, <<"Fax Call Error">>, <<"Fax Call Error">>)
          ,?MACRO_VALUE(<<"error.fax_info">>, <<"error_fax_info">>, <<"Fax Processor Error">>, <<"Fax Processor Error">>)
           | ?DEFAULT_CALL_MACROS
          ]
         )).

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
    kz_util:put_callid(?MODULE),
    Fields = [{'macros', ?TEMPLATE_MACROS}
             ,{'subject', ?TEMPLATE_SUBJECT}
             ,{'category', ?TEMPLATE_CATEGORY}
             ,{'friendly_name', ?TEMPLATE_NAME}
             ,{'to', ?TEMPLATE_TO}
             ,{'from', ?TEMPLATE_FROM}
             ,{'cc', ?TEMPLATE_CC}
             ,{'bcc', ?TEMPLATE_BCC}
             ,{'reply_to', ?TEMPLATE_REPLY_TO}],
    teletype_templates:init(?TEMPLATE_ID_FILTERED, Fields),
    teletype_templates:init(?TEMPLATE_ID, Fields).

-spec handle_fax_inbound_error(kz_json:object(), kz_proplist()) -> 'ok'.
handle_fax_inbound_error(JObj, _Props) ->
    'true' = kapi_notifications:fax_inbound_error_v(JObj),
    kz_util:put_callid(JObj),

    lager:debug("processing fax inbound error to email"),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_fax_inbound(DataJObj, ?TEMPLATE_ID)
    end,
    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID_FILTERED) and is_true_fax_error(JObj) of
        'false' -> lager:debug("filtered notification handling not configured for this account");
        'true' -> handle_fax_inbound(DataJObj, ?TEMPLATE_ID_FILTERED)
    end.

-spec is_true_fax_error(kz_json:object()) -> boolean().
is_true_fax_error(JObj) ->
    Code = kz_json:get_value(<<"Fax-Result-Code">>, JObj),
    %% see: https://wiki.freeswitch.org/wiki/Variable_fax_result_code
    Codes = kapps_config:get(?MOD_CONFIG_CAT, <<"filter_error_codes">>, [<<"49">>]),
    lists:member(Code, Codes).

-spec handle_fax_inbound(kz_json:object(), ne_binary()) -> 'ok'.
handle_fax_inbound(DataJObj, TemplateId) ->
    Macros = build_template_data(
               kz_json:set_values([{<<"error">>, error_data(DataJObj)}], DataJObj)),

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
                kz_json:set_value(<<"to">>, to_email_addresses(DataJObj), DataJObj)
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

-spec error_data(kz_json:object()) -> kz_json:object().
error_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'false' ->
            kz_json:from_list(
              [{<<"call_info">>, kz_json:get_value(<<"fax_error">>, DataJObj)}
              ,{<<"fax_info">>, kz_json:get_value([<<"fax_info">>, <<"fax_result_text">>], DataJObj)}
              ]);
        'true'->
            kz_json:from_list(
              [{<<"call_info">>, <<"CALL_INFO">>}
              ,{<<"fax_info">>, <<"FAX_INFO">>}
              ])
    end.

-spec build_template_data(kz_json:object()) -> kz_proplist().
build_template_data(DataJObj) ->
    [{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"fax">>, build_fax_template_data(DataJObj)}
    ,{<<"system">>, teletype_util:system_params()}
    ,{<<"caller_id">>, caller_id_data(DataJObj)}
    ,{<<"callee_id">>, callee_id_data(DataJObj)}
    ,{<<"date_called">>, date_called_data(DataJObj)}
    ,{<<"from">>, from_data(DataJObj)}
    ,{<<"to">>, to_data(DataJObj)}
    ,{<<"call_id">>, kz_json:get_value(<<"call_id">>, DataJObj)}
    ,{<<"error">>, kz_json:to_proplist(<<"error">>, DataJObj)}
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
    DateCalled = kz_json:get_integer_value(<<"fax_timestamp">>, DataJObj, kz_util:current_tstamp()),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),
    Timezone = kz_json:get_value([<<"fax">>, <<"rx_result">>, <<"timezone">>], DataJObj, <<"UTC">>),
    ClockTimezone = kapps_config:get(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    props:filter_undefined(
      [{<<"utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
      ,{<<"local">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
      ]).

-spec from_data(kz_json:object()) -> kz_proplist().
from_data(DataJObj) ->
    FromE164 = kz_json:get_value(<<"from_user">>, DataJObj),
    props:filter_undefined(
      [{<<"from_user">>, knm_util:pretty_print(FromE164)}
      ,{<<"from_realm">>, kz_json:get_value(<<"from_realm">>, DataJObj)}
      ]).

-spec to_data(kz_json:object()) -> kz_proplist().
to_data(DataJObj) ->
    ToE164 = kz_json:get_value(<<"to_user">>, DataJObj),
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
    props:filter_undefined(
      [{<<"id">>, kz_json:get_value(<<"fax_id">>, DataJObj)}
      ,{<<"info">>, kz_json:to_proplist(<<"fax_info">>, DataJObj)}
      ,{<<"box_id">>, kz_json:get_value(<<"faxbox_id">>, DataJObj)}
      ,{<<"timestamp">>, kz_json:get_value(<<"fax_timestamp">>, DataJObj)}
      ,{<<"notifications">>, kz_json:get_value(<<"fax_notifications">>, DataJObj)}
      ,{<<"remote_station_id">>, kz_json:get_value(<<"fax_remote_station_id">>, DataJObj)}
      ]).
