%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Max Lay
%%%-------------------------------------------------------------------
-module(teletype_fax_outbound_smtp_error_to_email).

-export([init/0
        ,handle_fax_outbound_smtp_error/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"fax_outbound_smtp_error_to_email">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).
-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).

-define(TEMPLATE_MACROS, kz_json:from_list([])).

-define(TEMPLATE_SUBJECT, <<"Error Sending Fax">>).
-define(TEMPLATE_CATEGORY, <<"fax">>).
-define(TEMPLATE_NAME, <<"Outbound Fax SMTP Error to Email">>).

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
                                          ,{'from', ?TEMPLATE_FROM}
                                          ,{'cc', ?TEMPLATE_CC}
                                          ,{'bcc', ?TEMPLATE_BCC}
                                          ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                          ]),
    teletype_bindings:bind(<<"outbound_smtp_fax_error">>, ?MODULE, 'handle_fax_outbound_smtp_error').

-spec handle_fax_outbound_smtp_error(kz_json:object()) -> 'ok'.
handle_fax_outbound_smtp_error(JObj) ->
    'true' = kapi_notifications:fax_outbound_smtp_error_v(JObj),
    kz_util:put_callid(JObj),

    lager:debug("processing fax outbound error to email"),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = build_macros(DataJObj),

    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, teletype_util:find_account_id(DataJObj)),

    Subject = teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                          ,Macros
                                          ),

    EmailsJObj =
        case teletype_util:is_preview(DataJObj) of
            'true' -> DataJObj;
            'false' ->
                kz_json:set_value(<<"to">>, [kz_json:get_ne_binary_value(<<"fax_from_email">>, DataJObj)], DataJObj)
        end,

    Emails = teletype_util:find_addresses(EmailsJObj
                                         ,TemplateMetaJObj
                                         ,?MOD_CONFIG_CAT
                                         ),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec build_macros(kz_json:object()) -> kz_proplist().
build_macros(DataJObj) ->
    [Error | _]=Errors = kz_json:get_list_value(<<"errors">>, DataJObj),

    [{<<"errors">>, Errors}
    ,{<<"error">>, Error}
    ,{<<"to_email">>, kz_json:get_ne_binary_value(<<"fax_to_email">>, DataJObj)}
    ,{<<"from_email">>, kz_json:get_ne_binary_value(<<"fax_from_email">>, DataJObj)}
    ,{<<"account_id">>, kz_json:get_ne_binary_value(<<"account_id">>, DataJObj)}
    ].
