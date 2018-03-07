%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_port_pending).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"port_pending">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          ?PORT_REQUEST_MACROS
          ++ ?COMMON_TEMPLATE_MACROS
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Port request: '{{port_request.name}}' is now pending">>).
-define(TEMPLATE_CATEGORY, <<"port_request">>).
-define(TEMPLATE_NAME, <<"Port Pending">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address()).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to()).

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
    teletype_bindings:bind(<<"port_pending">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:port_pending_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [?TEMPLATE_ID]),
    teletype_util:notification_failed(?TEMPLATE_ID, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?TEMPLATE_ID]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> teletype_util:notification_disabled(DataJObj, ?TEMPLATE_ID);
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    PortReqId = kz_json:get_first_defined([<<"port_request_id">>, [<<"port">>, <<"port_id">>]], DataJObj),
    {'ok', PortReqJObj} = teletype_util:open_doc(<<"port_request">>, PortReqId, DataJObj),

    ReqData = kz_json:set_value(<<"port_request">>
                               ,teletype_port_utils:fix_port_request_data(PortReqJObj, DataJObj)
                               ,DataJObj
                               ),

    case teletype_util:is_preview(DataJObj) of
        'false' -> handle_port_request(teletype_port_utils:fix_email(ReqData));
        'true' -> handle_port_request(kz_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec handle_port_request(kz_json:object()) -> template_response().
handle_port_request(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"port_request">>, teletype_util:public_proplist(<<"port_request">>, DataJObj)}
             ],

    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,kapi_notifications:account_id(DataJObj)
                                             ),

    Subject =
        teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                    ,Macros
                                    ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?TEMPLATE_ID),

    EmailAttachements = teletype_port_utils:get_attachments(DataJObj),
    case teletype_util:send_email(Emails, Subject, RenderedTemplates, EmailAttachements) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} -> teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end.
