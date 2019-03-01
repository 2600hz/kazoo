%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_port_request).

-export([init/0
        ,id/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"port_request">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          ?PORT_REQUEST_MACROS
          ++ ?COMMON_TEMPLATE_MACROS
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Number port request for account '{{account.name|safe}}'">>).
-define(TEMPLATE_CATEGORY, <<"port_request">>).
-define(TEMPLATE_NAME, <<"Port Request">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address()).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to()).

-spec id() -> kz_term:ne_binary().
id() -> ?TEMPLATE_ID.

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
    teletype_bindings:bind(<<"port_request">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:port_request_v(JObj)).

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
        'true' ->
            %% TODO: this notification and admin version is almost same
            %% when we're ready to merge admin and port request notfiication
            %% we should remove one of them.
            process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    NewData = teletype_port_utils:port_request_data(DataJObj, ?TEMPLATE_ID),
    case teletype_util:is_preview(NewData) of
        'false' -> handle_port_request(NewData);
        'true' -> handle_port_request(kz_json:merge_jobjs(DataJObj, NewData))
    end.

-spec handle_port_request(kz_json:object()) -> template_response().
handle_port_request(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"port_request">>, teletype_util:public_proplist(<<"port_request">>, DataJObj)}
             ],

    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID, kapi_notifications:account_id(DataJObj)),

    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT),
    Subject = teletype_util:render_subject(Subject0, Macros),

    send_emails(DataJObj, TemplateMetaJObj, Subject, RenderedTemplates, teletype_util:is_preview(DataJObj)).


-spec send_emails(kz_json:object(), kz_json:object(), kz_term:ne_binary(), rendered_templates(), boolean()) -> template_response().
send_emails(DataJObj, TemplateMetaJObj, Subject, RenderedTemplates, 'true') ->
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?TEMPLATE_ID),
    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} ->
            teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end;
send_emails(DataJObj, TemplateMetaJObj, Subject, RenderedTemplates, 'false') ->
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?TEMPLATE_ID),
    EmailAttachements = kz_json:get_value(<<"attachments">>, DataJObj),

    lager:debug("sending ~s to port sumbitter (or template default): ~p"
               ,[?TEMPLATE_ID, props:get_value(<<"to">>, Emails)]
               ),
    _ = teletype_util:send_email(Emails, Subject, RenderedTemplates, EmailAttachements),

    AuthorityEmails = props:set_value(<<"to">>
                                     ,kz_json:get_value(<<"authority_emails">>, DataJObj, [])
                                     ,props:delete_keys([<<"bcc">>, <<"cc">>], Emails)
                                     ),

    lager:debug("sending ~s to port authority: ~p"
               ,[?TEMPLATE_ID, props:get_value(<<"to">>, AuthorityEmails)]
               ),
    _ = put('skip_smtp_log', 'true'),
    case teletype_util:send_email(AuthorityEmails, Subject, RenderedTemplates, EmailAttachements) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} ->
            lager:debug("unable to send emails to port authority: ~p", [Reason]),
            teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end.
