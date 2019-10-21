%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_port_comment).

-export([init/0
        ,id/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"port_comment">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          ?PORT_REQUEST_MACROS
          ++ ?USER_MACROS
          ++ ?COMMON_TEMPLATE_MACROS
         )
       ).

-define(TEMPLATE_SUBJECT, <<"New comment for port request '{{port_request.name|safe}}'">>).
-define(TEMPLATE_CATEGORY, <<"port_request">>).
-define(TEMPLATE_NAME, <<"Port Comment">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address()).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to()).

-spec id() -> kz_term:ne_binary().
id() -> ?TEMPLATE_ID.

-spec init() -> 'ok'.
init() ->
    kz_log:put_callid(?MODULE),
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
    teletype_bindings:bind(<<"port_comment">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:port_comment_v(JObj)).

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
    NewData = teletype_port_utils:port_request_data(DataJObj, ?TEMPLATE_ID),
    case teletype_util:is_preview(NewData) of
        'false' -> handle_port_request(NewData);
        'true' -> handle_port_request(kz_json:merge_jobjs(DataJObj, NewData))
    end.

-spec handle_port_request(kz_json:object()) -> template_response().
handle_port_request(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"user">>, user_data(DataJObj)}
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
    maybe_send_to_submitter(Emails, Subject, RenderedTemplates),

    AuthorityEmails = props:set_value(<<"to">>
                                     ,kz_json:get_value(<<"authority_emails">>, DataJObj, [])
                                     ,props:delete_keys([<<"bcc">>, <<"cc">>], Emails)
                                     ),

    lager:debug("sending ~s to port authority: ~p"
               ,[?TEMPLATE_ID, props:get_value(<<"to">>, AuthorityEmails)]
               ),
    _ = put('skip_smtp_log', 'true'),
    case teletype_util:send_email(AuthorityEmails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} ->
            lager:debug("unable to send emails to port authority: ~p", [Reason]),
            teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end.

-spec maybe_send_to_submitter(email_map(), kz_term:ne_binary(), rendered_templates()) -> 'ok'.
maybe_send_to_submitter([], _, _) ->
    lager:debug("no port submitter email addresses were found in data or template");
maybe_send_to_submitter(Emails, Subject, RenderedTemplates) ->
    lager:debug("sending ~s to port sumbitter (or template default): ~p"
               ,[?TEMPLATE_ID, props:get_value(<<"to">>, Emails)]
               ),
    _ = teletype_util:send_email(Emails, Subject, RenderedTemplates),
    'ok'.

-spec user_data(kz_json:object()) -> kz_term:proplist().
user_data(DataJObj) ->
    user_data(DataJObj, teletype_util:is_preview(DataJObj)).

-spec user_data(kz_json:object(), boolean()) -> kz_term:proplist().
user_data(DataJObj, 'true') ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    teletype_util:user_params(teletype_util:find_account_admin(AccountId));
user_data(DataJObj, 'false') ->
    AccountId = kz_json:get_value([<<"port_request">>, <<"comment">>, <<"account_id">>], DataJObj),
    UserId = kz_json:get_value([<<"port_request">>, <<"comment">>, <<"user_id">>], DataJObj),
    case kzd_users:fetch(AccountId, UserId) of
        {error, _} -> [];
        {ok, UserJObj} -> teletype_util:user_params(UserJObj)
    end.
