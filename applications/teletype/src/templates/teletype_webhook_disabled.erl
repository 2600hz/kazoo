%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_webhook_disabled).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"webhook_disabled">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"hook.id">>, <<"hook_id">>, <<"Hook ID">>, <<"Hook ID">>)
          ,?MACRO_VALUE(<<"hook.name">>, <<"hook_name">>, <<"Hook Name">>, <<"Hook Name">>)
          ,?MACRO_VALUE(<<"hook.uri">>, <<"hook_uri">>, <<"Hook URI">>, <<"Hook URI">>)
          ,?MACRO_VALUE(<<"hook.event">>, <<"hook_event">>, <<"Hook Event">>, <<"Hook Event">>)
          ,?MACRO_VALUE(<<"hook.disable_reason">>, <<"hook_disable_reason">>, <<"Disable Reason">>, <<"Why the hook was disabled">>)
           | ?COMMON_TEMPLATE_MACROS
          ]
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Webhook '{{hook.name}}' auto-disabled">>).
-define(TEMPLATE_CATEGORY, <<"webhook">>).
-define(TEMPLATE_NAME, <<"Webhook Auto-Disabled">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address()).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to()).

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
    teletype_bindings:bind(<<"webhook_disabled">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:webhook_disabled_v(JObj)).

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
        'true' -> process_req(DataJObj, AccountId)
    end.

-spec process_req(kz_json:object(), kz_term:ne_binary()) -> template_response().
process_req(DataJObj, AccountId) ->
    HookId = kz_json:get_value(<<"hook_id">>, DataJObj),

    lager:debug("looking for hook ~s in account ~s", [HookId, AccountId]),

    {'ok', HookJObj} = teletype_util:open_doc(<<"webhook">>, HookId, DataJObj),

    ReqData = kz_json:set_value(<<"hook">>, HookJObj, DataJObj),
    process_req(kz_json:merge_jobjs(DataJObj, ReqData)).

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    teletype_util:send_update(DataJObj, <<"pending">>),
    Macros = [{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"hook">>, hook_data(kz_json:get_value(<<"hook">>, DataJObj))}
             ,{<<"system">>, teletype_util:system_params()}
             ],

    %% Populate templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,kapi_notifications:account_id(DataJObj)
                                             ),

    Subject = teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                          ,Macros
                                          ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?TEMPLATE_ID),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} -> teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end.

-spec hook_data(kzd_webhooks:doc()) -> kz_term:proplist().
hook_data(HookJObj) ->
    props:filter_undefined(
      [{<<"id">>, kz_doc:id(HookJObj)}
      ,{<<"name">>, kzd_webhooks:name(HookJObj)}
      ,{<<"uri">>, kzd_webhooks:uri(HookJObj)}
      ,{<<"event">>, kzd_webhooks:hook(HookJObj)}
      ,{<<"disable_reason">>, kzd_webhooks:disabled_message(HookJObj)}
      ]).
