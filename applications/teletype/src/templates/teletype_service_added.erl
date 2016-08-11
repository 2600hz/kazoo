%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(teletype_service_added).

-export([init/0
        ,handle_req/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"service_added">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?NOTIFY_CONFIG_CAT)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"user.id">>, <<"user_id">>, <<"User ID">>, <<"User ID">>)
          ,?MACRO_VALUE(<<"user.name">>, <<"user_name">>, <<"User Name">>, <<"User Name">>)
          ,?MACRO_VALUE(<<"user.realm">>, <<"user_realm">>, <<"User Realm">>, <<"User Realm">>)
           | ?ACCOUNT_MACROS
          ])
       ).

-define(TEMPLATE_TEXT, <<"Service addition notice for your sub-account {{user.name}} (ID #{{user.id}})\n\n{% if service %}New Services\n{% for srv_cat, srv_item in service %}{{ srv_cat }}:\n{% for item, quantity in srv_item %}    {{ item }}: {{ quantity }}\n{% endfor %}{% endfor %}\n{% endif %}\n\nAffected account\nAccount ID: {{user.id}}\nAccount Name: {{user.name}}\nAccount Realm: {{user.realm}}\n\nReseller account\nAccount ID: {{account.id}}\nAccount Name: {{account.name}}\nAccount Realm: {{account.realm}}\n\nSent from {{system.hostname}}">>).
-define(TEMPLATE_HTML, <<"<html><head><meta charset=\"utf-8\" /></head><body><h1>Service addition notice for your sub-account {{user.name}} (ID #{{user.id}})</h1><br/>{% if service %}<h2>New Services</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">{% for srv_cat, srv_item in service %}<tr><td colspan=\"2\">{{ srv_cat }}</td></tr>{% for item, quantity in srv_item %}<tr><td style=\"text-align: center;\">{{ item }}</td><td>{{ quantity }}</td></tr>{% endfor %}{% endfor %}</table>{% endif %}<h2>Affected account</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Account ID: </td><td>{{user.id}}</td></tr><tr><td>Account Name: </td><td>{{user.name}}</td></tr><tr><td>Account Realm: </td><td>{{user.realm}}</td></tr></table><h2>Reseller account</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Account ID: </td><td>{{account.id}}</td></tr><tr><td>Account Name: </td><td>{{account.name}}</td></tr><tr><td>Account Realm: </td><td>{{account.realm}}</td></tr></table><p style=\"font-size:9pt;color:#CCCCCC\">Sent from {{system.hostname}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"New service addition notice (sub-account ID #{{user.id}})">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"New Service Addition">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
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

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:service_added_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> io:format("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, reseller_info_data(DataJObj)}
             ,{<<"user">>, user_info_data(DataJObj)}
             ,{<<"service">>, service_added_data(DataJObj)}
             ],
    %% Load templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    AccountId = teletype_util:find_account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, AccountId),

    Subject = teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                                          ,Macros
                                          ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec reseller_info_data(kz_json:object()) -> kz_proplist().
reseller_info_data(DataJObj) ->
    Audit = kz_json:get_value(<<"audit_log">>, DataJObj),
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' ->
            AccountId = lists:last(kz_json:get_value(<<"tree">>, Audit)),
            ResellerId = kz_services:find_reseller_id(AccountId),
            {'ok', AccountJObj} = kz_account:fetch(ResellerId),
            [{<<"name">>, kz_account:name(AccountJObj)}
            ,{<<"id">>, ResellerId}
            ,{<<"realm">>, kz_account:realm(AccountJObj)}
            ,{<<"language">>, kz_account:language(AccountJObj)}
            ,{<<"timezone">>, kz_account:timezone(AccountJObj)}
            ]
    end.

-spec user_info_data(kz_json:object()) -> kz_proplist().
user_info_data(DataJObj) ->
    Audit = kz_json:get_value(<<"audit_log">>, DataJObj),
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' ->
            AccountId = kzd_audit_log:authenticating_user_account_id(Audit),
            {'ok', AccountJObj} = kz_account:fetch(AccountId),

            [{<<"name">>, kz_account:name(AccountJObj)}
            ,{<<"id">>, AccountId}
            ,{<<"realm">>, kz_account:realm(AccountJObj)}
            ,{<<"language">>, kz_account:language(AccountJObj)}
            ,{<<"timezone">>, kz_account:timezone(AccountJObj)}
            ]
    end.

-spec service_added_data(kz_json:object()) -> kz_proplist().
service_added_data(DataJObj) ->
    Audit = kz_json:get_value(<<"audit_log">>, DataJObj),
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' ->
            AccountId = lists:last(kz_json:get_value(<<"tree">>, Audit)),
            Diff = kz_json:get_value([<<"audit">>, AccountId, <<"diff_quantities">>], Audit),
            kz_json:recursive_to_proplist(Diff)
    end.
