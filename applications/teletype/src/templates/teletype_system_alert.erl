%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_system_alert).

-export([init/0
         ,handle_req/2
        ]).

-include("../teletype.hrl").

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".system_alert">>).

-define(TEMPLATE_ID, <<"system_alert">>).
-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"message">>, <<"message">>, <<"Message">>, <<"System message">>)
            | ?SERVICE_MACROS ++ ?ACCOUNT_MACROS ++ ?USER_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"Alert\n{{message}}\n\nProducer:\n{% for key, value in request %}{{ key }}: {{ value }}\n{% endfor %}\n{% if details %}Details\n{% for key, value in details %}{{ key }}: {{ value }}\n{% endfor %}\n{% endif %}{% if account %}Account\nAccount ID: {{account.id}}\nAccount Name: {{account.name}}\nAccount Realm: {{account.realm}}\n\n{% endif %}{% if user %}Admin\nName: {{user.first_name}} {{user.last_name}}\nEmail: {{user.email}}\nTimezone: {{user.timezone}}\n\n{% endif %}{% if account.pvt_wnm_numbers %}Phone Numbers\n{% for number in account.pvt_wnm_numbers %}{{number}}\n{% endfor %}\n{% endif %}Service\nURL: {{service.url}}\nName: {{service.name}}\nService Provider: {{service.provider}}\n\nSent from {{service.host}}">>).
-define(TEMPLATE_HTML, <<"<html><head><meta charset=\"utf-8\" /></head><body><h2>Alert</h2><p>{{message}}</p><h2>Producer</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">{% for key, value in request %}<tr><td>{{ key }}: </td><td>{{ value }}</td></tr>{% endfor %}</table>{% if details %}<h2>Details</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">{% for key, value in details %}<tr><td>{{ key }}: </td><td>{{ value }}</td></tr>{% endfor %}</table>{% endif %}{% if account %}<h2>Account</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Account ID: </td><td>{{account.id}}</td></tr><tr><td>Account Name: </td><td>{{account.name}}</td></tr><tr><td>Account Realm: </td><td>{{account.realm}}</td></tr></table>{% endif %}{% if admin %}<h2>Admin</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Name: </td><td>{{user.first_name}} {{user.last_name}}</td></tr><tr><td>Email: </td><td>{{user.email}}</td></tr><tr><td>Timezone: </td><td>{{user.timezone}}</td></tr></table>{% endif %}{% if account.pvt_wnm_numbers %}<h2>Phone Numbers</h2><ul>{% for number in account.pvt_wnm_numbers %}<li>{{number}}</li>{% endfor %}</ul>{% endif %}<h2>Service</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>URL: </td><td>{{service.url}}</td></tr><tr><td>Name: </td><td>{{service.name}}</td></tr><tr><td>Service Provider: </td><td>{{service.provider}}</td></tr></table><p style=\"font-size:9pt;color:#CCCCCC\">Sent from {{service.host}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"{{service.name}}: {{request.level}} from {{request.node}}">>).
-define(TEMPLATE_CATEGORY, <<"system">>).
-define(TEMPLATE_NAME, <<"System Notifications">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_util:init_template(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
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

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:system_alert_v(JObj),

    wh_util:put_callid(JObj),
    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    case teletype_util:should_handle_notification(DataJObj) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(DataJObj)
    end.

-spec handle_req(wh_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AccountJObj} = couch_mgr:open_cache_doc(AccountDb, AccountId),

    ReqData = wh_json:set_value(<<"account">>, AccountJObj, DataJObj),
    case wh_json:is_true(<<"preview">>, DataJObj, 'false') of
        'false' -> process_req(ReqData);
        'true' ->
            process_req(wh_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec process_req(wh_json:object()) -> 'ok'.
-spec process_req(wh_json:object(), wh_proplist()) -> 'ok'.
process_req(DataJObj) ->
    _ = teletype_util:send_update(DataJObj, <<"pending">>),
    %% Load templates
    process_req(DataJObj, teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj)).

process_req(_DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]);
process_req(DataJObj, Templates) ->
    ServiceData = teletype_util:service_params(DataJObj, ?MOD_CONFIG_CAT),

    Macros = [{<<"service">>, ServiceData}
              ,{<<"account">>, public_proplist(<<"account">>, DataJObj)}
              ,{<<"user">>, public_proplist(<<"user">>, DataJObj)}
              ,{<<"request">>, request_macros(DataJObj)}
              ,{<<"details">>, details_macros(DataJObj)}
              ,{<<"message">>, wh_json:get_value(<<"message">>, DataJObj)}
             ],

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} =
        teletype_util:fetch_template_meta(?TEMPLATE_ID
                                          ,teletype_util:find_account_id(DataJObj)
                                         ),

    Subject =
        teletype_util:render_subject(
            wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
            ,Macros
        ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    %% Send email
    case teletype_util:send_email(Emails
                                  ,Subject
                                  ,ServiceData
                                  ,RenderedTemplates
                                 )
    of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec details_macros(wh_json:object()) -> wh_proplist().
details_macros(DataJObj) ->
    wh_json:recursive_to_proplist(wh_json:get_value(<<"details">>, DataJObj)).

-spec request_macros(wh_json:object()) -> wh_proplist().
request_macros(DataJObj) ->
    wh_json:recursive_to_proplist(
      wh_json:delete_keys([<<"details">>
                           ,<<"app_version">>
                           ,<<"app_name">>
                           ,<<"event_name">>
                           ,<<"event_category">>
                           ,<<"server_id">>
                           ,<<"message">>
                           ,<<"subject">>
                           ,<<"account">>
                          ]
                          ,DataJObj)
     ).

-spec public_proplist(wh_json:key(), wh_json:object()) -> wh_proplist().
public_proplist(Key, JObj) ->
    wh_json:to_proplist(
        wh_json:public_fields(
            wh_json:get_value(Key, JObj, wh_json:new())
        )
    ).
