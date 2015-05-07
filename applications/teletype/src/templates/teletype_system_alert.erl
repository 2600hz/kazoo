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
         ,handle_system_alert/2
        ]).

-include("../teletype.hrl").

-define(TEMPLATE_ID, <<"system_alert">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"message">>, <<"message">>, <<"Message">>, <<"System message">>)
            | ?ACCOUNT_MACROS ++ ?USER_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"Alert\n{{message}}\n\nProducer:\n{% for key, value in request %}{{ key }}: {{ value }}\n{% endfor %}\n{% if details %}Details\n{% for key, value in details %}{{ key }}: {{ value }}\n{% endfor %}\n{% endif %}{% if account %}Account\nAccount ID: {{account.id}}\nAccount Name: {{account.name}}\nAccount Realm: {{account.realm}}\n\n{% endif %}{% if user %}Admin\nName: {{user.first_name}} {{user.last_name}}\nEmail: {{user.email}}\nTimezone: {{user.timezone}}\n\n{% endif %}{% if account.pvt_wnm_numbers %}Phone Numbers\n{% for number in account.pvt_wnm_numbers %}{{number}}\n{% endfor %}\n{% endif %}Service\nURL: https://apps.2600hz.com/\nName: VoIP Services\nService Provider: 2600hz\n\nSent from {{system.hostname}}">>).
-define(TEMPLATE_HTML, <<"<html><head><meta charset=\"utf-8\" /></head><body><h2>Alert</h2><p>{{message}}</p><h2>Producer</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">{% for key, value in request %}<tr><td>{{ key }}: </td><td>{{ value }}</td></tr>{% endfor %}</table>{% if details %}<h2>Details</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">{% for key, value in details %}<tr><td>{{ key }}: </td><td>{{ value }}</td></tr>{% endfor %}</table>{% endif %}{% if account %}<h2>Account</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Account ID: </td><td>{{account.id}}</td></tr><tr><td>Account Name: </td><td>{{account.name}}</td></tr><tr><td>Account Realm: </td><td>{{account.realm}}</td></tr></table>{% endif %}{% if user %}<h2>Admin</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Name: </td><td>{{user.first_name}} {{user.last_name}}</td></tr><tr><td>Email: </td><td>{{user.email}}</td></tr><tr><td>Timezone: </td><td>{{user.timezone}}</td></tr></table>{% endif %}{% if account.pvt_wnm_numbers %}<h2>Phone Numbers</h2><ul>{% for number in account.pvt_wnm_numbers %}<li>{{number}}</li>{% endfor %}</ul>{% endif %}<h2>Service</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>URL: </td><td>https://apps.2600hz.com/</td></tr><tr><td>Name: </td><td>VoIP Services</td></tr><tr><td>Service Provider: </td><td>2600hz</td></tr></table><p style=\"font-size:9pt;color:#CCCCCC\">Sent from {{system.hostname}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"VoIP Services: {{request.level}} from {{request.node}}">>).
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

-spec handle_system_alert(wh_json:object(), wh_proplist()) -> 'ok'.
handle_system_alert(JObj, _Props) ->
    'true' = wapi_notifications:system_alert_v(JObj),

    wh_util:put_callid(JObj),

    case wh_json:get_value([<<"Details">>, <<"Format">>], JObj) of
        'undefined' -> handle_req_as_email(JObj, 'true');
        _Format ->
            lager:debug("using format string '~s'", [_Format]),
            UseEmail = whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enable_email_alerts">>, 'true'),
            Url = whapps_config:get_string(?MOD_CONFIG_CAT, <<"subscriber_url">>),
            handle_req_as_email(JObj, UseEmail),
            handle_req_as_http(JObj, Url, UseEmail)
    end.

-spec handle_req_as_http(wh_json:object(), api_binary(), boolean()) -> 'ok'.
handle_req_as_http(JObj, 'undefined', UseEmail) ->
    handle_req_as_email(JObj, UseEmail);
handle_req_as_http(JObj, Url, UseEmail) ->
    Headers = [{"Content-Type", "application/json"}],
    Encoded = wh_json:encode(JObj),

    case ibrowse:send_req(wh_util:to_list(Url), Headers, 'post', Encoded) of
        {'ok', "2" ++ _, _ResponseHeaders, _ResponseBody} ->
            lager:debug("JSON data successfully POSTed to '~s'", [Url]);
        _Error ->
            lager:debug("failed to POST JSON data to ~p for reason: ~p", [Url,_Error]),
            handle_req_as_email(JObj, UseEmail)
    end.

-spec handle_req_as_email(wh_json:object(), boolean() | wh_json:object()) -> 'ok'.
handle_req_as_email(_JObj, 'false') ->
    lager:debug("email not enabled for system alerts");
handle_req_as_email(JObj, 'true') ->
    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    AccountId = find_account_id(DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID)
    of
        'false' -> lager:debug("notification handling not configured for this account ~s", [AccountId]);
        'true' -> process_req(wh_json:set_value(<<"account_id">>, AccountId, DataJObj))
    end.

-spec find_account_id(wh_json:object()) -> ne_binary().
find_account_id(DataJObj) ->
    case wh_json:get_value(<<"account_id">>, DataJObj) of
        'undefined' ->
            {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
            MasterAccountId;
        AccountId -> AccountId
    end.

-spec process_req(wh_json:object()) -> 'ok'.
-spec process_req(wh_json:object(), wh_proplist()) -> 'ok'.
process_req(DataJObj) ->
    lager:debug("template is enabled for account, fetching templates for rendering"),
    %% Load templates
    process_req(DataJObj, teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj)).

process_req(DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]),
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    process_req(wh_json:set_value(<<"account_id">>, MasterAccountId, DataJObj));
process_req(DataJObj, Templates) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
              ,{<<"request">>, request_macros(DataJObj)}
              ,{<<"details">>, details_macros(DataJObj)}
              ,{<<"message">>, wh_json:get_value(<<"message">>, DataJObj, <<>>)}
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

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec details_macros(wh_json:object()) -> wh_proplist().
details_macros(DataJObj) ->
    case wh_json:get_value(<<"details">>, DataJObj) of
        'undefined' -> [];
        <<_/binary>> = Details -> [{<<"message">>, Details}];
        Details when is_list(Details) -> Details;
        Details -> wh_json:recursive_to_proplist(Details)
    end.

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
                           ,<<"preview">>
                           ,<<"text">>
                           ,<<"html">>
                           ,<<"from">>
                           ,<<"bcc">>
                           ,<<"cc">>
                           ,<<"to">>
                           ,<<"reply_to">>
                           ,<<"format">>
                          ]
                          ,DataJObj
                         )
     ).
