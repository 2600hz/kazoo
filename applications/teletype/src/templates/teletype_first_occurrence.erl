%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(teletype_first_occurrence).

-export([init/0
        ,first_occurrence/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"first_occurrence">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"event">>, <<"event">>, <<"Event">>, <<"Event">>)
           | ?USER_MACROS ++ ?ACCOUNT_MACROS ++ ?SYSTEM_MACROS
          ])
       ).

-define(TEMPLATE_TEXT, <<"The first {{event}} was detected on an account.\n\nAccount\nAccount ID: {{account.id}}\nAccount Name: {{account.name}}\nAccount Realm: {{account.realm}}\n\n{% if admin %}Admin\nName: {{admin.first_name}} {{admin.last_name}}\nEmail: {{admin.email}}\nTimezone: {{admin.timezone}}\n\n{% endif %}\n\nSent from {{system.host}}">>).
-define(TEMPLATE_HTML, <<"<html><body><h2>The first {{event}} was detected on an account.</h2><h2>Account</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Account ID: </td><td>{{account.id}}</td></tr><tr><td>Account Name: </td><td>{{account.name}}</td></tr><tr><td>Account Realm: </td><td>{{account.realm}}</td></tr></table>{% if admin %}<h2>Admin</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Name: </td><td>{{admin.first_name}} {{admin.last_name}}</td></tr><tr><td>Email: </td><td>{{admin.email}}</td></tr><tr><td>Timezone: </td><td>{{admin.timezone}}</td></tr></table>{% endif %}<p style=\"font-size:9pt;color:#CCCCCC\">Sent from {{system.host}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"First {{event}} on {{account.name}}">>).
-define(TEMPLATE_CATEGORY, <<"sip">>).
-define(TEMPLATE_NAME, <<"First Occurrence">>).

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

-spec first_occurrence(kz_json:object(), kz_proplist()) -> 'ok'.
first_occurrence(JObj, _Props) ->
    'true' = kapi_notifications:first_occurrence_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(DataJObj)
    end.

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"user">>, teletype_util:find_account_admin(AccountId)}
             ,{<<"event">>, kz_json:get_value(<<"occurrence">>, DataJObj)}
             ],

    %% Load templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, teletype_util:find_account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                ,Macros
               ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.
