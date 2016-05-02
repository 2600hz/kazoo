%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_deregister).

-export([init/0
         ,handle_deregister/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"deregister">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,kz_json:from_list(
           [?MACRO_VALUE(<<"last_registration.username">>, <<"last_registration_username">>, <<"SIP Username">>, <<"SIP username">>)
            ,?MACRO_VALUE(<<"last_registration.status">>, <<"last_registration_status">>, <<"Status">>, <<"Status">>)
            ,?MACRO_VALUE(<<"last_registration.user_agent">>, <<"last_registration_user_agent">>, <<"SIP User Agent">>, <<"SIP User Agent">>)
            ,?MACRO_VALUE(<<"last_registration.call_id">>, <<"last_registration_call_id">>, <<"SIP Call ID">>, <<"SIP Call ID">>)
            ,?MACRO_VALUE(<<"last_registration.profile_name">>, <<"last_registration_profile_name">>, <<"Profile Name">>, <<"Profile Name">>)
            ,?MACRO_VALUE(<<"last_registration.presence_hosts">>, <<"last_registration_presence_hosts">>, <<"Presence Hosts">>, <<"Presence Hosts">>)
            ,?MACRO_VALUE(<<"last_registration.from_user">>, <<"last_registration_from_user">>, <<"SIP From User">>, <<"SIP From User">>)
            ,?MACRO_VALUE(<<"last_registration.from_host">>, <<"last_registration_from_host">>, <<"SIP From Host">>, <<"SIP From Host">>)
            ,?MACRO_VALUE(<<"last_registration.to_user">>, <<"last_registration_to_user">>, <<"SIP To User">>, <<"SIP To User">>)
            ,?MACRO_VALUE(<<"last_registration.to_host">>, <<"last_registration_to_host">>, <<"SIP To Host">>, <<"SIP To Host">>)
            ,?MACRO_VALUE(<<"last_registration.rpid">>, <<"last_registration_rpid">>, <<"SIP RPID">>, <<"SIP RPID">>)
            ,?MACRO_VALUE(<<"last_registration.network_ip">>, <<"last_registration_network_ip">>, <<"Network IP">>, <<"Network IP">>)
            ,?MACRO_VALUE(<<"last_registration.network_port">>, <<"last_registration_network_port">>, <<"Network Port">>, <<"Network Port">>)
            ,?MACRO_VALUE(<<"last_registration.contact">>, <<"last_registration_contact">>, <<"SIP Contact">>, <<"SIP Contact">>)
            ,?MACRO_VALUE(<<"last_registration.expires">>, <<"last_registration_expires">>, <<"Expires">>, <<"Expires">>)
            ,?MACRO_VALUE(<<"last_registration.authorizing_id">>, <<"last_registration_authorizing_id">>, <<"Authorizing ID">>, <<"Authorizing ID">>)
            | ?ACCOUNT_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"Expired registration in account \"{{account.name}}\".\nNotifications are enabled for loss of registration on the device {{last_registration.username}}@{{account.realm}}\n\nLast Registration:\nDevice ID: {{last_registration.authorizing_id}}\nAccount ID: {{account.id}}\nUser Agent: {{last_registration.user_agent}}\nContact: {{last_registration.contact}}\n\nThis may be due to a network connectivity issue, power outage, or misconfiguration. Please check the device.">>).
-define(TEMPLATE_HTML, <<"<html><body><h2>Expired registration in account \"{{account.name}}\"</h2><p>Notifications are enabled for loss of registration on the device {{last_registration.username}}@{{account.realm}}</p><h3>Last Registration</h3><table><tr><td>Device ID</td><td>{{last_registration.authorizing_id}}</td></tr><tr><td>Account ID</td><td>{{account.id}}</td></tr><tr><td>User Agent</td><td>{{last_registration.user_agent}}</td></tr><tr><td>Contact</td><td>{{last_registration.contact}}</td></tr></table><p>This may be due to a network connectivity issue, power outage, or misconfiguration. Please check the device.</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Loss of Registration for {{last_registration.username}}@{{account.realm}}">>).
-define(TEMPLATE_CATEGORY, <<"registration">>).
-define(TEMPLATE_NAME, <<"Deregister Notice">>).

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

-spec handle_deregister(kz_json:object(), kz_proplist()) -> 'ok'.
handle_deregister(JObj, _Props) ->
    'true' = kapi_notifications:deregister_v(JObj),
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
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              ,{<<"last_registration">>, kz_json:to_proplist(DataJObj)}
             ],

    %% Load templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, kz_json:get_value(<<"account_id">>, DataJObj)),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                ,Macros
               ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.
