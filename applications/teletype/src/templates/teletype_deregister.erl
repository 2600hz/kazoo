%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_deregister).

-export([init/0
        ,handle_deregister/1
        ]).

-ifdef(TEST).
-export([macros/1]).
-endif.

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
           | ?COMMON_TEMPLATE_MACROS
          ])).

-define(TEMPLATE_SUBJECT, <<"Loss of Registration for '{{last_registration.username}}'">>).

-define(TEMPLATE_PARAMS, [{'macros', ?TEMPLATE_MACROS}
                         ,{'subject', ?TEMPLATE_SUBJECT}
                         ,{'category', <<"registration">>}
                         ,{'friendly_name', <<"Deregister Notice">>}
                         ,{'to', ?CONFIGURED_EMAILS(?EMAIL_ADMINS)}
                         ,{'from', teletype_util:default_from_address(?MOD_CONFIG_CAT)}
                         ,{'cc', ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])}
                         ,{'bcc', ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])}
                         ,{'reply_to', teletype_util:default_reply_to(?MOD_CONFIG_CAT)}
                         ]).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, ?TEMPLATE_PARAMS),
    teletype_bindings:bind(?TEMPLATE_ID, ?MODULE, 'handle_deregister').

-spec handle_deregister(kz_json:object()) -> 'ok'.
handle_deregister(JObj) ->
    'true' = kapi_notifications:deregister_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> teletype_util:notification_disabled(DataJObj, ?TEMPLATE_ID);
        'true' -> handle_req(DataJObj)
    end.

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    Macros = macros(DataJObj),

    %% Load templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    AccountId = teletype_util:find_account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, AccountId),
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec macros(kz_json:object()) -> kz_proplist().
-spec macros(kz_json:object(), kz_proplist()) -> kz_proplist().
macros(DataJObj) ->
    macros(DataJObj, teletype_util:account_params(DataJObj)).

macros(DataJObj, []) ->
    lager:info("no account data available for deregister, not sending notification"),
    teletype_util:send_update(DataJObj, <<"failed">>, <<"missing account">>),
    exit('normal');
macros(DataJObj, AccountParams) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, AccountParams}
    ,{<<"last_registration">>, kz_json:to_proplist(DataJObj)}
    ].
