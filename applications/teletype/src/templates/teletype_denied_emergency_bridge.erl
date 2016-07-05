%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_denied_emergency_bridge).

-export([init/0
        ,handle_req/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"denied_emergency_bridge">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"call.outbound_caller_id_name">>, <<"outbound_caller_id_name">>, <<"Outbound Caller ID Name">>, <<"Outbound Caller ID Name">>)
          ,?MACRO_VALUE(<<"call.outbound_caller_id_number">>, <<"outbound_caller_id_number">>, <<"Outbound Caller ID Number">>, <<"Outbound Caller ID Number">>)
          ,?MACRO_VALUE(<<"call.emergency_caller_id_name">>, <<"emergency_caller_id_name">>, <<"Emergency Caller ID Name">>, <<"Emergency Caller ID Name">>)
          ,?MACRO_VALUE(<<"call.emergency_caller_id_number">>, <<"emergency_caller_id_number">>, <<"Emergency Caller ID Number">>, <<"Emergency Caller ID Number">>)
          ,?MACRO_VALUE(<<"call.call_id">>, <<"call_id">>, <<"Call ID">>, <<"Call ID">>)
           | ?ACCOUNT_MACROS
          ])
       ).

-define(TEMPLATE_TEXT, <<"Attention! The account '{{account.name}}' attempted a call ({{call.call_id}}) emergency services but the call was blocked because the address was not configured!  The emergency caller id was: {{call.emergency_caller_id_name}} {{call.emergency_caller_id_number}}  The external caller id was: {{call.outbound_caller_id_name}} {{call.outbound_caller_id_number}}">>).
-define(TEMPLATE_HTML, <<"<h1>Attention!</h1><p>The account '{{account.name}}' attempted a call ({{call.call_id}}) emergency services but the call was blocked because the address was not configured!</p><p>The emergency caller id was: {{call.emergency_caller_id_name}} {{call.emergency_caller_id_number}}</p><p>The external caller id was: {{call.outbound_caller_id_name}} {{call.outbound_caller_id_number}}</p>">>).
-define(TEMPLATE_SUBJECT, <<"Blocked emergency call from account {{account.name}}">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"Denied Emergency Bridge">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
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
    'true' = kapi_notifications:denied_emergency_bridge_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"call">>, kz_json:to_proplist(kz_api:remove_defaults(DataJObj))}
             ],

    %% Load templates
    Templates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

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
