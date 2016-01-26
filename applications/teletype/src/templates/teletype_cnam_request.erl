%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_cnam_request).

-export([init/0
         ,handle_cnam_request/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"cnam_request">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"request.number">>, <<"request_number">>, <<"Number">>, <<"Number to add CNAM">>)
            ,?MACRO_VALUE(<<"cnam.display_name">>, <<"cnam_display_name">>, <<"Display Name">>, <<"What to display">>)
            ,?MACRO_VALUE(<<"request.number_state">>, <<"request_number_state">>, <<"Number State">>, <<"Number State">>)
            ,?MACRO_VALUE(<<"request.local_number">>, <<"request_local_number">>, <<"Local Number">>, <<"Local Number">>)
            | ?ACCOUNT_MACROS ++ ?USER_MACROS
           ]
          )).

-define(TEMPLATE_TEXT, <<"Caller name update request for {{request.number}}\n\nRequest\nDisplay-Name: \"{{cnam.display_name}}\"\n\nNumber\nNumber: {{request.number}}\nState: {{request.number_state}}\nLocal-Number: {{request.local_number}}\n\nAccount\nAccount ID: {{account.id}}\nAccount Name: {{account.name}}\nAccount Realm: {{account.realm}}\n\n{% if admin %}Admin\nFirst Name: {{user.first_name}}\nLast Name: {{user.last_name}}\nEmail: {{user.email}}\nTimezone: {{user.timezone}}\n\n{% endif %}{% if devices %}SIP Credentials\n{% for device in devices %}User: {{device.user.first_name}} {{device.user.last_name}}\nEmail: {{device.user.email|default:\"\"}}\nSIP Username: {{device.sip.username}}\nSIP Password: {{device.sip.password}}\nSIP Realm: {{account.realm}}\n\n{% endfor %}{% endif %}{% if account.pvt_wnm_numbers %}Phone Numbers\n{% for number in account.pvt_wnm_numbers %}{{number}}\n{% endfor %}\n{% endif %}\n\nSent from {{system.hostname}}">>).

-define(TEMPLATE_HTML, <<"<html><head><meta charset=\"utf-8\" /></head><body><h3>Caller name update request for {{request.number}}</h3><h2>Request</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Display-Name: </td><td>\"{{cnam.display_name}}\"</td></tr></table><h2>Number</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Number: </td><td>{{request.number}}</td></tr><tr><td>State: </td><td>{{request.number_state}}</td></tr><tr><td>Local-Number: </td><td>{{request.local_number}}</td></tr></table><h2>Account</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Account ID: </td><td>{{account.id}}</td></tr><tr><td>Account Name: </td><td>{{account.name}}</td></tr><tr><td>Account Realm: </td><td>{{account.realm}}</td></tr></table>{% if admin %}<h2>Admin</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Name: </td><td>{{user.first_name}} {{user.last_name}}</td></tr><tr><td>Email: </td><td>{{user.email}}</td></tr><tr><td>Timezone: </td><td>{{user.timezone}}</td></tr></table>{% endif %}{% if devices %}<h2>SIP Credentials</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"1\"><tr><th>User</th><th>Email</th><th>SIP Username</th><th>SIP Password</th><th>SIP Realm</th></tr>{% for device in devices %}<tr><td>{{device.user.first_name}}{{device.user.last_name}}</td><td>{{device.user.email|default:\"\"}}</td><td>{{device.sip.username}}</td><td>{{device.sip.password}}</td><td>{{account.realm}}</td></tr>{% endfor %}</table>{% endif %}{% if account.pvt_wnm_numbers %}<h2>Phone Numbers</h2><ul>{% for number in account.pvt_wnm_numbers %}<li>{{number}}</li>{% endfor %}</ul>{% endif %}<p style=\"font-size:9pt;color:#CCCCCC\">Sent from {{system.hostname}}</p></body></html>">>).

-define(TEMPLATE_SUBJECT, <<"Caller name update request for {{request.number}}">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"CNAM Request">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
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

-spec handle_cnam_request(wh_json:object(), wh_proplist()) -> 'ok'.
handle_cnam_request(JObj, _Props) ->
    'true' = wapi_notifications:cnam_request_v(JObj),
    wh_util:put_callid(JObj),
    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),

    ReqData =
        wh_json:set_value(<<"user">>, teletype_util:find_account_admin(AccountId), DataJObj),
    CNAMJObj =
        wh_json:set_values([{<<"request">>, DataJObj}
                            ,{<<"cnam">>, cnam_data(DataJObj)}
                           ]
                           ,wh_json:merge_jobjs(DataJObj, ReqData)
                          ),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(CNAMJObj)
    end.

-spec cnam_data(wh_json:object()) -> api_object().
cnam_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'false' ->
            wh_json:get_json_value(<<"cnam">>, DataJObj);
        'true' ->
            wh_json:from_list([{<<"display_name">>, <<"Display Name">>}])
    end.

-spec process_req(wh_json:object()) -> 'ok'.
-spec process_req(wh_json:object(), wh_proplist()) -> 'ok'.
process_req(DataJObj) ->
    process_req(DataJObj, teletype_templates:fetch(?TEMPLATE_ID, DataJObj)).

process_req(_DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]);
process_req(DataJObj, Templates) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
              ,{<<"request">>, teletype_util:public_proplist(<<"request">>, DataJObj)}
              ,{<<"cnam">>, teletype_util:public_proplist(<<"cnam">>, DataJObj)}
             ],

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_meta(?TEMPLATE_ID
                                          ,teletype_util:find_account_id(DataJObj)
                                         ),

    Subject =
        teletype_util:render_subject(
            wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
            ,Macros
        ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' ->
            teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} ->
            teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.
