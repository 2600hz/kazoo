%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_new_account).

-export([init/0
         ,handle_new_account/2
        ]).

-include("../teletype.hrl").

-define(TEMPLATE_ID, <<"new_account">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"user.first_name">>, <<"first_name">>, <<"First Name">>, <<"First Name">>)
            ,?MACRO_VALUE(<<"user.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last Name">>)
            ,?MACRO_VALUE(<<"user.password">>, <<"password">>, <<"Password">>, <<"Password">>)
            | ?ACCOUNT_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"Thank you for registering!\n\nYour new 2600hz VoIP Services has been setup!\nTo login please visit https://apps.2600hz.com/.\nFor help or questions using your phone or voicemail, please contact (415) 886-7900 or email support@2600hz.com\n\nPorting Numbers\nHere is info on how to port numbers to your new account.\n\nAccount\nAccount ID: {{account.id}}\nAccount Name: {{account.name}}\nAccount Realm: {{account.realm}}\n\n{% if admin %}Admin\nFirst Name: {{admin.first_name}}\nLast Name: {{admin.last_name}}\nEmail: {{admin.email}}\nTimezone: {{admin.timezone}}\n\n{% endif %}{% if devices %}SIP Credentials\n{% for device in devices %}User: {{device.user.first_name}} {{device.user.last_name}}\nEmail: {{device.user.email|default:\"\"}}\nSIP Username: {{device.sip.username}}\nSIP Password: {{device.sip.password}}\nSIP Realm: {{account.realm}}\n\n{% endfor %}{% endif %}{% if account.pvt_wnm_numbers %}Phone Numbers\n{% for number in account.pvt_wnm_numbers %}{{number}}\n{% endfor %}Please note that it could take up to 1 hour for your number to become active.\n\n{% endif %}Service\nURL: https://apps.2600hz.com/\nName: VoIP Services\nProvider: 2600hz\n\nSent from {{system.hostname}}">>).
-define(TEMPLATE_HTML, <<"<html><head><meta charset=\"utf-8\" /></head><body><h3>Thank you for registering!</h3><h2>Welcome</h2><p>Your new 2600hz VoIP Services has been setup!</p><p>To login please visit <a href=\"https://apps.2600hz.com/\">2600hz.com</a></p><p>For help or questions using your phone or voicemail, please contact (415) 886-7900 or email <a href=\"mailto:support@2600hz.com\">support@2600hz.com</a></p><h2>Porting Numbers</h2><p>Here is info on how to port numbers to your new account.</p><h2>Account</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Account ID: </td><td>{{account.pvt_account_id}}</td></tr><tr><td>Account Name: </td><td>{{account.name}}</td></tr><tr><td>Account Realm: </td><td>{{account.realm}}</td></tr></table>{% if admin %}<h2>Admin</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Name: </td><td>{{admin.first_name}} {{admin.last_name}}</td></tr><tr><td>Email: </td><td>{{admin.email}}</td></tr><tr><td>Timezone: </td><td>{{admin.timezone}}</td></tr></table>{% endif %}{% if devices %}<h2>SIP Credentials</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"1\"><tr><th>User</th><th>Email</th><th>SIP Username</th><th>SIP Password</th><th>SIP Realm</th></tr>{% for device in devices %}<tr><td>{{device.user.first_name}}{{device.user.last_name}}</td><td>{{device.user.email|default:\"\"}}</td><td>{{device.sip.username}}</td><td>{{device.sip.password}}</td><td>{{account.realm}}</td></tr>{% endfor %}</table>{% endif %}{% if account.pvt_wnm_numbers %}<h2>Phone Numbers</h2><ul>{% for number in account.pvt_wnm_numbers %}<li>{{number}}</li>{% endfor %}</ul><p>Please note that it could take up to 1 hour for your number to become active.</p>{% endif %}<h2>Service</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>URL: </td><td>https://apps.2600hz.com/</td></tr><tr><td>Name: </td><td>VoIP Services</td></tr><tr><td>Service Provider: </td><td>2600hz</td></tr></table><p style=\"font-size:9pt;color:#CCCCCC\">Sent from {{system.hostname}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Your new 2600hz VoIP Services Account">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"New Account">>).

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

-spec handle_new_account(wh_json:object(), wh_proplist()) -> 'ok'.
handle_new_account(JObj, _Props) ->
    'true' = wapi_notifications:new_account_v(JObj),

    wh_util:put_callid(JObj),
    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(wh_json:object()) -> 'ok'.
-spec process_req(wh_json:object(), wh_proplist()) -> 'ok'.
process_req(DataJObj) ->
    %% Load templates
    process_req(DataJObj, teletype_templates:fetch(?TEMPLATE_ID, DataJObj)).

process_req(_DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]);
process_req(DataJObj, Templates) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              ,{<<"admin">>, admin_user_properties(DataJObj)}
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
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

admin_user_properties(DataJObj) ->
    AccountJObj = wh_json:get_value(<<"account">>, DataJObj),
    account_admin_user_properties(AccountJObj).

account_admin_user_properties(AccountJObj) ->
    AccountDb = wh_doc:account_db(AccountJObj),
    case couch_mgr:get_all_results(AccountDb, <<"users/crossbar_listing">>) of
        {'error', _E} ->
            lager:debug("failed to get user listing from ~s: ~p", [AccountDb, _E]),
            [];
        {'ok', Users} ->
            find_admin(Users)
    end.

find_admin([]) ->
    lager:debug("account has no admin users"),
    [];
find_admin([User|Users]) ->
    case wh_json:get_value([<<"value">>, <<"priv_level">>], User) of
        <<"admin">> -> admin_properties(wh_json:get_value(<<"value">>, User));
        _ -> find_admin(Users)
    end.

admin_properties(User) ->
    Ks = [<<"first_name">>, <<"last_name">>, <<"email">>, <<"timezone">>],
    [{K, wh_json:get_value(K, User)} || K <- Ks].
