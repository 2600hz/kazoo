%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz Inc
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

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"new_account">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
	  [?MACRO_VALUE(<<"user.first_name">>, <<"first_name">>, <<"First Name">>, <<"First Name">>)
	  ,?MACRO_VALUE(<<"user.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last Name">>)
	  ,?MACRO_VALUE(<<"user.password">>, <<"password">>, <<"Password">>, <<"Password">>)
	   | ?ACCOUNT_MACROS
	  ])
       ).

-define(TEMPLATE_TEXT, <<"Thank you for registering!
Your account is ready to use, here are some details to help get you started!

			 Account ID: {{account.id}}
			 Account Name: {{account.name}}
			 Account Realm: {{account.realm}}

			 {% if admin %}Admin
			   First Name: {{admin.first_name}}
			   Last Name: {{admin.last_name}}
			   Email: {{admin.email}}
			   Timezone: {{admin.timezone}}

			   {% endif %}{% if devices %}SIP Credentials
			     {% for device in devices %}User: {{device.user.first_name}} {{device.user.last_name}}
			       Email: {{device.user.email|default:\"\"}}
SIP Username: {{device.sip.username}}
					SIP Password: {{device.sip.password}}
					SIP Realm: {{account.realm}}

					{% endfor %}{% endif %}

					  Sent from {{system.hostname}}">>).
-define(TEMPLATE_HTML, <<"<html><head><meta charset=\"utf-8\" /></head>
<body>
			 <h3>Thank you for registering!</h3>
			 <h2>Welcome</h2>
			 <p>Your account is ready to use, here are some details to help get you started!</p>
			 <h2>Account</h2>
			 <table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">
<tr><td>Account ID: </td><td>{{account.pvt_account_id}}</td></tr>
			 <tr><td>Account Name: </td><td>{{account.name}}</td></tr>
			 <tr><td>Account Realm: </td><td>{{account.realm}}</td></tr>
			 </table>
			 {% if admin %}
			   <h2>Admin</h2>
			       <table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">
<tr><td>Name: </td><td>{{admin.first_name}} {{admin.last_name}}</td></tr>
			       <tr><td>Email: </td><td>{{admin.email}}</td></tr>
			       <tr><td>Timezone: </td><td>{{admin.timezone}}</td></tr>
			       </table>
			       {% endif %}
				 {% if devices %}
				   <h2>SIP Credentials</h2>
				       <table cellpadding=\"4\" cellspacing=\"0\" border=\"1\">
<tr><th>User</th><th>Email</th><th>SIP Username</th><th>SIP Password</th><th>SIP Realm</th></tr>
				       {% for device in devices %}
					 <tr><td>{{device.user.first_name}}{{device.user.last_name}}</td><td>{{device.user.email|default:\"\"}}</td><td>{{device.sip.username}}</td><td>{{device.sip.password}}</td><td>{{account.realm}}</td></tr>
{% endfor %}
  </table>
      {% endif %}
      <p style='font-size:9pt;color:#CCCCCC'>Sent from {{system.hostname}}</p>
	  </body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Your new VoIP services Account">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"New Account">>).

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

-spec handle_new_account(kz_json:object(), kz_proplist()) -> 'ok'.
handle_new_account(JObj, _Props) ->
    'true' = kapi_notifications:new_account_v(JObj),

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
	     ,{<<"admin">>, admin_user_properties(DataJObj)}
             ],

    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,teletype_util:find_account_id(DataJObj)
                                             ),

    Subject =
        teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                    ,Macros
                                    ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec admin_user_properties(kz_json:object()) -> kz_proplist().
admin_user_properties(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    case kz_account:fetch(AccountId) of
        {'ok', JObj} -> account_admin_user_properties(JObj);
        {'error', _} -> []
    end.

-spec account_admin_user_properties(kz_json:object()) -> kz_proplist().
account_admin_user_properties(AccountJObj) ->
    AccountDb = kz_doc:account_db(AccountJObj),
    case kz_datamgr:get_all_results(AccountDb, <<"users/crossbar_listing">>) of
        {'error', _E} ->
            lager:debug("failed to get user listing from ~s: ~p", [AccountDb, _E]),
            [];
        {'ok', Users} ->
            find_admin(Users)
    end.

-spec find_admin(api_binaries()) -> kz_proplist().
find_admin([]) ->
    lager:debug("account has no admin users"),
    [];
find_admin([User|Users]) ->
    case kz_json:get_value([<<"value">>, <<"priv_level">>], User) of
        <<"admin">> -> admin_properties(kz_json:get_value(<<"value">>, User));
        _ -> find_admin(Users)
    end.

-spec admin_properties(kz_json:object()) -> kz_proplist().
admin_properties(User) ->
    Ks = [<<"first_name">>, <<"last_name">>, <<"email">>, <<"timezone">>],
    [{K, kz_json:get_value(K, User)} || K <- Ks].
