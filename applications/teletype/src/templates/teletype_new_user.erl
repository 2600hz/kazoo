%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_new_user).

-export([init/0
	,handle_req/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"new_user">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
	  [?MACRO_VALUE(<<"user.password">>, <<"password">>, <<"Password">>, <<"Password">>)
	   | ?USER_MACROS ++ ?ACCOUNT_MACROS
	  ])
       ).

-define(TEMPLATE_TEXT, <<"Welcome {{user.first_name}} {{user.last_name}}.\n\nYour password is: {{user.password}}\n Please make sure your change it soon!">>).
-define(TEMPLATE_HTML, <<"<p>Welcome {{user.first_name}} {{user.last_name}}.</p><p>Your password is: {{user.password}}</p><p>Please make sure you change it soon!</p>">>).
-define(TEMPLATE_SUBJECT, <<"New user">>).
-define(TEMPLATE_CATEGORY, <<"user">>).
-define(TEMPLATE_NAME, <<"New User">>).

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
    'true' = kapi_notifications:new_user_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> do_handle_req(DataJObj)
    end.

-spec do_handle_req(kz_json:object()) -> 'ok'.
do_handle_req(DataJObj) ->
    UserId = kz_json:get_value(<<"user_id">>, DataJObj),
    {'ok', UserJObj} = teletype_util:open_doc(<<"user">>, UserId, DataJObj),
    Password = kz_json:get_value(<<"password">>, DataJObj),

    ReqData =
        kz_json:set_values(
          [{<<"user">>, kz_json:set_value(<<"password">>, Password, UserJObj)}
	  ,{<<"to">>, [kz_json:get_ne_value(<<"email">>, UserJObj)]}
          ]
			  ,DataJObj
         ),

    case teletype_util:is_preview(DataJObj) of
        'false' -> process_req(ReqData);
        'true' -> process_req(kz_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
	     ,{<<"account">>, teletype_util:account_params(DataJObj)}
	     ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
             ],

    %% Populate templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,teletype_util:find_account_id(DataJObj)
                                             ),

    Subject =
        teletype_util:render_subject(
          kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
				    ,Macros
         ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' ->
            teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} ->
            teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.
