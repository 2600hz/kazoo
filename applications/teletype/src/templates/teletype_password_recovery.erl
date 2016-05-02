%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_password_recovery).

-export([init/0
         ,handle_password_recovery/2
        ]).

-include("teletype.hrl").

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".password_recovery">>).

-define(TEMPLATE_ID, <<"password_recovery">>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list([?MACRO_VALUE(<<"link">>, <<"link">>, <<"Password Reset Link">>, <<"Link going to click to reset password">>)
                           | ?ACCOUNT_MACROS ++ ?USER_MACROS
                           ])
       ).

-define(TEMPLATE_TEXT, <<"Hello, {{user.first_name}} {{user.last_name}}!\n\nWe received a request to change the password for your 2600hz VoIP Services account \"{{account.name}}\".\nIf you did not make this request, just ignore this email. Otherwise, please click the link below to change your password:\n\n{{link}}">>).
-define(TEMPLATE_HTML, <<"<html></head><body><h3>Hello, {{user.first_name}} {{user.last_name}}!</h3><p>We received a request to change the password of your 2600hz VoIP Services account \"{{account.name}}\".</p><p>If you did not make this request, just ignore this email. Otherwise, please click the link below to change your password:</p><p><a href=\"{{link}}\">{{link}}</a></p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Reset your VoIP services account password.">>).
-define(TEMPLATE_CATEGORY, <<"user">>).
-define(TEMPLATE_NAME, <<"Password Recovery">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
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

-spec handle_password_recovery(wh_json:object(), wh_proplist()) -> 'ok'.
handle_password_recovery(JObj, _Props) ->
    'true' = wapi_notifications:pwd_recovery_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' ->
            lager:debug("notification not enabled for account ~s: prefers ~s"
                        ,[AccountId
                          ,kz_account:notification_preference(DataJObj)
                         ]);
        'true' ->
            lager:debug("notification enabled for account ~s", [AccountId]),

            User = get_user(DataJObj),
            ReqData =
                wh_json:set_values(
                    [{<<"user">>, User}
                     ,{<<"to">>, [wh_json:get_ne_value(<<"email">>, User)]}
                     ,{<<"link">>, wh_json:get_ne_value([<<"Password-Reset-Link">>], DataJObj, <<"missing_link">>)}
                    ]
                  ,DataJObj
                 ),
            process_req(wh_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec get_user(wh_json:object()) -> wh_json:object().
get_user(DataJObj) ->
    Ks = [<<"first_name">>, <<"last_name">>, <<"email">>, <<"password">>],
    lists:foldl(fun(K, Acc) -> get_user_fold(K, Acc, DataJObj) end
                ,wh_json:new()
                ,Ks
               ).

-spec get_user_fold(wh_json:key(), wh_json:object(), wh_json:object()) -> wh_json:object().
get_user_fold(K, Acc, DataJObj) ->
    wh_json:set_value(K, wh_json:get_value(K, DataJObj), Acc).

-spec process_req(wh_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
              ,{<<"link">>, wh_json:get_value([<<"link">>], DataJObj)}
             ],

    %% Populate templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,teletype_util:find_account_id(DataJObj)
                                             ),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                ,Macros
               ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.
