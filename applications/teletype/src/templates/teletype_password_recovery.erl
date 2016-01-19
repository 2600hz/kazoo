%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz Inc
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
        ,wh_json:from_list([?MACRO_VALUE(<<"user.password">>, <<"user_password">>, <<"Password">>, <<"User's Password">>)
                            | ?ACCOUNT_MACROS ++ ?USER_MACROS
                           ])
       ).

-define(TEMPLATE_TEXT, <<"Hello, {{user.first_name}} {{user.last_name}}.\n\nThis email is to inform you that the password for your VoIP services account \"{{account.name}}\" has been set to \"{{user.password}}\".\n\nTo login please visit the website and use your normal username with the password \"{{user.password}}\".\n\nOnce you login you will be prompted to customize your password.">>).
-define(TEMPLATE_HTML, <<"<html></head><body><h3>Hello {{user.first_name}} {{user.last_name}}</h3><p>This email is to inform you that the password for your VoIP services account \"{{account.name}}\" has been set to \"{{user.password}}\".</p><p>To login please visit the website and use your normal username with the password \"{{user.password}}\".</p><p>Once you login you will be prompted to customize your password.</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Password reset for your VoIP services account.">>).
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
-spec process_req(wh_json:object(), wh_proplist()) -> 'ok'.
process_req(DataJObj) ->
    %% Load templates
    process_req(DataJObj, teletype_templates:fetch(?TEMPLATE_ID, DataJObj)).

process_req(_DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]);
process_req(DataJObj, Templates) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
             ],

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_meta(?TEMPLATE_ID
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
