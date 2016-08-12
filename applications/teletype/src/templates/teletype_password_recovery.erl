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

-define(TEMPLATE_ID, <<"password_recovery">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list([?MACRO_VALUE(<<"link">>, <<"link">>, <<"Password Reset Link">>, <<"Link going to click to reset password">>)
                           | ?ACCOUNT_MACROS ++ ?USER_MACROS
                          ])
       ).

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
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
                                          ,{'subject', ?TEMPLATE_SUBJECT}
                                          ,{'category', ?TEMPLATE_CATEGORY}
                                          ,{'friendly_name', ?TEMPLATE_NAME}
                                          ,{'to', ?TEMPLATE_TO}
                                          ,{'from', ?TEMPLATE_FROM}
                                          ,{'cc', ?TEMPLATE_CC}
                                          ,{'bcc', ?TEMPLATE_BCC}
                                          ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                          ]).

-spec handle_password_recovery(kz_json:object(), kz_proplist()) -> 'ok'.
handle_password_recovery(JObj, _Props) ->
    'true' = kapi_notifications:password_recovery_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec get_user(kzd_user:doc()) -> kz_proplist().
get_user(DataJObj) ->
    [{<<"password">>, kz_json:get_value(<<"password">>, DataJObj)}
     | teletype_util:user_params(DataJObj)
    ].

-spec build_macro_data(kz_json:object()) -> kz_proplist().
build_macro_data(DataJObj) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"user">>, get_user(DataJObj)}
    ,{<<"link">>, [kz_json:get_value(<<"password_reset_link">>, DataJObj)]}
    ].

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = build_macro_data(DataJObj),

    %% Populate templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,teletype_util:find_account_id(DataJObj)
                                             ),

    Subject = teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                          ,Macros
                                          ),

    Emails0 = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),
    Emails = props:set_value(<<"to">>, [kz_json:get_value(<<"email">>, DataJObj)], Emails0),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.
