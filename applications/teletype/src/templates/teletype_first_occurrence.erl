%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(teletype_first_occurrence).

-export([init/0
        ,first_occurrence/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"first_occurrence">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"event">>, <<"event">>, <<"Event">>, <<"Event">>)
           | ?USER_MACROS ++ ?ACCOUNT_MACROS ++ ?SYSTEM_MACROS
          ])
       ).

-define(TEMPLATE_SUBJECT, <<"First {{event}} on {{account.name}}">>).
-define(TEMPLATE_CATEGORY, <<"sip">>).
-define(TEMPLATE_NAME, <<"First Occurrence">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
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

-spec first_occurrence(kz_json:object(), kz_proplist()) -> 'ok'.
first_occurrence(JObj, _Props) ->
    'true' = kapi_notifications:first_occurrence_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(DataJObj)
    end.

-spec build_macro_data(kz_json:object()) -> kz_proplist().
build_macro_data(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"user">>, teletype_util:find_account_admin(AccountId)}
    ,{<<"event">>, kz_json:get_value(<<"occurrence">>, DataJObj)}
    ].

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    Macros = build_macro_data(DataJObj),

    %% Load templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, teletype_util:find_account_id(DataJObj)),

    Subject = teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                                          ,Macros
                                          ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.
