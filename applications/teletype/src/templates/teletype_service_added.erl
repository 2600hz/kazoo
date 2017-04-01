%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(teletype_service_added).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"service_added">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?NOTIFY_CONFIG_CAT)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"sub_account.id">>, <<"sub_account_id">>, <<"Sub-Account ID">>, <<"Sub-Account ID">>)
          ,?MACRO_VALUE(<<"sub_account.name">>, <<"sub_account_name">>, <<"Sub-Account Name">>, <<"Sub-Account Name">>)
          ,?MACRO_VALUE(<<"sub_account.realm">>, <<"sub_account_realm">>, <<"Sub-Account Realm">>, <<"Sub-Account Realm">>)
          ,?MACRO_VALUE(<<"sub_account.language">>, <<"sub_account_language">>, <<"Sub-Account Language">>, <<"Sub-Account Language">>)
          ,?MACRO_VALUE(<<"sub_account.timezone">>, <<"sub_account_timezone">>, <<"Sub-Account Timezone">>, <<"Sub-Account Timezone">>)
          ,?MACRO_VALUE(<<"service_changes">>, <<"service_changes">>, <<"Sub-Account Service Changes object">>, <<"Sub-Account Service Changes object">>)
           | ?ACCOUNT_MACROS ++ ?USER_MACROS
          ])
       ).

-define(TEMPLATE_SUBJECT, <<"New VoIP services were added to sub-account '{{sub_account.name}}'">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"New Service Addition">>).

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
                                          ]),
    teletype_bindings:bind(<<"service_added">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(JObj) ->
    'true' = kapi_notifications:service_added_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    ReqData =
        kz_json:set_value(<<"user">>, teletype_util:find_account_admin(AccountId), DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("~s notification handling not configured for account ~s"
                              ,[?TEMPLATE_ID, AccountId]
                              );
        'true' -> process_req(kz_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, reseller_info_data(DataJObj)}
             ,{<<"sub_account">>, sub_account_data(DataJObj)}
             ,{<<"service_changes">>, service_added_data(DataJObj)}
             ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
             ],
    %% Load templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    AccountId = teletype_util:find_account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, AccountId),

    Subject = teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                                          ,Macros
                                          ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec reseller_info_data(kz_json:object()) -> kz_proplist().
reseller_info_data(DataJObj) ->
    Audit = kz_json:get_value(<<"audit_log">>, DataJObj),
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' ->
            AccountId = lists:last(kz_json:get_value(<<"tree">>, Audit)),
            ResellerId = kz_services:find_reseller_id(AccountId),
            teletype_util:find_account_params(DataJObj, ResellerId)
    end.

-spec sub_account_data(kz_json:object()) -> kz_proplist().
sub_account_data(DataJObj) ->
    Audit = kz_json:get_value(<<"audit_log">>, DataJObj),
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' ->
            AccountId = kzd_audit_log:authenticating_user_account_id(Audit),
            teletype_util:find_account_params(DataJObj, AccountId)
    end.

-spec service_added_data(kz_json:object()) -> kz_proplist().
service_added_data(DataJObj) ->
    Audit = kz_json:get_value(<<"audit_log">>, DataJObj),
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' ->
            AccountId = lists:last(kz_json:get_value(<<"tree">>, Audit)),
            Diff = kz_json:get_value([<<"audit">>, AccountId, <<"diff_quantities">>], Audit),
            kz_json:recursive_to_proplist(Diff)
    end.
