%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(teletype_service_added).
-behaviour(teletype_gen_email_template).

-export([id/0
        ,init/0
        ,macros/0, macros/1
        ,subject/0
        ,category/0
        ,friendly_name/0
        ,to/1, from/1, cc/1, bcc/1, reply_to/1
        ]).
-export([handle_req/1]).

-include("teletype.hrl").

-spec id() -> ne_binary().
id() -> <<"service_added">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"sub_account.id">>, <<"sub_account_id">>, <<"Sub-Account ID">>, <<"Sub-Account ID">>)
      ,?MACRO_VALUE(<<"sub_account.name">>, <<"sub_account_name">>, <<"Sub-Account Name">>, <<"Sub-Account Name">>)
      ,?MACRO_VALUE(<<"sub_account.realm">>, <<"sub_account_realm">>, <<"Sub-Account Realm">>, <<"Sub-Account Realm">>)
      ,?MACRO_VALUE(<<"sub_account.language">>, <<"sub_account_language">>, <<"Sub-Account Language">>, <<"Sub-Account Language">>)
      ,?MACRO_VALUE(<<"sub_account.timezone">>, <<"sub_account_timezone">>, <<"Sub-Account Timezone">>, <<"Sub-Account Timezone">>)
      ,?MACRO_VALUE(<<"service_changes">>, <<"service_changes">>, <<"Sub-Account Service Changes object">>, <<"Sub-Account Service Changes object">>)
       | ?USER_MACROS
       ++ ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> ne_binary().
subject() -> <<"New VoIP services were added to sub-account '{{sub_account.name}}'">>.

-spec category() -> ne_binary().
category() -> <<"account">>.

-spec friendly_name() -> ne_binary().
friendly_name() -> <<"New Service Addition">>.

-spec to(ne_binary()) -> kz_json:object().
to(_) -> ?CONFIGURED_EMAILS(?EMAIL_ADMINS).

-spec from(ne_binary()) -> api_ne_binary().
from(ModConfigCat) -> teletype_util:default_from_address(ModConfigCat).

-spec cc(ne_binary()) -> kz_json:object().
cc(_) -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec bcc(ne_binary()) -> kz_json:object().
bcc(_) -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec reply_to(ne_binary()) -> api_ne_binary().
reply_to(ModConfigCat) -> teletype_util:default_reply_to(ModConfigCat).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?MODULE),
    teletype_bindings:bind(id(), ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:service_added_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> 'ok'.
handle_req(JObj, 'false') ->
    lager:debug("invalid data for ~s", [id()]),
    teletype_util:send_update(JObj, <<"failed">>, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [id()]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, id()) of
        'false' -> teletype_util:notification_disabled(DataJObj, id());
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = macros(DataJObj),

    %% Load templates
    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    AccountId = kapi_notifications:account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj]),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, teletype_util:mod_config_cat(id())),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec macros(kz_json:object()) -> kz_proplist().
macros(DataJObj) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, reseller_info_data(DataJObj)}
    ,{<<"sub_account">>, sub_account_data(DataJObj)}
    ,{<<"service_changes">>, service_added_data(DataJObj)}
    ,{<<"user">>, auth_user_data(DataJObj)}
    ,{<<"time_stamp">>, time_stamp(DataJObj)}
    ].

-spec time_stamp(kz_json:object()) -> kz_proplist().
time_stamp(DataJObj) ->
    TS = kz_json:get_integer_value(<<"time_stamp">>, DataJObj),
    teletype_util:fix_timestamp(TS, DataJObj).

-spec reseller_info_data(kz_json:object()) -> kz_proplist().
reseller_info_data(DataJObj) ->
    Audit = kz_json:get_value(<<"audit_log">>, DataJObj),
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' ->
            AccountId = lists:last(kz_json:get_value(<<"tree">>, Audit)),
            ResellerId = teletype_util:find_reseller_id(AccountId),
            teletype_util:find_account_params(ResellerId)
    end.

-spec sub_account_data(kz_json:object()) -> kz_proplist().
sub_account_data(DataJObj) ->
    Audit = kz_json:get_value(<<"audit_log">>, DataJObj),
    case teletype_util:is_preview(DataJObj) of
        'true' -> teletype_util:account_params(DataJObj);
        'false' ->
            AccountId = kzd_audit_log:authenticating_user_account_id(Audit),
            teletype_util:find_account_params(AccountId)
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

-spec auth_user_data(kz_json:object()) -> kz_proplist().
auth_user_data(DataJObj) ->
    Audit = kz_json:get_value(<<"audit_log">>, DataJObj),
    case teletype_util:is_preview(DataJObj) of
        'true' ->
            case teletype_util:open_doc(<<"user">>, 'undefined', DataJObj) of
                {'ok', UserJObj} -> teletype_util:user_params(UserJObj);
                {'error', _} -> []
            end;
        'false' ->
            AccountId = kzd_audit_log:authenticating_user_account_id(Audit),
            UserId = kz_json:get_value([<<"authenticating_user">>, <<"auth_user_id">>], Audit),
            case fetch_user(AccountId, UserId) of
                {'ok', UserJObj} -> teletype_util:user_params(UserJObj);
                {'error', _} -> []
            end
    end.

-ifdef(TEST).
fetch_user(?AN_ACCOUNT_ID, ?AN_ACCOUNT_USER_ID) ->
    kz_json:fixture(?APP, "an_account_user.json").
-else.
fetch_user(AccountId, UserId) -> kzd_user:fetch(AccountId, UserId).
-endif.
