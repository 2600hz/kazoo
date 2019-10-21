%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_new_account).
-behaviour(teletype_gen_email_template).

-export([id/0
        ,init/0
        ,macros/0, macros/1
        ,subject/0
        ,category/0
        ,friendly_name/0
        ,to/0, from/0, cc/0, bcc/0, reply_to/0
        ]).
-export([handle_req/1]).

-include("teletype.hrl").

-spec id() -> kz_term:ne_binary().
id() -> <<"new_account">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"admin.first_name">>, <<"first_name">>, <<"First Name">>, <<"Admin user first name">>)
      ,?MACRO_VALUE(<<"admin.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Admin user last name">>)
      ,?MACRO_VALUE(<<"admin.email">>, <<"email">>, <<"email">>, <<"Admin user email">>)
      ,?MACRO_VALUE(<<"admin.timezone">>, <<"timezone">>, <<"timezone">>, <<"Admin user timezone">>)
       | ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> kz_term:ne_binary().
subject() -> <<"Your new VoIP services account '{{account.name}}' has been created">>.

-spec category() -> kz_term:ne_binary().
category() -> <<"account">>.

-spec friendly_name() -> kz_term:ne_binary().
friendly_name() -> <<"New Account">>.

-spec to() -> kz_json:object().
to() -> ?CONFIGURED_EMAILS(?EMAIL_ADMINS).

-spec from() -> kz_term:api_ne_binary().
from() -> teletype_util:default_from_address().

-spec cc() -> kz_json:object().
cc() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec bcc() -> kz_json:object().
bcc() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec reply_to() -> kz_term:api_ne_binary().
reply_to() -> teletype_util:default_reply_to().

-spec init() -> 'ok'.
init() ->
    kz_log:put_callid(?MODULE),
    teletype_templates:init(?MODULE),
    teletype_bindings:bind(id(), ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:new_account_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [id()]),
    teletype_util:notification_failed(id(), <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [id()]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, id()) of
        'false' -> teletype_util:notification_disabled(DataJObj, id());
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    Macros = macros(DataJObj),

    %% Load templates
    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    AccountId = kapi_notifications:account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], subject()),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = fix_to_addresses(DataJObj, TemplateMetaJObj),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(id());
        {'error', Reason} -> teletype_util:notification_failed(id(), Reason)
    end.

-spec fix_to_addresses(kz_json:object(), kz_json:object()) -> email_map().
fix_to_addresses(DataJObj, TemplateMetaJObj) ->
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, id()),
    EmailType = kz_json:find([<<"to">>, <<"type">>]
                            ,[DataJObj, TemplateMetaJObj]
                            ),
    IsPreview = teletype_util:is_preview(DataJObj),
    fix_to_addresses(DataJObj, Emails, EmailType, IsPreview).

fix_to_addresses(_, Emails, _, 'true') ->
    Emails;
fix_to_addresses(DataJObj, Emails, ?EMAIL_ADMINS, 'false') ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    ResellerId = kz_services_reseller:get_id(AccountId),
    props:set_value(<<"to">>, teletype_util:find_account_admin_email(ResellerId), Emails);
fix_to_addresses(_, Emails, _, 'false') ->
    Emails.

-spec macros(kz_json:object()) -> kz_term:proplist().
macros(DataJObj) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"admin">>, admin_user_properties(DataJObj)}
    ].

-spec admin_user_properties(kz_json:object()) -> kz_term:proplist().
admin_user_properties(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    case kzd_accounts:fetch(AccountId) of
        {'ok', JObj} -> account_admin_user_properties(JObj);
        {'error', _} -> []
    end.

-spec account_admin_user_properties(kz_json:object()) -> kz_term:proplist().
account_admin_user_properties(AccountJObj) ->
    AccountDb = kz_doc:account_db(AccountJObj),
    case kz_datamgr:get_results(AccountDb, <<"users/crossbar_listing">>, ['include_docs']) of
        {'error', _E} ->
            ?LOG_DEBUG("failed to get user listing from ~s: ~p", [AccountDb, _E]),
            [];
        {'ok', Users} ->
            find_admin(Users)
    end.

-spec find_admin(kz_json:objects()) -> kz_term:proplist().
find_admin([]) ->
    ?LOG_DEBUG("account has no admin users"),
    [];
find_admin([User|Users]) ->
    UserDoc = kz_json:get_json_value(<<"doc">>, User),
    case kzd_users:is_account_admin(UserDoc) of
        'true' -> teletype_util:user_params(UserDoc);
        'false' -> find_admin(Users)
    end.
