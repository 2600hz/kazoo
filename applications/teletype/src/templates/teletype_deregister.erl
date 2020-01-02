%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_deregister).
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
id() -> <<"deregister">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"last_registration.username">>, <<"last_registration_username">>, <<"SIP Username">>, <<"SIP username">>)
      ,?MACRO_VALUE(<<"last_registration.status">>, <<"last_registration_status">>, <<"Status">>, <<"Status">>)
      ,?MACRO_VALUE(<<"last_registration.user_agent">>, <<"last_registration_user_agent">>, <<"SIP User Agent">>, <<"SIP User Agent">>)
      ,?MACRO_VALUE(<<"last_registration.call_id">>, <<"last_registration_call_id">>, <<"SIP Call ID">>, <<"SIP Call ID">>)
      ,?MACRO_VALUE(<<"last_registration.profile_name">>, <<"last_registration_profile_name">>, <<"Profile Name">>, <<"Profile Name">>)
      ,?MACRO_VALUE(<<"last_registration.presence_hosts">>, <<"last_registration_presence_hosts">>, <<"Presence Hosts">>, <<"Presence Hosts">>)
      ,?MACRO_VALUE(<<"last_registration.from_user">>, <<"last_registration_from_user">>, <<"SIP From User">>, <<"SIP From User">>)
      ,?MACRO_VALUE(<<"last_registration.from_host">>, <<"last_registration_from_host">>, <<"SIP From Host">>, <<"SIP From Host">>)
      ,?MACRO_VALUE(<<"last_registration.to_user">>, <<"last_registration_to_user">>, <<"SIP To User">>, <<"SIP To User">>)
      ,?MACRO_VALUE(<<"last_registration.to_host">>, <<"last_registration_to_host">>, <<"SIP To Host">>, <<"SIP To Host">>)
      ,?MACRO_VALUE(<<"last_registration.rpid">>, <<"last_registration_rpid">>, <<"SIP RPID">>, <<"SIP RPID">>)
      ,?MACRO_VALUE(<<"last_registration.network_ip">>, <<"last_registration_network_ip">>, <<"Network IP">>, <<"Network IP">>)
      ,?MACRO_VALUE(<<"last_registration.network_port">>, <<"last_registration_network_port">>, <<"Network Port">>, <<"Network Port">>)
      ,?MACRO_VALUE(<<"last_registration.contact">>, <<"last_registration_contact">>, <<"SIP Contact">>, <<"SIP Contact">>)
      ,?MACRO_VALUE(<<"last_registration.expires">>, <<"last_registration_expires">>, <<"Expires">>, <<"Expires">>)
      ,?MACRO_VALUE(<<"last_registration.authorizing_id">>, <<"last_registration_authorizing_id">>, <<"Authorizing ID">>, <<"Authorizing ID">>)
       | ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> kz_term:ne_binary().
subject() -> <<"Loss of Registration for '{{last_registration.username}}'">>.

-spec category() -> kz_term:ne_binary().
category() -> <<"registration">>.

-spec friendly_name() -> kz_term:ne_binary().
friendly_name() -> <<"Deregister Notice">>.

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
    handle_req(JObj, kapi_notifications:deregister_v(JObj)).

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
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, id()),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(id());
        {'error', Reason} -> teletype_util:notification_failed(id(), Reason)
    end.

-spec macros(kz_json:object()) -> kz_term:proplist().
macros(DataJObj) ->
    macros(DataJObj, teletype_util:account_params(DataJObj)).

-spec macros(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
macros(DataJObj, []) ->
    lager:info("no account data available for deregister, not sending notification"),
    teletype_util:send_update(DataJObj, <<"failed">>, <<"missing account">>),
    exit('normal');
macros(DataJObj, AccountParams) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, AccountParams}
    ,{<<"last_registration">>, kz_json:to_proplist(DataJObj)}
    ].
