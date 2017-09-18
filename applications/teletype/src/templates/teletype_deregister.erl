%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_deregister).
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

-spec subject() -> ne_binary().
subject() -> <<"Loss of Registration for '{{last_registration.username}}'">>.

-spec category() -> ne_binary().
category() -> <<"registration">>.

-spec friendly_name() -> ne_binary().
friendly_name() -> <<"Deregister Notice">>.

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
    handle_req(JObj, kapi_notifications:deregister_v(JObj)).

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
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], subject()),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, teletype_util:mod_config_cat(id())),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec macros(kz_json:object()) -> kz_proplist().
macros(DataJObj) ->
    macros(DataJObj, teletype_util:account_params(DataJObj)).

-spec macros(kz_json:object(), kz_proplist()) -> kz_proplist().
macros(DataJObj, []) ->
    lager:info("no account data available for deregister, not sending notification"),
    teletype_util:send_update(DataJObj, <<"failed">>, <<"missing account">>),
    exit('normal');
macros(DataJObj, AccountParams) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, AccountParams}
    ,{<<"last_registration">>, kz_json:to_proplist(DataJObj)}
    ].
