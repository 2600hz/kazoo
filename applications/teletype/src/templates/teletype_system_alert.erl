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
-module(teletype_system_alert).
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
id() -> <<"system_alert">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"message">>, <<"message">>, <<"Message">>, <<"System message">>)
       | ?USER_MACROS
       ++ ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> kz_term:ne_binary().
subject() -> <<"System Alert: '{{request.level}}' from '{{request.node}}'">>.

-spec category() -> kz_term:ne_binary().
category() -> <<"system">>.

-spec friendly_name() -> kz_term:ne_binary().
friendly_name() -> <<"System Notifications">>.

-spec to() -> kz_json:object().
to() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

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

-spec handle_req(kz_json:object()) -> template_responses().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:system_alert_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_responses().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [id()]),
    teletype_util:notification_failed(id(), <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [id()]),

    case kz_json:get_value([<<"Details">>, <<"Format">>], JObj) of
        'undefined' -> process_req_as_email(JObj, 'true');
        _Format ->
            lager:debug("using format string '~s'", [_Format]),

            UseEmail = kz_term:is_true(teletype_util:template_system_value(id(), <<"enable_email_alerts">>, 'true')),
            SubscriberUrl = teletype_util:template_system_value(id(), <<"subscriber_url">>),
            [process_req_as_email(JObj, UseEmail)
            ,process_req_as_http(JObj, SubscriberUrl)
            ]
    end.

-spec process_req_as_http(kz_json:object(), kz_term:api_binary()) -> template_response().
process_req_as_http(_JObj, 'undefined') ->
    teletype_util:notification_ignored(<<(id())/binary, "_http">>);
process_req_as_http(JObj, Url) ->
    Headers = [{"Content-Type", "application/json"}],
    Encoded = kz_json:encode(JObj),
    case kz_http:post(kz_term:to_list(Url), Headers, Encoded) of
        {'ok', _2xx, _ResponseHeaders, _ResponseBody}
          when (_2xx - 200) < 100 -> %% ie: match "2"++_
            lager:debug("JSON data successfully POSTed to '~s'", [Url]),
            teletype_util:notification_completed(<<(id())/binary, "_http">>);
        Error ->
            lager:debug("failed to POST JSON data to ~p for reason: ~p", [Url, Error]),
            Reason = kz_term:to_binary(io_lib:format("~p", [Error])),
            teletype_util:notification_failed(Reason, <<(id())/binary, "_http">>)
    end.

-spec process_req_as_email(kz_json:object(), boolean() | kz_json:object()) -> template_response().
process_req_as_email(_JObj, 'false') ->
    lager:debug("email not enabled for system alerts");
process_req_as_email(JObj, 'true') ->
    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    case teletype_util:is_notice_enabled_default(id()) of
        'false' -> teletype_util:notification_disabled(DataJObj, <<(id())/binary, "_email">>);
        'true' -> process_req_as_email(DataJObj)
    end.

-spec process_req_as_email(kz_json:object()) -> template_response().
process_req_as_email(DataJObj) ->
    Macros = macros(DataJObj),

    %% Populate templates
    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    AccountId = kapi_notifications:account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:get_ne_binary_value(<<"subject">>, TemplateMetaJObj, subject()),
    Subject = try kz_json:get_ne_binary_value(<<"subject">>, DataJObj) of
                  'undefined' -> teletype_util:render_subject(Subject0, Macros);
                  Text -> Text
              catch
                  _:_ -> <<"system alert received into ", (kz_term:to_binary(node()))/binary>>
              end,

    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    Emails = teletype_util:find_addresses(kz_json:set_value(<<"account_id">>, MasterAccountId, DataJObj)
                                         ,TemplateMetaJObj
                                         ,id()
                                         ),

    Attachments = teletype_util:maybe_get_attachments(DataJObj),

    put('skip_smtp_log', 'true'),
    case teletype_util:send_email(Emails, Subject, RenderedTemplates, Attachments) of
        'ok' -> teletype_util:notification_completed(<<(id())/binary, "_email">>);
        {'error', Reason} -> teletype_util:notification_failed(<<(id())/binary, "_email">>, Reason)
    end.

-spec macros(kz_json:object()) -> kz_term:proplist().
macros(DataJObj) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"user">>, admin_user_data(DataJObj)}
    ,{<<"request">>, request_macros(DataJObj)}
    ,{<<"message">>, kz_json:get_value(<<"message">>, DataJObj, <<>>)}
     | details_macros(DataJObj)
    ].

-spec details_macros(kz_json:object()) -> kz_term:proplist().
details_macros(DataJObj) ->
    case kz_json:get_value(<<"details">>, DataJObj) of
        'undefined' -> [];
        <<_/binary>> = Details -> [{<<"details">>, [{<<"message">>, Details}]}];
        Details when is_list(Details) -> details_groups(Details);
        Details -> details_groups(kz_json:recursive_to_proplist(Details))
    end.

-spec details_groups(kz_term:proplist()) -> kz_term:proplist().
details_groups(Details) ->
    details_groups(Details, {<<"details">>, []}).

-spec details_groups(kz_term:proplist(), {kz_term:ne_binary(), kz_term:proplist()}) ->
                            kz_term:proplist().
details_groups([], {_, Acc}) -> Acc;

details_groups([{<<"key_value_store">>, V} | KS], {Group, Acc}) ->
    details_groups(KS, {Group, details_groups(V, {<<"key_store">>, Acc})});
details_groups([{<<"custom_channel_vars">>, V} | KS], {Group, Acc}) ->
    details_groups(KS, {Group, details_groups(V, {<<"channel_vars">>, Acc})});
details_groups([{<<"custom_sip_headers">>, V} | KS], {Group, Acc}) ->
    details_groups(KS, {Group, details_groups(V, {<<"sip_headers">>, Acc})});
details_groups([{<<"cf_flow">>, V} | KS], {Group, Acc}) ->
    details_groups(KS, {Group, details_groups(V, {<<"callflow">>, Acc})});
details_groups([{<<"error_details">>, V} | KS], {Group, Acc}) ->
    details_groups(KS, {Group, details_groups(V, {<<"error_details">>, Acc})});
details_groups([{<<"reply_headers">>, V} | KS], {Group, Acc}) ->
    details_groups(KS, {Group, details_groups(V, {<<"http_headers">>, Acc})});
details_groups([{<<"cf_", _/binary>>,_}=KV | KS], {Group, Acc}) ->
    details_groups(KS, {Group, add_to_group(<<"callflow">>, KV, Acc)});
details_groups([KV | KS], {Group, Acc}) ->
    details_groups(KS, {Group, add_to_group(Group, KV, Acc)}).

-spec add_to_group(kz_term:ne_binary(), {kz_term:ne_binary(), kz_json:json_term()}, kz_term:proplist()) ->
                          kz_term:proplist().
add_to_group(Group, KV, Acc) ->
    case props:get_value(Group, Acc) of
        'undefined' -> props:set_value(Group, [KV], Acc);
        Props -> props:set_value(Group, props:insert_value(KV, Props), Acc)
    end.

-spec request_macros(kz_json:object()) -> kz_term:proplist().
request_macros(DataJObj) ->
    kz_json:recursive_to_proplist(
      kz_json:delete_keys([<<"details">>
                          ,<<"app_version">>
                          ,<<"app_name">>
                          ,<<"event_name">>
                          ,<<"event_category">>
                          ,<<"server_id">>
                          ,<<"message">>
                          ,<<"subject">>
                          ,<<"account">>
                          ,<<"preview">>
                          ,<<"text">>
                          ,<<"html">>
                          ,<<"from">>
                          ,<<"bcc">>
                          ,<<"cc">>
                          ,<<"to">>
                          ,<<"reply_to">>
                          ,<<"format">>
                          ,<<"attachment_url">>
                          ]
                         ,DataJObj
                         )
     ).

-spec admin_user_data(kz_json:object()) -> kz_term:proplist().
admin_user_data(DataJObj) ->
    AccountId = kapi_notifications:account_id(DataJObj),
    case teletype_util:find_account_admin(AccountId) of
        'undefined' -> [];
        UserDoc -> teletype_util:user_params(UserDoc)
    end.
