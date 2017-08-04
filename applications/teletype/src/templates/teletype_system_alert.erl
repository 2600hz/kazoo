%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_system_alert).
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

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".system_alert">>).

-define(SHOULD_USE_EMAIL
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enable_email_alerts">>, 'true')).
-define(SUBSCRIBER_URL
       ,kapps_config:get_string(?MOD_CONFIG_CAT, <<"subscriber_url">>)).


-spec id() -> ne_binary().
id() -> <<"system_alert">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"message">>, <<"message">>, <<"Message">>, <<"System message">>)
       | ?USER_MACROS
       ++ ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> ne_binary().
subject() -> <<"System Alert: '{{request.level}}' from '{{request.node}}'">>.

-spec category() -> ne_binary().
category() -> <<"system">>.

-spec friendly_name() -> ne_binary().
friendly_name() -> <<"System Notifications">>.

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
    handle_req(JObj, kapi_notifications:system_alert_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> 'ok'.
handle_req(JObj, 'false') ->
    lager:debug("invalid data for ~s", [id()]),
    teletype_util:send_update(JObj, <<"failed">>, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [id()]),

    case kz_json:get_value([<<"Details">>, <<"Format">>], JObj) of
        'undefined' -> handle_req_as_email(JObj, 'true');
        _Format ->
            lager:debug("using format string '~s'", [_Format]),
            UseEmail = ?SHOULD_USE_EMAIL,
            handle_req_as_email(JObj, UseEmail),
            handle_req_as_http(JObj, ?SUBSCRIBER_URL, UseEmail)
    end.

-spec handle_req_as_http(kz_json:object(), api_binary(), boolean()) -> 'ok'.
handle_req_as_http(_JObj, 'undefined', _UseEmail) -> 'ok';
handle_req_as_http(JObj, Url, UseEmail) ->
    Headers = [{"Content-Type", "application/json"}],
    Encoded = kz_json:encode(JObj),
    case kz_http:post(kz_term:to_list(Url), Headers, Encoded) of
        {'ok', _2xx, _ResponseHeaders, _ResponseBody}
          when (_2xx - 200) < 100 -> %% ie: match "2"++_
            _ = not UseEmail
                andalso teletype_util:send_update(JObj, <<"completed">>),
            lager:debug("JSON data successfully POSTed to '~s'", [Url]);
        _Error ->
            lager:debug("failed to POST JSON data to ~p for reason: ~p", [Url,_Error]),
            maybe_send_email(JObj, UseEmail)
    end.

-spec maybe_send_email(kz_json:object(), boolean()) -> 'ok'.
maybe_send_email(JObj, 'false') ->
    handle_req_as_email(JObj, 'true');
maybe_send_email(_JObj, 'true') ->
    'ok'.

-spec handle_req_as_email(kz_json:object(), boolean() | kz_json:object()) -> 'ok'.
handle_req_as_email(_JObj, 'false') ->
    lager:debug("email not enabled for system alerts");
handle_req_as_email(JObj, 'true') ->
    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    case teletype_util:is_notice_enabled_default(id()) of
        'false' -> teletype_util:notification_disabled(DataJObj, id());
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
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
                                         ,teletype_util:mod_config_cat(id())
                                         ),

    Attachments = teletype_util:maybe_get_attachments(DataJObj),

    put('skip_smtp_log', 'true'),
    case teletype_util:send_email(Emails, Subject, RenderedTemplates, Attachments) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec macros(kz_json:object()) -> kz_proplist().
macros(DataJObj) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"user">>, admin_user_data(DataJObj)}
    ,{<<"request">>, request_macros(DataJObj)}
    ,{<<"message">>, kz_json:get_value(<<"message">>, DataJObj, <<>>)}
     | details_macros(DataJObj)
    ].

-spec details_macros(kz_json:object()) -> kz_proplist().
details_macros(DataJObj) ->
    case kz_json:get_value(<<"details">>, DataJObj) of
        'undefined' -> [];
        <<_/binary>> = Details -> [{<<"details">>, [{<<"message">>, Details}]}];
        Details when is_list(Details) -> details_groups(Details);
        Details -> details_groups(kz_json:recursive_to_proplist(Details))
    end.

-spec details_groups(kz_proplist()) -> kz_proplist().
details_groups(Details) ->
    details_groups(Details, {<<"details">>, []}).

-spec details_groups(kz_proplist(), {ne_binary(), kz_proplist()}) ->
                            kz_proplist().
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

-spec add_to_group(ne_binary(), {kz_json:path(), kz_json:json_term()}, kz_proplist()) ->
                          kz_proplist().
add_to_group(Group, KV, Acc) ->
    case props:get_value(Group, Acc) of
        'undefined' -> props:set_value(Group,[KV], Acc);
        Props -> props:set_value(Group, props:insert_value(KV, Props), Acc)
    end.

-spec request_macros(kz_json:object()) -> kz_proplist().
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

-spec admin_user_data(kz_json:object()) -> kz_proplist().
admin_user_data(DataJObj) ->
    AccountId = kapi_notifications:account_id(DataJObj),
    case teletype_util:find_account_admin(AccountId) of
        'undefined' -> [];
        UserDoc -> teletype_util:user_params(UserDoc)
    end.
