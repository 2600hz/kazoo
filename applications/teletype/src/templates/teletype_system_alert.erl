%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_system_alert).

-export([init/0
         ,handle_system_alert/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"system_alert">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,kz_json:from_list(
           [?MACRO_VALUE(<<"message">>, <<"message">>, <<"Message">>, <<"System message">>)
            | ?ACCOUNT_MACROS ++ ?USER_MACROS
           ])
       ).

-define(TEMPLATE_HTML_HEAD, "<html><head><meta charset=\"utf-8\" /></head><body>").
-define(TEMPLATE_HTML_TAIL, "</body></html>").
-define(TEMPLATE_HTML_ALERT, "<h2>Alert</h2><p>{{message}}</p>").
-define(TEMPLATE_HTML_GROUP(T,C), io_lib:format("{% if ~s %}<h2>~s</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">{% for key, value in ~s %}<tr><td>{{ key }}: </td><td>{{ value }}</td></tr>{% endfor %}</table>{% endif %}", [C, T, C]) ).

-define(TEMPLATE_HTML_PRODUCER, ?TEMPLATE_HTML_GROUP("Producer", "request")).
-define(TEMPLATE_HTML_DETAILS, ?TEMPLATE_HTML_GROUP("Details", "details")).
-define(TEMPLATE_HTML_CCVS, ?TEMPLATE_HTML_GROUP("Channel Vars", "channel_vars")).
-define(TEMPLATE_HTML_SIPHDR, ?TEMPLATE_HTML_GROUP("SIP Headers", "sip_headers")).
-define(TEMPLATE_HTML_KVS, ?TEMPLATE_HTML_GROUP("Key Value Store", "key_store")).
-define(TEMPLATE_HTML_ERROR, ?TEMPLATE_HTML_GROUP("Error Details", "error_details")).
-define(TEMPLATE_HTML_FLOW, ?TEMPLATE_HTML_GROUP("Callflow", "callflow")).

-define(TEMPLATE_HTML_ACCOUNT, "{% if account %}<h2>Account</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Account ID: </td><td>{{account.id}}</td></tr><tr><td>Account Name: </td><td>{{account.name}}</td></tr><tr><td>Account Realm: </td><td>{{account.realm}}</td></tr></table>{% endif %}").
-define(TEMPLATE_HTML_USER, "{% if user %}<h2>Admin</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Name: </td><td>{{user.first_name}} {{user.last_name}}</td></tr><tr><td>Email: </td><td>{{user.email}}</td></tr><tr><td>Timezone: </td><td>{{user.timezone}}</td></tr></table>{% endif %}").
-define(TEMPLATE_HTML_NUMBERS, "{% if account.pvt_wnm_numbers %}<h2>Phone Numbers</h2><ul>{% for number in account.pvt_wnm_numbers %}<li>{{number}}</li>{% endfor %}</ul>{% endif %}").

-define(TEMPLATE_HTML, kz_term:to_binary(
                         lists:flatten(
                           [?TEMPLATE_HTML_HEAD
                            ,?TEMPLATE_HTML_ALERT
                            ,?TEMPLATE_HTML_PRODUCER
                            ,?TEMPLATE_HTML_DETAILS
                            ,?TEMPLATE_HTML_FLOW
                            ,?TEMPLATE_HTML_ERROR
                            ,?TEMPLATE_HTML_KVS
                            ,?TEMPLATE_HTML_CCVS
                            ,?TEMPLATE_HTML_SIPHDR
                            ,?TEMPLATE_HTML_ACCOUNT
                            ,?TEMPLATE_HTML_USER
                            ,?TEMPLATE_HTML_NUMBERS
                            ,?TEMPLATE_HTML_TAIL
                           ]
                          )
                        )
       ).


-define(TEMPLATE_TEXT_ALERT, "Alert\n{{message}}\n").
-define(TEMPLATE_TEXT_GROUP(T,C), io_lib:format("{% if ~s %}~s\n{% for key, value in ~s %}{{ key }}: {{ value }}\n{% endfor %}\n{% endif %}", [C, T, C])).

-define(TEMPLATE_TEXT_PRODUCER, ?TEMPLATE_TEXT_GROUP("Producer", "request")).
-define(TEMPLATE_TEXT_DETAILS, ?TEMPLATE_TEXT_GROUP("Details", "details")).
-define(TEMPLATE_TEXT_CCVS, ?TEMPLATE_TEXT_GROUP("Channel Vars", "channel_vars")).
-define(TEMPLATE_TEXT_SIPHDR, ?TEMPLATE_TEXT_GROUP("SIP Headers", "sip_headers")).
-define(TEMPLATE_TEXT_KVS, ?TEMPLATE_TEXT_GROUP("Key Value Store", "key_store")).
-define(TEMPLATE_TEXT_ERROR, ?TEMPLATE_TEXT_GROUP("Error Details", "error_details")).
-define(TEMPLATE_TEXT_FLOW, ?TEMPLATE_TEXT_GROUP("Callflow", "callflow")).

-define(TEMPLATE_TEXT_ACCOUNT, "{% if account %}Account\nAccount ID: {{account.id}}\nAccount Name: {{account.name}}\nAccount Realm: {{account.realm}}\n\n{% endif %}").
-define(TEMPLATE_TEXT_USER, "{% if user %}Admin\nName: {{user.first_name}} {{user.last_name}}\nEmail: {{user.email}}\nTimezone: {{user.timezone}}\n\n{% endif %}").
-define(TEMPLATE_TEXT_NUMBERS, "{% if account.pvt_wnm_numbers %}Phone Numbers\n{% for number in account.pvt_wnm_numbers %}{{number}}\n{% endfor %}\n{% endif %}").

-define(TEMPLATE_TEXT, kz_term:to_binary(
                         lists:flatten(
                           [?TEMPLATE_TEXT_ALERT
                            ,?TEMPLATE_TEXT_PRODUCER
                            ,?TEMPLATE_TEXT_DETAILS
                            ,?TEMPLATE_TEXT_FLOW
                            ,?TEMPLATE_TEXT_ERROR
                            ,?TEMPLATE_TEXT_KVS
                            ,?TEMPLATE_TEXT_CCVS
                            ,?TEMPLATE_TEXT_SIPHDR
                            ,?TEMPLATE_TEXT_ACCOUNT
                            ,?TEMPLATE_TEXT_USER
                            ,?TEMPLATE_TEXT_NUMBERS
                           ]
                          )
                        )
       ).

-define(TEMPLATE_SUBJECT, <<"VoIP Services: {{request.level}} from {{request.node}}">>).
-define(TEMPLATE_CATEGORY, <<"system">>).
-define(TEMPLATE_NAME, <<"System Notifications">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
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

-spec handle_system_alert(kz_json:object(), kz_proplist()) -> 'ok'.
handle_system_alert(JObj, _Props) ->
    'true' = kapi_notifications:system_alert_v(JObj),

    kz_util:put_callid(JObj),

    case kz_json:get_value([<<"Details">>, <<"Format">>], JObj) of
        'undefined' -> handle_req_as_email(JObj, 'true');
        _Format ->
            lager:debug("using format string '~s'", [_Format]),
            UseEmail = kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enable_email_alerts">>, 'true'),
            Url = kapps_config:get_string(?MOD_CONFIG_CAT, <<"subscriber_url">>),
            handle_req_as_email(JObj, UseEmail),
            handle_req_as_http(JObj, Url, UseEmail)
    end.

-spec handle_req_as_http(kz_json:object(), api_binary(), boolean()) -> 'ok'.
handle_req_as_http(JObj, 'undefined', UseEmail) ->
    handle_req_as_email(JObj, UseEmail);
handle_req_as_http(JObj, Url, UseEmail) ->
    Headers = [{"Content-Type", "application/json"}],
    Encoded = kz_json:encode(JObj),
    case kz_http:post(kz_term:to_list(Url), Headers, Encoded) of
        {'ok', _2xx, _ResponseHeaders, _ResponseBody}
          when (_2xx - 200) < 100 -> %% ie: match "2"++_
            lager:debug("JSON data successfully POSTed to '~s'", [Url]);
        _Error ->
            lager:debug("failed to POST JSON data to ~p for reason: ~p", [Url,_Error]),
            handle_req_as_email(JObj, UseEmail)
    end.

-spec handle_req_as_email(kz_json:object(), boolean() | kz_json:object()) -> 'ok'.
handle_req_as_email(_JObj, 'false') ->
    lager:debug("email not enabled for system alerts");
handle_req_as_email(JObj, 'true') ->
    %% Gather data for template
    case teletype_util:is_notice_enabled_default(?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured");
        'true' -> process_req(kz_json:normalize(JObj))
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    lager:debug("template is enabled for account, fetching templates for rendering"),
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
              ,{<<"request">>, request_macros(DataJObj)}
              ,{<<"message">>, kz_json:get_value(<<"message">>, DataJObj, <<>>)}
              | details_macros(DataJObj)
             ],

    %% Populate templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                      ,teletype_util:find_account_id(DataJObj)
                                     ),

    Subject =
        teletype_util:render_subject(
          kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
          ,Macros
         ),

    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    Emails = teletype_util:find_addresses(
               kz_json:set_value(<<"account_id">>, MasterAccountId, DataJObj)
               ,TemplateMetaJObj
               ,?MOD_CONFIG_CAT
              ),

    put('skip_smtp_log', 'true'),
    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

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
details_groups([{<<"cf_", _/binary>>,_}=KV | KS], {Group, Acc}) ->
    details_groups(KS, {Group, add_to_group(<<"callflow">>, KV, Acc)});
details_groups([KV | KS], {Group, Acc}) ->
    details_groups(KS, {Group, add_to_group(Group, KV, Acc)}).

-spec add_to_group(ne_binary(), {kz_json:key(), kz_json:json_term()}, kz_proplist()) ->
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
                          ]
                          ,DataJObj
                         )
     ).
