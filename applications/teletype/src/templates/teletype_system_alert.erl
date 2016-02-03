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

-include("../teletype.hrl").

-define(TEMPLATE_ID, <<"system_alert">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
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

-define(TEMPLATE_HTML, wh_util:to_binary(
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

-define(TEMPLATE_TEXT, wh_util:to_binary(
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
    wh_util:put_callid(?MODULE),
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
                                          ]),
    build_renderers().

-spec handle_system_alert(wh_json:object(), wh_proplist()) -> 'ok'.
handle_system_alert(JObj, _Props) ->
    'true' = wapi_notifications:system_alert_v(JObj),

    wh_util:put_callid(JObj),

    case wh_json:get_value([<<"Details">>, <<"Format">>], JObj) of
        'undefined' -> handle_req_as_email(JObj, 'true');
        _Format ->
            lager:debug("using format string '~s'", [_Format]),
            UseEmail = whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enable_email_alerts">>, 'true'),
            Url = whapps_config:get_string(?MOD_CONFIG_CAT, <<"subscriber_url">>),
            handle_req_as_email(JObj, UseEmail),
            handle_req_as_http(JObj, Url, UseEmail)
    end.

-spec handle_req_as_http(wh_json:object(), api_binary(), boolean()) -> 'ok'.
handle_req_as_http(JObj, 'undefined', UseEmail) ->
    handle_req_as_email(JObj, UseEmail);
handle_req_as_http(JObj, Url, UseEmail) ->
    Headers = [{"Content-Type", "application/json"}],
    Encoded = wh_json:encode(JObj),

    case ibrowse:send_req(wh_util:to_list(Url), Headers, 'post', Encoded) of
        {'ok', "2" ++ _, _ResponseHeaders, _ResponseBody} ->
            lager:debug("JSON data successfully POSTed to '~s'", [Url]);
        _Error ->
            lager:debug("failed to POST JSON data to ~p for reason: ~p", [Url,_Error]),
            handle_req_as_email(JObj, UseEmail)
    end.

-spec handle_req_as_email(wh_json:object(), boolean() | wh_json:object()) -> 'ok'.
handle_req_as_email(_JObj, 'false') ->
    lager:debug("email not enabled for system alerts");
handle_req_as_email(JObj, 'true') ->
    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    case teletype_util:is_notice_enabled_default(?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(wh_json:object()) -> 'ok'.
-spec process_req(wh_json:object(), wh_proplist()) -> 'ok'.
process_req(DataJObj) ->
    lager:debug("template is enabled for account, fetching templates for rendering"),
    %% Load templates
    process_req(DataJObj, teletype_templates:fetch(?TEMPLATE_ID)).

process_req(DataJObj, Templates) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
              ,{<<"request">>, request_macros(DataJObj)}
              ,{<<"message">>, wh_json:get_value(<<"message">>, DataJObj, <<>>)}
              | details_macros(DataJObj)
             ],

    %% Populate templates
    RenderedTemplates = [{ContentType, render(ContentType, Macros)}
                         || {ContentType, _Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_meta(?TEMPLATE_ID
                                      ,teletype_util:find_account_id(DataJObj)
                                     ),

    Subject =
        teletype_util:render_subject(
          wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
          ,Macros
         ),

    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    Emails = teletype_util:find_addresses(
               wh_json:set_value(<<"account_id">>, MasterAccountId, DataJObj)
               ,TemplateMetaJObj
               ,?MOD_CONFIG_CAT
              ),

    put('skip_smtp_log', 'true'),
    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec details_macros(wh_json:object()) -> wh_proplist().
details_macros(DataJObj) ->
    case wh_json:get_value(<<"details">>, DataJObj) of
        'undefined' -> [];
        <<_/binary>> = Details -> [{<<"details">>, [{<<"message">>, Details}]}];
        Details when is_list(Details) -> details_groups(Details);
        Details -> details_groups(wh_json:recursive_to_proplist(Details))
    end.

-spec details_groups(wh_proplist()) -> wh_proplist().
details_groups(Details) ->
    details_groups(Details, {<<"details">>, []}).

-spec details_groups(wh_proplist(), {ne_binary(), wh_proplist()}) ->
                            wh_proplist().
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

-spec add_to_group(ne_binary(), {wh_json:key(), wh_json:json_term()}, wh_proplist()) ->
                          wh_proplist().
add_to_group(Group, KV, Acc) ->
    case props:get_value(Group, Acc) of
        'undefined' -> props:set_value(Group,[KV], Acc);
        Props -> props:set_value(Group, props:insert_value(KV, Props), Acc)
    end.

-spec request_macros(wh_json:object()) -> wh_proplist().
request_macros(DataJObj) ->
    wh_json:recursive_to_proplist(
      wh_json:delete_keys([<<"details">>
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

-spec build_renderers() -> 'ok'.
build_renderers() ->
    [build_renderer(ContentType, Template) ||
        {ContentType, Template} <- teletype_templates:fetch(?TEMPLATE_ID)
    ],
    lager:debug("built renderers for system_alerts").

-spec build_renderer(ne_binary(), iolist()) -> 'ok'.
build_renderer(ContentType, Template) ->
    ModuleName = renderer_name(ContentType),
    case erlydtl:compile_template(Template
                                 ,ModuleName
                                 ,[{'out_dir', 'false'}
                                  ,'return'
                                  ]
                                 )
    of
        {'ok', Name} ->
            lager:debug("built system_alerts renderer for ~s", [Name]);
        {'ok', Name, []} ->
            lager:debug("built system_alerts renderer for ~s", [Name]);
        {'ok', Name, Warnings} ->
            lager:debug("compiling template ~s produced warnings: ~p", [Name, Warnings])
    end.

-spec renderer_name(ne_binary()) -> atom().
renderer_name(ContentType) ->
    wh_util:to_atom(<<(?TEMPLATE_ID)/binary, ContentType/binary>>, 'true').

-spec render(ne_binary(), wh_proplist()) -> {'ok', iolist()} |
                                            {'error', any()}.
render(ContentType, Macros) ->
    ModuleName = renderer_name(ContentType),
    try ModuleName:render(Macros) of
        {'ok', IOList} ->
            lager:debug("rendered ~s template successfully", [ContentType]),
            iolist_to_binary(IOList);
        {'error', _E} ->
            lager:debug("failed to render ~s template: ~p", [ContentType, _E]),
            throw({'error', 'template_error'})
    catch
        'error':'undef' ->
            ST = erlang:get_stacktrace(),
            lager:debug("something in the template ~s is undefined", [ModuleName]),
            wh_util:log_stacktrace(ST),
            throw({'error', 'template_error'});
        _E:R ->
            ST = erlang:get_stacktrace(),
            lager:debug("crashed rendering template ~s: ~s: ~p", [ModuleName, _E, R]),
            wh_util:log_stacktrace(ST),
            throw({'error', 'template_error'})
    end.
