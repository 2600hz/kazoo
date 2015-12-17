%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_port_request_admin).

-export([init/0
         ,handle_req/2
        ]).

-include("../teletype.hrl").

-define(TEMPLATE_ID, <<"port_request_admin">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           ?PORT_REQUEST_MACROS
           ++ ?ACCOUNT_MACROS
          )
       ).

-define(TEMPLATE_TEXT, <<"Port request submitted for {{account.name}} by {{port_request.customer_contact}}.\n\nThe account's tree:\n\n {{ account.name }} ({{ account.id }})\n{% for id, name in account_tree %} {{ name }} ({{ id }})\n{% endfor %}\n\nRequest to port numbers: {{ port_request.numbers }}.\n\nPort Details:\n\n {% for k,v in port_request %} {{ k }} : {{ v }}\n {% endfor %}\n">>).
-define(TEMPLATE_HTML, <<"<p>Port request submitted for {{account.name}} by {{port_request.customer_contact}}.</p>\n<p>The account's tree:</p>\n<ul><li>{{ account.name }} ({{ account.id }})</li>\n{% for id, name in account_tree %}<li>{{ name }} ({{ id }})</li>\n{% endfor %}</ul>\n<p>Request to port numbers: {{ port_request.numbers }}</p>\n\n<p>Port Details:</p>\n\n<ul>{% for k,v in port_request %}<li>{{ k }} : {{ v }}</li>\n{% endfor %}</ul>\n">>).
-define(TEMPLATE_SUBJECT, <<"Port request for {{account.name}}">>).
-define(TEMPLATE_CATEGORY, <<"system">>).
-define(TEMPLATE_NAME, <<"Admin Port Request">>).

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
                                          ]).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:port_request_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(wh_json:object()) -> 'ok'.
process_req(DataJObj) ->
    PortReqId = wh_json:get_value(<<"port_request_id">>, DataJObj),
    {'ok', PortReqJObj} = teletype_util:open_doc(<<"port_request">>, PortReqId, DataJObj),

    ReqData = wh_json:set_value(<<"port_request">>
                                ,teletype_port_utils:fix_port_request_data(PortReqJObj)
                                ,DataJObj
                               ),

    case teletype_util:is_preview(DataJObj) of
        'false' -> handle_port_request(teletype_port_utils:fix_email(ReqData));
        'true' -> handle_port_request(wh_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec handle_port_request(wh_json:object()) -> 'ok'.
-spec handle_port_request(wh_json:object(), wh_proplist()) -> 'ok'.
handle_port_request(DataJObj) ->
    handle_port_request(DataJObj, teletype_templates:fetch(?TEMPLATE_ID, DataJObj)).

handle_port_request(_DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]);
handle_port_request(DataJObj, Templates) ->
    Macros =
        props:filter_undefined(
          [{<<"system">>, teletype_util:system_params()}
           ,{<<"account">>, teletype_util:account_params(DataJObj)}
           ,{<<"port_request">>, port_request_data(wh_json:get_value(<<"port_request">>, DataJObj))}
           ,{<<"account_tree">>, account_tree(wh_json:get_value(<<"account_id">>, DataJObj))}
          ]),

    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
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

    Emails = teletype_util:find_addresses(maybe_set_emails(DataJObj), TemplateMetaJObj, ?MOD_CONFIG_CAT),

    EmailAttachements = teletype_port_utils:get_attachments(DataJObj),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates, EmailAttachements) of
        'ok' ->
            teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} ->
            teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec account_tree(ne_binary()) -> wh_proplist().
-spec account_tree(ne_binaries(), wh_proplist()) -> wh_proplist().
account_tree(AccountId) ->
    {'ok', AccountJObj} = kz_account:fetch(AccountId),
    account_tree(kz_account:tree(AccountJObj), []).

account_tree([], KVs) -> KVs;
account_tree([AncestorId | AncestorIds], KVs) ->
    {'ok', AncestorJObj} = kz_account:fetch(AncestorId),
    account_tree(AncestorIds, [{AncestorId, kz_account:name(AncestorJObj)} | KVs]).

maybe_set_emails(DataJObj) ->
    Fs = [fun maybe_set_from/1
          ,fun maybe_set_to/1
         ],
    lists:foldl(fun(F, Acc) -> F(Acc) end
                ,DataJObj
                ,Fs
               ).

-spec maybe_set_from(wh_json:object()) -> wh_json:object().
maybe_set_from(DataJObj) ->
    SystemFrom = wh_util:to_binary(node()),
    PortRequest = wh_json:get_value(<<"port_request">>, DataJObj),
    DefaultFrom = wh_json:get_value(<<"from">>, DataJObj, SystemFrom),

    Initiator = wh_json:get_value([<<"notifications">>, <<"email">>, <<"send_to">>], PortRequest, DefaultFrom),
    wh_json:set_value(<<"from">>, Initiator, DataJObj).

-spec maybe_set_to(wh_json:object()) -> wh_json:object().
maybe_set_to(DataJObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    case find_port_authority(MasterAccountId, AccountId) of
        'undefined' -> DataJObj;
        <<_/binary>> = To ->
            lager:debug("found port authority: ~p", [To]),
            wh_json:set_value(<<"to">>, [To], DataJObj);
        [_|_] = To ->
            lager:debug("found port authority: ~p", [To]),
            wh_json:set_value(<<"to">>, To, DataJObj)
    end.

-spec find_port_authority(ne_binary(), ne_binary()) -> api_binary() | ne_binaries().
find_port_authority(MasterAccountId, MasterAccountId) ->
    case kz_whitelabel:fetch(MasterAccountId) of
        {'error', _R} ->
            lager:debug("failed to find master account ~s, using system value", [MasterAccountId]),
            whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>);
        {'ok', JObj} ->
            lager:debug("getting master account's port authority"),
            kz_whitelabel:port_authority(JObj)
    end;
find_port_authority(MasterAccountId, AccountId) ->
    case kz_whitelabel:fetch(AccountId) of
        {'error', _R} ->
            ResellerId = wh_services:get_reseller_id(AccountId),
            lager:debug("failed to find whitelabel for ~s, checking ~s", [AccountId, ResellerId]),
            find_port_authority(MasterAccountId, ResellerId);
      {'ok', JObj} ->
            lager:debug("using account ~s for port authority", [AccountId]),
            kz_whitelabel:port_authority(JObj)
    end.

-spec port_request_data(wh_json:object()) -> wh_proplist().
port_request_data(PortRequestJObj) ->
    PublicJObj = wh_json:public_fields(PortRequestJObj),
    wh_json:to_proplist(
      wh_json:foldl(fun port_request_data_fold/3, wh_json:new(), PublicJObj)
     ).

-spec port_request_data_fold(wh_json:key(), wh_json:json_term(), wh_json:object()) ->
                                    wh_json:object().
port_request_data_fold(<<"name">> = K, V, Acc) ->
    wh_json:set_value(K, V, Acc);
port_request_data_fold(<<"port_state">> = K, V, Acc) ->
    wh_json:set_value(K, V, Acc);
port_request_data_fold(<<"id">> = K, V, Acc) ->
    wh_json:set_value(K, V, Acc);
port_request_data_fold(<<"account_id">> = K, V, Acc) ->
    wh_json:set_value(K, V, Acc);
port_request_data_fold(<<"transfer_date">>, Date, Acc) ->
    wh_json:set_value(<<"requested_port_date">>, wh_util:iso8601({Date, {0,0,0}}), Acc);
port_request_data_fold(<<"bill_", _/binary>> = K, V, Acc) ->
    wh_json:set_value(K, V, Acc);
port_request_data_fold(<<"numbers">> = K, Numbers, Acc) ->
    wh_json:set_value(K, wh_util:join_binary(Numbers, <<", ">>), Acc);
port_request_data_fold(<<"notifications">>, NJObj, Acc) ->
    wh_json:set_value(<<"customer_contact">>, wh_json:get_value([<<"email">>, <<"send_to">>], NJObj), Acc);
port_request_data_fold(<<"carrier">>, V, Acc) ->
    wh_json:set_value(<<"service_provider">>, V, Acc);
port_request_data_fold(_K, _V, Acc) ->
    lager:debug("ignoring ~s", [_K]),
    Acc.
