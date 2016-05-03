%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz Inc
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

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"port_request_admin">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,kz_json:from_list(
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

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:port_request_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    PortReqId = kz_json:get_value(<<"port_request_id">>, DataJObj),
    {'ok', PortReqJObj} = teletype_util:open_doc(<<"port_request">>, PortReqId, DataJObj),

    ReqData = kz_json:set_value(<<"port_request">>
                                ,teletype_port_utils:fix_port_request_data(PortReqJObj)
                                ,DataJObj
                               ),

    case teletype_util:is_preview(DataJObj) of
        'false' -> handle_port_request(teletype_port_utils:fix_email(ReqData));
        'true' -> handle_port_request(kz_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec handle_port_request(kz_json:object()) -> 'ok'.
handle_port_request(DataJObj) ->
    Macros =
        props:filter_undefined(
          [{<<"system">>, teletype_util:system_params()}
           ,{<<"account">>, teletype_util:account_params(DataJObj)}
           ,{<<"port_request">>, port_request_data(kz_json:get_value(<<"port_request">>, DataJObj))}
           ,{<<"account_tree">>, account_tree(kz_json:get_value(<<"account_id">>, DataJObj))}
          ]),

    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,teletype_util:find_account_id(DataJObj)
                                             ),

    Subject =
        teletype_util:render_subject(
          kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
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

-spec account_tree(ne_binary()) -> kz_proplist().
-spec account_tree(ne_binaries(), kz_proplist()) -> kz_proplist().
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

-spec maybe_set_from(kz_json:object()) -> kz_json:object().
maybe_set_from(DataJObj) ->
    SystemFrom = kz_util:to_binary(node()),
    PortRequest = kz_json:get_value(<<"port_request">>, DataJObj),
    DefaultFrom = kz_json:get_value(<<"from">>, DataJObj, SystemFrom),

    Initiator = kz_json:get_value([<<"notifications">>, <<"email">>, <<"send_to">>], PortRequest, DefaultFrom),
    kz_json:set_value(<<"from">>, Initiator, DataJObj).

-spec maybe_set_to(kz_json:object()) -> kz_json:object().
maybe_set_to(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    case find_port_authority(MasterAccountId, AccountId) of
        'undefined' -> DataJObj;
        <<_/binary>> = To ->
            lager:debug("found port authority: ~p", [To]),
            kz_json:set_value(<<"to">>, [To], DataJObj);
        [_|_] = To ->
            lager:debug("found port authority: ~p", [To]),
            kz_json:set_value(<<"to">>, To, DataJObj)
    end.

-spec find_port_authority(ne_binary(), ne_binary()) -> api_binary() | ne_binaries().
find_port_authority(MasterAccountId, MasterAccountId) ->
    case kz_whitelabel:fetch(MasterAccountId) of
        {'error', _R} ->
            lager:debug("failed to find master account ~s, using system value", [MasterAccountId]),
            kapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>);
        {'ok', JObj} ->
            lager:debug("getting master account's port authority"),
            kz_whitelabel:port_authority(JObj)
    end;
find_port_authority(MasterAccountId, AccountId) ->
    case kz_whitelabel:fetch(AccountId) of
        {'error', _R} ->
            ResellerId = kz_services:get_reseller_id(AccountId),
            lager:debug("failed to find whitelabel for ~s, checking ~s", [AccountId, ResellerId]),
            find_port_authority(MasterAccountId, ResellerId);
      {'ok', JObj} ->
            lager:debug("using account ~s for port authority", [AccountId]),
            kz_whitelabel:port_authority(JObj)
    end.

-spec port_request_data(kz_json:object()) -> kz_proplist().
port_request_data(PortRequestJObj) ->
    PublicJObj = kz_json:public_fields(PortRequestJObj),
    kz_json:to_proplist(
      kz_json:foldl(fun port_request_data_fold/3, kz_json:new(), PublicJObj)
     ).

-spec port_request_data_fold(kz_json:key(), kz_json:json_term(), kz_json:object()) ->
                                    kz_json:object().
port_request_data_fold(<<"name">> = K, V, Acc) ->
    kz_json:set_value(K, V, Acc);
port_request_data_fold(<<"port_state">> = K, V, Acc) ->
    kz_json:set_value(K, V, Acc);
port_request_data_fold(<<"id">> = K, V, Acc) ->
    kz_json:set_value(K, V, Acc);
port_request_data_fold(<<"account_id">> = K, V, Acc) ->
    kz_json:set_value(K, V, Acc);
port_request_data_fold(<<"transfer_date">>, Date, Acc) ->
    kz_json:set_value(<<"requested_port_date">>, kz_util:iso8601({Date, {0,0,0}}), Acc);
port_request_data_fold(<<"bill_", _/binary>> = K, V, Acc) ->
    kz_json:set_value(K, V, Acc);
port_request_data_fold(<<"numbers">> = K, Numbers, Acc) ->
    kz_json:set_value(K, kz_util:join_binary(Numbers, <<", ">>), Acc);
port_request_data_fold(<<"notifications">>, NJObj, Acc) ->
    kz_json:set_value(<<"customer_contact">>, kz_json:get_value([<<"email">>, <<"send_to">>], NJObj), Acc);
port_request_data_fold(<<"carrier">>, V, Acc) ->
    kz_json:set_value(<<"service_provider">>, V, Acc);
port_request_data_fold(_K, _V, Acc) ->
    lager:debug("ignoring ~s", [_K]),
    Acc.
