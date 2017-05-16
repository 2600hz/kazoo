%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_port_request_admin).

-export([init/0
        ,handle_req/1
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

-define(TEMPLATE_SUBJECT, <<"Number port request for account '{{account.name}}' (Details)">>).
-define(TEMPLATE_CATEGORY, <<"system">>).
-define(TEMPLATE_NAME, <<"Admin Port Request">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
                                          ,{'subject', ?TEMPLATE_SUBJECT}
                                          ,{'category', ?TEMPLATE_CATEGORY}
                                          ,{'friendly_name', ?TEMPLATE_NAME}
                                          ,{'to', ?TEMPLATE_TO}
                                          ,{'from', ?TEMPLATE_FROM}
                                          ,{'cc', ?TEMPLATE_CC}
                                          ,{'bcc', ?TEMPLATE_BCC}
                                          ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                          ]),
    teletype_bindings:bind(<<"port_request">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(JObj) ->
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
          ,{<<"port_request">>, teletype_util:public_proplist(<<"port_request">>, DataJObj)}
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
    SystemFrom = kz_term:to_binary(node()),
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
            kapps_config:get_ne_binary_or_ne_binaries(?MOD_CONFIG_CAT, <<"default_to">>);
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
