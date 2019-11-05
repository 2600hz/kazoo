%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_util).

-export([send_email/3, send_email/4
        ,render_subject/2
        ,render/3
        ,system_params/0
        ,account_params/1
        ,user_params/1
        ,send_update/2, send_update/3, send_update/4
        ,find_addresses/3
        ,find_account_rep_email/1
        ,find_account_admin_email/1
        ,find_account_admin/1
        ,find_account_db/2
        ,find_account_params/1
        ,is_notice_enabled/3, is_notice_enabled_default/1
        ,should_handle_notification/1

        ,default_from_address/0
        ,default_reply_to/0

        ,template_system_value/1, template_system_value/2, template_system_value/3

        ,open_doc/3
        ,is_preview/1
        ,read_preview_doc/1
        ,fix_timestamp/1, fix_timestamp/2, fix_timestamp/3
        ,timestamp_params/3
        ,build_call_data/2

        ,public_proplist/2

        ,notification_completed/1
        ,notification_ignored/1
        ,notification_disabled/2
        ,notification_failed/2

        ,maybe_get_attachments/1
        ,fetch_attachment_from_url/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_RENDERING_ORDER, [{?TEXT_PLAIN, 3}
                                  ,{?TEXT_HTML, 2}
                                  ]).

-define(NOTICE_ENABLED_BY_DEFAULT
       ,kapps_config:get_is_true(?APP_NAME, <<"notice_enabled_by_default">>, 'true')
       ).

-spec send_email(email_map(), kz_term:ne_binary(), rendered_templates()) ->
                        'ok' | {'error', any()}.
send_email(Emails, Subject, RenderedTemplates) ->
    send_email(Emails, Subject, RenderedTemplates, []).

-spec send_email(email_map(), kz_term:ne_binary(), rendered_templates(), attachments()) ->
                        'ok' | {'error', any()}.
send_email(Emails0, Subject, RenderedTemplates, Attachments) ->
    Emails = [{Key, lists:usort(KeyEmails)} || {Key, KeyEmails} <- Emails0, KeyEmails =/= 'undefined'],
    ?LOG_DEBUG("emails: ~p", [Emails]),
    To = props:get_value(<<"to">>, Emails),
    From = props:get_value(<<"from">>, Emails),
    Email = {<<"multipart">>
            ,<<"mixed">>
            ,email_parameters([{<<"To">>, To}
                              ,{<<"Cc">>, props:get_value(<<"cc">>, Emails)}
                              ,{<<"Bcc">>, props:get_value(<<"bcc">>, Emails)}
                              ]
                             ,[{<<"From">>, From}
                              ,{<<"Reply-To">>, props:get_value(<<"reply_to">>, Emails)}
                              ,{<<"Subject">>, Subject}
                              ]
                             )
            ,[{<<"content-type-params">>, [{<<"charset">>, <<"utf-8">>}]}]
            ,[email_body(RenderedTemplates)
              | add_attachments(Attachments)
             ]
            },
    case relay_email(To, From, Email) of
        {'ok', Receipt} ->
            maybe_log_smtp(Emails, Subject, RenderedTemplates, Receipt, 'undefined');
        {'error', {'error', Reason} = E} ->
            maybe_log_smtp(Emails, Subject, RenderedTemplates, 'undefined', kz_term:to_binary(Reason)),
            E;
        {'error', Reason} = E ->
            maybe_log_smtp(Emails, Subject, RenderedTemplates, 'undefined', kz_term:to_binary(Reason)),
            E
    end.

-spec maybe_log_smtp(email_map(), kz_term:ne_binary(), list(), kz_term:api_binary(), kz_term:api_binary()) -> 'ok'.
maybe_log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error) ->
    Skip = kz_term:is_true(get('skip_smtp_log')),
    maybe_log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error, Skip).

-spec maybe_log_smtp(email_map(), kz_term:ne_binary(), list(), kz_term:api_binary(), kz_term:api_binary(), boolean()) -> 'ok'.
maybe_log_smtp(_Emails, _Subject, _RenderedTemplates, _Receipt, _Error, 'true') ->
    lager:debug("skipping smtp log");
maybe_log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error, 'false') ->
    case get('account_id') of
        'undefined' ->
            lager:debug("skipping smtp log since account_id is 'undefined'");
        AccountId ->
            log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error, AccountId)
    end.

-spec log_smtp(email_map(), kz_term:ne_binary(), list(), kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> 'ok'.
log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error, AccountId) ->
    AccountDb = kazoo_modb:get_modb(AccountId),
    Id = make_smtplog_id(AccountDb),
    TemplateId = get('template_id'),
    CallId = kz_log:get_callid(),
    Doc = kz_json:from_list(
            [{<<"rendered_templates">>, kz_json:from_list(RenderedTemplates)}
            ,{<<"subject">>, Subject}
            ,{<<"emails">>, kz_json:from_list(Emails)}
            ,{<<"receipt">>, Receipt}
            ,{<<"error">>, Error}
            ,{<<"pvt_type">>, <<"notify_smtp_log">>}
            ,{<<"account_id">>, AccountId}
            ,{<<"account_db">>, AccountDb}
            ,{<<"pvt_created">>, kz_time:now_s()}
            ,{<<"template_id">>, TemplateId}
            ,{<<"template_account_id">>, get('template_account_id')}
            ,{<<"payload_callid">>, CallId}
            ,{<<"macros">>, get('macros')}
            ,{<<"_id">>, Id}
            ]),
    lager:debug("attempting to save notify smtp log for ~s in ~s/~s", [TemplateId, AccountDb, Id]),
    _ = kazoo_modb:save_doc(AccountDb, Doc),
    'ok'.

-spec make_smtplog_id(kz_term:ne_binary()) -> kz_term:ne_binary().
make_smtplog_id(?MATCH_MODB_SUFFIX_ENCODED(_Account, Year, Month)) ->
    ?MATCH_MODB_PREFIX(Year, Month, kz_binary:rand_hex(16)).

-spec email_body(rendered_templates()) -> mimemail:mimetuple().
email_body(RenderedTemplates) ->
    {<<"multipart">>
    ,<<"alternative">>
    ,[] %% Headers
    ,[] %% ContentTypeParams
    ,add_rendered_templates_to_email(RenderedTemplates)
    }.

-spec email_parameters(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
email_parameters([], Params) ->
    lists:reverse(props:filter_empty(Params));
email_parameters([{_Key, 'undefined'}|T], Params) ->
    email_parameters(T, Params);
email_parameters([{Key, Vs}|T], Params) when is_list(Vs) ->
    email_parameters(T, [{Key, V} || V <- Vs] ++ Params);
email_parameters([{Key, V}|T], Params) ->
    email_parameters(T, [{Key, V} | Params]).

-spec relay_email(kz_term:api_binaries(), kz_term:ne_binary(), mimemail:mimetuple()) ->
                         {'ok', kz_term:ne_binary()} |
                         {'error', any()}.
relay_email(To, From, {_Type
                      ,_SubType
                      ,Addresses
                      ,_ContentTypeParams
                      ,_Body
                      }=Email
           ) ->
    try mimemail:encode(Email) of
        Encoded ->
            RelayResult = relay_encoded_email(To, From, Encoded),
            maybe_relay_to_bcc(From, Encoded, props:get_value(<<"Bcc">>, Addresses)),
            RelayResult
    catch
        'error':'missing_from' ->
            lager:warning("no From address: ~s: ~p", [From, Addresses]),
            {'error', 'missing_from'};
        ?STACKTRACE(_E, _R, ST)
        lager:warning("failed to encode email: ~s: ~p", [_E, _R]),
        kz_log:log_stacktrace(ST),
        {'error', 'email_encoding_failed'}
        end.

-spec maybe_relay_to_bcc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binaries()) -> 'ok'.
maybe_relay_to_bcc(_From, _Encoded, 'undefined') -> 'ok';
maybe_relay_to_bcc(_From, _Encoded, []) -> 'ok';
maybe_relay_to_bcc(From, Encoded, Bcc) ->
    case kapps_config:get_is_true(?APP_NAME, <<"iterate_over_bcc">>, 'true') of
        'true' -> relay_to_bcc(From, Encoded, Bcc);
        'false' -> 'ok'
    end.

-spec relay_to_bcc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries() | kz_term:ne_binary()) ->
                          {'ok', kz_term:ne_binary()} | {'error', any()}.
relay_to_bcc(From, Encoded, Bcc) when is_binary(Bcc) ->
    relay_encoded_email([Bcc], From, Encoded);
relay_to_bcc(From, Encoded, Bcc) ->
    relay_encoded_email(Bcc, From, Encoded).

-spec relay_encoded_email(kz_term:api_binaries(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                 {'ok', kz_term:ne_binary()} | {'error', any()}.
relay_encoded_email('undefined', _From, _Encoded) ->
    lager:debug("failed to send email as the TO address(es) are missing"),
    {'error', 'invalid_to_addresses'};
relay_encoded_email([], _From, _Encoded) ->
    lager:debug("failed to send email as the TO addresses list is empty"),
    {'error', 'no_to_addresses'};
relay_encoded_email(To, From, Encoded) ->
    lager:debug("relaying from ~s to ~p", [From, To]),
    handle_send(To, From, send(To, From, Encoded)).

-type gen_smtp_send_resp() :: {'ok', pid()} | {'error', any()} | pid().

-spec send(kz_term:binaries(), kz_term:ne_binary(), kz_term:ne_binary()) -> gen_smtp_send_resp().
send(To, From, Encoded) ->
    Self = self(),
    gen_smtp_client:send({From, To, Encoded}
                        ,smtp_options()
                        ,fun(X) -> Self ! {'relay_response', X} end
                        ).

-spec handle_send(kz_term:binaries(), kz_term:ne_binary(), gen_smtp_send_resp()) ->
                         {'ok', kz_term:ne_binary()} | {'error', any()}.
handle_send(To, From, {'ok', _Pid}) ->
    lager:debug("smtp client is processing with pid ~p", [_Pid]),
    wait_for_response(To, From);
handle_send(_To, _From, {'error', _R}=Error) ->
    lager:info("error trying to send email: ~p", [_R]),
    Error;
handle_send(To, From, _Pid) when is_pid(_Pid) ->
    lager:debug("smtp client is processing with pid ~p", [_Pid]),
    wait_for_response(To, From).

-spec wait_for_response(kz_term:binaries(), kz_term:ne_binary()) -> {'ok', kz_term:ne_binary()} | {'error', any()}.
wait_for_response(To, From) ->
    Timeout = kapps_config:get_pos_integer(<<"smtp_client">>, <<"send_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND),

    %% The callback will receive either `{ok, Receipt}' where Receipt is the SMTP server's receipt
    %% identifier,  `{error, Type, Message}' or `{exit, ExitReason}', as the single argument.
    receive
        {'relay_response', Resp} ->
            handle_relay_response(To, From, Resp)
    after Timeout ->
            lager:debug("timed out waiting for relay response"),
            {'error', 'timeout'}
    end.

-spec handle_relay_response(kz_term:binaries(), kz_term:ne_binary(), {'ok', kz_term:ne_binary()} | {'error', any()}) ->
                                   {'ok', kz_term:ne_binary()} | {'error', any()}.
handle_relay_response(To, From, {'ok', Receipt}) ->
    kz_cache:store_local(?CACHE_NAME
                        ,{'receipt', Receipt}
                        ,#email_receipt{to=To
                                       ,from=From
                                       ,timestamp=kz_time:now_s()
                                       ,call_id=kz_log:get_callid()
                                       }
                        ,[{'expires', ?MILLISECONDS_IN_HOUR}]
                        ),
    _ = lager:debug("relayed message: ~p", [Receipt]),
    {'ok', binary:replace(Receipt, <<"\r\n">>, <<>>, ['global'])};
handle_relay_response(_To, _From, {'error', _Type, {_SubType, _FailHost, Message}}) ->
    lager:debug("error relaying message: ~p(~p): ~p", [_Type, _SubType, Message]),
    {'error', Message};
handle_relay_response(_To, _From, {'exit', Reason}) ->
    lager:debug("failed to send email:"),
    log_email_send_error(Reason),
    {'error', Reason}.

-spec log_email_send_error(any()) -> 'ok'.
log_email_send_error({'function_clause', Stacktrace}) ->
    kz_log:log_stacktrace(Stacktrace);
log_email_send_error(Reason) ->
    lager:debug("exit relaying message: ~p", [Reason]).

-spec smtp_options() -> kz_term:proplist().
smtp_options() ->
    Relay = kapps_config:get_string(<<"smtp_client">>, <<"relay">>, "localhost"),
    Username = kapps_config:get_string(<<"smtp_client">>, <<"username">>, ""),
    Password = kapps_config:get_string(<<"smtp_client">>, <<"password">>, ""),
    Auth = kapps_config:get_binary(<<"smtp_client">>, <<"auth">>, <<"never">>),
    Port = kapps_config:get_integer(<<"smtp_client">>, <<"port">>, 25),
    Retries = kapps_config:get_integer(<<"smtp_client">>, <<"retries">>, 1),
    NoMxLookups = kapps_config:get_is_true(<<"smtp_client">>, <<"no_mx_lookups">>, 'true'),
    TLS = kapps_config:get_ne_binary(<<"smtp_client">>, <<"tls">>),
    SSL = kapps_config:get_is_true(<<"smtp_client">>, <<"use_ssl">>, 'false'),

    lager:debug("relaying via ~s", [Relay]),

    props:filter_empty(
      [{'relay', Relay}
      ,{'username', Username}
      ,{'password', Password}
      ,{'port', Port}
      ,{'auth', smtp_auth_option(Auth)}
      ,{'retries', Retries}
      ,{'no_mx_lookups', NoMxLookups}
      ,{'tls', smtp_tls_option(TLS)}
      ,{'ssl', SSL}
      ]).

-spec smtp_auth_option(kz_term:ne_binary()) -> atom().
smtp_auth_option(<<"if_available">>) -> 'if_available';
smtp_auth_option(<<"always">>) -> 'always';
smtp_auth_option(_) -> 'never'.

-spec smtp_tls_option(kz_term:ne_binary()) -> atom().
smtp_tls_option(<<"if_available">>) -> 'if_available';
smtp_tls_option(<<"always">>) -> 'always';
smtp_tls_option(_) -> 'never'.

-spec add_attachments(attachments()) -> mime_tuples().
add_attachments(Attachments) ->
    add_attachments(Attachments, []).

-spec add_attachments(attachments(), mime_tuples()) -> mime_tuples().
add_attachments([], Acc) -> Acc;
add_attachments([{ContentType, Filename, Content}|As], Acc) ->
    [Type, SubType] = binary:split(ContentType, <<"/">>),

    Attachment = {Type
                 ,SubType
                 ,[{<<"Content-Disposition">>, <<"attachment; filename=\"", Filename/binary, "\"">>}
                  ,{<<"Content-Type">>, <<ContentType/binary, "; name=\"", Filename/binary, "\"">>}
                  ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
                  ]
                 ,[]
                 ,Content
                 },
    lager:debug("adding attachment ~s (~s)", [Filename, ContentType]),
    add_attachments(As, [Attachment | Acc]).

-spec add_rendered_templates_to_email(rendered_templates()) -> mime_tuples().
add_rendered_templates_to_email(RenderedTemplates) ->
    add_rendered_templates_to_email(sort_templates(RenderedTemplates), []).

-spec add_rendered_templates_to_email(rendered_templates(), mime_tuples()) -> mime_tuples().
add_rendered_templates_to_email([], Acc) -> Acc;
add_rendered_templates_to_email([{ContentType, Content}|Rs], Acc) ->
    [Type, SubType] = binary:split(ContentType, <<"/">>),
    CTEncoding = kapps_config:get_ne_binary(?NOTIFY_CONFIG_CAT
                                           ,[<<"mime-encoding">>
                                            ,ContentType
                                            ,<<"content_transfer_encoding">>
                                            ]
                                           ,default_content_transfer_encoding(ContentType)
                                           ),
    Template = {Type
               ,SubType
               ,props:filter_undefined(
                  [{<<"Content-Type">>, iolist_to_binary([ContentType, <<";charset=utf-8">>])}
                  ,{<<"Content-Transfer-Encoding">>, CTEncoding}
                  ])
               ,[]
               ,iolist_to_binary(Content)
               },
    lager:debug("adding template ~s (encoding ~s)", [ContentType, CTEncoding]),
    add_rendered_templates_to_email(Rs, [Template | Acc]).

-spec default_content_transfer_encoding(binary()) -> binary().
default_content_transfer_encoding(<<"text/html">>) -> <<"base64">>;
default_content_transfer_encoding(_) -> <<"7BIT">>.

-spec system_params() -> kz_term:proplist().
system_params() ->
    [{<<"hostname">>, kz_term:to_binary(net_adm:localhost())}
    ,{<<"encoded_hostname">>, kz_base64url:encode(crypto:hash('md5', kz_term:to_binary(net_adm:localhost())))}
    ,{<<"node">>, node()}
    ,{<<"encoded_node">>, kz_nodes:node_encoded()}
    ].

-spec user_params(kzd_users:doc()) -> kz_term:proplist().
user_params(UserJObj) ->
    Ks = [{<<"first_name">>, fun kzd_users:first_name/1}
         ,{<<"last_name">>, fun kzd_users:last_name/1}
         ,{<<"email">>, fun kzd_users:email/1}
         ,{<<"timezone">>, fun kzd_users:timezone/1}
         ],
    props:filter_undefined(
      [{Key, Fun(UserJObj)} || {Key, Fun} <- Ks]
     ).

-spec account_params(kz_json:object()) -> kz_term:proplist().
account_params(DataJObj) ->
    case kapi_notifications:account_id(DataJObj) of
        'undefined' ->
            put('account_id', 'undefined'),
            [];
        AccountId ->
            put('account_id', AccountId),
            find_account_params(AccountId)
    end.

-spec find_account_params(kz_term:api_binary()) -> kz_term:proplist().
find_account_params('undefined') -> [];
find_account_params(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', AccountJObj} ->
            props:filter_undefined(
              [{<<"name">>, kzd_accounts:name(AccountJObj)}
              ,{<<"realm">>, kzd_accounts:realm(AccountJObj)}
              ,{<<"id">>, kz_doc:id(AccountJObj)}
              ,{<<"language">>, kzd_accounts:language(AccountJObj)}
              ,{<<"timezone">>, kzd_accounts:timezone(AccountJObj)}
               | maybe_add_parent_params(AccountId, AccountJObj)
              ]);
        {'error', _E} ->
            ?LOG_DEBUG("failed to find account doc for ~s: ~p", [AccountId, _E]),
            []
    end.

-spec maybe_add_parent_params(kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
maybe_add_parent_params(AccountId, AccountJObj) ->
    case kzd_accounts:parent_account_id(AccountJObj) of
        'undefined' -> [];
        AccountId -> [];
        ParentAccountId ->
            {'ok', ParentAccountJObj} = kzd_accounts:fetch(ParentAccountId),
            [{<<"parent_name">>, kzd_accounts:name(ParentAccountJObj)}
            ,{<<"parent_realm">>, kzd_accounts:realm(ParentAccountJObj)}
            ,{<<"parent_id">>, kz_doc:id(ParentAccountJObj)}
            ]
    end.

-spec default_from_address() -> kz_term:ne_binary().
default_from_address() ->
    list_to_binary([<<"no_reply@">>, net_adm:localhost()]).

-spec default_reply_to() -> kz_term:api_ne_binary().
default_reply_to() -> 'undefined'.

-spec render_subject(kz_term:ne_binary(), kz_term:proplist()) -> binary().
render_subject(Template, Macros) ->
    render(<<"subject">>, Template, Macros).

-spec render(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> binary().
render(TemplateId, Template, Macros) ->
    case teletype_renderer:render(TemplateId, Template, Macros) of
        {'ok', IOData} -> iolist_to_binary(IOData);
        {'error', _E} ->
            lager:debug("failed to render '~s': ~p '~s'", [TemplateId, _E, Template]),
            throw({'error', 'template_error', <<"failed to render template ", TemplateId/binary>>})
    end.

-spec sort_templates(rendered_templates()) -> rendered_templates().
sort_templates(RenderedTemplates) ->
    lists:sort(fun sort_templates/2, RenderedTemplates).

-spec sort_templates({kz_term:ne_binary(), any()}, {kz_term:ne_binary(), any()}) -> boolean().
sort_templates({K1, _}, {K2, _}) ->
    props:get_value(K1, ?TEMPLATE_RENDERING_ORDER, 1) =<
        props:get_value(K2, ?TEMPLATE_RENDERING_ORDER, 1).

-spec find_account_db(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
find_account_db(<<"account">>, JObj) -> kapi_notifications:account_db(JObj, 'false');
find_account_db(<<"user">>, JObj) -> kapi_notifications:account_db(JObj, 'false');
find_account_db(<<"faxbox">>, JObj) -> kapi_notifications:account_db(JObj, 'false');
find_account_db(<<"fax">>, JObj) -> kapi_notifications:account_db(JObj, 'true');
find_account_db(<<"port_request">>, _JObj) -> ?KZ_PORT_REQUESTS_DB;
find_account_db(<<"webhook">>, _JObj) -> ?KZ_WEBHOOKS_DB;
find_account_db(<<"function">>, _JObj) -> ?KZ_FUNCTIONS_DB;
find_account_db(_, JObj) -> kapi_notifications:account_db(JObj, 'false').

-spec send_update(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_update(DataJObj, Status) ->
    send_update(DataJObj, Status, 'undefined').

-spec send_update(kz_json:object(), kz_term:ne_binary(), kz_term:api_binary()) -> 'ok'.
send_update(DataJObj, Status, Message) ->
    send_update(DataJObj, Status, Message, 'undefined').

-spec send_update(kz_json:object(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_object()) -> 'ok'.
send_update(DataJObj, Status, Message, Metadata) ->
    send_update(kz_json:get_first_defined([<<"server_id">>, <<"Server-ID">>], DataJObj)
               ,kz_json:get_first_defined([<<"msg_id">>, <<"Msg-ID">>], DataJObj)
               ,Status
               ,Message
               ,Metadata
               ).

-spec send_update(kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_object()) -> 'ok'.
send_update('undefined', _, _, _, _) ->
    ?LOG_DEBUG("no response queue available, not publishing update");
send_update(RespQ, MsgId, Status, Msg, Metadata) ->
    Prop = props:filter_undefined(
             [{<<"Status">>, Status}
             ,{<<"Failure-Message">>, Msg}
             ,{<<"Msg-ID">>, MsgId}
             ,{<<"Metadata">>, Metadata}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    %% ?LOG_DEBUG("notification update (~s) sending to ~s", [Status, RespQ]),
    lager:debug("notification update (~s) sending to ~s", [Status, RespQ]),
    kz_amqp_worker:cast(Prop, fun(P) -> kapi_notifications:publish_notify_update(RespQ, P) end).

-spec find_account_rep_email(kz_term:api_object() | kz_term:ne_binary()) -> kz_term:api_binaries().
find_account_rep_email('undefined') -> 'undefined';
find_account_rep_email(?NE_BINARY=AccountId) ->
    case kz_services_reseller:is_reseller(AccountId) of
        'true' ->
            lager:debug("finding admin email for reseller account ~s", [AccountId]),
            find_account_admin_email(AccountId);
        'false' ->
            lager:debug("finding admin email for reseller of account ~s", [AccountId]),
            find_account_admin_email(kz_services_reseller:get_id(AccountId))
    end;
find_account_rep_email(AccountJObj) ->
    find_account_rep_email(kapi_notifications:account_id(AccountJObj)).

-spec find_account_admin_email(kz_term:api_binary()) -> kz_term:api_binaries().
find_account_admin_email('undefined') -> 'undefined';
find_account_admin_email(AccountId) ->
    find_account_admin_email(AccountId, kz_services_reseller:get_id(AccountId)).

-spec find_account_admin_email(kz_term:api_binary(), kz_term:api_binary()) -> kz_term:api_binaries().
find_account_admin_email('undefined', _Id) ->
    'undefined';
find_account_admin_email(AccountId, AccountId) ->
    case query_account_for_admin_emails(AccountId) of
        [] -> 'undefined';
        Emails -> Emails
    end;
find_account_admin_email(AccountId, ResellerId) ->
    case query_account_for_admin_emails(AccountId) of
        [] -> find_account_admin_email(kzd_accounts:get_parent_account_id(AccountId), ResellerId);
        Emails -> Emails
    end.

-spec query_account_for_admin_emails(kz_term:ne_binary()) -> kz_term:ne_binaries().
query_account_for_admin_emails(<<_/binary>> = AccountId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    ViewOptions = [{'key', <<"user">>}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> [];
        {'ok', Users} -> extract_admin_emails(Users);
        {'error', _E} ->
            ?LOG_DEBUG("failed to find users in ~s: ~p", [AccountId, _E]),
            []
    end.

-spec extract_admin_emails(kzd_users:docs()) -> kz_term:ne_binaries().
extract_admin_emails(Users) ->
    [Email
     || Admin <- filter_for_admins(Users),
        (Email = kzd_users:email(Admin)) =/= 'undefined'
    ].

-spec find_account_admin(kz_term:api_binary()) -> kz_term:api_object().
find_account_admin('undefined') -> 'undefined';
find_account_admin(?MATCH_ACCOUNT_RAW(AccountId)) ->
    find_account_admin(AccountId, kz_services_reseller:get_id(AccountId)).

-spec find_account_admin(kz_term:ne_binary(), kz_term:ne_binary()) -> 'undefined' | kzd_users:doc().
find_account_admin(AccountId, AccountId) ->
    query_for_account_admin(AccountId);
find_account_admin(AccountId, ResellerId) ->
    case query_for_account_admin(AccountId) of
        'undefined' -> find_account_admin(ResellerId);
        Admin -> Admin
    end.

-spec query_for_account_admin(kz_term:ne_binary()) -> 'undefined' | kzd_users:doc().
query_for_account_admin(AccountId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    ViewOptions = [{'key', <<"user">>}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> 'undefined';
        {'ok', Users} ->
            case filter_for_admins(Users) of
                [] -> 'undefined';
                [Admin|_] -> Admin
            end;
        {'error', _E} ->
            ?LOG_DEBUG("failed to find users in ~s: ~p", [AccountId, _E]),
            'undefined'
    end.

-spec filter_for_admins(kz_json:objects()) -> kzd_users:docs().
filter_for_admins(Users) ->
    [Doc
     || User <- Users,
        Doc <- [kz_json:get_value(<<"doc">>, User)],
        kzd_users:is_account_admin(Doc)
    ].

-spec should_handle_notification(kz_json:object()) -> boolean().
should_handle_notification(JObj) ->
    DataJObj = kz_json:normalize(JObj),
    should_handle_notification(DataJObj, is_preview(JObj)).

-spec should_handle_notification(kz_json:object(), boolean()) -> boolean().
should_handle_notification(_JObj, 'true') ->
    lager:debug("notification is a preview, handling"),
    'true';

should_handle_notification(JObj, 'false') ->
    Account = kapi_notifications:account_id(JObj),

    Config = kzd_accounts:get_inherited_value(Account
                                             ,fun kzd_accounts:notification_preference/1
                                             ,kapps_config:get_ne_binary(?NOTIFY_CONFIG_CAT, <<"notification_app">>, ?APP_NAME)
                                             ),

    lager:debug("notification configuration is: ~p", [Config]),
    Config =:= ?APP_NAME.

-spec is_notice_enabled(kz_term:api_binary(), kz_json:object(), kz_term:ne_binary()) -> boolean().
is_notice_enabled('undefined', _ApiJObj, TemplateKey) ->
    is_notice_enabled_default(TemplateKey);
is_notice_enabled(AccountId, ApiJObj, TemplateKey) ->
    case kz_json:is_true(<<"Preview">>, ApiJObj, 'false') of
        'true' -> 'true';
        'false' ->
            ResellerAccountId = kz_services_reseller:get_id(AccountId),
            is_account_notice_enabled(AccountId, TemplateKey, ResellerAccountId)
    end.

-spec is_account_notice_enabled(kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_account_notice_enabled('undefined', TemplateKey, _ResellerAccountId) ->
    lager:debug("no account id to check, checking system config for ~s", [TemplateKey]),
    is_notice_enabled_default(TemplateKey);
is_account_notice_enabled(AccountId, TemplateKey, ResellerAccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    TemplateId = teletype_templates:doc_id(TemplateKey),

    case kz_datamgr:open_cache_doc(AccountDb, TemplateId) of
        {'ok', TemplateJObj} ->
            lager:debug("account ~s has ~s, checking if enabled", [AccountId, TemplateId]),
            kz_notification:is_enabled(TemplateJObj, ?NOTICE_ENABLED_BY_DEFAULT);
        _Otherwise when AccountId =/= ResellerAccountId ->
            lager:debug("account ~s is mute, checking parent", [AccountId]),
            is_account_notice_enabled(kzd_accounts:get_parent_account_id(AccountId)
                                     ,TemplateId
                                     ,ResellerAccountId
                                     );
        _Otherwise ->
            is_notice_enabled_default(TemplateKey)
    end.

-spec is_notice_enabled_default(kz_term:ne_binary()) -> boolean().
is_notice_enabled_default(TemplateKey) ->
    TemplateId = teletype_templates:doc_id(TemplateKey),
    case kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, TemplateId) of
        {'ok', TemplateJObj} ->
            lager:debug("system has ~s, checking if enabled", [TemplateId]),
            kz_notification:is_enabled(TemplateJObj, ?NOTICE_ENABLED_BY_DEFAULT);
        _Otherwise ->
            lager:debug("system is mute, ~s not enabled", [TemplateId]),
            'false'
    end.

-spec find_addresses(kz_json:object(), kz_json:object(), kz_term:ne_binary()) ->
                            email_map().
find_addresses(DataJObj, TemplateMetaJObj, ConfigCat) ->
    AddressKeys = [<<"to">>, <<"cc">>, <<"bcc">>, <<"from">>, <<"reply_to">>],
    find_addresses(DataJObj, TemplateMetaJObj, ConfigCat, AddressKeys, []).

-spec find_addresses(kz_json:object(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binaries(), email_map()) ->
                            email_map().
find_addresses(_DataJObj, _TemplateMetaJObj, _ConfigCat, [], Acc) -> Acc;
find_addresses(DataJObj, TemplateMetaJObj, ConfigCat, [Key|Keys], Acc) ->
    find_addresses(DataJObj
                  ,TemplateMetaJObj
                  ,ConfigCat
                  ,Keys
                  ,[find_address(DataJObj, TemplateMetaJObj, ConfigCat, Key)|Acc]
                  ).

-spec find_address(kz_json:object(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                          email_pair().
find_address(DataJObj, TemplateMetaJObj, ConfigCat, Key) ->
    find_address(DataJObj
                ,TemplateMetaJObj
                ,ConfigCat
                ,Key
                ,kz_json:find([Key, <<"type">>]
                             ,[DataJObj, TemplateMetaJObj]
                             )
                ,is_preview(DataJObj)
                ).

-spec find_address(kz_json:object(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), boolean()) ->
                          email_pair().
find_address(DataJObj, TemplateMetaJObj, _ConfigCat, Key, 'undefined', _) ->
    lager:debug("email type for '~s' not defined in template, checking just the key", [Key]),
    {Key, find_first_defined_address(Key, [Key], [DataJObj, TemplateMetaJObj])};
find_address(DataJObj, _TemplateMetaJObj, _ConfigCat, <<"to">> = Key, ?EMAIL_SPECIFIED, 'true') ->
    lager:debug("notification is preview, checking data only for '~s' email addresses", [Key]),
    {Key, find_first_defined_address(Key, [[Key, <<"email_addresses">>], Key], [DataJObj])};
find_address(DataJObj, TemplateMetaJObj, _ConfigCat, Key, ?EMAIL_SPECIFIED, _) ->
    lager:debug("checking template for '~s' email addresses", [Key]),
    {Key, find_first_defined_address(Key, [[Key, <<"email_addresses">>], Key], [TemplateMetaJObj, DataJObj])};
find_address(DataJObj, TemplateMetaJObj, _ConfigCat, Key, ?EMAIL_ORIGINAL, _) ->
    lager:debug("checking data for '~s' email address(es)", [Key]),
    {Key, find_first_defined_address(Key, [Key, [Key, <<"email_addresses">>]], [DataJObj, TemplateMetaJObj])};
find_address(DataJObj, _TemplateMetaJObj, ConfigCat, Key, ?EMAIL_ADMINS, _) ->
    lager:debug("looking for admin emails for '~s'", [Key]),
    {Key, find_admin_emails(DataJObj, ConfigCat, Key)}.

-spec find_first_defined_address(kz_term:ne_binary(), [kz_json:get_key()], kz_json:objects()) -> kz_term:api_ne_binaries().
find_first_defined_address(_Key, [], _JObjs) -> 'undefined';
find_first_defined_address(Key, [Path|Paths], JObjs) ->
    case get_address_value(Key, Path, JObjs) of
        'undefined' -> find_first_defined_address(Key, Paths, JObjs);
        Emails -> Emails
    end.

-spec get_address_value(kz_term:ne_binary(), kz_json:get_key(), kz_json:objects()) ->
                               kz_term:api_ne_binaries().
get_address_value(_Key, _Path, []) -> 'undefined';
get_address_value(Key, Path, [JObj|JObjs]) ->
    Email0 = kz_json:get_value(Key, JObj),
    case check_address_value(Email0) of
        'undefined' ->  get_address_value(Key, Path, JObjs);
        Emails -> Emails
    end.

-spec check_address_value(binary() | kz_term:binaries() | kz_term:api_object()) ->
                                 kz_term:api_ne_binaries().
check_address_value('undefined') -> 'undefined';
check_address_value(<<>>) -> 'undefined';
check_address_value(<<_/binary>> = Email) -> check_address_value([Email]);
check_address_value(Emails) when is_list(Emails) ->
    case [E || E <- Emails,
               kz_term:is_ne_binary(E),
               length(binary:split(E, <<"@">>, ['global'])) == 2
         ]
    of
        [] -> 'undefined';
        Es -> Es
    end;
check_address_value(Emails) ->
    case kz_json:is_json_object(Emails) of
        'true' -> check_address_value(kz_json:get_value(<<"email_addresses">>, Emails));
        'false' -> 'undefined'
    end.

-spec find_admin_emails(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                               kz_term:api_ne_binaries().
find_admin_emails(DataJObj, ConfigCat, Key) ->
    case find_account_rep_email(kapi_notifications:account_id(DataJObj)) of
        'undefined' ->
            ?LOG_DEBUG("didn't find account rep for '~s'", [Key]),
            admin_emails_from_system_template(ConfigCat, Key);
        Emails -> Emails
    end.

-spec admin_emails_from_system_template(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                               kz_term:api_ne_binaries().
admin_emails_from_system_template(ConfigCat, Key) ->
    case kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, ConfigCat) of
        {'ok', JObj} -> admin_emails_from_system_template(ConfigCat, Key, JObj);
        {'error', _} -> 'undefined'
    end.

-spec admin_emails_from_system_template(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                                               kz_term:api_ne_binaries().
admin_emails_from_system_template(ConfigCat, Key, JObj) ->
    case check_address_value(kz_json:get_ne_value([<<"default">>, <<"default_", Key/binary>>], JObj)) of
        'undefined' ->
            case check_address_value(kz_json:get_ne_value(Key, JObj)) of
                'undefined' ->
                    ?LOG_DEBUG("no default in ~s for default_~s", [ConfigCat, Key]),
                    'undefined';
                Emails -> Emails
            end;
        Emails -> Emails
    end.

-spec template_system_value(kz_term:ne_binary()) -> kz_json:object().
template_system_value(TemplateId) ->
    ConfigCat = teletype_templates:doc_id(TemplateId),
    case kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, ConfigCat) of
        {'ok', JObj} -> JObj;
        {'error', _} -> kz_json:new()
    end.

-spec template_system_value(kz_term:ne_binary(), kz_json:get_key()) -> any().
template_system_value(TemplateId, Key) ->
    template_system_value(TemplateId, Key, 'undefined').

-spec template_system_value(kz_term:ne_binary(), kz_json:get_key(), any()) -> any().
template_system_value(TemplateId, Key, Default) ->
    kz_json:get_first_defined([Key, lists:flatten([<<"default">>, Key])]
                             ,template_system_value(TemplateId)
                             ,Default
                             ).

-spec open_doc(kz_term:ne_binary(), kz_term:api_binary(), kz_json:object()) ->
                      {'ok', kz_json:object()} |
                      {'error', any()}.
open_doc(Type, 'undefined', DataJObj) ->
    maybe_load_preview(Type, {'error', 'empty_doc_id'}, is_preview(DataJObj));
open_doc(Type, DocId, DataJObj) ->
    AccountDb = find_account_db(Type, DataJObj),
    case kz_datamgr:open_cache_doc(AccountDb, {Type, DocId}) of
        {'ok', _JObj}=OK -> OK;
        {'error', _E}=Error ->
            maybe_load_preview(Type, Error, is_preview(DataJObj))
    end.

-type read_file_error() :: file:posix() | 'badarg' | 'terminated' | 'system_limit'.

-spec maybe_load_preview(kz_term:ne_binary(), E, boolean()) ->
                                {'ok', kz_json:object()} |
                                {'error', read_file_error()} | E.
maybe_load_preview(_Type, Error, 'false') ->
    Error;
maybe_load_preview(Type, _Error, 'true') ->
    read_preview_doc(Type).

-spec read_preview_doc(kz_term:ne_binary()) ->
                              {'ok', kz_json:object()} |
                              {'error', read_file_error()}.
read_preview_doc(File) ->
    PreviewFile = filename:join([code:priv_dir(?APP), "preview_data", <<File/binary,".json">>]),
    case file:read_file(PreviewFile) of
        {'ok', JSON} ->
            lager:debug("read preview data from ~s: ~s", [PreviewFile, JSON]),
            {'ok', kz_json:decode(JSON)};
        {'error', _Reason}=E ->
            lager:debug("failed to read preview data from ~s: ~p", [PreviewFile, _Reason]),
            E
    end.

-spec is_preview(kz_json:object()) -> boolean().
is_preview(DataJObj) ->
    kz_term:is_true(
      kz_json:get_first_defined([<<"Preview">>, <<"preview">>], DataJObj, 'false')
     ).

-spec timestamp_params(kz_time:gregorian_seconds(), kz_term:ne_binary(), string()) -> kz_term:proplist().
timestamp_params(Timestamp, <<Timezone/binary>>, ClockTimezone) when is_integer(Timestamp) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Timestamp),
    lager:debug("using tz ~s (system ~s) for ~p", [Timezone, ClockTimezone, DateTime]),

    props:filter_undefined(
      [{<<"utc">>, localtime:local_to_utc(DateTime, kz_term:to_list(ClockTimezone))}
      ,{<<"local">>, localtime:local_to_local(DateTime, kz_term:to_list(ClockTimezone), kz_term:to_list(Timezone))}
      ,{<<"timestamp">>, Timestamp}
      ,{<<"timezone">>, Timezone}
      ]).

%% make timestamp ready to process by "date" filter in ErlyDTL
%% returns a prop list with local, UTC time and timezone
-spec fix_timestamp(kz_time:gregorian_seconds() | kz_term:api_ne_binary()) -> kz_term:proplist().
fix_timestamp(Timestamp) ->
    fix_timestamp(Timestamp, <<"UTC">>).

-spec fix_timestamp(kz_time:gregorian_seconds() | kz_term:api_ne_binary(), kz_term:api_binary() | kz_json:object()) -> kz_term:proplist().
fix_timestamp('undefined', Thing) ->
    fix_timestamp(kz_time:now_s(), Thing);
fix_timestamp(<<Timestamp/binary>>, Thing) ->
    fix_timestamp(kz_term:to_integer(Timestamp), Thing);
fix_timestamp(Timestamp, <<TZ/binary>>) when is_integer(Timestamp) ->
    ClockTimezone = kapps_config:get_string(<<"servers">>, <<"clock_timezone">>, "UTC"),
    timestamp_params(Timestamp, TZ, ClockTimezone);
fix_timestamp(Timestamp, 'undefined') ->
    fix_timestamp(Timestamp, <<"UTC">>);
fix_timestamp(Timestamp, DataJObj) ->
    Params = account_params(DataJObj),
    TZ = props:get_ne_binary_value(<<"timezone">>, Params),
    fix_timestamp(Timestamp, TZ).

-spec fix_timestamp(kz_time:gregorian_seconds() | kz_term:api_ne_binary(), kz_json:object(), kz_term:api_ne_binary()) -> kz_term:proplist().
fix_timestamp(Timestamp, DataJObj, 'undefined') ->
    fix_timestamp(Timestamp, DataJObj);
fix_timestamp(Timestamp, _DataJObj, TZ) ->
    fix_timestamp(Timestamp, TZ).

-spec build_call_data(kz_json:object(), kz_term:api_ne_binary()) -> kz_term:proplist().
build_call_data(DataJObj, Timezone) ->
    props:filter_empty(
      [{<<"caller_id">>, build_caller_id_data(DataJObj)}
      ,{<<"callee_id">>, build_callee_id_data(DataJObj)}
      ,{<<"date_called">>, build_date_called_data(DataJObj, Timezone)}
      ,{<<"from">>, build_from_data(DataJObj)}
      ,{<<"to">>, build_to_data(DataJObj)}
      ,{<<"call_id">>, kz_json:get_value(<<"call_id">>, DataJObj)}
      ]).

-spec build_caller_id_data(kz_json:object()) -> kz_term:proplist().
build_caller_id_data(DataJObj) ->
    Name = knm_util:pretty_print(kz_json:get_ne_binary_value(<<"caller_id_name">>, DataJObj)),
    Number = knm_util:pretty_print(kz_json:get_ne_binary_value(<<"caller_id_number">>, DataJObj)),
    NameNumber = build_name_and_number(Name, Number),
    props:filter_undefined(
      [{<<"name">>, Name}
      ,{<<"number">>, Number}
      ,{<<"name_number">>, NameNumber}
      ]).

-spec build_callee_id_data(kz_json:object()) -> kz_term:proplist().
build_callee_id_data(DataJObj) ->
    Name = knm_util:pretty_print(kz_json:get_ne_binary_value(<<"callee_id_name">>, DataJObj)),
    Number = knm_util:pretty_print(kz_json:get_ne_binary_value(<<"callee_id_number">>, DataJObj)),
    NameNumber = build_name_and_number(Name, Number),
    props:filter_undefined(
      [{<<"name">>, Name}
      ,{<<"number">>, Number}
      ,{<<"name_number">>, NameNumber}
      ]).

-spec build_name_and_number(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
build_name_and_number(<<"unknown">>, <<"unknown">>) ->
    'undefined';
build_name_and_number(<<"unknown">>, Number) ->
    Number;
build_name_and_number(Name, <<"unknown">>) ->
    Name;
build_name_and_number(Number, Number) ->
    Number;
build_name_and_number(Name, Number) ->
    <<Name/binary, "(", Number/binary, ")">>.

-spec build_date_called_data(kz_json:object(), kz_term:api_ne_binary()) -> kz_term:proplist().
build_date_called_data(DataJObj, Timezone) ->
    DateCalled = find_date_called(DataJObj),
    Timezone = kz_json:get_ne_binary_value(<<"timezone">>, DataJObj, Timezone),
    fix_timestamp(DateCalled, DataJObj, Timezone).

-spec find_date_called(kz_json:object()) -> kz_time:gregorian_seconds().
find_date_called(DataJObj) ->
    Paths = [<<"voicemail_timestamp">>
            ,<<"fax_timestamp">>
            ,<<"timestamp">>
            ],
    Timestamp = kz_json:get_first_defined(Paths, DataJObj, kz_time:now_s()),
    try kz_term:to_integer(Timestamp)
    catch _:_ -> kz_time:now_s()
    end.

-spec build_from_data(kz_json:object()) -> kz_term:proplist().
build_from_data(DataJObj) ->
    FromE164 = kz_json:get_first_defined([<<"from_user">>, <<"caller_id_number">>], DataJObj),
    props:filter_undefined(
      [{<<"user">>, knm_util:pretty_print(FromE164)}
      ,{<<"realm">>, kz_json:get_ne_binary_value(<<"from_realm">>, DataJObj)}
      ]).

-spec build_to_data(kz_json:object()) -> kz_term:proplist().
build_to_data(DataJObj) ->
    ToE164 = kz_json:get_first_defined([<<"to_user">>, <<"callee_id_number">>], DataJObj),
    props:filter_undefined(
      [{<<"user">>, knm_util:pretty_print(ToE164)}
      ,{<<"realm">>, kz_json:get_ne_binary_value(<<"to_realm">>, DataJObj)}
      ]).

-spec public_proplist(kz_json:get_key(), kz_json:object()) -> kz_term:proplist().
public_proplist(Key, JObj) ->
    kz_json:recursive_to_proplist(
      kz_doc:public_fields(kz_json:get_value(Key, JObj, kz_json:new()))
     ).

-spec notification_completed(kz_term:ne_binary()) -> template_response().
notification_completed(TemplateId) -> {'completed', TemplateId}.

-spec notification_ignored(kz_term:ne_binary()) -> template_response().
notification_ignored(TemplateId) -> {'ignored', TemplateId}.

-spec notification_failed(kz_term:ne_binary(), any()) -> template_response().
notification_failed(TemplateId, Reason) -> {'failed', Reason, TemplateId}.

-spec notification_disabled(kz_json:object(), kz_term:ne_binary()) -> template_response().
notification_disabled(DataJObj, TemplateId) ->
    AccountId = kapi_notifications:account_id(DataJObj),
    lager:debug("notification ~s is disabled for account ~s", [TemplateId, AccountId]),
    {'disabled', TemplateId}.

-spec maybe_get_attachments(kz_json:object() | kz_term:api_binary()) -> attachments().
maybe_get_attachments('undefined') -> [];
maybe_get_attachments(URL)
  when is_binary(URL) ->
    case fetch_attachment_from_url(URL) of
        {'ok', Attachment} -> [Attachment];
        {'error', _} -> []
    end;
maybe_get_attachments(DataObj) ->
    maybe_get_attachments(kz_json:get_value(<<"attachment_url">>, DataObj)).

-spec fetch_attachment_from_url(kz_term:ne_binary()) -> {'ok', attachment()} | {'error', any()}.
fetch_attachment_from_url(URL) ->
    case kz_http:get(kz_term:to_list(URL)) of
        {'ok', _2xx, Headers, Body}
          when (_2xx - 200) < 100 -> %% ie: match "2"++_
            {'ok', attachment_from_url_result(Headers, Body)};
        {'ok', _Code, _Headers, _Body} ->
            lager:debug("failed to get attachment from url ~s for reason: ~p : ~p", [URL, _Code, _Headers]),
            {'error', 'not_handled'};
        Error ->
            lager:debug("failed to get attachment from url ~s for reason: ~p", [URL, Error]),
            Error
    end.

-spec attachment_from_url_result(kz_term:proplist(), binary()) -> attachment().
attachment_from_url_result(Headers, Body) ->
    CT = kz_term:to_binary(props:get_value("content-type", Headers, <<"text/plain">>)),
    Disposition = kz_term:to_binary(props:get_value("content-disposition", Headers, <<>>)),
    CDs = [list_to_tuple(binary:split(kz_binary:strip(CD), <<"=">>))
           || CD <- binary:split(Disposition, <<";">>)
          ],
    Filename = case props:get_value(<<"filename">>, CDs) of
                   'undefined' -> kz_mime:to_filename(CT);
                   FileDisposition -> FileDisposition
               end,
    {CT, Filename, Body}.
