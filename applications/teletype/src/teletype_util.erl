%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_util).

-export([send_email/3, send_email/4
        ,render_subject/2
        ,render/3
        ,system_params/0
        ,account_params/1
        ,user_params/1
        ,send_update/2, send_update/3
        ,find_addresses/3
        ,find_account_rep_email/1
        ,find_account_admin_email/1
        ,find_account_admin/1
        ,find_account_id/1
        ,find_account_db/1, find_account_db/2
        ,is_notice_enabled/3, is_notice_enabled_default/1
        ,should_handle_notification/1

        ,get_parent_account_id/1

        ,default_from_address/1
        ,default_reply_to/1

        ,open_doc/3
        ,is_preview/1

        ,public_proplist/2

        ,stop_processing/2

        ,maybe_get_attachments/1
        ,fetch_attachment_from_url/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_RENDERING_ORDER, [{?TEXT_PLAIN, 3}
                                  ,{?TEXT_HTML, 2}
                                  ]).

-spec send_email(email_map(), ne_binary(), rendered_templates()) ->
                        'ok' | {'error', any()}.
-spec send_email(email_map(), ne_binary(), rendered_templates(), attachments()) ->
                        'ok' | {'error', any()}.
send_email(Emails, Subject, RenderedTemplates) ->
    send_email(Emails, Subject, RenderedTemplates, []).
send_email(Emails, Subject, RenderedTemplates, Attachments) ->
    lager:debug("emails: ~p", [Emails]),
    To = props:get_value(<<"to">>, Emails),
    From = props:get_value(<<"from">>, Emails),
    Email = {<<"multipart">>
            ,<<"mixed">>
            ,email_parameters(
               [{<<"To">>, To}
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
            maybe_log_smtp(Emails, Subject, RenderedTemplates, 'undefined', kz_util:to_binary(Reason)),
            E;
        {'error', Reason} = E ->
            maybe_log_smtp(Emails, Subject, RenderedTemplates, 'undefined', kz_util:to_binary(Reason)),
            E
    end.

-spec maybe_log_smtp(email_map(), ne_binary(), list(), api_binary(), api_binary()) -> 'ok'.
-spec maybe_log_smtp(email_map(), ne_binary(), list(), api_binary(), api_binary(), boolean()) -> 'ok'.
maybe_log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error) ->
    Skip = kz_util:is_true(get('skip_smtp_log')),
    maybe_log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error, Skip).

maybe_log_smtp(_Emails, _Subject, _RenderedTemplates, _Receipt, _Error, 'true') ->
    lager:debug("skipping smtp log");
maybe_log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error, 'false') ->
    case get('account_id') of
        'undefined' ->
            lager:debug("skipping smtp log since account_id is 'undefined'");
        AccountId ->
            log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error, AccountId)
    end.

-spec log_smtp(email_map(), ne_binary(), list(), api_binary(), api_binary(), ne_binary()) -> 'ok'.
log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error, AccountId) ->
    AccountDb = kazoo_modb:get_modb(AccountId),
    Doc = props:filter_undefined(
            [{<<"rendered_templates">>, kz_json:from_list(RenderedTemplates)}
            ,{<<"subject">>, Subject}
            ,{<<"emails">>, kz_json:from_list(props:filter_undefined(Emails))}
            ,{<<"receipt">>, Receipt}
            ,{<<"error">>, Error}
            ,{<<"pvt_type">>, <<"notify_smtp_log">>}
            ,{<<"account_id">>, AccountId}
            ,{<<"account_db">>, AccountDb}
            ,{<<"pvt_created">>, kz_util:current_tstamp()}
            ,{<<"template_id">>, get('template_id')}
            ,{<<"template_account_id">>, get('template_account_id')}
            ,{<<"_id">>, make_smtplog_id(AccountDb)}
            ]),
    lager:debug("attempting to save notify smtp log"),
    _ = kazoo_modb:save_doc(AccountDb, kz_json:from_list(Doc)),
    'ok'.

-spec make_smtplog_id(ne_binary()) -> ne_binary().
make_smtplog_id(?MATCH_MODB_SUFFIX_ENCODED(_Account, Year, Month)) ->
    ?MATCH_MODB_PREFIX(Year, Month, kz_util:rand_hex_binary(16)).

-spec email_body(rendered_templates()) -> mimemail:mimetuple().
email_body(RenderedTemplates) ->
    {<<"multipart">>
    ,<<"alternative">>
    ,[] %% Headers
    ,[] %% ContentTypeParams
    ,add_rendered_templates_to_email(RenderedTemplates)
    }.

-spec email_parameters(kz_proplist(), kz_proplist()) -> kz_proplist().
email_parameters([], Params) ->
    lists:reverse(props:filter_empty(Params));
email_parameters([{_Key, 'undefined'}|T], Params) ->
    email_parameters(T, Params);
email_parameters([{Key, Vs}|T], Params) when is_list(Vs) ->
    email_parameters(T, [{Key, V} || V <- Vs] ++ Params);
email_parameters([{Key, V}|T], Params) ->
    email_parameters(T, [{Key, V} | Params]).

-spec relay_email(api_binaries(), ne_binary(), mimemail:mimetuple()) ->
                         'ok' | {'error', any()}.
relay_email(To, From, {_Type
                      ,_SubType
                      ,Addresses
                      ,_ContentTypeParams
                      ,_Body
                      }=Email) ->
    try mimemail:encode(Email) of
        Encoded ->
            RelayResult = relay_encoded_email(To, From, Encoded),
            maybe_relay_to_bcc(From, Encoded, props:get_value(<<"Bcc">>, Addresses)),
            RelayResult
    catch
        'error':'missing_from' ->
            lager:warning("no From address: ~s: ~p", [From, Addresses]),
            {'error', 'missing_from'};
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:warning("failed to encode email: ~s: ~p", [_E, _R]),
            kz_util:log_stacktrace(ST),
            {'error', 'email_encoding_failed'}
    end.

-spec maybe_relay_to_bcc(ne_binary(), ne_binary(), api_binaries()) -> 'ok'.
maybe_relay_to_bcc(_From, _Encoded, 'undefined') -> 'ok';
maybe_relay_to_bcc(_From, _Encoded, []) -> 'ok';
maybe_relay_to_bcc(From, Encoded, Bcc) ->
    case kapps_config:get_is_true(?APP_NAME, <<"iterate_over_bcc">>, 'true') of
        'true' -> relay_to_bcc(From, Encoded, Bcc);
        'false' -> 'ok'
    end.

-spec relay_to_bcc(ne_binary(), ne_binary(), ne_binaries() | ne_binary()) ->
                          {'ok', ne_binary()} | {'error', any()}.
relay_to_bcc(From, Encoded, Bcc) when is_binary(Bcc) ->
    relay_encoded_email([Bcc], From, Encoded);
relay_to_bcc(From, Encoded, Bcc) ->
    relay_encoded_email(Bcc, From, Encoded).

-spec relay_encoded_email(api_binaries(), ne_binary(), ne_binary()) ->
                                 {'ok', ne_binary()} | {'error', any()}.
relay_encoded_email('undefined', _From, _Encoded) ->
    lager:debug("failed to send email as the TO address(es) are missing"),
    {'error', 'invalid_to_addresses'};
relay_encoded_email([], _From, _Encoded) ->
    lager:debug("failed to send email as the TO addresses list is empty"),
    {'error', 'no_to_addresses'};
relay_encoded_email(To, From, Encoded) ->
    Self = self(),

    lager:debug("relaying from ~s to ~p", [From, To]),
    gen_smtp_client:send({From, To, Encoded}
                        ,smtp_options()
                        ,fun(X) -> Self ! {'relay_response', X} end
                        ),
    %% The callback will receive either `{ok, Receipt}' where Receipt is the SMTP server's receipt
    %% identifier,  `{error, Type, Message}' or `{exit, ExitReason}', as the single argument.
    receive
        {'relay_response', {'ok', Receipt}} ->
            kz_cache:store_local(?CACHE_NAME
                                ,{'receipt', Receipt}
                                ,#email_receipt{to=To
                                               ,from=From
                                               ,timestamp=kz_util:current_tstamp()
                                               ,call_id=kz_util:get_callid()
                                               }
                                ,[{'expires', ?MILLISECONDS_IN_HOUR}]
                                ),
            _ = lager:debug("relayed message: ~p", [Receipt]),
            {'ok', binary:replace(Receipt, <<"\r\n">>, <<>>, ['global'])};
        {'relay_response', {'error', _Type, {_SubType, _FailHost, Message}}} ->
            lager:debug("error relaying message: ~p(~p): ~p", [_Type, _SubType, Message]),
            {'error', Message};
        {'relay_response', {'exit', Reason}} ->
            lager:debug("failed to send email:"),
            log_email_send_error(Reason),
            {'error', Reason}
    after 10 * ?MILLISECONDS_IN_SECOND ->
            lager:debug("timed out waiting for relay response"),
            {'error', 'timeout'}
    end.

log_email_send_error({'function_clause', Stacktrace}) ->
    kz_util:log_stacktrace(Stacktrace);
log_email_send_error(Reason) ->
    lager:debug("exit relaying message: ~p", [Reason]).

-spec smtp_options() -> kz_proplist().
smtp_options() ->
    Relay = kz_util:to_list(kapps_config:get(<<"smtp_client">>, <<"relay">>, <<"localhost">>)),
    Username = kz_util:to_list(kapps_config:get(<<"smtp_client">>, <<"username">>)),
    Password = kz_util:to_list(kapps_config:get(<<"smtp_client">>, <<"password">>)),
    Auth = kapps_config:get(<<"smtp_client">>, <<"auth">>, <<"never">>),
    Port = kapps_config:get_integer(<<"smtp_client">>, <<"port">>, 25),
    Retries = kapps_config:get_integer(<<"smtp_client">>, <<"retries">>, 1),
    NoMxLookups = kapps_config:get_is_true(<<"smtp_client">>, <<"no_mx_lookups">>, 'true'),
    TLS = kapps_config:get(<<"smtp_client">>, <<"tls">>),
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

-spec smtp_auth_option(ne_binary()) -> atom().
smtp_auth_option(<<"if_available">>) -> 'if_available';
smtp_auth_option(<<"always">>) -> 'always';
smtp_auth_option(_) -> 'never'.

-spec smtp_tls_option(ne_binary()) -> atom().
smtp_tls_option(<<"if_available">>) -> 'if_available';
smtp_tls_option(<<"always">>) -> 'always';
smtp_tls_option(_) -> 'never'.

-spec add_attachments(attachments()) -> mime_tuples().
-spec add_attachments(attachments(), mime_tuples()) -> mime_tuples().
add_attachments(Attachments) ->
    add_attachments(Attachments, []).
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
    CTEncoding = kapps_config:get_ne_binary(?NOTIFY_CONFIG_CAT,
                                            [<<"mime-encoding">>
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

-spec system_params() -> kz_proplist().
system_params() ->
    [{<<"hostname">>, kz_util:to_binary(net_adm:localhost())}].

-spec user_params(kzd_user:doc()) -> kz_proplist().
user_params(UserJObj) ->
    Ks = [{<<"first_name">>, fun kzd_user:first_name/1}
         ,{<<"last_name">>, fun kzd_user:last_name/1}
         ,{<<"email">>, fun kzd_user:email/1}
         ,{<<"timezone">>, fun kzd_user:timezone/1}
         ],
    props:filter_undefined(
      [user_property(UserJObj, K, F) || {K, F} <- Ks]
     ).

-spec user_property(kzd_user:doc(), Key, fun((kz_json:object()) -> api_binary())) ->
                           {Key, api_binary()}.
user_property(User, <<_/binary>>=Key, Fun) when is_function(Fun, 1) ->
    {Key, Fun(User)}.

-spec account_params(kz_json:object()) -> kz_proplist().
account_params(DataJObj) ->
    case find_account_id(DataJObj) of
        'undefined' ->
            put('account_id', 'undefined'),
            [];
        AccountId ->
            put('account_id', AccountId),
            find_account_params(DataJObj, AccountId)
    end.

-spec find_account_params(kz_json:object(), ne_binary()) -> kz_proplist().
find_account_params(DataJObj, AccountId) ->
    case open_doc(<<"account">>, AccountId, DataJObj) of
        {'ok', AccountJObj} ->
            props:filter_undefined([{<<"name">>, kz_account:name(AccountJObj)}
                                   ,{<<"realm">>, kz_account:realm(AccountJObj)}
                                   ,{<<"id">>, kz_account:id(AccountJObj)}
                                   ,{<<"language">>, kz_account:language(AccountJObj)}
                                   ,{<<"timezone">>, kz_account:timezone(AccountJObj)}
                                    | maybe_add_parent_params(AccountJObj)
                                   ]);
        {'error', _E} ->
            lager:debug("failed to find account doc for ~s: ~p", [AccountId, _E]),
            []
    end.

-spec maybe_add_parent_params(kz_json:object()) -> kz_proplist().
maybe_add_parent_params(AccountJObj) ->
    case kz_account:parent_account_id(AccountJObj) of
        'undefined' -> [];
        ParentAccountId ->
            {'ok', ParentAccountJObj} = kz_account:fetch(ParentAccountId),
            [{<<"parent_name">>, kz_account:name(ParentAccountJObj)}
            ,{<<"parent_realm">>, kz_account:realm(ParentAccountJObj)}
            ,{<<"parent_id">>, kz_account:id(ParentAccountJObj)}
            ]
    end.


-spec default_from_address(ne_binary()) -> ne_binary().
-spec default_from_address(kz_json:object(), ne_binary()) -> ne_binary().
default_from_address(ConfigCat) ->
    default_from_address(kz_json:new(), ConfigCat).
default_from_address(JObj, ConfigCat) ->
    default_system_value(JObj, ConfigCat
                        ,<<"send_from">>, <<"default_from">>
                        ,list_to_binary([<<"no_reply@">>, net_adm:localhost()])
                        ).

-spec default_reply_to(ne_binary()) -> api_binary().
-spec default_reply_to(kz_json:object(), ne_binary()) -> api_binary().
default_reply_to(ConfigCat) ->
    default_reply_to(kz_json:new(), ConfigCat).
default_reply_to(JObj, ConfigCat) ->
    default_system_value(JObj, ConfigCat
                        ,<<"reply_to">>, <<"default_reply_to">>
                        ,'undefined'
                        ).

-spec default_system_value(kz_json:object(), ne_binary(), kz_json:path(), kz_json:path(), kz_json:json_term()) ->
                                  kz_json:json_term().
default_system_value(JObj, ConfigCat, JSONKey, ConfigKey, ConfigDefault) ->
    case kz_json:get_ne_value(JSONKey, JObj) of
        'undefined' ->
            kapps_config:get(ConfigCat, ConfigKey, ConfigDefault);
        Value -> Value
    end.

-spec render_subject(ne_binary(), kz_proplist()) -> binary().
render_subject(Template, Macros) ->
    render(<<"subject">>, Template, Macros).

render(TemplateId, Template, Macros) ->
    case teletype_renderer:render(TemplateId, Template, Macros) of
        {'ok', IOData} -> iolist_to_binary(IOData);
        {'error', _E} ->
            lager:debug("failed to render '~s': ~p '~s'", [TemplateId, _E, Template]),
            throw({'error', 'template_error'})
    end.

-spec sort_templates(rendered_templates()) -> rendered_templates().
sort_templates(RenderedTemplates) ->
    lists:sort(fun sort_templates/2, RenderedTemplates).

-spec sort_templates({ne_binary(), any()}, {ne_binary(), any()}) -> boolean().
sort_templates({K1, _}, {K2, _}) ->
    props:get_value(K1, ?TEMPLATE_RENDERING_ORDER, 1) =<
        props:get_value(K2, ?TEMPLATE_RENDERING_ORDER, 1).

-spec find_account_id(kz_json:object()) -> api_binary().
find_account_id(JObj) ->
    kz_json:get_first_defined([<<"account_id">>
                              ,[<<"account">>, <<"_id">>]
                              ,<<"pvt_account_id">>
                              ,<<"_id">>, <<"id">>
                              ,<<"Account-ID">>
                              ,[<<"details">>, <<"account_id">>]
                              ,[<<"Details">>, <<"Account-ID">>]
                              ,[<<"details">>, <<"custom_channel_vars">>, <<"account_id">>]
                              ,[<<"Details">>, <<"Custom-Channel-Vars">>, <<"Account-ID">>]
                              ]
                             ,JObj
                             ).

-spec find_account_db(ne_binary(), kz_json:object()) -> api_binary().
find_account_db(<<"account">>, JObj) -> find_account_db_from_id(JObj);
find_account_db(<<"user">>, JObj) -> find_account_db_from_id(JObj);
find_account_db(<<"fax">>, JObj) -> find_account_db(JObj);
find_account_db(<<"port_request">>, _JObj) -> ?KZ_PORT_REQUESTS_DB;
find_account_db(<<"webhook">>, _JObj) -> ?KZ_WEBHOOKS_DB;
find_account_db(_, JObj) -> find_account_db_from_id(JObj).

-spec find_account_db(kz_json:object()) -> api_binary().
find_account_db(JObj) ->
    PossibleDbs = [<<"account_db">>, <<"pvt_account_db">>, <<"Account-DB">>],
    case kz_json:get_first_defined(PossibleDbs, JObj) of
        'undefined' -> find_account_db_from_id(JObj);
        Db -> Db
    end.

-spec find_account_db_from_id(kz_json:object()) -> api_binary().
find_account_db_from_id(JObj) ->
    case find_account_id(JObj) of
        'undefined' -> 'undefined';
        Id -> kz_util:format_account_id(Id, 'encoded')
    end.

-spec send_update(kz_json:object(), ne_binary()) -> 'ok'.
-spec send_update(kz_json:object(), ne_binary(), api_binary()) -> 'ok'.
-spec send_update(api_binary(), ne_binary(), ne_binary(), api_binary()) -> 'ok'.
send_update(DataJObj, Status) ->
    send_update(DataJObj, Status, 'undefined').
send_update(DataJObj, Status, Message) ->
    send_update(kz_json:get_first_defined([<<"server_id">>, <<"Server-ID">>], DataJObj)
               ,kz_json:get_value(<<"msg_id">>, DataJObj)
               ,Status
               ,Message
               ).

send_update('undefined', _, _, _) ->
    lager:debug("no response queue available, not publishing update");
send_update(RespQ, MsgId, Status, Msg) ->
    Prop = props:filter_undefined(
             [{<<"Status">>, Status}
             ,{<<"Failure-Message">>, Msg}
             ,{<<"Msg-ID">>, MsgId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("notification update (~s) sending to ~s", [Status, RespQ]),
    kz_amqp_worker:cast(Prop, fun(P) -> kapi_notifications:publish_notify_update(RespQ, P) end).

-spec find_account_rep_email(api_object() | ne_binary()) -> api_binaries().
find_account_rep_email('undefined') -> 'undefined';
find_account_rep_email(<<_/binary>> = AccountId) ->
    case kz_services:is_reseller(AccountId) of
        'true' ->
            lager:debug("finding admin email for reseller account ~s", [AccountId]),
            find_account_admin_email(AccountId);
        'false' ->
            lager:debug("finding admin email for reseller of account ~s", [AccountId]),
            find_account_admin_email(kz_services:find_reseller_id(AccountId))
    end;
find_account_rep_email(AccountJObj) ->
    find_account_rep_email(
      find_account_id(AccountJObj)
     ).

-spec find_account_admin_email(api_binary()) -> api_binaries().
-spec find_account_admin_email(api_binary(), api_binary()) -> api_binaries().
find_account_admin_email('undefined') -> 'undefined';
find_account_admin_email(AccountId) ->
    find_account_admin_email(AccountId, kz_services:find_reseller_id(AccountId)).

find_account_admin_email('undefined', _Id) ->
    'undefined';
find_account_admin_email(AccountId, AccountId) ->
    case query_account_for_admin_emails(AccountId) of
        [] -> 'undefined';
        Emails -> Emails
    end;
find_account_admin_email(AccountId, ResellerId) ->
    case query_account_for_admin_emails(AccountId) of
        [] -> find_account_admin_email(get_parent_account_id(AccountId), ResellerId);
        Emails -> Emails
    end.

-spec query_account_for_admin_emails(ne_binary()) -> ne_binaries().
query_account_for_admin_emails(<<_/binary>> = AccountId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    ViewOptions = [{'key', <<"user">>}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> [];
        {'ok', Users} -> extract_admin_emails(Users);
        {'error', _E} ->
            lager:debug("failed to find users in ~s: ~p", [AccountId, _E]),
            []
    end.

-spec extract_admin_emails(kzd_user:docs()) -> ne_binaries().
extract_admin_emails(Users) ->
    [Email
     || Admin <- filter_for_admins(Users),
        (Email = kzd_user:email(Admin)) =/= 'undefined'
    ].

-spec find_account_admin(api_binary()) -> api_object().
-spec find_account_admin(ne_binary(), ne_binary()) -> 'undefined' | kzd_user:doc().
find_account_admin('undefined') -> 'undefined';
find_account_admin(<<_/binary>> = AccountId) ->
    find_account_admin(AccountId, kz_services:find_reseller_id(AccountId)).

find_account_admin(AccountId, AccountId) ->
    query_for_account_admin(AccountId);
find_account_admin(AccountId, ResellerId) ->
    case query_for_account_admin(AccountId) of
        'undefined' -> find_account_admin(ResellerId);
        Admin -> Admin
    end.

-spec query_for_account_admin(ne_binary()) -> 'undefined' | kzd_user:doc().
query_for_account_admin(AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
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
            lager:debug("failed to find users in ~s: ~p", [AccountId, _E]),
            'undefined'
    end.

-spec filter_for_admins(kz_json:objects()) -> kzd_user:docs().
filter_for_admins(Users) ->
    [Doc
     || User <- Users,
        kzd_user:priv_level(Doc = kz_json:get_value(<<"doc">>, User)) =:= <<"admin">>
    ].

-spec should_handle_notification(kz_json:object()) -> boolean().
-spec should_handle_notification(kz_json:object(), boolean()) -> boolean().
should_handle_notification(JObj) ->
    DataJObj = kz_json:normalize(JObj),
    should_handle_notification(DataJObj, is_preview(JObj)).

should_handle_notification(_JObj, 'true') ->
    lager:debug("notification is a preview, handling"),
    'true';
should_handle_notification(JObj, 'false') ->
    case find_account_id(JObj) of
        'undefined' -> should_handle_system();
        Account -> should_handle_account(Account)
    end.

-spec should_handle_system() -> boolean().
should_handle_system() ->
    lager:debug("should system handle notification"),
    kapps_config:get(?NOTIFY_CONFIG_CAT
                    ,<<"notification_app">>
                    ,?APP_NAME
                    )
        =:= ?APP_NAME.

-spec should_handle_account(ne_binary()) -> boolean().
-spec should_handle_account(api_binary(), api_binary()) -> boolean().
should_handle_account(Account) ->
    case kz_account:fetch(Account) of
        {'error', _E} ->
            lager:debug("teletype should handle account ~s", [Account]),
            'true';
        {'ok', JObj} ->
            should_handle_account(Account
                                 ,kz_account:notification_preference(JObj)
                                 )
    end.

should_handle_account(_Account, ?APP_NAME) -> 'true';
should_handle_account('undefined', 'undefined') ->
    should_handle_system();
should_handle_account(Account, 'undefined') ->
    should_handle_reseller(Account);
should_handle_account(_Account, _Preference) ->
    lager:debug("not handling notification;"
                " unknown notification preference '~s' for '~s'"
               ,[_Preference, _Account]
               ).

-spec should_handle_reseller(ne_binary()) -> boolean().
should_handle_reseller(Account) ->
    case kz_account:fetch(kz_services:find_reseller_id(Account)) of
        {'error', _E} -> 'true';
        {'ok', ResellerJObj} ->
            should_handle_account('undefined'
                                 ,kz_account:notification_preference(ResellerJObj)
                                 )
    end.

-define(MOD_CONFIG_CAT(Key), <<(?NOTIFY_CONFIG_CAT)/binary, ".", Key/binary>>).

-spec is_notice_enabled(api_binary(), kz_json:object(), ne_binary()) -> boolean().
is_notice_enabled('undefined', _ApiJObj, TemplateKey) ->
    is_notice_enabled_default(TemplateKey);
is_notice_enabled(AccountId, ApiJObj, TemplateKey) ->
    case kz_json:is_true(<<"Preview">>, ApiJObj, 'false') of
        'true' -> 'true';
        'false' ->
            ResellerAccountId = kz_services:find_reseller_id(AccountId),
            is_account_notice_enabled(AccountId, TemplateKey, ResellerAccountId)
    end.

-spec is_account_notice_enabled(api_binary(), ne_binary(), ne_binary()) -> boolean().
is_account_notice_enabled('undefined', TemplateKey, _ResellerAccountId) ->
    lager:debug("no account id to check, checking system config for ~s", [TemplateKey]),
    is_notice_enabled_default(TemplateKey);
is_account_notice_enabled(AccountId, TemplateKey, ResellerAccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    TemplateId = teletype_templates:doc_id(TemplateKey),

    case kz_datamgr:open_cache_doc(AccountDb, TemplateId) of
        {'ok', TemplateJObj} ->
            lager:debug("account ~s has ~s, checking if enabled", [AccountId, TemplateId]),
            kz_notification:is_enabled(TemplateJObj);
        _Otherwise when AccountId =/= ResellerAccountId ->
            lager:debug("account ~s is mute, checking parent", [AccountId]),
            is_account_notice_enabled(
              get_parent_account_id(AccountId)
                                     ,TemplateId
                                     ,ResellerAccountId
             );
        _Otherwise ->
            is_notice_enabled_default(TemplateKey)
    end.

-spec is_notice_enabled_default(ne_binary()) -> boolean().
is_notice_enabled_default(TemplateKey) ->
    TemplateId = teletype_templates:doc_id(TemplateKey),
    case kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, TemplateId) of
        {'ok', TemplateJObj} ->
            lager:debug("system has ~s, checking if enabled", [TemplateId]),
            kz_notification:is_enabled(TemplateJObj);
        _Otherwise ->
            lager:debug("system is mute, ~s not enabled", [TemplateId]),
            'false'
    end.

-spec get_parent_account_id(ne_binary()) -> api_binary().
get_parent_account_id(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', JObj} -> kz_account:parent_account_id(JObj);
        {'error', _E} ->
            lager:error("failed to find parent account for ~s", [AccountId]),
            'undefined'
    end.

-spec find_addresses(kz_json:object(), kz_json:object(), ne_binary()) ->
                            email_map().
-spec find_addresses(kz_json:object(), kz_json:object(), ne_binary(), kz_json:path(), email_map()) ->
                            email_map().
find_addresses(DataJObj, TemplateMetaJObj, ConfigCat) ->
    AddressKeys = [<<"to">>, <<"cc">>, <<"bcc">>, <<"from">>, <<"reply_to">>],
    find_addresses(DataJObj, TemplateMetaJObj, ConfigCat, AddressKeys, []).

find_addresses(_DataJObj, _TemplateMetaJObj, _ConfigCat, [], Acc) -> Acc;
find_addresses(DataJObj, TemplateMetaJObj, ConfigCat, [Key|Keys], Acc) ->
    find_addresses(
      DataJObj
                  ,TemplateMetaJObj
                  ,ConfigCat
                  ,Keys
                  ,[find_address(DataJObj, TemplateMetaJObj, ConfigCat, Key)|Acc]
     ).

-spec find_address(kz_json:object(), kz_json:object(), ne_binary(), kz_json:path()) ->
                          {kz_json:path(), api_binaries()}.
-spec find_address(kz_json:object(), kz_json:object(), ne_binary(), kz_json:path(), api_binary()) ->
                          {kz_json:path(), api_binaries()}.
find_address(DataJObj, TemplateMetaJObj, ConfigCat, Key) ->
    find_address(
      DataJObj
                ,TemplateMetaJObj
                ,ConfigCat
                ,Key
                ,kz_json:find([Key, <<"type">>]
                             ,[DataJObj, TemplateMetaJObj]
                             )
     ).

find_address(DataJObj, TemplateMetaJObj, _ConfigCat, Key, 'undefined') ->
    lager:debug("email type for '~s' not defined in template, checking just the key", [Key]),
    {Key, kz_json:find(Key, [DataJObj, TemplateMetaJObj])};
find_address(DataJObj, TemplateMetaJObj, _ConfigCat, Key, ?EMAIL_SPECIFIED) ->
    lager:debug("checking template for '~s' email addresses", [Key]),
    {Key, find_address([Key, <<"email_addresses">>], DataJObj, TemplateMetaJObj)};
find_address(DataJObj, TemplateMetaJObj, _ConfigCat, Key, ?EMAIL_ORIGINAL) ->
    lager:debug("checking data for '~s' email address(es)", [Key]),
    {Key, find_address(Key, DataJObj, TemplateMetaJObj)};
find_address(DataJObj, _TemplateMetaJObj, ConfigCat, Key, ?EMAIL_ADMINS) ->
    lager:debug("looking for admin emails for '~s'", [Key]),
    {Key, find_admin_emails(DataJObj, ConfigCat, Key)}.

-spec find_address(kz_json:path(), kz_json:object(), kz_json:object()) ->
                          api_binaries().
find_address(Key, DataJObj, TemplateMetaJObj) ->
    case kz_json:get_ne_value(Key, DataJObj) of
        'undefined' -> kz_json:get_ne_value(Key, TemplateMetaJObj);
        Emails -> Emails
    end.

-spec find_admin_emails(kz_json:object(), ne_binary(), kz_json:path()) ->
                               api_binaries().
find_admin_emails(DataJObj, ConfigCat, Key) ->
    case find_account_rep_email(find_account_id(DataJObj)) of
        'undefined' ->
            lager:debug("didn't find account rep for '~s'", [Key]),
            find_default(ConfigCat, Key);
        Emails -> Emails
    end.

-spec find_default(ne_binary(), kz_json:path()) -> api_binaries().
find_default(ConfigCat, Key) ->
    case kapps_config:get(ConfigCat, <<"default_", Key/binary>>) of
        'undefined' ->
            lager:debug("no default in ~s for default_~s", [ConfigCat, Key]),
            'undefined';
        <<>> ->
            lager:debug("empty default in ~s for default_~s", [ConfigCat, Key]),
            'undefined';
        <<_/binary>> = Email -> [Email];
        Emails -> Emails
    end.

-spec open_doc(ne_binary(), api_binary(), kz_json:object()) ->
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

-spec maybe_load_preview(ne_binary(), E, boolean()) ->
                                {'ok', kz_json:object()} |
                                {'error', read_file_error()} | E.
maybe_load_preview(_Type, Error, 'false') ->
    Error;
maybe_load_preview(Type, _Error, 'true') ->
    read_doc(Type).

-spec read_doc(ne_binary()) ->
                      {'ok', kz_json:object()} |
                      {'error', read_file_error()}.
read_doc(File) ->
    AppDir = code:lib_dir('teletype'),
    PreviewFile = filename:join([AppDir, "priv", "preview_data", <<File/binary, ".json">>]),
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
    kz_util:is_true(
      kz_json:get_first_defined([<<"Preview">>, <<"preview">>], DataJObj, 'false')
     ).

-spec public_proplist(kz_json:path(), kz_json:object()) -> kz_proplist().
public_proplist(Key, JObj) ->
    kz_json:to_proplist(
      kz_json:public_fields(
        kz_json:get_value(Key, JObj, kz_json:new())
       )
     ).

-spec stop_processing(string(), list()) -> no_return().
stop_processing(Format, Args) ->
    lager:debug(Format, Args),
    exit('normal').

-spec maybe_get_attachments(kz_json:object() | api_binary()) -> attachments().
maybe_get_attachments('undefined') -> [];
maybe_get_attachments(URL)
  when is_binary(URL) ->
    case fetch_attachment_from_url(URL) of
        {'ok', Attachment} -> [Attachment];
        {'error', _} -> []
    end;
maybe_get_attachments(DataObj) ->
    maybe_get_attachments(kz_json:get_value(<<"attachment_url">>, DataObj)).

-spec fetch_attachment_from_url(ne_binary()) -> {'ok', attachment()} | {'error', any()}.
fetch_attachment_from_url(URL) ->
    case kz_http:get(kz_util:to_list(URL)) of
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

-spec attachment_from_url_result(kz_proplist(), binary()) -> attachment().
attachment_from_url_result(Headers, Body) ->
    CT = kz_util:to_binary(props:get_value("content-type", Headers, <<"text/plain">>)),
    Disposition = kz_util:to_binary(props:get_value("content-disposition", Headers, <<>>)),
    CDs = [ list_to_tuple(binary:split(kz_util:strip_binary(CD), <<"=">>)) || CD <- binary:split(Disposition, <<";">>)],
    Filename = case props:get_value(<<"filename">>, CDs) of
                   'undefined' -> kz_mime:to_filename(CT);
                   FileDisposition -> FileDisposition
               end,
    {CT, Filename, Body}.
