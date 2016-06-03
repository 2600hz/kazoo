%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_util).

-export([send_email/3, send_email/4
         ,render_subject/2, render/3
         ,system_params/0
         ,account_params/1
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
        {'error', Reason} = E ->
            maybe_log_smtp(Emails, Subject, RenderedTemplates, 'undefined', wh_util:to_binary(Reason)),
            E
    end.

-spec maybe_log_smtp(email_map(), ne_binary(), list(), api_binary(), api_binary()) -> 'ok'.
-spec maybe_log_smtp(email_map(), ne_binary(), list(), api_binary(), api_binary(), boolean()) -> 'ok'.
maybe_log_smtp(Emails, Subject, RenderedTemplates, Receipt, Error) ->
    Skip = wh_util:is_true(get('skip_smtp_log')),
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
            [{<<"rendered_templates">>, wh_json:from_list(RenderedTemplates)}
             ,{<<"subject">>, Subject}
             ,{<<"emails">>, wh_json:from_list(props:filter_undefined(Emails))}
             ,{<<"receipt">>, Receipt}
             ,{<<"error">>, Error}
             ,{<<"pvt_type">>, <<"notify_smtp_log">>}
             ,{<<"account_id">>, AccountId}
             ,{<<"account_db">>, AccountDb}
             ,{<<"pvt_created">>, wh_util:current_tstamp()}
             ,{<<"template_id">>, get('template_id')}
             ,{<<"template_account_id">>, get('template_account_id')}
             ,{<<"_id">>, make_smtplog_id(AccountDb)}
            ]),
    lager:debug("attempting to save notify smtp log"),
    _ = kazoo_modb:save_doc(AccountDb, wh_json:from_list(Doc)),
    'ok'.

-spec make_smtplog_id(ne_binary()) -> ne_binary().
make_smtplog_id(?MATCH_MODB_SUFFIX_ENCODED(_Account, Year, Month)) ->
    ?MATCH_MODB_PREFIX(Year, Month, wh_util:rand_hex_binary(16)).

-spec email_body(rendered_templates()) -> mimemail:mimetuple().
email_body(RenderedTemplates) ->
    {<<"multipart">>
     ,<<"alternative">>
     ,[] %% Headers
     ,[] %% ContentTypeParams
     ,add_rendered_templates_to_email(RenderedTemplates)
    }.

-spec email_parameters(wh_proplist(), wh_proplist()) -> wh_proplist().
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
            wh_util:log_stacktrace(ST),
            {'error', 'email_encoding_failed'}
    end.

-spec maybe_relay_to_bcc(ne_binary(), ne_binary(), api_binaries()) -> 'ok'.
maybe_relay_to_bcc(_From, _Encoded, 'undefined') -> 'ok';
maybe_relay_to_bcc(_From, _Encoded, []) -> 'ok';
maybe_relay_to_bcc(From, Encoded, Bcc) ->
    case whapps_config:get_is_true(?APP_NAME, <<"iterate_over_bcc">>, 'true') of
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
            wh_cache:store_local(?CACHE_NAME
                                 ,{'receipt', Receipt}
                                 ,#email_receipt{to=To
                                                 ,from=From
                                                 ,timestamp=wh_util:current_tstamp()
                                                 ,call_id=wh_util:get_callid()
                                                }
                                 ,[{'expires', ?MILLISECONDS_IN_HOUR}]
                                ),
            _ = lager:debug("relayed message: ~p", [Receipt]),
            {'ok', binary:replace(Receipt, <<"\r\n">>, <<>>, ['global'])};
        {'relay_response', {'error', _Type, Message}} ->
            lager:debug("error relaying message: ~p: ~p", [_Type, Message]),
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
    wh_util:log_stacktrace(Stacktrace);
log_email_send_error(Reason) ->
    lager:debug("exit relaying message: ~p", [Reason]).

-spec smtp_options() -> wh_proplist().
smtp_options() ->
    Relay = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"relay">>, <<"localhost">>)),
    Username = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"username">>)),
    Password = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"password">>)),
    Auth = whapps_config:get(<<"smtp_client">>, <<"auth">>, <<"never">>),
    Port = whapps_config:get_integer(<<"smtp_client">>, <<"port">>, 25),
    Retries = whapps_config:get_integer(<<"smtp_client">>, <<"retries">>, 1),
    NoMxLookups = whapps_config:get_is_true(<<"smtp_client">>, <<"no_mx_lookups">>, 'true'),
    TLS = whapps_config:get(<<"smtp_client">>, <<"tls">>),
    SSL = whapps_config:get_is_true(<<"smtp_client">>, <<"use_ssl">>, 'false'),

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
    CTEncoding = whapps_config:get_ne_binary(?NOTIFY_CONFIG_CAT,
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

-spec system_params() -> wh_proplist().
system_params() ->
    [{<<"hostname">>, wh_util:to_binary(net_adm:localhost())}].

-spec account_params(wh_json:object()) -> wh_proplist().
account_params(DataJObj) ->
    case find_account_id(DataJObj) of
        'undefined' ->
            put('account_id', 'undefined'),
            [];
        AccountId ->
            put('account_id', AccountId),
            find_account_params(DataJObj, AccountId)
    end.

-spec find_account_params(wh_json:object(), ne_binary()) -> wh_proplist().
find_account_params(DataJObj, AccountId) ->
    case open_doc(<<"account">>, AccountId, DataJObj) of
        {'ok', AccountJObj} ->
            props:filter_undefined([{<<"name">>, kz_account:name(AccountJObj)}
                                    ,{<<"realm">>, kz_account:realm(AccountJObj)}
                                    ,{<<"id">>, kz_account:id(AccountJObj)}
                                    ,{<<"language">>, kz_account:language(AccountJObj)}
                                    ,{<<"timezone">>, kz_account:timezone(AccountJObj)}
                                   ]);
        {'error', _E} ->
            lager:debug("failed to find account doc for ~s: ~p", [AccountId, _E]),
            []
    end.

-spec default_from_address(ne_binary()) -> ne_binary().
-spec default_from_address(wh_json:object(), ne_binary()) -> ne_binary().
default_from_address(ConfigCat) ->
    default_from_address(wh_json:new(), ConfigCat).
default_from_address(JObj, ConfigCat) ->
    default_system_value(JObj, ConfigCat
                         ,<<"send_from">>, <<"default_from">>
                         ,list_to_binary([<<"no_reply@">>, net_adm:localhost()])
                        ).

-spec default_reply_to(ne_binary()) -> api_binary().
-spec default_reply_to(wh_json:object(), ne_binary()) -> api_binary().
default_reply_to(ConfigCat) ->
    default_reply_to(wh_json:new(), ConfigCat).
default_reply_to(JObj, ConfigCat) ->
    default_system_value(JObj, ConfigCat
                         ,<<"reply_to">>, <<"default_reply_to">>
                         ,'undefined'
                        ).

-spec default_system_value(wh_json:object(), ne_binary(), wh_json:key(), wh_json:key(), wh_json:json_term()) ->
                                  wh_json:json_term().
default_system_value(JObj, ConfigCat, JSONKey, ConfigKey, ConfigDefault) ->
    case wh_json:get_ne_value(JSONKey, JObj) of
        'undefined' ->
            whapps_config:get(ConfigCat, ConfigKey, ConfigDefault);
        Value -> Value
    end.

-spec render_subject(ne_binary(), wh_proplist()) -> binary().
render_subject(Template, Macros) ->
    render(<<"subject">>, Template, Macros).

-spec render(ne_binary(), binary(), wh_proplist()) -> binary().
render(TemplateId, Template, Macros) ->
    case teletype_renderer:render(TemplateId, Template, Macros) of
        {'ok', IOData} -> iolist_to_binary(IOData);
        {'error', _E} ->
            lager:debug("failed to render template ~s: ~p '~s'", [TemplateId, _E, Template]),
            throw({'error', 'template_error'})
    end.

-spec sort_templates(rendered_templates()) -> rendered_templates().
sort_templates(RenderedTemplates) ->
    lists:sort(fun sort_templates/2, RenderedTemplates).

-spec sort_templates({ne_binary(), any()}, {ne_binary(), any()}) -> boolean().
sort_templates({K1, _}, {K2, _}) ->
    props:get_value(K1, ?TEMPLATE_RENDERING_ORDER, 1) =<
        props:get_value(K2, ?TEMPLATE_RENDERING_ORDER, 1).

-spec find_account_id(wh_json:object()) -> api_binary().
find_account_id(JObj) ->
    wh_json:get_first_defined([<<"account_id">>
                               ,[<<"account">>, <<"_id">>]
                               ,<<"pvt_account_id">>
                               ,<<"_id">>, <<"id">>
                               ,<<"Account-ID">>
                              ]
                              ,JObj
                             ).

-spec find_account_db(ne_binary(), wh_json:object()) -> api_binary().
find_account_db(<<"account">>, JObj) -> find_account_db_from_id(JObj);
find_account_db(<<"user">>, JObj) -> find_account_db_from_id(JObj);
find_account_db(<<"fax">>, JObj) -> find_account_db(JObj);
find_account_db(<<"port_request">>, _JObj) -> ?KZ_PORT_REQUESTS_DB;
find_account_db(<<"webhook">>, _JObj) -> ?KZ_WEBHOOKS_DB;
find_account_db(_, JObj) -> find_account_db_from_id(JObj).

-spec find_account_db(wh_json:object()) -> api_binary().
find_account_db(JObj) ->
    PossibleDbs = [<<"account_db">>, <<"pvt_account_db">>, <<"Account-DB">>],
    case wh_json:get_first_defined(PossibleDbs, JObj) of
        'undefined' -> find_account_db_from_id(JObj);
        Db -> Db
    end.

-spec find_account_db_from_id(wh_json:object()) -> api_binary().
find_account_db_from_id(JObj) ->
    case find_account_id(JObj) of
        'undefined' -> 'undefined';
        Id -> wh_util:format_account_id(Id, 'encoded')
    end.

-spec send_update(wh_json:object(), ne_binary()) -> 'ok'.
-spec send_update(wh_json:object(), ne_binary(), api_binary()) -> 'ok'.
-spec send_update(api_binary(), ne_binary(), ne_binary(), api_binary()) -> 'ok'.
send_update(DataJObj, Status) ->
    send_update(DataJObj, Status, 'undefined').
send_update(DataJObj, Status, Message) ->
    send_update(wh_json:get_first_defined([<<"server_id">>, <<"Server-ID">>], DataJObj)
                ,wh_json:get_value(<<"msg_id">>, DataJObj)
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
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("notification update (~s) sending to ~s", [Status, RespQ]),
    wh_amqp_worker:cast(Prop, fun(P) -> wapi_notifications:publish_notify_update(RespQ, P) end).

-spec find_account_rep_email(api_object() | ne_binary()) -> api_binaries().
find_account_rep_email('undefined') -> 'undefined';
find_account_rep_email(<<_/binary>> = AccountId) ->
    case wh_services:is_reseller(AccountId) of
        'true' ->
            lager:debug("finding admin email for reseller account ~s", [AccountId]),
            find_account_admin_email(AccountId);
        'false' ->
            lager:debug("finding admin email for reseller of account ~s", [AccountId]),
            find_account_admin_email(wh_services:find_reseller_id(AccountId))
    end;
find_account_rep_email(AccountJObj) ->
    find_account_rep_email(
      find_account_id(AccountJObj)
     ).

-spec find_account_admin_email(api_binary()) -> api_binaries().
-spec find_account_admin_email(ne_binary(), api_binary()) -> api_binaries().
find_account_admin_email('undefined') -> 'undefined';
find_account_admin_email(AccountId) ->
    find_account_admin_email(AccountId, wh_services:find_reseller_id(AccountId)).

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
query_account_for_admin_emails(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'key', <<"user">>}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> [];
        {'ok', Users} ->
            [Email
             || Admin <- filter_for_admins(Users),
                (Email = wh_json:get_ne_value([<<"doc">>, <<"email">>], Admin)) =/= 'undefined'
            ];
        {'error', _E} ->
            lager:debug("failed to find users in ~s: ~p", [AccountId, _E]),
            []
    end.

-spec find_account_admin(api_binary()) -> api_object().
-spec find_account_admin(ne_binary(), ne_binary()) -> api_object().
find_account_admin('undefined') -> 'undefined';
find_account_admin(<<_/binary>> = AccountId) ->
    find_account_admin(AccountId, wh_services:find_reseller_id(AccountId)).

find_account_admin(AccountId, AccountId) ->
    query_for_account_admin(AccountId);
find_account_admin(AccountId, ResellerId) ->
    case query_for_account_admin(AccountId) of
        'undefined' -> find_account_admin(ResellerId);
        Admin -> Admin
    end.

-spec query_for_account_admin(ne_binary()) -> api_object().
query_for_account_admin(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'key', <<"user">>}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
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

-spec filter_for_admins(wh_json:objects()) -> wh_json:objects().
filter_for_admins(Users) ->
    [User
     || User <- Users,
        wh_json:get_value([<<"doc">>, <<"priv_level">>], User) =:= <<"admin">>
    ].

-spec should_handle_notification(wh_json:object()) -> boolean().
-spec should_handle_notification(wh_json:object(), boolean()) -> boolean().
should_handle_notification(JObj) ->
    DataJObj = wh_json:normalize(JObj),
    should_handle_notification(DataJObj, is_preview(JObj)).

should_handle_notification(_JObj, 'true') -> 'true';
should_handle_notification(JObj, 'false') ->
    case find_account_id(JObj) of
        'undefined' -> should_handle_system();
        Account -> should_handle_account(Account)
    end.

-spec should_handle_system() -> boolean().
should_handle_system() ->
    lager:debug("should system handle notification"),
    whapps_config:get(?NOTIFY_CONFIG_CAT
                      ,<<"notification_app">>
                      ,?APP_NAME
                     )
        =:= ?APP_NAME.

-spec should_handle_account(ne_binary()) -> boolean().
-spec should_handle_account(ne_binary(), api_binary()) -> boolean().
should_handle_account(Account) ->
    case kz_account:fetch(Account) of
        {'error', _E} ->
            lager:debug("teletype should handle account ~s", [Account]),
            'true';
        {'ok', JObj} ->
            should_handle_account(
              Account
              ,kz_account:notification_preference(JObj)
             )
    end.

should_handle_account(_Account, ?APP_NAME) -> 'true';
should_handle_account(Account, 'undefined') ->
    should_handle_reseller(Account);
should_handle_account(_Account, _Preference) ->
    lager:debug(
      "not handling notification; unknown notification preference '~s' for '~s'"
      ,[_Preference, _Account]
     ).

-spec should_handle_reseller(ne_binary()) -> boolean().
should_handle_reseller(Account) ->
    ResellerId = wh_services:find_reseller_id(Account),

    lager:debug("should reseller ~s handle notification", [ResellerId]),
    case kz_account:fetch(ResellerId) of
        {'error', _E} -> 'true';
        {'ok', ResellerJObj} ->
            kz_account:notification_preference(ResellerJObj) =:= ?APP_NAME
    end.

-define(MOD_CONFIG_CAT(Key), <<(?NOTIFY_CONFIG_CAT)/binary, ".", Key/binary>>).

-spec is_notice_enabled(api_binary(), wh_json:object(), ne_binary()) -> boolean().
is_notice_enabled('undefined', _ApiJObj, TemplateKey) ->
    is_notice_enabled_default(TemplateKey);
is_notice_enabled(AccountId, ApiJObj, TemplateKey) ->
    case wh_json:is_true(<<"Preview">>, ApiJObj, 'false') of
        'true' -> 'true';
        'false' ->
            ResellerAccountId = wh_services:find_reseller_id(AccountId),
            is_account_notice_enabled(AccountId, TemplateKey, ResellerAccountId)
    end.

-spec is_account_notice_enabled(api_binary(), ne_binary(), ne_binary()) -> boolean().
is_account_notice_enabled('undefined', TemplateKey, _ResellerAccountId) ->
    lager:debug("no account id to check, checking system config for ~s", [TemplateKey]),
    is_notice_enabled_default(TemplateKey);
is_account_notice_enabled(AccountId, TemplateKey, ResellerAccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    TemplateId = teletype_templates:doc_id(TemplateKey),

    case couch_mgr:open_cache_doc(AccountDb, TemplateId) of
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
    case couch_mgr:open_cache_doc(?WH_CONFIG_DB, TemplateId) of
        {'ok', TemplateJObj} ->
            lager:debug("system has ~s, checking if enabled", [TemplateId]),
            kz_notification:is_enabled(TemplateJObj);
        _Otherwise ->
            lager:debug("system is mute, ~s not enabled", [TemplateId]),
            'false'
    end.

-spec get_parent_account_id(ne_binary()) -> api_binary().
get_parent_account_id(AccountId) ->
    case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} -> kz_account:parent_account_id(JObj);
        {'error', _E} ->
            lager:error("failed to find parent account for ~s", [AccountId]),
            'undefined'
    end.

-spec find_addresses(wh_json:object(), wh_json:object(), ne_binary()) ->
                            email_map().
-spec find_addresses(wh_json:object(), wh_json:object(), ne_binary(), wh_json:keys(), email_map()) ->
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

-spec find_address(wh_json:object(), wh_json:object(), ne_binary(), wh_json:key()) ->
                          {wh_json:key(), api_binaries()}.
-spec find_address(wh_json:object(), wh_json:object(), ne_binary(), wh_json:key(), api_binary()) ->
                          {wh_json:key(), api_binaries()}.
find_address(DataJObj, TemplateMetaJObj, ConfigCat, Key) ->
    find_address(
      DataJObj
      ,TemplateMetaJObj
      ,ConfigCat
      ,Key
      ,wh_json:find([Key, <<"type">>]
                    ,[DataJObj, TemplateMetaJObj]
                   )
     ).

find_address(DataJObj, TemplateMetaJObj, _ConfigCat, Key, 'undefined') ->
    lager:debug("email type for '~s' not defined in template, checking just the key", [Key]),
    {Key, wh_json:find(Key, [DataJObj, TemplateMetaJObj])};
find_address(DataJObj, TemplateMetaJObj, _ConfigCat, Key, ?EMAIL_SPECIFIED) ->
    lager:debug("checking template for '~s' email addresses", [Key]),
    {Key, find_address([Key, <<"email_addresses">>], DataJObj, TemplateMetaJObj)};
find_address(DataJObj, TemplateMetaJObj, _ConfigCat, Key, ?EMAIL_ORIGINAL) ->
    lager:debug("checking data for '~s' email address(es)", [Key]),
    {Key, find_address(Key, DataJObj, TemplateMetaJObj)};
find_address(DataJObj, _TemplateMetaJObj, ConfigCat, Key, ?EMAIL_ADMINS) ->
    lager:debug("looking for admin emails for '~s'", [Key]),
    {Key, find_admin_emails(DataJObj, ConfigCat, Key)}.

-spec find_address(wh_json:key(), wh_json:object(), wh_json:object()) ->
                          api_binaries().
find_address(Key, DataJObj, TemplateMetaJObj) ->
    case wh_json:get_ne_value(Key, DataJObj) of
        'undefined' -> wh_json:get_ne_value(Key, TemplateMetaJObj);
        Emails -> Emails
    end.

-spec find_admin_emails(wh_json:object(), ne_binary(), wh_json:key()) ->
                               api_binaries().
find_admin_emails(DataJObj, ConfigCat, Key) ->
    case ?MODULE:find_account_rep_email(
           ?MODULE:find_account_id(DataJObj)
          )
    of
        'undefined' ->
            lager:debug("didn't find account rep for '~s'", [Key]),
            find_default(ConfigCat, Key);
        Emails -> Emails
    end.

-spec find_default(ne_binary(), wh_json:key()) -> api_binaries().
find_default(ConfigCat, Key) ->
    case whapps_config:get(ConfigCat, <<"default_", Key/binary>>) of
        'undefined' ->
            lager:debug("no default in ~s for default_~s", [ConfigCat, Key]),
            'undefined';
        <<>> ->
            lager:debug("empty default in ~s for default_~s", [ConfigCat, Key]),
            'undefined';
        <<_/binary>> = Email -> [Email];
        Emails -> Emails
    end.

-spec open_doc(ne_binary(), api_binary(), wh_json:object()) ->
                      {'ok', wh_json:object()} |
                      {'error', any()}.
open_doc(Type, 'undefined', DataJObj) ->
    maybe_load_preview(Type, {'error', 'empty_doc_id'}, is_preview(DataJObj));
open_doc(Type, DocId, DataJObj) ->
    AccountDb = find_account_db(Type, DataJObj),
    case couch_mgr:open_cache_doc(AccountDb, DocId) of
        {'ok', _JObj}=OK -> OK;
        {'error', _E}=Error ->
            maybe_load_preview(Type, Error, is_preview(DataJObj))
    end.

-type read_file_error() :: file:posix() | 'badarg' | 'terminated' | 'system_limit'.

-spec maybe_load_preview(ne_binary(), E, boolean()) ->
                                {'ok', wh_json:object()} |
                                {'error', read_file_error()} | E.
maybe_load_preview(_Type, Error, 'false') ->
    Error;
maybe_load_preview(Type, _Error, 'true') ->
    read_doc(Type).

-spec read_doc(ne_binary()) ->
                      {'ok', wh_json:object()} |
                      {'error', read_file_error()}.
read_doc(File) ->
    AppDir = code:lib_dir('teletype'),
    PreviewFile = filename:join([AppDir, "priv", "preview_data", <<File/binary, ".json">>]),
    case file:read_file(PreviewFile) of
        {'ok', JSON} ->
            lager:debug("read preview data from ~s: ~s", [PreviewFile, JSON]),
            {'ok', wh_json:decode(JSON)};
        {'error', _Reason}=E ->
            lager:debug("failed to read preview data from ~s: ~p", [PreviewFile, _Reason]),
            E
    end.

-spec is_preview(wh_json:object()) -> boolean().
is_preview(DataJObj) ->
    wh_util:is_true(
      wh_json:get_first_defined([<<"Preview">>, <<"preview">>], DataJObj, 'false')
     ).

-spec public_proplist(wh_json:key(), wh_json:object()) -> wh_proplist().
public_proplist(Key, JObj) ->
    wh_json:to_proplist(
      wh_json:public_fields(
        wh_json:get_value(Key, JObj, wh_json:new())
       )
     ).

-spec stop_processing(string(), list()) -> no_return().
stop_processing(Format, Args) ->
    lager:debug(Format, Args),
    exit('normal').
