%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_util).

-export([template_doc_id/1
         ,init_template/4
         ,fetch_template_meta/2
         ,fetch_templates/1, fetch_templates/2
         ,send_email/5, send_email/6
         ,render_subject/2, render/3
         ,service_params/2, service_params/3
         ,send_update/2, send_update/3
         ,find_account_rep_email/1
         ,find_account_admin_email/1
        ]).

-include("teletype.hrl").

-spec send_email(ne_binary(), wh_json:object(), wh_proplist(), iolist(), wh_proplist()) ->
                        'ok' |
                        {'error', _}.
-spec send_email(ne_binary(), wh_json:object(), wh_proplist(), iolist(), wh_proplist(), attachments()) ->
                        'ok' |
                        {'error', _}.
send_email(TemplateId, DataJObj, ServiceData, Subject, RenderedTemplates) ->
    send_email(TemplateId, DataJObj, ServiceData, Subject, RenderedTemplates, []).
send_email(TemplateId, DataJObj, ServiceData, Subject, RenderedTemplates, Attachments) ->
    {'ok', TemplateJObj} = fetch_template_meta(TemplateId, wh_json:get_value(<<"Account-ID">>, DataJObj)),

    L = [DataJObj, TemplateJObj],

    To = wh_json:find([<<"to">>, <<"email_addresses">>], L),
    Cc = wh_json:find([<<"cc">>, <<"email_addresses">>], L),
    Bcc = wh_json:find([<<"bcc">>, <<"email_addresses">>], L),

    From = wh_json:find(<<"from">>, [wh_json:from_list(ServiceData) | L]),
    ReplyTo = wh_json:find(<<"reply_to">>, [wh_json:from_list(ServiceData) | L]),

    Body = {<<"multipart">>, <<"alternative">>
            ,[] %% Headers
            ,[] %% ContentTypeParams
            ,add_rendered_templates_to_email(RenderedTemplates, ServiceData)
            ++ add_attachments(Attachments)
           },

    Email = {<<"multipart">>
             ,<<"mixed">>
             ,email_parameters(
                [{<<"To">>, To}
                 ,{<<"Cc">>, Cc}
                 ,{<<"Bcc">>, Bcc}
                ]
                ,[{<<"From">>, From}
                  ,{<<"Reply-To">>, ReplyTo}
                  ,{<<"Subject">>, iolist_to_binary(Subject)}
                  ,{<<"X-Call-ID">>, wh_json:get_value(<<"Call-ID">>, DataJObj)}
                 ]
               )
             ,service_content_type_params(ServiceData)
             ,[Body]
            },

    relay_email(To, From, Email).

-spec email_parameters(wh_proplist(), wh_proplist()) -> wh_proplist().
email_parameters([], Params) ->
    lists:reverse(props:filter_empty(Params));
email_parameters([{_Key, 'undefined'}|T], Params) ->
    email_parameters(T, Params);
email_parameters([{Key, Vs}|T], Params) ->
    email_parameters(T, [{Key, V} || V <- Vs] ++ Params).

-spec relay_email(ne_binaries(), ne_binary(), mimemail:mimetuple()) -> 'ok'.
relay_email(To, From, Email) ->
    try mimemail:encode(Email) of
        Encoded -> relay_encoded_email(To, From, Encoded)
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to encode email: ~s: ~p", [_E, _R]),
            wh_util:log_stacktrace(ST),
            throw({'error', 'email_encoding_failed'})
    end.

-spec relay_encoded_email(ne_binaries(), ne_binary(), iolist()) -> 'ok'.
relay_encoded_email(To, From, Encoded) ->
    ReqId = get('callid'),
    Self = self(),

    lager:debug("encoded: ~s", [Encoded]),
    gen_smtp_client:send({From, To, Encoded}
                         ,smtp_options()
                         ,fun(X) ->
                                  put('callid', ReqId),
                                  lager:debug("email relay responded: ~p, send to ~p", [X, Self]),
                                  Self ! {'relay_response', X}
                          end),
    %% The callback will receive either `{ok, Receipt}' where Receipt is the SMTP server's receipt
    %% identifier,  `{error, Type, Message}' or `{exit, ExitReason}', as the single argument.
    receive
        {'relay_response', {'ok', _Msg}} -> lager:debug("relayed message: ~p", [_Msg]);
        {'relay_response', {'error', _Type, Message}} ->
            lager:debug("error relyaing message: ~p: ~p", [_Type, Message]),
            {'error', Message};
        {'relay_response', {'exit', Reason}} ->
            lager:debug("exit relaying message: ~p", [Reason]),
            {'error', Reason}
    after 10000 ->
            lager:debug("timed out waiting for relay response"),
            {'error', 'timeout'}
    end.

-spec smtp_options() -> wh_proplist().
smtp_options() ->
    Relay = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"relay">>, <<"localhost">>)),
    Username = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"username">>)),
    Password = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"password">>)),
    Auth = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"auth">>, <<"never">>)),
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

-type mime_tuples() :: [mimemail:mimetuple(),...] | [].

-type attachment() :: {ne_binary(), ne_binary(), binary()}.
-type attachments() :: [attachment(),...] | [].

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
                    ,{<<"Content-Tranfer-Encoding">>, <<"base64">>}
                   ]
                  ,[]
                  ,Content
                 },
    add_attachments(As, [Attachment | Acc]).

-spec add_rendered_templates_to_email(wh_proplist(), wh_proplist()) -> mime_tuples().
add_rendered_templates_to_email(RenderedTemplates, ServiceData) ->
    add_rendered_templates_to_email(RenderedTemplates, service_charset(ServiceData), []).

-spec add_rendered_templates_to_email(wh_proplist(), binary(), mime_tuples()) -> mime_tuples().
add_rendered_templates_to_email([], _Charset, Acc) -> Acc;
add_rendered_templates_to_email([{ContentType, Content}|Rs], Charset, Acc) ->
    [Type, SubType] = binary:split(ContentType, <<"/">>),
    CTEncoding = whapps_config:get_ne_binary(?NOTIFY_CONFIG_CAT, <<SubType/binary, "_content_transfer_encoding">>),

    Template = {Type
                ,SubType
                ,props:filter_undefined(
                   [{<<"Content-Type">>, iolist_to_binary([ContentType, Charset])}
                    ,{<<"Content-Transfer-Encoding">>, CTEncoding}
                   ])
                ,[]
                ,iolist_to_binary(Content)
               },
    add_rendered_templates_to_email(Rs, Charset, [Template | Acc]).

-spec service_content_type_params(wh_proplist()) -> wh_proplist().
service_content_type_params(ServiceData) ->
    case props:get_value(<<"template_charset">>, ServiceData) of
        <<>> -> [];
        <<_/binary>> = Charset ->
            [{<<"content-type-params">> ,[{<<"charset">>, Charset}]}];
        _ -> []
    end.

-spec service_charset(wh_proplist()) -> binary().
service_charset(ServiceData) ->
    case props:get_value(<<"template_charset">>, ServiceData) of
        <<>> -> <<>>;
        <<_/binary>> = Charset ->  <<";charset=", Charset/binary>>;
        _ -> <<>>
    end.

-spec service_params(wh_json:object(), ne_binary()) -> wh_proplist().
-spec service_params(wh_json:object(), ne_binary(), api_binary()) -> wh_proplist().
service_params(APIJObj, ConfigCat) ->
    service_params(APIJObj, ConfigCat, wh_json:get_value(<<"account_id">>, APIJObj)).
service_params(APIJObj, ConfigCat, AccountId) ->
    {'ok', AccountJObj} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded')
                                                   ,AccountId
                                                  ),

    NotificationJObj = find_notification_settings(
                         binary:split(ConfigCat, <<".">>)
                         ,kz_account:parent_account_id(AccountJObj)
                        ),
    [{<<"url">>, wh_json:get_value(<<"service_url">>, NotificationJObj, default_service_url(APIJObj, ConfigCat))}
     ,{<<"name">>, wh_json:get_value(<<"service_name">>, NotificationJObj, default_service_name(APIJObj, ConfigCat))}
     ,{<<"provider">>, wh_json:get_value(<<"service_provider">>, NotificationJObj, default_service_provider(APIJObj, ConfigCat))}
     ,{<<"support_number">>, wh_json:get_value(<<"support_number">>, NotificationJObj, default_support_number(APIJObj, ConfigCat))}
     ,{<<"support_email">>, wh_json:get_value(<<"support_email">>, NotificationJObj, default_support_email(APIJObj, ConfigCat))}
     ,{<<"from">>, wh_json:get_value(<<"send_from">>, NotificationJObj, default_from_address(APIJObj, ConfigCat))}
     ,{<<"template_charset">>, wh_json:get_value(<<"template_charset">>, NotificationJObj, default_charset(APIJObj, ConfigCat))}
     ,{<<"host">>, wh_util:to_binary(net_adm:localhost())}
    ].

-spec find_notification_settings(ne_binaries(), api_binary()) -> wh_json:object().
find_notification_settings(_, 'undefined') -> wh_json:new();
find_notification_settings([_, Module], AccountId) ->
    case couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded')
                                  ,AccountId
                                 )
    of
        {'error', _E} -> wh_json:new();
        {'ok', AccountJObj} ->
            lager:debug("looking for notifications '~s' service info in: ~s"
                        ,[Module, AccountId]
                       ),
            case wh_json:get_ne_value([<<"notifications">>, Module], AccountJObj) of
                'undefined' -> maybe_find_deprecated_settings(Module, AccountJObj);
                Settings -> Settings
            end
    end.

-spec maybe_find_deprecated_settings(ne_binary(), wh_json:object()) -> wh_json:object().
maybe_find_deprecated_settings(<<"fax_inbound_to_email">>, JObj) ->
    wh_json:get_ne_value([<<"notifications">>, <<"fax_to_email">>], JObj, wh_json:new());
maybe_find_deprecated_settings(<<"fax_outbound_to_email">>, JObj) ->
    wh_json:get_ne_value([<<"notifications">>, <<"fax_to_email">>], JObj, wh_json:new());
maybe_find_deprecated_settings(<<"fax_outbound_error_to_email">>, JObj) ->
    wh_json:get_ne_value([<<"notifications">>, <<"fax_to_email">>], JObj, wh_json:new());
maybe_find_deprecated_settings(<<"fax_inbound_error_to_email">>, JObj) ->
    wh_json:get_ne_value([<<"notifications">>, <<"fax_to_email">>], JObj, wh_json:new());
maybe_find_deprecated_settings(_, _) -> wh_json:new().

-spec default_service_url(wh_json:object(), ne_binary()) -> ne_binary().
default_service_url(JObj, ConfigCat) ->
    default_service_value(JObj, ConfigCat
                          ,<<"service_url">>, <<"default_service_url">>, <<"https://apps.2600hz.com">>
                         ).

-spec default_service_name(wh_json:object(), ne_binary()) -> ne_binary().
default_service_name(JObj, ConfigCat) ->
    default_service_value(JObj, ConfigCat
                          ,<<"service_name">>, <<"default_service_name">>, <<"VoIP Services">>
                         ).

-spec default_service_provider(wh_json:object(), ne_binary()) -> ne_binary().
default_service_provider(JObj, ConfigCat) ->
    default_service_value(JObj, ConfigCat
                          ,<<"service_provider">>, <<"default_service_provider">>, <<"2600Hz">>
                         ).

-spec default_support_number(wh_json:object(), ne_binary()) -> ne_binary().
default_support_number(JObj, ConfigCat) ->
    default_service_value(JObj, ConfigCat
                          ,<<"support_number">>, <<"default_support_number">>, <<"(415) 886-7900">>
                         ).

-spec default_support_email(wh_json:object(), ne_binary()) -> ne_binary().
default_support_email(JObj, ConfigCat) ->
    default_service_value(JObj, ConfigCat
                          ,<<"support_email">>, <<"default_support_email">>, <<"support@2600hz.com">>
                         ).

-spec default_from_address(wh_json:object(), ne_binary()) -> ne_binary().
default_from_address(JObj, ConfigCat) ->
    default_service_value(JObj, ConfigCat
                          ,<<"send_from">>, <<"default_from">>
                          ,list_to_binary([<<"no_reply@">>, net_adm:localhost()])
                         ).

-spec default_charset(wh_json:object(), ne_binary()) -> binary().
default_charset(JObj, ConfigCat) ->
    default_service_value(JObj, ConfigCat
                          ,<<"template_charset">>, <<"default_template_charset">>, <<>>
                         ).

-spec default_service_value(wh_json:object(), ne_binary(), wh_json:key(), wh_json:key(), wh_json:json_term()) ->
                                   wh_json:json_term().
default_service_value(JObj, ConfigCat, JSONKey, ConfigKey, ConfigDefault) ->
    case wh_json:get_ne_value(JSONKey, JObj) of
        'undefined' ->
            whapps_config:get(ConfigCat, ConfigKey, ConfigDefault);
        Value -> Value
    end.

-spec render_subject(api_binary(), wh_proplist()) -> api_binary().
render_subject('undefined', _Macros) -> <<"Default Subject">>;
render_subject(Template, Macros) -> render(<<"subject">>, Template, Macros).

-spec render(ne_binary(), binary(), wh_proplist()) -> iodata() | 'undefined'.
render(TemplateId, Template, Macros) ->
    case teletype_render_farm_sup:render(TemplateId, Template, Macros) of
        {'ok', IOData} -> IOData;
        {'error', _E} ->
            lager:debug("failed to render template: ~p '~s'", [_E, Template]),
            'undefined'
    end.

-spec template_doc_id(ne_binary()) -> ne_binary().
template_doc_id(<<"notification.", _/binary>> = ID) -> ID;
template_doc_id(<<_/binary>> = ID) -> <<"notification.", ID/binary>>.

-spec init_template(ne_binary(), wh_json:object(), binary(), binary()) -> 'ok'.
init_template(Id, Macros, Text, HTML) ->
    DocId = template_doc_id(Id),
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),

    Attachments = [{<<"text/plain">>, Text}
                   ,{<<"text/html">>, HTML}
                  ],

    lager:debug("looking for ~s", [DocId]),
    case couch_mgr:open_doc(MasterAccountDb, DocId) of
        {'ok', TemplateJObj} -> update_template(TemplateJObj, Macros, Attachments);
        {'error', 'not_found'} -> create_template(MasterAccountDb, DocId, Macros, Attachments);
        {'error', _E} -> lager:warning("failed to find template ~s", [DocId])
    end.

-spec create_template(ne_binary(), ne_binary(), wh_json:object(), wh_proplist()) ->
                             'ok' |
                             couch_mgr:couchbeam_error().
create_template(MasterAccountDb, DocId, Macros, Attachments) ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),

    lager:debug("attempting to create template ~s", [DocId]),
    Doc = wh_doc:update_pvt_parameters(
            wh_json:from_list(
              [{<<"_id">>, DocId}
               ,{<<"macros">>, Macros}
              ])
            ,MasterAccountDb
            ,[{'account_db', MasterAccountDb}
              ,{'account_id', MasterAccountId}
              ,{'type', ?PVT_TYPE}
             ]),
    case couch_mgr:save_doc(MasterAccountDb, Doc) of
        {'ok', _UpdatedDoc} ->
            lager:debug("created template ~s", [DocId]),
            create_template_attachments(MasterAccountDb
                                        ,DocId
                                        ,Attachments
                                       );
        {'error', _E}=E ->
            lager:debug("failed to create template ~s: ~p", [DocId, _E]),
            E
    end.

create_template_attachments(_MasterAccountDb, _DocId, []) -> 'ok';
create_template_attachments(MasterAccountDb, DocId, [{_ContentType, <<>>} | As]) ->
    create_template_attachments(MasterAccountDb, DocId, As);
create_template_attachments(MasterAccountDb, DocId, [{ContentType, Contents} | As]) ->
    AName = template_attachment_name(ContentType),

    case couch_mgr:put_attachment(MasterAccountDb
                                  ,DocId
                                  ,AName
                                  ,Contents
                                  ,[{'content_type', wh_util:to_list(ContentType)}]
                                 )
    of
        {'ok', _Doc} ->
            lager:debug("saved attachment ~s for ~s", [AName, DocId]),
            create_template_attachments(MasterAccountDb, DocId, As);
        {'error', 'conflict'} ->
            case does_attachment_exist(MasterAccountDb, DocId, AName) of
                'true' ->
                    lager:debug("template attachment ~s exists", [AName]),
                    create_template_attachments(MasterAccountDb, DocId, As);
                'false' ->
                    lager:debug("uploading ~s appears to have failed for ~s", [AName, DocId]),
                    create_template_attachments(MasterAccountDb, DocId, As)
            end;
        {'error', _E} ->
            lager:debug("uploading ~s appears to have failed for ~s: ~p", [AName, DocId, _E]),
            create_template_attachments(MasterAccountDb, DocId, As)
    end.

-spec template_attachment_name(ne_binary()) -> ne_binary().
template_attachment_name(ContentType) ->
    wh_util:clean_binary(<<"template.", (cow_qs:urlencode(ContentType))/binary>>).

-spec does_attachment_exist(ne_binary(), ne_binary(), ne_binary()) -> boolean().
does_attachment_exist(MasterAccountDb, DocId, AName) ->
    case couch_mgr:open_doc(MasterAccountDb, DocId) of
        {'ok', JObj} ->
            does_attachment_exist(JObj, AName);
        {'error', _E} ->
            lager:debug("failed to open ~s to check for ~s: ~p", [DocId, AName, _E]),
            'false'
    end.

-spec does_attachment_exist(wh_json:object(), ne_binary()) -> boolean().
does_attachment_exist(JObj, AName) ->
    wh_doc:attachment(JObj, cow_qs:urldecode(AName)) =/= 'undefined'.

-spec update_template(wh_json:object(), wh_json:object(), wh_proplist()) ->
                                   'ok' | couch_mgr:couchbeam_error().
update_template(TemplateJObj, MacroJObj, Attachments) ->
    {HasUpdates, JObj} =
        wh_json:foldl(fun maybe_update_template_fold/3
                      ,{'false', TemplateJObj}
                      ,MacroJObj
                     ),
    maybe_update_template_with_changes(JObj, HasUpdates, Attachments).

-spec maybe_update_template_with_changes(wh_json:object(), boolean(), wh_proplist()) ->
                                                'ok' | couch_mgr:couchbeam_error().
maybe_update_template_with_changes(JObj, 'false', Attachments) ->
    lager:debug("no updates to the macros found"),
    maybe_update_attachments(JObj, Attachments);
maybe_update_template_with_changes(JObj, 'true', Attachments) ->
    lager:debug("updating to ~p", [JObj]),
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    case couch_mgr:save_doc(MasterAccountDb, JObj) of
        {'ok', UpdatedJObj} ->
            lager:debug("updated template with latest macros"),
            maybe_update_attachments(UpdatedJObj, Attachments);
        {'error', _E}=E -> E
    end.

-spec maybe_update_template_fold(wh_json:key(), wh_json:json_term(), {boolean(), wh_json:object()}) ->
                                        {boolean(), wh_json:object()}.
maybe_update_template_fold(MacroKey, MacroValue, {_HU, JObj}=Acc) ->
    case wh_json:get_value([<<"macros">>, MacroKey], JObj) of
        'undefined' ->
            lager:debug("adding macro ~s to template", [MacroKey]),
            {'true', wh_json:set_value([<<"macros">>, MacroKey], MacroValue, JObj)};
        _Value -> Acc
    end.

-spec maybe_update_attachments(wh_json:object(), wh_proplist()) -> 'ok'.
maybe_update_attachments(_JObj, []) ->
    lager:debug("finished checking attachments");
maybe_update_attachments(JObj, [{_ContentType, <<>>} | As]) ->
    maybe_update_attachments(JObj, As);
maybe_update_attachments(JObj, [{ContentType, Contents}|As]) ->
    AName = template_attachment_name(ContentType),

    case does_attachment_exist(JObj, AName) of
        'true' ->
            maybe_update_attachments(JObj, As);
        'false' ->
            DocId = wh_json:get_first_defined([<<"id">>,<<"_id">>], JObj),
            case save_attachment(DocId, AName, ContentType, Contents) of
                'true' ->
                    lager:debug("saved attachment ~s", [AName]);
                'false' ->
                    lager:debug("failed to save attachment ~s", [AName])
            end,
            maybe_update_attachments(JObj, As)
    end.

-spec save_attachment(ne_binary(), ne_binary(), ne_binary(), binary()) -> boolean().
save_attachment(DocId, AName, ContentType, Contents) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),

    case couch_mgr:put_attachment(MasterAccountDb
                                  ,DocId
                                  ,AName
                                  ,Contents
                                  ,[{'content_type', wh_util:to_list(ContentType)}]
                                 )
    of
        {'ok', _UpdatedJObj} ->
            lager:debug("added attachment ~s to ~s", [AName, DocId]),
            'true';
        {'error', 'conflict'} ->
            case does_attachment_exist(MasterAccountDb, DocId, AName) of
                'true' ->
                    lager:debug("added attachment ~s to ~s", [AName, DocId]),
                    'true';
                'false' ->
                    lager:debug("failed to add attachment ~s to ~s", [AName, DocId]),
                    'false'
            end;
        {'error', _E} ->
            lager:debug("failed to add attachment ~s to ~s: ~p", [AName, DocId, _E]),
            'false'
    end.

-spec fetch_template_meta(ne_binary(), api_binary()) ->
                                 {'ok', wh_json:object()} |
                                 couch_mgr:couchbeam_error().
-spec fetch_template_meta(ne_binary(), api_binary(), ne_binary()) ->
                                 {'ok', wh_json:object()} |
                                 couch_mgr:couchbeam_error().
fetch_template_meta(TemplateId, AccountId) ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    fetch_template_meta(TemplateId, AccountId, MasterAccountId).

fetch_template_meta(TemplateId, 'undefined', _MasterAccountId) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    couch_mgr:open_cache_doc(MasterAccountDb, template_doc_id(TemplateId));
fetch_template_meta(TemplateId, MasterAccountId, MasterAccountId) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    couch_mgr:open_cache_doc(MasterAccountDb, template_doc_id(TemplateId));
fetch_template_meta(TemplateId, AccountId, MasterAccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, template_doc_id(TemplateId)) of
        {'ok', _TemplateJObj}=OK -> OK;
        {'error', 'not_found'} ->
            fetch_template_meta(TemplateId, wh_services:find_reseller_id(AccountId), MasterAccountId);
        {'error', _E} ->
            lager:debug("failed to fetch template ~s from ~s", [TemplateId, AccountId]),
            fetch_template_meta(TemplateId, MasterAccountId, MasterAccountId)
    end.

-spec fetch_templates(api_binary()) -> wh_proplist().
-spec fetch_templates(api_binary(), api_binary() | wh_json:object()) -> wh_proplist().
-spec fetch_templates(ne_binary(), ne_binary(), wh_json:object()) -> wh_proplist().
fetch_templates(TemplateId) ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    fetch_templates(TemplateId, MasterAccountId).

fetch_templates(_TemplateId, 'undefined') ->
    lager:debug("no account id for ~s, no template available", [_TemplateId]),
    [];
fetch_templates(TemplateId, <<_/binary>> = AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    DocId = template_doc_id(TemplateId),
    case couch_mgr:open_cache_doc(AccountDb, DocId) of
        {'ok', TemplateJObj} ->
            fetch_templates(DocId
                            ,AccountDb
                            ,wh_doc:attachments(TemplateJObj, wh_json:new())
                           );
        {'error', 'not_found'} ->
            maybe_fetch_reseller_templates(TemplateId, AccountId);
        {'error', _E} ->
            lager:debug("failed to fetch template ~s from ~s", [TemplateId, AccountId]),
            []
    end;
fetch_templates(TemplateId, DataJObj) ->
    case props:filter_undefined(
           [{<<"text/html">>, maybe_decode_html(wh_json:get_value(<<"html">>, DataJObj))}
            ,{<<"text/plain">>, wh_json:get_value(<<"text">>, DataJObj)}
           ])
    of
        [] ->
            fetch_templates(TemplateId, wh_json:get_value(<<"account_id">>, DataJObj));
        Templates -> Templates
    end.

-spec maybe_decode_html(api_binary()) -> api_binary().
maybe_decode_html('undefined') -> 'undefined';
maybe_decode_html(HTML) ->
    try base64:decode(HTML) of
        Decoded -> Decoded
    catch
        _E:'badarg' -> HTML;
        _E:_R ->
            lager:debug("failed to decode HTML ~s: ~p", [_E, _R]),
            'undefined'
    end.

fetch_templates(TemplateId, AccountDb, Attachments) ->
    props:filter_undefined(
      [fetch_template(TemplateId, AccountDb, Attachment)
       || Attachment <- wh_json:to_proplist(Attachments)
      ]).

-spec maybe_fetch_reseller_templates(ne_binary(), ne_binary()) -> wh_proplist().
maybe_fetch_reseller_templates(TemplateId, AccountId) ->
    ResellerAccountId = wh_services:find_reseller_id(AccountId),
    lager:debug("failed to find ~s in ~s, checking reseller ~s", [TemplateId, AccountId, ResellerAccountId]),
    fetch_templates(TemplateId, ResellerAccountId).

-spec fetch_template(ne_binary(), ne_binary(), {wh_json:key(), wh_json:object()}) ->
                            {ne_binary(), binary()} |
                            'undefined'.
fetch_template(TemplateId, AccountDb, {AName, Properties}) ->
    case couch_mgr:fetch_attachment(AccountDb, TemplateId, AName) of
        {'ok', Contents} ->
            {wh_json:get_value(<<"content_type">>, Properties), Contents};
        {'error', _E} ->
            lager:debug("failed to load attachment ~s from ~s(~s): ~p", [AName, TemplateId, AccountDb, _E]),
            'undefined'
    end.

-spec send_update(wh_json:object(), ne_binary()) -> 'ok'.
-spec send_update(wh_json:object(), ne_binary(), api_binary()) -> 'ok'.
-spec send_update(api_binary(), ne_binary(), ne_binary(), api_binary()) -> 'ok'.
send_update(DataJObj, Status) ->
    send_update(DataJObj, Status, 'undefined').
send_update(DataJObj, Status, Message) ->
    send_update(wh_json:get_value(<<"server_id">>, DataJObj)
                ,wh_json:get_value(<<"msg_id">>, DataJObj)
                ,Status
                ,Message
               ).

send_update('undefined', _, _, _) -> 'ok';
send_update(RespQ, MsgId, Status, Msg) ->
    Prop = props:filter_undefined(
             [{<<"Status">>, Status}
              ,{<<"Failure-Message">>, Msg}
              ,{<<"Msg-ID">>, MsgId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("notification update (~s) sending to ~s", [Status, RespQ]),
    wapi_notifications:publish_notify_update(RespQ, Prop).

-spec find_account_rep_email(api_object() | ne_binary()) -> api_binaries().
-spec find_account_rep_email(ne_binary(), api_binary()) -> api_binaries().
-spec find_account_rep_email(ne_binary(), ne_binary(), wh_json:object()) -> api_binaries().
find_account_rep_email('undefined') -> 'undefined';
find_account_rep_email(<<_/binary>> = AccountId) ->
    case wh_services:find_reseller_id(AccountId) of
        'undefined' -> 'undefined';
        ResellerId -> find_account_rep_email(AccountId, ResellerId)
    end;
find_account_rep_email(AccountJObj) ->
    find_account_rep_email(
      wh_json:get_first_defined([<<"_id">>, <<"id">>, <<"pvt_account_id">>]
                                ,AccountJObj
                               )
     ).

find_account_rep_email(AccountId, 'undefined') ->
    find_account_admin_email(AccountId);
find_account_rep_email(AccountId, ResellerId) ->
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    ViewOptions = ['include_docs', {'key', AccountId}],
    case couch_mgr:open_cache_doc(ResellerDb, <<"sub_account_reps/find_assignments">>, ViewOptions) of
        {'ok', [View|_]} ->
            find_account_rep_email(AccountId, ResellerId, View);
        {'ok', []} ->
            find_account_rep_email(ResellerId, wh_services:find_reseller_id(ResellerId));
        {'error', _} ->
            find_account_rep_email(ResellerId, wh_services:find_reseller_id(ResellerId))
    end.

find_account_rep_email(_AccountId, ResellerId, View) ->
    case wh_json:get_value([<<"doc">>, <<"email">>], View) of
        'undefined' ->
            find_account_admin_email(ResellerId);
        Email -> [Email]
    end.

-spec find_account_admin_email(api_binary()) -> api_binaries().
-spec find_account_admin_email(ne_binary(), wh_json:objects()) -> api_binaries().
find_account_admin_email('undefined') -> 'undefined';
find_account_admin_email(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'key', <<"user">>}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', Users} ->
            find_account_admin_email(AccountId, Users);
        {'error', _E} ->
            find_account_admin_email(wh_services:find_reseller_id(AccountId))
    end.

find_account_admin_email(AccountId, Users) ->
    case filter_for_admins(Users) of
        [] ->
            find_account_admin_email(wh_services:find_reseller_id(AccountId));
        Admins ->
            [wh_json:get_value([<<"doc">>, <<"email">>], Admin) || Admin <- Admins]
    end.

-spec filter_for_admins(wh_json:objects()) -> wh_json:objects().
filter_for_admins(Users) ->
    [User
     || User <- Users,
        wh_json:get_value([<<"doc">>, <<"priv_level">>], User) =:= <<"admin">>,
        wh_json:get_ne_value([<<"doc">>, <<"email">>], User) =/= 'undefined'
    ].
