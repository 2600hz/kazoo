%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @docg
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_util).

-export([send_email/3
        ,send_update/3, send_update/4
        ,maybe_send_update/3
        ,render_template/3
        ,normalize_proplist/1
        ,json_to_template_props/1
        ,get_service_props/2, get_service_props/3
        ,get_rep_email/1
        ,compile_default_text_template/2
        ,compile_default_html_template/2
        ,compile_default_subject_template/2
        ,compile_default_template/3
        ,find_admin/1
        ,get_account_doc/1
        ,qr_code_image/1
        ,get_charset_params/1
        ]).

-include("notify.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec send_email(ne_binary(), api_binary(), any()) -> 'ok' | {'error', any()}.
send_email(_, 'undefined', _) -> lager:debug("no email to send to");
send_email(_, <<>>, _) -> lager:debug("empty email to send to");
send_email(From, To, Email) ->
    Encoded = mimemail:encode(Email),
    Relay = kz_term:to_list(kapps_config:get_ne_binary(<<"smtp_client">>, <<"relay">>, <<"localhost">>)),
    Username = kz_term:to_list(kapps_config:get_binary(<<"smtp_client">>, <<"username">>, <<>>)),
    Password = kz_term:to_list(kapps_config:get_binary(<<"smtp_client">>, <<"password">>, <<>>)),
    Auth = kz_term:to_list(kapps_config:get_ne_binary(<<"smtp_client">>, <<"auth">>, <<"never">>)),
    Port = kapps_config:get_integer(<<"smtp_client">>, <<"port">>, 25),

    lager:debug("sending email to ~s from ~s via ~s", [To, From, Relay]),
    ReqId = get('callid'),

    Self = self(),

    gen_smtp_client:send({From, [To], Encoded}
                        ,[{'relay', Relay}
                         ,{'username', Username}
                         ,{'password', Password}
                         ,{'port', Port}
                         ,{'auth', Auth}
                         ]
                        ,fun(X) ->
                                 kz_util:put_callid(ReqId),
                                 lager:debug("email relay responded: ~p, send to ~p", [X, Self]),
                                 Self ! {'relay_response', X}
                         end),
    %% The callback will receive either `{ok, Receipt}' where Receipt is the SMTP server's receipt
    %% identifier,  `{error, Type, Message}' or `{exit, ExitReason}', as the single argument.
    receive
        {'relay_response', {'ok', _Msg}} -> 'ok';
        {'relay_response', {'error', _Type, Message}} -> {'error', Message};
        {'relay_response', {'exit', Reason}} -> {'error', Reason}
    after 10 * ?MILLISECONDS_IN_SECOND -> {'error', 'timeout'}
    end.

-spec send_update(api_binary(), ne_binary(), ne_binary()) -> 'ok'.
-spec send_update(api_binary(), ne_binary(), ne_binary(), api_binary()) -> 'ok'.
send_update(RespQ, MsgId, Status) ->
    send_update(RespQ, MsgId, Status, 'undefined').
send_update('undefined', _, _, _) -> lager:debug("no response queue to send update");
send_update(RespQ, MsgId, Status, Msg) ->
    Prop = props:filter_undefined(
             [{<<"Status">>, Status}
             ,{<<"Failure-Message">>, Msg}
             ,{<<"Msg-ID">>, MsgId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("notification update (~s) sending to ~s", [Status, RespQ]),
    kz_amqp_worker:cast(Prop, fun(P) -> kapi_notifications:publish_notify_update(RespQ, P) end).

-spec maybe_send_update(send_email_return(), ne_binary(), ne_binary()) -> 'ok'.
maybe_send_update('ok', RespQ, MsgId) -> send_update(RespQ, MsgId, <<"completed">>);
maybe_send_update({'error', Reason}, RespQ, MsgId) -> send_update(RespQ, MsgId, <<"failed">>, Reason);
maybe_send_update([LastResp|_]=Responses, RespQ, MsgId) ->
    case lists:any(fun('ok') -> 'true';
                      ({'error', _}) -> 'false';
                      ('disabled') -> 'true'
                   end
                  ,Responses
                  )
    of
        'true' -> send_update(RespQ, MsgId, <<"completed">>);
        'false' ->
            {'error', Reason} = LastResp,
            send_update(RespQ, MsgId, <<"failed">>, Reason)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec json_to_template_props(api_object() | kz_json:objects()) -> 'undefined' | kz_proplist().
json_to_template_props('undefined') -> 'undefined';
json_to_template_props(JObj) ->
    normalize_proplist(kz_json:recursive_to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_proplist(kz_proplist()) -> kz_proplist().
normalize_proplist(Props) ->
    [normalize_proplist_element(Elem) || Elem <- Props].

normalize_proplist_element({K, V}) when is_list(V) ->
    {normalize_value(K), normalize_proplist(V)};
normalize_proplist_element({K, V}) when is_binary(V) ->
    {normalize_value(K), kz_html:escape(V)};
normalize_proplist_element({K, V}) ->
    {normalize_value(K), V};
normalize_proplist_element(Else) ->
    Else.

normalize_value(Value) ->
    binary:replace(kz_term:to_lower_binary(Value), <<"-">>, <<"_">>, ['global']).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_default_text_template(atom(), ne_binary()) -> {'ok', atom()}.
-spec compile_default_html_template(atom(), ne_binary()) -> {'ok', atom()}.
-spec compile_default_subject_template(atom(), ne_binary()) -> {'ok', atom()}.
-spec compile_default_template(atom(), ne_binary(), atom()) -> {'ok', atom()}.

compile_default_text_template(TemplateModule, Category) ->
    compile_default_template(TemplateModule, Category, 'default_text_template').

compile_default_html_template(TemplateModule, Category) ->
    compile_default_template(TemplateModule, Category, 'default_html_template').

compile_default_subject_template(TemplateModule, Category) ->
    compile_default_template(TemplateModule, Category, 'default_subject_template').

compile_default_template(TemplateModule, Category, Key) ->
    Template = case kapps_config:get_ne_binary(Category, Key) of
                   'undefined' -> get_default_template(Category, Key);
                   Else -> Else
               end,
    lager:debug("compiling ~s: '~s'", [TemplateModule, Template]),
    {'ok', TemplateModule} = kz_template:compile(Template, TemplateModule).

get_default_template(Category, Key) ->
    File = category_to_file(Category),
    case file:consult(File) of
        {'ok', Props} ->
            case props:get_value(Key, Props) of
                'undefined' -> 'undefined';
                Template ->
                    lager:debug("loading ~s from file ~s", [Key, File]),
                    _ = kapps_config:set(Category, Key, Template),
                    Template
            end;
        {'error', _R} ->
            lager:warning("failed to find default template for ~s/~s: ~p", [Category, Key, _R]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec render_template(api_binary(), atom(), kz_proplist()) ->
                             {'ok', string()} |
                             {'error', any()}.
render_template(Template, DefaultTemplate, Props) ->
    case do_render_template(Template, DefaultTemplate, Props) of
        {'ok', R} -> {'ok', binary_to_list(iolist_to_binary(R))};
        Else -> Else
    end.

-spec do_render_template(api_binary(), atom(), kz_proplist()) ->
                                {'ok', string()} |
                                {'error', any()}.
do_render_template('undefined', DefaultTemplate, Props) ->
    lager:debug("rendering default ~s template", [DefaultTemplate]),
    kz_template:render(DefaultTemplate, Props);
do_render_template(Template, DefaultTemplate, Props) ->
    try
        'false' = kz_term:is_empty(Template),
        CustomTemplate = kz_term:to_atom(list_to_binary([kz_datamgr:get_uuid(), "_"
                                                        ,kz_term:to_binary(DefaultTemplate)
                                                        ])
                                        ,'true'),
        lager:debug("compiling custom ~s template", [DefaultTemplate]),
        {'ok', CustomTemplate} = kz_template:compile(Template, CustomTemplate),

        lager:debug("rendering custom template ~s", [CustomTemplate]),
        Result = kz_template:render(CustomTemplate, Props),

        code:purge(CustomTemplate),
        code:delete(CustomTemplate),
        Result
    catch
        _:_E ->
            lager:debug("error compiling custom ~s template: ~p", [DefaultTemplate, _E]),
            do_render_template('undefined', DefaultTemplate, Props)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% determine the service name, provider, and url. Hunts (in order)
%% in the event, parent account notification object, and then default.
%% @end
%%--------------------------------------------------------------------
-spec get_service_props(kz_json:object(), ne_binary()) -> kz_proplist().
-spec get_service_props(kz_json:object(), kz_json:object(), ne_binary()) -> kz_proplist().

get_service_props(Account, ConfigCat) ->
    get_service_props(kz_json:new(), Account, ConfigCat).

get_service_props(Request, Account, ConfigCat) ->
    DefaultUrl = kz_json:get_ne_value(<<"service_url">>, Request
                                     ,kapps_config:get_ne_binary(ConfigCat, <<"default_service_url">>, <<"http://apps.2600hz.com">>)),
    DefaultName = kz_json:get_ne_value(<<"service_name">>, Request
                                      ,kapps_config:get_ne_binary(ConfigCat, <<"default_service_name">>, <<"VOIP Services">>)),
    DefaultProvider = kz_json:get_ne_value(<<"service_provider">>, Request
                                          ,kapps_config:get_ne_binary(ConfigCat, <<"default_service_provider">>, <<"2600hz">>)),
    DefaultNumber = kz_json:get_ne_value(<<"support_number">>, Request
                                        ,kapps_config:get_ne_binary(ConfigCat, <<"default_support_number">>, <<"(415) 886-7900">>)),
    DefaultEmail = kz_json:get_ne_value(<<"support_email">>, Request
                                       ,kapps_config:get_ne_binary(ConfigCat, <<"default_support_email">>, <<"support@2600hz.com">>)),
    UnconfiguredFrom = list_to_binary([<<"no_reply@">>, kz_term:to_binary(net_adm:localhost())]),
    DefaultFrom = kz_json:get_ne_value(<<"send_from">>, Request
                                      ,kapps_config:get_ne_binary(ConfigCat, <<"default_from">>, UnconfiguredFrom)),
    DefaultCharset = kz_json:get_ne_value(<<"template_charset">>, Request
                                         ,kapps_config:get_binary(ConfigCat, <<"default_template_charset">>, <<>>)),
    JObj = find_notification_settings(binary:split(ConfigCat, <<".">>), kz_account:tree(Account)),
    [{<<"url">>, kz_json:get_value(<<"service_url">>, JObj, DefaultUrl)}
    ,{<<"name">>, kz_json:get_value(<<"service_name">>, JObj, DefaultName)}
    ,{<<"provider">>, kz_json:get_value(<<"service_provider">>, JObj, DefaultProvider)}
    ,{<<"support_number">>, kz_json:get_value(<<"support_number">>, JObj, DefaultNumber)}
    ,{<<"support_email">>, kz_json:get_value(<<"support_email">>, JObj, DefaultEmail)}
    ,{<<"send_from">>, kz_json:get_value(<<"send_from">>, JObj, DefaultFrom)}
    ,{<<"template_charset">>, kz_json:get_value(<<"template_charset">>, JObj, DefaultCharset)}
    ,{<<"host">>, kz_term:to_binary(net_adm:localhost())}
    ].

-spec find_notification_settings(ne_binaries() | ne_binary(), ne_binaries()) -> kz_json:object().
find_notification_settings(_, []) ->
    lager:debug("unable to get service props, pvt_tree for the account was empty", []),
    kz_json:new();
find_notification_settings([_, Module], Tree) ->
    case kz_datamgr:open_cache_doc(?KZ_ACCOUNTS_DB, lists:last(Tree)) of
        {'error', _} -> kz_json:new();
        {'ok', JObj} ->
            lager:debug("looking for notifications '~s' service info in: ~s"
                       ,[Module, kz_doc:id(JObj)]),
            case kz_json:get_ne_value([<<"notifications">>, Module], JObj) of
                'undefined' -> maybe_find_deprecated_settings(Module, JObj);
                Settings -> Settings
            end
    end;
find_notification_settings(_ConfigCat, _) ->
    lager:debug("unable to get service props, unexpected configuration category: ~p"
               ,[_ConfigCat]),
    kz_json:new().

-spec maybe_find_deprecated_settings(ne_binary(), kz_json:object()) -> kz_json:object().
maybe_find_deprecated_settings(<<"fax_inbound_to_email">>, JObj) ->
    kz_json:get_ne_value([<<"notifications">>, <<"fax_to_email">>], JObj, kz_json:new());
maybe_find_deprecated_settings(<<"fax_outbound_to_email">>, JObj) ->
    kz_json:get_ne_value([<<"notifications">>, <<"fax_to_email">>], JObj, kz_json:new());
maybe_find_deprecated_settings(<<"fax_outbound_error_to_email">>, JObj) ->
    kz_json:get_ne_value([<<"notifications">>, <<"fax_to_email">>], JObj, kz_json:new());
maybe_find_deprecated_settings(<<"fax_inbound_error_to_email">>, JObj) ->
    kz_json:get_ne_value([<<"notifications">>, <<"fax_to_email">>], JObj, kz_json:new());
maybe_find_deprecated_settings(_, _) -> kz_json:new().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Try to find the email address of a sub_account_rep for a given
%% account object
%% @end
%%--------------------------------------------------------------------
-spec get_rep_email(kz_json:object()) -> api_binary().
get_rep_email(JObj) ->
    case kapps_config:get_is_true(?NOTIFY_CONFIG_CAT, <<"search_rep_email">>, 'true') of
        'true' -> find_rep_email(JObj);
        'false' -> 'undefined'
    end.

-spec find_rep_email(kz_json:object()) -> api_binary().
find_rep_email(JObj) ->
    AccountId = kz_doc:account_id(JObj),
    Admin =
        case kz_services:is_reseller(AccountId) of
            'true' ->
                lager:debug("finding admins for reseller account ~s", [AccountId]),
                find_admin(AccountId);
            'false' ->
                lager:debug("finding admins for reseller of account ~s", [AccountId]),
                find_admin(kz_services:find_reseller_id(AccountId))
        end,
    kzd_user:email(Admin).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% try to find the first user with admin privileges and an email given
%% a sub account object or sub account db name.
%% @end
%%--------------------------------------------------------------------
-type account_ids() :: ne_binaries().
-spec find_admin(api_binary() | account_ids() | kz_json:object()) -> kz_json:object().
find_admin('undefined') -> kz_json:new();
find_admin([]) -> kz_json:new();
find_admin(Account) when is_binary(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kz_account:fetch(Account) of
        {'error', _} -> find_admin([AccountId]);
        {'ok', JObj} -> find_admin([AccountId | lists:reverse(kz_account:tree(JObj))])
    end;
find_admin([AcctId|Tree]) ->
    AccountDb = kz_util:format_account_id(AcctId, 'encoded'),
    ViewOptions = [{'key', <<"user">>}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', Users} ->
            case [User
                  || User <- Users,
                     kz_json:get_value([<<"doc">>, <<"priv_level">>], User) =:= <<"admin">>,
                     kz_json:get_ne_value([<<"doc">>, <<"email">>], User) =/= 'undefined'
                 ]
            of
                [] -> find_admin(Tree);
                [Admin|_] -> kz_json:get_value(<<"doc">>, Admin)
            end;
        _E ->
            lager:debug("faild to find users in ~s: ~p", [AccountDb, _E]),
            find_admin(Tree)
    end;
find_admin(Account) ->
    find_admin([kz_doc:account_id(Account)
                | lists:reverse(kz_account:tree(Account))
               ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% given a notification event try to open the account definition doc
%% @end
%%--------------------------------------------------------------------
-spec get_account_doc(kz_json:object()) ->
                             {'ok', kz_json:object()} |
                             {'error', _} |
                             'undefined'.
get_account_doc(JObj) ->
    case kz_json:get_first_defined([<<"Account-DB">>
                                   ,<<"Account-ID">>
                                   ], JObj)
    of
        'undefined' -> 'undefined';
        Account -> kz_account:fetch(Account)
    end.

-spec category_to_file(ne_binary()) -> iolist() | 'undefined'.
category_to_file(<<"notify.voicemail_to_email">>) ->
    [code:priv_dir(?APP), "/notify_voicemail_to_email.config"];
category_to_file(<<"notify.voicemail_full">>) ->
    [code:priv_dir(?APP), "/notify_voicemail_full.config"];
category_to_file(<<"notify.fax_to_email">>) ->
    [code:priv_dir(?APP), "/notify_fax_to_email.config"];
category_to_file(<<"notify.fax_inbound_to_email">>) ->
    [code:priv_dir(?APP), "/notify_fax_inbound_to_email.config"];
category_to_file(<<"notify.fax_outbound_to_email">>) ->
    [code:priv_dir(?APP), "/notify_fax_outbound_to_email.config"];
category_to_file(<<"notify.fax_inbound_error_to_email">>) ->
    [code:priv_dir(?APP), "/notify_fax_inbound_error_to_email.config"];
category_to_file(<<"notify.fax_outbound_error_to_email">>) ->
    [code:priv_dir(?APP), "/notify_fax_outbound_error_to_email.config"];
category_to_file(<<"notify.deregister">>) ->
    [code:priv_dir(?APP), "/notify_deregister.config"];
category_to_file(<<"notify.password_recovery">>) ->
    [code:priv_dir(?APP), "/notify_password_recovery.config"];
category_to_file(<<"notify.new_account">>) ->
    [code:priv_dir(?APP), "/notify_new_account.config"];
category_to_file(<<"notify.first_occurrence">>) ->
    [code:priv_dir(?APP), "/notify_first_occurrence.config"];
category_to_file(<<"notify.cnam_request">>) ->
    [code:priv_dir(?APP), "/notify_cnam_request.config"];
category_to_file(<<"notify.port_request">>) ->
    [code:priv_dir(?APP), "/notify_port_request.config"];
category_to_file(<<"notify.port_cancel">>) ->
    [code:priv_dir(?APP), "/notify_port_cancel.config"];
category_to_file(<<"notify.ported">>) ->
    [code:priv_dir(?APP), "/notify_ported.config"];
category_to_file(<<"notify.low_balance">>) ->
    [code:priv_dir(?APP), "/notify_low_balance.config"];
category_to_file(<<"notify.system_alert">>) ->
    [code:priv_dir(?APP), "/notify_system_alert.config"];
category_to_file(<<"notify.transaction">>) ->
    [code:priv_dir(?APP), "/notify_transaction.config"];
category_to_file(<<"notify.topup">>) ->
    [code:priv_dir(?APP), "/notify_topup.config"];
category_to_file(_) ->
    'undefined'.

-spec qr_code_image(api_binary()) -> kz_proplist() | 'undefined'.
qr_code_image('undefined') -> 'undefined';
qr_code_image(Text) ->
    lager:debug("create qr code for ~s", [Text]),
    CHL = kz_util:uri_encode(Text),
    Url = <<"https://chart.googleapis.com/chart?chs=300x300&cht=qr&chl=", CHL/binary, "&choe=UTF-8">>,

    case kz_http:get(kz_term:to_list(Url)) of
        {'ok', 200, _RespHeaders, RespBody} ->
            lager:debug("generated QR code from ~s: ~s", [Url, RespBody]),
            [{<<"image">>, base64:encode(RespBody)}];
        _E ->
            lager:debug("failed to generate QR code: ~p", [_E]),
            'undefined'
    end.

-spec get_charset_params(kz_proplist()) -> {kz_proplist(), binary()}.
get_charset_params(Service) ->
    case props:get_value(<<"template_charset">>, Service) of
        <<>> -> {[], <<>>};
        <<_/binary>> = Charset ->
            {[{<<"content-type-params">>,[{<<"charset">>,Charset}]}]
            ,iolist_to_binary([<<";charset=">>, Charset])
            };
        _ -> {[], <<>>}
    end.
