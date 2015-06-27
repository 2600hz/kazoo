%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @docg
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_util).

-export([send_email/3
         ,send_update/3, send_update/4
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
         ,post_json/3
        ]).

-include("notify.hrl").
-include_lib("whistle/include/wh_databases.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec send_email(ne_binary(), 'undefined' | binary(), term()) ->
                        'ok' | {'error', _}.
send_email(_, 'undefined', _) -> lager:debug("no email to send to");
send_email(_, <<>>, _) -> lager:debug("empty email to send to");
send_email(From, To, Email) ->
    Encoded = mimemail:encode(Email),
    Relay = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"relay">>, <<"localhost">>)),
    Username = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"username">>, <<"">>)),
    Password = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"password">>, <<"">>)),
    Auth = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"auth">>, <<"never">>)),
    Port = whapps_config:get_integer(<<"smtp_client">>, <<"port">>, 25),

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
                                  put('callid', ReqId),
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
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("notification update (~s) sending to ~s", [Status, RespQ]),
    wapi_notifications:publish_notify_update(RespQ, Prop).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec json_to_template_props(api_object() | wh_json:objects()) -> 'undefined' | wh_proplist().
json_to_template_props('undefined') -> 'undefined';
json_to_template_props(JObj) ->
    normalize_proplist(wh_json:recursive_to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_proplist(wh_proplist()) -> wh_proplist().
normalize_proplist(Props) ->
    [normalize_proplist_element(Elem) || Elem <- Props].

normalize_proplist_element({K, V}) when is_list(V) ->
    {normalize_value(K), normalize_proplist(V)};
normalize_proplist_element({K, V}) when is_binary(V) ->
    {normalize_value(K), mochiweb_html:escape(V)};
normalize_proplist_element({K, V}) ->
    {normalize_value(K), V};
normalize_proplist_element(Else) ->
    Else.

normalize_value(Value) ->
    binary:replace(wh_util:to_lower_binary(Value), <<"-">>, <<"_">>, ['global']).

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
    Template = case whapps_config:get(Category, Key) of
                   'undefined' -> get_default_template(Category, Key);
                   Else -> Else
               end,
    lager:debug("compiling ~s: '~s'", [TemplateModule, Template]),
    {'ok', TemplateModule} = erlydtl:compile_template(Template, TemplateModule, [{'out_dir', 'false'}]).

get_default_template(Category, Key) ->
    File = category_to_file(Category),
    case file:consult(File) of
        {'ok', Props} ->
            case props:get_value(Key, Props) of
                'undefined' -> 'undefined';
                Template ->
                    lager:debug("loading ~s from file ~s", [Key, File]),
                    _ = whapps_config:set(Category, Key, Template),
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
-spec render_template(api_binary(), atom(), wh_proplist()) ->
                                   {'ok', string()} |
                                   {'error', _}.
render_template(Template, DefaultTemplate, Props) ->
    case do_render_template(Template, DefaultTemplate, Props) of
        {'ok', R} -> {'ok', binary_to_list(iolist_to_binary(R))};
        Else -> Else
    end.

-spec do_render_template(api_binary(), atom(), wh_proplist()) ->
                                   {'ok', string()} |
                                   {'error', _}.
do_render_template('undefined', DefaultTemplate, Props) ->
    lager:debug("rendering default ~s template", [DefaultTemplate]),
    DefaultTemplate:render(Props);
do_render_template(Template, DefaultTemplate, Props) ->
    try
        'false' = wh_util:is_empty(Template),
        CustomTemplate = wh_util:to_atom(list_to_binary([couch_mgr:get_uuid(), "_"
                                                        ,wh_util:to_binary(DefaultTemplate)
                                                        ])
                                         ,'true'),
        lager:debug("compiling custom ~s template", [DefaultTemplate]),
        {'ok', CustomTemplate} = erlydtl:compile_template(Template, CustomTemplate, [{'out_dir', 'false'}]),

        lager:debug("rendering custom template ~s", [CustomTemplate]),
        Result = CustomTemplate:render(Props),

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
-spec get_service_props(wh_json:object(), ne_binary()) -> wh_proplist().
-spec get_service_props(wh_json:object(), wh_json:object(), ne_binary()) -> wh_proplist().

get_service_props(Account, ConfigCat) ->
    get_service_props(wh_json:new(), Account, ConfigCat).

get_service_props(Request, Account, ConfigCat) ->
    DefaultUrl = wh_json:get_ne_value(<<"service_url">>, Request
                                      ,whapps_config:get(ConfigCat, <<"default_service_url">>, <<"http://apps.2600hz.com">>)),
    DefaultName = wh_json:get_ne_value(<<"service_name">>, Request
                                       ,whapps_config:get(ConfigCat, <<"default_service_name">>, <<"VOIP Services">>)),
    DefaultProvider = wh_json:get_ne_value(<<"service_provider">>, Request
                                           ,whapps_config:get(ConfigCat, <<"default_service_provider">>, <<"2600hz">>)),
    DefaultNumber = wh_json:get_ne_value(<<"support_number">>, Request
                                           ,whapps_config:get(ConfigCat, <<"default_support_number">>, <<"(415) 886-7900">>)),
    DefaultEmail = wh_json:get_ne_value(<<"support_email">>, Request
                                        ,whapps_config:get(ConfigCat, <<"default_support_email">>, <<"support@2600hz.com">>)),
    UnconfiguredFrom = list_to_binary([<<"no_reply@">>, wh_util:to_binary(net_adm:localhost())]),
    DefaultFrom = wh_json:get_ne_value(<<"send_from">>, Request
                                       ,whapps_config:get(ConfigCat, <<"default_from">>, UnconfiguredFrom)),
    DefaultCharset = wh_json:get_ne_value(<<"template_charset">>, Request
                                       ,whapps_config:get(ConfigCat, <<"default_template_charset">>, <<>>)),
    JObj = find_notification_settings(
             binary:split(ConfigCat, <<".">>)
             ,kz_account:tree(Account)
            ),
    [{<<"url">>, wh_json:get_value(<<"service_url">>, JObj, DefaultUrl)}
     ,{<<"name">>, wh_json:get_value(<<"service_name">>, JObj, DefaultName)}
     ,{<<"provider">>, wh_json:get_value(<<"service_provider">>, JObj, DefaultProvider)}
     ,{<<"support_number">>, wh_json:get_value(<<"support_number">>, JObj, DefaultNumber)}
     ,{<<"support_email">>, wh_json:get_value(<<"support_email">>, JObj, DefaultEmail)}
     ,{<<"send_from">>, wh_json:get_value(<<"send_from">>, JObj, DefaultFrom)}
     ,{<<"template_charset">>, wh_json:get_value(<<"template_charset">>, JObj, DefaultCharset)}
     ,{<<"host">>, wh_util:to_binary(net_adm:localhost())}
    ].

-spec find_notification_settings(ne_binaries() | ne_binary(), ne_binaries()) -> wh_json:object().
find_notification_settings(_, []) ->
    lager:debug("unable to get service props, pvt_tree for the account was empty", []),
    wh_json:new();
find_notification_settings([_, Module], Tree) ->
    case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, lists:last(Tree)) of
        {'error', _} -> wh_json:new();
        {'ok', JObj} ->
            lager:debug("looking for notifications '~s' service info in: ~s"
                        ,[Module, wh_json:get_value(<<"_id">>, JObj)]),
            case wh_json:get_ne_value([<<"notifications">>, Module], JObj) of
                'undefined' -> maybe_find_deprecated_settings(Module, JObj);
                Settings -> Settings
            end
    end;
find_notification_settings(_ConfigCat, _) ->
    lager:debug("unable to get service props, unexpected configuration category: ~p"
                ,[_ConfigCat]),
    wh_json:new().

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Try to find the email address of a sub_account_rep for a given
%% account object
%% @end
%%--------------------------------------------------------------------
-spec get_rep_email(wh_json:object()) -> api_binary().
get_rep_email(JObj) ->
    case whapps_config:get_is_true(?NOTIFY_CONFIG_CAT, <<"search_rep_email">>, 'true') of
        'true' -> find_rep_email(JObj);
        'false' -> 'undefined'
    end.

-spec find_rep_email(wh_json:object()) -> api_binary().
find_rep_email(JObj) ->
    AccountId = wh_doc:account_id(JObj),
    Admin =
        case wh_services:is_reseller(AccountId) of
            'true' ->
                lager:debug("finding admins for reseller account ~s", [AccountId]),
                find_admin(AccountId);
            'false' ->
                lager:debug("finding admins for reseller of account ~s", [AccountId]),
                find_admin(wh_services:find_reseller_id(AccountId))
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
-spec find_admin(api_binary() | account_ids() | wh_json:object()) -> wh_json:object().
find_admin('undefined') -> wh_json:new();
find_admin([]) -> wh_json:new();
find_admin(Account) when is_binary(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case kz_account:fetch(Account) of
        {'error', _} -> find_admin([AccountId]);
        {'ok', JObj} -> find_admin([AccountId | lists:reverse(kz_account:tree(JObj))])
    end;
find_admin([AcctId|Tree]) ->
    AccountDb = wh_util:format_account_id(AcctId, 'encoded'),
    ViewOptions = [{'key', <<"user">>}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', Users} ->
            case [User
                  || User <- Users,
                     wh_json:get_value([<<"doc">>, <<"priv_level">>], User) =:= <<"admin">>,
                     wh_json:get_ne_value([<<"doc">>, <<"email">>], User) =/= 'undefined'
                 ]
            of
                [] -> find_admin(Tree);
                [Admin|_] -> wh_json:get_value(<<"doc">>, Admin)
            end;
        _E ->
            lager:debug("faild to find users in ~s: ~p", [AccountDb, _E]),
            find_admin(Tree)
    end;
find_admin(Account) ->
    find_admin([wh_doc:account_id(Account)
                | lists:reverse(kz_account:tree(Account))
               ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% given a notification event try to open the account definition doc
%% @end
%%--------------------------------------------------------------------
-spec get_account_doc(wh_json:object()) ->
                             {'ok', wh_json:object()} |
                             {'error', _} |
                             'undefined'.
get_account_doc(JObj) ->
    case {wh_json:get_value(<<"Account-DB">>, JObj), wh_json:get_value(<<"Account-ID">>, JObj)} of
        {'undefined', 'undefined'} -> 'undefined';
        {'undefined', Id} ->
            couch_mgr:open_cache_doc(wh_util:format_account_id(Id, 'encoded'), Id);
        {Db, 'undefined'} ->
            couch_mgr:open_cache_doc(Db, wh_util:format_account_id(Db, 'raw'));
        {Db, AccountId} ->
            couch_mgr:open_cache_doc(Db, AccountId)
    end.

-spec category_to_file(ne_binary()) -> iolist() | 'undefined'.
category_to_file(<<"notify.voicemail_to_email">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_voicemail_to_email.config"];
category_to_file(<<"notify.voicemail_full">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_voicemail_full.config"];
category_to_file(<<"notify.fax_to_email">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_fax_to_email.config"];
category_to_file(<<"notify.fax_inbound_to_email">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_fax_inbound_to_email.config"];
category_to_file(<<"notify.fax_outbound_to_email">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_fax_outbound_to_email.config"];
category_to_file(<<"notify.fax_inbound_error_to_email">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_fax_inbound_error_to_email.config"];
category_to_file(<<"notify.fax_outbound_error_to_email">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_fax_outbound_error_to_email.config"];
category_to_file(<<"notify.deregister">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_deregister.config"];
category_to_file(<<"notify.password_recovery">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_password_recovery.config"];
category_to_file(<<"notify.new_account">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_new_account.config"];
category_to_file(<<"notify.first_occurrence">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_first_occurrence.config"];
category_to_file(<<"notify.cnam_request">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_cnam_request.config"];
category_to_file(<<"notify.port_request">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_port_request.config"];
category_to_file(<<"notify.port_cancel">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_port_cancel.config"];
category_to_file(<<"notify.ported">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_ported.config"];
category_to_file(<<"notify.low_balance">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_low_balance.config"];
category_to_file(<<"notify.system_alert">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_system_alert.config"];
category_to_file(<<"notify.transaction">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_transaction.config"];
category_to_file(<<"notify.topup">>) ->
    [code:lib_dir('notify', 'priv'), "/notify_topup.config"];
category_to_file(_) ->
    'undefined'.

-spec qr_code_image(api_binary()) -> wh_proplist() | 'undefined'.
qr_code_image('undefined') -> 'undefined';
qr_code_image(Text) ->
    lager:debug("create qr code for ~s", [Text]),
    CHL = wh_util:uri_encode(Text),
    Url = <<"https://chart.googleapis.com/chart?chs=300x300&cht=qr&chl=", CHL/binary, "&choe=UTF-8">>,

    case ibrowse:send_req(wh_util:to_list(Url)
                          ,[]
                          ,'get'
                          ,[]
                          ,[{'response', 'binary'}]
                         )
    of
        {'ok', "200", _RespHeaders, RespBody} ->
            lager:debug("generated QR code from ~s: ~s", [Url, RespBody]),
            [{<<"image">>, base64:encode(RespBody)}];
        _E ->
            lager:debug("failed to generate QR code: ~p", [_E]),
            'undefined'
    end.

-spec get_charset_params(proplist()) -> {proplist(), binary()}.
get_charset_params(Service) ->
        case props:get_value(<<"template_charset">>, Service) of
            <<>> -> {[], <<>>};
            <<_/binary>> = Charset ->
                {[{<<"content-type-params">>,[{<<"charset">>,Charset}]}]
                 ,iolist_to_binary([<<";charset=">>, Charset])
                };
            _ -> {[], <<>>}
        end.

-spec post_json(ne_binary(), wh_json:object(), fun((wh_json:object()) -> 'ok')) -> 'ok'.
post_json(Url, JObj, OnErrorCallback) ->
    Headers = [{"Content-Type", "application/json"}],
    Encoded = wh_json:encode(JObj),

    case ibrowse:send_req(wh_util:to_list(Url), Headers, 'post', Encoded) of
        {'ok', "2" ++ _, _ResponseHeaders, _ResponseBody} ->
            lager:debug("JSON data successfully POSTed to '~s'", [Url]);
        _Error ->
            lager:debug("failed to POST JSON data to ~p for reason: ~p", [Url,_Error]),
            OnErrorCallback(JObj)
    end.
