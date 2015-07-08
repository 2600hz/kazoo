%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600hz INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%% Peter Defebvre
%%%
%%%-------------------------------------------------------------------
-module(notify_voicemail_full).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_voicemail_full_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_voicemail_full_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_voicemail_full_subj_tmpl').

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".voicemail_full">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% initialize the module
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_full_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("voicemail full notice, sending to email if enabled"),
    {'ok', Account} = notify_util:get_account_doc(JObj),
    case is_notice_enabled(Account) of
        'true' -> send(JObj, Account);
        'false' -> 'ok'
    end.

-spec send(wh_json:object(), wh_json:object()) -> 'ok'.
send(JObj, Account) ->
    lager:debug("a vm_full notice has been received, sending email notification"),

    Props = create_template_props(JObj),

    CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"vm_full">>, <<"email_text_template">>], Account),
    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"vm_full">>, <<"email_html_template">>], Account),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"vm_full">>, <<"email_subject_template">>], Account),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    AccountDb = wh_json:get_value(<<"Account-DB">>, JObj),

    VMBoxId = wh_json:get_value(<<"Voicemail-Box">>, JObj),
    lager:debug("loading vm box ~s", [VMBoxId]),
    {'ok', VMBox} = couch_mgr:open_cache_doc(AccountDb, VMBoxId),

    {'ok', UserJObj} = get_owner(AccountDb, VMBox),

    BoxEmails = kzd_voicemail_box:notification_emails(VMBox),
    Emails = maybe_add_user_email(BoxEmails
                                  ,get_user_email(UserJObj, Account)
                                 ),

    build_and_send_email(TxtBody, HTMLBody, Subject, Emails, Props).

-spec get_user_email(wh_json:object(), wh_json:object()) -> api_binary().
get_user_email(UserJObj, Account) ->
    case wh_json:get_first_defined([<<"email">>, <<"username">>], UserJObj) of
        'undefined' -> get_rep_email(Account);
        Email -> Email
    end.

-spec get_rep_email(wh_json:object()) -> api_binary().
get_rep_email(Account) ->
    case notify_util:get_rep_email(Account) of
        'undefined' -> get_sys_admin_email();
        RepEmail -> RepEmail
    end.

-spec get_sys_admin_email() -> api_binary().
get_sys_admin_email() ->
    whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>).

-spec get_owner(ne_binary(), kzd_voicemail_box:doc(), api_binary()) ->
                       {'ok', kzd_user:doc()}.
get_owner(AccountDb, VMBox) ->
    get_owner(AccountDb, VMBox, kzd_voicemail_box:owner_id(VMBox)).
get_owner(_AccountDb, _VMBox, 'undefined') ->
    lager:debug("no owner of voicemail box ~s, using empty owner", [wh_doc:id(_VMBox)]),
    {'ok', wh_json:new()};
get_owner(AccountDb, _VMBox, OwnerId) ->
    lager:debug("attempting to load owner: ~s", [OwnerId]),
    {'ok', _} = couch_mgr:open_cache_doc(AccountDb, OwnerId).

-spec maybe_add_user_email(ne_binaries(), api_binary()) -> ne_binaries().
maybe_add_user_email(BoxEmails, 'undefined') -> BoxEmails;
maybe_add_user_email(BoxEmails, UserEmail) -> [UserEmail | BoxEmails].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object()) -> wh_proplist().
create_template_props(JObj) ->
    AccountDb = wh_json:get_value(<<"Account-DB">>, JObj),
    {'ok', AccountJObj} = kz_account:fetch(AccountDb),

    [{<<"service">>, notify_util:get_service_props(JObj, AccountJObj, ?MOD_CONFIG_CAT)}
     ,{<<"voicemail">>, [{<<"name">>, get_vm_name(JObj)}
                         ,{<<"box">>, wh_json:get_value(<<"Voicemail-Box">>, JObj)}
                         ,{<<"number">>, wh_json:get_value(<<"Voicemail-Number">>, JObj)}
                         ,{<<"max_message_count">>, wh_json:get_value(<<"Max-Message-Count">>, JObj)}
                         ,{<<"message_count">>, wh_json:get_value(<<"Message-Count">>, JObj)}
                        ]}
     ,{<<"custom">>, notify_util:json_to_template_props(JObj)}
    ].

-spec get_vm_name(wh_json:object()) -> binary().
get_vm_name(JObj) ->
    case get_vm_doc(JObj) of
        'error' -> <<>>;
        VMDoc -> wh_json:get_binary_value(<<"name">>, VMDoc)
    end.

-spec get_vm_doc(wh_json:object()) -> wh_json:object() | 'error'.
get_vm_doc(JObj) ->
    VMId = wh_json:get_value(<<"Voicemail-Box">>, JObj),
    AccoundDB = wh_json:get_value(<<"Account-DB">>, JObj),
    case couch_mgr:open_cache_doc(AccoundDB, VMId) of
        {'ok', VMDoc} -> VMDoc;
        {'error', _E} ->
            lager:info("could not open doc ~p in ~p", [VMId, AccoundDB]),
            'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | ne_binaries(), wh_proplist()) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Service = props:get_value(<<"service">>, Props),

    From = props:get_value(<<"send_from">>, Service),

    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
                 ,[{<<"From">>, From}
                   ,{<<"To">>, To}
                   ,{<<"Subject">>, Subject}
                  ]
             ,[]
             ,[{<<"multipart">>, <<"alternative">>, [], []
                ,[{<<"text">>, <<"plain">>, [{<<"Content-Type">>, <<"text/plain">>}], [], iolist_to_binary(TxtBody)}
                  ,{<<"text">>, <<"html">>, [{<<"Content-Type">>, <<"text/html">>}], [], iolist_to_binary(HTMLBody)}
                 ]
               }
              ]
            },
    notify_util:send_email(From, To, Email).

-spec is_notice_enabled(wh_json:object()) -> boolean().
is_notice_enabled(JObj) ->
    case  wh_json:get_value([<<"notifications">>
                             ,<<"voicemail_full">>
                             ,<<"enabled">>
                            ], JObj)
    of
        'undefined' -> is_notice_enabled_default();
        Value -> wh_util:is_true(Value)
    end.

-spec is_notice_enabled_default() -> boolean().
is_notice_enabled_default() ->
    whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"default_enabled">>, 'false').
