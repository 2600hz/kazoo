%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_password_recovery).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_password_recovery_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_password_recovery_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_password_recovery_subj_tmpl').

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".password_recovery">>).

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
-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:password_recovery_v(JObj),
    kz_util:put_callid(JObj),

    lager:debug("request for password reset taken into account, sending email notification"),

    {'ok', Account} = notify_util:get_account_doc(JObj),

    To = kz_json:get_value(<<"Email">>, JObj, kapps_config:get_ne_binary_or_ne_binaries(?MOD_CONFIG_CAT, <<"default_to">>)),

    lager:debug("creating notice for request of password reset"),

    Props = create_template_props(JObj, Account),

    CustomTxtTemplate = kz_json:get_value([<<"notifications">>, <<"password_recovery">>, <<"email_text_template">>], Account),
    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = kz_json:get_value([<<"notifications">>, <<"password_recovery">>, <<"email_html_template">>], Account),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = kz_json:get_value([<<"notifications">>, <<"password_recovery">>, <<"email_subject_template">>], Account),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    build_and_send_email(TxtBody, HTMLBody, Subject, To, Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(kz_json:object(), kz_json:object()) -> kz_proplist().
create_template_props(Event, Account) ->
    User = kz_json:delete_key(<<"Request">>, Event),
    Request = kz_json:get_value(<<"Request">>, Event, kz_json:new()),
    [{<<"user">>, notify_util:json_to_template_props(User)}
    ,{<<"request">>, notify_util:json_to_template_props(Request)}
    ,{<<"account">>, notify_util:json_to_template_props(Account)}
    ,{<<"service">>, notify_util:get_service_props(Request, Account, ?MOD_CONFIG_CAT)}
    ,{<<"link">>, kz_json:get_first_defined([<<"password_reset_link">>, <<"Password-Reset-Link">>], Request)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | ne_binaries(), kz_proplist()) ->
                                  'ok' | {'error', any()}.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To)->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Service = props:get_value(<<"service">>, Props),
    From = props:get_value(<<"send_from">>, Service),

    {ContentTypeParams, CharsetString} = notify_util:get_charset_params(Service),
    PlainTransferEncoding = kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"text_content_transfer_encoding">>, <<"7BIT">>),
    HTMLTransferEncoding = kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"html_content_transfer_encoding">>, <<"7BIT">>),

    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
            ,[{<<"From">>, From}
             ,{<<"To">>, To}
             ,{<<"Subject">>, Subject}
             ]
            ,ContentTypeParams
            ,[{<<"multipart">>, <<"alternative">>, [], []
              ,[{<<"text">>, <<"plain">>
                ,props:filter_undefined(
                   [{<<"Content-Type">>, iolist_to_binary([<<"text/plain">>, CharsetString])}
                   ,{<<"Content-Transfer-Encoding">>, PlainTransferEncoding}
                   ])
                ,[], iolist_to_binary(TxtBody)}
               ,{<<"text">>, <<"html">>
                ,props:filter_undefined(
                   [{<<"Content-Type">>, iolist_to_binary([<<"text/html">>, CharsetString])}
                   ,{<<"Content-Transfer-Encoding">>, HTMLTransferEncoding}
                   ])
                ,[], iolist_to_binary(HTMLBody)}
               ]
              }
             ]
            },
    notify_util:send_email(From, To, Email).
