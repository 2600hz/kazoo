%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%%
%%%
%%% @author James Aimonetti <james@2600hz.org>
%%% @author Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(notify_deregister).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_deregister_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_deregister_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_deregister_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".deregister">>).

%%------------------------------------------------------------------------------
%% @doc initialize the module
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%------------------------------------------------------------------------------
%% @doc process the AMQP requests
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(JObj, _Props) ->
    true = kapi_notifications:deregister_v(JObj),
    _ = kz_util:put_callid(JObj),

    lager:debug("endpoint has become unregistered, sending email notification"),

    RespQ = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    notify_util:send_update(RespQ, MsgId, <<"pending">>),

    {ok, Account} = notify_util:get_account_doc(JObj),

    lager:debug("creating deregisted notice"),

    Props = create_template_props(JObj, Account),

    CustomTxtTemplate = kz_json:get_value([<<"notifications">>, <<"deregister">>, <<"email_text_template">>], Account),
    {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = kz_json:get_value([<<"notifications">>, <<"deregister">>, <<"email_html_template">>], Account),
    {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = kz_json:get_value([<<"notifications">>, <<"deregister">>, <<"email_subject_template">>], Account),
    {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    To = kz_json:get_value([<<"notifications">>, <<"deregister">>, <<"send_to">>], Account
                          ,kapps_config:get_ne_binary_or_ne_binaries(?MOD_CONFIG_CAT, <<"default_to">>)),

    notify_util:maybe_send_update(build_and_send_email(TxtBody, HTMLBody, Subject, To, Props)
                                 ,RespQ
                                 ,MsgId
                                 ).

%%------------------------------------------------------------------------------
%% @doc create the props used by the template render function
%% @end
%%------------------------------------------------------------------------------
-spec create_template_props(kz_json:object(), kz_json:object()) -> kz_term:proplist().
create_template_props(Event, Account) ->
    [{<<"last_registration">>, notify_util:json_to_template_props(Event)}
    ,{<<"account">>, notify_util:json_to_template_props(Account)}
    ,{<<"service">>, notify_util:get_service_props(Event, Account, ?MOD_CONFIG_CAT)}
    ].

%%------------------------------------------------------------------------------
%% @doc process the AMQP requests
%% @end
%%------------------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), kz_term:ne_binary() | kz_term:ne_binaries(), kz_term:proplist()) -> send_email_return().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
    [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
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
