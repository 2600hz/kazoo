%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Notification for 'first' registration and call
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(notify_first_occurrence).

-include("notify.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-export([init/0, send/2
        ,handle_req/2
        ]).

-define(SERVER, ?MODULE).

-define(DEFAULT_TEXT_TMPL, notify_init_occur_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_init_occur_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_init_occur_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".first_occurrence">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?LOG_SYSTEM_ID),
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

-spec handle_req(kz_json:object(), kz_proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:first_occurrence_v(JObj),
    {'ok', Account} = kz_account:fetch(kz_json:get_value(<<"Account-ID">>, JObj)),
    send(kz_json:get_binary_value(<<"Occurrence">>, JObj)
        ,Account
        ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Send an email notifying that a first occurrence event has happened.
%% @end
%%--------------------------------------------------------------------
-spec send(ne_binary(), kz_json:object()) -> any().
send(Occurrence, Account) ->
    lager:debug("creating first occurrence notice"),

    Props = create_template_props(Account, Occurrence),

    CustomTxtTemplate = kz_json:get_value([<<"notifications">>, <<"first_occurrence">>, <<"email_text_template">>], Account),
    {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = kz_json:get_value([<<"notifications">>, <<"first_occurrence">>, <<"email_html_template">>], Account),
    {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = kz_json:get_value([<<"notifications">>, <<"first_occurrence">>, <<"email_subject_template">>], Account),
    {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    To = kz_json:get_value([<<"notifications">>, <<"first_occurrence">>, <<"send_to">>], Account
                          ,kapps_config:get_ne_binary_or_ne_binaries(?MOD_CONFIG_CAT, <<"default_to">>)),
    RepEmail = notify_util:get_rep_email(Account),

    _ = build_and_send_email(TxtBody, HTMLBody, Subject, To, Props),
    build_and_send_email(TxtBody, HTMLBody, Subject, RepEmail, Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(kz_json:object(), ne_binary()) -> kz_proplist().
create_template_props(Account, Occurrence) ->
    Admin = notify_util:find_admin(Account),
    [{<<"event">>, Occurrence}
    ,{<<"account">>, notify_util:json_to_template_props(Account)}
    ,{<<"admin">>, notify_util:json_to_template_props(Admin)}
    ,{<<"service">>, notify_util:get_service_props(kz_json:new(), Account, ?MOD_CONFIG_CAT)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), api_binary() | ne_binaries(), kz_proplist()) -> any().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
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
