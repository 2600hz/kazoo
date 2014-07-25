%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% @end
%%%
%%% @contributors
%%% Karl Anderson <karl@2600hz.org>
%%%
%%%-------------------------------------------------------------------
-module(notify_topup).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_topup_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_topup_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_topup_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".topup">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% initialize the module
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:topup_v(JObj),
    lager:debug("creating topup notice"),
    {'ok', Account} = notify_util:get_account_doc(JObj),
    Props = create_template_props(JObj, Account),
    {'ok', TxtBody} = notify_util:render_template('undefined', ?DEFAULT_TEXT_TMPL, Props),
    {'ok', HTMLBody} = notify_util:render_template('undefined', ?DEFAULT_HTML_TMPL, Props),
    To = whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>, <<"">>),
    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"topup">>, <<"email_subject_template">>], Account),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),
    build_and_send_email(TxtBody, HTMLBody, Subject, To, Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object(), wh_json:object()) -> wh_proplist().
create_template_props(_, AccountJObj) ->
    Amount = wh_json:get_value([<<"topup">>, <<"amount">>], AccountJObj),
    Threshold = wh_json:get_value([<<"topup">>, <<"threshold">>], AccountJObj),
    props:filter_empty([
        {<<"account">>, notify_util:json_to_template_props(AccountJObj)}
        ,{<<"amount">>, pretty_print_dollars(wht_util:units_to_dollars(Amount))}
        ,{<<"threshold">>, pretty_print_dollars(Threshold)}
    ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec pretty_print_dollars(float()) -> ne_binary().
pretty_print_dollars(Amount) ->
    wh_util:to_binary(io_lib:format("$~.2f", [Amount])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | [ne_binary(),...], proplist()) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To)->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To],
    ok;
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Service = props:get_value(<<"service">>, Props),
    From = props:get_value(<<"send_from">>, Service),

    {ContentTypeParams, CharsetString} = notify_util:get_charset_params(Service),
    PlainTransferEncoding = whapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"text_content_transfer_encoding">>),
    HTMLTransferEncoding = whapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"html_content_transfer_encoding">>),

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
