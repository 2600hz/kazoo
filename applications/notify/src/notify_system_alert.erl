%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%%
%%%
%%% @author Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(notify_system_alert).

-export([init/0
        ,handle_req/2
        ]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_system_alert_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_system_alert_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_system_alert_subj_tmpl').

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".system_alert">>).

%%------------------------------------------------------------------------------
%% @doc initialize the module
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%------------------------------------------------------------------------------
%% @doc process the AMQP requests
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:system_alert_v(JObj),

    kz_util:put_callid(JObj),

    lager:debug("creating system alert notice"),

    RespQ = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    notify_util:send_update(RespQ, MsgId, <<"pending">>),

    UseEmail = kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enable_email_alerts">>, 'true'),
    SUBUrl = kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"subscriber_url">>),

    SendResult =
        case kz_json:get_value([<<"Details">>,<<"Format">>], JObj) of
            'undefined' ->
                alert_using_email('true', JObj);
            _Format ->
                EmailResult = alert_using_email(UseEmail, JObj),
                case alert_using_POST(SUBUrl, JObj) of
                    'ok' -> EmailResult;
                    {'error', _}=Error ->
                        [Error] ++ [OK
                                    || OK <- alert_using_email(not UseEmail, JObj),
                                       'ok' =:= OK
                                           orelse (is_list(OK)
                                                   andalso lists:member('ok', OK))
                                   ] ++ [EmailResult]
                end
        end,
    send_update(lists:flatten(SendResult), RespQ, MsgId).

-spec send_update(send_email_return(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_update(Result, RespQ, MsgId) ->
    notify_util:maybe_send_update(Result, RespQ, MsgId).

-spec alert_using_POST(kz_term:ne_binary(), kz_json:object()) -> 'ok' | {'error', any()}.
alert_using_POST(Url, JObj) ->
    Headers = [{"Content-Type", "application/json"}],
    Encoded = kz_json:encode(JObj),

    case kz_http:post(kz_term:to_list(Url), Headers, Encoded) of
        {'ok', _2xx, _ResponseHeaders, _ResponseBody}
          when (_2xx - 200) < 100 -> %% ie: match "2"++_
            lager:debug("JSON data successfully POSTed to '~s'", [Url]);
        _Error ->
            Msg = io_lib:format("failed to POST JSON data to ~p for reason: ~p", [Url,_Error]),
            lager:debug(Msg),
            {'error', kz_term:to_binary(Msg)}
    end.

-spec alert_using_email(boolean(), kz_json:object()) -> send_email_return() | 'disabled'.
alert_using_email('false', _JObj) -> 'disabled';
alert_using_email('true', JObj) ->
    Props = create_template_props(JObj),
    {'ok', TxtBody} = notify_util:render_template('undefined', ?DEFAULT_TEXT_TMPL, Props),
    {'ok', HTMLBody} = notify_util:render_template('undefined', ?DEFAULT_HTML_TMPL, Props),
    Subject = kz_json:get_ne_value(<<"Subject">>, JObj),
    To = kapps_config:get_ne_binary_or_ne_binaries(?MOD_CONFIG_CAT, <<"default_to">>),
    build_and_send_email(TxtBody, HTMLBody, Subject, To, Props).

%%------------------------------------------------------------------------------
%% @doc create the props used by the template render function
%% @end
%%------------------------------------------------------------------------------
-spec create_template_props(kz_json:object()) -> kz_term:proplist().
create_template_props(Event) ->
    [{<<"request">>, notify_util:json_to_template_props(
                       kz_json:delete_keys([<<"Details">>
                                           ,<<"App-Version">>
                                           ,<<"App-Name">>
                                           ,<<"Event-Name">>
                                           ,<<"Event-Category">>
                                           ,<<"Server-ID">>
                                           ,<<"Message">>
                                           ,<<"Subject">>
                                           ]
                                          ,Event
                                          )
                      )
     }
    ,{<<"message">>, kz_json:get_binary_value(<<"Message">>, Event)}
    ,{<<"details">>, notify_util:json_to_template_props(kz_json:get_value(<<"Details">>, Event))}
    ,{<<"service">>, notify_util:get_service_props(kz_json:new(), ?MOD_CONFIG_CAT)}
    ].

%%------------------------------------------------------------------------------
%% @doc process the AMQP requests
%% @end
%%------------------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), kz_term:ne_binary() | kz_term:ne_binaries(), kz_term:proplist()) -> send_email_return().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To)->
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
                ,[]
                ,iolist_to_binary(TxtBody)
                }
               ,{<<"text">>, <<"html">>
                ,props:filter_undefined(
                   [{<<"Content-Type">>, iolist_to_binary([<<"text/html">>, CharsetString])}
                   ,{<<"Content-Transfer-Encoding">>, HTMLTransferEncoding}
                   ])
                ,[]
                ,iolist_to_binary(HTMLBody)
                }
               ]
              }
             ]
            },
    notify_util:send_email(From, To, Email).
