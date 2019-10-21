%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Renders a custom account email template, or the system default,
%%% and sends the email with fax attachment to the user.
%%%
%%%
%%% @author James Aimonetti <james@2600hz.org>
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(notify_fax_inbound_error_to_email).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_fax_inbound_error_to_email_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_fax_inbound_error_to_email_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_fax_inbound_error_to_email_subj_tmpl').

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax_inbound_error_to_email">>).

-spec init() -> 'ok'.
init() ->
    %% ensure the fax template can compile, otherwise crash the processes
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:fax_inbound_error_v(JObj),
    _ = kz_log:put_callid(JObj),

    lager:debug("new fax error left, sending to email if enabled"),

    RespQ = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    notify_util:send_update(RespQ, MsgId, <<"pending">>),

    {'ok', Account} = notify_util:get_account_doc(JObj),

    SendResult =
        case is_notice_enabled(Account) of
            'true' -> send(JObj, Account);
            'false' -> lager:debug("fax inbound error notice is disabled")
        end,
    notify_util:maybe_send_update(SendResult, RespQ, MsgId).

-spec send(kz_json:object(), kz_json:object()) -> any().
send(JObj, AcctObj) ->
    Docs = [JObj, AcctObj],
    Props = create_template_props(JObj, Docs, AcctObj),
    CustomTxtTemplate = kz_json:get_value([<<"notifications">>
                                          ,<<"inbound_fax_error_to_email">>
                                          ,<<"email_text_template">>
                                          ], AcctObj),
    CustomHtmlTemplate = kz_json:get_value([<<"notifications">>
                                           ,<<"inbound_fax_error_to_email">>
                                           ,<<"email_html_template">>
                                           ], AcctObj),
    CustomSubjectTemplate = kz_json:get_value([<<"notifications">>
                                              ,<<"inbound_fax_error_to_email">>
                                              ,<<"email_subject_template">>
                                              ], AcctObj),
    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),
    Emails = kz_json:get_value([<<"Fax-Notifications">>
                               ,<<"email">>
                               ,<<"send_to">>
                               ], JObj, []),
    try build_and_send_email(TxtBody, HTMLBody, Subject, Emails, props:filter_empty(Props))
    catch
        ?STACKTRACE(C, R, ST)
        Msg = io_lib:format("failed: ~s:~p", [C, R]),
        lager:debug(Msg),
        kz_log:log_stacktrace(ST),
        {'error', Msg}
        end.

-spec is_notice_enabled(kz_json:object()) -> boolean().
is_notice_enabled(JObj) ->
    case  kz_json:get_value([<<"notifications">>,
                             <<"inbound_fax_error_to_email">>,
                             <<"enabled">>], JObj)
    of
        'undefined' -> is_notice_enabled_default();
        Value -> kz_term:is_true(Value)
    end.

-spec is_notice_enabled_default() -> boolean().
is_notice_enabled_default() ->
    kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"default_enabled">>, 'false').

%%------------------------------------------------------------------------------
%% @doc create the props used by the template render function
%% @end
%%------------------------------------------------------------------------------
-spec create_template_props(kz_json:object(), kz_json:objects(), kz_json:object()) -> kz_term:proplist().
create_template_props(Event, Docs, Account) ->
    Now = kz_time:now_s(),

    CIDName = kz_json:get_value(<<"Caller-ID-Name">>, Event),
    CIDNum = kz_json:get_value(<<"Caller-ID-Number">>, Event),
    ToName = kz_json:get_value(<<"Callee-ID-Name">>, Event),
    ToNum = kz_json:get_value(<<"Callee-ID-Number">>, Event),
    ToE164 = kz_json:get_value(<<"To-User">>, Event),
    FromE164 = kz_json:get_value(<<"From-User">>, Event),
    DateCalled = kz_json:get_integer_value(<<"Fax-Timestamp">>, Event, Now),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    Timezone = kz_term:to_list(kz_json:find(<<"fax_timezone">>, Docs, <<"UTC">>)),
    ClockTimezone = kapps_config:get_string(<<"servers">>, <<"clock_timezone">>, "UTC"),

    [{<<"account">>, notify_util:json_to_template_props(Account)}
    ,{<<"service">>, notify_util:get_service_props(Event, Account, ?MOD_CONFIG_CAT)}
    ,{<<"fax">>, [{<<"caller_id_number">>, knm_util:pretty_print(CIDNum)}
                 ,{<<"caller_id_name">>, knm_util:pretty_print(CIDName)}
                 ,{<<"callee_id_number">>, knm_util:pretty_print(ToNum)}
                 ,{<<"callee_id_name">>, knm_util:pretty_print(ToName)}
                 ,{<<"date_called_utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
                 ,{<<"date_called">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
                 ,{<<"from_user">>, knm_util:pretty_print(FromE164)}
                 ,{<<"from_realm">>, kz_json:get_value(<<"From-Realm">>, Event)}
                 ,{<<"to_user">>, knm_util:pretty_print(ToE164)}
                 ,{<<"to_realm">>, kz_json:get_value(<<"To-Realm">>, Event)}
                 ,{<<"fax_id">>, kz_json:get_value(<<"Fax-ID">>, Event)}
                 ,{<<"fax_media">>, kz_json:get_value(<<"Fax-Name">>, Event)}
                 ,{<<"call_id">>, kz_json:get_value(<<"Call-ID">>, Event)}
                  | fax_values(kz_json:get_value(<<"Fax-Info">>, Event))
                 ]}
    ,{<<"error">>, [{<<"call_info">>, kz_json:get_value(<<"Fax-Error">>, Event)}
                   ,{<<"fax_info">>, kz_json:get_value([<<"Fax-Info">>,<<"Fax-Result-Text">>], Event)}
                   ]}
    ,{<<"account_db">>, kz_doc:account_db(Account)}
    ].

fax_values(Event) ->
    [{kz_json:normalize_key(K), V}
     || {<<"Fax-", K/binary>>, V} <- kz_json:to_proplist(Event)
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

    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
            ,[{<<"From">>, From}
             ,{<<"To">>, To}
             ,{<<"Subject">>, Subject}
             ]
            ,ContentTypeParams
            ,[{<<"multipart">>, <<"alternative">>, [], []
              ,[{<<"text">>, <<"plain">>, [{<<"Content-Type">>, iolist_to_binary([<<"text/plain">>, CharsetString])}], [], iolist_to_binary(TxtBody)}
               ,{<<"text">>, <<"html">>, [{<<"Content-Type">>, iolist_to_binary([<<"text/html">>, CharsetString])}], [], iolist_to_binary(HTMLBody)}
               ]
              }
             ]
            },
    notify_util:send_email(From, To, Email).
