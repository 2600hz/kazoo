%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with fax attachment to the user.
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_fax_inbound_to_email).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_fax_inbound_to_email_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_fax_inbound_to_email_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_fax_inbound_to_email_subj_tmpl').

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax_inbound_to_email">>).

-spec init() -> 'ok'.
init() ->
    %% ensure the fax template can compile, otherwise crash the processes
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

-spec handle_req(kz_json:object(), kz_proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:fax_inbound_v(JObj),

    _ = kz_util:put_callid(JObj),

    lager:debug("new fax left, sending to email if enabled"),

    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    FaxId = kz_json:get_value(<<"Fax-ID">>, JObj),
    lager:debug("account-id: ~s, fax-id: ~s", [AccountId, FaxId]),
    {'ok', FaxDoc} = kazoo_modb:open_doc(AccountId, FaxId),

    Emails = kz_json:get_value([<<"notifications">>,<<"email">>,<<"send_to">>], FaxDoc, []),

    {'ok', AcctObj} = kz_account:fetch(AccountId),
    Docs = [FaxDoc, JObj, AcctObj],
    Props = create_template_props(JObj, Docs, AcctObj),

    CustomTxtTemplate = kz_json:get_value([<<"notifications">>
                                          ,<<"inbound_fax_to_email">>
                                          ,<<"email_text_template">>
                                          ], AcctObj),
    CustomHtmlTemplate = kz_json:get_value([<<"notifications">>
                                           ,<<"inbound_fax_to_email">>
                                           ,<<"email_html_template">>
                                           ], AcctObj),
    CustomSubjectTemplate = kz_json:get_value([<<"notifications">>
                                              ,<<"inbound_fax_to_email">>
                                              ,<<"email_subject_template">>
                                              ], AcctObj),

    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    try build_and_send_email(TxtBody, HTMLBody, Subject, Emails, props:filter_empty(Props)) of
        _ -> lager:debug("built and sent")
    catch
        C:R ->
            lager:debug("failed: ~s:~p", [C, R]),
            ST = erlang:get_stacktrace(),
            kz_util:log_stacktrace(ST)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(kz_json:object(), kz_json:objects(), kz_json:object()) -> kz_proplist().
create_template_props(Event, [FaxDoc | _Others]=_Docs, Account) ->
    Now = kz_util:current_tstamp(),

    CIDName = kz_json:get_value(<<"Caller-ID-Name">>, Event),
    CIDNum = kz_json:get_value(<<"Caller-ID-Number">>, Event),
    ToName = kz_json:get_value(<<"Callee-ID-Name">>, Event),
    ToNum = kz_json:get_value(<<"Callee-ID-Number">>, Event),
    ToE164 = kz_json:get_value(<<"To-User">>, Event),
    FromE164 = kz_json:get_value(<<"From-User">>, Event),
    DateCalled = kz_json:get_integer_value(<<"Fax-Timestamp">>, Event, Now),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),
    Timezone = kz_term:to_list(kz_json:get_value([<<"rx_result">>,<<"timezone">>], FaxDoc, <<"UTC">>)),
    ClockTimezone = kapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),
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
    ,{<<"account_db">>, kz_doc:account_db(FaxDoc)}
    ].

fax_values(Event) ->
    [{kz_json:normalize_key(K), V}
     || {<<"Fax-", K/binary>>, V} <- kz_json:to_proplist(Event)
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | ne_binaries(), kz_proplist()) -> any().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
build_and_send_email(_TxtBody, _HTMLBody, _Subject, 'undefined', _Props) ->
    'ok';
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Service = props:get_value(<<"service">>, Props),
    From = props:get_value(<<"send_from">>, Service),

    {ContentType, AttachmentFileName, AttachmentBin} = notify_fax_util:get_attachment(?MOD_CONFIG_CAT, Props),
    [ContentTypeA,ContentTypeB] = binary:split(ContentType,<<"/">>),

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
             ,{ContentTypeA, ContentTypeB
              ,[{<<"Content-Disposition">>, list_to_binary([<<"attachment; filename=\"">>, AttachmentFileName, "\""])}
               ,{<<"Content-Type">>, list_to_binary([<<ContentType/binary, "; name=\"">>, AttachmentFileName, "\""])}
               ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
               ]
              ,[], AttachmentBin
              }
             ]
            },
    notify_util:send_email(From, To, Email).

