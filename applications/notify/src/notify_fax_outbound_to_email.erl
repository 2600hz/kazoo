%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc Renders a custom account email template, or the system default,
%%% and sends the email with fax attachment to the user.
%%%
%%%
%%% @author James Aimonetti <james@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(notify_fax_outbound_to_email).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_fax_outbound_to_email_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_fax_outbound_to_email_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_fax_outbound_to_email_subj_tmpl').

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax_outbound_to_email">>).

-spec init() -> 'ok'.
init() ->
    %% ensure the fax template can compile, otherwise crash the processes
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(JObj, _Props) ->
    true = kapi_notifications:fax_outbound_v(JObj),
    _ = kz_util:put_callid(JObj),

    lager:debug("new outbound fax left, sending to email if enabled"),

    RespQ = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    notify_util:send_update(RespQ, MsgId, <<"pending">>),

    AccountDb = kapi_notifications:account_db(JObj, 'true'),
    JobId = kz_json:get_value(<<"Fax-JobId">>, JObj),
    lager:debug("account-db: ~s, fax-id: ~s", [AccountDb, JobId]),

    SendResult =
        case kz_datamgr:open_cache_doc(AccountDb, JobId) of
            {'ok', FaxDoc} ->
                process_req(FaxDoc, JObj, _Props);
            {'error', Err} ->
                Msg = io_lib:format("could not load fax document: ~p", [Err]),
                lager:error(Msg),
                {'error', Msg}
        end,
    notify_util:maybe_send_update(SendResult, RespQ, MsgId).

-spec process_req(kzd_fax:doc(), kz_json:object(), kz_term:proplist()) -> send_email_return().
process_req(FaxDoc, JObj, _Props) ->
    Emails = kz_json:get_value([<<"notifications">>,<<"email">>,<<"send_to">>], FaxDoc, []),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),

    {'ok', AcctObj} = kzd_accounts:fetch(AccountId),
    Docs = [FaxDoc, JObj, AcctObj],
    Props = create_template_props(JObj, Docs, AcctObj),

    CustomTxtTemplate = kz_json:get_value([<<"notifications">>,
                                           <<"outbound_fax_to_email">>,
                                           <<"email_text_template">>], AcctObj),
    CustomHtmlTemplate = kz_json:get_value([<<"notifications">>,
                                            <<"outbound_fax_to_email">>,
                                            <<"email_html_template">>], AcctObj),
    CustomSubjectTemplate = kz_json:get_value([<<"notifications">>,
                                               <<"outbound_fax_to_email">>,
                                               <<"email_subject_template">>], AcctObj),

    AccountDb = kapi_notifications:account_db(JObj, 'true'),

    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    notify_util:send_update(kz_api:server_id(JObj), kz_api:msg_id(JObj), <<"pending">>),
    try build_and_send_email(TxtBody, HTMLBody, Subject, Emails, props:filter_empty(Props), AccountDb)
    catch
        C:R ->
            Msg = io_lib:format("failed: ~s:~p", [C, R]),
            lager:debug(Msg),
            ST = erlang:get_stacktrace(),
            kz_util:log_stacktrace(ST),
            {'error', Msg}
    end.

%%------------------------------------------------------------------------------
%% @doc create the props used by the template render function
%% @end
%%------------------------------------------------------------------------------
-spec create_template_props(kz_json:object(), kz_json:objects(), kz_json:object()) -> kz_term:proplist().
create_template_props(Event, [FaxDoc | _Others]=_Docs, Account) ->
    Now = kz_time:now_s(),

    CIDName = kz_json:get_value(<<"Caller-ID-Name">>, Event),
    CIDNum = kz_json:get_value(<<"Caller-ID-Number">>, Event),
    ToName = kz_json:get_value(<<"Callee-ID-Name">>, Event),
    ToNum = kz_json:get_value(<<"Callee-ID-Number">>, Event),
    ToE164 = kz_json:get_value(<<"To-User">>, Event),
    FromE164 = kz_json:get_value(<<"From-User">>, Event),
    DateCalled = kz_json:get_integer_value(<<"Fax-Timestamp">>, Event, Now),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),
    Timezone = kz_term:to_list(kz_json:get_value([<<"tx_result">>,<<"timezone">>], FaxDoc, <<"UTC">>)),
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
                 ,{<<"fax_jobid">>, kz_json:get_value(<<"Fax-JobId">>, Event)}
                 ,{<<"fax_media">>, kz_json:get_value(<<"Fax-Name">>, Event)}
                 ,{<<"call_id">>, kz_json:get_value(<<"Call-ID">>, Event)}
                  | fax_values(kz_json:get_value(<<"Fax-Info">>, Event))
                 ]}
    ,{<<"account_db">>, kapi_notifications:account_db(Event, 'true')}
    ].

fax_values(Event) ->
    [{kz_json:normalize_key(K), V}
     || {<<"Fax-", K/binary>>, V} <- kz_json:to_proplist(Event)
    ].

%%------------------------------------------------------------------------------
%% @doc process the AMQP requests
%% @end
%%------------------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), kz_term:ne_binary() | kz_term:ne_binaries(), kz_term:proplist(), kz_term:ne_binary()) -> send_email_return().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props, AccountDb) when is_list(To) ->
    [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props, AccountDb) || T <- To];
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props, AccountDb) ->
    Service = props:get_value(<<"service">>, Props),
    From = props:get_value(<<"send_from">>, Service),

    {ContentType, AttachmentFileName, AttachmentBin} = notify_fax_util:get_attachment(AccountDb, ?MOD_CONFIG_CAT, Props),
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
