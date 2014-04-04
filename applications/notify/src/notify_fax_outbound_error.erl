%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with fax attachment to the user.
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_fax_outbound_error).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_fax_outbound_error_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_fax_outbound_error_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_fax_outbound_error_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax_outbound_error_to_email">>).

-spec init() -> 'ok'.
init() ->
    %% ensure the fax template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    true = wapi_notifications:fax_outbound_error_v(JObj),
    _ = whapps_util:put_callid(JObj),

    lager:debug("new outbound fax error left, sending to email if enabled"),

    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),

    Emails = wh_json:get_value([<<"Fax-Notifications">>,<<"email">>,<<"send_to">>], JObj, []),   

    {ok, AcctObj} = couch_mgr:open_cache_doc(AccountDb, AccountId),
    Docs = [JObj, AcctObj],
    Props = create_template_props(JObj, Docs, AcctObj),

    CustomTxtTemplate = wh_json:get_value([<<"notifications">>,
                                           <<"outbound_fax_error_to_email">>,
                                           <<"email_text_template">>], AcctObj),
    CustomHtmlTemplate = wh_json:get_value([<<"notifications">>,
                                            <<"outbound_fax_error_to_email">>,
                                            <<"email_html_template">>], AcctObj),
    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>,
                                               <<"outbound_fax_error_to_email">>,
                                               <<"email_subject_template">>], AcctObj),

    {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),
    {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),
    {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),
    
    try build_and_send_email(TxtBody, HTMLBody, Subject, Emails, props:filter_empty(Props)) of
        _ -> lager:debug("built and sent")
    catch
        C:R ->
            lager:debug("failed: ~s:~p", [C, R]),
            ST = erlang:get_stacktrace(),
            wh_util:log_stacktrace(ST)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object(), wh_json:objects(), wh_json:object()) -> wh_proplist().
create_template_props(Event, Docs, Account) ->
    Now = wh_util:current_tstamp(),

    CIDName = wh_json:get_value(<<"Caller-ID-Name">>, Event),
    CIDNum = wh_json:get_value(<<"Caller-ID-Number">>, Event),
    ToName = wh_json:get_value(<<"Callee-ID-Name">>, Event),
    ToNum = wh_json:get_value(<<"Callee-ID-Number">>, Event),
    ToE164 = wh_json:get_value(<<"To-User">>, Event),
    FromE164 = wh_json:get_value(<<"From-User">>, Event),
    DateCalled = wh_json:get_integer_value(<<"Fax-Timestamp">>, Event, Now),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    Timezone = wh_util:to_list(wh_json:find(<<"timezone">>, Docs, <<"UTC">>)),
    ClockTimezone = whapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    [{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"service">>, notify_util:get_service_props(Event, Account, ?MOD_CONFIG_CAT)}
     ,{<<"fax">>, [{<<"caller_id_number">>, wnm_util:pretty_print(CIDNum)}
                   ,{<<"caller_id_name">>, wnm_util:pretty_print(CIDName)}
                   ,{<<"callee_id_number">>, wnm_util:pretty_print(ToNum)}
                   ,{<<"callee_id_name">>, wnm_util:pretty_print(ToName)}
                   ,{<<"date_called_utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
                   ,{<<"date_called">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
                   ,{<<"from_user">>, wnm_util:pretty_print(FromE164)}
                   ,{<<"from_realm">>, wh_json:get_value(<<"From-Realm">>, Event)}
                   ,{<<"to_user">>, wnm_util:pretty_print(ToE164)}
                   ,{<<"to_realm">>, wh_json:get_value(<<"To-Realm">>, Event)}
                   ,{<<"fax_jobid">>, wh_json:get_value(<<"Fax-JobId">>, Event)}
                   ,{<<"fax_media">>, wh_json:get_value(<<"Fax-Name">>, Event)}
                   ,{<<"call_id">>, wh_json:get_value(<<"Call-ID">>, Event)}
                   | fax_values(wh_json:get_value(<<"Fax-Info">>, Event))
                  ]}
     ,{<<"error">>, [{<<"call_info">>, wh_json:get_value(<<"Fax-Error">>, Event)}
                    ,{<<"fax_info">>, wh_json:get_value([<<"Fax-Info">>,<<"Fax-Result-Text">>], Event)}
                    ]}
     ,{<<"account_db">>, wh_json:get_value(<<"pvt_account_db">>, Account)}
    ].

fax_values(Event) ->
    [{wh_json:normalize_key(K), V}
     || {<<"Fax-", K/binary>>, V} <- wh_json:to_proplist(Event)
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | ne_binaries(), wh_proplist()) -> any().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
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

