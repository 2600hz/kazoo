%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti <james@2600hz.org>
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_vm).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_vm_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_vm_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_vm_subj_tmpl').

-define(EMAIL_TXT_TEMPLATE_KEY, [<<"notifications">>, <<"voicemail_to_email">>, <<"email_text_template">>]).
-define(EMAIL_HTML_TEMPLATE_KEY, [<<"notifications">>, <<"voicemail_to_email">>, <<"email_html_template">>]).
-define(EMAIL_SUBJECT_TEMPLATE_KEY, [<<"notifications">>, <<"voicemail_to_email">>, <<"email_subject_template">>]).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".voicemail_to_email">>).

-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_v(JObj),
    _ = whapps_util:put_callid(JObj),

    lager:debug("new voicemail left, sending to email if enabled"),

    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    AcctDB = wh_json:get_value(<<"Account-DB">>, JObj),

    lager:debug("loading vm box ~s", [wh_json:get_value(<<"Voicemail-Box">>, JObj)]),
    {'ok', VMBox} = couch_mgr:open_doc(AcctDB, wh_json:get_value(<<"Voicemail-Box">>, JObj)),

    lager:debug("loading owner ~s", [wh_json:get_value(<<"owner_id">>, VMBox)]),
    {'ok', UserJObj} = couch_mgr:open_doc(AcctDB, wh_json:get_value(<<"owner_id">>, VMBox)),

    case {wh_json:get_ne_value(<<"email">>, UserJObj), wh_json:is_true(<<"vm_to_email_enabled">>, UserJObj)} of
        {'undefined', _} ->
            notify_util:send_update(RespQ, MsgId, <<"failed">>, <<"not configured">>),
            lager:debug("no email found for user ~s", [wh_json:get_value(<<"username">>, UserJObj)]);
        {_Email, 'false'} ->
            notify_util:send_update(RespQ, MsgId, <<"failed">>, <<"not configured">>),
            lager:debug("voicemail to email disabled for ~s", [_Email]);
        {Email, 'true'} ->
            lager:debug("VM->Email enabled for user, sending to ~s", [Email]),
            {'ok', AcctObj} = couch_mgr:open_doc(AcctDB, wh_util:format_account_id(AcctDB, raw)),
            Docs = [VMBox, UserJObj, AcctObj],

            Props = [{<<"email_address">>, Email}
                     | create_template_props(JObj, Docs, AcctObj)
                    ],

            CustomTxtTemplate = wh_json:get_value(?EMAIL_TXT_TEMPLATE_KEY, AcctObj),
            {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

            CustomHtmlTemplate = wh_json:get_value(?EMAIL_HTML_TEMPLATE_KEY, AcctObj),
            {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

            CustomSubjectTemplate = wh_json:get_value(?EMAIL_SUBJECT_TEMPLATE_KEY, AcctObj),
            {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

            build_and_send_email(TxtBody, HTMLBody, Subject, Email
                                 ,props:filter_undefined(Props)
                                 ,{RespQ, MsgId}
                                )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object(), wh_json:objects(), wh_json:object()) -> wh_proplist().
create_template_props(Event, Docs, Account) ->
    CIDName = wh_json:get_value(<<"Caller-ID-Name">>, Event),
    CIDNum = wh_json:get_value(<<"Caller-ID-Number">>, Event),
    ToE164 = wh_json:get_value(<<"To-User">>, Event),
    FromE164 = wh_json:get_value(<<"From-User">>, Event),
    DateCalled = wh_json:get_integer_value(<<"Voicemail-Timestamp">>, Event),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    Timezone = wh_util:to_list(wh_json:find(<<"timezone">>, Docs, <<"UTC">>)),
    ClockTimezone = whapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    [{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"service">>, notify_util:get_service_props(Event, Account, ?MOD_CONFIG_CAT)}
     ,{<<"voicemail">>, [{<<"caller_id_number">>, wnm_util:pretty_print(CIDNum)}
                         ,{<<"caller_id_name">>, wnm_util:pretty_print(CIDName)}  %% sometimes the name is a number...
                         ,{<<"date_called_utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
                         ,{<<"date_called">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
                         ,{<<"from_user">>, wnm_util:pretty_print(FromE164)}
                         ,{<<"from_realm">>, wh_json:get_value(<<"From-Realm">>, Event)}
                         ,{<<"to_user">>, wnm_util:pretty_print(ToE164)}
                         ,{<<"to_realm">>, wh_json:get_value(<<"To-Realm">>, Event)}
                         ,{<<"box">>, wh_json:get_value(<<"Voicemail-Box">>, Event)}
                         ,{<<"media">>, wh_json:get_value(<<"Voicemail-Name">>, Event)}
                         ,{<<"length">>, preaty_print_length(Event)}
                         ,{<<"transcription">>, wh_json:get_value([<<"Voicemail-Transcription">>, <<"text">>], Event)}
                         ,{<<"call_id">>, wh_json:get_value(<<"Call-ID">>, Event)}
                        ]}
     ,{<<"account_db">>, wh_json:get_value(<<"pvt_account_db">>, Account)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | ne_binaries(), wh_proplist(), {api_binary(), ne_binary()}) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props, Resp) when is_list(To) ->
    [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props, Resp) || T <- To];
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props, {RespQ, MsgId}) ->
    Voicemail = props:get_value(<<"voicemail">>, Props),
    Service = props:get_value(<<"service">>, Props),
    DB = props:get_value(<<"account_db">>, Props),
    DocId = props:get_value(<<"media">>, Voicemail),

    From = props:get_value(<<"send_from">>, Service),
    To = props:get_value(<<"email_address">>, Props),

    lager:debug("attempting to attach media ~s in ~s", [DocId, DB]),
    {'ok', VMJObj} = couch_mgr:open_doc(DB, DocId),

    [AttachmentId] = wh_json:get_keys(<<"_attachments">>, VMJObj),
    lager:debug("attachment id ~s", [AttachmentId]),
    {'ok', AttachmentBin} = couch_mgr:fetch_attachment(DB, DocId, AttachmentId),

    AttachmentFileName = get_file_name(Props),
    lager:debug("attachment renamed to ~s", [AttachmentFileName]),

    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
                 ,[{<<"From">>, From}
                   ,{<<"To">>, To}
                   ,{<<"Subject">>, Subject}
                   ,{<<"X-Call-ID">>, props:get_value(<<"call_id">>, Voicemail)}
                  ]
             ,[]
             ,[{<<"multipart">>, <<"alternative">>, [], []
                ,[{<<"text">>, <<"plain">>, [{<<"Content-Type">>, <<"text/plain">>}], [], iolist_to_binary(TxtBody)}
                  ,{<<"text">>, <<"html">>, [{<<"Content-Type">>, <<"text/html">>}], [], iolist_to_binary(HTMLBody)}
                 ]
               }
               ,{<<"audio">>, <<"mpeg">>
                     ,[{<<"Content-Disposition">>, list_to_binary([<<"attachment; filename=\"">>, AttachmentFileName, "\""])}
                       ,{<<"Content-Type">>, list_to_binary([<<"audio/mpeg; name=\"">>, AttachmentFileName, "\""])}
                       ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
                      ]
                 ,[], AttachmentBin
                }
              ]
            },
    case notify_util:send_email(From, To, Email) of
        'ok' -> notify_util:send_update(RespQ, MsgId, <<"completed">>);
        {'error', Reason} -> notify_util:send_update(RespQ, MsgId, <<"failed">>, Reason)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a friendly file name
%% @end
%%--------------------------------------------------------------------
-spec get_file_name(proplist()) -> ne_binary().
get_file_name(Props) ->
    %% CallerID_Date_Time.mp3
    Voicemail = props:get_value(<<"voicemail">>, Props),
    CallerID = case {props:get_value(<<"caller_id_name">>, Voicemail), props:get_value(<<"caller_id_number">>, Voicemail)} of
                   {'undefined', 'undefined'} -> <<"Unknown">>;
                   {'undefined', Num} -> wnm_util:pretty_print(wh_util:to_binary(Num));
                   {Name, _} -> wnm_util:pretty_print(wh_util:to_binary(Name))
               end,
    LocalDateTime = props:get_value(<<"date_called">>, Voicemail, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", wh_util:pretty_print_datetime(LocalDateTime), ".mp3"]),
    binary:replace(wh_util:to_lower_binary(FName), <<" ">>, <<"_">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec preaty_print_length('undefined' | integer() | wh_json:object()) -> ne_binary().
preaty_print_length('undefined') ->
    <<"00:00">>;
preaty_print_length(Milliseconds) when is_integer(Milliseconds) ->
    Seconds = round(Milliseconds / 1000) rem 60,
    Minutes = trunc(Milliseconds / (1000*60)) rem 60,
    wh_util:to_binary(io_lib:format("~2..0w:~2..0w", [Minutes, Seconds]));
preaty_print_length(Event) ->
    preaty_print_length(wh_json:get_integer_value(<<"Voicemail-Length">>, Event)).
