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

-define(DEFAULT_TEXT_TMPL, notify_vm_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_vm_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_vm_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".voicemail_to_email">>).

-spec init/0 :: () -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

-spec handle_req/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _) ->
    true = wapi_notifications:voicemail_v(JObj),
    whapps_util:put_callid(JObj),

    lager:debug("new voicemail left, sending to email if enabled"),

    AcctDB = wh_json:get_value(<<"Account-DB">>, JObj),
    {ok, VMBox} = couch_mgr:open_doc(AcctDB, wh_json:get_value(<<"Voicemail-Box">>, JObj)),
    {ok, UserJObj} = couch_mgr:open_doc(AcctDB, wh_json:get_value(<<"owner_id">>, VMBox)),
    case {wh_json:get_ne_value(<<"email">>, UserJObj), wh_json:is_true(<<"vm_to_email_enabled">>, UserJObj)} of
        {undefined, _} ->
            lager:debug("no email found for user ~s", [wh_json:get_value(<<"username">>, UserJObj)]);
        {_Email, false} ->
            lager:debug("voicemail to email disabled for ~s", [_Email]);
        {Email, true} ->
            lager:debug("VM->Email enabled for user, sending to ~s", [Email]),
            {ok, AcctObj} = couch_mgr:open_doc(AcctDB, wh_util:format_account_id(AcctDB, raw)),
            Docs = [VMBox, UserJObj, AcctObj],

            Props = [{<<"email_address">>, Email}
                     | create_template_props(JObj, Docs, AcctObj)
                    ],

            CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"voicemail_to_email">>, <<"email_text_template">>], AcctObj),
            {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

            CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"voicemail_to_email">>, <<"email_html_template">>], AcctObj),
            {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

            CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"voicemail_to_email">>, <<"email_subject_template">>], AcctObj),
            {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

            build_and_send_email(TxtBody, HTMLBody, Subject, Email
                                 ,props:filter_undefined(Props)
                                ),
            maybe_remove_voicemail(AcctDB, JObj, VMBox)
    end.

maybe_remove_voicemail(AcctDb, NotifyJObj, VMBoxJObj) ->
    BoxDeleteFlag = wh_json:is_true(<<"delete_after_notify">>, VMBoxJObj, false),
    maybe_remove_voicemail(AcctDb, VMBoxJObj
                           ,wh_json:get_value(<<"Voicemail-Name">>, NotifyJObj)
                           ,wh_json:is_true(<<"Delete-After-Notify">>, NotifyJObj, BoxDeleteFlag)
                          ).

maybe_remove_voicemail(_, _, _, false) -> ok;
maybe_remove_voicemail(AcctDb, VMBoxJObj, MediaId, true) -> 
    lager:debug("moving voicemail ~s to deleted folder", [MediaId]),

    Messages = [update_msg_folder(MediaId, Message) || Message <- wh_json:get_value(<<"messages">>, VMBoxJObj, [])],
    case couch_mgr:ensure_saved(AcctDb, wh_json:set_value(<<"messages">>, Messages, VMBoxJObj)) of
        {ok, _} -> lager:debug("voicemail box saved");
        {error, _E} -> lager:debug("failed to save voicemail box: ~p", [_E])
    end,

    {ok, MediaDoc} = couch_mgr:open_doc(AcctDb, MediaId),
    case couch_mgr:ensure_saved(AcctDb, wh_json:set_value(<<"pvt_deleted">>, true, MediaDoc)) of
        {ok, _} -> lager:debug("media doc updated");
        {error, _Err} -> lager:debug("failed to update media doc: ~p", [_Err])
    end.

update_msg_folder(MediaId, Message) ->    
    case wh_json:get_value(<<"media_id">>, Message) =:= MediaId of
        false -> Message;
        true -> wh_json:set_value(<<"folder">>, <<"deleted">>, Message)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props/3 :: (wh_json:json_object(), wh_json:json_objects(), wh_json:json_objects()) -> proplist().
create_template_props(Event, Docs, Account) ->
    CIDName = wh_json:get_value(<<"Caller-ID-Name">>, Event),
    CIDNum = wh_json:get_value(<<"Caller-ID-Number">>, Event),
    ToE164 = wnm_util:to_e164(wh_json:get_value(<<"To-User">>, Event)),
    FromE164 = wnm_util:to_e164(wh_json:get_value(<<"From-User">>, Event)),
    DateCalled = wh_util:to_integer(wh_json:get_value(<<"Voicemail-Timestamp">>, Event)),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    Timezone = wh_util:to_list(wh_json:find(<<"timezone">>, Docs, <<"UTC">>)),
    ClockTimezone = whapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    [{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"service">>, notify_util:get_service_props(Event, Account, ?MOD_CONFIG_CAT)}
     ,{<<"voicemail">>, [{<<"caller_id_number">>, pretty_print_did(CIDNum)}
                         ,{<<"caller_id_name">>, CIDName}
                         ,{<<"date_called_utc">>, localtime:local_to_utc(DateTime, ClockTimezone)}
                         ,{<<"date_called">>, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
                         ,{<<"from_user">>, pretty_print_did(FromE164)}
                         ,{<<"from_realm">>, wh_json:get_value(<<"From-Realm">>, Event)}
                         ,{<<"to_user">>, pretty_print_did(ToE164)}
                         ,{<<"to_realm">>, wh_json:get_value(<<"To-Realm">>, Event)}
                         ,{<<"voicemail_box">>, wh_json:get_value(<<"Voicemail-Box">>, Event)}
                         ,{<<"voicemail_media">>, wh_json:get_value(<<"Voicemail-Name">>, Event)}
                         ,{<<"voicemail_transcription">>, wh_json:get_value([<<"Voicemail-Transcription">>, <<"text">>], Event)}
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
-spec build_and_send_email/5 :: (iolist(), iolist(), iolist(), ne_binary() | [ne_binary(),...], proplist()) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To],
    ok;
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Voicemail = props:get_value(<<"voicemail">>, Props),
    Service = props:get_value(<<"service">>, Props),
    DB = props:get_value(<<"account_db">>, Props),
    DocId = props:get_value(<<"voicemail_media">>, Voicemail),

    From = props:get_value(<<"send_from">>, Service),
    To = props:get_value(<<"email_address">>, Props),

    lager:debug("attempting to attach media ~s in ~s", [DocId, DB]),
    {ok, VMJObj} = couch_mgr:open_doc(DB, DocId),

    [AttachmentId] = wh_json:get_keys(<<"_attachments">>, VMJObj),
    lager:debug("attachment id ~s", [AttachmentId]),
    {ok, AttachmentBin} = couch_mgr:fetch_attachment(DB, DocId, AttachmentId),

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
    notify_util:send_email(From, To, Email),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a friendly file name
%% @end
%%--------------------------------------------------------------------
-spec get_file_name/1 :: (proplist()) -> ne_binary().
get_file_name(Props) ->
    %% CallerID_Date_Time.mp3
    Voicemail = props:get_value(<<"voicemail">>, Props),
    CallerID = case {props:get_value(<<"caller_id_name">>, Voicemail), props:get_value(<<"caller_id_number">>, Voicemail)} of
                   {undefined, undefined} -> <<"Unknown">>;
                   {undefined, Num} -> wh_util:to_binary(Num);
                   {Name, _} -> wh_util:to_binary(Name)
               end,
    LocalDateTime = props:get_value(<<"date_called">>, Voicemail, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", wh_util:pretty_print_datetime(LocalDateTime), ".mp3"]),
    binary:replace(wh_util:to_lower_binary(FName), <<" ">>, <<"_">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a friendly format for DIDs
%% @end
%%--------------------------------------------------------------------
-spec pretty_print_did/1 :: (ne_binary()) -> ne_binary().
pretty_print_did(<<"+1", Area:3/binary, Locale:3/binary, Rest:4/binary>>) ->
    <<"1.", Area/binary, ".", Locale/binary, ".", Rest/binary>>;
pretty_print_did(<<"011", Rest/binary>>) ->
    pretty_print_did(wnm_util:to_e164(Rest));
pretty_print_did(Other) ->
    Other.
