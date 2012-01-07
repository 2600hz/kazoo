%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%% James Aimonetti <james@2600hz.org>
%%% Karl Anderson <karl@2600hz.org>
%%%
%%% Created : 22 Dec 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_vm).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_vm_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_vm_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_vm_subj_tmpl).

-spec init/0 :: () -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, ?DEFAULT_TEXT_TMPL} = erlydtl:compile(whapps_config:get(?MODULE, default_text_template), ?DEFAULT_TEXT_TMPL),
    {ok, ?DEFAULT_HTML_TMPL} = erlydtl:compile(whapps_config:get(?MODULE, default_html_template), ?DEFAULT_HTML_TMPL),
    {ok, ?DEFAULT_SUBJ_TMPL} = erlydtl:compile(whapps_config:get(?MODULE, default_subject_template), ?DEFAULT_SUBJ_TMPL),
    ?LOG_SYS("init done for vm-to-email").

-spec handle_req/2 :: (json_object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_notifications:voicemail_v(JObj),
    whapps_util:put_callid(JObj),

    ?LOG_START("new voicemail left, sending to email if enabled"),

    AcctDB = wh_json:get_value(<<"Account-DB">>, JObj),
    {ok, VMBox} = couch_mgr:open_doc(AcctDB, wh_json:get_value(<<"Voicemail-Box">>, JObj)),
    {ok, UserJObj} = couch_mgr:open_doc(AcctDB, wh_json:get_value(<<"owner_id">>, VMBox)),
    case {wh_json:get_ne_value(<<"email">>, UserJObj), wh_json:is_true(<<"vm_to_email_enabled">>, UserJObj)} of
        {undefined, _} ->
            ?LOG_END("no email found for user ~s", [wh_json:get_value(<<"username">>, UserJObj)]);
        {_Email, false} ->
            ?LOG_END("voicemail to email disabled for ~s", [_Email]);
        {Email, true} ->
            ?LOG("VM->Email enabled for user, sending to ~s", [Email]),
            {ok, AcctObj} = couch_mgr:open_doc(AcctDB, whapps_util:get_db_name(AcctDB, raw)),
            Docs = [VMBox, UserJObj, AcctObj],

            Props = [{email_address, Email}
                     | get_template_props(JObj, Docs)
                    ],

            {ok, TxtBody} = render_template(wh_json:find(<<"vm_to_email_template">>, Docs), ?DEFAULT_TEXT_TMPL, Props),
            {ok, HTMLBody} = render_template(wh_json:find(<<"html_vm_to_email_template">>, Docs), ?DEFAULT_HTML_TMPL, Props),
            {ok, Subject} = render_template(wh_json:find(<<"subject_vm_to_email_template">>, Docs), ?DEFAULT_SUBJ_TMPL, Props),

            send_vm_to_email(TxtBody, HTMLBody, Subject, [ KV || {_, V}=KV <- Props, V =/= undefined ])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec get_template_props/2 :: (json_object(), json_objects()) -> proplist().
get_template_props(Event, Docs) ->
    CIDName = wh_json:get_value(<<"Caller-ID-Name">>, Event),
    CIDNum = wh_json:get_value(<<"Caller-ID-Number">>, Event),
    ToE164 = wh_util:to_e164(wh_json:get_value(<<"To-User">>, Event)),
    FromE164 = wh_util:to_e164(wh_json:get_value(<<"From-User">>, Event)),
    DateCalled = wh_util:to_integer(wh_json:get_value(<<"Voicemail-Timestamp">>, Event)),
    DateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    SupportNumber = wh_json:find([<<"vm_to_email">>, <<"support_number">>], Docs
				 ,whapps_config:get(?MODULE, <<"default_support_number">>, <<"(415) 886 - 7900">>)),    
    SupportEmail = wh_json:find([<<"vm_to_email">>, <<"support_email">>], Docs
				,whapps_config:get(?MODULE, <<"default_support_email">>, <<"support@2600hz.com">>)),
    FromAddress = wh_json:find([<<"vm_to_email">>, <<"from_address">>], Docs
			       ,whapps_config:get(?MODULE, <<"default_from">>, <<"no_reply@2600hz.com">>)),

    Timezone = wh_util:to_list(wh_json:find(<<"timezone">>, Docs, <<"UTC">>)),

    ClockTimezone = whapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),

    [{caller_id_number, pretty_print_did(CIDNum)}
     ,{caller_id_name, CIDName}
     ,{date_called_utc, localtime:local_to_utc(DateTime, ClockTimezone)}
     ,{date_called, localtime:local_to_local(DateTime, ClockTimezone, Timezone)}
     ,{support_number, SupportNumber}
     ,{support_email, SupportEmail}
     ,{from_address, FromAddress}
     ,{from_user, pretty_print_did(FromE164)}
     ,{from_realm, wh_json:get_value(<<"From-Realm">>, Event)}
     ,{to_user, pretty_print_did(ToE164)}
     ,{to_realm, wh_json:get_value(<<"To-Realm">>, Event)}
     ,{account_db, wh_json:get_value(<<"Account-DB">>, Event)}
     ,{voicemail_box, wh_json:get_value(<<"Voicemail-Box">>, Event)}
     ,{voicemail_media, wh_json:get_value(<<"Voicemail-Name">>, Event)}
     ,{call_id, wh_json:get_value(<<"Call-ID">>, Event)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec send_vm_to_email/4 :: (iolist(), iolist(), iolist(), proplist()) -> 'ok'.
send_vm_to_email(TxtBody, HTMLBody, Subject, Props) ->
    DB = props:get_value(account_db, Props),
    DocId = props:get_value(voicemail_media, Props),

    ?LOG("attempting to attach media ~s in ~s", [DocId, DB]),
    {ok, VMJObj} = couch_mgr:open_doc(DB, DocId),

    [AttachmentId] = wh_json:get_keys(<<"_attachments">>, VMJObj),
    ?LOG("attachment id ~s", [AttachmentId]),
    {ok, AttachmentBin} = couch_mgr:fetch_attachment(DB, DocId, AttachmentId),

    AttachmentFileName = get_file_name(Props),
    ?LOG("attachment renamed to ~s", [AttachmentFileName]),

    HostFrom = list_to_binary([<<"no_reply@">>, wh_util:to_binary(net_adm:localhost())]),
    From = props:get_value(from_address, Props, HostFrom),
    To = props:get_value(email_address, Props),

    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
                 ,[
                    {<<"From">>, From},
                    {<<"To">>, To},
                    {<<"Subject">>, Subject}
                  ]
             ,[]
             ,[
               {<<"multipart">>, <<"alternative">>, [], []
                ,[
                  {<<"text">>, <<"plain">>, [{<<"Content-Type">>, <<"text/plain">>}], [], iolist_to_binary(TxtBody)}
                  ,{<<"text">>, <<"html">>, [{<<"Content-Type">>, <<"text/html">>}], [], iolist_to_binary(HTMLBody)}
                 ]
               }
               ,{<<"audio">>, <<"mpeg">>
                     ,[
                       {<<"Content-Disposition">>, list_to_binary([<<"attachment; filename=\"">>, AttachmentFileName, "\""])}
                       ,{<<"Content-Type">>, list_to_binary([<<"audio/mpeg; name=\"">>, AttachmentFileName, "\""])}
                       ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
                      ]
                 ,[], AttachmentBin
                }
              ]
            },

    Encoded = mimemail:encode(Email),
    Relay = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"relay">>, <<"localhost">>)),
    ?LOG("sending email to ~s from ~s via ~s", [To, From, Relay]),

    CallId = props:get_value(call_id, Props),
    gen_smtp_client:send({From, [To], Encoded}, [{relay, Relay}]
                         ,fun(X) -> put(callid, CallId), ?LOG_END("email relay responded: ~p", [X]) end),
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
    CallerID = case {props:get_value(caller_id_name, Props), props:get_value(caller_id_number, Props)} of
                   {undefined, undefined} -> <<"Unknown">>;
                   {undefined, Num} -> wh_util:to_binary(Num);
                   {Name, _} -> wh_util:to_binary(Name)
               end,
    LocalDateTime = props:get_value(date_called, Props, <<"0000-00-00_00-00-00">>),
    list_to_binary([CallerID, "_", wh_util:pretty_print_datetime(LocalDateTime), ".mp3"]).

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
    pretty_print_did(wh_util:to_e164(Rest));
pretty_print_did(Other) ->
    Other.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec render_template/3 :: (ne_binary() | 'undefined', atom(), proplist()) -> {'ok', iolist()} | {'error', term()}.
render_template(undefined, DefaultTemplate, Props) ->
    ?LOG("rendering default ~s template", [DefaultTemplate]),
    DefaultTemplate:render(Props);
render_template(Template, DefaultTemplate, Props) ->
    try                                       
        CustomTemplate = wh_util:to_atom(list_to_binary([props:get_value(account_db, Props), "_"
							,wh_json:to_binary(DefaultTemplate)]), true
					),
        ?LOG("compiling custom ~s template", [DefaultTemplate]),
        {ok, CustomTemplate} = erlydtl:compile(Template, CustomTemplate),
        ?LOG("rendering custom template ~s", [CustomTemplate]),
        CustomTemplate:render(Props)
    catch
        _:_E ->
            ?LOG("error compiling custom ~s template: ~p", [DefaultTemplate, _E]),
            render_template(undefined, DefaultTemplate, Props)
    end.
