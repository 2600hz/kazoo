%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_vm).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-type txt_template_module() :: notify_vm_text_custom_tmpl | notify_vm_text_tmpl.
-type html_template_module() :: notify_vm_html_custom_tmpl | notify_vm_html_tmpl.
-type subj_template_module() :: notify_vm_subj_custom_tmpl | notify_vm_subj_tmpl.

-spec init/0 :: () -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, notify_vm_text_tmpl} = erlydtl:compile(whapps_config:get(?MODULE, default_text_template), notify_vm_text_tmpl),
    {ok, notify_vm_html_tmpl} = erlydtl:compile(whapps_config:get(?MODULE, default_html_template), notify_vm_html_tmpl),
    {ok, notify_vm_subj_tmpl} = erlydtl:compile(whapps_config:get(?MODULE, default_subject_template), notify_vm_subj_tmpl),
    ?LOG_SYS("init done for vm-to-email").

-spec handle_req/2 :: (json_object(), proplist()) -> no_return().
handle_req(JObj, _Props) ->
    true = wapi_notification:voicemail_v(JObj),
    whapps_util:put_callid(JObj),

    AcctDB = wh_json:get_value(<<"Account-DB">>, JObj),
    {ok, VMBox} = couch_mgr:open_doc(AcctDB, wh_json:get_value(<<"Voicemail-Box">>, JObj)),
    {ok, UserJObj} = couch_mgr:open_doc(AcctDB, wh_json:get_value(<<"owner_id">>, VMBox)),
    case {wh_json:get_value(<<"email">>, UserJObj), wh_json:is_true(<<"vm_to_email_enabled">>, UserJObj)} of
	{undefined, _} ->
	    ?LOG_END("no email found for user ~s", [wh_json:get_value(<<"username">>, UserJObj)]);
	{_Email, false} ->
	    ?LOG_END("voicemail to email disabled for ~s", [_Email]);
	{Email, true} ->
	    {ok, AcctObj} = couch_mgr:open_doc(AcctDB, whapps_util:get_db_name(AcctDB, raw)),
	    TxtTemplate = get_txt_tmpl(AcctObj),
	    HTMLTemplate = get_html_tmpl(AcctObj),
	    SubjTemplate = get_subject_tmpl(AcctObj),
	    send_vm_to_email(Email, TxtTemplate, HTMLTemplate, SubjTemplate, wh_json:set_value(<<"timezone">>, wh_json:get_value(<<"timezone">>, UserJObj), JObj))
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec send_vm_to_email/5 :: (ne_binary(), txt_template_module(), html_template_module(), subj_template_module(), json_object()) -> no_return().
send_vm_to_email(To, TxtTmpl, HTMLTmpl, SubjTmpl, JObj) ->
    TemplateProps = get_template_props(JObj),
    {ok, TxtBody} = TxtTmpl:render(TemplateProps),
    {ok, HTMLBody} = HTMLTmpl:render(TemplateProps),
    {ok, Subject} = SubjTmpl:render(TemplateProps),

    DB = wh_json:get_value(<<"Account-DB">>, JObj),
    DocId = wh_json:get_value(<<"Voicemail-Name">>, JObj),

    HostFrom = list_to_binary([<<"no_reply@">>, wh_util:to_binary(net_adm:localhost())]),
    From = whapps_config:get_binary(?MODULE, <<"default_from">>, HostFrom),

    ?LOG_SYS("Opening ~s in ~s", [DocId, DB]),
    {ok, VMJObj} = couch_mgr:open_doc(DB, DocId),

    [AttachmentId] = wh_json:get_keys(<<"_attachments">>, VMJObj),
    ?LOG_SYS("Attachment doc: ~s", [AttachmentId]),
    {ok, AttachmentBin} = couch_mgr:fetch_attachment(DB, DocId, AttachmentId),

    AttachmentFileName = get_file_name(TemplateProps),

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
    ?LOG_SYS("Sending email to ~s", [To]),
    Relay = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"relay">>, <<"localhost">>)),
    gen_smtp_client:send({From, [To], Encoded}, [{relay, Relay}]
			 ,fun(X) -> ?LOG("Sending email to ~s resulted in ~p", [To, X]) end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec get_template_props/1 :: (json_object()) -> proplist().
get_template_props(JObj) ->
    CIDName = wh_json:get_value(<<"Caller-ID-Name">>, JObj),
    CIDNum = wh_json:get_value(<<"Caller-ID-Number">>, JObj),
    ToE164 = wh_util:to_e164(wh_json:get_value(<<"To-User">>, JObj)),
    DateCalled = wh_util:to_integer(wh_json:get_value(<<"Voicemail-Timestamp">>, JObj)),
    UTCDateTime = calendar:gregorian_seconds_to_datetime(DateCalled),

    [{caller_id_number, pretty_print_did(CIDNum)}
     ,{caller_id_name, CIDName}
     ,{to_user, pretty_print_did(ToE164)}
     ,{date_called, UTCDateTime}
     ,{date_called_local, localtime:utc_to_localtime(UTCDateTime, wh_json:get_value(<<"timezone">>, JObj, <<"UTC">>))}
     ,{support_number, whapps_config:get(?MODULE, <<"default_support_number">>, <<"(415) 886 - 7900">>)}
     ,{support_email, whapps_config:get(?MODULE, <<"default_support_email">>, <<"support@2600hz.com">>)}
    ].

-spec get_file_name/1 :: (proplist()) -> ne_binary().
get_file_name(Props) ->
    %% CallerID_Date_Time.mp3
    CallerID = case {props:get_value(caller_id_name, Props), props:get_value(caller_id_number, Props)} of
		   {undefined, undefined} -> <<"Unknown">>;
		   {undefined, Num} -> wh_util:to_binary(Num);
		   {Name, _} -> wh_util:to_binary(Name)
	       end,
    ?LOG("CallerID for filename: ~s", [CallerID]),

    LocalDateTime = props:get_value(date_called_local, Props),
    ?LOG("UTC: ~p Local: ~p", [props:get_value(date_called, Props), LocalDateTime]),

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

get_txt_tmpl(AcctObj) ->
    case wh_json:get_value(<<"vm_to_email_template">>, AcctObj) of
	undefined -> notify_vm_text_tmpl;
	Tmpl ->
	    try
		{ok, notify_vm_text_custom_tmpl} = erlydtl:compile(Tmpl, notify_vm_text_custom_tmpl),
		?LOG("compiled custom text template"),
		notify_vm_text_custom_tmpl
	    catch
		_:E ->
		    ?LOG("error compiling text template ~p", [E]),
		    notify_vm_text_tmpl
	    end
    end.

get_html_tmpl(AcctObj) ->
    case wh_json:get_value(<<"html_vm_to_email_template">>, AcctObj) of
	undefined -> notify_vm_html_tmpl;
	Tmpl ->
	    try
		{ok, notify_vm_html_custom_tmpl} = erlydtl:compile(Tmpl, notify_vm_html_custom_tmpl),
		?LOG("compiled custom html template"),
		notify_vm_html_custom_tmpl
	    catch
		_:E ->
		    ?LOG("error compiling html template ~p", [E]),
		    notify_vm_html_tmpl
	    end
    end.

get_subject_tmpl(AcctObj) ->
    case wh_json:get_value(<<"subject_vm_to_email_template">>, AcctObj) of
	undefined -> notify_vm_subj_tmpl;
	Tmpl ->
	    try
		{ok, notify_vm_subj_custom_tmpl} = erlydtl:compile(Tmpl, notify_vm_subj_custom_tmpl),
		?LOG("compiled custom subject template"),
		notify_vm_subj_custom_tmpl
	    catch
		_:E ->
		    ?LOG("error compiling subject template ~p", [E]),
		    notify_vm_subj_tmpl
	    end
    end.
