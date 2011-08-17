%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2011 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(notify_vm).

-export([start_link/0, handle_req/1]).

-include("notify.hrl").

-spec start_link/0 :: () -> ignore.
start_link() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, notify_vm_tmpl} = erlydtl:compile(?DEFAULT_VM_TEMPLATE, notify_vm_tmpl),
    {ok, notify_html_vm_tmpl} = erlydtl:compile(?DEFAULT_HTML_VM_TEMPLATE, notify_html_vm_tmpl),
    ignore.

-spec handle_req/1 :: (JObj) -> no_return() when
      JObj :: json_object().
handle_req(JObj) ->
    true = cf_api:new_voicemail_v(JObj),
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
	    send_vm_to_email(Email, TxtTemplate, HTMLTemplate, JObj)
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec send_vm_to_email/4 :: (To, TxtTmpl, HTMLTmpl, JObj) -> no_return() when
      To :: binary(),
      TxtTmpl :: notify_vm_custom_tmpl | notify_vm_tmpl,
      HTMLTmpl :: notify_html_vm_custom_tmpl | notify_html_vm_tmpl,
      JObj :: json_object().
send_vm_to_email(To, TxtTmpl, HTMLTmpl, JObj) ->
    Subject = <<"New voicemail received">>,
    {ok, TxtBody} = format_plaintext(JObj, TxtTmpl),
    {ok, HTMLBody} = format_html(JObj, HTMLTmpl),

    DB = wh_json:get_value(<<"Account-DB">>, JObj),
    Doc = wh_json:get_value(<<"Voicemail-Box">>, JObj),
    AttachmentId = wh_json:get_value(<<"Voicemail-Name">>, JObj),

    From = <<"no_reply@", (wh_util:to_binary(net_adm:localhost()))/binary>>,

    {ok, AttachmentBin} = couch_mgr:fetch_attachment(DB, Doc, AttachmentId),

    Email = {<<"multipart">>, <<"mixed">> %% Content Type / Sub Type
		 ,[ %% Headers
		    {<<"From">>, From},
		    {<<"To">>, To},
		    {<<"Subject">>, Subject}
		  ]
	     ,[] %% Parameters
	     ,[ %% Body
		{<<"text">>, <<"plain">>, [{<<"Content-Type">>, <<"text/plain">>}], [], iolist_to_binary(TxtBody)} %% Content Type, Subtype, Headers, Parameters, Body
		,{<<"text">>, <<"html">>, [{<<"Content-Type">>, <<"text/html">>}], [], iolist_to_binary(HTMLBody)} %% Content Type, Subtype, Headers, Parameters, Body
		,{<<"audio">>, <<"mpeg">>
		      ,[
			{<<"Content-Disposition">>, list_to_binary([<<"attachment; filename=\"">>, AttachmentId, "\""])}
			,{<<"Content-Type">>, list_to_binary([<<"audio/mpeg; name=\"">>, AttachmentId, "\""])}
		       ]
		  ,[], AttachmentBin
		 }
	      ]
	    },
    Encoded = mimemail:encode(Email),
    gen_smtp_client:send({From, [To], Encoded}, [{relay, "localhost"}]
			 ,fun(X) -> ?LOG("Sending email to ~s resulted in ~p", [To, X]) end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the plain text vm to email component
%% @end
%%--------------------------------------------------------------------
-spec format_plaintext/2 :: (JObj, Tmpl) -> tuple(ok, iolist()) when
      JObj :: json_object(),
      Tmpl :: notify_vm_custom_tmpl | notify_vm_tmpl.
format_plaintext(JObj, Tmpl) ->
    CIDName = wh_json:get_value(<<"Caller-ID-Name">>, JObj),
    CIDNum = wh_json:get_value(<<"Caller-ID-Number">>, JObj),
    ToE164 = wh_util:to_e164(wh_json:get_value(<<"To-User">>, JObj)),
    DateCalled = wh_util:to_integer(wh_json:get_value(<<"Voicemail-Timestamp">>, JObj)),

    Tmpl:render([{caller_id_number, pretty_print_did(CIDNum)}
		 ,{caller_id_name, CIDName}
		 ,{to_user, pretty_print_did(ToE164)}
		 ,{date_called, calendar:gregorian_seconds_to_datetime(DateCalled)}
		 ,{support_number, ?DEFAULT_SUPPORT_NUMBER}
		 ,{support_email, ?DEFAULT_SUPPORT_EMAIL}
		]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the HTML vm to email component
%% @end
%%--------------------------------------------------------------------
-spec format_html/2 :: (JObj, Tmpl) -> tuple(ok, iolist()) when
      JObj :: json_object(),
      Tmpl :: notify_html_vm_custom_tmpl | notify_html_vm_tmpl.
format_html(JObj, Tmpl) ->
    CIDName = wh_json:get_value(<<"Caller-ID-Name">>, JObj),
    CIDNum = wh_json:get_value(<<"Caller-ID-Number">>, JObj),
    ToE164 = wh_util:to_e164(wh_json:get_value(<<"To-User">>, JObj)),
    DateCalled = wh_util:to_integer(wh_json:get_value(<<"Voicemail-Timestamp">>, JObj)),

    Tmpl:render([{caller_id_number, pretty_print_did(CIDNum)}
		 ,{caller_id_name, CIDName}
		 ,{to_user, pretty_print_did(ToE164)}
		 ,{date_called, calendar:gregorian_seconds_to_datetime(DateCalled)}
		 ,{support_number, ?DEFAULT_SUPPORT_NUMBER}
		 ,{support_email, ?DEFAULT_SUPPORT_EMAIL}
		]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a friendly format for DIDs
%% @end
%%--------------------------------------------------------------------
-spec pretty_print_did/1 :: (DID) -> binary() when
      DID :: binary().
pretty_print_did(<<"+1", Area:3/binary, Locale:3/binary, Rest:4/binary>>) ->
    <<"1.", Area/binary, ".", Locale/binary, ".", Rest/binary>>;
pretty_print_did(<<"011", Rest/binary>>) ->
    pretty_print_did(wh_util:to_e164(Rest));
pretty_print_did(Other) ->
    Other.

get_txt_tmpl(AcctObj) ->
    case wh_json:get_value(<<"vm_to_email_template">>, AcctObj) of
	undefined -> notify_vm_tmpl;
	Tmpl ->
	    try
		{ok, notify_vm_custom_tmpl} = erlydtl:compile(Tmpl, notify_vm_custom_tmpl),
		?LOG("Compiled custom template"),
		notify_vm_custom_tmpl
	    catch
		_:E ->
		    ?LOG("Error compiling txt template ~p", [E]),
		    notify_vm_tmpl
	    end
    end.

get_html_tmpl(AcctObj) ->
    case wh_json:get_value(<<"html_vm_to_email_template">>, AcctObj) of
	undefined -> notify_html_vm_tmpl;
	Tmpl ->
	    try
		{ok, notify_html_vm_custom_tmpl} = erlydtl:compile(Tmpl, notify_html_vm_custom_tmpl),
		?LOG("Compiled custom template"),
		notify_html_vm_custom_tmpl
	    catch
		_:E ->
		    ?LOG("Error compiling html template ~p", [E]),
		    notify_html_vm_tmpl
	    end
    end.
