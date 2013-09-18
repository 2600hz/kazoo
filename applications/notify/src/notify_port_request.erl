%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%% Karl Anderson <karl@2600hz.org>
%%%
%%% Created : 27 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_port_request).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_port_request_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_port_request_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_port_request_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".port_request">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% initialize the module
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_notifications:port_request_v(JObj),
    whapps_util:put_callid(JObj),

    lager:debug("a port change has been requested, sending email notification"),

    {ok, Account} = notify_util:get_account_doc(JObj),

    lager:debug("creating port change notice"),

    Props = create_template_props(JObj, Account),

    CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"port_request">>, <<"email_text_template">>], Account),
    {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"port_request">>, <<"email_html_template">>], Account),
    {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"port_request">>, <<"email_subject_template">>], Account),
    {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    Number = wnm_util:normalize_number(wh_json:get_value(<<"Number">>, JObj)),
    NumberDb = wnm_util:number_to_db_name(Number),

    Attachments = case couch_mgr:open_doc(NumberDb, Number) of
                      {ok, NumberJObj} ->
                          DocAttach = wh_json:get_value(<<"_attachments">>, NumberJObj, wh_json:new()),
                          get_attachments(wh_json:to_proplist(DocAttach), Number, NumberDb, []);
                      _ -> []
                  end,

    case notify_util:get_rep_email(Account) of
        undefined ->
            SysAdminEmail = whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>, <<"">>),
            build_and_send_email(TxtBody, HTMLBody, Subject, SysAdminEmail, Props, Attachments);
        RepEmail ->
            build_and_send_email(TxtBody, HTMLBody, Subject, RepEmail, Props, Attachments)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object(), wh_json:objects()) -> proplist().
create_template_props(Event, Account) ->
    Admin = notify_util:find_admin(wh_json:get_value(<<"Authorized-By">>, Event)),
    Port = wh_json:get_value(<<"Port">>, Event, wh_json:new()),
    [{<<"request">>, notify_util:json_to_template_props(Event)}
     ,{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"admin">>, notify_util:json_to_template_props(Admin)}
     ,{<<"service">>, notify_util:get_service_props(Account, ?MOD_CONFIG_CAT)}
     ,{<<"send_from">>, get_send_from(Port, Admin)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_send_from(wh_json:object(), wh_json:object()) -> ne_binary().
get_send_from(Port, Admin) ->
    case wh_json:get_ne_value(<<"email">>, Port) of
        'undefined' ->
            DefaultFrom = wh_util:to_binary(node()),
            wh_json:get_ne_value(<<"email">>, Admin, DefaultFrom);
         Email -> Email
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | [ne_binary(),...], proplist(), list()) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props, Attachements) when is_list(To)->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props, Attachements) || T <- To],
    ok;
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props, Attachements) ->
    From = props:get_value(<<"send_from">>, Props),
    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
                 ,[{<<"From">>, From}
                   ,{<<"To">>, To}
                   ,{<<"Subject">>, Subject}
                  ]
             ,[]
             ,[{<<"multipart">>, <<"alternative">>, [], []
                ,[{<<"text">>, <<"plain">>, [{<<"Content-Type">>, <<"text/plain">>}], [], iolist_to_binary(TxtBody)}
                  ,{<<"text">>, <<"html">>, [{<<"Content-Type">>, <<"text/html">>}], [], iolist_to_binary(HTMLBody)}
                 ]
               } | Attachements
              ]
            },
    notify_util:send_email(From, To, Email),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec get_attachments(proplist(), ne_binary(), ne_binary(), list()) -> list().
get_attachments([], _, _, EmailAttachments) ->
    EmailAttachments;
get_attachments([{AttachmentName, AttachmentJObj}|Attachments], Number, Db, EmailAttachments) ->
    case couch_mgr:fetch_attachment(Db, Number, fix_attachment_name(AttachmentName)) of
        {ok, AttachmentBin} ->
            [Type, Subtype] = 
                binary:split(wh_json:get_ne_value(<<"content_type">>, AttachmentJObj, <<"application/octet-stream">>), <<"/">>),
            lager:debug("attempting to attach ~s (~s/~s)", [AttachmentName, Type, Subtype]),
            Attachment = {Type, Subtype
                          ,[{<<"Content-Disposition">>, list_to_binary([<<"attachment; filename=\"">>, AttachmentName, "\""])}
                            ,{<<"Content-Type">>, list_to_binary([Type, "/", Subtype, <<"; name=\"">>, AttachmentName, "\""])}
                            ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
                           ]
                          ,[], AttachmentBin
                         },
            get_attachments(Attachments, Number, Db, [Attachment|EmailAttachments]);
        _E ->
            lager:debug("failed to attach ~s: ~p", [AttachmentName, _E]),
            get_attachments(Attachments, Number, Db, EmailAttachments)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fix_attachment_name(ne_binary() | list()) -> ne_binary().
fix_attachment_name(Name) when is_binary(Name) ->
    fix_attachment_name(wh_util:to_list(Name));
fix_attachment_name(Name) ->
    wh_util:to_binary(http_uri:encode(Name)).
