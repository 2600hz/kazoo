%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with port request information to configured email address
%%% @end
%%%
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(notify_port_request).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_port_request_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_port_request_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_port_request_subj_tmpl').

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
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:port_request_v(JObj),
    whapps_util:put_callid(JObj),

    lager:debug("a port change has been requested, sending email notification"),

    {'ok', AccountDoc} = notify_util:get_account_doc(JObj),
    AccountJObj = wh_doc:public_fields(AccountDoc),

    lager:debug("creating port change notice for ~s(~s)", [wh_json:get_value(<<"name">>, AccountJObj)
                                                           ,wh_json:get_value(<<"pvt_account_id">>, AccountDoc)
                                                          ]),

    Props = create_template_props(JObj, AccountJObj),

    CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"port_request">>, <<"email_text_template">>], AccountJObj),
    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),
    lager:debug("txt body: ~s", [TxtBody]),

    CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"port_request">>, <<"email_html_template">>], AccountJObj),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),
    lager:debug("html body: ~s", [HTMLBody]),

    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"port_request">>, <<"email_subject_template">>], AccountJObj),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),
    lager:debug("subject: ~s", [Subject]),

    PortRequestId = wh_json:get_value(<<"Port-Request-ID">>, JObj),
    EmailAttachments =
        case couch_mgr:open_cache_doc(?KZ_PORT_REQUESTS_DB, PortRequestId) of
            {'ok', PortJObj} ->
                Attachments = wh_json:get_value(<<"_attachments">>, PortJObj, wh_json:new()),
                get_attachments(wh_json:to_proplist(Attachments), PortRequestId);
            _ -> []
        end,

    case notify_util:get_rep_email(AccountJObj) of
        'undefined' ->
            SysAdminEmail = whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>, <<>>),
            build_and_send_email(TxtBody, HTMLBody, Subject, SysAdminEmail, Props, EmailAttachments);
        RepEmail ->
            build_and_send_email(TxtBody, HTMLBody, Subject, RepEmail, Props, EmailAttachments)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object(), wh_json:objects()) -> proplist().
create_template_props(NotifyJObj, AccountJObj) ->
    Admin = notify_util:find_admin(wh_json:get_value(<<"Authorized-By">>, NotifyJObj)),

    {'ok', PortDoc} = couch_mgr:open_cache_doc(?KZ_PORT_REQUESTS_DB, wh_json:get_value(<<"Port-Request-ID">>, NotifyJObj)),

    PortData = notify_util:json_to_template_props(wh_doc:public_fields(PortDoc)),
    Numbers = props:get_value(<<"numbers">>, PortData, []),
    Request = props:delete_keys([<<"uploads">>, <<"numbers">>], PortData),

    [{<<"numbers">>, Numbers}
     ,{<<"request">>, Request}
     ,{<<"account">>, notify_util:json_to_template_props(AccountJObj)}
     ,{<<"admin">>, notify_util:json_to_template_props(Admin)}
     ,{<<"service">>, notify_util:get_service_props(AccountJObj, ?MOD_CONFIG_CAT)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | ne_binaries(), wh_proplist(), list()) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props, Attachements) when is_list(To)->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props, Attachements) || T <- To],
    'ok';
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
    lager:debug("sending email from ~s to ~s", [From, To]),
    notify_util:send_email(From, To, Email),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec get_attachments(wh_proplist(), ne_binary()) -> list().
-spec get_attachments(wh_proplist(), ne_binary(), list()) -> list().
get_attachments(Attachments, PortRequestId) ->
    get_attachments(Attachments, PortRequestId, []).
get_attachments([], _PortRequestId, EmailAttachments) -> EmailAttachments;
get_attachments([{AttachmentName, AttachmentJObj}|Attachments], PortRequestId, EmailAttachments) ->
    case couch_mgr:fetch_attachment(?KZ_PORT_REQUESTS_DB, PortRequestId, fix_attachment_name(AttachmentName)) of
        {'ok', AttachmentBin} ->
            [Type, Subtype] =
                binary:split(wh_json:get_ne_value(<<"content_type">>, AttachmentJObj, <<"application/octet-stream">>), <<"/">>),

            Attachment =
                {Type, Subtype
                 ,[{<<"Content-Disposition">>, list_to_binary([<<"attachment; filename=\"">>, AttachmentName, "\""])}
                   ,{<<"Content-Type">>, list_to_binary([Type, "/", Subtype, <<"; name=\"">>, AttachmentName, "\""])}
                   ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
                  ]
                 ,[], AttachmentBin
                },
            lager:debug("adding attachment ~s (~s/~s)", [AttachmentName, Type, Subtype]),
            get_attachments(Attachments, PortRequestId, [Attachment|EmailAttachments]);
        _E ->
            lager:debug("failed to attach ~s: ~p", [AttachmentName, _E]),
            get_attachments(Attachments, PortRequestId, EmailAttachments)
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
