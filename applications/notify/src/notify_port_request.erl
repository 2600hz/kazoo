%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
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
-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:port_request_v(JObj),
    kz_util:put_callid(JObj),

    lager:debug("a port change has been requested, sending email notification"),

    {'ok', AccountDoc} = notify_util:get_account_doc(JObj),
    AccountJObj = kz_doc:public_fields(AccountDoc),

    lager:debug("creating port change notice for ~s(~s)", [kz_account:name(AccountJObj)
                                                          ,kz_doc:account_id(AccountDoc)
                                                          ]),

    Version = kz_json:get_value(<<"Version">>, JObj),
    Props = create_template_props(Version, JObj, AccountJObj),

    CustomTxtTemplate = kz_json:get_value([<<"notifications">>, <<"port_request">>, <<"email_text_template">>], AccountJObj),
    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),
    lager:debug("txt body: ~s", [TxtBody]),

    CustomHtmlTemplate = kz_json:get_value([<<"notifications">>, <<"port_request">>, <<"email_html_template">>], AccountJObj),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),
    lager:debug("html body: ~s", [HTMLBody]),

    CustomSubjectTemplate = kz_json:get_value([<<"notifications">>, <<"port_request">>, <<"email_subject_template">>], AccountJObj),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),
    lager:debug("subject: ~s", [Subject]),

    EmailAttachments = get_attachments(JObj),

    case notify_util:get_rep_email(AccountDoc) of
        'undefined' ->
            SysAdminEmail = kapps_config:get_ne_binary_or_ne_binaries(?MOD_CONFIG_CAT, <<"default_to">>),
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
-spec create_template_props(api_binary(), kz_json:object(), kz_json:object()) -> kz_proplist().
create_template_props(<<"v2">>, NotifyJObj, AccountJObj) ->
    Admin = notify_util:find_admin(kz_json:get_value(<<"Authorized-By">>, NotifyJObj)),

    PortDoc = find_port_info(NotifyJObj),
    PortData = notify_util:json_to_template_props(kz_doc:public_fields(PortDoc)),
    [Number|_]=Numbers = find_numbers(PortData, NotifyJObj),

    NumberString = kz_binary:join(Numbers, <<" ">>),

    Request = [{<<"port">>
               ,[{<<"service_provider">>, kz_json:get_value(<<"carrier">>, PortDoc)}
                ,{<<"billing_name">>, kz_json:get_value([<<"bill">>, <<"name">>], PortDoc)}
                ,{<<"billing_account_id">>, kz_doc:account_id(PortDoc)}
                ,{<<"billing_street_address">>, kz_json:get_value([<<"bill">>, <<"address">>], PortDoc)}
                ,{<<"billing_locality">>, kz_json:get_value([<<"bill">>, <<"locality">>], PortDoc)}
                ,{<<"billing_postal_code">>, kz_json:get_value([<<"bill">>, <<"postal_code">>], PortDoc)}
                ,{<<"billing_telephone_number">>, kz_json:get_value([<<"bill">>, <<"phone_number">>], PortDoc)}
                ,{<<"requested_port_date">>, kz_json:get_value(<<"transfer_date">>, PortDoc)}
                ,{<<"customer_contact">>, kz_json:get_value([<<"notifications">>, <<"email">>, <<"send_to">>], PortDoc)}
                ]
               }
              ,{<<"number">>, NumberString}
              ],

    [{<<"numbers">>, Numbers}
    ,{<<"number">>, Number}
    ,{<<"request">>, Request}
    ,{<<"account">>, notify_util:json_to_template_props(AccountJObj)}
    ,{<<"admin">>, notify_util:json_to_template_props(Admin)}
    ,{<<"service">>, notify_util:get_service_props(AccountJObj, ?MOD_CONFIG_CAT)}
    ,{<<"send_from">>, get_send_from(PortDoc, Admin)}
    ];
create_template_props(_, NotifyJObj, AccountJObj) ->
    Admin = notify_util:find_admin(kz_json:get_value(<<"Authorized-By">>, NotifyJObj)),

    PortDoc = find_port_info(NotifyJObj),
    PortData = notify_util:json_to_template_props(kz_doc:public_fields(PortDoc)),
    Request = props:delete_keys([<<"uploads">>, <<"numbers">>], PortData),

    [Number|_]=Numbers = find_numbers(PortData, NotifyJObj),

    [{<<"numbers">>, Numbers}
    ,{<<"number">>, Number}
    ,{<<"request">>, Request}
    ,{<<"account">>, notify_util:json_to_template_props(AccountJObj)}
    ,{<<"admin">>, notify_util:json_to_template_props(Admin)}
    ,{<<"service">>, notify_util:get_service_props(AccountJObj, ?MOD_CONFIG_CAT)}
    ,{<<"send_from">>, get_send_from(PortDoc, Admin)}
    ].

-spec get_send_from(kz_json:object(), kz_json:object()) -> ne_binary().
get_send_from(PortDoc, Admin) ->
    case kz_json:get_first_defined([<<"email">>
                                   ,[<<"Port">>, <<"email">>]
                                   ], PortDoc)
    of
        'undefined' -> get_admin_send_from(Admin);
        Email -> Email
    end.

-spec get_admin_send_from(kz_json:object()) -> ne_binary().
get_admin_send_from(Admin) ->
    case kz_json:get_ne_value(<<"email">>, Admin) of
        'undefined' -> get_default_from();
        Email -> Email
    end.

-spec get_default_from() -> ne_binary().
get_default_from() ->
    DefaultFrom = kz_term:to_binary(node()),
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"default_from">>, DefaultFrom).

-spec find_numbers(kz_proplist(), kz_json:object()) -> ne_binaries().
find_numbers(PortData, NotifyJObj) ->
    Numbers = case props:get_value(<<"numbers">>, PortData) of
                  'undefined' -> find_numbers(NotifyJObj);
                  Ns -> Ns
              end,
    [normalize_find_numbers(Number) || Number <- Numbers].

-spec find_numbers(kz_json:object()) -> ne_binaries() | kz_proplists().
find_numbers(NotifyJObj) ->
    [kz_json:get_value(<<"Number">>, NotifyJObj)].

-spec find_port_info(kz_json:object()) -> kz_json:object().
find_port_info(NotifyJObj) ->
    case kz_json:get_ne_value(<<"Port-Request-ID">>, NotifyJObj) of
        'undefined' -> NotifyJObj;
        PortRequestId ->
            Doc = find_port_doc(PortRequestId),
            kz_json:set_value(<<"port_id">>, PortRequestId, Doc)
    end.

-spec find_port_doc(ne_binary()) -> kz_json:object().
find_port_doc(PortRequestId) ->
    case kz_datamgr:open_cache_doc(?KZ_PORT_REQUESTS_DB, PortRequestId) of
        {'ok', PortDoc} -> PortDoc;
        {'error', _} -> kz_json:new()
    end.

-spec normalize_find_numbers(kz_proplist() | ne_binary()) -> ne_binary().
normalize_find_numbers({Number, _}) -> Number;
normalize_find_numbers(Number) -> Number.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | ne_binaries(), kz_proplist(), list()) -> 'ok'.
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
-type attachment() :: {ne_binary(), ne_binary(), kz_proplist(), kz_proplist(), ne_binary()}.
-type attachments() :: [attachment()].

-spec get_attachments(kz_json:object()) -> attachments().
get_attachments(JObj) ->
    case kz_json:get_ne_value(<<"Port-Request-ID">>, JObj) of
        'undefined' -> get_number_attachments(JObj);
        PortRequestId -> get_port_attachments(PortRequestId)
    end.

-spec get_number_attachments(kz_json:object()) -> attachments().
get_number_attachments(JObj) ->
    Number = knm_converters:normalize(kz_json:get_value(<<"Number">>, JObj)),
    NumberDb = knm_converters:to_db(Number),
    case kz_datamgr:open_doc(NumberDb, Number) of
        {'ok', NumberJObj} ->
            Attachments = kz_doc:attachments(NumberJObj, kz_json:new()),
            fetch_attachments(kz_json:to_proplist(Attachments), NumberDb, Number);
        _ -> []
    end.

-spec get_port_attachments(ne_binary()) -> attachments().
get_port_attachments(PortRequestId) ->
    case kz_datamgr:open_cache_doc(?KZ_PORT_REQUESTS_DB, PortRequestId) of
        {'ok', PortJObj} ->
            Attachments = kz_doc:attachments(PortJObj, kz_json:new()),
            fetch_attachments(kz_json:to_proplist(Attachments), ?KZ_PORT_REQUESTS_DB, PortRequestId);
        _ -> []
    end.

-spec fetch_attachments(kz_proplist(), ne_binary(), ne_binary()) -> attachments().
fetch_attachments(Attachments, Db, Id) ->
    fetch_attachments(Attachments, Db, Id, []).

-spec fetch_attachments(kz_proplist(), ne_binary(), ne_binary(), attachments()) -> attachments().
fetch_attachments([], _, _, EmailAttachments) -> EmailAttachments;
fetch_attachments([{AttachmentName, AttachmentJObj}|Attachments], Db, Id, EmailAttachments) ->
    case kz_datamgr:fetch_attachment(Db, Id, fix_attachment_name(AttachmentName)) of
        {'ok', AttachmentBin} ->
            Attachment = create_attachment(AttachmentName, AttachmentJObj, AttachmentBin),
            fetch_attachments(Attachments, Db, Id, [Attachment|EmailAttachments]);
        _E ->
            lager:debug("failed to attach ~s: ~p", [AttachmentName, _E]),
            fetch_attachments(Attachments, Db, Id, EmailAttachments)
    end.

-spec create_attachment(ne_binary(), kz_json:object(), ne_binary()) -> attachment().
create_attachment(AttachmentName, AttachmentJObj, AttachmentBin) ->
    [Type, Subtype] =
        binary:split(kz_json:get_ne_value(<<"content_type">>, AttachmentJObj, <<"application/octet-stream">>), <<"/">>),
    lager:debug("found attachment ~s (~s/~s)", [AttachmentName, Type, Subtype]),
    {Type, Subtype
    ,[{<<"Content-Disposition">>, list_to_binary([<<"attachment; filename=\"">>, AttachmentName, "\""])}
     ,{<<"Content-Type">>, list_to_binary([Type, "/", Subtype, <<"; name=\"">>, AttachmentName, "\""])}
     ,{<<"Content-Transfer-Encoding">>, <<"base64">>}
     ]
    ,[], AttachmentBin
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fix_attachment_name(ne_binary() | list()) -> ne_binary().
fix_attachment_name(Name) when is_binary(Name) ->
    fix_attachment_name(kz_term:to_list(Name));
fix_attachment_name(Name) ->
    kz_term:to_binary(http_uri:encode(Name)).
