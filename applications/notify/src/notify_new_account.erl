%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_new_account).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_new_account_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_new_account_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_new_account_subj_tmpl').

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".new_account">>).

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
-spec handle_req(wh_json:object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:new_account_v(JObj),
    whapps_util:put_callid(JObj),

    lager:debug("a new account has been created, sending email notification"),

    AccountDb = case {wh_json:get_value(<<"Account-DB">>, JObj), wh_json:get_value(<<"Account-ID">>, JObj)} of
                    {'undefined', 'undefined'} -> 'undefined';
                    {'undefined', Id1} -> wh_util:format_account_id(Id1, 'encoded');
                    {Id2, _} -> Id2
                end,

    lager:debug("attempting to load all docs in account db ~s", [AccountDb]),
    {'ok', AllDocs} = couch_mgr:all_docs(AccountDb, [{<<"include_docs">>, 'true'}]),
    Account = find_account(AllDocs),
    Admin = find_admin(AllDocs),

    Props = create_template_props(JObj, Admin, Account, AllDocs),

    lager:debug("creating new account notice"),

    CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"new_account">>, <<"email_text_template">>], Account),
    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"new_account">>, <<"email_html_template">>], Account),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"new_account">>, <<"email_subject_template">>], Account),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    To = wh_json:get_value(<<"email">>, Admin, whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>, <<"">>)),
    RepEmail = notify_util:get_rep_email(Account),

    _ = build_and_send_email(TxtBody, HTMLBody, Subject, To, Props),
    build_and_send_email(TxtBody, HTMLBody, Subject, RepEmail, Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object(), wh_json:object(), wh_json:object(), wh_json:objects()) ->
                                   wh_proplist().
create_template_props(Event, Admin, Account, AllDocs) ->
    Owners = [{wh_json:get_value([<<"doc">>, <<"_id">>], J1), wh_json:get_value(<<"doc">>, J1)}
              || J1 <- AllDocs,
                 wh_json:get_value([<<"doc">>, <<"pvt_type">>], J1) =:= <<"user">>
             ],
    DevicesWithOwners = [begin
                             J3 = wh_json:get_value(<<"doc">>, J2),
                             wh_json:set_value(<<"user">>, props:get_value(wh_json:get_value(<<"owner_id">>, J3), Owners), J3)
                         end
                         || J2 <- AllDocs,
                            wh_json:get_value([<<"doc">>, <<"pvt_type">>], J2) =:= <<"device">>
                        ],
    [{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"devices">>, notify_util:json_to_template_props(DevicesWithOwners)}
     ,{<<"admin">>, notify_util:json_to_template_props(Admin)}
     ,{<<"request">>, notify_util:json_to_template_props(Event)}
     ,{<<"service">>, notify_util:get_service_props(Event, Account, ?MOD_CONFIG_CAT)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), api_binary() | ne_binaries(), wh_proplist()) ->
                                  'ok' | {'error', _}.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Service = props:get_value(<<"service">>, Props),
    From = props:get_value(<<"send_from">>, Service),

    {ContentTypeParams, CharsetString} = notify_util:get_charset_params(Service),
    PlainTransferEncoding = whapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"text_content_transfer_encoding">>),
    HTMLTransferEncoding = whapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"html_content_transfer_encoding">>),

    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
                 ,[{<<"From">>, From}
                   ,{<<"To">>, To}
                   ,{<<"Subject">>, Subject}
                  ]
             ,ContentTypeParams
             ,[{<<"multipart">>, <<"alternative">>, [], []
                ,[{<<"text">>, <<"plain">>
                       ,props:filter_undefined(
                          [{<<"Content-Type">>, iolist_to_binary([<<"text/plain">>, CharsetString])}
                           ,{<<"Content-Transfer-Encoding">>, PlainTransferEncoding}
                          ])
                   ,[], iolist_to_binary(TxtBody)}
                  ,{<<"text">>, <<"html">>
                        ,props:filter_undefined(
                           [{<<"Content-Type">>, iolist_to_binary([<<"text/html">>, CharsetString])}
                            ,{<<"Content-Transfer-Encoding">>, HTMLTransferEncoding}
                           ])
                    ,[], iolist_to_binary(HTMLBody)}
                 ]
               }
              ]
            },
    notify_util:send_email(From, To, Email).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_account(wh_json:objects()) -> wh_json:object().
find_account([]) ->
    wh_json:new();
find_account([Doc|Docs]) ->
    JObj = wh_json:get_value(<<"doc">>, Doc),
    case wh_doc:type(JObj) of
        <<"account">> -> JObj;
        _ -> find_account(Docs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_admin(wh_json:objects()) -> wh_json:object().
find_admin([]) -> wh_json:new();
find_admin([Doc|Docs]) ->
    JObj = wh_json:get_value(<<"doc">>, Doc),
    case wh_doc:type(JObj) =:= <<"user">>
        andalso wh_json:get_value(<<"priv_level">>, JObj) =:= <<"admin">>
    of
        'true' -> JObj;
        'false' -> find_admin(Docs)
    end.
