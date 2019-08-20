%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%%
%%%
%%% @author Karl Anderson <karl@2600hz.org>
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(notify_new_account).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_new_account_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_new_account_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_new_account_subj_tmpl').

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".new_account">>).

%%------------------------------------------------------------------------------
%% @doc initialize the module
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%------------------------------------------------------------------------------
%% @doc process the AMQP requests
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_notifications:new_account_v(JObj),
    kz_util:put_callid(JObj),

    lager:debug("a new account has been created, sending email notification"),

    RespQ = kz_api:server_id(JObj),
    MsgId = kz_api:msg_id(JObj),
    notify_util:send_update(RespQ, MsgId, <<"pending">>),

    AccountDb = case {kz_json:get_value(<<"Account-DB">>, JObj), kz_json:get_value(<<"Account-ID">>, JObj)} of
                    {'undefined', 'undefined'} -> 'undefined';
                    {'undefined', Id1} -> kz_util:format_account_id(Id1, 'encoded');
                    {Id2, _} -> Id2
                end,

    lager:debug("attempting to load all docs in account db ~s", [AccountDb]),
    {'ok', AllDocs} = kz_datamgr:all_docs(AccountDb, ['include_docs']),
    Account = find_account(AllDocs),
    Admin = find_admin(AllDocs),

    Props = create_template_props(JObj, Admin, Account, AllDocs),

    lager:debug("creating new account notice"),

    CustomTxtTemplate = kz_json:get_value([<<"notifications">>, <<"new_account">>, <<"email_text_template">>], Account),
    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = kz_json:get_value([<<"notifications">>, <<"new_account">>, <<"email_html_template">>], Account),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = kz_json:get_value([<<"notifications">>, <<"new_account">>, <<"email_subject_template">>], Account),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    To = kz_json:get_value(<<"email">>, Admin, kapps_config:get_ne_binary_or_ne_binaries(?MOD_CONFIG_CAT, <<"default_to">>)),
    RepEmail = notify_util:get_rep_email(Account),

    SendResult0 = build_and_send_email(TxtBody, HTMLBody, Subject, To, Props),
    SendResult1 = build_and_send_email(TxtBody, HTMLBody, Subject, RepEmail, Props),
    notify_util:maybe_send_update(lists:flatten([SendResult0, SendResult1]), RespQ, MsgId).

%%------------------------------------------------------------------------------
%% @doc create the props used by the template render function
%% @end
%%------------------------------------------------------------------------------
-spec create_template_props(kz_json:object(), kz_json:object(), kz_json:object(), kz_json:objects()) ->
                                   kz_term:proplist().
create_template_props(Event, Admin, Account, AllDocs) ->
    Owners = [{kz_json:get_value([<<"doc">>, <<"_id">>], J1), kz_json:get_value(<<"doc">>, J1)}
              || J1 <- AllDocs,
                 kz_json:get_value([<<"doc">>, <<"pvt_type">>], J1) =:= <<"user">>
             ],
    DevicesWithOwners = [begin
                             J3 = kz_json:get_value(<<"doc">>, J2),
                             kz_json:set_value(<<"user">>, props:get_value(kz_json:get_value(<<"owner_id">>, J3), Owners), J3)
                         end
                         || J2 <- AllDocs,
                            kz_json:get_value([<<"doc">>, <<"pvt_type">>], J2) =:= <<"device">>
                        ],
    [{<<"account">>, notify_util:json_to_template_props(Account)}
    ,{<<"devices">>, notify_util:json_to_template_props(DevicesWithOwners)}
    ,{<<"admin">>, notify_util:json_to_template_props(Admin)}
    ,{<<"request">>, notify_util:json_to_template_props(Event)}
    ,{<<"service">>, notify_util:get_service_props(Event, Account, ?MOD_CONFIG_CAT)}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), kz_term:api_binary() | kz_term:ne_binaries(), kz_term:proplist()) -> send_email_return().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
    [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Service = props:get_value(<<"service">>, Props),
    From = props:get_value(<<"send_from">>, Service),

    {ContentTypeParams, CharsetString} = notify_util:get_charset_params(Service),
    PlainTransferEncoding = kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"text_content_transfer_encoding">>, <<"7BIT">>),
    HTMLTransferEncoding = kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"html_content_transfer_encoding">>, <<"7BIT">>),

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_account(kz_json:objects()) -> kz_json:object().
find_account([]) ->
    kz_json:new();
find_account([Doc|Docs]) ->
    JObj = kz_json:get_value(<<"doc">>, Doc),
    case kz_doc:type(JObj) of
        <<"account">> -> JObj;
        _ -> find_account(Docs)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_admin(kz_json:objects()) -> kz_json:object().
find_admin([]) -> kz_json:new();
find_admin([Doc|Docs]) ->
    JObj = kz_json:get_value(<<"doc">>, Doc),
    case kz_doc:type(JObj) =:= <<"user">>
        andalso kzd_users:is_account_admin(JObj)
    of
        'true' -> JObj;
        'false' -> find_admin(Docs)
    end.
