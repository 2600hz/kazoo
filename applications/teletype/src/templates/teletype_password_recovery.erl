%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_password_recovery).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"password_recovery">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"link">>, <<"link">>, <<"Password Reset Link">>, <<"Link to reset password">>)
           | ?USER_MACROS
           ++ ?COMMON_TEMPLATE_MACROS
          ]
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Reset your VoIP services user password">>).
-define(TEMPLATE_CATEGORY, <<"user">>).
-define(TEMPLATE_NAME, <<"Password Recovery">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address()).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to()).

-spec init() -> 'ok'.
init() ->
    kz_log:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
                                          ,{'subject', ?TEMPLATE_SUBJECT}
                                          ,{'category', ?TEMPLATE_CATEGORY}
                                          ,{'friendly_name', ?TEMPLATE_NAME}
                                          ,{'to', ?TEMPLATE_TO}
                                          ,{'from', ?TEMPLATE_FROM}
                                          ,{'cc', ?TEMPLATE_CC}
                                          ,{'bcc', ?TEMPLATE_BCC}
                                          ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                          ]),
    teletype_bindings:bind(<<"password_recovery">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:password_recovery_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [?TEMPLATE_ID]),
    teletype_util:notification_failed(?TEMPLATE_ID, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?TEMPLATE_ID]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> teletype_util:notification_disabled(DataJObj, ?TEMPLATE_ID);
        'true' -> process_req(DataJObj)
    end.

-spec build_macro_data(kz_json:object()) -> kz_term:proplist().
build_macro_data(DataJObj) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"user">>, teletype_util:user_params(DataJObj)}
    ,{<<"link">>, [kz_json:get_value(<<"password_reset_link">>, DataJObj)]}
    ].

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    Macros = build_macro_data(DataJObj),

    %% Populate templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,kapi_notifications:account_id(DataJObj)
                                             ),

    Subject = teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                          ,Macros
                                          ),

    Emails0 = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?TEMPLATE_ID),
    Emails = props:set_value(<<"to">>, get_email_address(DataJObj, Emails0), Emails0),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} -> teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end.

-spec get_email_address(kz_json:object(), kz_term:proplist()) -> kz_term:api_ne_binaries().
get_email_address(DataJObj, Emails0) ->
    ToEmails = props:get_value(<<"to">>, Emails0),
    case kz_json:get_value(<<"email">>, DataJObj) of
        <<"Email">> ->
            case teletype_util:is_preview(DataJObj) of
                'true' -> ToEmails;
                'false' -> []
            end;
        ?NE_BINARY=UserEmail ->
            [UserEmail];
        [] ->
            ToEmails;
        Emails when is_list(Emails) ->
            case [E || E <- Emails, kz_term:is_ne_binary(E)] of
                [] -> ToEmails;
                Es -> Es
            end;
        _Other ->
            ToEmails
    end.
