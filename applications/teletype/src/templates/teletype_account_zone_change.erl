%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% @author Mark Magnusson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_account_zone_change).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"account_zone_change">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"zones">>, <<"zones">>, <<"Zones">>, <<"List of account's zones">>)
           | ?COMMON_TEMPLATE_MACROS
          ]
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Account '{{account.name}}' zone have changed">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"Account Zone Change">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address()).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to()).

-spec init() -> template_response().
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
    teletype_bindings:bind(<<"account_zone_change">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:account_zone_change_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [?TEMPLATE_ID]),
    teletype_util:notification_failed(?TEMPLATE_ID, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?TEMPLATE_ID]),
    DataJObj  = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> teletype_util:notification_disabled(DataJObj, ?TEMPLATE_ID);
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"zones">>, zones_data(DataJObj)}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ],

    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,kapi_notifications:account_id(DataJObj)
                                             ),

    Subject =
        teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                    ,Macros
                                    ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?TEMPLATE_ID),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} -> teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end.

-spec zones_data(kz_json:object()) -> kz_term:proplist().
zones_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'false' ->
            kz_json:recursive_to_proplist(kz_json:get_json_value(<<"zones">>, DataJObj, []));
        'true' ->
            [{<<"home">>, <<"Zone">>}]
    end.
