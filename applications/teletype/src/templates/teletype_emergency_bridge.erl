%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2022, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_emergency_bridge).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"emergency_bridge">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"call.outbound_caller_id_name">>, <<"outbound_caller_id_name">>, <<"Outbound Caller ID Name">>, <<"Outbound Caller ID Name">>)
          ,?MACRO_VALUE(<<"call.outbound_caller_id_number">>, <<"outbound_caller_id_number">>, <<"Outbound Caller ID Number">>, <<"Outbound Caller ID Number">>)
          ,?MACRO_VALUE(<<"call.emergency_caller_id_name">>, <<"emergency_caller_id_name">>, <<"Emergency Caller ID Name">>, <<"Emergency Caller ID Name">>)
          ,?MACRO_VALUE(<<"call.emergency_caller_id_number">>, <<"emergency_caller_id_number">>, <<"Emergency Caller ID Number">>, <<"Emergency Caller ID Number">>)
          ,?MACRO_VALUE(<<"call.call_id">>, <<"call_id">>, <<"Call ID">>, <<"Call ID">>)
          ,?MACRO_VALUE(<<"call.device_name">>, <<"device_name">>, <<"Device Name">>, <<"Device Name">>)
          ,?MACRO_VALUE(<<"call.emergency_address_street_1">>, <<"emergency_address_street_1">>, <<"Emergency Address Street 1">>, <<"Emergency Address Street 1">>)
          ,?MACRO_VALUE(<<"call.emergency_address_street_2">>, <<"emergency_address_street_2">>, <<"Emergency Address Street 2">>, <<"Emergency Address Street 2">>)
          ,?MACRO_VALUE(<<"call.emergency_address_city">>, <<"emergency_address_city">>, <<"Emergency Address City">>, <<"Emergency Address City">>)
          ,?MACRO_VALUE(<<"call.emergency_address_region">>, <<"emergency_address_region">>, <<"Emergency Address Region">>, <<"Emergency Address Region">>)
          ,?MACRO_VALUE(<<"call.emergency_address_postal_code">>, <<"emergency_address_postal_code">>, <<"Emergency Address Postal Code">>, <<"Emergency Address Postal Code">>)
          ,?MACRO_VALUE(<<"call.emergency_notification_contact_emails">>, <<"emergency_notification_contact_emails">>, <<"Emergency Contact Emails">>, <<"Emergency Contact Emails">>)
          ,?MACRO_VALUE(<<"call.emergency_test_call">>, <<"emergency_test_call">>, <<"Emergency Test Call">>, <<"Emergency Test Call">>)
          ,?MACRO_VALUE(<<"call.emergency_to_did">>, <<"emergency_to_did">>, <<"Emergency To DID">>, <<"Emergency To DID">>)
          ,?MACRO_VALUE(<<"call.owner_id">>, <<"owner_id">>, <<"Owner ID">>, <<"Owner ID">>)
          ,?MACRO_VALUE(<<"call.user_email">>, <<"user_email">>, <<"User Email">>, <<"User Email">>)
          ,?MACRO_VALUE(<<"call.user_first_name">>, <<"user_first_name">>, <<"User First Name">>, <<"User First Name">>)
          ,?MACRO_VALUE(<<"call.user_last_name">>, <<"user_last_name">>, <<"User Last Name">>, <<"User Last Name">>)
           | ?COMMON_TEMPLATE_MACROS
          ]
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Emergency{%if call.emergency_test_call %} Test{% endif %} Call Notification from Account '{{account.name}}'">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"Emergency Bridge">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address()).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to()).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
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
    teletype_bindings:bind(<<"emergency_bridge">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:emergency_bridge_v(JObj)).

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

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"call">>, kz_json:to_proplist(kz_api:remove_defaults(DataJObj))}
             ],
    %% Load templates
    Templates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),


    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, kapi_notifications:account_id(DataJObj)),

    Subject = teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                                          ,Macros
                                          ),

    Emails = maybe_update_to(DataJObj, TemplateMetaJObj),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} -> teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end.

-spec maybe_update_to(kz_json:object(), kz_json:object()) -> email_map().
maybe_update_to(DataJObj, TemplateMetaJObj) ->
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?TEMPLATE_ID),
    case kz_json:get_list_value(<<"emergency_notfication_contact_emails">>, DataJObj, []) of
        [] -> Emails;
        ToList -> lists:keyreplace(<<"to">>, 1, Emails, {<<"to">>, ToList})
    end.
