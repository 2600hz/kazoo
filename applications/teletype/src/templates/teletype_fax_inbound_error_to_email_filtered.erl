%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_fax_inbound_error_to_email_filtered).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"fax_inbound_error_to_email_filtered">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          ?FAX_ERROR_MACROS
          ++ ?FAX_MACROS
          ++ ?DEFAULT_CALL_MACROS
          ++ ?USER_MACROS
          ++ ?COMMON_TEMPLATE_MACROS
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Error receiving fax{% if caller_id.name_number or fax.remote_station_id %} from {% firstof caller_id.name_number fax.remote_station_id %}{% endif %}">>).
-define(TEMPLATE_CATEGORY, <<"fax">>).
-define(TEMPLATE_NAME, <<"Inbound Fax Receive Error to Email">>).

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
    teletype_bindings:bind(<<"inbound_fax_error">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:fax_inbound_error_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [?TEMPLATE_ID]),
    teletype_util:notification_failed(?TEMPLATE_ID, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?TEMPLATE_ID]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case is_notice_enabled(AccountId, JObj) of
        'disabled' -> teletype_util:notification_disabled(DataJObj, ?TEMPLATE_ID);
        'ignored' -> teletype_util:notification_ignored(?TEMPLATE_ID);
        TemplateMetaJObj ->
            lager:debug("handling ~s", [?TEMPLATE_ID]),
            teletype_fax_inbound_error_to_email:process_req(teletype_fax_util:add_data(DataJObj), ?TEMPLATE_ID, TemplateMetaJObj)
    end.

-spec is_notice_enabled(kz_term:ne_binary(), kz_json:object()) -> kz_json:object() | 'disabled' | 'ignored'.
is_notice_enabled(AccountId, JObj) ->
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, AccountId),
    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> 'disabled';
        'true' ->
            case is_true_fax_error(JObj, TemplateMetaJObj) of
                'true' -> TemplateMetaJObj;
                'false' -> 'ignored'
            end
    end.

%% see: https://wiki.freeswitch.org/wiki/Variable_fax_result_code
-spec is_true_fax_error(kz_json:object(), kz_json:object()) -> boolean().
is_true_fax_error(JObj, AccountTemplateJObj) ->
    DefaultCodes = teletype_util:template_system_value(?TEMPLATE_ID, <<"filter_error_codes">>, [<<"0">>, <<"49">>]),
    Code = kz_json:get_value(<<"Fax-Result-Code">>, JObj),
    Codes = kz_json:get_first_defined([<<"filter_error_codes">>, [<<"default">>, <<"filter_error_codes">>]]
                                     ,AccountTemplateJObj
                                     ,DefaultCodes
                                     ),
    not lists:member(Code, Codes).
