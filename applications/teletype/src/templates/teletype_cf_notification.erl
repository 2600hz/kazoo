%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Sergey Safarov <s.safarov@gmail.com>, Sponsored by Audian
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_cf_notification).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").


-spec init() -> 'ok'.
init() ->
    kz_log:put_callid(?MODULE),
    teletype_bindings:bind(<<"cf_notification">>, ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:cf_notification_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for cf_notification"),
    teletype_util:notification_failed(<<"cf_notification">>, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    TemplateId = kz_json:get_value(<<"template_id">>, DataJObj),
    case teletype_templates:templates_source(TemplateId, AccountId) of
        'undefined' ->
            lager:warning("cannot determine '~s' template notification source", [TemplateId]),
            teletype_util:notification_failed(<<"cf_notification">>, <<"validation_failed">>);
        'not_found' ->
            lager:warning("template notification '~s' is not exists", [TemplateId]),
            teletype_util:notification_failed(<<"cf_notification">>, <<"validation_failed">>);
        ?KZ_CONFIG_DB ->
            lager:warning("template notification '~s' is system global and cannot be used", [TemplateId]),
            teletype_util:notification_failed(<<"cf_notification">>, <<"validation_failed">>);
        AccountId ->
            lager:debug("valid data for '~s' notification, processing...", [TemplateId]),
            case teletype_util:is_notice_enabled(AccountId, JObj, TemplateId) of
                'false' -> teletype_util:notification_disabled(DataJObj, TemplateId);
                'true' -> process_req(DataJObj, TemplateId)
            end;
        _Resseler ->
            lager:warning("template notification '~s' is defined in reseller account and cannot be used", [TemplateId]),
            teletype_util:notification_failed(<<"cf_notification">>, <<"validation_failed">>)
    end.

-spec process_req(kz_json:object(), kz_json:ne_api_binary()) -> template_response().
process_req(DataJObj, TemplateId) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"call">>, kz_json:to_proplist(kz_api:remove_defaults(DataJObj))}
              | teletype_util:build_call_data(DataJObj, 'undefined')
             ],

    %% Load templates
    Templates = teletype_templates:render(TemplateId, Macros, DataJObj),

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(TemplateId, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(TemplateId, kapi_notifications:account_id(DataJObj)),

    Subject = teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                                          ,Macros
                                          ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, TemplateId),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(TemplateId);
        {'error', Reason} -> teletype_util:notification_failed(TemplateId, Reason)
    end.
