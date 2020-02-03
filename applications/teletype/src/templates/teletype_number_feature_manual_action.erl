%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_number_feature_manual_action).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"number_feature_manual_action">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"request.number">>, <<"request_number">>, <<"Number">>, <<"Number that requires manual action">>)
          ,?MACRO_VALUE(<<"feature.action">>, <<"feature_action">>, <<"Action">>, <<"What manual action is required">>)
          ,?MACRO_VALUE(<<"feature.name">>, <<"feature_name">>, <<"Feature">>, <<"Feature for which the manual action is required">>)
          ,?MACRO_VALUE(<<"feature.provider">>, <<"feature_provider">>, <<"Provider">>, <<"Provider where manual action for the feature must be done">>)
           | ?USER_MACROS
           ++ ?COMMON_TEMPLATE_MACROS
          ]
         )
       ).

-define(TEMPLATE_SUBJECT, <<"{{feature.name}} manual {{feature.action}} required for {{request.number}}">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"Number Feature Manual Action Required">>).

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
    teletype_bindings:bind(<<"number_feature_manual_action">>, ?MODULE, 'handle_req').


-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:number_feature_manual_action_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [?TEMPLATE_ID]),
    teletype_util:notification_failed(?TEMPLATE_ID, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?TEMPLATE_ID]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    ReqData =
        kz_json:set_value(<<"user">>, teletype_util:find_account_admin(AccountId), DataJObj),
    FeatureJObj =
        kz_json:set_values([{<<"request">>, request_data(DataJObj)}
                           ,{<<"feature">>, feature_data(DataJObj)}
                           ]
                          ,kz_json:merge_jobjs(DataJObj, ReqData)
                          ),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> teletype_util:notification_disabled(DataJObj, ?TEMPLATE_ID);
        'true' -> process_req(FeatureJObj)
    end.

-spec feature_data(kz_json:object()) -> kz_term:api_object().
feature_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'false' ->
            kz_json:get_json_value(<<"feature">>, DataJObj);
        'true' ->
            kz_json:from_list([{<<"name">>, <<"awesome feature">>}
                              ,{<<"provider">>, <<"awesome provider">>}
                              ,{<<"action">>, <<"amazing action">>}
                              ])
    end.

-spec request_data(kz_json:object()) -> kz_term:api_object().
request_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'false' ->
            kz_json:from_list([{<<"number">>, kz_json:get_ne_binary_value(<<"number">>, DataJObj)}
                              ]);
        'true' ->
            kz_json:from_list([{<<"number">>, <<"+15550001234">>}
                              ])
    end.

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
             ,{<<"request">>, teletype_util:public_proplist(<<"request">>, DataJObj)}
             ,{<<"feature">>, teletype_util:public_proplist(<<"feature">>, DataJObj)}
             ],

    %% Populate templates
    RenderedTemplates =
        teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

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
