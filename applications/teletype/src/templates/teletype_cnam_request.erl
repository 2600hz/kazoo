%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_cnam_request).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"cnam_request">>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"request.number">>, <<"request_number">>, <<"Number">>, <<"Number to add CNAM">>)
          ,?MACRO_VALUE(<<"cnam.display_name">>, <<"cnam_display_name">>, <<"Display Name">>, <<"What to display">>)
          ,?MACRO_VALUE(<<"request.number_state">>, <<"request_number_state">>, <<"Number State">>, <<"Number state">>)
          ,?MACRO_VALUE(<<"request.local_number">>, <<"request_local_number">>, <<"Local Number">>, <<"Is a local number">>)
          ,?MACRO_VALUE(<<"request.acquired_for">>, <<"request_acquired_for">>, <<"Acquired For">>, <<"Who authorized the request">>)
           | ?USER_MACROS
           ++ ?COMMON_TEMPLATE_MACROS
          ]
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Caller name update request for {{request.number}}">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"CNAM Request">>).

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
    teletype_bindings:bind(<<"cnam_request">>, ?MODULE, 'handle_req').


-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:cnam_request_v(JObj)).

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
    CNAMJObj =
        kz_json:set_values([{<<"request">>, DataJObj}
                           ,{<<"cnam">>, cnam_data(DataJObj)}
                           ]
                          ,kz_json:merge_jobjs(DataJObj, ReqData)
                          ),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> teletype_util:notification_disabled(DataJObj, ?TEMPLATE_ID);
        'true' -> process_req(CNAMJObj)
    end.

-spec cnam_data(kz_json:object()) -> api_object().
cnam_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'false' ->
            kz_json:get_json_value(<<"cnam">>, DataJObj);
        'true' ->
            kz_json:from_list([{<<"display_name">>, <<"Display Name">>}])
    end.

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
             ,{<<"request">>, teletype_util:public_proplist(<<"request">>, DataJObj)}
             ,{<<"cnam">>, teletype_util:public_proplist(<<"cnam">>, DataJObj)}
             ],

    %% Populate templates
    RenderedTemplates =
        teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(?TEMPLATE_ID
                                             ,kapi_notifications:account_id(DataJObj)
                                             ),

    Subject =
        teletype_util:render_subject(
          kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                    ,Macros
         ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?TEMPLATE_ID),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(?TEMPLATE_ID);
        {'error', Reason} -> teletype_util:notification_failed(?TEMPLATE_ID, Reason)
    end.
