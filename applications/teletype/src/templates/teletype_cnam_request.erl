%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_cnam_request).

-export([init/0
        ,handle_cnam_request/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"cnam_request">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"request.number">>, <<"request_number">>, <<"Number">>, <<"Number to add CNAM">>)
          ,?MACRO_VALUE(<<"cnam.display_name">>, <<"cnam_display_name">>, <<"Display Name">>, <<"What to display">>)
          ,?MACRO_VALUE(<<"request.number_state">>, <<"request_number_state">>, <<"Number State">>, <<"Number State">>)
          ,?MACRO_VALUE(<<"request.local_number">>, <<"request_local_number">>, <<"Local Number">>, <<"Local Number">>)
           | ?ACCOUNT_MACROS ++ ?USER_MACROS
          ]
         )).

-define(TEMPLATE_SUBJECT, <<"Caller name update request for {{request.number}}">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"CNAM Request">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

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
                                          ]).

-spec handle_cnam_request(kz_json:object(), kz_proplist()) -> 'ok'.
handle_cnam_request(JObj, _Props) ->
    'true' = kapi_notifications:cnam_request_v(JObj),
    kz_util:put_callid(JObj),
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
        'false' -> lager:debug("notification handling not configured for this account");
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

-spec process_req(kz_json:object()) -> 'ok'.
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
                                             ,teletype_util:find_account_id(DataJObj)
                                             ),

    Subject =
        teletype_util:render_subject(
          kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
                                    ,Macros
         ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' ->
            teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} ->
            teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.
