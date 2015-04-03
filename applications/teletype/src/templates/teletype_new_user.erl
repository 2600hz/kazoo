%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_new_user).

-export([init/0
         ,handle_req/2
        ]).

-include("../teletype.hrl").

-define(TEMPLATE_ID, <<"new_user">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"user.password">>, <<"password">>, <<"Password">>, <<"Password">>)
            | ?USER_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"Welcome {{user.first_name}} {{user.last_name}}.\n\nYour password is : {{user.password}}.\n Make sure to change it.">>).
-define(TEMPLATE_HTML, <<"<p>Welcome {{user.first_name}} {{user.last_name}}.</p><p>Your password is : {{user.password}}.</p><p>Make sure to change it.</p>">>).
-define(TEMPLATE_SUBJECT, <<"New user">>).
-define(TEMPLATE_CATEGORY, <<"user">>).
-define(TEMPLATE_NAME, <<"New User">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_util:init_template(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
                                               ,{'text', ?TEMPLATE_TEXT}
                                               ,{'html', ?TEMPLATE_HTML}
                                               ,{'subject', ?TEMPLATE_SUBJECT}
                                               ,{'category', ?TEMPLATE_CATEGORY}
                                               ,{'friendly_name', ?TEMPLATE_NAME}
                                               ,{'to', ?TEMPLATE_TO}
                                               ,{'from', ?TEMPLATE_FROM}
                                               ,{'cc', ?TEMPLATE_CC}
                                               ,{'bcc', ?TEMPLATE_BCC}
                                               ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                              ]).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:new_user_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    {'ok', AccountJObj} = teletype_util:open_doc(<<"account">>, AccountId, DataJObj),

    case teletype_util:should_handle_notification(DataJObj)
        andalso teletype_util:is_notice_enabled(AccountJObj, JObj, ?TEMPLATE_ID)
    of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> do_handle_req(DataJObj, AccountJObj)
    end.

-spec do_handle_req(wh_json:object(), wh_json:object()) -> 'ok'.
do_handle_req(DataJObj, AccountJObj) ->
    UserId = wh_json:get_value(<<"user_id">>, DataJObj),
    {'ok', UserJObj} = teletype_util:open_doc(<<"user">>, UserId, DataJObj),
    Password = wh_json:get_value(<<"password">>, DataJObj),

    ReqData =
        wh_json:set_values(
          [{<<"account">>, AccountJObj}
           ,{<<"user">>, wh_json:set_value(<<"password">>, Password, UserJObj)}
           ,{<<"to">>, [wh_json:get_ne_value(<<"email">>, UserJObj)]}
          ]
          ,DataJObj
         ),

    case teletype_util:is_preview(DataJObj) of
        'false' ->
            process_req(ReqData);
        'true' ->
            process_req(wh_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec process_req(wh_json:object()) -> 'ok'.
-spec process_req(wh_json:object(), wh_proplist()) -> 'ok'.
process_req(DataJObj) ->
    %% Load templates
    process_req(DataJObj, teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj)).

process_req(_DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]);
process_req(DataJObj, Templates) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:public_proplist(<<"account">>, DataJObj)}
              ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
             ],

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} =
        teletype_util:fetch_template_meta(?TEMPLATE_ID
                                          ,teletype_util:find_account_id(DataJObj)
                                         ),

    Subject =
        teletype_util:render_subject(
          wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], ?TEMPLATE_SUBJECT)
          ,Macros
         ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' ->
            teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} ->
            teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.
