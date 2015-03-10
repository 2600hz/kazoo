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

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".new_user">>).

-define(TEMPLATE_ID, <<"new_user">>).
-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"user.first_name">>, <<"first_name">>, <<"First Name">>, <<"First Name">>)
            ,?MACRO_VALUE(<<"user.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last Name">>)
            ,?MACRO_VALUE(<<"user.password">>, <<"password">>, <<"Password">>, <<"Password">>)
            | ?SERVICE_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"Welcome {{user.first_name}} {{user.last_name}}.\n\nYour password is : {{user.password}}.\n Make sure to change it.">>).
-define(TEMPLATE_HTML, <<"<p>Welcome {{user.first_name}} {{user.last_name}}.</p><p>Your password is : {{user.password}}.</p><p>Make sure to change it.</p>">>).
-define(TEMPLATE_SUBJECT, <<"New user">>).
-define(TEMPLATE_CATEGORY, <<"new_user">>).
-define(TEMPLATE_NAME, <<"new_user">>).

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
    erlang:put(server_id, wh_api:server_id(JObj)),
    wh_util:put_callid(JObj),
    %% Gather data for template
    DataJObj = wh_json:normalize(wh_api:remove_defaults(JObj)),
    case teletype_util:should_handle_notification(DataJObj) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(DataJObj)
    end.

-spec handle_req(wh_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    {'ok', AccountJObj} = couch_mgr:open_cache_doc(AccountDb, AccountId),

    UserId = wh_json:get_value(<<"user_id">>, DataJObj),
    {'ok', UserJObj} = couch_mgr:open_cache_doc(AccountDb, UserId),
    Password = wh_json:get_value(<<"password">>, DataJObj),

    ReqData =
        wh_json:set_values([
            {<<"account">>, AccountJObj}
            ,{<<"user">>, wh_json:set_value(<<"password">>, Password, UserJObj)}
            ,{<<"to">>, [wh_json:get_ne_value(<<"email">>, UserJObj)]}
        ], DataJObj),
    case wh_json:is_true(<<"preview">>, DataJObj, 'false') of
        'false' -> process_req(ReqData);
        'true' ->
            process_req(wh_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec process_req(wh_json:object()) -> 'ok'.
-spec process_req(wh_json:object(), wh_proplist()) -> 'ok'.
process_req(DataJObj) ->
    _ = send_update(<<"pending">>),
    %% Load templates
    process_req(DataJObj, teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj)).

process_req(_DataJObj, []) ->
    lager:debug("no templates to render for ~s", [?TEMPLATE_ID]);
process_req(DataJObj, Templates) ->
    ServiceData = teletype_util:service_params(DataJObj, ?MOD_CONFIG_CAT),

    Macros = [{<<"service">>, ServiceData}
              ,{<<"account">>, public_proplist(<<"account">>, DataJObj)}
              ,{<<"user">>, public_proplist(<<"user">>, DataJObj)}
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

    %% Send email
    case teletype_util:send_email(Emails
                                  ,Subject
                                  ,ServiceData
                                  ,RenderedTemplates
                                 )
    of
        'ok' -> send_update(<<"completed">>);
        {'error', Reason} -> send_update(<<"failed">>, Reason)
    end.

-spec public_proplist(wh_json:key(), wh_json:object()) -> wh_proplist().
public_proplist(Key, JObj) ->
    wh_json:to_proplist(
        wh_json:public_fields(
            wh_json:get_value(Key, JObj, wh_json:new())
        )
    ).

-spec send_update(ne_binary()) -> 'ok'.
-spec send_update(ne_binary(), api_binary()) -> 'ok'.
send_update(Status) ->
    send_update(Status, 'undefined').

send_update(Status, Reason) ->
    teletype_util:send_update(
        wh_json:from_list([{<<"server_id">>, erlang:get('server_id')}])
        ,Status
        ,Reason
    ).
