%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_template_skel).

-export([init/0
         ,handle_req/2
        ]).

-include("../teletype.hrl").

-define(TEMPLATE_ID, <<"skel">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"user.first_name">>, <<"first_name">>, <<"First Name">>, <<"First Name">>)
            ,?MACRO_VALUE(<<"user.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last Name">>)
            | ?USER_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"Hi {{user.first_name}} {{user.last_name}}.\n\nThis is the skeleton template\nBrought to you by VoIP Services">>).
-define(TEMPLATE_HTML, <<"<p>Hi {{user.first_name}} {{user.last_name}}.</p><p>This is the skeleton template</p><p>Brought to you by VoIP Services</p>">>).
-define(TEMPLATE_SUBJECT, <<"Skeleton Template">>).
-define(TEMPLATE_CATEGORY, <<"skel">>).
-define(TEMPLATE_NAME, <<"Skeleton">>).

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
    'true' = wapi_notifications:skel_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(JObj),
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(wh_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"system">>, teletype_util:account_params(DataJObj)}
              | build_macro_data(DataJObj)
             ],

    %% Load templates
    Templates = teletype_util:fetch_templates(?TEMPLATE_ID, DataJObj),

    %% Populate templates
    RenderedTemplates = [{ContentType, teletype_util:render(?TEMPLATE_ID, Template, Macros)}
                         || {ContentType, Template} <- Templates
                        ],

    {'ok', TemplateMetaJObj} = teletype_util:fetch_template_meta(?TEMPLATE_ID, teletype_util:find_account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                ,Macros
               ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec build_macro_data(wh_json:object()) -> wh_proplist().
build_macro_data(DataJObj) ->
    wh_json:foldl(fun(MacroKey, _V, Acc) ->
                          maybe_add_macro_key(MacroKey, Acc, DataJObj)
                  end
                  ,[]
                  ,?TEMPLATE_MACROS
                 ).

-spec maybe_add_macro_key(wh_json:key(), wh_proplist(), wh_json:object()) -> wh_proplist().
maybe_add_macro_key(<<"user.", UserKey/binary>>, Acc, DataJObj) ->
    maybe_add_user_data(UserKey, Acc, DataJObj);
maybe_add_macro_key(_Key, Acc, _DataJObj) ->
    lager:debug("unprocessed macro key ~s: ~p", [_Key, _DataJObj]),
    Acc.

-spec maybe_add_user_data(wh_json:key(), wh_proplist(), wh_json:object()) -> wh_proplist().
maybe_add_user_data(Key, Acc, DataJObj) ->
    User = get_user(DataJObj),

    UserMacros = props:get_value(<<"user">>, Acc, []),

    case wh_json:get_value(Key, User) of
        'undefined' ->
            lager:debug("unprocessed user macro key ~s: ~p", [Key, User]),
            Acc;
        V -> props:set_value(<<"user">>, [{Key, V} | UserMacros], Acc)
    end.

-spec get_user(wh_json:object()) -> wh_json:object().
get_user(DataJObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    UserId = wh_json:get_value(<<"user_id">>, DataJObj),

    case couch_mgr:open_cache_doc(AccountDb, UserId) of
        {'ok', UserJObj} -> UserJObj;
        {'error', _E} ->
            lager:debug("failed to find user ~s in ~s: ~p", [UserId, AccountId, _E]),
            case teletype_util:is_preview(DataJObj) of
                'false' -> throw({'error', 'not_found'});
                'true' -> wh_json:new()
            end
    end.
