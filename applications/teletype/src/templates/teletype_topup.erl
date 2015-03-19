%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_topup).

-export([init/0
         ,handle_req/2
        ]).

-include("../teletype.hrl").

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".topup">>).

-define(TEMPLATE_ID, <<"topup">>).
-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"user.first_name">>, <<"first_name">>, <<"First Name">>, <<"First Name">>)
            ,?MACRO_VALUE(<<"user.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last Name">>)
            | ?SERVICE_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"The account \"{{account.name}}\" has less than {{threshold}} of credit remaining.\n We are toping up the account for {{amount}}.">>).
-define(TEMPLATE_HTML, <<"<html><body><h2>The account \"{{account.name}}\" has less than {{threshold}} of credit remaining.</h2><p> We are toping up the account for {{amount}}.</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Account {{account.name}} has been top up">>).
-define(TEMPLATE_CATEGORY, <<"topup">>).
-define(TEMPLATE_NAME, <<"Top Up">>).

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
    'true' = wapi_notifications:topup_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_json:normalize(wh_api:remove_defaults(JObj)),

    case teletype_util:should_handle_notification(DataJObj) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(teletype_util:add_account(DataJObj))
    end.

-spec handle_req(wh_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    ServiceData = teletype_util:service_params(DataJObj, ?MOD_CONFIG_CAT),
    Macros = [{<<"service">>, ServiceData}
             ,{<<"account">>, teletype_util:public_proplist(<<"account">>, DataJObj)}
             ,{<<"threshold">>, teletype_util:get_balance_threshold(DataJObj)}
             ,{<<"amount">>, teletype_util:get_topup_amount(DataJObj)}
              | build_macro_data(DataJObj)],

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

    Emails = teletype_util:find_addresses(set_to_address(DataJObj), TemplateMetaJObj, ?MOD_CONFIG_CAT),

    %% Send email
    teletype_util:send_email(Emails, Subject, ServiceData, RenderedTemplates).

-spec build_macro_data(wh_json:object()) -> wh_proplist().
build_macro_data(DataJObj) ->
    wh_json:foldl(fun(MacroKey, _V, Acc) ->
                          maybe_add_macro_key(MacroKey, Acc, DataJObj)
                  end
                  ,[]
                  ,?TEMPLATE_MACROS
                 ).

-spec set_to_address(wh_json:object()) -> wh_json:object().
set_to_address(DataJObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    UserId = wh_json:get_value(<<"user_id">>, DataJObj),
    {'ok', UserJObj} = couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded')
                                                ,UserId
                                               ),
    wh_json:set_value(<<"to">>, [find_email(UserJObj)], DataJObj).

-spec find_email(wh_json:object()) -> api_binary().
find_email(UserJObj) ->
    wh_json:get_first_defined([<<"email">>, <<"username">>], UserJObj).

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
            case wh_json:is_true(<<"preview">>, DataJObj) of
                'false' -> throw({'error', 'not_found'});
                'true' -> wh_json:new()
            end
    end.
