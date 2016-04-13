%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Kirill Sysoev
%%%-------------------------------------------------------------------
-module(teletype_customer_update).

-export([init/0
         ,handle_req/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"customer_update">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(ACC_CHILDREN_LIST, <<"accounts/listing_by_children">>).
-define(ACC_USERS_LIST, <<"users/crossbar_listing">>).

-define(TEMPLATE_MACROS
        ,wh_json:from_list(
           [?MACRO_VALUE(<<"user.first_name">>, <<"first_name">>, <<"First Name">>, <<"First Name">>)
            ,?MACRO_VALUE(<<"user.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Last Name">>)
            | ?USER_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"Dear {{user.first_name}} {{user.last_name}}.\n\nHere are some news that we have selected for you.\n\nBest regards,">>).
-define(TEMPLATE_HTML, <<"<p>Dear {{user.first_name}} {{user.last_name}}.</p><p>Here are some news that we have selected for you.</p><p>Best regards,</p>">>).
-define(TEMPLATE_SUBJECT, <<"Customer update">>).
-define(TEMPLATE_CATEGORY, <<"user">>).
-define(TEMPLATE_NAME, <<"Customer update">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_templates:init(?TEMPLATE_ID, [{'macros', ?TEMPLATE_MACROS}
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

-spec handle_req(wh_json:object(), wh_proplist()) -> wh_proplist()|'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:customer_update_v(JObj),
    DataJObj = wh_json:normalize(JObj),
    AccountId = wh_json:get_value(<<"account_id">>, DataJObj),
    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> process_req(DataJObj)
    end.

-spec process_req(wh_json:object()) -> wh_proplist()|'ok'.
process_req(DataJObj) ->
    case wh_json:get_value(<<"recipient_id">>, DataJObj) of
        <<RecipientId:32/binary>> -> process_account(RecipientId, DataJObj);
        'undefined' -> process_accounts(DataJObj)
    end.

-spec process_accounts(wh_json:object()) -> wh_proplist()|'ok'.
process_accounts(DataJObj) ->
    SenderId = wh_json:get_value(<<"account_id">>, DataJObj),
    ViewOpts = [{'startkey', [SenderId]}
               ,{'endkey', [SenderId, wh_json:new()]}
               ],
    case kz_datamgr:get_results(?WH_ACCOUNTS_DB, ?ACC_CHILDREN_LIST, ViewOpts) of
        {'ok', Accounts} ->
            [process_account(wh_json:get_value(<<"id">>, Account), DataJObj) || Account <- Accounts];
        {'error', _Reason} = E ->
            lager:info("failed to load children. error: ~p", [E])
    end.

-spec process_account(ne_binary(), wh_json:object()) -> wh_proplist()|'ok'.
process_account(AccountId, DataJObj) ->
    case wh_json:get_value(<<"user_type">>, DataJObj) of
        <<UserId:32/binary>> ->
            {'ok', UserJObj} = kzd_user:fetch(AccountId, UserId),
            send_update_to_user(UserJObj, DataJObj);
        _ ->
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            {'ok', Users} = kz_datamgr:get_results(AccountDb, ?ACC_USERS_LIST, []),
            select_users_to_update(lists:map(fun(User) -> wh_json:get_value(<<"value">>, User) end, Users), DataJObj)
    end.

-spec select_users_to_update(wh_proplist(), wh_json:object()) -> wh_proplist().
select_users_to_update(Users, DataJObj) ->
    case wh_json:get_value(<<"user_type">>, DataJObj) of
        <<"all_users">> ->
            [send_update_to_user(User, DataJObj) || User <- Users];
        _ ->
            [send_update_to_user(User, DataJObj) || User <- Users, wh_json:get_value(<<"priv_level">>, User) == <<"admin">>]
    end.

-spec send_update_to_user(wh_json:object(), wh_json:object()) -> 'ok'.
send_update_to_user(UserJObj, DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              | build_macro_data(UserJObj)
             ],

    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj, 'true'),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, teletype_util:find_account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                wh_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                ,Macros
               ),
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),
    To = [wh_json:get_value(<<"email">>, UserJObj)],
    case teletype_util:send_email(props:set_value(<<"to">>, To, Emails), Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec build_macro_data(wh_json:object()) -> wh_proplist().
build_macro_data(UserJObj) ->
    wh_json:foldl(fun(MacroKey, _V, Acc) ->
                          maybe_add_macro_key(MacroKey, Acc, UserJObj)
                  end
                  ,[]
                  ,?TEMPLATE_MACROS
                 ).

-spec maybe_add_macro_key(wh_json:key(), wh_proplist(), wh_json:object()) -> wh_proplist().
maybe_add_macro_key(<<"user.", UserKey/binary>>, Acc, UserJObj) ->
    maybe_add_user_data(UserKey, Acc, UserJObj);
maybe_add_macro_key(_Key, Acc, _UserJObj) ->
    lager:debug("unprocessed macro key ~s: ~p", [_Key, _UserJObj]),
    Acc.

-spec maybe_add_user_data(wh_json:key(), wh_proplist(), wh_json:object()) -> wh_proplist().
maybe_add_user_data(Key, Acc, UserJObj) ->
    UserMacros = props:get_value(<<"user">>, Acc, []),
    case wh_json:get_value(Key, UserJObj) of
        'undefined' ->
            lager:debug("unprocessed user macro key ~s: ~p", [Key, UserJObj]),
            Acc;
        V -> props:set_value(<<"user">>, [{Key, V} | UserMacros], Acc)
    end.
