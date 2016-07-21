%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_low_balance).

-export([init/0
        ,handle_low_balance/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"low_balance">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(?ACCOUNT_MACROS)
       ).

-define(TEMPLATE_TEXT, <<"The account \"{{account.name}}\" has less than {{threshold}} of credit remaining.\nIf the account runs out of credit it will not be able to make or receive per-minute calls.\nThe current balance is: {{current_balance}}\n\nAccount ID: {{account.id}}">>).
-define(TEMPLATE_HTML, <<"<html><body><h2>The account \"{{account.name}}\" has less than {{threshold}} of credit remaining.</h2><p>Current Balance: {{current_balance}}</p><p>If the account runs out of credit it will not be able to make or receive per-minute calls.</body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Account {{account.name}} is running out of credit">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"Low Balance">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
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

-spec handle_low_balance(kz_json:object(), kz_proplist()) -> 'ok'.
handle_low_balance(JObj, _Props) ->
    'true' = kapi_notifications:low_balance_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(DataJObj)
    end.

-spec get_current_balance(kz_json:object()) -> ne_binary().
get_current_balance(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    Dollars = wht_util:current_account_dollars(AccountId),
    wht_util:pretty_print_dollars(Dollars).

-spec get_balance_threshold(kz_json:object()) -> ne_binary().
get_balance_threshold(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    wht_util:pretty_print_dollars(kz_account:low_balance_threshold(AccountId)).

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"current_balance">>, get_current_balance(DataJObj)}
             ,{<<"threshold">>, get_balance_threshold(DataJObj)}
              | build_macro_data(DataJObj)
             ],

    %% Load templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, teletype_util:find_account_id(DataJObj)),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                                          ,Macros
               ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec build_macro_data(kz_json:object()) -> kz_proplist().
build_macro_data(DataJObj) ->
    kz_json:foldl(fun(MacroKey, _V, Acc) ->
                          maybe_add_macro_key(MacroKey, Acc, DataJObj)
                  end
                 ,[]
                 ,?TEMPLATE_MACROS
                 ).

-spec maybe_add_macro_key(kz_json:key(), kz_proplist(), kz_json:object()) -> kz_proplist().
maybe_add_macro_key(<<"user.", UserKey/binary>>, Acc, DataJObj) ->
    maybe_add_user_data(UserKey, Acc, DataJObj);
maybe_add_macro_key(_Key, Acc, _DataJObj) ->
    lager:debug("unprocessed macro key ~s: ~p", [_Key, _DataJObj]),
    Acc.

-spec maybe_add_user_data(kz_json:key(), kz_proplist(), kz_json:object()) -> kz_proplist().
maybe_add_user_data(Key, Acc, DataJObj) ->
    User = get_user(DataJObj),

    UserMacros = props:get_value(<<"user">>, Acc, []),

    case kz_json:get_value(Key, User) of
        'undefined' ->
            lager:debug("unprocessed user macro key ~s: ~p", [Key, User]),
            Acc;
        V -> props:set_value(<<"user">>, [{Key, V} | UserMacros], Acc)
    end.

-spec get_user(kz_json:object()) -> kz_json:object().
get_user(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    UserId = kz_json:get_value(<<"user_id">>, DataJObj),

    case kz_datamgr:open_cache_doc(AccountDb, UserId) of
        {'ok', UserJObj} -> UserJObj;
        {'error', _E} ->
            lager:debug("failed to find user ~s in ~s: ~p", [UserId, AccountId, _E]),
            kz_json:new()
    end.
