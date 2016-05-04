%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_topup).

-export([init/0
         ,handle_topup/2
         ,get_balance/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"topup">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TOPUP_MACROS
       ,[?MACRO_VALUE(<<"amount">>, <<"amount">>, <<"Amount">>, <<"The top up amount">>)
         ,?MACRO_VALUE(<<"success">>, <<"success">>, <<"Success">>, <<"Whether or not the top up was successful">>)
         ,?MACRO_VALUE(<<"response">>, <<"response">>, <<"Response">>, <<"Transaction processor response">>)
         ,?MACRO_VALUE(<<"balance">>, <<"balance">>, <<"Balance">>, <<"The resulting account balance">>)
        ]).

-define(TEMPLATE_MACROS
        ,kz_json:from_list(?USER_MACROS
                           ++ ?ACCOUNT_MACROS
                           ++ ?TOPUP_MACROS
                          )
       ).

-define(TEMPLATE_TEXT, <<"Attempted to top-up account \"{{account.name}}\" for {{amount}}.  The transaction processor response was {{response}} resulting in a new balance of {{balance}}.">>).
-define(TEMPLATE_HTML, <<"<html><body><h2>Attempted to top-up account \"{{account.name}}\" for {{amount}}</h2><p>The transaction processor response was {{response}} resulting in a new balance of {{balance}}.</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Account {{account.name}} has been topped up">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"Top Up">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL)).
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

-spec handle_topup(kz_json:object(), kz_proplist()) -> 'ok'.
handle_topup(JObj, _Props) ->
    'true' = kapi_notifications:topup_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(DataJObj)
    end.

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    Macros = build_macro_data(
               kz_json:set_value(<<"account_params">>
                                 ,kz_json:from_list(teletype_util:account_params(DataJObj))
                                 ,DataJObj
                                )
              ),

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

-spec get_balance(kz_json:object()) -> ne_binary().
get_balance(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    Amount = wht_util:current_account_dollars(AccountId),
    wht_util:pretty_print_dollars(Amount).

-spec get_topup_amount(kz_json:object()) -> ne_binary().
get_topup_amount(DataJObj) ->
    IsPreview = teletype_util:is_preview(DataJObj),
    case kz_json:get_integer_value(<<"amount">>, DataJObj) of
        'undefined' when IsPreview -> 0;
        'undefined' ->
            lager:warning("failed to get topup amount from data: ~p", [DataJObj]),
            throw({'error', 'no_topup_amount'});
        Amount ->
            wht_util:pretty_print_dollars(
              wht_util:units_to_dollars(Amount)
             )
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
maybe_add_macro_key(<<"account.", AccountKey/binary>>, Acc, DataJObj) ->
    maybe_add_account_data(AccountKey, Acc, DataJObj);
maybe_add_macro_key(<<"balance">> = Key, Acc, DataJObj) ->
    props:set_value(Key, get_balance(DataJObj), Acc);
maybe_add_macro_key(<<"amount">> = Key, Acc, DataJObj) ->
    props:set_value(Key, get_topup_amount(DataJObj), Acc);
maybe_add_macro_key(<<"success">> = Key, Acc, DataJObj) ->
    props:set_value(Key, kz_json:is_true(<<"success">>, DataJObj), Acc);
maybe_add_macro_key(<<"response">> = Key, Acc, DataJObj) ->
    props:set_value(Key
                    ,kz_json:get_value(<<"response">>, DataJObj, <<>>)
                    ,Acc
                   );
maybe_add_macro_key(_Key, Acc, _DataJObj) ->
    lager:debug("unprocessed macro key ~s: ~p", [_Key, _DataJObj]),
    Acc.

-spec maybe_add_account_data(ne_binary(), kz_proplist(), kz_json:object()) ->
                                    kz_proplist().
-spec maybe_add_account_data(ne_binary(), kz_proplist(), kz_json:object(), maybe(binary())) ->
                                    kz_proplist().
maybe_add_account_data(Key, Acc, DataJObj) ->
    maybe_add_account_data(Key, Acc, DataJObj
                           ,kz_json:get_value([<<"account_params">>, Key], DataJObj)
                          ).
maybe_add_account_data(_Key, Acc, _DataJObj, 'undefined') ->
    lager:debug("failed to find account param ~s", [_Key]),
    Acc;
maybe_add_account_data(Key, Acc, _DataJObj, Value) ->
    AccountData = props:get_value(<<"account">>, Acc, []),

    props:set_value(<<"account">>
                    ,props:set_value(Key, Value, AccountData)
                    ,Acc
                   ).

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
    UserId = kz_json:get_value(<<"user_id">>, DataJObj),

    case teletype_util:open_doc(<<"user">>, UserId, DataJObj) of
        {'ok', UserJObj} -> UserJObj;
        {'error', _E} ->
            lager:debug("failed to find user ~s in ~s: ~p", [UserId, AccountId, _E]),
            kz_json:new()
    end.
