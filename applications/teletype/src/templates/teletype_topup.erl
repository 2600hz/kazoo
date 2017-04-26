%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_topup).

-export([init/0
        ,handle_topup/1
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"topup">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"balance">>, <<"balance">>, <<"Balance">>, <<"The Resulting Account Balance">>)
           | ?TRANSACTION_MACROS
           ++ ?ACCOUNT_MACROS
           ++ ?USER_MACROS
          ]
         )
       ).

-define(TEMPLATE_SUBJECT, <<"Account '{{account.name}}' has been attempted to top-up">>).
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
                                          ,{'subject', ?TEMPLATE_SUBJECT}
                                          ,{'category', ?TEMPLATE_CATEGORY}
                                          ,{'friendly_name', ?TEMPLATE_NAME}
                                          ,{'to', ?TEMPLATE_TO}
                                          ,{'from', ?TEMPLATE_FROM}
                                          ,{'cc', ?TEMPLATE_CC}
                                          ,{'bcc', ?TEMPLATE_BCC}
                                          ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                          ]),
    teletype_bindings:bind(<<"topup">>, ?MODULE, 'handle_topup').

-spec handle_topup(kz_json:object()) -> 'ok'.
handle_topup(JObj) ->
    'true' = kapi_notifications:topup_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    ReqData =
        kz_json:set_value(<<"user">>, teletype_util:find_account_admin(AccountId), DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(kz_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    TransactionProps = transaction_data(DataJObj),
    Macros = [{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"system">>, teletype_util:system_params()}
             ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
             ,{<<"transaction">>, TransactionProps}
             ,{<<"balance">>, get_balance(DataJObj)}
             ,{<<"amount">>, props:get_value(<<"amount">>, TransactionProps)} %% backward compatibility
             ,{<<"response">>, props:get_value(<<"response">>, TransactionProps)} %% backward compatibility
             ,{<<"success">>, props:get_value(<<"success">>, TransactionProps)} %% backward compatibility
             ],

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

-spec transaction_data(kz_json:object()) -> kz_proplist().
transaction_data(DataJObj) ->
    transaction_data(DataJObj, teletype_util:is_preview(DataJObj)).

-spec transaction_data(kz_json:object(), boolean()) -> kz_proplist().
transaction_data(DataJObj, 'true') ->
    {'ok', JObj} = teletype_util:read_preview_doc(<<"transaction">>),
    Props = kz_json:recursive_to_proplist(JObj),
    props:set_value(<<"date">>, teletype_util:fix_timestamp(props:get_value(<<"date">>, Props), DataJObj), Props);
transaction_data(DataJObj, 'false') ->
    props:filter_undefined(
      [{<<"amount">>, get_topup_amount(DataJObj)}
      ,{<<"success">>, kz_json:is_true(<<"success">>, DataJObj)}
      ,{<<"response">>, kz_json:get_value(<<"response">>, DataJObj)}
      ,{<<"id">>, kz_json:get_value(<<"id">>, DataJObj)}
      ,{<<"address">>
       ,props:filter_undefined(
          [{<<"first_name">>, kz_json:get_value([<<"billing_address">>, <<"first_name">>], DataJObj)}
          ,{<<"last_name">>, kz_json:get_value([<<"billing_address">>, <<"last_name">>], DataJObj)}
          ,{<<"company">>, kz_json:get_value([<<"billing_address">>, <<"company">>], DataJObj)}
          ,{<<"street_address">>, kz_json:get_value([<<"billing_address">>, <<"street_address">>], DataJObj)}
          ,{<<"extended_address">>, kz_json:get_value([<<"billing_address">>, <<"extended_address">>], DataJObj)}
          ,{<<"locality">>, kz_json:get_value([<<"billing_address">>, <<"locality">>], DataJObj)}
          ,{<<"region">>, kz_json:get_value([<<"billing_address">>, <<"region">>], DataJObj)}
          ,{<<"postal_code">>, kz_json:get_value([<<"billing_address">>, <<"postal_code">>], DataJObj)}
          ,{<<"country_name">>, kz_json:get_value([<<"billing_address">>, <<"country_name">>], DataJObj)}
          ,{<<"phone">>, kz_json:get_value([<<"billing_address">>, <<"phone">>], DataJObj)}
          ,{<<"email">>, kz_json:get_value([<<"billing_address">>, <<"email">>], DataJObj)}
          ])
       }
      ,{<<"card_last_four">>, kz_json:get_value(<<"card_last_four">>, DataJObj)}
      ,{<<"tax_amount">>, kz_json:get_value(<<"tax_amount">>, DataJObj)}
      ,{<<"date">>, teletype_util:fix_timestamp(kz_json:get_value(<<"timestamp">>, DataJObj), DataJObj)}
      ,{<<"purchase_order">>, purchase_order(DataJObj)}
      ,{<<"currency_code">>, kz_json:get_value(<<"currency_code">>, DataJObj)}
      ]
     ).

-spec get_balance(kz_json:object()) -> ne_binary().
get_balance(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    case wht_util:current_account_dollars(AccountId) of
        {'ok', Amount} -> wht_util:pretty_print_dollars(Amount);
        {'error', _} -> <<"\"not known at the moment\"">>
    end.

%% amount is expected to be in dollars
-spec get_topup_amount(kz_json:object()) -> ne_binary().
get_topup_amount(DataJObj) ->
    IsPreview = teletype_util:is_preview(DataJObj),
    case kz_json:get_float_value(<<"amount">>, DataJObj) of
        'undefined' when IsPreview -> 20.0;
        'undefined' ->
            lager:warning("failed to get topup amount from data: ~p", [DataJObj]),
            throw({'error', 'no_topup_amount'});
        Amount -> Amount
    end.

-spec purchase_order(kz_json:object()) -> binary().
purchase_order(DataJObj) ->
    binary:replace(kz_json:get_ne_binary_value(<<"purchase_order">>, DataJObj, <<>>)
                  ,<<"_">>
                  ,<<" ">>
                  ,[global]
                  ).
