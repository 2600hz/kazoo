%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_transaction).

-export([init/0
        ,handle_req/1
        ]).

-include("teletype.hrl").

-define(SUCCESS_TEMPLATE_ID, <<"transaction">>).
-define(FAILED_TEMPLATE_ID, <<"transaction_failed">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?SUCCESS_TEMPLATE_ID)/binary>>).

-define(TEMPLATE_MACROS
       ,kz_json:from_list(
          [?MACRO_VALUE(<<"plan.id">>, <<"plan_id">>, <<"Plan ID">>, <<"Plan ID">>)
          ,?MACRO_VALUE(<<"plan.category">>, <<"plan_category">>, <<"Plan Category">>, <<"Plan Category">>)
          ,?MACRO_VALUE(<<"plan.item">>, <<"plan_item">>, <<"Plan Item">>, <<"Plan Item">>)
          ,?MACRO_VALUE(<<"plan.activation_charge">>, <<"plan_activation_charge">>, <<"Activation Charge">>, <<"Activiation Charge">>)
           | ?TRANSACTION_MACROS
           ++ ?USER_MACROS
           ++ ?COMMON_TEMPLATE_MACROS
          ]
         )
       ).

-define(SUCCESS_TEMPLATE_SUBJECT, <<"Receipt for your payment of account '{{account.name}}'">>).
-define(FAILED_TEMPLATE_SUBJECT, <<"Payment problem for your account '{{account.name}}'">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(SUCCESS_TEMPLATE_NAME, <<"Transaction Success">>).
-define(FAILED_TEMPLATE_NAME, <<"Transaction Problem">>).

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address(?MOD_CONFIG_CAT)).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to(?MOD_CONFIG_CAT)).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    Fields =  [{'macros', ?TEMPLATE_MACROS}
              ,{'category', ?TEMPLATE_CATEGORY}
              ,{'to', ?TEMPLATE_TO}
              ,{'from', ?TEMPLATE_FROM}
              ,{'cc', ?TEMPLATE_CC}
              ,{'bcc', ?TEMPLATE_BCC}
              ,{'reply_to', ?TEMPLATE_REPLY_TO}
              ],
    SucessParams = [{'friendly_name', ?SUCCESS_TEMPLATE_NAME}
                   ,{'subject', ?SUCCESS_TEMPLATE_SUBJECT}
                    | Fields
                   ],
    FailedParams = [{'friendly_name', ?FAILED_TEMPLATE_NAME}
                   ,{'subject', ?FAILED_TEMPLATE_SUBJECT}
                    | Fields
                   ],
    teletype_templates:init(?SUCCESS_TEMPLATE_ID, SucessParams),
    teletype_templates:init(?FAILED_TEMPLATE_ID, FailedParams),
    teletype_bindings:bind(<<"transaction">>, ?MODULE, 'handle_req').

-spec transaction_template_id(kz_json:object()) -> ne_binary().
transaction_template_id(DataJObj) ->
    case kz_json:is_true(<<"success">>, DataJObj) of
        'true' -> ?SUCCESS_TEMPLATE_ID;
        'false' -> ?FAILED_TEMPLATE_ID
    end.

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:transaction_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> 'ok'.
handle_req(JObj, 'false') ->
    lager:debug("invalid data for ~s", [?SUCCESS_TEMPLATE_ID]),
    teletype_util:send_update(JObj, <<"failed">>, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [?SUCCESS_TEMPLATE_ID]),

    'true' = kapi_notifications:transaction_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    ReqData =
        kz_json:set_value(<<"user">>, teletype_util:find_account_admin(AccountId), DataJObj),

    TemplateId = transaction_template_id(DataJObj),
    case teletype_util:is_notice_enabled(AccountId, JObj, TemplateId) of
        'false' -> teletype_util:notification_disabled(DataJObj, TemplateId);
        'true' -> process_req(kz_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    TemplateId = transaction_template_id(DataJObj),
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
             ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
             ,{<<"plan">>, service_plan_data(DataJObj)}
             ,{<<"transaction">>, transaction_data(DataJObj)}
             ],

    %% Load templates
    RenderedTemplates = teletype_templates:render(TemplateId, Macros, DataJObj),

    AccountId = kapi_notifications:account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(TemplateId, AccountId),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                                          ,Macros
               ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec service_plan_data(kz_json:object()) -> kz_proplist().
service_plan_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' ->teletype_util:public_proplist(<<"service_plan">>, DataJObj)
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
      [{<<"amount">>, get_transaction_amount(DataJObj)}
      ,{<<"success">>, kz_json:is_true(<<"success">>, DataJObj)}
      ,{<<"response">>, kz_json:get_value(<<"response">>, DataJObj)}
      ,{<<"id">>, kz_json:get_value(<<"id">>, DataJObj)}
      ,{<<"add_ons">>, calculate_total(kz_json:get_ne_value(<<"add_ons">>, DataJObj, []))}
      ,{<<"discounts">>, calculate_total(kz_json:get_ne_value(<<"discounts">>, DataJObj, []))}
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

%% amount is expected to be in dollars
-spec get_transaction_amount(kz_json:object()) -> float().
get_transaction_amount(DataJObj) ->
    IsPreview = teletype_util:is_preview(DataJObj),
    case kz_json:get_float_value(<<"amount">>, DataJObj) of
        'undefined' when IsPreview -> 20.0;
        'undefined' ->
            lager:warning("failed to get topup amount from data: ~p", [DataJObj]),
            throw({'error', 'missing_data',  <<"no transaction amount">>});
        Amount -> Amount
    end.

-spec purchase_order(kz_json:object()) -> binary().
purchase_order(DataJObj) ->
    binary:replace(kz_json:get_ne_binary_value(<<"purchase_order">>, DataJObj, <<>>)
                  ,<<"_">>
                  ,<<" ">>
                  ,[global]
                  ).

-spec calculate_total(api_objects() | binary()) -> non_neg_integer().
calculate_total(JObjs) when is_list(JObjs) ->
    lists:sum(
      [kz_json:get_integer_value(<<"quantity">>, J, 0) * kz_json:get_integer_value(<<"amount">>, J, 0)
       || J <- JObjs
      ]
     );
calculate_total(_) -> 0.
