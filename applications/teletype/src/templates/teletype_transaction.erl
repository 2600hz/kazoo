%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_transaction).

-export([init/0
         ,handle_transaction/2
        ]).

-include("teletype.hrl").

-define(TEMPLATE_ID, <<"transaction">>).
-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".", (?NOTIFY_CONFIG_CAT)/binary>>).

-define(TEMPLATE_MACROS
        ,kz_json:from_list(
           [?MACRO_VALUE(<<"plan.id">>, <<"plan_id">>, <<"Plan ID">>, <<"Plan ID">>)
            ,?MACRO_VALUE(<<"plan.category">>, <<"plan_category">>, <<"Plan Category">>, <<"Plan Category">>)
            ,?MACRO_VALUE(<<"plan.item">>, <<"plan_item">>, <<"Plan Item">>, <<"Plan Item">>)
            ,?MACRO_VALUE(<<"plan.activation_charge">>, <<"plan_activation_charge">>, <<"Activation Charge">>, <<"Activiation Charge">>)

            ,?MACRO_VALUE(<<"transaction.id">>, <<"transaction_id">>, <<"Transaction ID">>, <<"Transaction ID">>)
            ,?MACRO_VALUE(<<"transaction.status">>, <<"transaction_status">>, <<"Transaction Status">>, <<"Transaction Status">>)
            ,?MACRO_VALUE(<<"transaction.type">>, <<"transaction_type">>, <<"Transaction Type">>, <<"Transaction Type">>)
            ,?MACRO_VALUE(<<"transaction.currency_code">>, <<"transaction_currency_code">>, <<"Currency Code">>, <<"Currency Code">>)
            ,?MACRO_VALUE(<<"transaction.amount">>, <<"transaction_amount">>, <<"Transaction Amount">>, <<"Transaction Amount">>)
            ,?MACRO_VALUE(<<"transaction.merchant_account_id">>, <<"transaction_merchant_account_id">>, <<"Merchant Account ID">>, <<"Merchant Account ID">>)
            ,?MACRO_VALUE(<<"transaction.order_id">>, <<"transaction_order_id">>, <<"Order ID">>, <<"Order ID">>)
            ,?MACRO_VALUE(<<"transaction.purchase_order">>, <<"transaction_purchase_order">>, <<"Purchase Order">>, <<"Purchase Order">>)
            ,?MACRO_VALUE(<<"transaction.created_at">>, <<"transaction_created_at">>, <<"Transaction Created">>, <<"Transaction Created">>)
            ,?MACRO_VALUE(<<"transaction.update_at">>, <<"transaction_update_at">>, <<"Transaction Updated">>, <<"Transaction Updated">>)
            ,?MACRO_VALUE(<<"transaction.refund_id">>, <<"transaction_refund_id">>, <<"Refund ID">>, <<"Refund ID">>)
            ,?MACRO_VALUE(<<"transaction.refunded_transaction">>, <<"transaction_refunded_transaction">>, <<"Refunded Transaction">>, <<"Refunded Transaction">>)
            ,?MACRO_VALUE(<<"transaction.settlement_batch">>, <<"transaction_settlement_batch">>, <<"Settlement Batch">>, <<"Settlement Batch">>)
            ,?MACRO_VALUE(<<"transaction.avs_error_code">>, <<"transaction_avs_error_code">>, <<"AVS Error Code">>, <<"AVS Error Code">>)
            ,?MACRO_VALUE(<<"transaction.avs_postal_response">>, <<"transaction_avs_postal_response">>, <<"AVS Postal Response">>, <<"AVS Postal Response">>)
            ,?MACRO_VALUE(<<"transaction.avs_street_response">>, <<"transaction_avs_street_response">>, <<"AVS Street Response">>, <<"AVS Street Response">>)
            ,?MACRO_VALUE(<<"transaction.ccv_response_code">>, <<"transaction_ccv_response_code">>, <<"CCV Response Code">>, <<"CCV Response Code">>)
            ,?MACRO_VALUE(<<"transaction.gateway_rejection">>, <<"transaction_gateway_rejection">>, <<"Gateway Rejection">>, <<"Gateway Rejection">>)
            ,?MACRO_VALUE(<<"transaction.processor_authorization_code">>, <<"transaction_processer_authorization_code">>, <<"Processor Authorization Code">>, <<"Processer Authorization Code">>)
            ,?MACRO_VALUE(<<"transaction.processor_response_code">>, <<"transaction_processer_response_code">>, <<"Processor Response Code">>, <<"Processer Response Code">>)
            ,?MACRO_VALUE(<<"transaction.processor_response_text">>, <<"transaction_processer_response_text">>, <<"Processor Response Text">>, <<"Processer Response Test">>)
            ,?MACRO_VALUE(<<"transaction.tax_amount">>, <<"transaction_tax_amount">>, <<"Tax Amount">>, <<"Tax Amount">>)
            ,?MACRO_VALUE(<<"transaction.tax_exempt">>, <<"transaction_tax_exempt">>, <<"Tax Exempt">>, <<"Tax Exempt">>)
            ,?MACRO_VALUE(<<"transaction.billing_address">>, <<"transaction_billing_address">>, <<"Billing Address">>, <<"Billing Address">>)
            ,?MACRO_VALUE(<<"transaction.shipping_address_id">>, <<"transaction_shipping_address_id">>, <<"Shipping Address ID">>, <<"Shipping Address ID">>)
            ,?MACRO_VALUE(<<"transaction.shipping_address">>, <<"transaction_shipping_address">>, <<"Shipping Address">>, <<"Shipping Address">>)
            ,?MACRO_VALUE(<<"transaction.customer_id">>, <<"transaction_customer_id">>, <<"Customer ID">>, <<"Customer ID">>)
            ,?MACRO_VALUE(<<"transaction.customer">>, <<"transaction_customer">>, <<"Customer">>, <<"Customer">>)
            ,?MACRO_VALUE(<<"transaction.payment_token">>, <<"transaction_payment_token">>, <<"Payment Token">>, <<"Payment Token">>)
            ,?MACRO_VALUE(<<"transaction.card">>, <<"transaction_card">>, <<"Card">>, <<"Card">>)
            ,?MACRO_VALUE(<<"transaction.subscription_id">>, <<"transaction_subscription_id">>, <<"Subscription ID">>, <<"Subscription ID">>)
            ,?MACRO_VALUE(<<"transaction.addons">>, <<"transaction_addons">>, <<"Transaction Add Ons">>, <<"Transaction Add Ons">>)
            ,?MACRO_VALUE(<<"transaction.discounts">>, <<"transaction_discounts">>, <<"Transaction Discounts">>, <<"Transaction Discounts">>)
            ,?MACRO_VALUE(<<"transaction.is_api">>, <<"transaction_is_api">>, <<"Is API">>, <<"Is API">>)
            ,?MACRO_VALUE(<<"transaction.is_automatic">>, <<"transaction_is_automatic">>, <<"Is Automatic">>, <<"Is Automatic">>)
            ,?MACRO_VALUE(<<"transaction.is_recurring">>, <<"transaction_is_recurring">>, <<"Is Recurring">>, <<"Is Recurring">>)
            | ?ACCOUNT_MACROS
           ])
       ).

-define(TEMPLATE_TEXT, <<"Transaction notice for {{account.name}} - ${{transaction.amount}} (ID #{{account.id}})\n\n{% if transaction %}Transaction\n{% for key, value in transaction %}{{ key }}: {{ value }}\n{% endfor %}\n{% endif %}{% if plan %}Service Plan\nID: {{plan.id}}\nCategory: {{plan.category}}\nItem: {{plan.item}}\nActivation-Charge: {{plan.activation_charge}}\n\n{% endif %}Account\nAccount ID: {{account.id}}\nAccount Name: {{account.name}}\nAccount Realm: {{account.realm}}\n\nSent from {{system.hostname}}">>).
-define(TEMPLATE_HTML, <<"<html><head><meta charset=\"utf-8\" /></head><body><h1>KAZOO: transaction notice for {{account.name}} - ${{transaction.amount}} (ID #{{account.id}})</h1><br/>{% if transaction %}<h2>Transaction</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\">{% for key, value in transaction %}<tr><td>{{ key }}: </td><td>{{ value }}</td></tr>{% endfor %}</table>{% endif %}{% if plan %}<h2>Service Plan</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>ID: </td><td>{{plan.id}}</td></tr><tr><td>Category: </td><td>{{plan.category}}</td></tr><tr><td>Item: </td><td>{{plan.item}}</td></tr><tr><td>Activation-Charge: </td><td>{{plan.activation_charge}}</td></tr></table>{% endif %}<h2>Account</h2><table cellpadding=\"4\" cellspacing=\"0\" border=\"0\"><tr><td>Account ID: </td><td>{{account.id}}</td></tr><tr><td>Account Name: </td><td>{{account.name}}</td></tr><tr><td>Account Realm: </td><td>{{account.realm}}</td></tr></table><p style=\"font-size:9pt;color:#CCCCCC\">Sent from {{system.hostname}}</p></body></html>">>).
-define(TEMPLATE_SUBJECT, <<"Transaction notice (account ID #{{account.id}})">>).
-define(TEMPLATE_CATEGORY, <<"account">>).
-define(TEMPLATE_NAME, <<"Transaction">>).

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

-spec handle_transaction(kz_json:object(), kz_proplist()) -> 'ok'.
handle_transaction(JObj, _Props) ->
    'true' = kapi_notifications:transaction_v(JObj),
    kz_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, ?TEMPLATE_ID) of
        'false' -> lager:debug("notification handling not configured for this account");
        'true' -> handle_req(DataJObj)
    end.

-spec service_plan_data(kz_json:object()) -> kz_proplist().
service_plan_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' -> teletype_util:public_proplist(<<"service_plan">>, DataJObj)
    end.

-spec transaction_data(kz_json:object()) -> kz_proplist().
transaction_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'true' -> [];
        'false' -> teletype_util:public_proplist(<<"transaction">>, DataJObj)
    end.

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
              ,{<<"account">>, teletype_util:account_params(DataJObj)}
              ,{<<"plan">>, service_plan_data(DataJObj)}
              ,{<<"transaction">>, transaction_data(DataJObj)}
             ],

    %% Load templates
    RenderedTemplates = teletype_templates:render(?TEMPLATE_ID, Macros, DataJObj),

    AccountId = teletype_util:find_account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(?TEMPLATE_ID, AccountId),

    Subject = teletype_util:render_subject(
                kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj])
                ,Macros
               ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, ?MOD_CONFIG_CAT),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.
