%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_bill_reminder).
-behaviour(teletype_gen_email_template).

-export([id/0
        ,init/0
        ,macros/0, macros/1
        ,subject/0
        ,category/0
        ,friendly_name/0
        ,to/0, from/0, cc/0, bcc/0, reply_to/0
        ]).
-export([handle_req/1]).

-include("teletype.hrl").

-spec id() -> kz_term:ne_binary().
id() -> <<"bill_reminder">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"invoice.method">>, <<"invoice_method">>, <<"Payment Method">>, <<"Default payment method">>)
      ,?MACRO_VALUE(<<"invoice.card_type">>, <<"invoice_card_type">>, <<"Credit Card Type">>, <<"Default payment method card type">>)
      ,?MACRO_VALUE(<<"invoice.last_four">>, <<"invoice_last_four">>, <<"Credit Card Last Four Digits">>, <<"Credit Card Last Four Digits">>)
      ,?MACRO_VALUE(<<"invoice.due_date.local">>, <<"invoice_due_date_local">>, <<"Due Date (Local)">>, <<"Invoice due date (Local)">>)
      ,?MACRO_VALUE(<<"invoice.due_date.timezone">>, <<"invoice_due_date_timezone">>, <<"Due Date Local Timezone">>, <<"Invoice due date (Local Timezone)">>)
      ,?MACRO_VALUE(<<"invoice.due_date.utc">>, <<"invoice_due_date_utc">>, <<"Due Date (Local)">>, <<"Invoice due date (UTC)">>)
      ,?MACRO_VALUE(<<"invoice.due_date.timestamp">>, <<"invoice_due_date_timestamp">>, <<"Due Date Timestamp (Gregorian)">>, <<"Due Date Timestamp (Gregorian)">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].category">>, <<"invoice_item_category">>, <<"Invoice Item Category">>, <<"Category name that the item belongs to">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].discounts">>, <<"invoice_item_discounts">>, <<"Invoice Item Discounts">>, <<"Item's discounts">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].item">>, <<"invoice_item_item">>, <<"Invoice Item">>, <<"Item in the category">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].quantity">>, <<"invoice_item_quantity">>, <<"Invoice Item Quantity">>, <<"Item's quantity">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].rate">>, <<"invoice_item_rate">>, <<"Invoice Item Rate">>, <<"Item's rate">>)
      ,?MACRO_VALUE(<<"invoice.items.[item_name].total">>, <<"invoice_item_total">>, <<"Invoice Item Total">>, <<"Item's total price">>)
      ,?MACRO_VALUE(<<"invoice.total_discounts">>, <<"invoice_total_discounts">>, <<"Invoice Total Discounts">>, <<"Invoice total discounts">>)
      ,?MACRO_VALUE(<<"invoice.total">>, <<"invoice_total">>, <<"Invoice Total">>, <<"Invoice total price">>)
      ,?MACRO_VALUE(<<"reseller.id">>, <<"reseller_id">>, <<"Reseller Account ID">>, <<"Reseller Account ID">>)
      ,?MACRO_VALUE(<<"reseller.name">>, <<"reseller_name">>, <<"Reseller Account Name">>, <<"Reseller Account Name">>)
      ,?MACRO_VALUE(<<"reseller.realm">>, <<"reseller_realm">>, <<"Reseller Account Realm">>, <<"Reseller Account Realm">>)
      ,?MACRO_VALUE(<<"reseller.language">>, <<"reseller_language">>, <<"Reseller Account Language">>, <<"Reseller Account Language">>)
      ,?MACRO_VALUE(<<"reseller.timezone">>, <<"reseller_timezone">>, <<"Reseller Account Timezone">>, <<"Reseller Account Timezone">>)
      ,?MACRO_VALUE(<<"timestamp.utc">>, <<"date_called_utc">>, <<"Date (UTC)">>, <<"When was the change happened (UTC)">>)
      ,?MACRO_VALUE(<<"timestamp.local">>, <<"date_called_local">>, <<"Date (Local)">>, <<"When was the changed happened (Local time)">>)
      ,?MACRO_VALUE(<<"timestamp.timezone">>, <<"date_called_timezone">>, <<"Timezone">>, <<"Timezone">>)
      ,?MACRO_VALUE(<<"timestamp.timestamp">>, <<"date_called_timestamp">>, <<"Timestamp">>, <<"Timestamp">>)
       | ?ACCOUNT_MACROS
       ++ ?SYSTEM_MACROS
      ]).

-spec subject() -> kz_term:ne_binary().
subject() -> <<"Payment Reminder for account '{{account.name}}'">>.

-spec category() -> kz_term:ne_binary().
category() -> <<"account">>.

-spec friendly_name() -> kz_term:ne_binary().
friendly_name() -> <<"Payment Reminder">>.

-spec to() -> kz_json:object().
to() -> ?CONFIGURED_EMAILS(?EMAIL_ADMINS).

-spec from() -> kz_term:api_ne_binary().
from() -> teletype_util:default_from_address().

-spec cc() -> kz_json:object().
cc() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec bcc() -> kz_json:object().
bcc() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec reply_to() -> kz_term:api_ne_binary().
reply_to() -> teletype_util:default_reply_to().

-spec init() -> 'ok'.
init() ->
    kz_log:put_callid(?MODULE),
    teletype_templates:init(?MODULE),
    teletype_bindings:bind(id(), ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:bill_reminder_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [id()]),
    teletype_util:notification_failed(id(), <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [id()]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    maybe_handle_req(DataJObj, teletype_util:is_notice_enabled(AccountId, JObj, id())).

-spec maybe_handle_req(kz_json:object(), boolean()) -> template_response().
maybe_handle_req(DataJObj, 'true') ->
    process_req(DataJObj);
maybe_handle_req(DataJObj, 'false') ->
    teletype_util:notification_disabled(DataJObj, id()).

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    Macros = macros(DataJObj),

    %% Load templates
    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    AccountId = kapi_notifications:account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj]),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, id()),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(id());
        {'error', Reason} -> teletype_util:notification_failed(id(), Reason)
    end.

-spec macros(kz_json:object()) -> kz_term:proplist().
macros(DataJObj) ->
    macros(DataJObj, teletype_util:is_preview(DataJObj)).

-spec macros(kz_json:object(), boolean()) -> kz_term:proplist().
macros(DataJObj, 'true') ->
    {'ok', AccountJObj} = teletype_util:read_preview_doc(<<"account">>),
    Now = kz_time:now_s(),
    Account = [{<<"name">>, kzd_accounts:name(AccountJObj)}
              ,{<<"realm">>, kzd_accounts:realm(AccountJObj)}
              ,{<<"id">>, kz_doc:id(AccountJObj)}
              ,{<<"language">>, kzd_accounts:language(AccountJObj)}
              ,{<<"timezone">>, kzd_accounts:timezone(AccountJObj)}
              ],
    [{<<"account">>, Account}
    ,{<<"invoice">>, kz_json:recursive_to_proplist(fake_invoice(Now))}
    ,{<<"reseller">>, Account}
    ,{<<"system">>, teletype_util:system_params()}
    ,{<<"timestamp">>, teletype_util:fix_timestamp(Now, DataJObj)}
    ];
macros(DataJObj, 'false') ->
    Timestamp = timestamp(DataJObj),
    Invoice = invoice_data(DataJObj),
    Reseller = reseller_info_data(DataJObj),
    Affected = affected_account_data(DataJObj),
    [{<<"account">>, Affected}
    ,{<<"invoice">>, Invoice}
    ,{<<"reseller">>, Reseller}
    ,{<<"system">>, teletype_util:system_params()}
    ,{<<"timestamp">>, Timestamp}
    ].

-spec fake_invoice(integer()) -> kz_json:object().
fake_invoice(Now) ->
    {'ok', Invoice} = teletype_util:read_preview_doc(<<"service_invoice">>),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Now),
    DueDate = calendar:datetime_to_gregorian_seconds({kz_date:normalize({Year, Month + 1, 1}), {1, 0, 0}}),

    Token = kz_json:from_list(
              [{<<"card_type">>, <<"Visa">>}
              ,{<<"last_four">>, <<"1111">>}
              ,{<<"method">>, <<"Credit Card">>}
              ]
             ),

    kz_json:set_values(
      [{<<"due_date">>, DueDate}
      ,{<<"payment_method">>, Token}
      ], Invoice
     ).

-spec timestamp(kz_json:object()) -> kz_term:proplist().
timestamp(DataJObj) ->
    TS = kz_json:get_integer_value(<<"timestamp">>, DataJObj),
    teletype_util:fix_timestamp(TS, DataJObj).

-spec reseller_info_data(kz_json:object()) -> kz_term:proplist().
reseller_info_data(DataJObj) ->
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, DataJObj),
    ResellerId = kz_services_reseller:get_id(AccountId),
    teletype_util:find_account_params(ResellerId).

-spec affected_account_data(kz_json:object()) -> kz_term:proplist().
affected_account_data(DataJObj) ->
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, DataJObj),
    teletype_util:find_account_params(AccountId).

-spec invoice_data(kz_json:object()) -> kz_term:proplist().
invoice_data(DataJObj) ->
    case kz_json:get_list_value(<<"items">>, DataJObj, []) of
        [] -> [];
        Items0 ->
            {TotalDiscounts, Total, Items} = invoice_items_fold(Items0),
            props:filter_undefined(
              [{<<"due_date">>
               ,teletype_util:fix_timestamp(kz_json:get_integer_value(<<"due_date">>, DataJObj, kz_time:now_s()))
               }
              ,{<<"items">>, Items}
              ,{<<"payment_method">>
               ,get_payment_token(kz_json:values(kz_json:get_json_value(<<"payment_token">>, DataJObj, kz_json:new())))
               }
              ,{<<"total_discounts">>, format_price(TotalDiscounts)}
              ,{<<"total">>, format_price(Total)}
              ]
             )
    end.

get_payment_token([]) ->
    'undefined';
get_payment_token([Token|_]) ->
    case kz_json:get_ne_binary_value(<<"bookkeeper">>, Token) of
        'undefined' -> 'undefined';
        <<"braintree">> -> get_payment_token(<<"Credit Card">>, Token);
        <<"iou">> -> get_payment_token(<<"Credit Card">>, Token);
        Bookkeeper -> get_payment_token(Bookkeeper, Token)
    end.

get_payment_token(Bookkeeper, Token) ->
    props:filter_undefined(
      [{<<"method">>, kz_binary:ucfirst(Bookkeeper)}
      ,{<<"card_type">>, kz_json:get_ne_binary_value([<<"metadata">>, <<"card_type">>], Token)}
      ,{<<"last_four">>, kz_json:get_ne_binary_value([<<"metadata">>, <<"last_four">>], Token)}
      ]
     ).

-spec invoice_items_fold(kz_json:objects()) -> {float(), float(), kz_term:proplist()}.
invoice_items_fold(Items0) ->
    {TotalDiscounts, Total, Items1} = lists:foldl(fun invoice_items_fold/2 , {0.0, 0.0, []}, Items0),
    {TotalDiscounts, Total, lists:keysort(1, Items1)}.

-spec invoice_items_fold(kz_json:object(), {float(), float(), kz_term:proplist()}) -> {float(), float(), kz_term:proplist()}.
invoice_items_fold(ItemJObj, {TotalDiscounts, TotalAcc, CategoryAcc}) ->
    Discounts = kz_json:get_float_value([<<"discounts">>, <<"total">>], ItemJObj, 0.0),
    Item = all_name(kz_json:get_ne_binary_value(<<"item">>, ItemJObj)),
    Category = all_name(kz_json:get_ne_binary_value(<<"category">>, ItemJObj)),
    Quantity = kz_json:get_integer_value(<<"quantity">>, ItemJObj, 0),
    Name = kz_binary:ucfirst(kz_json:get_ne_binary_value(<<"name">>, ItemJObj, <<Category/binary, "/", Item/binary>>)),
    Total = kz_json:get_float_value(<<"total">>, ItemJObj),
    ItemProps = [{<<"category">>, Category}
                ,{<<"discounts">>, format_price(Discounts)}
                ,{<<"item">>, Item}
                ,{<<"quantity">>, Quantity}
                ,{<<"rate">>, format_price(kz_json:get_float_value(<<"rate">>, ItemJObj))}
                ,{<<"total">>, format_price(Total)}
                ],
    case Quantity =:= 0 of
        'true' -> {TotalDiscounts, TotalAcc, CategoryAcc};
        'false' ->
            {TotalDiscounts + Discounts, Total + TotalAcc, [{Name, props:filter_undefined(ItemProps)} | CategoryAcc]}
    end.

-spec all_name(kz_term:api_ne_binary()) -> kz_term:ne_binary().
all_name('undefined') ->
    %% it shouldn't be here
    <<"All">>;
all_name(<<"_all">>) ->
    <<"All">>;
all_name(<<"All/All">>) ->
    <<"All">>;
all_name(Name) ->
    kz_binary:ucfirst(Name).

-spec format_price(number()) -> kz_term:ne_binary().
format_price(Price) when is_integer(Price) ->
    kz_term:to_binary(io_lib:format("~b", [Price]));
format_price(Price) when is_float(Price) ->
    kz_term:to_binary(io_lib:format("~.2f", [Price])).
