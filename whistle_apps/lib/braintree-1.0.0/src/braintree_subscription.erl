%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_subscription).

-export([url/0, url/1, url/2]).
-export([new/2, new/3]).
-export([get_id/1]).
-export([get_addon/2]).
-export([reset/1]).
-export([reset_discounts/1]).
-export([reset_addons/1]).
-export([get_addon_quantity/2]).
-export([update_addon_amount/3]).
-export([get_discount/2]).
-export([update_discount_amount/3]).
-export([find/1]).
-export([create/1, create/2]).
-export([update/1]).
-export([cancel/1]).
-export([xml_to_record/1, xml_to_record/2]).
-export([record_to_xml/1]).
-export([update_addon_quantity/3]).
-export([increment_addon_quantity/2]).
-export([update_discount_quantity/3]).
-export([increment_discount_quantity/2]).

-import(braintree_util, [make_doc_xml/2]).
-import(wh_util, [get_xml_value/2]).

-include_lib("braintree/include/braintree.hrl").

-type changes() :: [{atom(), proplist(), [proplist(),...] | []}] | [].
-type subscription() :: #bt_subscription{}.
-type subscriptions() :: [subscription(),...] | [].

-export_type([subscription/0
              ,subscriptions/0
             ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create the partial url for this module
%% @end
%%--------------------------------------------------------------------
-spec url/0 :: () -> string().
-spec url/1 :: (ne_binary()) -> string().
-spec url/2 :: (ne_binary(), ne_binary()) -> string().

url() ->
    "/subscriptions/".

url(SubscriptionId) ->
    lists:append(["/subscriptions/", wh_util:to_list(SubscriptionId)]).

url(SubscriptionId, Options) ->
    lists:append(["/subscriptions/"
                  ,wh_util:to_list(SubscriptionId)
                  ,"/"
                  ,wh_util:to_list(Options)
                 ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new subscription record
%% @end
%%--------------------------------------------------------------------
-spec new/2 :: (ne_binary(), ne_binary()) -> subscription().
-spec new/3 :: (ne_binary(), ne_binary(), ne_binary()) -> subscription().

new(PlanId, PaymentToken) ->
    new(wh_util:rand_hex_binary(16), PlanId, PaymentToken).

new(SubscriptionId, PlanId, PaymentToken) ->
    #bt_subscription{id=SubscriptionId
                     ,payment_token=PaymentToken
                     ,plan_id=PlanId
                     ,create=true
                    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the subscription id
%% @end
%%--------------------------------------------------------------------
-spec get_id/1 :: (subscription()) -> 'undefined' | ne_binary().
get_id(#bt_subscription{id=SubscriptionId}) ->
    SubscriptionId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the subscription id
%% @end
%%--------------------------------------------------------------------
-spec get_addon/2 :: (subscription(), ne_binary()) -> #bt_addon{}.
get_addon(#bt_subscription{add_ons=AddOns}, AddOnId) ->
    case lists:keyfind(AddOnId, #bt_addon.id, AddOns) of
        false ->
            case lists:keyfind(AddOnId, #bt_addon.inherited_from, AddOns) of
                false -> braintree_util:error_not_found(<<"Add-On">>);
                #bt_addon{}=AddOn -> AddOn
            end;
        #bt_addon{}=AddOn -> AddOn
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the quantity of addons
%% @end
%%--------------------------------------------------------------------
-spec get_addon_quantity/2 :: (subscription(), ne_binary()) -> integer().
get_addon_quantity(#bt_subscription{add_ons=AddOns}, AddOnId) ->
    case lists:keyfind(AddOnId, #bt_addon.id, AddOns) of
        false ->
            case lists:keyfind(AddOnId, #bt_addon.inherited_from, AddOns) of
                false -> 0;
                #bt_addon{}=AddOn -> braintree_addon:get_quantity(AddOn)
            end;
        #bt_addon{}=AddOn -> braintree_addon:get_quantity(AddOn)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the subscription id
%% @end
%%--------------------------------------------------------------------
-spec update_addon_amount/3 :: (subscription(), ne_binary(), ne_binary()) -> subscription().
update_addon_amount(#bt_subscription{add_ons=AddOns}=Subscription, AddOnId, Amount) ->
    case lists:keyfind(AddOnId, #bt_addon.id, AddOns) of
        false ->
            case lists:keyfind(AddOnId, #bt_addon.inherited_from, AddOns) of
                false -> braintree_util:error_not_found(<<"Add-On">>);
                #bt_addon{}=AddOn -> 
                    AddOn1 = AddOn#bt_addon{amount=Amount},
                    Subscription#bt_subscription{add_ons=lists:keyreplace(AddOnId, #bt_addon.inherited_from, AddOns, AddOn1)}
            end;
        #bt_addon{}=AddOn -> 
            AddOn1 = AddOn#bt_addon{existing_id=AddOnId, amount=Amount},
            Subscription#bt_subscription{add_ons=lists:keyreplace(AddOnId, #bt_addon.id, AddOns, AddOn1)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a specific discount
%% @end
%%--------------------------------------------------------------------
-spec get_discount/2 :: (subscription(), ne_binary()) -> #bt_discount{}.
get_discount(#bt_subscription{discounts=Discounts}, DiscountId) ->
    case lists:keyfind(DiscountId, #bt_discount.id, Discounts) of
        false ->
            case lists:keyfind(DiscountId, #bt_discount.inherited_from, Discounts) of
                false -> braintree_util:error_not_found(<<"Discount">>);
                #bt_discount{}=Discount -> Discount
            end;
        #bt_discount{}=Discount -> Discount
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the amount of a specific discount
%% @end
%%--------------------------------------------------------------------
-spec update_discount_amount/3 :: (subscription(), ne_binary(), ne_binary()) -> subscription().
update_discount_amount(#bt_subscription{discounts=Discounts}=Subscription, DiscountId, Amount) ->
    case lists:keyfind(DiscountId, #bt_discount.id, Discounts) of
        false ->
            case lists:keyfind(DiscountId, #bt_discount.inherited_from, Discounts) of
                false -> braintree_util:error_not_found(<<"Discount">>);
                #bt_discount{}=Discount -> 
                    Discount1 = Discount#bt_discount{amount=Amount},
                    Subscription#bt_subscription{discounts=lists:keyreplace(DiscountId, #bt_discount.inherited_from, Discounts, Discount1)}
            end;
        #bt_discount{}=Discount -> 
            Discount1 = Discount#bt_discount{existing_id=DiscountId, amount=Amount},
            Subscription#bt_subscription{discounts=lists:keyreplace(DiscountId, #bt_discount.id, Discounts, Discount1)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a subscription by id
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (ne_binary()) -> subscription().
find(SubscriptionId) ->
    Url = url(SubscriptionId),
    Xml = braintree_request:get(Url),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new subscription using the given record
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (subscription()) -> subscription().
-spec create/2 :: (ne_binary(), ne_binary()) -> subscription().

create(#bt_subscription{id=undefined}=Subscription) ->
    create(Subscription#bt_subscription{id=wh_util:rand_hex_binary(16)});
create(#bt_subscription{}=Subscription) ->
    Url = url(),
    Request = record_to_xml(Subscription, true),
    Xml = braintree_request:post(Url, Request),
    xml_to_record(Xml).

create(Plan, Token) ->
    create(#bt_subscription{payment_token=Token, plan_id=Plan}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a subscription with the given record
%% @end
%%--------------------------------------------------------------------
-spec update/1 :: (subscription()) -> subscription().
update(#bt_subscription{create=true}=Subscription) ->
    create(Subscription);
update(#bt_subscription{id=SubscriptionId}=Subscription) ->
    Url = url(SubscriptionId),
    Request = record_to_xml(Subscription, true),
    Xml = braintree_request:put(Url, Request),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a subscription id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec cancel/1 :: (subscription() | ne_binary()) -> subscription().
cancel(#bt_subscription{id=SubscriptionId}) ->
    cancel(SubscriptionId);
cancel(SubscriptionId) ->
    Url = url(SubscriptionId, <<"cancel">>),
    _ = braintree_request:put(Url, <<>>),
    #bt_subscription{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reset/1 :: (subscription()) -> subscription().
reset(Subscription) ->
    lists:foldl(fun(F, S) -> F(S) end, Subscription, [fun reset_addons/1
                                                      ,fun reset_discounts/1
                                                     ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_addons/1 :: (subscription()) -> subscription().
reset_addons(#bt_subscription{add_ons=AddOns}=Subscription) ->
    Subscription#bt_subscription{add_ons=[AddOn#bt_addon{quantity=0} || AddOn <- AddOns]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_discounts/1 :: (subscription()) -> subscription().
reset_discounts(#bt_subscription{discounts=Discounts}=Subscription) ->
    Subscription#bt_subscription{discounts=[Discount#bt_discount{quantity=0} || Discount <- Discounts]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Really ugly function to update an addon for a given subscription
%% or subscription id
%% @end
%%--------------------------------------------------------------------
-spec update_addon_quantity/3 :: (subscription() | ne_binary(), ne_binary(), integer()) -> subscription().
update_addon_quantity(Subscription, AddOnId, Quantity) when not is_integer(Quantity) ->
    update_addon_quantity(Subscription, AddOnId, wh_util:to_integer(Quantity));
update_addon_quantity(#bt_subscription{add_ons=AddOns}=Subscription, AddOnId, Quantity) ->
    case lists:keyfind(AddOnId, #bt_addon.id, AddOns) of
        false ->
            case lists:keyfind(AddOnId, #bt_addon.inherited_from, AddOns) of
                false ->
                    AddOn = #bt_addon{inherited_from=AddOnId, quantity=Quantity},
                    Subscription#bt_subscription{add_ons=[AddOn|AddOns]};
                #bt_addon{}=AddOn ->
                    AddOn1 = AddOn#bt_addon{quantity=Quantity},
                    Subscription#bt_subscription{add_ons=lists:keyreplace(AddOnId, #bt_addon.inherited_from, AddOns, AddOn1)}
            end;
        #bt_addon{}=AddOn ->
            AddOn1 = AddOn#bt_addon{existing_id=AddOnId, quantity=Quantity},
            Subscription#bt_subscription{add_ons=lists:keyreplace(AddOnId, #bt_addon.id, AddOns, AddOn1)}
    end;
update_addon_quantity(SubscriptionId, AddOnId, Quantity) ->
    Subscription = find(SubscriptionId),
    update_addon_quantity(Subscription, AddOnId, Quantity).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Really ugly function to increment an addon for a given subscription
%% or subscription id
%% @end
%%--------------------------------------------------------------------
-spec increment_addon_quantity/2 :: (subscription() | ne_binary(), ne_binary()) -> subscription().
increment_addon_quantity(#bt_subscription{add_ons=AddOns}=Subscription, AddOnId) ->
    case lists:keyfind(AddOnId, #bt_addon.id, AddOns) of
        false ->
            case lists:keyfind(AddOnId, #bt_addon.inherited_from, AddOns) of
                false ->
                    AddOn = #bt_addon{inherited_from=AddOnId, quantity=1},
                    Subscription#bt_subscription{add_ons=[AddOn|AddOns]};
                #bt_addon{quantity=Quantity}=AddOn ->
                    AddOn1 = AddOn#bt_addon{quantity=wh_util:to_integer(Quantity) + 1},
                    Subscription#bt_subscription{add_ons=lists:keyreplace(AddOnId, #bt_addon.inherited_from, AddOns, AddOn1)}
            end;
        #bt_addon{quantity=Quantity}=AddOn ->
            AddOn1 = AddOn#bt_addon{existing_id=AddOnId, quantity=wh_util:to_integer(Quantity) + 1},
            Subscription#bt_subscription{add_ons=lists:keyreplace(AddOnId, #bt_addon.id, AddOns, AddOn1)}
    end;
increment_addon_quantity(SubscriptionId, AddOnId) ->
    Subscription = find(SubscriptionId),
    increment_addon_quantity(Subscription, AddOnId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Really ugly function to update an discount for a given subscription
%% or subscription id
%% @end
%%--------------------------------------------------------------------
-spec update_discount_quantity/3 :: (subscription() | ne_binary(), ne_binary(), integer()) -> subscription().
update_discount_quantity(Subscription, DiscountId, Quantity) when not is_integer(Quantity) ->
    update_discount_quantity(Subscription, DiscountId, wh_util:to_integer(Quantity));
update_discount_quantity(#bt_subscription{discounts=Discounts}=Subscription, DiscountId, Quantity) ->
    case lists:keyfind(DiscountId, #bt_discount.id, Discounts) of
        false ->
            case lists:keyfind(DiscountId, #bt_discount.inherited_from, Discounts) of
                false ->
                    Discount = #bt_discount{inherited_from=DiscountId, quantity=Quantity},
                    Subscription#bt_subscription{discounts=[Discount|Discounts]};
                #bt_discount{}=Discount ->
                    Discount1 = Discount#bt_discount{quantity=Quantity},
                    Subscription#bt_subscription{discounts=lists:keyreplace(DiscountId, #bt_discount.inherited_from, Discounts, Discount1)}
            end;
        #bt_discount{}=Discount ->
            Discount1 = Discount#bt_discount{existing_id=DiscountId, quantity=Quantity},
            Subscription#bt_subscription{discounts=lists:keyreplace(DiscountId, #bt_discount.id, Discounts, Discount1)}
    end;
update_discount_quantity(SubscriptionId, DiscountId, Quantity) ->
    Subscription = find(SubscriptionId),
    update_discount_quantity(Subscription, DiscountId, Quantity).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Really ugly function to increment an discount for a given subscription
%% or subscription id
%% @end
%%--------------------------------------------------------------------
-spec increment_discount_quantity/2 :: (subscription() | ne_binary(), ne_binary()) -> subscription().
increment_discount_quantity(#bt_subscription{discounts=Discounts}=Subscription, DiscountId) ->
    case lists:keyfind(DiscountId, #bt_discount.id, Discounts) of
        false ->
            case lists:keyfind(DiscountId, #bt_discount.inherited_from, Discounts) of
                false ->
                    Discount = #bt_discount{inherited_from=DiscountId, quantity=1},
                    Subscription#bt_subscription{discounts=[Discount|Discounts]};
                #bt_discount{quantity=Quantity}=Discount ->
                    Discount1 = Discount#bt_discount{quantity=wh_util:to_integer(Quantity) + 1},
                    Subscription#bt_subscription{discounts=lists:keyreplace(DiscountId, #bt_discount.inherited_from, Discounts, Discount1)}
            end;
        #bt_discount{quantity=Quantity}=Discount ->
            Discount1 = Discount#bt_discount{existing_id=DiscountId, quantity=wh_util:to_integer(Quantity) + 1},
            Subscription#bt_subscription{discounts=lists:keyreplace(DiscountId, #bt_discount.id, Discounts, Discount1)}
    end;
increment_discount_quantity(SubscriptionId, DiscountId) ->
    Subscription = find(SubscriptionId),
    increment_discount_quantity(Subscription, DiscountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a subscription record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (bt_xml()) -> subscription().
-spec xml_to_record/2 :: (bt_xml(), wh_deeplist()) -> subscription().

xml_to_record(Xml) ->
    xml_to_record(Xml, "/subscription").

xml_to_record(Xml, Base) ->
    AddOnsPath = lists:flatten([Base, "/add-ons/add-on"]),
    DiscountsPath = lists:flatten([Base, "/discounts/discount"]),
    #bt_subscription{id = get_xml_value([Base, "/id/text()"], Xml)
                     ,balance = get_xml_value([Base, "/balance/text()"], Xml)
                     ,billing_dom = get_xml_value([Base, "/billing-day-of-month/text()"], Xml)
                     ,billing_first_date = get_xml_value([Base, "/first-billing-date/text()"], Xml)
                     ,billing_end_date = get_xml_value([Base, "/billing-period-end-date/text()"], Xml)
                     ,billing_start_date = get_xml_value([Base, "/billing-period-start-date/text()"], Xml)
                     ,billing_cycle = get_xml_value([Base, "/current-billing-cycle/text()"], Xml)
                     ,number_of_cycles = get_xml_value([Base, "/number-of-billing-cycles/text()"], Xml)
                     ,days_past_due = get_xml_value([Base, "/days-past-due/text()"], Xml)
                     ,failure_count = get_xml_value([Base, "/failure-count/text()"], Xml)
                     ,merchant_account_id = get_xml_value([Base, "/merchant-account-id/text()"], Xml)
                     ,never_expires = wh_util:is_true(get_xml_value([Base, "/never-expires/text()"], Xml))
                     ,next_bill_amount = get_xml_value([Base, "/next-bill-amount/text()"], Xml)
                     ,next_cycle_amount = get_xml_value([Base, "/next-billing-period-amount/text()"], Xml)
                     ,next_bill_date = get_xml_value([Base, "/next-billing-date/text()"], Xml)
                     ,paid_through_date = get_xml_value([Base, "/paid-through-date/text()"], Xml)
                     ,payment_token = get_xml_value([Base, "/payment-method-token/text()"], Xml)
                     ,plan_id = get_xml_value([Base, "/plan-id/text()"], Xml)
                     ,price = get_xml_value([Base, "/price/text()"], Xml)
                     ,status = get_xml_value([Base, "/status/text()"], Xml)
                     ,trial_duration = get_xml_value([Base, "/trial-duration/text()"], Xml)
                     ,trial_duration_unit = get_xml_value([Base, "/trial-duration-unit/text()"], Xml)
                     ,trial_period = get_xml_value([Base, "/trial-period/text()"], Xml)
                     ,add_ons = [braintree_addon:xml_to_record(Addon)
                                 || Addon <- xmerl_xpath:string(AddOnsPath, Xml)
                                ]
                     ,discounts = [braintree_discount:xml_to_record(Discount)
                                   || Discount <- xmerl_xpath:string(DiscountsPath, Xml)
                                  ]
%%                     ,descriptor = get_xml_value([Base, "/token/text()"], Xml)
%%                     ,transactions = [braintree_transaction:xml_to_record(Trans)
%%                                      || Trans <- xmerl_xpath:string([Base, "/transactions/transaction"], Xml)
%%                                     ]
                     }.
   

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a subscription record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (subscription()) -> proplist() | bt_xml().
-spec record_to_xml/2 :: (subscription(), boolean()) -> proplist() | bt_xml().

record_to_xml(Subscription) ->
    record_to_xml(Subscription, false).

record_to_xml(#bt_subscription{}=Subscription, ToString) ->
    Props = [{'id', Subscription#bt_subscription.id}
             ,{'merchant-account-id', Subscription#bt_subscription.merchant_account_id}
             ,{'never-expires', Subscription#bt_subscription.never_expires}
             ,{'number-of-billing-cycles', Subscription#bt_subscription.number_of_cycles}
             ,{'payment-method-token', Subscription#bt_subscription.payment_token}
             ,{'plan-id', Subscription#bt_subscription.plan_id}
             ,{'price', Subscription#bt_subscription.price}
             ,{'trial-duration', Subscription#bt_subscription.trial_duration}
             ,{'trial-duration-unit', Subscription#bt_subscription.trial_duration_unit}
             ,{'trial-period', Subscription#bt_subscription.trial_period}
             ,{'add-ons', create_addon_changes(Subscription#bt_subscription.add_ons)}
             ,{'discounts', create_discount_changes(Subscription#bt_subscription.discounts)}
            ],
    Conditionals = [fun(#bt_subscription{do_not_inherit=Value}, P) ->
                            update_options('do-not-inherit-add-ons-or-discounts', Value, P)
                    end
                    ,fun(#bt_subscription{prorate_charges=Value}, P) ->
                            update_options('prorate-charges', Value, P)
                     end
                    ,fun(#bt_subscription{revert_on_prorate_fail=Value}, P) ->
                            update_options('revert-subscription-on-proration-failure', Value, P)
                     end
                    ,fun(#bt_subscription{replace_add_ons=Value}, P) ->
                             update_options('replace-all-add-ons-and-discounts', Value, P)
                     end
                    ,fun(#bt_subscription{start_immediately=Value}, P) ->
                             update_options('start-immediately', Value, P)
                     end
                   ],
    Props1 = lists:foldr(fun(F, P) -> F(Subscription, P) end, Props, Conditionals),
    case ToString of
        true -> make_doc_xml(Props1, 'subscription');
        false -> Props1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the necessary steps to change the add ons
%% @end
%%--------------------------------------------------------------------
-spec update_options/3 :: (_, _, proplist()) -> proplist().
update_options(Key, Value, Props) ->
    case proplists:get_value('options', Props) of
        undefined ->
            [{'options', [{Key, Value}]}|Props];
        Options ->
            [{'options', [{Key, Value}|Options]}
             |proplists:delete('options', Props)
            ]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the necessary steps to change the add ons
%% @end
%%--------------------------------------------------------------------
-spec create_addon_changes/1 :: ([#bt_addon{},...] | []) -> changes().
create_addon_changes(AddOns) ->
    lists:foldr(fun(#bt_addon{id=Id, quantity=0}, C) ->
                        append_items('remove', Id, C);
                   (#bt_addon{existing_id=undefined
                              ,inherited_from=undefined}, C) ->
                        C;
                   (#bt_addon{existing_id=undefined
                              ,inherited_from=Id
                              ,quantity=Q
                              ,amount=A}, C) ->
                        Item = [{'inherited_from_id', Id}
                                ,{'quantity', Q}
                                ,{'amount', A}
                               ],
                        append_items('add', props:filter_undefined(Item), C);
                   (#bt_addon{existing_id=Id, quantity=Q, amount=A}, C) ->
                        Item = [{'existing_id', Id}
                                ,{'quantity', Q}
                                ,{'amount', A}
                               ],
                        append_items('update', props:filter_undefined(Item), C)
                end, [], AddOns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the necessary steps to change the discounts
%% @end
%%--------------------------------------------------------------------
-spec create_discount_changes/1 :: ([#bt_discount{},...] | []) -> changes().
create_discount_changes(Discounts) ->
    lists:foldr(fun(#bt_discount{id=Id, quantity=0}, C) ->
                        append_items('remove', Id, C);
                   (#bt_discount{existing_id=undefined
                              ,inherited_from=undefined}, C) ->
                        C;
                   (#bt_discount{existing_id=undefined
                              ,inherited_from=Id
                              ,quantity=Q
                              ,amount=A}, C) ->
                        Item = [{'inherited_from_id', Id}
                                ,{'quantity', Q}
                                ,{'amount', A}
                               ],
                        append_items('add', props:filter_undefined(Item), C);
                   (#bt_discount{existing_id=Id, quantity=Q, amount=A}, C) ->
                        Item = [{'existing_id', Id}
                                ,{'quantity', Q}
                                ,{'amount', A}
                               ],
                        append_items('update', props:filter_undefined(Item), C)
                end, [], Discounts).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the necessary steps to change the add ons
%% @end
%%--------------------------------------------------------------------
-spec append_items/3 :: (atom(), ne_binary() | proplist(), changes()) -> changes().    
append_items(Change, Item, Changes) -> 
    case lists:keyfind(Change, 1, Changes) of
        false -> 
            [{Change, [{'type', "array"}], [{'item', Item}]}|Changes];
        {_, Props, Items} ->
            lists:keyreplace(Change, 1, Changes, {Change, Props, [{'item', Item}|Items]})
    end.
