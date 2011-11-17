%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_subscription).

-include("braintree.hrl").

-export([create/1, create/2, update/1, cancel/1]).
-export([find/1]).
-export([xml_to_record/1, xml_to_record/2, record_to_xml/1]).
-export([update_addon_quantity/3]).

-import(braintree_util, [get_xml_value/2, make_doc_xml/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new subscription using the given record
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (Subscription) -> bt_result() when
      Subscription :: #bt_subscription{}.
-spec create/2 :: (Plan, Token) -> bt_result() when
      Plan :: string() | binary(),
      Token :: string() | binary().

create(Subscription) ->
    try
        true = validate_id(Subscription#bt_subscription.payment_token),
        Request = record_to_xml(Subscription, true),
        case braintree_request:post("/subscriptions", Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, payment_token_invalid}
    end.

create(Plan, Token) ->
    create(#bt_subscription{payment_token=Token, plan_id=Plan}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a subscription with the given record
%% @end
%%--------------------------------------------------------------------
-spec update/1 :: (Subscription) -> bt_result() when
      Subscription :: #bt_subscription{}.
update(#bt_subscription{id=Id}=Subscription) ->
    try
        true = validate_id(Id),
        Request = record_to_xml(Subscription, true),
        case braintree_request:put("/subscriptions/" ++ wh_util:to_list(Id), Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, subscription_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a subscription id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec cancel/1 :: (Id) -> bt_result() when
      Id :: #bt_subscription{} | binary() | string().
cancel(#bt_subscription{id=Id}) ->
    cancel(Id);
cancel(Id) ->
    try
        true = validate_id(Id),
        case braintree_request:put("/subscriptions/" ++ wh_util:to_list(Id) ++ "/cancel", <<>>) of
            {ok, _} ->
                {ok, #bt_subscription{}};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, subscription_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a subscription by id
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (Id) -> bt_result() when
      Id :: binary() | string().
find(Id) ->
        try
            true = validate_id(Id),
            case braintree_request:get("/subscriptions/" ++ wh_util:to_list(Id)) of
                {ok, Xml} ->
                    {ok, xml_to_record(Xml)};
                {error, _}=E ->
                    E
            end
        catch
            error:{badmatch, _} ->
                {error, token_invalid}
        end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Really ugly function to update a add on for a given subscription
%% or subscription id
%% @end
%%--------------------------------------------------------------------
-spec update_addon_quantity/3 :: (Subscription, AddOnId, Quantity) -> bt_result() when
      Subscription :: #bt_subscription{} | binary() | string(),
      AddOnId :: binary() | string(),
      Quantity :: integer() | binary() | string().
update_addon_quantity(Subscription, AddOnId, Quantity) when not is_list(Quantity) ->
    update_addon_quantity(Subscription, AddOnId, wh_util:to_list(Quantity));
update_addon_quantity(#bt_subscription{add_ons=undefined}=Subscription, AddOnId, Quantity) ->
    update_addon_quantity(Subscription#bt_subscription{add_ons=[]}, AddOnId, Quantity);
update_addon_quantity(#bt_subscription{add_ons=AddOns}=Subscription, AddOnId, Quantity) ->
    case lists:keyfind(AddOnId, #bt_addon.id, AddOns) of
        false ->
            AddOn = #bt_addon{inherited_from=AddOnId, quantity=Quantity},
            {ok, Subscription#bt_subscription{add_ons=[AddOn|AddOns]}};
        AddOn ->
            AddOn1 = AddOn#bt_addon{existing_id=AddOnId, quantity=Quantity},
            {ok, Subscription#bt_subscription{add_ons=[AddOn1|lists:keydelete(AddOnId, #bt_addon.id, AddOns)]}}
    end;
update_addon_quantity(SubscriptionId, AddOnId, Quantity) ->
    case find(SubscriptionId) of
        {ok, Subscription} ->
            update_addon_quantity(Subscription, AddOnId, Quantity);
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verifies that the id being used is valid
%% @end
%%--------------------------------------------------------------------
-spec validate_id/1 :: (Id) -> boolean() when
      Id :: string() | binary().
-spec validate_id/2 :: (Id, AllowUndefined) -> boolean() when
      Id :: string() | binary(),
      AllowUndefined :: boolean().

validate_id(Id) ->
    validate_id(Id, false).

validate_id(undefined, false) ->
    false;
validate_id(Id, _) ->
    (Id =/= <<>> andalso Id =/= "")
        andalso (re:run(Id, "^[0-9A-Za-z_-]+$") =/= nomatch).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a subscription record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (Xml) -> #bt_address{} when
      Xml :: bt_xml().
-spec xml_to_record/2 :: (Xml, Base) -> #bt_address{} when
      Xml :: bt_xml(),
      Base :: string().

xml_to_record(Xml) ->
    xml_to_record(Xml, "/subscription").

xml_to_record(Xml, Base) ->
    #bt_subscription{id = get_xml_value(Base ++ "/id/text()", Xml)
                     ,balance = get_xml_value(Base ++ "/balance/text()", Xml)
                     ,billing_dom = get_xml_value(Base ++ "/billing-day-of-month/text()", Xml)
                     ,billing_first_date = get_xml_value(Base ++ "/first-billing-date/text()", Xml)
                     ,billing_end_date = get_xml_value(Base ++ "/billing-period-end-date/text()", Xml)
                     ,billing_start_date = get_xml_value(Base ++ "/billing-period-start-date/text()", Xml)
                     ,billing_cycle = get_xml_value(Base ++ "/current-billing-cycle/text()", Xml)
                     ,number_of_cycles = get_xml_value(Base ++ "/number-of-billing-cycles/text()", Xml)
                     ,days_past_due = get_xml_value(Base ++ "/days-past-due/text()", Xml)
                     ,failure_count = get_xml_value(Base ++ "/failure-count/text()", Xml)
                     ,merchant_account_id = get_xml_value(Base ++ "/merchant-account-id/text()", Xml)
                     ,never_expires = wh_util:is_true(get_xml_value(Base ++ "/never-expires/text()", Xml))
                     ,next_bill_amount = get_xml_value(Base ++ "/next-bill-amount/text()", Xml)
                     ,next_cycle_amount = get_xml_value(Base ++ "/next-billing-period-amount/text()", Xml)
                     ,next_bill_date = get_xml_value(Base ++ "/next-billing-date/text()", Xml)
                     ,paid_through_date = get_xml_value(Base ++ "/paid-through-date/text()", Xml)
                     ,payment_token = get_xml_value(Base ++ "/payment-method-token/text()", Xml)
                     ,plan_id = get_xml_value(Base ++ "/plan-id/text()", Xml)
                     ,price = get_xml_value(Base ++ "/price/text()", Xml)
                     ,status = get_xml_value(Base ++ "/status/text()", Xml)
                     ,trial_duration = get_xml_value(Base ++ "/trial-duration/text()", Xml)
                     ,trial_duration_unit = get_xml_value(Base ++ "/trial-duration-unit/text()", Xml)
                     ,trial_period = get_xml_value(Base ++ "/trial-period/text()", Xml)
                     ,add_ons = [braintree_addon:xml_to_record(Addon)
                                 || Addon <- xmerl_xpath:string(Base ++ "/add-ons/add-on", Xml)]
%%                     ,discounts = get_xml_value(Base ++ "/token/text()", Xml)
%%                     ,descriptor = get_xml_value(Base ++ "/token/text()", Xml)
                     ,transactions = [braintree_transaction:xml_to_record(Trans)
                                      || Trans <- xmerl_xpath:string(Base ++ "/transactions/transaction", Xml)]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a subscription record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (Subscription) -> bt_xml() when
      Subscription :: #bt_subscription{}.
-spec record_to_xml/2 :: (Subscription, ToString) -> bt_xml() when
      Subscription :: #bt_subscription{},
      ToString :: boolean().

record_to_xml(Subscription) ->
    record_to_xml(Subscription, false).

record_to_xml(Subscription, ToString) ->
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
             ,{'add-ons', create_addon_changes(Subscription#bt_subscription.add_ons)}],
    Conditionals = [fun(#bt_subscription{do_not_inherit=true}, P) ->
                            case proplists:get_value('options', P) of
                                undefined ->
                                    [{'options', [{'do-not-inherit-add-ons-or-discounts', true}]}|P];
                                Options ->
                                    Options1 = [{'do-not-inherit-add-ons-or-discounts', true}|Options],
                                    [{'options', Options1}|proplists:delete('options', P)]
                            end;
                       (_, P) ->
                            P
                    end,
                    fun(#bt_subscription{prorate_charges=true}, P) ->
                            case proplists:get_value('options', P) of
                                undefined ->
                                    [{'options', [{'prorate-charges', true}]}|P];
                                Options ->
                                    Options1 = [{'prorate-charges', true}|Options],
                                    [{'options', Options1}|proplists:delete('options', P)]
                            end;
                       (_, P) ->
                            P
                    end,
                    fun(#bt_subscription{revert_on_prorate_fail=true}, P) ->
                            case proplists:get_value('options', P) of
                                undefined ->
                                    [{'options', [{'revert-subscription-on-proration-failure', true}]}|P];
                                Options ->
                                    Options1 = [{'revert-subscription-on-proration-failure', true}|Options],
                                    [{'options', Options1}|proplists:delete('options', P)]
                            end;
                       (_, P) ->
                            P
                    end,
                    fun(#bt_subscription{replace_add_ons=true}, P) ->
                            case proplists:get_value('options', P) of
                                undefined ->
                                    [{'options', [{'replace-all-add-ons-and-discounts', true}]}|P];
                                Options ->
                                    Options1 = [{'replace-all-add-ons-and-discounts', true}|Options],
                                    [{'options', Options1}|proplists:delete('options', P)]
                            end;
                       (_, P) ->
                            P
                    end,
                    fun(#bt_subscription{start_immediately=true}, P) ->
                            [{'options', [{'start-immediately', true}]}|P];
                       (_, P) ->
                            P
                    end],
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
-spec create_addon_changes/1 :: (AddOns) -> undefined | list() when
      AddOns :: list().
create_addon_changes(undefined) ->
    undefined;
create_addon_changes(AddOns) ->
    Remove = [{'item', Id}
              || #bt_addon{id=Id, quantity=Q} <- AddOns, Id =/= undefined, Q =:= "0"],
    Add = [{'item', [{'inherited_from_id', IFId},{'quantity', Q}]}
           || #bt_addon{quantity=Q, inherited_from=IFId} <- AddOns, IFId =/= undefined, Q =/= "0"],
    Update = [{'item', [{'existing_id', EId},{'quantity', Q}]}
              || #bt_addon{quantity=Q, existing_id=EId} <- AddOns, EId =/= undefined, Q =/= "0"],
    Changes = [{'remove', [{'type', "array"}], Remove}
               ,{'add', [{'type', "array"}], Add}
               ,{'update', [{'type', "array"}], Update}],
    case [Change || {_, _, V}=Change <- Changes, V =/= []] of
        [] -> undefined;
        Else -> Else
    end.
