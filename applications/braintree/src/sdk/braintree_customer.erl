%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_customer).

-export([url/0, url/1]).
-export([new/1]).
-export([new_subscription/2]).
-export([default_payment_token/1]).
-export([default_payment_card/1]).
-export([get_id/1]).
-export([get_cards/1]).
-export([get_subscriptions/1, get_subscription/2]).
-export([all/0]).
-export([find/1]).
-export([create/1]).
-export([update/1]).
-export([delete/1]).
-export([xml_to_record/1, xml_to_record/2]).
-export([record_to_xml/1, record_to_xml/2]).
-export([json_to_record/1]).
-export([record_to_json/1]).

-include("braintree.hrl").

-type customer() :: #bt_customer{}.
-type customers() :: [customer()].
-export_type([customer/0
             ,customers/0
             ]).

%%------------------------------------------------------------------------------
%% @doc Create the partial URL for this module.
%% @end
%%------------------------------------------------------------------------------

-spec url() -> string().
url() ->
    "/customers/".

-spec url(kz_term:ne_binary()) -> string().
url(CustomerId) ->
    lists:append(["/customers/", kz_term:to_list(CustomerId)]).

%%------------------------------------------------------------------------------
%% @doc Creates a new customer record.
%% @end
%%------------------------------------------------------------------------------

-spec new(kz_term:ne_binary()) -> customer().
new(CustomerId) ->
    #bt_customer{id=CustomerId}.

%%------------------------------------------------------------------------------
%% @doc Creates a new subscription record.
%% @end
%%------------------------------------------------------------------------------

-spec new_subscription(kz_term:ne_binary(), customer()) -> braintree_subscription:subscription().
new_subscription(PlanId, Customer) ->
    PaymentToken = default_payment_token(Customer),
    braintree_subscription:new(PlanId, PaymentToken).

%%------------------------------------------------------------------------------
%% @doc Given a customer record find (if any) the default payment token.
%% @end
%%------------------------------------------------------------------------------

-spec default_payment_token(kz_term:ne_binary() | customer()) -> kz_term:api_binary().
default_payment_token(#bt_customer{}=Customer) ->
    braintree_card:default_payment_token(get_cards(Customer));
default_payment_token(CustomerId) ->
    default_payment_token(find(CustomerId)).

-spec default_payment_card(kz_term:ne_binary() | customer()) -> bt_card().
default_payment_card(#bt_customer{}=Customer) ->
    braintree_card:default_payment_card(get_cards(Customer));
default_payment_card(CustomerId) ->
    default_payment_card(find(CustomerId)).

%%------------------------------------------------------------------------------
%% @doc Get the customer ID.
%% @end
%%------------------------------------------------------------------------------

-spec get_id(customer()) -> kz_term:api_binary().
get_id(#bt_customer{id=CustomerId}) ->
    CustomerId.

%%------------------------------------------------------------------------------
%% @doc Get credit cards.
%% @end
%%------------------------------------------------------------------------------

-spec get_cards(customer()) -> bt_cards().
get_cards(#bt_customer{credit_cards=Cards}) ->
    Cards.

%%------------------------------------------------------------------------------
%% @doc Get subscriptions.
%% @end
%%------------------------------------------------------------------------------

-spec get_subscriptions(customer()) -> bt_subscriptions().
get_subscriptions(#bt_customer{subscriptions=Subscriptions}) ->
    Subscriptions.

%%------------------------------------------------------------------------------
%% @doc Get a subscription.
%% @end
%%------------------------------------------------------------------------------

-spec get_subscription(kz_term:ne_binary(), customer() | bt_subscriptions()) ->
                              bt_subscription().
get_subscription(PlanId, #bt_customer{subscriptions=Subscriptions}) ->
    get_subscription(PlanId, Subscriptions);
get_subscription(_, []) ->
    braintree_util:error_not_found(<<"Subscription">>);
get_subscription(PlanId, [#bt_subscription{plan_id=PlanId, status=Status}=Subscription
                          |Subscriptions
                         ]) ->
    case lists:member(Status, ?BT_ACTIVE_STATUSES) of
        'true' -> Subscription;
        'false' -> get_subscription(PlanId, Subscriptions)
    end;
get_subscription(PlanId, [_|Subscriptions]) ->
    get_subscription(PlanId, Subscriptions).

%%------------------------------------------------------------------------------
%% @doc Get all customers.
%% @end
%%------------------------------------------------------------------------------

-spec all() -> customers().
all() ->
    Url = url(),
    Xml = braintree_request:get(Url),
    [xml_to_record(Customer)
     || Customer <- xmerl_xpath:string("/customers/customer", Xml)
    ].

%%------------------------------------------------------------------------------
%% @doc Find a customer by ID.
%% @end
%%------------------------------------------------------------------------------

-spec find(kz_term:ne_binary() | customer()) -> customer().
find(#bt_customer{id = CustomerId}) -> find(CustomerId);
find(CustomerId) ->
    Url = url(CustomerId),
    Xml = braintree_request:get(Url),
    xml_to_record(Xml).

%%------------------------------------------------------------------------------
%% @doc Creates a new customer using the given record.
%% @end
%%------------------------------------------------------------------------------

-spec create(customer() | kz_term:ne_binary()) -> customer().
create(#bt_customer{}=Customer) ->
    Url = url(),
    Request = record_to_xml(Customer, 'true'),
    Xml = braintree_request:post(Url, Request),
    xml_to_record(Xml);
create(CustomerId) ->
    create(new(CustomerId)).

%%------------------------------------------------------------------------------
%% @doc Updates a customer with the given record.
%% @end
%%------------------------------------------------------------------------------

-spec update(customer()) -> customer().
update(#bt_customer{}=Customer) ->
    %% Note: coming from cb_braintree, Customer only has (unsynced) card data
    case [Card || Card <- get_cards(Customer),
                  braintree_card:make_default(Card) =:= 'true'
         ]
    of
        [] ->
            maybe_add_card_nonce(Customer);
        [Card] ->
            update_card(Customer, Card)
    end.

-spec maybe_add_card_nonce(customer()) -> customer().
maybe_add_card_nonce(#bt_customer{payment_method_nonce = 'undefined'}=Customer) ->
    do_update(Customer);
maybe_add_card_nonce(Customer) ->
    OldCustomer = find(Customer),
    OldPaymentTokens = braintree_card:payment_tokens(get_cards(OldCustomer)),

    %% Add new credit card, not setting it as default yet.
    UpdatedCustomer = do_update(Customer),

    NewPaymentTokens = braintree_card:payment_tokens(get_cards(UpdatedCustomer)),
    [NewPaymentToken] = lists:subtract(NewPaymentTokens, OldPaymentTokens),
    update_subsciption(NewPaymentToken, UpdatedCustomer).

-spec update_card(customer(), bt_card()) -> customer().
update_card(Customer, Card) ->
    Cards = [braintree_card:make_default(Card, 'false')
             | lists:delete(Card, get_cards(Customer))
            ],
    %% Add new credit card, not setting it as default yet.
    UpdatedCustomer = do_update(Customer#bt_customer{credit_cards = Cards}),

    NewPaymentToken = braintree_card:payment_token(Card),
    update_subsciption(NewPaymentToken, UpdatedCustomer).

-spec update_subsciption(kz_term:ne_binary(), bt_customer()) -> bt_customer().
update_subsciption(NewPaymentToken, UpdatedCustomer) ->
    %% NewCard = Card with updated fields
    {[NewCard], OldCards} =
        lists:partition(fun(CC) -> braintree_card:payment_token(CC) =:= NewPaymentToken end
                       ,get_cards(UpdatedCustomer)
                       ),

    lists:foreach(fun(Sub) -> update_subsciption_with_token(Sub, NewPaymentToken) end
                 ,get_subscriptions(UpdatedCustomer)
                 ),

    %% Make card as default /after/ updating subscriptions: this way
    %%  subscriptions are not attached to a deleted card and thus do not
    %%  get canceled before we can update their payment token.
    NewCard1 = braintree_card:update(braintree_card:make_default(NewCard, 'true')),

    %% Delete previous cards and addresses /after/ changing subscriptions' payment token.
    delete_old_cards_and_addresses(OldCards, NewCard1),

    %%get all the new user info
    find(UpdatedCustomer).

-spec update_subsciption_with_token(bt_subscription(), kz_term:ne_binary()) -> 'ok'.
update_subsciption_with_token(Sub, NewPaymentToken) ->
    ShouldUpdate = not braintree_subscription:is_canceled(Sub)
        andalso not braintree_subscription:is_expired(Sub),
    update_subsciption_with_token(Sub, NewPaymentToken, ShouldUpdate).

-spec update_subsciption_with_token(bt_subscription(), kz_term:ne_binary(), boolean()) -> 'ok'.
update_subsciption_with_token(Sub, NewPaymentToken, 'true') ->
    SubWithToken = braintree_subscription:update_payment_token(Sub, NewPaymentToken),
    _ = braintree_subscription:update(SubWithToken),
    'ok';
update_subsciption_with_token(_Sub, _NewPaymentToken, 'false') -> 'ok'.

-spec delete_old_cards_and_addresses(bt_cards(), bt_card()) -> 'ok'.
delete_old_cards_and_addresses(OldCards, #bt_card{billing_address_id=NewAddressId}) ->
    lists:foreach(fun(Card) -> delete_old_stuff(Card, NewAddressId) end
                 ,OldCards
                 ).

-spec delete_old_stuff(bt_card(), kz_term:ne_binary()) -> bt_card().
delete_old_stuff(#bt_card{billing_address_id='undefined'}=OldCard, _NewAddressId) ->
    braintree_card:delete(OldCard);
delete_old_stuff(#bt_card{billing_address_id=NewAddressId}=OldCard, NewAddressId) ->
    braintree_card:delete(OldCard);
delete_old_stuff(#bt_card{billing_address=OldAddress}=OldCard, _NewAddressId) ->
    _ = braintree_card:delete(OldCard),
    braintree_address:delete(OldAddress).

-spec do_update(bt_customer()) -> bt_customer().
do_update(#bt_customer{id=CustomerId}=Customer) ->
    Url = url(CustomerId),
    Record = record_to_xml(Customer, 'true'),
    Xml = braintree_request:put(Url, Record),
    xml_to_record(Xml).

%%------------------------------------------------------------------------------
%% @doc Deletes a customer ID from Braintree's system.
%% @end
%%------------------------------------------------------------------------------

-spec delete(customer() | kz_term:ne_binary()) -> customer().
delete(#bt_customer{id=CustomerId}) ->
    delete(CustomerId);
delete(CustomerId) ->
    Url = url(CustomerId),
    _ = braintree_request:delete(Url),
    #bt_customer{}.

%% @equiv xml_to_record(Xml, "/customer")
-spec xml_to_record(bt_xml()) -> customer().
xml_to_record(Xml) ->
    xml_to_record(Xml, "/customer").

%%------------------------------------------------------------------------------
%% @doc Convert the given XML to a customer record. Uses `Base' as base path
%% to get values from XML.
%% @end
%%------------------------------------------------------------------------------

-spec xml_to_record(bt_xml(), kz_term:deeplist()) -> customer().
xml_to_record(Xml, Base) ->
    CreditCardPath = lists:flatten([Base, "/credit-cards/credit-card"]),
    AddressPath = lists:flatten([Base, "/addresses/address"]),
    SubscriptionPath = lists:flatten([Base, "/credit-cards/credit-card/subscriptions/subscription"]),
    #bt_customer{id = kz_xml:get_value([Base, "/id/text()"], Xml)
                ,first_name = kz_xml:get_value([Base, "/first-name/text()"], Xml)
                ,last_name = kz_xml:get_value([Base, "/last-name/text()"], Xml)
                ,company = kz_xml:get_value([Base, "/company/text()"], Xml)
                ,email = kz_xml:get_value([Base, "/email/text()"], Xml)
                ,phone = kz_xml:get_value([Base, "/phone/text()"], Xml)
                ,fax = kz_xml:get_value([Base, "/fax/text()"], Xml)
                ,website = kz_xml:get_value([Base, "/website/text()"], Xml)
                ,created_at = kz_xml:get_value([Base, "/created-at/text()"], Xml)
                ,updated_at = kz_xml:get_value([Base, "/updated-at/text()"], Xml)
                ,credit_cards = [braintree_card:xml_to_record(Card)
                                 || Card <- xmerl_xpath:string(CreditCardPath, Xml)
                                ]
                ,addresses = [braintree_address:xml_to_record(Address)
                              || Address <- xmerl_xpath:string(AddressPath, Xml)
                             ]
                ,subscriptions = [braintree_subscription:xml_to_record(Subscription)
                                  || Subscription <- xmerl_xpath:string(SubscriptionPath, Xml)
                                 ]
                }.

%% @equiv record_to_xml(Customer, 'false')
-spec record_to_xml(customer()) -> kz_term:proplist() | bt_xml().
record_to_xml(Customer) ->
    record_to_xml(Customer, 'false').

%%------------------------------------------------------------------------------
%% @doc Convert the given record to XML. If `ToString' is
%% `true' returns exported XML as string binary.
%% @end
%%------------------------------------------------------------------------------

-spec record_to_xml(customer(), boolean()) -> kz_term:proplist() | bt_xml().
record_to_xml(Customer, ToString) ->
    Props = [{'id', Customer#bt_customer.id}
            ,{'first-name', Customer#bt_customer.first_name}
            ,{'last-name', Customer#bt_customer.last_name}
            ,{'company', Customer#bt_customer.company}
            ,{'email', Customer#bt_customer.email}
            ,{'phone', Customer#bt_customer.phone}
            ,{'fax', Customer#bt_customer.fax}
            ,{'website', Customer#bt_customer.website}
            ,{'payment-method-nonce', Customer#bt_customer.payment_method_nonce}
             |
             [{'credit-card', braintree_card:record_to_xml(Card)}
              || Card <- Customer#bt_customer.credit_cards, Card =/= 'undefined'
             ]
            ],
    case ToString of
        'true' -> braintree_util:make_doc_xml(Props, 'customer');
        'false' -> Props
    end.

%%------------------------------------------------------------------------------
%% @doc Convert a given JSON object into a record.
%% @end
%%------------------------------------------------------------------------------

-spec json_to_record(kz_term:api_object()) -> customer().
json_to_record('undefined') -> #bt_customer{};
json_to_record(JObj) ->
    #bt_customer{id = kz_doc:id(JObj)
                ,first_name = kz_json:get_binary_value(<<"first_name">>, JObj)
                ,last_name = kz_json:get_binary_value(<<"last_name">>, JObj)
                ,company = kz_json:get_binary_value(<<"company">>, JObj)
                ,email = kz_json:get_binary_value(<<"email">>, JObj)
                ,phone = kz_json:get_binary_value(<<"phone">>, JObj)
                ,fax = kz_json:get_binary_value(<<"fax">>, JObj)
                ,website = kz_json:get_binary_value(<<"website">>, JObj)
                ,payment_method_nonce = kz_json:get_binary_value(<<"payment_method_nonce">>, JObj)
                ,credit_cards = maybe_add_credit_card(JObj)
                }.

-spec maybe_add_credit_card(kz_term:api_object()) -> bt_cards().
maybe_add_credit_card(JObj) ->
    case kz_json:get_value(<<"credit_card">>, JObj) of
        'undefined' -> [];
        Card -> [braintree_card:json_to_record(Card)]
    end.

%%------------------------------------------------------------------------------
%% @doc Convert a given record into a JSON object.
%% @end
%%------------------------------------------------------------------------------

-spec record_to_json(customer()) -> kz_json:object().
record_to_json(Customer) ->
    kz_json:from_list(
      [{<<"id">>, Customer#bt_customer.id}
      ,{<<"first_name">>, Customer#bt_customer.first_name}
      ,{<<"last_name">>, Customer#bt_customer.last_name}
      ,{<<"company">>, Customer#bt_customer.company}
      ,{<<"email">>, Customer#bt_customer.email}
      ,{<<"phone">>, Customer#bt_customer.phone}
      ,{<<"fax">>, Customer#bt_customer.fax}
      ,{<<"website">>, Customer#bt_customer.website}
      ,{<<"created_at">>, Customer#bt_customer.created_at}
      ,{<<"updated_at">>, Customer#bt_customer.updated_at}
      ,{<<"credit_cards">>, [braintree_card:record_to_json(Card)
                             || Card <- Customer#bt_customer.credit_cards
                            ]}
      ,{<<"addresses">>, [braintree_address:record_to_json(Address)
                          || Address <- Customer#bt_customer.addresses
                         ]}
      ]).
