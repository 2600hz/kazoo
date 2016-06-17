%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
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

-import('kz_util', [get_xml_value/2]).

-include_lib("braintree/include/braintree.hrl").

-type customer() :: #bt_customer{}.
-type customers() :: [customer()].
-export_type([customer/0
              ,customers/0
             ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create the partial url for this module
%% @end
%%--------------------------------------------------------------------
-spec url() -> string().
-spec url(ne_binary()) -> string().

url() ->
    "/customers/".

url(CustomerId) ->
    lists:append(["/customers/", kz_util:to_list(CustomerId)]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer record
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary()) -> customer().
new(CustomerId) ->
    #bt_customer{id=CustomerId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new subscription record
%% @end
%%--------------------------------------------------------------------
-spec new_subscription(ne_binary(), customer()) -> braintree_subscription:subscription().
new_subscription(PlanId, Customer) ->
    PaymentToken = default_payment_token(Customer),
    braintree_subscription:new(PlanId, PaymentToken).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a customer record find (if any) the default payment token
%% @end
%%--------------------------------------------------------------------
-spec default_payment_token(ne_binary() | customer()) -> api_binary().
default_payment_token(#bt_customer{}=Customer) ->
    braintree_card:default_payment_token(get_cards(Customer));
default_payment_token(CustomerId) ->
    default_payment_token(find(CustomerId)).

-spec default_payment_card(ne_binary() | customer()) -> bt_card().
default_payment_card(#bt_customer{}=Customer) ->
    braintree_card:default_payment_card(get_cards(Customer));
default_payment_card(CustomerId) ->
    default_payment_card(find(CustomerId)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the customer id
%% @end
%%--------------------------------------------------------------------
-spec get_id(customer()) -> api_binary().
get_id(#bt_customer{id=CustomerId}) ->
    CustomerId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get credit cards
%% @end
%%--------------------------------------------------------------------
-spec get_cards(customer()) -> bt_cards().
get_cards(#bt_customer{credit_cards=Cards}) ->
    Cards.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get subscriptions
%% @end
%%--------------------------------------------------------------------
-spec get_subscriptions(customer()) -> braintree_subscription:subscriptions().
get_subscriptions(#bt_customer{subscriptions=Subscriptions}) ->
    Subscriptions.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get a subscription
%% @end
%%--------------------------------------------------------------------
-spec get_subscription(ne_binary(), customer() | braintree_subscription:subscriptions()) ->
                              braintree_subscription:subscription().
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec all() -> customers().
all() ->
    Url = url(),
    Xml = braintree_request:get(Url),
    [xml_to_record(Customer)
     || Customer <- xmerl_xpath:string("/customers/customer", Xml)
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec find(ne_binary() | customer()) -> customer().
find(#bt_customer{id = CustomerId}) -> find(CustomerId);
find(CustomerId) ->
    Url = url(CustomerId),
    Xml = braintree_request:get(Url),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer using the given record
%% @end
%%--------------------------------------------------------------------
-spec create(customer() | ne_binary()) -> customer().
create(#bt_customer{}=Customer) ->
    Url = url(),
    Request = record_to_xml(Customer, 'true'),
    Xml = braintree_request:post(Url, Request),
    xml_to_record(Xml);
create(CustomerId) ->
    create(new(CustomerId)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a customer with the given record
%% @end
%%--------------------------------------------------------------------
-spec update(customer()) -> customer().
update(#bt_customer{}=Customer) ->
    %% Note: coming from cb_braintree, Customer only has (unsynced) card data
    case [Card || Card <- get_cards(Customer),
                  braintree_card:make_default(Card) =:= 'true'
         ]
    of
        [] -> do_update(Customer);
        [Card] ->
            update_card(Customer, Card)
    end.

-spec update_card(customer(), bt_card()) -> customer().
update_card(Customer, Card) ->
    Cards = [braintree_card:make_default(Card, 'false')
             | lists:delete(Card, get_cards(Customer))
            ],
    %% Add new credit card, not setting it as default yet.
    UpdatedCustomer = do_update(Customer#bt_customer{credit_cards = Cards}),

    NewPaymentToken = braintree_card:payment_token(Card),
    %% NewCard = Card with updated fields
    {[NewCard], OldCards} =
        lists:partition(fun(CC) -> braintree_card:payment_token(CC) =:= NewPaymentToken end
                        ,get_cards(UpdatedCustomer)
                       ),

    NewSubscriptions =
        [braintree_subscription:update(
           braintree_subscription:update_payment_token(Sub, NewPaymentToken)
          )
         || Sub <- get_subscriptions(UpdatedCustomer),
            not braintree_subscription:is_cancelled(Sub)
                andalso not braintree_subscription:is_expired(Sub)
        ],
    %% Make card as default /after/ updating subscriptions: this way
    %%  subscriptions are not attached to a deleted card and thus do not
    %%  get cancelled before we can update their payment token.
    NewCards = [braintree_card:update(
                  braintree_card:make_default(NewCard, 'true')
                 )
               ],

    %% Delete previous cards and addresses /after/ changing subscriptions' payment token.
    lists:foreach(fun delete_old_card_and_address/1, OldCards),

    UpdatedCustomer#bt_customer{credit_cards = NewCards
                                ,subscriptions = NewSubscriptions
                               }.

-spec delete_old_card_and_address(bt_card()) -> bt_card().
delete_old_card_and_address(#bt_card{billing_address=OldAddress}=OldCard) ->
    braintree_address:delete(OldAddress),
    braintree_card:delete(OldCard).

-spec do_update(bt_customer()) -> bt_customer().
do_update(#bt_customer{id=CustomerId}=Customer) ->
    Url = url(CustomerId),
    Record = record_to_xml(Customer, 'true'),
    Xml = braintree_request:put(Url, Record),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a customer id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete(customer() | ne_binary()) -> customer().
delete(#bt_customer{id=CustomerId}) ->
    delete(CustomerId);
delete(CustomerId) ->
    Url = url(CustomerId),
    _ = braintree_request:delete(Url),
    #bt_customer{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record(bt_xml()) -> customer().
-spec xml_to_record(bt_xml(), kz_deeplist()) -> customer().

xml_to_record(Xml) ->
    xml_to_record(Xml, "/customer").

xml_to_record(Xml, Base) ->
    CreditCardPath = lists:flatten([Base, "/credit-cards/credit-card"]),
    AddressPath = lists:flatten([Base, "/addresses/address"]),
    SubscriptionPath = lists:flatten([Base, "/credit-cards/credit-card/subscriptions/subscription"]),
    #bt_customer{id = get_xml_value([Base, "/id/text()"], Xml)
                 ,first_name = get_xml_value([Base, "/first-name/text()"], Xml)
                 ,last_name = get_xml_value([Base, "/last-name/text()"], Xml)
                 ,company = get_xml_value([Base, "/company/text()"], Xml)
                 ,email = get_xml_value([Base, "/email/text()"], Xml)
                 ,phone = get_xml_value([Base, "/phone/text()"], Xml)
                 ,fax = get_xml_value([Base, "/fax/text()"], Xml)
                 ,website = get_xml_value([Base, "/website/text()"], Xml)
                 ,created_at = get_xml_value([Base, "/created-at/text()"], Xml)
                 ,updated_at = get_xml_value([Base, "/updated-at/text()"], Xml)
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert the given record to XML
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml(customer()) -> kz_proplist() | bt_xml().
-spec record_to_xml(customer(), boolean()) -> kz_proplist() | bt_xml().

record_to_xml(Customer) ->
    record_to_xml(Customer, 'false').

record_to_xml(Customer, ToString) ->
    Props = [{'id', Customer#bt_customer.id}
             ,{'first-name', Customer#bt_customer.first_name}
             ,{'last-name', Customer#bt_customer.last_name}
             ,{'company', Customer#bt_customer.company}
             ,{'email', Customer#bt_customer.email}
             ,{'phone', Customer#bt_customer.phone}
             ,{'fax', Customer#bt_customer.fax}
             ,{'website', Customer#bt_customer.website}
             |
             [{'credit-card', braintree_card:record_to_xml(Card)}
              || Card <- Customer#bt_customer.credit_cards, Card =/= 'undefined'
             ]
            ],
    case ToString of
        'true' -> braintree_util:make_doc_xml(Props, 'customer');
        'false' -> Props
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given json object into a record
%% @end
%%--------------------------------------------------------------------
-spec json_to_record(api_object()) -> customer().
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
                 ,credit_cards = [braintree_card:json_to_record(kz_json:get_value(<<"credit_card">>, JObj))]
                }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json(customer()) -> kz_json:object().
record_to_json(Customer) ->
    Props = [{<<"id">>, Customer#bt_customer.id}
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
            ],
    kz_json:from_list(props:filter_undefined(Props)).
