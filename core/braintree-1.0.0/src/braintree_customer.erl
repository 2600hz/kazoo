%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_customer).

-export([url/0, url/1]).
-export([new/1]).
-export([new_subscription/2]).
-export([default_payment_token/1]).
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

-import(braintree_util, [make_doc_xml/2]).
-import(wh_util, [get_xml_value/2]).

-include_lib("braintree/include/braintree.hrl").

-type customer() :: #bt_customer{}.
-type customers() :: [customer(),...] | [].
-export_type([customer/0
              ,customers/0
             ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create the partial url for this module
%% @end
%%--------------------------------------------------------------------
-spec url/0 :: () -> string().
-spec url/1 :: (ne_binary()) -> string().

url() ->
    "/customers/".

url(CustomerId) ->
    lists:append(["/customers/", wh_util:to_list(CustomerId)]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer record
%% @end
%%--------------------------------------------------------------------
-spec new/1 :: (ne_binary()) -> customer().
new(CustomerId) ->
    #bt_customer{id=CustomerId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new subscription record
%% @end
%%--------------------------------------------------------------------
-spec new_subscription/2 :: (ne_binary(), customer()) -> braintree_subscription:subscription().
new_subscription(PlanId, Customer) ->
    PaymentToken = default_payment_token(Customer),
    braintree_subscription:new(PlanId, PaymentToken).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a cutomer record find (if any) the default payment token
%% @end
%%--------------------------------------------------------------------
-spec default_payment_token/1 :: (ne_binary() | customer()) -> api_binary().
default_payment_token(#bt_customer{}=Customer) ->
    braintree_card:default_payment_token(get_cards(Customer));
default_payment_token(CustomerId) ->
    default_payment_token(find(CustomerId)).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the customer id
%% @end
%%--------------------------------------------------------------------
-spec get_id/1 :: (customer()) -> api_binary().
get_id(#bt_customer{id=CustomerId}) ->
    CustomerId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get credit cards
%% @end
%%--------------------------------------------------------------------
-spec get_cards/1 :: (customer()) -> braintree_card:cards().
get_cards(#bt_customer{credit_cards=Cards}) ->
    Cards.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get subscriptions
%% @end
%%--------------------------------------------------------------------
-spec get_subscriptions/1 :: (customer()) -> braintree_subscription:subscriptions().
get_subscriptions(#bt_customer{subscriptions=Subscriptions}) ->
    Subscriptions.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get a subscription
%% @end
%%--------------------------------------------------------------------
-spec get_subscription/2 :: (ne_binary(), customer() | braintree_subscription:subscriptions()) ->
                                    braintree_subscription:subscription().
get_subscription(PlanId, #bt_customer{subscriptions=Subscriptions}) ->
    get_subscription(PlanId, Subscriptions);
get_subscription(_, []) ->
    braintree_util:error_not_found(<<"Subscription">>);
get_subscription(PlanId, [#bt_subscription{plan_id=PlanId, status=Status}=Subscription
                          |Subscriptions
                         ]) ->
    case lists:member(Status, ?BT_ACTIVE_STATUSES) of
        true -> Subscription;
        false -> get_subscription(PlanId, Subscriptions)
    end;
get_subscription(PlanId, [_|Subscriptions]) ->
    get_subscription(PlanId, Subscriptions).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec all/0 :: () -> customers().
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
-spec find/1 :: (ne_binary() | nonempty_string()) -> customer().
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
-spec create/1 :: (customer() | ne_binary()) -> customer().
create(#bt_customer{}=Customer) ->
    Url = url(),
    Request = record_to_xml(Customer, true),
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
-spec update/1 :: (customer()) -> customer().
update(#bt_customer{id=CustomerId}=Customer) ->
    Url = url(CustomerId),
    Request = record_to_xml(Customer, true),
    Xml = braintree_request:put(Url, Request),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a customer id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (customer() | binary() | string()) -> customer().
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
-spec xml_to_record/1 :: (bt_xml()) -> customer().
-spec xml_to_record/2 :: (bt_xml(), wh_deeplist()) -> customer().

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
                                  ]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert the given record to XML
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (customer()) -> wh_proplist() | bt_xml().
-spec record_to_xml/2 :: (customer(), boolean()) -> wh_proplist() | bt_xml().

record_to_xml(Customer) ->
    record_to_xml(Customer, false).

record_to_xml(Customer, ToString) ->
    Props = [{'id', Customer#bt_customer.id}
             ,{'first-name', Customer#bt_customer.first_name}
             ,{'last-name', Customer#bt_customer.last_name}
             ,{'company', Customer#bt_customer.company}
             ,{'email', Customer#bt_customer.email}
             ,{'phone', Customer#bt_customer.phone}
             ,{'fax', Customer#bt_customer.fax}
             ,{'website', Customer#bt_customer.website}
             |[{'credit-card', braintree_card:record_to_xml(Card)}
               || Card <- Customer#bt_customer.credit_cards, Card =/= undefined
              ]
            ],
    case ToString of
        true -> make_doc_xml(Props, 'customer');
        false -> Props
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given json object into a record
%% @end
%%--------------------------------------------------------------------
-spec json_to_record/1 :: ('undefined' | wh_json:object()) -> customer().
json_to_record(undefined) ->
    #bt_customer{};
json_to_record(JObj) ->
    #bt_customer{id = wh_json:get_binary_value(<<"id">>, JObj)
                 ,first_name = wh_json:get_binary_value(<<"first_name">>, JObj)
                 ,last_name = wh_json:get_binary_value(<<"last_name">>, JObj)
                 ,company = wh_json:get_binary_value(<<"company">>, JObj)
                 ,email = wh_json:get_binary_value(<<"email">>, JObj)
                 ,phone = wh_json:get_binary_value(<<"phone">>, JObj)
                 ,fax = wh_json:get_binary_value(<<"fax">>, JObj)
                 ,website = wh_json:get_binary_value(<<"website">>, JObj)
                 ,credit_cards = [braintree_card:json_to_record(wh_json:get_value(<<"credit_card">>, JObj))]
                }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json/1 :: (customer()) -> wh_json:object().
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
    wh_json:from_list(props:filter_undefined(Props)).
