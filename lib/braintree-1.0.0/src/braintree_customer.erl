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
-export([new_subscription/3]).
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
-spec new/1 :: (ne_binary()) -> #bt_customer{}.
new(CustomerId) ->
    #bt_customer{id=CustomerId}.
    
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new subscription record
%% @end
%%--------------------------------------------------------------------
-spec new_subscription/3 :: (ne_binary(), ne_binary(), #bt_customer{}) -> {'ok', #bt_subscription{}} |
                                                                          {'error', _}.
new_subscription(SubscriptionId, PlanId, Customer) ->
    case default_payment_token(Customer) of
        undefined -> {error, no_payment_token};
        PaymentToken ->
            {ok, braintree_subscription:new(SubscriptionId, PlanId, PaymentToken)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a cutomer record find (if any) the default payment token
%% @end
%%--------------------------------------------------------------------
-spec default_payment_token/1 :: (#bt_customer{}) -> 'undefined' | string().
default_payment_token(#bt_customer{}=Customer) ->
    braintree_card:default_payment_token(get_cards(Customer)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get the customer id
%% @end
%%--------------------------------------------------------------------
-spec get_id/1 :: (#bt_customer{}) -> string().
get_id(#bt_customer{id=CustomerId}) ->
    CustomerId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get credit cards
%% @end
%%--------------------------------------------------------------------
-spec get_cards/1 :: (#bt_customer{}) -> [#bt_card{},...] | [].
get_cards(#bt_customer{credit_cards=Cards}) ->
    Cards.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get subscriptions
%% @end
%%--------------------------------------------------------------------
-spec get_subscriptions/1 :: (#bt_customer{}) -> [#bt_subscription{},...] | [].
get_subscriptions(#bt_customer{subscriptions=Subscriptions}) ->
    Subscriptions.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get a subscription
%% @end
%%--------------------------------------------------------------------
-spec get_subscription/2 :: (ne_binary(), #bt_customer{}) -> {'ok', #bt_subscription{}} | {'error', 'not_found'}.
get_subscription(SubscriptionId, Customer) ->
    case lists:keyfind(SubscriptionId, #bt_subscription.id, get_subscriptions(Customer)) of
        false -> {error, not_found};
        Subscription -> {ok, Subscription}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec all/0 :: () -> {'ok', [#bt_customer{},...]} | {'error', term()}.
all() ->
    Url = url(),
    case braintree_request:get(Url) of
        {error, _}=E -> E;
        {ok, Xml} ->
            {ok, [xml_to_record(Customer)
                  || Customer <- xmerl_xpath:string("/customers/customer", Xml)
                 ]}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (ne_binary() | nonempty_string()) -> bt_result().
find(CustomerId) ->
    case braintree_util:validate_id(CustomerId) of
        {error, _}=E -> E;
        ok ->
            Url = url(CustomerId),
            case braintree_request:get(Url) of
                {ok, Xml} -> {ok, xml_to_record(Xml)};
                {error, _}=E -> E
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer using the given record
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (#bt_customer{} | ne_binary()) -> bt_result().
create(#bt_customer{id=CustomerId}=Customer) ->
    case braintree_util:validate_id(CustomerId) of
        {error, _}=E -> E;
        ok ->
            Url = url(CustomerId),
            Request = record_to_xml(Customer, true),
            case braintree_request:post(Url, Request) of
                {ok, Xml} -> {ok, xml_to_record(Xml)};
                {error, _}=E -> E
            end
    end;
create(CustomerId) ->
    create(new(CustomerId)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a customer with the given record
%% @end
%%--------------------------------------------------------------------
-spec update/1 :: (#bt_customer{}) -> bt_result().
update(#bt_customer{id=CustomerId}=Customer) ->
    case braintree_util:validate_id(CustomerId) of
        {error, _}=E -> E;
        ok ->
            Url = url(CustomerId),
            Request = record_to_xml(Customer, true),
            case braintree_request:put(Url, Request) of
                {ok, Xml} -> {ok, xml_to_record(Xml)};
                {error, _}=E -> E
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a customer id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (#bt_customer{} | binary() | string()) -> bt_result().
delete(#bt_customer{id=CustomerId}) ->
    delete(CustomerId);
delete(CustomerId) ->
    case braintree_util:validate_id(CustomerId) of
        {error, _}=E -> E;
        ok ->
            Url = url(CustomerId),
            case braintree_request:delete(Url) of
                {ok, _} -> {ok, #bt_customer{}};
                {error, _}=E -> E
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (bt_xml()) -> #bt_customer{}.
-spec xml_to_record/2 :: (bt_xml(), string()) -> #bt_customer{}.

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
-spec record_to_xml/1 :: (#bt_customer{}) -> bt_xml().
-spec record_to_xml/2 :: (#bt_customer{}, boolean()) -> bt_xml().

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
-spec json_to_record/1 :: ('undefined' | wh_json:json_object()) -> #bt_customer{}.
json_to_record(undefined) ->
    #bt_customer{};
json_to_record(JObj) ->
    #bt_customer{id = wh_json:get_string_value(<<"id">>, JObj)
                 ,first_name = wh_json:get_string_value(<<"first_name">>, JObj)
                 ,last_name = wh_json:get_string_value(<<"last_name">>, JObj)
                 ,company = wh_json:get_string_value(<<"company">>, JObj)
                 ,email = wh_json:get_string_value(<<"email">>, JObj)
                 ,phone = wh_json:get_string_value(<<"phone">>, JObj)
                 ,fax = wh_json:get_string_value(<<"fax">>, JObj)
                 ,website = wh_json:get_string_value(<<"website">>, JObj)
                 ,credit_cards = [braintree_card:json_to_record(wh_json:get_value(<<"credit_card">>, JObj))]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json/1 :: (#bt_customer{}) -> wh_json:json_object().
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
    wh_json:from_list([KV || {_, V}=KV <- Props, V =/= undefined]).
