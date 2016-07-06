%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(braintree_card).

-export([url/0, url/1]).
-export([default_payment_token/1]).
-export([default_payment_card/1]).
-export([payment_token/1]).
-export([find/1]).
-export([create/1, create/2]).
-export([update/1]).
-export([delete/1]).
-export([expired/0
        ,expired/1
        ,expiring/2
        ]).
-export([make_default/1, make_default/2]).
-export([xml_to_record/1, xml_to_record/2]).
-export([record_to_xml/1, record_to_xml/2]).
-export([json_to_record/1]).
-export([record_to_json/1]).

-include("bt.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create the partial url for this module
%% @end
%%--------------------------------------------------------------------
-spec url() -> string().
-spec url(ne_binary()) -> string().
-spec url(ne_binary(), _) -> string().

url() ->
    "/payment_methods/".

url(Token) ->
    "/payment_methods/" ++ kz_util:to_list(Token).

url(Token, _) ->
    "/payment_methods/credit_card/" ++ kz_util:to_list(Token).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a list of #bt_cards{} find the current default payment token.
%% @end
%%--------------------------------------------------------------------
-spec default_payment_token(bt_cards()) -> api_binary().
default_payment_token(Cards) ->
    case lists:keyfind('true', #bt_card.default, Cards) of
        'false' -> braintree_util:error_no_payment_token();
        Card -> Card#bt_card.token
    end.

-spec default_payment_card(bt_cards()) -> bt_card().
default_payment_card(Cards) ->
    case lists:keyfind('true', #bt_card.default, Cards) of
        'false' -> braintree_util:error_no_payment_token();
        Card -> Card
    end.

-spec payment_token(bt_card()) -> api_binary().
payment_token(#bt_card{token = Value}) -> Value.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a credit card by id
%% @end
%%--------------------------------------------------------------------
-spec find(ne_binary() | bt_card()) -> bt_card().
find(#bt_card{token = CardId}) -> find(CardId);
find(Token) ->
                                                % github.com/braintree/braintree_php/blob/master/lib/Braintree/CreditCardGateway.php#L149
    Url = url(Token, ''),
    Xml = try braintree_request:get(Url)
          catch
              'throw':{'not_found', _JObj} ->
                  #bt_card{}
          end,
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new credit card using the given record
%% @end
%%--------------------------------------------------------------------
-spec create(bt_card()) -> bt_card().
-spec create(string() | ne_binary(), bt_card()) -> bt_card().

create(#bt_card{}=Card) ->
    Url = url(),
    Request = record_to_xml(Card, 'true'),
    Xml = braintree_request:post(Url, Request),
    xml_to_record(Xml).

create(CustomerId, Card) ->
    create(Card#bt_card{customer_id=CustomerId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a credit card with the given record
%% @end
%%--------------------------------------------------------------------
-spec update(bt_card()) -> bt_card().
update(#bt_card{token=Token}=Card) ->
    Url = url(Token),
    Request = record_to_xml(Card, 'true'),
    Xml = braintree_request:put(Url, Request),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a credit card id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete(bt_card() | binary() | string()) -> bt_card().
delete(#bt_card{token=Token}) -> delete(Token);
delete(Token) ->
    _ = braintree_request:delete(url(Token)),
    #bt_card{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Finds the tokens of credit cards that have all expired
%% @end
%%--------------------------------------------------------------------
-spec expired() -> [bt_xml()].
expired() ->
    Xml = braintree_request:post("/payment_methods/all/expired_ids", <<>>),
    [kz_util:get_xml_value("/item/text()", Item)
     || Item <- xmerl_xpath:string("/search-results/ids/item", Xml)
    ].

-spec expired(bt_card()) -> boolean().
expired(#bt_card{expired=Expired}) -> Expired.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Finds the tokens of credit cards expiring between the given
%% start and end dates. Dates are given as MMYYYY
%% @end
%%--------------------------------------------------------------------
-spec expiring(text(), text()) -> [bt_xml()].
expiring(Start, End) ->
    Url = lists:append(["/payment_methods/all/expiring?start="
                       ,kz_util:to_list(Start)
                       ,"&end="
                       ,kz_util:to_list(End)
                       ]),
    Xml = braintree_request:post(Url, <<>>),
    [xml_to_record(Item)
     || Item <- xmerl_xpath:string("/payment-methods/credit-card", Xml)
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Accessors for field 'make_default'.
%% @end
%%--------------------------------------------------------------------
-spec make_default(bt_card()) -> api_boolean().
-spec make_default(bt_card(), boolean()) -> bt_card().

make_default(#bt_card{make_default = Value}) -> Value.

make_default(#bt_card{}=Card, Value) ->
    Card#bt_card{make_default = Value}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the given XML to a record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record(bt_xml()) -> bt_card().
-spec xml_to_record(bt_xml(), kz_deeplist()) -> bt_card().

xml_to_record(Xml) ->
    xml_to_record(Xml, "/credit-card").

xml_to_record(Xml, Base) ->
    #bt_card{token = kz_util:get_xml_value([Base, "/token/text()"], Xml)
            ,bin = kz_util:get_xml_value([Base, "/bin/text()"], Xml)
            ,cardholder_name = kz_util:get_xml_value([Base, "/cardholder-name/text()"], Xml)
            ,card_type = kz_util:get_xml_value([Base, "/card-type/text()"], Xml)
            ,created_at = kz_util:get_xml_value([Base, "/created-at/text()"], Xml)
            ,updated_at = kz_util:get_xml_value([Base, "/updated-at/text()"], Xml)
            ,default = kz_util:is_true(kz_util:get_xml_value([Base, "/default/text()"], Xml))
            ,expiration_date = kz_util:get_xml_value([Base, "/expiration-date/text()"], Xml)
            ,expiration_month = kz_util:get_xml_value([Base, "/expiration-month/text()"], Xml)
            ,expiration_year = kz_util:get_xml_value([Base, "/expiration-year/text()"], Xml)
            ,expired = kz_util:is_true(kz_util:get_xml_value([Base, "/expired/text()"], Xml))
            ,customer_location = kz_util:get_xml_value([Base, "/customer-location/text()"], Xml)
            ,last_four = kz_util:get_xml_value([Base, "/last-4/text()"], Xml)
            ,customer_id = kz_util:get_xml_value([Base, "/customer-id/text()"], Xml)
            ,billing_address = braintree_address:xml_to_record(Xml, [Base, "/billing-address"])
            ,billing_address_id = kz_util:get_xml_value([Base, "/billing-address/id/text()"], Xml)
            }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the given record to XML
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml(bt_card()) -> kz_proplist() | bt_xml().
-spec record_to_xml(bt_card(), boolean()) -> kz_proplist() | bt_xml().

record_to_xml(Card) ->
    record_to_xml(Card, 'false').

record_to_xml(#bt_card{}=Card, ToString) ->
    Props = [{'token', Card#bt_card.token}
            ,{'cardholder-name', Card#bt_card.cardholder_name}
            ,{'expiration-date', Card#bt_card.expiration_date}
            ,{'expiration-month', Card#bt_card.expiration_month}
            ,{'expiration-year', Card#bt_card.expiration_year}
            ,{'customer-id', Card#bt_card.customer_id}
            ,{'number', Card#bt_card.number}
            ,{'cvv', Card#bt_card.cvv}
            ],
    Conditionals =
        [fun(#bt_card{billing_address=BA, billing_address_id=BAID}, P) ->
                 case BA == 'undefined' orelse
                     BA =:= (find(Card))#bt_card.billing_address
                 of
                     'true'  -> [{'billing-address-id', BAID} | P];
                     'false' -> [{'billing-address', braintree_address:record_to_xml(BA)} | P]
                 end
         end
        ,fun(#bt_card{update_existing='false'}, P) -> P;
            (#bt_card{update_existing=Token}, P) when is_binary(Token) ->
                 case props:get_value('options', P) of
                     'undefined' ->
                         [{'options', [{'update-existing-token', Token}]}
                          | props:delete('token', P)
                         ];
                     Options ->
                         [{'options', [{'update-existing-token', Token}|Options]}
                          | props:delete('token', props:delete('options', P))
                         ]
                 end;
            (#bt_card{update_existing='true'}, P) ->
                 case props:get_value('options', P) of
                     'undefined' ->
                         [{'options', [{'update-existing-token', Card#bt_card.token}]}
                          | props:delete('token', P)
                         ];
                     Options ->
                         [{'options', [{'update-existing-token', Card#bt_card.token}|Options]}
                          | props:delete('token', props:delete('options', P))
                         ]
                 end;
            (_, P) -> P
         end
        ,fun(#bt_card{verify='true', number=Number}, P) when Number =/= 'undefined' ->
                 case props:get_value('options', P) of
                     'undefined' ->
                         [{'options', [{'verify-card', 'true'}]} | P];
                     Options ->
                         Options1 = [{'verify-card', 'true'} | Options],
                         [{'options', Options1} | props:delete('options', P)]
                 end;
            (_, P) -> P
         end
        ,fun(#bt_card{make_default='true'}, P) ->
                 [{'options', [{'make-default', 'true'}]} | P];
            (_, P) -> P
         end
        ],
    Props1 = lists:foldr(fun(F, P) -> F(Card, P) end, Props, Conditionals),
    case ToString of
        'true' -> braintree_util:make_doc_xml(Props1, 'credit-card');
        'false' -> Props1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given json object into a record
%% @end
%%--------------------------------------------------------------------
-spec json_to_record(api_object()) -> bt_card().
json_to_record('undefined') -> 'undefined';
json_to_record(JObj) ->
    #bt_card{token = create_or_get_json_id(JObj)
            ,cardholder_name = kz_json:get_binary_value(<<"cardholder_name">>, JObj)
            ,expiration_date = kz_json:get_binary_value(<<"expiration_date">>, JObj)
            ,expiration_month = kz_json:get_binary_value(<<"expiration_month">>, JObj)
            ,expiration_year = kz_json:get_binary_value(<<"expiration_year">>, JObj)
            ,customer_id = kz_json:get_binary_value(<<"customer_id">>, JObj)
            ,number = kz_json:get_binary_value(<<"number">>, JObj)
            ,cvv = kz_json:get_binary_value(<<"cvv">>, JObj)
            ,billing_address_id = kz_json:get_binary_value(<<"billing_address_id">>, JObj)
            ,billing_address = braintree_address:json_to_record(kz_json:get_value(<<"billing_address">>, JObj))
            ,update_existing = kz_json:get_binary_value(<<"update_existing">>, JObj)
            ,verify = kz_json:is_true(<<"verify">>, JObj, 'true')
            ,make_default = kz_json:is_true(<<"make_default">>, JObj, 'true')
            }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json(bt_card()) -> kz_json:object().
record_to_json(#bt_card{}=Card) ->
    Props =[{<<"id">>, Card#bt_card.token}
           ,{<<"bin">>, Card#bt_card.bin}
           ,{<<"cardholder_name">>, Card#bt_card.cardholder_name}
           ,{<<"card_type">>, Card#bt_card.card_type}
           ,{<<"created_at">>, Card#bt_card.created_at}
           ,{<<"updated_at">>, Card#bt_card.updated_at}
           ,{<<"default">>, Card#bt_card.default}
           ,{<<"expiration_date">>, Card#bt_card.expiration_date}
           ,{<<"expiration_month">>, Card#bt_card.expiration_month}
           ,{<<"expiration_year">>, Card#bt_card.expiration_year}
           ,{<<"expired">>, Card#bt_card.expired}
           ,{<<"customer_location">>, Card#bt_card.customer_location}
           ,{<<"last_four">>, Card#bt_card.last_four}
           ,{<<"customer_id">>, Card#bt_card.customer_id}
           ,{<<"created_at">>, Card#bt_card.created_at}
           ,{<<"updated_at">>, Card#bt_card.updated_at}
           ,{<<"billing_address">>, braintree_address:record_to_json(Card#bt_card.billing_address)}
           ,{<<"billing_address_id">>, Card#bt_card.billing_address_id}
           ],
    kz_json:from_list(props:filter_undefined(Props)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the object exists in but no id has been provided then generate
%% a uuid to use during creation.
%% @end
%%--------------------------------------------------------------------
-spec create_or_get_json_id(kz_json:object()) -> api_binary().
create_or_get_json_id(JObj) ->
    case kz_json:get_value(<<"number">>, JObj) of
        'undefined' -> kz_doc:id(JObj);
        _ ->          kz_doc:id(JObj, kz_util:rand_hex_binary(16))
    end.
