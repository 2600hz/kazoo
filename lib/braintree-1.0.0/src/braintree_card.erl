%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_card).

-include("braintree.hrl").

-export([create/1, create/2, update/1, delete/1]).
%% -export([sale/2, credit/2]).
-export([find/1, expired/0, expiring/2]).
-export([xml_to_record/1, record_to_xml/1]).

-import(braintree_utils, [get_xml_value/2, make_doc_xml/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer using the given record
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (Card) -> bt_result() when
      Card :: #bt_card{}.
-spec create/2 :: (CustomerId, Card) -> bt_result() when
      CustomerId :: string() | binary(),
      Card :: #bt_card{}.

create(Card) ->
    try
        true = validate_id(Card#bt_card.customer_id, true),
        Request = record_to_xml(Card, true),
        case braintree_request:post("/payment_methods", Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, customer_id_invalid}
    end.

create(CustomerId, Card) ->
    create(Card#bt_card{customer_id=CustomerId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a customer with the given record
%% @end
%%--------------------------------------------------------------------
-spec update/1 :: (Card) -> bt_result() when
      Card :: #bt_card{}.
update(#bt_card{token=Token}=Card) ->
    try
        true = validate_id(Token),
        Request = record_to_xml(Card, true),
        case braintree_request:put("/payment_methods/" ++ wh_util:to_list(Token), Request) of
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
%% Deletes a customer id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (Token) -> bt_result() when
      Token :: #bt_card{} | binary() | string().
delete(#bt_card{token=Token}) ->
    delete(Token);
delete(Token) ->
    try
        true = validate_id(Token),
        case braintree_request:delete("/payment_methods/" ++ wh_util:to_list(Token)) of
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
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (Token) -> bt_result() when
      Token :: binary() | string().
find(Token) ->
        try
            true = validate_id(Token),
            case braintree_request:get("/payment_methods/" ++ wh_util:to_list(Token)) of
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
%% Finds the tokens of credit cards that have all expired
%% @end
%%--------------------------------------------------------------------
-spec expired/0 :: () -> tuple(ok, list()) | tuple(error, term()).
expired() ->
    case braintree_request:post("/payment_methods/all/expired_ids", <<>>) of
        {ok, Xml} ->
            {ok, [get_xml_value("/item/text()", Item)
                  || Item <- xmerl_xpath:string("/search-results/ids/item", Xml)]};
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Finds the tokens of credit cards expiring between the given
%% start and end dates.
%% @end
%%--------------------------------------------------------------------
-spec expiring/2 :: (Start, End) -> tuple(ok, list()) | tuple(error, term()) when
      Start :: string() | binary(),
      End :: string() | binary().
expiring(Start, End) ->
    case braintree_request:post("/payment_methods/all/expiring?start="
                                ++ wh_util:to_list(Start)
                                ++ "&end="
                                ++ wh_util:to_list(End), <<>>) of
        {ok, Xml} ->
            {ok, [xml_to_record(Item)
                  || Item <- xmerl_xpath:string("/payment-methods/credit-card", Xml)]};
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
%% @private
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (Xml) -> #bt_card{} when
      Xml :: bt_xml().
xml_to_record(Xml) ->
    #bt_card{token = get_xml_value("/credit-card/token/text()", Xml)
             ,bin = get_xml_value("/credit-card/bin/text()", Xml)
             ,cardholder_name = get_xml_value("/credit-card/cardholder-name/text()", Xml)
             ,card_type = get_xml_value("/credit-card/card-type/text()", Xml)
             ,created_at = get_xml_value("/credit-card/created-at/text()", Xml)
             ,updated_at = get_xml_value("/credit-card/updated-at/text()", Xml)
             ,default = wh_util:is_true(get_xml_value("/credit-card/default/text()", Xml))
             ,expiration_date = get_xml_value("/credit-card/expiration-date/text()", Xml)
             ,expiration_month = get_xml_value("/credit-card/expiration-month/text()", Xml)
             ,expiration_year = get_xml_value("/credit-card/expiration-year/text()", Xml)
             ,expired = wh_util:is_true(get_xml_value("/credit-card/expired/text()", Xml))
             ,customer_location = get_xml_value("/credit-card/customer-location/text()", Xml)
             ,last_four = get_xml_value("/credit-card/last-4/text()", Xml)
             ,customer_id = get_xml_value("/credit-card/customer-id/text()", Xml)
             ,billing_address = braintree_address:xml_to_record(Xml, "/credit-card/billing-address")}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (Card) -> bt_xml() when
      Card :: #bt_card{}.
-spec record_to_xml/2 :: (Card, ToString) -> bt_xml() when
      Card :: #bt_card{},
      ToString :: boolean().

record_to_xml(Card) ->
    record_to_xml(Card, false).

record_to_xml(Card, ToString) ->
    Props = [{'token', Card#bt_card.token}
%%             ,{'bin', Card#bt_card.bin}
             ,{'cardholder-name', Card#bt_card.cardholder_name}
%%             ,{'card-type', Card#bt_card.card_type}
%%             ,{'default', Card#bt_card.default}
             ,{'expiration-date', Card#bt_card.expiration_date}
             ,{'expiration-month', Card#bt_card.expiration_month}
             ,{'expiration-year', Card#bt_card.expiration_year}
%%             ,{'expired', Card#bt_card.expired}
%%             ,{'customer-location', Card#bt_card.customer_location}
%%             ,{'last-4', Card#bt_card.last_four}
             ,{'customer-id', Card#bt_card.customer_id}
             ,{'number', Card#bt_card.number}
             ,{'cvv', Card#bt_card.cvv}],
    Props1 = case Card#bt_card.make_default of
                 true -> [{'options', [{'make-default', true}]}|Props];
                 _ -> Props
             end,
    case ToString of
        true -> make_doc_xml(Props1, 'credit-card');
        false -> Props1
    end.
