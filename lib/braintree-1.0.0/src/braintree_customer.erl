%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_customer).

-include("braintree.hrl").

-export([create/1, update/1, delete/1]).
%% -export([sale/2, credit/2]).
-export([all/0, find/1]).
-export([xml_to_record/1, xml_to_record/2, record_to_xml/1]).

-import(braintree_utils, [get_xml_value/2, make_doc_xml/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer using the given record
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (Customer) -> bt_result() when
      Customer :: #bt_customer{}.
create(Customer) ->
    try
        true = validate_id(Customer#bt_customer.id, true),
        Request = record_to_xml(Customer),
        case braintree_request:post("/customers", Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, customer_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a customer with the given record
%% @end
%%--------------------------------------------------------------------
-spec update/1 :: (Customer) -> bt_result() when
      Customer :: #bt_customer{}.
update(Customer) ->
    try
        true = validate_id(Customer#bt_customer.id),
        Request = record_to_xml(Customer),
        case braintree_request:put("/customers/" ++ wh_util:to_list(Customer#bt_customer.id), Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, customer_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a customer id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (CustomerId) -> bt_result() when
      CustomerId :: #bt_customer{} | binary() | string().
delete(#bt_customer{id=CustomerId}) ->
    delete(CustomerId);
delete(CustomerId) ->
    try
        true = validate_id(CustomerId),
        case braintree_request:delete("/customers/" ++ wh_util:to_list(CustomerId)) of
            {ok, _} ->
                {ok, #bt_customer{}};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, customer_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec all/0 :: () -> list(#bt_customer{}) | tuple(error, term()).
all() ->
    case braintree_request:get("/customers/") of
        {ok, Xml} ->
            Customers = [xml_to_record(Customer)
                         || Customer <- xmerl_xpath:string("/customers/customer", Xml)],
            {ok, [Customers]};
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (CustomerId) -> bt_result() when
      CustomerId :: binary() | string().
find(CustomerId) ->
        try
            true = validate_id(CustomerId),
            case braintree_request:get("/customers/" ++ wh_util:to_list(CustomerId)) of
                {ok, Xml} ->
                    {ok, xml_to_record(Xml)};
                {error, _}=E ->
                    E
            end
        catch
            error:{badmatch, _} ->
                {error, customer_id_invalid}
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
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (Xml) -> #bt_address{} when
      Xml :: bt_xml().
-spec xml_to_record/2 :: (Xml, Base) -> #bt_address{} when
      Xml :: bt_xml(),
      Base :: string().

xml_to_record(Xml) ->
    xml_to_record(Xml, "/customer").

xml_to_record(Xml, Base) ->
    #bt_customer{id = get_xml_value(Base ++ "/id/text()", Xml)
                 ,first_name = get_xml_value(Base ++ "/first-name/text()", Xml)
                 ,last_name = get_xml_value(Base ++ "/last-name/text()", Xml)
                 ,company = get_xml_value(Base ++ "/company/text()", Xml)
                 ,email = get_xml_value(Base ++ "/email/text()", Xml)
                 ,phone = get_xml_value(Base ++ "/phone/text()", Xml)
                 ,fax = get_xml_value(Base ++ "/fax/text()", Xml)
                 ,website = get_xml_value(Base ++ "/website/text()", Xml)
                 ,created_at = get_xml_value(Base ++ "/created-at/text()", Xml)
                 ,updated_at = get_xml_value(Base ++ "/updated-at/text()", Xml)
                 ,credit_cards = [braintree_card:xml_to_record(Card)
                                  || Card <- xmerl_xpath:string(Base ++ "/credit-cards/credit-card", Xml)]
                 ,addresses = [braintree_address:xml_to_record(Address)
                                  || Address <- xmerl_xpath:string(Base ++ "/addresses/address", Xml)]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (Customer) -> bt_xml() when
      Customer :: #bt_customer{}.
record_to_xml(Customer) ->
    Props = [{'id', Customer#bt_customer.id}
             ,{'first-name', Customer#bt_customer.first_name}
             ,{'last-name', Customer#bt_customer.last_name}
             ,{'company', Customer#bt_customer.company}
             ,{'email', Customer#bt_customer.email}
             ,{'phone', Customer#bt_customer.phone}
             ,{'fax', Customer#bt_customer.fax}
             ,{'website', Customer#bt_customer.website}
             |[{'credit-card', braintree_card:record_to_xml(Card)}
               || Card <- Customer#bt_customer.credit_cards]],
    make_doc_xml(Props, customer).
