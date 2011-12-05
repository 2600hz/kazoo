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
-export([xml_to_record/1, xml_to_record/2, record_to_xml/1, record_to_xml/2]).
-export([json_to_record/1, record_to_json/1]).


-import(braintree_util, [get_xml_value/2, make_doc_xml/2]).

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
        Request = record_to_xml(Customer, true),
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
        Request = record_to_xml(Customer, true),
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
-spec all/0 :: () -> {'ok', [#bt_customer{},...]} | {'error', term()}.
all() ->
    case braintree_request:get("/customers/") of
        {ok, Xml} ->
            Customers = [xml_to_record(Customer)
                         || Customer <- xmerl_xpath:string("/customers/customer", Xml)],
            {ok, Customers};
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (ne_binary() | nonempty_string()) -> bt_result().
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
%% Convert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (bt_xml()) -> #bt_customer{}.
-spec xml_to_record/2 :: (bt_xml(), string()) -> #bt_customer{}.

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
%% Convert the given record to XML
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (Customer) -> bt_xml() when
      Customer :: #bt_customer{}.
-spec record_to_xml/2 :: (Customer, ToString) -> bt_xml() when
      Customer :: #bt_customer{},
      ToString :: boolean().

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
               || Card <- Customer#bt_customer.credit_cards, Card =/= undefined]],
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
-spec json_to_record/1 :: (JObj) -> #bt_customer{} when
      JObj :: undefined | json_object().
json_to_record(undefined) ->
    #bt_customer{};
json_to_record(JObj) ->
    #bt_customer{id = wh_json:get_list_value(<<"id">>, JObj)
                 ,first_name = wh_json:get_list_value(<<"first_name">>, JObj)
                 ,last_name = wh_json:get_list_value(<<"last_name">>, JObj)
                 ,company = wh_json:get_list_value(<<"company">>, JObj)
                 ,email = wh_json:get_list_value(<<"email">>, JObj)
                 ,phone = wh_json:get_list_value(<<"phone">>, JObj)
                 ,fax = wh_json:get_list_value(<<"fax">>, JObj)
                 ,website = wh_json:get_list_value(<<"website">>, JObj)
                 ,credit_cards = [braintree_card:json_to_record(wh_json:get_value(<<"credit_card">>, JObj))]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json/1 :: (Customer) -> json_object() when
      Customer :: #bt_customer{}.
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
                               || Card <- Customer#bt_customer.credit_cards]}
             ,{<<"addresses">>, [braintree_address:record_to_json(Address)
                             || Address <- Customer#bt_customer.addresses]}],
    braintree_util:props_to_json(Props).
